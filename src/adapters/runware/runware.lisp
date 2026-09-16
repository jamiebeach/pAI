;;;; runware.lisp -- image generation via Runware AI, as a new tool.
;;;;
;;;; Adds GENERATE-IMAGE to *tools*, following the exact same
;;;; check-then-append / rename-and-wrap-EXECUTE idiom already used for
;;;; web-search (enhancements.lisp) and broadcast-image (web-terminal.lisp)
;;;; -- purely additive, never clobbers the lisp-eval/propose-loop dispatch
;;;; chain from self-mod.lisp.
;;;;
;;;; API key lives in /agent/state/runwarekey.txt (plain text, one line),
;;;; read fresh on every call rather than cached at load time -- rotating
;;;; the key is just an edit to that file, no reload needed.
;;;;
;;;; Confirmed against Runware's docs (runware.ai/docs/image-inference,
;;;; runware.ai/docs/platform/authentication):
;;;;   POST https://api.runware.ai/v1, header Authorization: Bearer <key>
;;;;   Body is a JSON ARRAY of task objects, even for a single task.
;;;;   Success: {"data": [{"imageURL": "...", ...}, ...]}
;;;;   Failure: {"error": "..."} (no "data" key at all)
;;;; Reference-image base64 support isn't documented explicitly; handled
;;;; defensively below (URLs pass through untouched, anything else is
;;;; assumed to already be base64 -- a leading "data:...;base64," prefix
;;;; is stripped since Runware's other endpoints take raw base64, not a
;;;; full data URI).
;;;;
;;;; Load live (no restart) via the agent's own lisp-eval tool:
;;;;   (load "/agent/state/runware.lisp")

(in-package :agent)

(export '(runware-tool-handle))

(defparameter *runware-endpoint* "https://api.runware.ai/v1")
(defparameter *runware-key-file*
  (let ((root (uiop:getenv "PAI_SECRET_ROOT")))
    (if (and root (plusp (length root)))
        (merge-pathnames "runwarekey.txt" (pathname root))
        #P"/agent/state/runwarekey.txt")))
(defparameter *runware-default-model* "xai:grok-imagine@image")
(defparameter *runware-check-content* :false
  "Runware JSON boolean for its optional content-safety check.  The model or
upstream provider may still enforce its own policy.")
(defparameter *runware-read-timeout* 180
  "Image generation can genuinely take longer than a normal chat-completion
call; give it more room than *http-read-timeout* before giving up.")

(defun runware-api-key ()
  (let ((raw (read-file-string (namestring *runware-key-file*))))
    (string-trim '(#\Space #\Tab #\Newline #\Return) raw)))

(defun %runware-uuid4 ()
  "A random UUID v4 string. No external UUID library is loaded elsewhere
in this codebase, and the field only needs to be unique per request, not
cryptographically strong -- COMMON-LISP:RANDOM is sufficient."
  (let ((bytes (make-array 16 :element-type '(unsigned-byte 8))))
    (dotimes (i 16) (setf (aref bytes i) (random 256)))
    (setf (aref bytes 6) (logior (logand (aref bytes 6) #x0f) #x40)) ; version 4
    (setf (aref bytes 8) (logior (logand (aref bytes 8) #x3f) #x80)) ; variant
    (format nil "~(~2,'0x~2,'0x~2,'0x~2,'0x-~2,'0x~2,'0x-~2,'0x~2,'0x-~2,'0x~2,'0x-~2,'0x~2,'0x~2,'0x~2,'0x~2,'0x~2,'0x~)"
            (aref bytes 0) (aref bytes 1) (aref bytes 2) (aref bytes 3)
            (aref bytes 4) (aref bytes 5) (aref bytes 6) (aref bytes 7)
            (aref bytes 8) (aref bytes 9) (aref bytes 10) (aref bytes 11)
            (aref bytes 12) (aref bytes 13) (aref bytes 14) (aref bytes 15))))

;;; --- uploading a LOCAL file as a reference image --------------------------
;;; Problem this solves: passing a local file to generate-image's
;;; reference_images means base64-encoding it first, and that base64 has to
;;; travel through the model's tool-call arguments -- which get logged in
;;; full (pai-thinking.log, the web terminal's SSE stream) and can run
;;; into megabytes of text for a single photo, drowning out everything
;;; else. UPLOAD-REFERENCE-IMAGE does the base64 encoding *inside this
;;; function only*: the model just passes a short file PATH in, and gets a
;;; short mediaURL back -- the giant base64 payload never appears in a tool
;;; call, a log line, or the terminal at all.

(defparameter *base64-alphabet*
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")

(defun %base64-encode-bytes (bytes)
  (let ((len (length bytes)) (out (make-string-output-stream)))
    (loop for i from 0 below len by 3
          do (let* ((b0 (aref bytes i))
                    (b1 (if (< (+ i 1) len) (aref bytes (+ i 1)) 0))
                    (b2 (if (< (+ i 2) len) (aref bytes (+ i 2)) 0))
                    (n (logior (ash b0 16) (ash b1 8) b2)))
               (write-char (char *base64-alphabet* (ldb (byte 6 18) n)) out)
               (write-char (char *base64-alphabet* (ldb (byte 6 12) n)) out)
               (write-char (if (< (+ i 1) len) (char *base64-alphabet* (ldb (byte 6 6) n)) #\=) out)
               (write-char (if (< (+ i 2) len) (char *base64-alphabet* (ldb (byte 6 0) n)) #\=) out)))
    (get-output-stream-string out)))

(defun %read-file-bytes (path)
  (with-open-file (in path :element-type '(unsigned-byte 8))
    (let ((buf (make-array (file-length in) :element-type '(unsigned-byte 8))))
      (read-sequence buf in)
      buf)))

(defun %guess-image-mime (path)
  (let ((ext (string-downcase (or (pathname-type (pathname path)) ""))))
    (cond ((string= ext "png") "image/png")
          ((member ext '("jpg" "jpeg") :test #'string=) "image/jpeg")
          ((string= ext "gif") "image/gif")
          ((string= ext "webp") "image/webp")
          (t "image/png"))))

(defun upload-reference-image (path)
  "Upload a local image file (any absolute path readable on disk) to
Runware's media storage (taskType mediaStorage, per
runware.ai/docs/platform/media-storage) and return its public mediaURL.
That URL can then be passed straight into GENERATE-IMAGE's
reference-images -- no base64 ever touches a tool call or a log."
  (handler-case
      (let* ((bytes (%read-file-bytes path))
             (b64 (%base64-encode-bytes bytes))
             (data-uri (format nil "data:~a;base64,~a" (%guess-image-mime path) b64))
             (task (obj "taskType" "mediaStorage" "taskUUID" (%runware-uuid4)
                        "operation" "upload" "media" data-uri))
             (resp (shasht:read-json
                    (dex:post *runware-endpoint*
                              :headers `(("Authorization" . ,(format nil "Bearer ~a" (runware-api-key)))
                                         ("Content-Type" . "application/json"))
                              :connect-timeout *http-connect-timeout*
                              :read-timeout *runware-read-timeout*
                              :content (shasht:write-json (vector task) nil)))))
        (let ((err (gethash "error" resp)))
          (if (present-p err)
              (format nil "ERROR: Runware API: ~a" err)
              (let ((results (gethash "data" resp)))
                (if (and (present-p results) (plusp (length results)))
                    (gethash "mediaURL" (aref results 0))
                    "ERROR: Runware API returned no data and no error -- unexpected response shape")))))
    (error (e) (format nil "ERROR: ~a" e))))

;;; --- saving a generated image locally, and looking at any image ---------
;;; 2026-07-27: GENERATE-IMAGE always returned a bare Runware URL as the
;;; tool result -- fine for sharing, but it meant the model had no way to
;;; actually SEE what it made afterward, only whatever the user described
;;; back to it. (Confirmed live: asked what it thought of an image it'd
;;; just generated, the agent correctly diagnosed this exact gap itself --
;;; "the URL is just a string to me.") The fix has two parts: GENERATE-IMAGE
;;; now also downloads and saves a local copy under *GENERATED-IMAGES-DIR*
;;; (durable, and a path VIEW-IMAGE can read directly without a network
;;; round-trip), and the new VIEW-IMAGE tool lets its look at ANY image --
;;; a URL or a local path -- at any time, not just right after generating
;;; one. VIEW-IMAGE works by making its own small side call to the model
;;; via RAW-CALL-MODEL (same primitive VERIFIER-CALL-MODEL and the
;;; summarizer already use to stay off the broadcast/logging path) with a
;;; real OpenAI/OpenRouter-style image_url content block -- confirmed live
;;; against xiaomi/mimo-v2.5 via a standalone RAW-CALL-MODEL test before
;;; this was wired in as a tool: the model's own reasoning correctly
;;; described the literal contents of a test image, so this is genuine
;;; vision, not a guess from the filename or prompt text.

(defparameter *generated-images-dir*
  (let ((root (or (sb-ext:posix-getenv "PAI_ARTIFACT_ROOT")
                  (sb-ext:posix-getenv "PAI_R3A_ARTIFACT_ROOT"))))
    (if (and root (> (length root) 0))
        (merge-pathnames "generated-images/" (pathname root))
        #P"/agent/state/generated-images/")))

(defun %runware-guess-ext-from-url (url)
  (let ((dot (position #\. url :from-end t))
        (q (position #\? url)))
    (if dot
        (string-downcase (subseq url (1+ dot) (or q (length url))))
        "jpg")))

(defun %runware-download-to-local (url)
  "Download URL's bytes and save under *GENERATED-IMAGES-DIR*, returning the
local path as a string, or NIL on failure. Best-effort: the public URL
alone is still usable for sharing/viewing even if a local save fails, so
this never signals -- a download hiccup shouldn't break GENERATE-IMAGE."
  (handler-case
      (let* ((bytes (dex:get url :force-binary t))
             (ext (%runware-guess-ext-from-url url))
             (path (merge-pathnames (format nil "~a.~a" (%runware-uuid4) ext)
                                     *generated-images-dir*)))
        (ensure-directories-exist *generated-images-dir*)
        (with-open-file (out path :direction :output :element-type '(unsigned-byte 8)
                                   :if-exists :supersede :if-does-not-exist :create)
          (write-sequence bytes out))
        (namestring path))
    (error (e)
      (format t "~&[runware] local save failed for ~a: ~a~%" url e)
      nil)))

(defun %view-image-normalize (img-ref)
  "IMG-REF is either an http(s) URL or a data: URI (passed through
unchanged) or a local file path (read and base64-encoded into a data:
URI) -- the model can't reach an arbitrary filesystem path directly, only
a real URL or inline image data. Named IMG-REF, not REF, to avoid
shadowing the global REF (JSON path-lookup) helper used below."
  (if (or (and (>= (length img-ref) 7) (string-equal (subseq img-ref 0 7) "http://"))
          (and (>= (length img-ref) 8) (string-equal (subseq img-ref 0 8) "https://"))
          (and (>= (length img-ref) 5) (string-equal (subseq img-ref 0 5) "data:")))
      img-ref
      (let* ((bytes (%read-file-bytes img-ref))
             (b64 (%base64-encode-bytes bytes)))
        (format nil "data:~a;base64,~a" (%guess-image-mime img-ref) b64))))

(defun view-image (img-ref &optional (question "Describe this image in specific, concrete detail: subject, composition, colors, mood, anything notable."))
  "Look at an image right now -- IMG-REF is a URL or a local file path --
and return a text description of what's actually in it. A small side call
via RAW-CALL-MODEL, deliberately not part of the main conversation: this
is the agent asking its own model to look at something, not a full turn."
  (handler-case
      (let* ((url (%view-image-normalize img-ref))
             (resp (raw-call-model
                    (list (obj "role" "user" "content"
                               (vector (obj "type" "text" "text" question)
                                       (obj "type" "image_url" "image_url" (obj "url" url)))))))
             (content (gethash "content" (ref resp "choices" 0 "message"))))
        (if (stringp content) content "ERROR: model returned no description"))
    (error (e) (format nil "ERROR: ~a" e))))

(defun %runware-normalize-reference (img)
  "IMG is either an http(s) URL (passed through unchanged) or an image the
caller already has as base64 -- strip a data-URI prefix if present, since
Runware's other endpoints expect raw base64, not `data:...;base64,...`."
  (if (or (and (>= (length img) 7) (string-equal (subseq img 0 7) "http://"))
          (and (>= (length img) 8) (string-equal (subseq img 0 8) "https://")))
      img
      (let ((comma (position #\, img)))
        (if (and comma (search "base64" (subseq img 0 comma)))
            (subseq img (1+ comma))
            img))))

(defun %runware-image-task (prompt model width height number-results
                            reference-images)
  "Build the native imageInference task separately so its safety and payload
contract can be regression-tested without a network request."
  (let ((task (obj "taskType" "imageInference"
                   "taskUUID" (%runware-uuid4)
                   "model" model
                   "positivePrompt" prompt
                   "width" width
                   "height" height
                   "numberResults" number-results
                   "outputType" "URL"
                   "outputFormat" "JPG"
                   "outputQuality" 95
                   "deliveryMethod" "sync"
                   ;; The xai:grok-imagine@image model schema nests its safety
                   ;; toggle here. :FALSE is SHASHT's JSON-false sentinel;
                   ;; NIL would serialize null.
                   "safety" (obj "checkContent" *runware-check-content*))))
    (when reference-images
      (setf (gethash "inputs" task)
            (obj "referenceImages"
                 (map 'vector #'%runware-normalize-reference reference-images))))
    task))

(defun generate-image (prompt &key (model *runware-default-model*)
                                    (width 1024) (height 1024)
                                    (number-results 1)
                                    reference-images)
  "Generate an image (or images) from PROMPT via Runware. Returns a list of
https:// image URLs, or a string starting with \"ERROR:\" on failure.
REFERENCE-IMAGES, if given, is a list of URLs and/or base64 strings."
  (handler-case
      (let* ((task (%runware-image-task prompt model width height
                                        number-results reference-images))
             (resp (shasht:read-json
                    (dex:post *runware-endpoint*
                              :headers `(("Authorization" . ,(format nil "Bearer ~a" (runware-api-key)))
                                         ("Content-Type" . "application/json"))
                              :connect-timeout *http-connect-timeout*
                              :read-timeout *runware-read-timeout*
                              :content (shasht:write-json (vector task) nil)))))
        (let ((err (gethash "error" resp)))
          (if (present-p err)
              (format nil "ERROR: Runware API: ~a" err)
              (let ((results (gethash "data" resp)))
                (if (and (present-p results) (plusp (length results)))
                    (map 'list (lambda (r) (gethash "imageURL" r)) results)
                    "ERROR: Runware API returned no data and no error -- unexpected response shape")))))
    (error (e) (format nil "ERROR: ~a" e))))

;;; --- bounded state-file discovery ----------------------------------------

(defparameter *state-file-root*
  (let ((root (or (sb-ext:posix-getenv "PAI_ARTIFACT_ROOT")
                  (sb-ext:posix-getenv "PAI_R3A_ARTIFACT_ROOT"))))
    (if (and root (> (length root) 0))
        (pathname root)
        #P"/agent/state/")))
(defparameter *state-file-search-max-results* 50)
(defparameter *state-file-search-max-depth* 4)
(defparameter *state-file-search-max-directories* 200)
(defparameter *state-file-search-timeout-seconds* 5)
(defparameter *state-file-default-subdirectories*
  '("uploads" "deliverables" "generated-images")
  "Artifact-bearing subtrees searched when no explicit subdirectory is given.
Operational archives such as captured-functions and repl-drop are deliberately
excluded from a root search; callers may name one explicitly if needed.")

(defun %state-file-safe-relative-p (value)
  (and (stringp value)
       (not (uiop:absolute-pathname-p (pathname value)))
       (not (find ".." (uiop:split-string value :separator '(#\/ #\\))
                  :test #'string=))))

(defun %state-file-under-root-p (path root)
  (let ((candidate (namestring (truename path)))
        (prefix (namestring (truename root))))
    (and (>= (length candidate) (length prefix))
         (string-equal prefix candidate :end2 (length prefix)))))

(defun %state-file-relative-name (path root)
  (enough-namestring (truename path) (truename root)))

(defun find-state-files (&key (subdirectory "") query extension
                             (max-results 20) (max-depth 3))
  "Return bounded metadata for regular files below /agent/state.  This is a
read-only alternative to guessed DIRECTORY wildcards or recursive Lisp forms."
  (handler-case
      (sb-ext:with-timeout *state-file-search-timeout-seconds*
        (progn
        (unless (%state-file-safe-relative-p subdirectory)
          (error "subdirectory must be a relative path below /agent/state"))
        (let* ((root (truename *state-file-root*))
               (start (truename (merge-pathnames
                                 (uiop:ensure-directory-pathname subdirectory)
                                 root)))
               (limit (max 1 (min *state-file-search-max-results*
                                  (if (integerp max-results) max-results 20))))
               (depth-limit (max 0 (min *state-file-search-max-depth*
                                        (if (integerp max-depth) max-depth 3))))
               (needle (and (stringp query) (string-downcase query)))
               (wanted-ext (and (stringp extension)
                                (string-downcase
                                 (string-left-trim "." extension))))
               (matches nil)
               (visited 0))
          (unless (%state-file-under-root-p start root)
            (error "resolved subdirectory escapes /agent/state"))
          (labels ((match-p (path)
                     (let ((relative (string-downcase
                                      (%state-file-relative-name path root)))
                           (type (string-downcase
                                  (or (pathname-type path) ""))))
                       (and (or (null needle) (search needle relative))
                            (or (null wanted-ext)
                                (string= wanted-ext type)))))
                   (walk (directory depth)
                      (when (and (< (length matches) limit)
                                 (<= depth depth-limit)
                                 (< visited *state-file-search-max-directories*))
                       (incf visited)
                       (dolist (file (sort (copy-list (uiop:directory-files directory))
                                           #'string< :key #'namestring))
                         (when (and (< (length matches) limit)
                                    (%state-file-under-root-p file root)
                                    (match-p file))
                           (push (obj "path"
                                      (format nil "/agent/state/~a"
                                              (%state-file-relative-name file root))
                                      "relative_path"
                                      (%state-file-relative-name file root)
                                      "bytes" (with-open-file
                                                  (stream file
                                                          :element-type
                                                          '(unsigned-byte 8))
                                                (file-length stream))
                                      "modified_universal_time"
                                      (or (file-write-date file) :null))
                                 matches)))
                       (when (< depth depth-limit)
                         (dolist (child (sort (copy-list (uiop:subdirectories directory))
                                             #'string< :key #'namestring))
                            (when (and (< visited
                                             *state-file-search-max-directories*)
                                       (< (length matches) limit)
                                       (%state-file-under-root-p child root))
                              (when (or (plusp depth)
                                        (plusp (length subdirectory))
                                        (member
                                         (car (last (pathname-directory child)))
                                         *state-file-default-subdirectories*
                                         :test #'string-equal))
                                (walk child (1+ depth)))))))))
            (walk start 0))
          (shasht:write-json
           (obj "root" "/agent/state/"
                "subdirectory" subdirectory
                "query" (or query :null)
                "extension" (or extension :null)
                 "max_depth" depth-limit
                 "max_directories" *state-file-search-max-directories*
                 "timeout_seconds" *state-file-search-timeout-seconds*
                 "directories_visited" visited
                 "truncated" (if (or (= (length matches) limit)
                                      (>= visited
                                          *state-file-search-max-directories*))
                                  t nil)
                 "files" (coerce (nreverse matches) 'vector)) nil))))
    (sb-ext:timeout ()
      (format nil
              "ERROR: bounded state-file search exceeded the ~a-second timeout; narrow subdirectory, query, or depth."
              *state-file-search-timeout-seconds*))
    (error (condition)
      (format nil "ERROR: bounded state-file search failed: ~a" condition))))

;;; --- register as a tool, same idiom as web-search / broadcast-image ----

(unless (find "generate-image" *tools*
              :key (lambda (tool) (ref tool "function" "name")) :test #'string=)
  (setf *tools*
        (concatenate 'vector *tools*
          (vector
           (obj "type" "function" "function"
                (obj "name" "generate-image"
                     "description" "Generate an image from a text prompt using Runware AI. Defaults to xai:grok-imagine@image but any Runware model slug (creator:family@version) may be given. Optionally supply reference images (URLs or base64) for image-to-image / style reference. If the web terminal is open, the result is also pushed there live; either way the image URL(s) are returned so you can share them yourself (e.g. via Telegram)."
                     "parameters"
                     (obj "type" "object"
                          "properties"
                          (obj "prompt" (obj "type" "string" "description" "The image description (Runware's positivePrompt).")
                               "model" (obj "type" "string" "description" "Runware model slug, e.g. xai:grok-imagine@image. Defaults to xai:grok-imagine@image if omitted.")
                               "width" (obj "type" "integer" "description" "Image width in pixels. Defaults to 1024.")
                               "height" (obj "type" "integer" "description" "Image height in pixels. Defaults to 1024.")
                               "number_results" (obj "type" "integer" "description" "How many images to generate. Defaults to 1.")
                               "reference_images" (obj "type" "array" "items" (obj "type" "string")
                                                        "description" "Optional list of reference image URLs, for image-to-image / style reference. For a LOCAL file, call upload-reference-image first and pass its returned URL here -- never paste raw base64 into this argument, it floods the terminal and burns your context budget."))
                          "required" (vector "prompt"))))))))

(unless (find "upload-reference-image" *tools*
              :key (lambda (tool) (ref tool "function" "name")) :test #'string=)
  (setf *tools*
        (concatenate 'vector *tools*
          (vector
           (obj "type" "function" "function"
                (obj "name" "upload-reference-image"
                     "description" "Upload a LOCAL image file (an absolute path readable on disk, e.g. one saved via a file upload) to Runware's media storage and get back a short, public URL. Use this before generate-image whenever you want to use an existing local file as a reference image -- pass the file PATH here, never the base64 content itself, and use the returned URL in generate-image's reference_images."
                     "parameters"
                     (obj "type" "object"
                          "properties" (obj "path" (obj "type" "string" "description" "Absolute path to the local image file, e.g. /agent/state/tmp/uploaded.png"))
                          "required" (vector "path"))))))))

(unless (find "view-image" *tools*
              :key (lambda (tool) (ref tool "function" "name")) :test #'string=)
  (setf *tools*
        (concatenate 'vector *tools*
          (vector
           (obj "type" "function" "function"
                (obj "name" "view-image"
                     "description" "Actually look at an image and get back a real description of what's in it -- works for any image at any time: a URL, or a local file path under /agent/state (including anything GENERATE-IMAGE saved to /agent/state/generated-images/). Use this whenever you want to know what's genuinely in a picture rather than guessing from the prompt that made it."
                     "parameters"
                     (obj "type" "object"
                          "properties"
                          (obj "image" (obj "type" "string" "description" "A URL or a local file path (e.g. /agent/state/generated-images/....jpg).")
                               "question" (obj "type" "string" "description" "Optional: what to look for or ask about. Defaults to a general detailed description."))
                          "required" (vector "image"))))))))

(unless (find "find-state-files" *tools*
              :key (lambda (tool) (ref tool "function" "name")) :test #'string=)
  (setf *tools*
        (concatenate 'vector *tools*
          (vector
           (obj "type" "function" "function"
                (obj "name" "find-state-files"
                     "description" "Safely find files already stored under /agent/state without shell commands, guessed Common Lisp wildcards, or unbounded recursion. Returns relative/absolute paths, byte size, and modification time. Use this before upload-reference-image or view-image when you know a file was uploaded but not its exact path."
                     "parameters"
                     (obj "type" "object"
                          "properties"
                          (obj "subdirectory" (obj "type" "string" "description" "Optional relative directory below /agent/state, such as uploads. Defaults to the state root.")
                               "query" (obj "type" "string" "description" "Optional case-insensitive substring matched against the relative path.")
                               "extension" (obj "type" "string" "description" "Optional filename extension such as jpg or .png.")
                               "max_results" (obj "type" "integer" "description" "Maximum returned files, clamped to 1..50. Defaults to 20.")
                               "max_depth" (obj "type" "integer" "description" "Maximum directory depth, clamped to 0..4. Defaults to 3.")))))))))

(defun runware-tool-handle (tool-call)
  (let ((name (ref tool-call "function" "name")))
    (cond
      ((string= name "upload-reference-image")
       (let* ((args (shasht:read-json (ref tool-call "function" "arguments")))
              (path (gethash "path" args))
              (result (upload-reference-image path)))
         (when (fboundp 'log-line)
           (funcall 'log-line "~&  ⤷ [upload-reference-image] ~a~%      => ~a~%" path result))
         (obj "role" "tool" "tool_call_id" (gethash "id" tool-call) "content" result)))
      ((string= name "generate-image")
       (let* ((args (shasht:read-json (ref tool-call "function" "arguments")))
              (prompt (gethash "prompt" args))
              (model (or (gethash "model" args) *runware-default-model*))
              (width (or (gethash "width" args) 1024))
              (height (or (gethash "height" args) 1024))
              (n (or (gethash "number_results" args) 1))
              (refs (gethash "reference_images" args))
              (refs (and (present-p refs) (coerce refs 'list)))
              (result (generate-image prompt :model model :width width :height height
                                      :number-results n :reference-images refs)))
         (if (stringp result)
             (progn
               (when (fboundp 'log-line)
                 (funcall 'log-line "~&  ⤷ [generate-image] ~a~%      => ~a~%" prompt result))
               (obj "role" "tool" "tool_call_id" (gethash "id" tool-call) "content" result))
             (let ((local-paths (mapcar #'%runware-download-to-local result)))
               (when (fboundp 'log-line)
                 (funcall 'log-line "~&  ⤷ [generate-image] ~a~%      => ~{~a~^, ~}~%" prompt result))
               ;; Push into the web terminal if it's loaded -- optional,
               ;; decoupled: generate-image works fine without web-terminal.lisp,
               ;; this just makes the result show up there live too.
               (when (fboundp '%v2-broadcast)
                 (dolist (url result)
                   (ignore-errors (funcall '%v2-broadcast "image" (obj "url" url "alt" prompt)))))
               (obj "role" "tool" "tool_call_id" (gethash "id" tool-call)
                    "content"
                    (if (some #'identity local-paths)
                        (format nil "~{~a~^~%~}~%~%Saved locally -- call view-image on any of these anytime to actually see it:~%~{~a~^~%~}"
                                result (remove nil local-paths))
                        (format nil "~{~a~^~%~}" result)))))))
      ((string= name "view-image")
       (let* ((args (shasht:read-json (ref tool-call "function" "arguments")))
              (image (gethash "image" args))
              (question (gethash "question" args))
              (result (if (and (stringp question) (plusp (length question)))
                          (view-image image question)
                          (view-image image))))
         (when (fboundp 'log-line)
           (funcall 'log-line "~&  ⤷ [view-image] ~a~%      => ~a~%" image result))
         (obj "role" "tool" "tool_call_id" (gethash "id" tool-call) "content" result)))
      ((string= name "find-state-files")
       (let* ((args (shasht:read-json (ref tool-call "function" "arguments")))
              (result (find-state-files
                       :subdirectory (or (gethash "subdirectory" args) "")
                       :query (gethash "query" args)
                       :extension (gethash "extension" args)
                       :max-results (or (gethash "max_results" args) 20)
                       :max-depth (or (gethash "max_depth" args) 3))))
         (when (fboundp 'log-line)
           (funcall 'log-line "~&  [find-state-files] bounded read-only search~%      => ~a~%" result))
         (obj "role" "tool" "tool_call_id" (gethash "id" tool-call)
              "content" result)))
      (t (error "RUNWARE tool port does not own ~a" name)))))

(when (or (not (fboundp 'tool-dispatch-legacy-wrapper-enabled-p))
          (funcall 'tool-dispatch-legacy-wrapper-enabled-p))
  (unless (fboundp 'pai-base-execute-runware)
    (setf (fdefinition 'pai-base-execute-runware) (fdefinition 'execute)))
  (defun execute (tool-call)
    (let ((name (ref tool-call "function" "name")))
      (if (member name '("upload-reference-image" "generate-image" "view-image"
                         "find-state-files") :test #'string=)
          (runware-tool-handle tool-call)
          (pai-base-execute-runware tool-call)))))

;; Keep the explicit work tools adjacent to the existing composed tool owner.
;; The absolute probe is false in isolated contract tests and true in the
;; production image, where Docker copies both files into /agent/state.
(let* ((root (or (sb-ext:posix-getenv "PAI_APPLICATION_ROOT")
                 (sb-ext:posix-getenv "PAI_R3A_APPLICATION_ROOT")))
       (path (if (and root (> (length root) 0))
                 (merge-pathnames "bounded-work-tools.lisp" (pathname root))
                 #P"/agent/state/bounded-work-tools.lisp")))
  (when (probe-file path) (load path)))
