;;;; web-terminal.lisp -- the sole real-time browser terminal for pAI.
;;;;
;;;; WEB.LISP owns only authenticated acceptor lifecycle. This file owns `/`,
;;;; `/terminal`, `/api/v2/...`, SSE presentation, selected-mind submission and
;;;; the contained file browser. Cold history comes only from pAI event
;;;; authority; no pre-pAI chat history or interaction loop is retained.

(in-package :agent)

(export '(web-terminal-tool-handle web-terminal-configure-submit
          web-terminal-configure-command web-terminal-present-activity
          web-terminal-present-stream-progress
          web-terminal-present-operational-notice))

;;; --- event bus -----------------------------------------------------------

(defparameter *v2-lock* (bt:make-lock "web-terminal"))
(defparameter *v2-next-id* 0)
(defparameter *v2-ring* nil
  "List of broadcast events, NEWEST FIRST, capped at *v2-ring-cap*.")
(defparameter *v2-ring-cap* 2000)
(defparameter *v2-clients* nil
  "List of per-connected-client mailboxes (adjustable vectors, oldest-first,
drained by that client's SSE handler thread).")
(defvar *v2-turn-in-flight* nil)
(defvar *v2-authoritative-history-seeded-p* nil)
(defvar *v2-submit-fn* nil
  "Transport seam configured by the selected cognition runtime. The web
adapter never chooses or reconstructs a conversation loop.")
(defvar *v2-command-fn* nil
  "Operator-command seam configured by the selected runtime. Exact slash
commands cross this deterministic seam instead of entering model context.")

(defun web-terminal-configure-submit (function)
  "Install the one synchronous submission function owned by the selected
mind. FUNCTION receives message text and channel and returns a receipt."
  (unless (functionp function)
    (error "Web terminal submission requires a function"))
  (setf *v2-submit-fn* function)
  (when (fboundp '%v2-seed-authoritative-history)
    (funcall '%v2-seed-authoritative-history))
  t)

(defun web-terminal-configure-command (function)
  "Install the deterministic operator-command function. FUNCTION receives
the complete command line and returns bounded display text."
  (unless (functionp function)
    (error "Web terminal command handling requires a function"))
  (setf *v2-command-fn* function)
  t)

(defun %v2-json (value)
  "Compact (single-line) JSON. SHASHT:WRITE-JSON follows the ambient
CL:*PRINT-PRETTY*, which this image has bound to T -- fine for a plain
HTTP JSON response, but fatal for SSE: the `data:` field is line-based,
so a pretty-printed multi-line body only delivers its first line (just
`{`) to EventSource, and the browser's JSON.parse chokes on it."
  (let ((*print-pretty* nil))
    (shasht:write-json value nil)))

(defun %v2-broadcast (type data &key trace-id turn-id (retain-p t))
  "Deliver an event to every connected client. Retained events also enter the
bounded presentation ring; transient ambient activity deliberately does not."
  (bt:with-lock-held (*v2-lock*)
    (let ((event (obj "id" (incf *v2-next-id*) "ts" (get-universal-time)
                       "type" type "data" data
                       "trace_id" (or trace-id
                                      (and (boundp '*timing-trace-id*)
                                           (stringp (symbol-value '*timing-trace-id*))
                                           (symbol-value '*timing-trace-id*))
                                      :null)
                       "turn_id" (or turn-id
                                     (and (boundp '*timing-turn-id*)
                                          (stringp (symbol-value '*timing-turn-id*))
                                          (symbol-value '*timing-turn-id*))
                                     :null))))
      (when retain-p
        (push event *v2-ring*)
        (when (> (length *v2-ring*) *v2-ring-cap*)
          (setf *v2-ring* (subseq *v2-ring* 0 *v2-ring-cap*))))
      (dolist (mailbox *v2-clients*)
        (vector-push-extend event mailbox))
      event)))

(defun web-terminal-present-activity (data &key private-p turn-id)
  "Present deterministic runtime activity without confusing private cognition
with conversation. Operator-root work remains in the transcript; private-root
work is live-only ambient presentation and is never replayed as chat history."
  (%v2-broadcast (if private-p "ambient" "tool") data
                 :turn-id turn-id :retain-p (not private-p)))

(defun web-terminal-present-stream-progress (data &key turn-id)
  "Present one replaceable, content-free provider-stream progress snapshot.

The browser keys snapshots by generation id and never retains them in chat
history. DATA contains counts and a human label, but no reasoning or response
text."
  (%v2-broadcast "stream-progress" data :turn-id turn-id :retain-p nil))

(defun web-terminal-present-operational-notice (data &key turn-id)
  "Retain an operator-visible runtime anomaly outside the chat transcript.

Identical consecutive notices describe one continuing condition, not new chat
history.  Reuse the retained event until another durable presentation occurs."
  (let ((latest
          (bt:with-lock-held (*v2-lock*)
            (first *v2-ring*))))
    (if (and latest
             (string= "operational" (gethash "type" latest ""))
             (equal data (gethash "data" latest)))
        latest
        (%v2-broadcast "operational" data :turn-id turn-id :retain-p t))))

(defun %v2-condition-summary (condition)
  "Bound an authenticated operator diagnostic without retaining a backtrace."
  (let* ((raw (format nil "~a" condition))
         (flat (substitute #\Space #\Return
                           (substitute #\Space #\Newline raw))))
    (subseq flat 0 (min 1024 (length flat)))))

;;; --- filesystem containment ------------------------------------------------
;;; The HTTP caller is not the agent. File routes are confined to the same
;;; workspace as recursive tools; generated images remain confined to mutable
;;; instance state. The shared adapter policy guards both roots, including
;;; nonexistent write destinations.

(defparameter *v2-state-root*
  (truename (if (fboundp 'pai-state-root)
                (funcall 'pai-state-root)
                #P"/agent/state/")))

(defun %v2-workspace-root ()
  "Return the same workspace root used by the recursive primitive tools.

The tool configuration is authoritative once installed.  The environment and
source-tree fallbacks keep the file browser useful when the web adapter is run
without recursive tools (for example in a local UI qualification process)."
  (let* ((configured
           (or (and (boundp '*recursive-primitive-workspace-root*)
                    (symbol-value '*recursive-primitive-workspace-root*))
               (let ((value (uiop:getenv "PAI_RECURSIVE_WORKSPACE_ROOT")))
                 (and value (plusp (length value)) (probe-file value)))
               (let ((source (and (fboundp 'pai-source-root)
                                  (funcall 'pai-source-root))))
                 (and source
                      (uiop:pathname-parent-directory-pathname source)))
               *v2-state-root*)))
    (truename (uiop:ensure-directory-pathname configured))))

(defun v2-file-safe-path (relpath)
  "Resolve a Files-panel path beneath the active agent workspace."
  (web-resolve-contained-path (%v2-workspace-root) (or relpath "")))

(defun v2-state-safe-path (relpath)
  "Resolve generated-image paths beneath mutable instance state."
  (web-resolve-contained-path *v2-state-root* (or relpath "")))

;;; --- broadcast-image tool --------------------------------------------------
;;; Same check-then-append-onto-*tools* / rename-and-wrap-EXECUTE idiom as
;;; web-search in enhancements.lisp:45-77.

(defun v2-resolve-image-src (raw)
  "HTTP(S) URLs pass through unchanged; an absolute path under /agent/state
is validated and served via /api/v2/image."
  (if (or (and (>= (length raw) 7) (string-equal (subseq raw 0 7) "http://"))
          (and (>= (length raw) 8) (string-equal (subseq raw 0 8) "https://")))
      raw
      (let* ((target (v2-state-safe-path raw))
             (rel (enough-namestring target *v2-state-root*)))
        (format nil "/api/v2/image?path=~a" (url-encode (namestring rel))))))

(unless (find "broadcast-image" *tools*
              :key (lambda (tool) (ref tool "function" "name")) :test #'string=)
  (setf *tools*
        (concatenate 'vector *tools*
          (vector
           (obj "type" "function" "function"
                (obj "name" "broadcast-image"
                     "description" "Send an image to everyone currently viewing the web terminal (/terminal). Provide either an http(s) URL or an absolute path under /agent/state to a local image file."
                     "parameters"
                     (obj "type" "object"
                          "properties"
                          (obj "url" (obj "type" "string"
                                          "description" "http(s) URL, or an absolute path under /agent/state")
                               "alt" (obj "type" "string" "description" "Short caption/alt text"))
                          "required" (vector "url"))))))))

(defun web-terminal-tool-handle (tool-call)
  (let ((name (ref tool-call "function" "name")))
    (if (string= name "broadcast-image")
        (handler-case
            (let* ((args (shasht:read-json (ref tool-call "function" "arguments")))
                   (raw (gethash "url" args))
                   (alt (or (gethash "alt" args) ""))
                   (served (v2-resolve-image-src raw)))
              (%v2-broadcast "image" (obj "url" served "alt" alt))
              (funcall 'log-line "~&  ⤷ [broadcast-image] ~a~%      => sent~%" raw)
              (obj "role" "tool" "tool_call_id" (gethash "id" tool-call)
                   "content" "image broadcast to connected web terminal clients"))
          (error (e)
            (obj "role" "tool" "tool_call_id" (gethash "id" tool-call)
                 "content" (format nil "ERROR: ~a" e))))
        (error "WEB-V2 tool port does not own ~a" name))))

(when (or (not (fboundp 'tool-dispatch-legacy-wrapper-enabled-p))
          (funcall 'tool-dispatch-legacy-wrapper-enabled-p))
  (unless (fboundp 'pai-base-execute-v2)
    (setf (fdefinition 'pai-base-execute-v2) (fdefinition 'execute)))
  (defun execute (tool-call)
    (if (string= (ref tool-call "function" "name") "broadcast-image")
        (web-terminal-tool-handle tool-call)
        (pai-base-execute-v2 tool-call))))

;;; --- multimodal user content -----------------------------------------------
;;; A user message's "content" is either a plain string (text-only, the
;;; original shape everywhere in this codebase) or, for an image-attached
;;; message, an OpenAI/OpenRouter-style vector of parts:
;;;   #(#H(type "text" text "...") #H(type "image_url" image_url #H(url "data:...")))
;;; %V2-USER-DISPLAY normalizes either shape into {"text":..., "images":[...]}
;;; for the client -- used by BOTH the live broadcast (in /api/v2/send) and
;;; the cold-start classifier below, so there is exactly one place that
;;; defines what a "user" event's DATA looks like.

(defun %v2-user-display (content)
  (if (stringp content)
      (obj "text" content "images" (vector))
      (let ((text-parts nil) (images nil))
        (loop for part across content
              do (cond
                   ((string= (gethash "type" part) "text")
                    (push (gethash "text" part) text-parts))
                   ((string= (gethash "type" part) "image_url")
                    (push (gethash "url" (gethash "image_url" part)) images))))
        (obj "text" (format nil "~{~a~^ ~}" (nreverse text-parts))
             "images" (coerce (nreverse images) 'vector)))))

;;; --- authoritative cold-start history -------------------------------------

(defun %v2-selected-agent-id ()
  (cond
    ((and (boundp '*conscious-recursive-mind-agent-id*)
          (stringp (symbol-value '*conscious-recursive-mind-agent-id*)))
     (symbol-value '*conscious-recursive-mind-agent-id*))
    ((and (boundp '*agent-id*) (stringp (symbol-value '*agent-id*)))
     (symbol-value '*agent-id*))
    (t nil)))

(defun %v2-pai-conversation-event (event)
  "Return one terminal presentation tuple for an exact pAI conversation event."
  (when (hash-table-p event)
    (let* ((payload (gethash "payload" event))
           (metadata (and (hash-table-p payload)
                          (gethash "metadata" payload)))
           (source (and (hash-table-p metadata)
                        (gethash "source" metadata "")))
           (type (gethash "type" event ""))
           (agent-id (%v2-selected-agent-id)))
      (when (and (hash-table-p payload)
                 (stringp agent-id)
                 (equal agent-id (gethash "agent_id" event))
                 (member source '("recursive-mind-v1" "q4.5-conversation"
                                  "recursive-curiosity-reach-out-v1")
                         :test #'string=))
        (cond
          ((string= type "user-message")
           (list "user" (%v2-user-display (gethash "text" payload ""))))
          ((string= type "agent-message")
           (list "final" (gethash "text" payload ""))))))))

(defun %v2-seed-authoritative-history ()
  "Seed the bounded presentation ring once from pAI event authority only."
  (let ((seed-p nil))
    (bt:with-lock-held (*v2-lock*)
      (unless *v2-authoritative-history-seeded-p*
        (setf *v2-authoritative-history-seeded-p* t
              seed-p t)))
    (when seed-p
      (handler-case
          (dolist (event (replay-events
                          :types '("user-message" "agent-message")
                          :limit *v2-ring-cap*))
            (let ((presentation (%v2-pai-conversation-event event)))
              (when presentation
                (%v2-broadcast (first presentation) (second presentation)))))
        (error ()
          ;; History presentation may fail closed without affecting admission.
          ;; Permit a later request to retry after authority is ready.
          (bt:with-lock-held (*v2-lock*)
            (setf *v2-authoritative-history-seeded-p* nil)))))))

(defun %v2-ring-page (before limit)
  "Return chat history without allowing operational notices to consume it.

The newest page also carries the latest operational notice for its persistent
shelf.  Older-page cursors and HAS-MORE are based only on transcript events."
  (bt:with-lock-held (*v2-lock*)
    (let* ((operational
             (and (null before)
                  (find "operational" *v2-ring*
                        :key (lambda (event) (gethash "type" event ""))
                        :test #'string=)))
           (transcript
             (remove-if (lambda (event)
                          (string= "operational"
                                   (gethash "type" event "")))
                        *v2-ring*))
           (all (if before
                    (remove-if (lambda (e) (>= (gethash "id" e) before))
                               transcript)
                    transcript))
           (n (length all))
           (page (nreverse (copy-list (subseq all 0 (min limit n))))))
      (values (if operational (append page (list operational)) page)
              (> n limit)))))

;;; --- embedded frontend assets ----------------------------------------------

(defparameter *v2-index-html* (asset "assets/terminal.html")
  "Served page. Lives on disk at assets/terminal.html so it can be edited,
   linted and opened in a browser; embedded here at compile time so the
   standalone image stays self-contained.")

(defparameter *v2-login-html* (asset "assets/login.html"))
(defparameter *v2-viewport-js* (asset "assets/viewport.js"))
(defparameter *v2-manifest* (asset "assets/manifest.webmanifest"))
(defparameter *v2-service-worker* (asset "assets/service-worker.js"))
(defparameter *v2-pwa-icon* (asset "assets/pwa-icon.svg"))

;;; --- routes ----------------------------------------------------------------

(hunchentoot:define-easy-handler (v2-terminal :uri "/terminal") ()
  (setf (hunchentoot:content-type*) "text/html; charset=utf-8"
        ;; The page owns the SSE event vocabulary and renderer.  Reusing an
        ;; old document across a runtime promotion can reconnect successfully
        ;; while silently ignoring newly introduced transient event types.
        (hunchentoot:header-out "Cache-Control") "no-store")
  *v2-index-html*)

(hunchentoot:define-easy-handler (v2-root :uri "/") ()
  (setf (hunchentoot:content-type*) "text/html; charset=utf-8"
        (hunchentoot:header-out "Cache-Control") "no-store")
  *v2-index-html*)

(hunchentoot:define-easy-handler (v2-login :uri "/login") ()
  (setf (hunchentoot:content-type*) "text/html; charset=utf-8")
  *v2-login-html*)

(hunchentoot:define-easy-handler (v2-login-submit :uri "/api/v2/login") ()
  (setf (hunchentoot:content-type*) "application/json; charset=utf-8")
  (handler-case
      (let* ((body (hunchentoot:raw-post-data :force-text t))
             (parsed (shasht:read-json body))
             (username (gethash "username" parsed))
             (password (gethash "password" parsed)))
        (if (web-request-credentials-authorized-p username password)
            (progn
              (hunchentoot:set-cookie
               *web-session-cookie-name*
               :value (%web-session-token)
               :max-age *web-session-cookie-max-age-seconds*
               :path "/" :same-site "Strict" :secure t :http-only t)
              (%v2-json (obj "ok" t "redirect" "/terminal")))
            (progn
              (setf (hunchentoot:return-code*) 401)
              (%v2-json (obj "ok" nil "error" "sign-in failed")))))
    (error ()
      (setf (hunchentoot:return-code*) 400)
      (%v2-json (obj "ok" nil "error" "sign-in failed")))))

(hunchentoot:define-easy-handler (v2-manifest :uri "/manifest.webmanifest") ()
  (setf (hunchentoot:content-type*) "application/manifest+json; charset=utf-8")
  *v2-manifest*)

(hunchentoot:define-easy-handler (v2-viewport :uri "/viewport.js") ()
  (setf (hunchentoot:content-type*) "text/javascript; charset=utf-8")
  (setf (hunchentoot:header-out "Cache-Control") "no-store")
  *v2-viewport-js*)

(hunchentoot:define-easy-handler (v2-service-worker :uri "/service-worker.js") ()
  (setf (hunchentoot:content-type*) "text/javascript; charset=utf-8")
  (setf (hunchentoot:header-out "Service-Worker-Allowed") "/")
  *v2-service-worker*)

(hunchentoot:define-easy-handler (v2-pwa-icon :uri "/pwa-icon.svg") ()
  (setf (hunchentoot:content-type*) "image/svg+xml; charset=utf-8")
  *v2-pwa-icon*)

(hunchentoot:define-easy-handler (v2-history :uri "/api/v2/history") (before limit)
  (setf (hunchentoot:content-type*) "application/json; charset=utf-8")
  (%v2-seed-authoritative-history)
  (let* ((before-n (and before (ignore-errors (parse-integer before))))
         (limit-n (or (and limit (ignore-errors (parse-integer limit))) 200)))
    (multiple-value-bind (events has-more)
        (%v2-ring-page before-n limit-n)
      (%v2-json
       (obj "events" (coerce events 'vector)
            "has_more" (if has-more t nil)
            "turn_in_flight" (if *v2-turn-in-flight* t nil))))))

(hunchentoot:define-easy-handler (v2-stream :uri "/api/v2/stream") (since)
  (%v2-seed-authoritative-history)
  (setf (hunchentoot:content-type*) "text/event-stream; charset=utf-8")
  (setf (hunchentoot:header-out "Cache-Control") "no-cache")
  (let* ((since-id (or (and since (ignore-errors (parse-integer since))) 0))
         (raw-stream (hunchentoot:send-headers))
         (stream (flexi-streams:make-flexi-stream raw-stream :external-format :utf-8))
         (mailbox (make-array 0 :adjustable t :fill-pointer 0)))
    (flet ((write-event (event)
             (format stream "id: ~a~%data: ~a~%~%"
                     (gethash "id" event) (%v2-json event))
             (finish-output stream)))
      (handler-case
          (progn
            (bt:with-lock-held (*v2-lock*)
              (dolist (event (reverse (remove-if (lambda (e) (<= (gethash "id" e) since-id))
                                                  *v2-ring*)))
                (write-event event))
              (push mailbox *v2-clients*))
            (unwind-protect
                (loop
                  (sleep 0.3)
                  (let (pending)
                    (bt:with-lock-held (*v2-lock*)
                      (setf pending (coerce mailbox 'list))
                      (setf (fill-pointer mailbox) 0))
                    (dolist (event pending) (write-event event))))
              (bt:with-lock-held (*v2-lock*)
                (setf *v2-clients* (remove mailbox *v2-clients*)))))
        (error () nil))))
  nil)

(hunchentoot:define-easy-handler (v2-send :uri "/api/v2/send") ()
  (setf (hunchentoot:content-type*) "application/json; charset=utf-8")
  (handler-case
      (let* ((body (hunchentoot:raw-post-data :force-text t))
             (parsed (shasht:read-json body))
             (text (or (gethash "message" parsed) ""))
             (images (gethash "images" parsed))
             (has-images (and (present-p images) (plusp (length images)))))
        (when has-images
          (error "image input is not admitted by the conscious interaction service"))
        (if (and (plusp (length text)) (char= (char text 0) #\/))
            (progn
              (unless (functionp *v2-command-fn*)
                (error "web terminal command service is unavailable"))
              (let ((display (funcall *v2-command-fn* text)))
                (unless (and (stringp display) (plusp (length display)))
                  (error "web terminal command returned no display text"))
                (%v2-broadcast "user" (%v2-user-display text))
                (%v2-broadcast "command" display)
                (%v2-json (obj "ok" t "command" text))))
            (progn
              (unless (functionp *v2-submit-fn*)
                (error "web terminal submission service is unavailable"))
              (setf *v2-turn-in-flight* t)
              (unwind-protect
                   (let ((receipt (funcall *v2-submit-fn* text "web")))
                     (unless (hash-table-p receipt)
                       (error "web terminal submission returned no receipt"))
                     (%v2-json (obj "ok" t "receipt" receipt)))
                (setf *v2-turn-in-flight* nil)))))
    (error (e)
      (let ((failure
              (obj "status" "failed"
                   "error_code" "web-interaction-submit-failed"
                   "condition_type" (format nil "~a" (type-of e))
                   "reason" (%v2-condition-summary e))))
        ;; This covers adapter failures outside the selected mind. Normal
        ;; recursive failures arrive through its observer and do not reach
        ;; this handler.
        (%v2-broadcast "error" failure)
        (format *error-output* "~&[web interaction failed] ~a: ~a~%"
                (gethash "condition_type" failure)
                (gethash "reason" failure))
        (finish-output *error-output*))
      (setf (hunchentoot:return-code*) 400)
      (%v2-json (obj "ok" nil "error" "interaction request rejected")))))

(hunchentoot:define-easy-handler (v2-files :uri "/api/v2/files") (path)
  (setf (hunchentoot:content-type*) "application/json; charset=utf-8")
  (handler-case
      (let* ((root (%v2-workspace-root))
             (dir (v2-file-safe-path (or path ""))))
        (unless (uiop:directory-exists-p dir)
          (error "not a directory"))
        (let ((entries
                (append
                 (loop for d in (uiop:subdirectories dir)
                       collect (obj "name" (car (last (pathname-directory d)))
                                    "is_dir" t "size" 0
                                    "mtime" (file-write-date d)))
                 (loop for f in (uiop:directory-files dir)
                       collect (obj "name" (file-namestring f)
                                    "is_dir" nil
                                    "size" (with-open-file (s f :element-type '(unsigned-byte 8))
                                             (file-length s))
                                    "mtime" (file-write-date f))))))
          (%v2-json
           (obj "path" (or path "")
                "root" (namestring root)
                "can_write" (if (web-file-mutation-authorized-p) t nil)
                "entries" (coerce entries 'vector)))))
    (error (e)
      (setf (hunchentoot:return-code*) 400)
      (%v2-json (obj "error" (format nil "~a" e))))))

(defun %v2-file-preview (target)
  "Bounded UTF-8 preview. Large and binary files remain downloadable."
  (with-open-file (stream target :element-type '(unsigned-byte 8))
    (let ((size (file-length stream)))
      (if (> size 2000000)
          (obj "content" "" "editable" nil "truncated" t "size" size)
          (let* ((bytes (make-array size :element-type '(unsigned-byte 8)))
                 (count (read-sequence bytes stream))
                 (text (handler-case
                           (babel:octets-to-string bytes :end count :encoding :utf-8)
                         (error () nil)))
                 (text-p (and text (notany (lambda (c)
                                            (and (< (char-code c) 32)
                                                 (not (find c '(#\Tab #\Newline #\Return)))))
                                          text))))
            (obj "content" (if text-p text "") "editable" (if text-p t nil)
                 "truncated" nil "size" size))))))

(hunchentoot:define-easy-handler (v2-file :uri "/api/v2/file") (path)
  (setf (hunchentoot:content-type*) "application/json; charset=utf-8")
  (unless (member (hunchentoot:request-method*) '(:get :post))
    (setf (hunchentoot:return-code*) 405)
    (return-from v2-file (%v2-json (obj "error" "GET or POST required"))))
  (if (and (eq (hunchentoot:request-method*) :post)
           (not (web-file-mutation-authorized-p)))
      (progn
        (setf (hunchentoot:return-code*) 403)
        (%v2-json (obj "ok" nil "error" "file mutation is disabled")))
      (handler-case
          (if (eq (hunchentoot:request-method*) :post)
          (let* ((body (hunchentoot:raw-post-data :force-text t))
                 (parsed (shasht:read-json body))
                 (rel (gethash "path" parsed))
                 (content (gethash "content" parsed))
                 (target (v2-file-safe-path rel)))
            (unless (and (stringp content) (<= (length content) 2000000)
                         (<= (length (babel:string-to-octets content :encoding :utf-8)) 2000000))
              (error "text must be at most 2 MB in UTF-8"))
            ;; Never let a partial or binary preview replace the original.
            (when (probe-file target)
              (unless (gethash "editable" (%v2-file-preview target))
                (error "this file is download-only")))
            (with-open-file (stream target :direction :output :if-exists :supersede
                                          :if-does-not-exist :create :external-format :utf-8)
              (write-string content stream))
            (%v2-json (obj "ok" t)))
          (let ((preview (%v2-file-preview (v2-file-safe-path (or path "")))))
            (setf (gethash "path" preview) (or path "")
                  (gethash "can_write" preview) (if (web-file-mutation-authorized-p) t nil))
            (%v2-json preview)))
        (error (e)
          (setf (hunchentoot:return-code*) 400)
          (%v2-json (obj "error" (format nil "~a" e)))))))

(hunchentoot:define-easy-handler (v2-upload :uri "/api/v2/upload") (path)
  (setf (hunchentoot:content-type*) "application/json; charset=utf-8")
  (unless (eq (hunchentoot:request-method*) :post)
    (setf (hunchentoot:return-code*) 405)
    (return-from v2-upload (%v2-json (obj "error" "POST required"))))
  (if (not (web-file-mutation-authorized-p))
      (progn
        (setf (hunchentoot:return-code*) 403)
        (%v2-json (obj "ok" nil "error" "file mutation is disabled")))
      (handler-case
          (let* ((file-param (hunchentoot:post-parameter "file"))
             (tmp-path (first file-param))
             (filename (second file-param)))
        (unless (and tmp-path filename (plusp (length filename))
                     (not (find #\/ filename)) (not (find #\\ filename))
                     (not (search ".." filename)))
          (error "invalid filename"))
        (let* ((dir (v2-file-safe-path (or path "")))
               (dest (v2-file-safe-path
                      (namestring
                       (merge-pathnames filename
                                        (uiop:ensure-directory-pathname dir))))))
          (unless (uiop:directory-exists-p dir) (error "not a directory"))
          (when (probe-file dest) (error "a file with that name already exists"))
          (with-open-file (s tmp-path :element-type '(unsigned-byte 8))
            (when (> (file-length s) 20000000) (error "upload limit is 20 MB")))
          (uiop:copy-file tmp-path dest)
          (%v2-json (obj "ok" t "path" (namestring dest)))))
        (error (e)
          (setf (hunchentoot:return-code*) 400)
          (%v2-json (obj "error" (format nil "~a" e)))))))

(hunchentoot:define-easy-handler (v2-download :uri "/api/v2/download") (path)
  (handler-case
      (let ((target (v2-file-safe-path (or path ""))))
        ;; A fixed disposition avoids reflecting a filename into HTTP headers.
        ;; The same-origin download link supplies the user's filename.
        (setf (hunchentoot:header-out :content-disposition) "attachment"
              (hunchentoot:header-out :x-content-type-options) "nosniff")
        (hunchentoot:handle-static-file target "application/octet-stream"))
    (error () (setf (hunchentoot:return-code*) 404) "file not found")))

(hunchentoot:define-easy-handler (v2-image :uri "/api/v2/image") (path)
  (handler-case
      (let* ((target (v2-state-safe-path (or path "")))
             (ext (string-downcase (or (pathname-type target) "")))
             (ctype (cond ((string= ext "png") "image/png")
                          ((member ext '("jpg" "jpeg") :test #'string=) "image/jpeg")
                          ((string= ext "gif") "image/gif")
                          ((string= ext "svg") "image/svg+xml")
                          ((string= ext "webp") "image/webp")
                          (t "application/octet-stream"))))
        (hunchentoot:handle-static-file target ctype))
    (error (e)
      (declare (ignore e))
      (setf (hunchentoot:return-code*) 404)
      "not found")))

(defun start-web-terminal ()
  "Idempotent sanity check -- routes self-register on LOAD via
define-easy-handler, this exists only to confirm from lisp-eval that
web.lisp's acceptor is actually up."
  (if (boundp '*acceptor*)
      (format t "~&web-terminal routes are live on the existing acceptor (port unchanged). Visit /terminal.~%")
      (format t "~&WARNING: *acceptor* not bound -- call (start-web) from web.lisp first.~%")))
