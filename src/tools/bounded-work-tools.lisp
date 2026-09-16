;;;; bounded-work-tools.lisp -- explicit web/document tools for public turns.
;;;;
;;;; These tools replace improvised DEXADOR and filesystem programs sent
;;;; through LISP-EVAL. They have narrow authority, bounded results, and no
;;;; outbound messaging capability: their results return only to AGENT-LOOP.

(in-package :agent)

(export '(web-fetch write-deliverable read-deliverable
          bounded-work-tools-report bounded-work-tool-handle))

(defparameter *deliverable-root*
  (let ((root (or (sb-ext:posix-getenv "PAI_ARTIFACT_ROOT")
                  (sb-ext:posix-getenv "PAI_R3A_ARTIFACT_ROOT"))))
    (if (and root (> (length root) 0))
        (merge-pathnames "deliverables/" (pathname root))
        #P"/agent/state/deliverables/")))
(defparameter *deliverable-write-max-chars* 100000)
(defparameter *deliverable-read-max-chars* 20000)
(defparameter *web-fetch-max-chars* 30000)
(defparameter *web-fetch-timeout-seconds* 15)
(defparameter *deliverable-extensions* '("md" "txt"))

(defvar *bounded-work-tool-stats* (make-hash-table :test #'equal))
(defvar *bounded-web-fetch-fn*
  (lambda (url)
    (dex:get url :connect-timeout *http-connect-timeout*
                 :read-timeout *web-fetch-timeout-seconds*
                 :want-stream nil)))

(defun %bounded-work-stat (name)
  (incf (gethash name *bounded-work-tool-stats* 0)))

(defun %bounded-safe-relative-p (value)
  (and (stringp value) (plusp (length value))
       (not (uiop:absolute-pathname-p (pathname value)))
       (not (find ".." (uiop:split-string value :separator '(#\/ #\\))
                  :test #'string=))))

(defun %bounded-deliverable-path (relative &key must-exist)
  (unless (%bounded-safe-relative-p relative)
    (error "path must be relative below /agent/state/deliverables"))
  (let* ((type (string-downcase (or (pathname-type (pathname relative)) "")))
         (root (uiop:ensure-directory-pathname *deliverable-root*))
         (path (merge-pathnames relative root)))
    (unless (member type *deliverable-extensions* :test #'string=)
      (error "deliverable extension must be one of: ~{.~a~^, ~}"
             *deliverable-extensions*))
    (when (and must-exist (not (probe-file path)))
      (error "deliverable does not exist"))
    path))

(defun %bounded-string (text limit label)
  (if (<= (length text) limit)
      text
      (let* ((notice (format nil "[~a; original characters: ~d]~%"
                             label (length text)))
             (available (max 0 (- limit (length notice) 40)))
             (head (floor (* available 4) 5))
             (tail (- available head)))
        (format nil "~a~a~%[...middle omitted...]~%~a"
                notice (subseq text 0 head)
                (subseq text (- (length text) tail))))))

(defun write-deliverable (relative content)
  "Atomically write one Markdown/text deliverable and return metadata only."
  (handler-case
      (progn
        (unless (stringp content) (error "content must be a string"))
        (when (> (length content) *deliverable-write-max-chars*)
          (error "content exceeds the ~d-character deliverable limit"
                 *deliverable-write-max-chars*))
        (let* ((path (%bounded-deliverable-path relative))
               (tmp (make-pathname :name
                                   (format nil ".~a-tmp" (pathname-name path))
                                   :type (pathname-type path) :defaults path)))
          (ensure-directories-exist path)
          (with-open-file (out tmp :direction :output :if-exists :supersede
                                   :if-does-not-exist :create
                                   :external-format :utf-8)
            (write-string content out)
            (finish-output out))
          (uiop:rename-file-overwriting-target tmp path)
          (%bounded-work-stat "deliverable_writes")
          (shasht:write-json
           (obj "path" (format nil "/agent/state/deliverables/~a" relative)
                "characters" (length content) "written" t) nil)))
    (error (condition)
      (format nil "ERROR: deliverable write failed: ~a" condition))))

(defun read-deliverable (relative)
  "Read one Markdown/text deliverable through a fixed conversational bound."
  (handler-case
      (let* ((path (%bounded-deliverable-path relative :must-exist t))
             (content (uiop:read-file-string path :external-format :utf-8))
             (bounded (%bounded-string content *deliverable-read-max-chars*
                                       "Deliverable preview compacted")))
        (%bounded-work-stat "deliverable_reads")
        (shasht:write-json
         (obj "path" (format nil "/agent/state/deliverables/~a" relative)
              "original_characters" (length content)
              "truncated" (if (> (length content)
                                  *deliverable-read-max-chars*) t nil)
              "content" bounded) nil))
    (error (condition)
      (format nil "ERROR: deliverable read failed: ~a" condition))))

(defun %bounded-web-host (url)
  (let* ((scheme-end (search "://" url))
         (host-start (and scheme-end (+ scheme-end 3)))
         (host-end (and host-start
                        (or (position #\/ url :start host-start)
                            (position #\? url :start host-start)
                            (position #\# url :start host-start)
                            (length url))))
         (authority (and host-end (subseq url host-start host-end)))
         (without-user (and authority
                            (subseq authority
                                    (1+ (or (position #\@ authority :from-end t)
                                            -1)))))
         (host (and without-user
                    (subseq without-user 0
                            (or (position #\: without-user)
                                (length without-user))))))
    (and host (string-downcase host))))

(defun %bounded-public-web-url-p (url)
  (let* ((lower (and (stringp url) (string-downcase url)))
         (host (and lower (%bounded-web-host lower))))
    (and host
         (or (string= "https://" lower :end2 (min 8 (length lower)))
             (string= "http://" lower :end2 (min 7 (length lower))))
         (not (or (string= host "localhost")
                  (and (plusp (length host)) (char= (char host 0) #\[))
                  (string= host "0.0.0.0")
                  (string= host "::1")
                  (search ".local" host :from-end t)
                  (string= "127." host :end2 (min 4 (length host)))
                  (string= "10." host :end2 (min 3 (length host)))
                  (string= "192.168." host :end2 (min 8 (length host)))
                  (string= "169.254." host :end2 (min 8 (length host)))
                  (loop for second from 16 to 31
                        thereis (let ((prefix (format nil "172.~d." second)))
                                  (and (>= (length host) (length prefix))
                                       (string= prefix host
                                                :end2 (length prefix))))))))))

(defun %bounded-strip-html (content)
  (let ((text content))
    (when (find-package :cl-ppcre)
      (setf text (cl-ppcre:regex-replace-all
                  "(?is)<(script|style)[^>]*>.*?</\\1>" text " ")
            text (cl-ppcre:regex-replace-all "(?s)<[^>]+>" text " ")
            text (cl-ppcre:regex-replace-all "[ \\t\\r\\n]+" text " ")))
    text))

(defun web-fetch (url)
  "Fetch one explicitly named public HTTP(S) page and return bounded text."
  (handler-case
      (progn
        (unless (%bounded-public-web-url-p url)
          (error "URL must be public http(s); local/private targets are unavailable"))
        (sb-ext:with-timeout *web-fetch-timeout-seconds*
          (let* ((body (funcall *bounded-web-fetch-fn* url))
                 (text (%bounded-strip-html
                        (if (stringp body) body (format nil "~a" body))))
                 (bounded (%bounded-string text *web-fetch-max-chars*
                                           "Web page compacted")))
            (%bounded-work-stat "web_fetches")
            (shasht:write-json
             (obj "url" url "original_characters" (length text)
                  "truncated" (if (> (length text) *web-fetch-max-chars*) t nil)
                  "content" bounded) nil))))
    (sb-ext:timeout ()
      (format nil "ERROR: web fetch exceeded the ~d-second timeout."
              *web-fetch-timeout-seconds*))
    (error (condition)
      (format nil "ERROR: web fetch failed: ~a" condition))))

(defun bounded-work-tools-report ()
  (obj "schema_version" 1
       "deliverable_root" "/agent/state/deliverables/"
       "deliverable_write_max_characters" *deliverable-write-max-chars*
       "deliverable_read_max_characters" *deliverable-read-max-chars*
       "web_fetch_max_characters" *web-fetch-max-chars*
       "web_fetch_timeout_seconds" *web-fetch-timeout-seconds*
       "web_fetches" (gethash "web_fetches" *bounded-work-tool-stats* 0)
       "deliverable_reads" (gethash "deliverable_reads" *bounded-work-tool-stats* 0)
       "deliverable_writes" (gethash "deliverable_writes" *bounded-work-tool-stats* 0)))

(dolist
    (tool
     (list
      (obj "type" "function" "function"
           (obj "name" "web-fetch"
                "description" "Read the contents of one exact public web page through a bounded fetch. Use this for a URL the operator supplies or a search result you need to inspect; do not improvise HTTP through lisp-eval."
                "parameters"
                (obj "type" "object" "properties"
                     (obj "url" (obj "type" "string" "description"
                                     "The exact public http(s) URL to read."))
                     "required" (vector "url"))))
      (obj "type" "function" "function"
           (obj "name" "write-deliverable"
                "description" "Atomically create or replace a Markdown/text document below /agent/state/deliverables. Use this instead of filesystem Lisp when the operator asks you to create or revise a document."
                "parameters"
                (obj "type" "object" "properties"
                     (obj "path" (obj "type" "string" "description"
                                      "Relative .md or .txt path below the deliverables directory.")
                          "content" (obj "type" "string" "description"
                                         "Complete document content."))
                     "required" (vector "path" "content"))))
      (obj "type" "function" "function"
           (obj "name" "read-deliverable"
                "description" "Read a previously created Markdown/text deliverable through a bounded preview. Use this to review or continue document work instead of filesystem Lisp."
                "parameters"
                (obj "type" "object" "properties"
                     (obj "path" (obj "type" "string" "description"
                                      "Relative .md or .txt path below the deliverables directory."))
                     "required" (vector "path"))))))
  (unless (find (ref tool "function" "name") *tools*
                :key (lambda (candidate) (ref candidate "function" "name"))
                :test #'string=)
    (setf *tools* (concatenate 'vector *tools* (vector tool)))))

(defun bounded-work-tool-handle (tool-call)
  (let* ((name (ref tool-call "function" "name"))
         (args (shasht:read-json (ref tool-call "function" "arguments"))))
    (cond
      ((string= name "web-fetch")
       (obj "role" "tool" "tool_call_id" (gethash "id" tool-call)
            "content" (web-fetch (gethash "url" args))))
      ((string= name "write-deliverable")
       (obj "role" "tool" "tool_call_id" (gethash "id" tool-call)
            "content" (write-deliverable (gethash "path" args)
                                          (gethash "content" args))))
      ((string= name "read-deliverable")
       (obj "role" "tool" "tool_call_id" (gethash "id" tool-call)
            "content" (read-deliverable (gethash "path" args))))
      (t (error "BOUNDED-WORK tool port does not own ~a" name)))))

(when (or (not (fboundp 'tool-dispatch-legacy-wrapper-enabled-p))
          (funcall 'tool-dispatch-legacy-wrapper-enabled-p))
  (unless (fboundp 'pai-base-execute-bounded-work-tools)
    (setf (fdefinition 'pai-base-execute-bounded-work-tools)
          (fdefinition 'execute)))
  (defun execute (tool-call)
    (let ((name (ref tool-call "function" "name")))
      (if (member name '("web-fetch" "write-deliverable" "read-deliverable")
                  :test #'string=)
          (bounded-work-tool-handle tool-call)
          (funcall 'pai-base-execute-bounded-work-tools tool-call)))))
