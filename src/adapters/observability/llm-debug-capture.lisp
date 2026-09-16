;;;; llm-debug-capture.lisp -- bounded private request/response diagnostics.
(in-package :agent)
(export '(llm-debug-capture-call llm-debug-capture-index
          llm-debug-capture-read llm-debug-capture-prune
          llm-debug-capture-find-model-call
          llm-debug-current-public-context
          *llm-debug-capture-mode*))

(defun %llm-debug-capture-parse-mode (value)
  (let ((normalized (string-downcase (or value "off"))))
    (cond ((member normalized '("" "off" "0" "false") :test #'string=)
           :off)
          ((string= normalized "metadata") :metadata)
          ((member normalized '("full" "on" "1" "true") :test #'string=)
           :full)
          (t (error "PAI_CONTEXT_TRACE must be off, metadata, or full")))))

(defvar *llm-debug-capture-mode*
  (%llm-debug-capture-parse-mode
   (or (uiop:getenv "PAI_CONTEXT_TRACE")
       (uiop:getenv "PAI_LLM_DEBUG_MODE"))))
(defvar *llm-debug-capture-loaded-p* t)
(defvar *llm-debug-capture-directory*
  (pathname
   (or (uiop:getenv "PAI_CONTEXT_TRACE_DIR")
       (uiop:getenv "PAI_LLM_DEBUG_DIR")
       (let ((root (or (uiop:getenv "PAI_ARTIFACT_ROOT")
                       (uiop:getenv "PAI_R3A_ARTIFACT_ROOT")
                       (uiop:getenv "PAI_STATE_ROOT"))))
         (and root (plusp (length root))
              (namestring (merge-pathnames "private-diagnostics/llm-debug/"
                                           (uiop:ensure-directory-pathname
                                            (pathname root))))))
       "/agent/state/llm-debug/")))
(defvar *llm-debug-capture-retention-seconds* (* 24 60 60))
(defvar *llm-debug-capture-max-file-bytes* (* 100 1024))
(defvar *llm-debug-capture-max-line-chars* 20000)
(defvar *llm-debug-capture-current-file* nil)
(defvar *llm-debug-capture-sequence* 0)
(defvar *llm-debug-current-public-context* nil)
(defparameter *llm-debug-current-context-max-json-chars* 8000000)
(defvar *timing-turn-id* nil)
(defvar *llm-debug-capture-lock*
  (bt:make-lock "llm-debug-capture"))

(defun %llm-debug-enabled-p ()
  (member *llm-debug-capture-mode* '(:metadata :full :on)))

(defun %llm-debug-full-p ()
  (member *llm-debug-capture-mode* '(:full :on)))

(defun %llm-debug-mode-name ()
  (if (%llm-debug-full-p) "full" "metadata"))

(defun %llm-debug-message-characters (messages)
  (reduce #'+ messages
          :key (lambda (message)
                 (let ((content
                         (and (hash-table-p message)
                              (gethash "content" message))))
                   (if (stringp content) (length content) 0)))
          :initial-value 0))

(defun %llm-debug-id ()
  (format nil "llm-~d-~d" (get-universal-time)
          (incf *llm-debug-capture-sequence*)))

(defun %llm-debug-sensitive-key-p (key)
  (member (string-downcase (format nil "~a" key))
          '("authorization" "api_key" "apikey" "api-key" "token"
            "access_token" "refresh_token" "password" "secret")
          :test #'string=))

(defun %llm-debug-token-end-p (character)
  (or (null character)
      (find character '(#\Space #\Tab #\Newline #\Return #\" #\' #\\
                        #\, #\} #\] #\: #\;))))

(defun %llm-debug-redact-prefix (text prefix &key retain-prefix)
  (loop with cursor = 0
        with output = (make-string-output-stream)
        for start = (search prefix text :start2 cursor :test #'char-equal)
        do (if (null start)
               (progn (write-string text output :start cursor)
                      (return (get-output-stream-string output)))
               (let ((end (loop for index from (+ start (length prefix))
                                while (and (< index (length text))
                                           (not (%llm-debug-token-end-p
                                                 (char text index))))
                                finally (return index))))
                 (write-string text output :start cursor :end start)
                 (when retain-prefix (write-string prefix output))
                 (write-string "[REDACTED]" output)
                 (setf cursor end)))))

(defun %llm-debug-redact-string (value)
  (%llm-debug-redact-prefix
   (%llm-debug-redact-prefix value "sk-") "Bearer " :retain-prefix t))

(defun %llm-debug-sanitize (value &optional key)
  (cond
    ((%llm-debug-sensitive-key-p key) "[REDACTED]")
    ((stringp value) (%llm-debug-redact-string value))
    ((hash-table-p value)
     (let ((copy (make-hash-table :test #'equal)))
       (maphash (lambda (child-key child-value)
                  (setf (gethash child-key copy)
                        (%llm-debug-sanitize child-value child-key)))
                value)
       copy))
    ((vectorp value)
     (map 'vector (lambda (item) (%llm-debug-sanitize item)) value))
    ((listp value)
     (mapcar (lambda (item) (%llm-debug-sanitize item)) value))
    (t value)))

(defun %llm-debug-json-line (value)
  (let ((*print-pretty* nil))
    (concatenate 'string (shasht:write-json (%llm-debug-sanitize value) nil)
                 (string #\Newline))))

(defun %llm-debug-file-sequence (path)
  ;; FILE-WRITE-DATE is only second-granular on some filesystems.  A large
  ;; capture can rotate through several shards in that second, so use the
  ;; writer's numeric filename suffix as the deterministic tie-breaker.
  (let* ((name (file-namestring path))
         (end (or (search ".jsonl" name :from-end t) (length name)))
         (dash (position #\- name :from-end t :end end)))
    (or (and dash
             (parse-integer name :start (1+ dash) :end end
                                 :junk-allowed t))
        0)))

(defun %llm-debug-files ()
  (sort (directory (merge-pathnames "llm-debug-*.jsonl"
                                    *llm-debug-capture-directory*))
        (lambda (left right)
          (let ((left-date (file-write-date left))
                (right-date (file-write-date right)))
            (if (= left-date right-date)
                (> (%llm-debug-file-sequence left)
                   (%llm-debug-file-sequence right))
                (> left-date right-date))))))

(defun llm-debug-capture-prune ()
  "Delete only capture shards older than the configured retention window."
  (let ((cutoff (- (get-universal-time)
                   *llm-debug-capture-retention-seconds*)))
    (dolist (path (%llm-debug-files))
      (when (<= (file-write-date path) cutoff)
        (when (and *llm-debug-capture-current-file*
                   (equal (namestring path)
                          (namestring *llm-debug-capture-current-file*)))
          (setf *llm-debug-capture-current-file* nil))
        (delete-file path)))))

(defun %llm-debug-file-size (path)
  (if (and path (probe-file path))
      (with-open-file (stream path :direction :input
                                   :element-type '(unsigned-byte 8))
        (file-length stream))
      0))

(defun %llm-debug-new-file ()
  (ensure-directories-exist
   (merge-pathnames "placeholder" *llm-debug-capture-directory*))
  (setf *llm-debug-capture-current-file*
        (merge-pathnames
         (format nil "llm-debug-~d-~d.jsonl" (get-universal-time)
                 (incf *llm-debug-capture-sequence*))
         *llm-debug-capture-directory*)))

(defun %llm-debug-append-line (line)
  (when (or (null *llm-debug-capture-current-file*)
            (> (+ (%llm-debug-file-size *llm-debug-capture-current-file*)
                  (length line))
               *llm-debug-capture-max-file-bytes*))
    (%llm-debug-new-file))
  (with-open-file (stream *llm-debug-capture-current-file*
                          :direction :output :if-exists :append
                          :if-does-not-exist :create
                          :external-format :utf-8)
    (write-string line stream)
    (finish-output stream)))

(defun %llm-debug-write (record)
  (bt:with-lock-held (*llm-debug-capture-lock*)
    (llm-debug-capture-prune)
    (let ((line (%llm-debug-json-line record)))
      (if (<= (length line) *llm-debug-capture-max-line-chars*)
          (%llm-debug-append-line line)
          (let* ((id (gethash "id" record))
                 (payload (let ((*print-pretty* nil))
                            (shasht:write-json
                             (%llm-debug-sanitize record) nil)))
                 (chunk-size (floor *llm-debug-capture-max-line-chars* 2))
                 (count (ceiling (length payload) chunk-size)))
            (%llm-debug-append-line
             (%llm-debug-json-line
              (obj "id" id "kind" "chunk-header" "chunk_count" count
                   "original_chars" (length payload))))
            (loop for index below count
                  for start = (* index chunk-size)
                  for end = (min (length payload) (+ start chunk-size))
                  do (%llm-debug-append-line
                      (%llm-debug-json-line
                       (obj "id" id "kind" "chunk" "chunk_index" index
                            "chunk_count" count
                            "payload_chunk" (subseq payload start end))))))))))

(defun %llm-debug-safe-write (record)
  ;; Diagnostics must never become a new availability dependency for chat.
  (handler-case (%llm-debug-write record)
    (error (condition)
      (format t "~&[llm-debug-capture] write failed: ~a~%" condition)
      nil)))

(defun %llm-debug-remember-public-context (messages purpose)
  "Keep one credential-redacted, in-memory snapshot of the latest public call."
  (when (string= (or purpose "") "public")
    (let* ((sanitized (%llm-debug-sanitize messages))
           (encoded (shasht:write-json sanitized nil))
           (record
             (if (<= (length encoded)
                     *llm-debug-current-context-max-json-chars*)
                 (obj "schema_version" 1 "status" "available"
                      "captured_at" (get-universal-time)
                      "purpose" "public"
                      "credential_redaction_boundary" t
                      "json_chars" (length encoded)
                      "messages" sanitized)
                 (obj "schema_version" 1 "status" "unavailable"
                      "captured_at" (get-universal-time)
                      "purpose" "public"
                      "reason" "snapshot-exceeds-private-admin-limit"
                      "json_chars" (length encoded)))))
      (bt:with-lock-held (*llm-debug-capture-lock*)
        (setf *llm-debug-current-public-context* record)))))

(defun llm-debug-current-public-context ()
  (bt:with-lock-held (*llm-debug-capture-lock*)
    (or *llm-debug-current-public-context*
        (obj "schema_version" 1 "status" "unavailable"
             "reason" "no-public-inference-since-process-start"))))

(defun llm-debug-capture-call (messages purpose thunk &key metadata)
  "Run THUNK and optionally capture metadata or its private request/result."
  (%llm-debug-remember-public-context messages purpose)
  (if (not (%llm-debug-enabled-p))
      (funcall thunk)
      (let* ((id (bt:with-lock-held (*llm-debug-capture-lock*)
                   (%llm-debug-id)))
             (started (get-universal-time))
             (provider-request nil)
             (request-observer-symbol
               (intern "*CONSCIOUS-CONVERSATION-PROVIDER-REQUEST-OBSERVER*"
                       :agent))
             (base (obj "id" id "kind" "model-call"
                        "schema_version" 2
                        "capture_mode" (%llm-debug-mode-name)
                        "started_at" started
                        "purpose" (or purpose "unknown")
                        "turn_id" (if (boundp '*timing-turn-id*)
                                      (or *timing-turn-id* :null) :null)
                        "message_count" (length messages)
                        "message_characters"
                        (%llm-debug-message-characters messages)
                        "request_metadata" (or metadata (obj)))))
        (when (%llm-debug-full-p)
          (setf (gethash "messages" base) messages))
        ;; The runtime owns request construction.  This dynamically scoped
        ;; observation port lets it hand the actual JSON body back to the
        ;; adapter without creating an adapter -> mind compile-time edge.
        (progv
            (list request-observer-symbol)
            (list (and (%llm-debug-full-p)
                       (lambda (payload) (setf provider-request payload))))
          (handler-case
              (let ((response (funcall thunk)))
                (setf (gethash "finished_at" base) (get-universal-time)
                      (gethash "status" base) "ok")
                (when (%llm-debug-full-p)
                  (when provider-request
                    (setf (gethash "request" base) provider-request))
                  (setf (gethash "response" base) response))
                (%llm-debug-safe-write base)
                response)
            (error (condition)
              (setf (gethash "finished_at" base) (get-universal-time)
                    (gethash "status" base) "error"
                    (gethash "error_type" base) (type-of condition))
              (when (%llm-debug-full-p)
                (when provider-request
                  (setf (gethash "request" base) provider-request))
                (setf (gethash "error" base) (princ-to-string condition)))
              (%llm-debug-safe-write base)
              (error condition)))))))

(defun llm-debug-capture-index ()
  (coerce
   (mapcar (lambda (path)
             (obj "name" (file-namestring path)
                  "bytes" (%llm-debug-file-size path)
                  "modified_at" (file-write-date path)))
           (%llm-debug-files))
   'vector))

(defun %llm-debug-safe-path (name)
  (when (and (stringp name)
             (zerop (or (search "llm-debug-" name :test #'char=) -1))
             (uiop:string-suffix-p name ".jsonl")
             (null (position #\/ name))
             (null (position #\\ name))
             (null (search ".." name)))
    (let ((path (merge-pathnames name *llm-debug-capture-directory*)))
      (and (probe-file path) path))))

(defun llm-debug-capture-read (name &key reveal)
  (let ((path (%llm-debug-safe-path name)))
    (unless path (error "Unknown debug capture file"))
    (with-open-file (stream path :direction :input :external-format :utf-8)
      (coerce
       (loop for line = (read-line stream nil nil)
             while line
             for row = (shasht:read-json line)
             do (unless reveal
                  (dolist (key '("request" "messages" "response"
                                 "payload_chunk" "error"))
                    (remhash key row)))
             collect row)
       'vector))))

(defun %llm-debug-model-call-id-valid-p (value)
  (and (stringp value)
       (<= 1 (length value) 512)
       (every (lambda (character)
                (or (alphanumericp character)
                    (find character "-_.:" :test #'char=)))
              value)))

(defun %llm-debug-record-model-call-id (record)
  (let ((metadata (and (hash-table-p record)
                       (gethash "request_metadata" record))))
    (and (hash-table-p metadata)
         (gethash "model_call_id" metadata))))

(defun %llm-debug-project-record (record reveal)
  (let ((copy (%llm-debug-sanitize record)))
    (unless reveal
      (dolist (key '("request" "messages" "response"
                     "payload_chunk" "error"))
        (remhash key copy)))
    copy))

(defun llm-debug-capture-find-model-call (model-call-id &key reveal)
  "Find one exact model call across rotated and chunked capture shards.

The capture writer may rotate between a chunk header and its final chunk, so
this deliberately streams all retained shards in chronological order.  It
retains only one in-progress chunked record and the matching result."
  (unless (%llm-debug-model-call-id-valid-p model-call-id)
    (error "A safe model_call_id is required"))
  (let ((chunk-id nil)
        (chunk-count 0)
        (chunks nil)
        (found nil))
    (labels ((consider (record)
               (when (and (hash-table-p record)
                          (equal model-call-id
                                 (%llm-debug-record-model-call-id record)))
                 (setf found (%llm-debug-project-record record reveal))))
             (finish-chunks ()
               (when (and chunk-id (= (length chunks) chunk-count))
                 (let ((ordered (sort chunks #'< :key #'car)))
                   (consider
                    (shasht:read-json
                     (with-output-to-string (stream)
                       (dolist (entry ordered)
                         (write-string (cdr entry) stream)))))))
               (setf chunk-id nil chunk-count 0 chunks nil)))
      (dolist (path (reverse (%llm-debug-files)))
        (with-open-file (stream path :direction :input
                                     :external-format :utf-8)
          (loop for line = (read-line stream nil nil)
                while line
                for row = (shasht:read-json line)
                for kind = (gethash "kind" row "")
                do (cond
                     ((string= kind "chunk-header")
                      (finish-chunks)
                      (setf chunk-id (gethash "id" row)
                            chunk-count (gethash "chunk_count" row 0)
                            chunks nil))
                     ((and chunk-id
                           (string= kind "chunk")
                           (equal chunk-id (gethash "id" row)))
                      (push (cons (gethash "chunk_index" row)
                                  (gethash "payload_chunk" row ""))
                            chunks)
                      (when (= (length chunks) chunk-count)
                        (finish-chunks)))
                     (t
                      (finish-chunks)
                      (consider row))))))
      (finish-chunks))
    (or found
        (obj "schema_version" 1 "status" "not-found"
             "model_call_id" model-call-id
             "reason" "capture-expired-disabled-or-not-yet-written"))))
