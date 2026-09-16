;;;; public-read-only-replay.lisp -- aligned, captured-output public replay.

(in-package :agent)

(export '(public-read-only-replay-tools
          public-read-only-replay-align-request
          public-read-only-replay-step
          public-read-only-replay-report))

(defparameter *public-read-only-replay-tool-names*
  '("search-memory" "read-deliverable"))

(defvar *public-read-only-replay-aligned* 0)
(defvar *public-read-only-replay-continuations* 0)
(defvar *public-read-only-replay-finals* 0)
(defvar *public-read-only-replay-rejections* 0)

(defun %public-read-only-replay-json-copy (value)
  (shasht:read-json (shasht:write-json value nil)))

(defun %public-read-only-replay-nonempty-string-p (value)
  (and (stringp value)
       (plusp (length (string-trim '(#\Space #\Tab #\Newline #\Return)
                                    value)))))

(defun %public-read-only-replay-list (value)
  (cond ((or (null value) (eq value :null)) nil)
        ((vectorp value) (coerce value 'list))
        ((listp value) value)
        (t (list value))))

(defun %public-read-only-replay-tool-name (tool)
  (and (hash-table-p tool)
       (let ((function (gethash "function" tool)))
         (and (hash-table-p function) (gethash "name" function)))))

(defun public-read-only-replay-tools (&optional
                                        (tools (and (boundp '*tools*) *tools*)))
  "Return exact production schemas for the two allowed read-only tools."
  (unless (vectorp tools) (error "The production tool registry is unavailable."))
  (let ((selected
          (loop for name in *public-read-only-replay-tool-names*
                for matches =
                  (loop for tool across tools
                        when (string= name
                                      (or (%public-read-only-replay-tool-name tool)
                                          ""))
                          collect tool)
                do (unless (= 1 (length matches))
                     (error "Expected exactly one production schema for ~a." name))
                collect (%public-read-only-replay-json-copy (first matches)))))
    (coerce selected 'vector)))

(defun %public-read-only-replay-messages (request)
  (unless (hash-table-p request) (error "Replay request must be an object."))
  (let ((messages (%public-read-only-replay-list (gethash "messages" request))))
    (unless messages (error "Replay request contains no messages."))
    (unless (= 1 (count "system" messages :test #'string=
                        :key (lambda (message)
                               (and (hash-table-p message)
                                    (gethash "role" message "")))))
      (error "Replay request must contain exactly one system message."))
    messages))

(defun public-read-only-replay-align-request (request &optional tools)
  "Copy REQUEST and align its API schemas with its advertised read-only tools."
  (let* ((copy (%public-read-only-replay-json-copy request))
         (messages (%public-read-only-replay-messages copy))
         (system (find "system" messages :test #'string=
                       :key (lambda (message) (gethash "role" message ""))))
         (content (gethash "content" system ""))
         (allowed (public-read-only-replay-tools
                   (or tools (and (boundp '*tools*) *tools*)))))
    (dolist (name *public-read-only-replay-tool-names*)
      (unless (and (stringp content) (search name content :test #'char-equal))
        (error "System message does not advertise required tool ~a." name)))
    (setf (gethash "tools" copy) allowed)
    (remhash "tool_choice" copy)
    (incf *public-read-only-replay-aligned*)
    copy))

(defun %public-read-only-replay-call (response)
  (unless (hash-table-p response) (error "Captured response must be an object."))
  (let ((choices (%public-read-only-replay-list (gethash "choices" response))))
    (unless (= 1 (length choices))
      (error "Captured response must contain exactly one choice."))
    (let* ((choice (first choices))
           (message (and (hash-table-p choice) (gethash "message" choice))))
      (unless (hash-table-p message)
        (error "Captured response choice contains no message."))
      (values message
              (%public-read-only-replay-list (gethash "tool_calls" message))
              (gethash "finish_reason" choice)))))

(defun %public-read-only-replay-arguments (tool-call)
  (unless (hash-table-p tool-call) (error "Tool call must be an object."))
  (let ((id (gethash "id" tool-call))
        (type (gethash "type" tool-call))
        (function (gethash "function" tool-call)))
    (unless (and (%public-read-only-replay-nonempty-string-p id)
                 (string= type "function")
                 (hash-table-p function)
                 (%public-read-only-replay-nonempty-string-p
                  (gethash "name" function))
                 (stringp (gethash "arguments" function)))
      (error "Tool call is missing request-required fields."))
    (let ((name (gethash "name" function))
          (arguments
            (handler-case (shasht:read-json (gethash "arguments" function))
              (error () (error "Tool-call arguments are not valid JSON.")))))
      (unless (member name *public-read-only-replay-tool-names* :test #'string=)
        (error "Tool ~a is outside the read-only replay boundary." name))
      (unless (hash-table-p arguments)
        (error "Tool-call arguments must be an object."))
      (values id name arguments))))

(defun %public-read-only-replay-validate-search (arguments content)
  (let ((query (gethash "query" arguments))
        (limit (gethash "limit" arguments 3)))
    (unless (and (%public-read-only-replay-nonempty-string-p query)
                 (<= (length query) 1000)
                 (integerp limit) (<= 1 limit 5))
      (error "search-memory arguments are outside the bounded contract.")))
  (let ((payload (handler-case (shasht:read-json content)
                   (error () (error "search-memory result is not JSON.")))))
    (unless (and (hash-table-p payload)
                 (member (gethash "status" payload)
                         '("available" "empty") :test #'string=)
                 (zerop (gethash "database_write_count" payload -1))
                 (or (vectorp (gethash "results" payload))
                     (listp (gethash "results" payload))))
      (error "search-memory result lacks the read-only result contract."))))

(defun %public-read-only-replay-validate-deliverable (arguments content)
  (let ((path (gethash "path" arguments)))
    (unless (and (%public-read-only-replay-nonempty-string-p path)
                 (not (search ".." path))
                 (or (uiop:string-suffix-p path ".md")
                     (uiop:string-suffix-p path ".txt")))
      (error "read-deliverable path is outside the bounded contract.")))
  (when (and (stringp content) (uiop:string-prefix-p "ERROR:" content))
    (error "read-deliverable returned an error."))
  (let ((payload (handler-case (shasht:read-json content)
                   (error () (error "read-deliverable result is not JSON.")))))
    (unless (and (hash-table-p payload)
                 (string= (gethash "path" payload "")
                          (format nil "/agent/state/deliverables/~a"
                                  (gethash "path" arguments)))
                 (integerp (gethash "original_characters" payload))
                 (stringp (gethash "content" payload))
                 (not (gethash "written" payload)))
      (error "read-deliverable result lacks the bounded read contract."))))

(defun %public-read-only-replay-result (tool-result id name arguments)
  (unless (hash-table-p tool-result) (error "Tool result must be an object."))
  (unless (and (string= (gethash "role" tool-result "") "tool")
               (string= (gethash "tool_call_id" tool-result "") id)
               (stringp (gethash "content" tool-result)))
    (error "Tool result does not match the captured tool call."))
  (let ((content (gethash "content" tool-result)))
    (cond ((string= name "search-memory")
           (%public-read-only-replay-validate-search arguments content))
          ((string= name "read-deliverable")
           (%public-read-only-replay-validate-deliverable arguments content)))
    (%public-read-only-replay-json-copy tool-result)))

(defun %public-read-only-replay-assistant-tool-message (message)
  (if (fboundp '%assistant-tool-message-for-request)
      (funcall '%assistant-tool-message-for-request message)
      (let ((call (first (%public-read-only-replay-list
                          (gethash "tool_calls" message)))))
        (multiple-value-bind (id name arguments)
            (%public-read-only-replay-arguments call)
          (declare (ignore arguments))
          (obj "role" "assistant" "content" :null
               "tool_calls"
               (vector
                (obj "id" id "type" "function"
                     "function"
                     (obj "name" name
                          "arguments"
                          (gethash "arguments" (gethash "function" call))))))))))

(defun public-read-only-replay-step (request response
                                     &key tool-result tools)
  "Validate one captured response and return FINAL or an exact continuation."
  (handler-case
      (multiple-value-bind (message tool-calls finish-reason)
          (%public-read-only-replay-call response)
        (if tool-calls
            (progn
              (unless (string= finish-reason "tool_calls")
                (error "Tool-call response has non-tool finish reason ~a."
                       finish-reason))
              (unless (= 1 (length tool-calls))
                (error "Read-only replay permits exactly one tool call per step."))
              (when (%public-read-only-replay-nonempty-string-p
                     (gethash "content" message))
                (error "Tool-call response also contained public text."))
              (unless tool-result (error "Captured tool call requires a result."))
              (multiple-value-bind (id name arguments)
                  (%public-read-only-replay-arguments (first tool-calls))
                (let* ((validated-result
                         (%public-read-only-replay-result
                          tool-result id name arguments))
                       (aligned (public-read-only-replay-align-request
                                 request tools))
                       (messages (%public-read-only-replay-messages aligned)))
                  (setf (gethash "messages" aligned)
                        (coerce
                         (append messages
                                 (list
                                  (%public-read-only-replay-assistant-tool-message
                                   message)
                                  validated-result))
                         'vector))
                  (incf *public-read-only-replay-continuations*)
                  (obj "schema_version" 1 "status" "continuation"
                       "tool_name" name "tool_call_id" id
                       "next_request" aligned
                       "database_write_count" 0 "provider_call_count" 0
                       "delivery_authority" nil))))
            (let ((content (gethash "content" message)))
              (unless (string= finish-reason "stop")
                (error "Final response has non-terminal finish reason ~a."
                       finish-reason))
              (unless (%public-read-only-replay-nonempty-string-p content)
                (error "Final response contains no text."))
              (when (search "<tool" content :test #'char-equal)
                (error "Final response contains raw tool-call markup."))
              (incf *public-read-only-replay-finals*)
              (obj "schema_version" 1 "status" "final"
                   "content" content "database_write_count" 0
                   "provider_call_count" 0 "delivery_authority" nil))))
    (error (condition)
      (incf *public-read-only-replay-rejections*)
      (obj "schema_version" 1 "status" "rejected"
           "reason" (princ-to-string condition)
           "database_write_count" 0 "provider_call_count" 0
           "delivery_authority" nil))))

(defun public-read-only-replay-report ()
  (obj "schema_version" 1
       "allowed_tools" (coerce *public-read-only-replay-tool-names* 'vector)
       "aligned_requests" *public-read-only-replay-aligned*
       "continuations" *public-read-only-replay-continuations*
       "finals" *public-read-only-replay-finals*
       "rejections" *public-read-only-replay-rejections*
       "database_write_count" 0 "provider_calls_available" nil
       "delivery_authority" nil))
