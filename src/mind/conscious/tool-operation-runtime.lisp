;;;; tool-operation-runtime.lisp -- durable execution of closed conscious tools.

(in-package :agent)

(export '(conscious-tool-operation-run conscious-tool-operation-report))

(defparameter *conscious-tool-operation-schema-version* 1)
(defparameter *conscious-tool-operation-runtime-revision*
  "conscious-tool-operation-v1")
(defparameter *conscious-tool-operation-lease-seconds* 120)
(defvar *conscious-tool-operation-search-fn* #'conscious-file-search)
(defvar *conscious-tool-operation-executions* 0)
(defvar *conscious-tool-operation-recoveries* 0)
(defvar *conscious-tool-operation-failures* 0)

(defun %conscious-tool-operation-hash (text)
  (string-downcase
   (ironclad:byte-array-to-hex-string
    (ironclad:digest-sequence
     :sha256 (sb-ext:string-to-octets text :external-format :utf-8)))))

(defun %conscious-tool-operation-canonical-json (value)
  (labels ((emit (item stream)
             (cond
               ((hash-table-p item)
                (let ((keys nil))
                  (maphash (lambda (key ignored)
                             (declare (ignore ignored)) (push key keys))
                           item)
                  (write-char #\{ stream)
                  (loop for key in (sort keys #'string< :key #'princ-to-string)
                        for first = t then nil
                        do (unless first (write-char #\, stream))
                           (format stream "~s:" (princ-to-string key))
                           (emit (gethash key item) stream))
                  (write-char #\} stream)))
               ((and (vectorp item) (not (stringp item)))
                (write-char #\[ stream)
                (loop for child across item for first = t then nil
                      do (unless first (write-char #\, stream))
                         (emit child stream))
                (write-char #\] stream))
               ((stringp item) (format stream "~s" item))
               (t (format stream "~a" item)))))
    (with-output-to-string (stream) (emit value stream))))

(defun %conscious-tool-operation-append (type payload caused-by)
  (unless (fboundp 'log-event)
    (error "Conscious tool execution requires durable event append"))
  (let* ((values (multiple-value-list
                  (funcall 'log-event type payload :caused-by caused-by)))
         (id (first values))
         (durable (second values))
         (event (third values)))
    (unless (and (>= (length values) 3) id durable (hash-table-p event)
                 (string= type (gethash "type" event "")))
      (error "Conscious tool ~a event has no exact durable receipt" type))
    (values id event)))

(defun %conscious-tool-operation-normalized-arguments (tool-name arguments)
  (unless (string= tool-name "search-files")
    (error "Tool ~s has no conscious execution authority" tool-name))
  (multiple-value-bind (query relative maximum)
      (%conscious-file-search-exact-arguments arguments)
    (obj "query" query "path" relative "max_results" maximum)))

(defun %conscious-tool-operation-validated-proposal (proposal manifest)
  (let* ((captured
           (obj "schema_version" 1 "proposals" (vector proposal)))
         (validated (conscious-proposals-validate captured manifest))
         (rows (gethash "proposals" validated)))
    (unless (and (= 1 (length rows))
                 (string= "tool-call-proposal"
                          (gethash "kind" (aref rows 0) "")))
      (error "Conscious tool execution requires exactly one tool proposal"))
    (aref rows 0)))

(defun %conscious-tool-operation-events ()
  (unless (fboundp 'replay-events)
    (error "Conscious tool execution requires durable event replay"))
  (funcall 'replay-events
           :types '("conscious-tool-operation-claimed"
                    "conscious-tool-operation-result"
                    "conscious-tool-operation-failed")))

(defun %conscious-tool-operation-event (events type operation-id)
  (find-if
   (lambda (event)
     (let ((payload (and (hash-table-p event) (gethash "payload" event))))
       (and (hash-table-p payload)
            (equal (and (boundp '*agent-id*) *agent-id*)
                   (gethash "agent_id" event))
            (string= type (gethash "type" event ""))
            (string= operation-id (gethash "operation_id" payload "")))))
   events :from-end t))

(defun %conscious-tool-operation-verify-result
    (event proposal-id interaction-id work-id user-event-id tool-name
     arguments-hash)
  (let* ((payload (gethash "payload" event))
         (result (and (hash-table-p payload) (gethash "result" payload)))
         (result-hash (and (hash-table-p payload)
                           (gethash "result_hash" payload))))
    (unless (and (string= proposal-id (gethash "proposal_id" payload ""))
                 (equal interaction-id
                        (let ((value (gethash "interaction_id" payload)))
                          (and (stringp value) value)))
                 (equal work-id
                        (let ((value (gethash "work_id" payload)))
                          (and (stringp value) value)))
                 (equal user-event-id (gethash "user_event_id" payload))
                 (string= tool-name (gethash "tool_name" payload ""))
                 (string= arguments-hash
                          (gethash "arguments_hash" payload ""))
                 (hash-table-p result)
                 (stringp result-hash)
                 (string= result-hash
                          (%conscious-tool-operation-hash
                           (%conscious-tool-operation-canonical-json result))))
      (error "Durable conscious tool result conflicts with its proposal"))
    result))

(defun conscious-tool-operation-run
    (proposal manifest &key interaction-id work-id user-event-id
                           (max-result-characters 10000000))
  "Execute one validated closed tool proposal or recover its durable result."
  (unless (and (or (and (stringp interaction-id)
                         (plusp (length interaction-id))
                         (<= (length interaction-id) 256))
                    (and (stringp work-id) (plusp (length work-id))
                         (<= (length work-id) 256)))
               (integerp user-event-id) (plusp user-event-id))
    (error "Conscious tool operation identity is invalid"))
  (unless (and (integerp max-result-characters)
               (plusp max-result-characters)
               (<= max-result-characters 10000000))
    (error "Conscious tool operation result bound is invalid"))
  (unless (and (boundp '*agent-id*) (stringp *agent-id*)
               (plusp (length *agent-id*)))
    (error "Conscious tool operation requires a durable agent partition"))
  (let* ((validated
           (%conscious-tool-operation-validated-proposal proposal manifest))
         (proposal-id (gethash "proposal_id" validated))
         (proposal-payload (gethash "payload" validated))
         (tool-name (gethash "tool_name" proposal-payload))
         (arguments
           (%conscious-tool-operation-normalized-arguments
            tool-name (gethash "arguments" proposal-payload)))
         (arguments-hash
           (%conscious-tool-operation-hash
            (%conscious-tool-operation-canonical-json arguments)))
         (operation-id (format nil "tool-operation:~a" proposal-id))
         (events (%conscious-tool-operation-events))
         (result-event
           (%conscious-tool-operation-event
            events "conscious-tool-operation-result" operation-id))
         (failure-event
           (%conscious-tool-operation-event
            events "conscious-tool-operation-failed" operation-id))
         (claim-event
           (%conscious-tool-operation-event
            events "conscious-tool-operation-claimed" operation-id)))
    (when result-event
      (incf *conscious-tool-operation-recoveries*)
      (return-from conscious-tool-operation-run
        (%conscious-tool-operation-verify-result
         result-event proposal-id interaction-id work-id user-event-id tool-name
         arguments-hash)))
    (when failure-event
      (error "Conscious tool operation is durably failed"))
    (when claim-event
      (error "Conscious tool operation outcome is uncertain"))
    (let* ((claimed-at (get-universal-time))
           (lease-expires-at
             (+ claimed-at *conscious-tool-operation-lease-seconds*)))
      (%conscious-tool-operation-append
       "conscious-tool-operation-claimed"
       (obj "schema_version" *conscious-tool-operation-schema-version*
           "operation_id" operation-id "proposal_id" proposal-id
           "interaction_id" (or interaction-id :null)
           "work_id" (or work-id :null) "user_event_id" user-event-id
           "tool_name" tool-name "arguments_hash" arguments-hash
            "attempt" 1 "claimed_at" claimed-at
            "lease_expires_at" lease-expires-at
            "runtime_revision" *conscious-tool-operation-runtime-revision*)
       user-event-id))
    (let ((result
            (handler-case
                (progn
                  (incf *conscious-tool-operation-executions*)
                  (funcall *conscious-tool-operation-search-fn* arguments))
              (error (condition)
                (declare (ignore condition))
                (incf *conscious-tool-operation-failures*)
                (%conscious-tool-operation-append
                 "conscious-tool-operation-failed"
                 (obj "schema_version" *conscious-tool-operation-schema-version*
                      "operation_id" operation-id "proposal_id" proposal-id
                      "interaction_id" (or interaction-id :null)
                      "work_id" (or work-id :null)
                      "user_event_id" user-event-id "tool_name" tool-name
                      "arguments_hash" arguments-hash
                      "error_code" "tool-handler-failed"
                      "runtime_revision"
                      *conscious-tool-operation-runtime-revision*)
                 user-event-id)
                (error "Conscious tool handler failed")))))
      (unless (and (hash-table-p result)
                   (string= "ok" (gethash "status" result ""))
                   (zerop (gethash "database_write_count" result -1)))
        (incf *conscious-tool-operation-failures*)
        (%conscious-tool-operation-append
         "conscious-tool-operation-failed"
         (obj "schema_version" *conscious-tool-operation-schema-version*
              "operation_id" operation-id "proposal_id" proposal-id
              "interaction_id" (or interaction-id :null)
              "work_id" (or work-id :null) "user_event_id" user-event-id
              "tool_name" tool-name "arguments_hash" arguments-hash
              "error_code" "tool-result-invalid" "runtime_revision"
              *conscious-tool-operation-runtime-revision*)
         user-event-id)
        (error "Conscious tool handler returned an invalid read-only result"))
      ;; Keep the result append outside the handler-case. If commit visibility
      ;; is uncertain, a later run sees the claim and refuses blind execution.
      (let ((canonical-result
              (%conscious-tool-operation-canonical-json result)))
        (when (> (length canonical-result) max-result-characters)
          (incf *conscious-tool-operation-failures*)
          (%conscious-tool-operation-append
           "conscious-tool-operation-failed"
           (obj "schema_version" *conscious-tool-operation-schema-version*
                "operation_id" operation-id "proposal_id" proposal-id
                "interaction_id" (or interaction-id :null)
                "work_id" (or work-id :null) "user_event_id" user-event-id
                "tool_name" tool-name "arguments_hash" arguments-hash
                "error_code" "tool-result-bound-exceeded"
                "result_characters" (length canonical-result)
                "max_result_characters" max-result-characters
                "runtime_revision"
                *conscious-tool-operation-runtime-revision*)
           user-event-id)
          (error "Conscious tool result exceeds its admitted bound"))
        (%conscious-tool-operation-append
         "conscious-tool-operation-result"
         (obj "schema_version" *conscious-tool-operation-schema-version*
              "operation_id" operation-id "proposal_id" proposal-id
              "interaction_id" (or interaction-id :null)
              "work_id" (or work-id :null) "user_event_id" user-event-id
              "tool_name" tool-name "arguments_hash" arguments-hash
              "result" result "result_characters" (length canonical-result)
              "result_hash"
              (%conscious-tool-operation-hash canonical-result)
              "runtime_revision" *conscious-tool-operation-runtime-revision*)
         user-event-id))
      result)))

(defun conscious-tool-operation-report ()
  (obj "schema_version" *conscious-tool-operation-schema-version*
       "runtime_revision" *conscious-tool-operation-runtime-revision*
       "allowed_tools" (vector "search-files")
       "executions" *conscious-tool-operation-executions*
       "durable_recoveries" *conscious-tool-operation-recoveries*
       "failures" *conscious-tool-operation-failures*
       "effect_authority" nil "database_write_count" 0))
