;;;; near-term-intention-tool.lisp -- same-turn causal receipt tool.
;;;;
;;;; Catalogue exposure is mode-gated. In :OFF mode the tool is absent from the
;;;; model catalogue. The stable dispatcher only creates a durable receipt; it cannot
;;;; think, send, schedule arbitrary text, or bypass publication/delivery.

(in-package :agent)

(export '(near-term-intention-tool-install near-term-intention-tool-uninstall
          near-term-intention-tool-report near-term-intention-tool-handle))

(defvar *near-term-intention-tool-installed* nil)
(defvar *near-term-intention-tool-base-execute* nil)
(defvar *publication-contract-current* nil)

(defun %near-term-intention-tool-definition ()
  (obj "type" "function" "function"
       (obj "name" "hold-near-term-thought"
            "description"
            "Create one durable, short-horizon private thinking commitment tied to the current conversation turn. Use only when a real 180-300 second deferred response is warranted. This creates a receipt but does not itself think, send, or grant permission to use other tools."
            "parameters"
            (obj "type" "object"
                 "properties"
                 (obj "subject" (obj "type" "string"
                                     "description" "Concise subject to keep in near-term attention.")
                      "aim" (obj "type" "string"
                                 "description" "Specific grounded result to develop.")
                      "return_window_seconds"
                      (obj "type" "integer" "minimum" 180 "maximum" 300
                           "description" "Requested short return horizon."))
                 "required" (vector "subject" "aim" "return_window_seconds")))))

(defun near-term-intention-tool-handle (tool-call)
  (unless (string= (ignore-errors (ref tool-call "function" "name"))
                   "hold-near-term-thought")
    (error "NEAR-TERM-INTENTION tool port name mismatch."))
  (handler-case
      (let* ((arguments (shasht:read-json
                         (ref tool-call "function" "arguments")))
             (context (and (boundp '*turn-capture-context*)
                           *turn-capture-context*))
             (turn-id (and (hash-table-p context)
                           (gethash "turn_id" context)))
             (user-event-id (and (hash-table-p context)
                                 (gethash "user_event_id" context)))
             (origin-events (if (or (null user-event-id) (eq user-event-id :null))
                                nil (list user-event-id))))
        (multiple-value-bind (record reason)
            (near-term-intention-create
             (gethash "subject" arguments) (gethash "aim" arguments)
             turn-id origin-events (gethash "return_window_seconds" arguments))
          (obj "role" "tool" "tool_call_id" (gethash "id" tool-call)
               "content"
               (if record
                   (progn
                     (when (and (boundp '*publication-contract-current*)
                                (hash-table-p *publication-contract-current*))
                       (setf (gethash "deferred_receipt_active"
                                      *publication-contract-current*) t
                             (gethash "deferred_receipt_id"
                                      *publication-contract-current*)
                             (gethash "commitment_receipt_id" record)))
                     (shasht:write-json
                      (obj "status" "accepted"
                           "intention_id" (gethash "id" record)
                           "receipt_id" (gethash "commitment_receipt_id" record)
                           "state" (gethash "state" record)
                           "response_deadline" (gethash "response_deadline" record)
                           "warning" "Receipt created; no thinking or delivery has occurred yet.")
                      nil))
                   (shasht:write-json
                    (obj "status" "rejected" "reason" reason) nil)))))
    (error (condition)
      (obj "role" "tool" "tool_call_id" (gethash "id" tool-call)
           "content"
           (shasht:write-json
            (obj "status" "rejected" "reason" "invalid-tool-request"
                 "error_type" (string-downcase
                               (symbol-name (type-of condition)))) nil)))))

(defun %near-term-intention-tool-execute (tool-call)
  (let ((name (ignore-errors (ref tool-call "function" "name"))))
    (if (string= name "hold-near-term-thought")
        (near-term-intention-tool-handle tool-call)
        (funcall *near-term-intention-tool-base-execute* tool-call))))

(defun near-term-intention-tool-install ()
  "Install one stable dispatcher; expose the tool only in :ENFORCED mode."
  (when (and (or (not (fboundp 'tool-dispatch-legacy-wrapper-enabled-p))
                 (funcall 'tool-dispatch-legacy-wrapper-enabled-p))
             (not *near-term-intention-tool-installed*))
    (setf *near-term-intention-tool-base-execute* (fdefinition 'execute)
          (fdefinition 'execute) #'%near-term-intention-tool-execute
          *near-term-intention-tool-installed* t))
  (when (and (boundp '*near-term-intentions-mode*)
             (eq *near-term-intentions-mode* :enforced)
             (not (find "hold-near-term-thought" *tools*
                        :key (lambda (tool) (ref tool "function" "name"))
                        :test #'string=)))
    (setf *tools* (concatenate 'vector *tools*
                               (vector (%near-term-intention-tool-definition)))))
  t)

(defun near-term-intention-tool-uninstall ()
  "Remove catalogue exposure. Keep the stable fall-through dispatcher installed."
  (setf *tools*
        (coerce (remove "hold-near-term-thought" (coerce *tools* 'list)
                        :key (lambda (tool) (ref tool "function" "name"))
                        :test #'string=)
                'vector))
  t)

(defun near-term-intention-tool-report ()
  (obj "schema_version" 1
       "installed" (if *near-term-intention-tool-installed* t nil)
       "listed" (if (find "hold-near-term-thought" *tools*
                          :key (lambda (tool) (ref tool "function" "name"))
                          :test #'string=) t nil)
       "mode" (if (boundp '*near-term-intentions-mode*)
                  (string-downcase (symbol-name *near-term-intentions-mode*)) "off")))

(define-init :install near-term-intention-tool
    "Register the near-term-intention tool handler."
  (near-term-intention-tool-install))
