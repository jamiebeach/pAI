;;;; interaction-runtime.lisp -- durable, serialized reactive ingress per mind.

(in-package :agent)

(export '(conscious-interaction-configure conscious-interaction-admit
          conscious-interaction-wait conscious-interaction-submit-and-wait
          conscious-interaction-recover conscious-interaction-start
          conscious-interaction-stop conscious-interaction-report))

(defparameter *conscious-interaction-schema-version* 1)
(defparameter *conscious-interaction-runtime-revision* "conscious-interaction-v1")
(defvar *public-inbound-channel* "internal")
(defparameter *conscious-interaction-lock*
  (bt:make-lock "conscious interaction coordinator"))
(defparameter *conscious-interaction-admission-lock*
  (bt:make-lock "conscious interaction admission"))
(defparameter *conscious-interaction-condition* (bt:make-condition-variable))
(defvar *conscious-interaction-agent-id* nil)
(defvar *conscious-interaction-executor-fn* nil)
(defvar *conscious-interaction-metadata-fn* nil)
(defvar *conscious-interaction-observer-fn* nil)
(defvar *conscious-interaction-queue-capacity* 32)
(defvar *conscious-interaction-queue* nil)
(defvar *conscious-interaction-results* (make-hash-table :test #'equal))
(defvar *conscious-interaction-result-order* nil)
(defparameter *conscious-interaction-result-capacity* 256)
(defvar *conscious-interaction-active* nil)
(defvar *conscious-interaction-admissions-in-progress* 0)
(defvar *conscious-interaction-sequence* 0)
(defvar *conscious-interaction-worker* nil)
(defvar *conscious-interaction-running-p* nil)

(defparameter *conscious-interaction-terminal-event-types*
  '("conscious-interaction-completed" "conscious-interaction-failed"
    "conscious-interaction-outcome-unknown"))
(declaim (ftype (function () t) conscious-interaction-report))

(defun %interaction-copy-object (value)
  (let ((copy (make-hash-table :test #'equal)))
    (when (hash-table-p value)
      (maphash (lambda (key item) (setf (gethash key copy) item)) value))
    copy))

(defun %interaction-notify (status item &optional result)
  (let ((observer *conscious-interaction-observer-fn*))
    (when (functionp observer)
      (handler-case (funcall observer status item result)
        (error () nil)))))

(defun %interaction-append (type payload &key caused-by)
  (unless (fboundp 'log-event)
    (error "Interaction coordinator requires durable event append"))
  (let* ((values (multiple-value-list
                  (funcall 'log-event type payload :caused-by caused-by)))
         (id (first values))
         (durable (second values))
         (event (third values)))
    (unless (and (>= (length values) 3) id durable (hash-table-p event)
                 (string= type (gethash "type" event "")))
      (error "Interaction ~a event has no exact durable receipt" type))
    (values id event)))

(defun %interaction-id ()
  (bt:with-lock-held (*conscious-interaction-lock*)
    (format nil "interaction:~a:~d:~d:~8,'0x"
            *conscious-interaction-agent-id* (get-universal-time)
            (incf *conscious-interaction-sequence*) (random #x100000000))))

(defun %interaction-result (status interaction-id user-event-id
                            &key content agent-event-id error-code)
  (obj "schema_version" *conscious-interaction-schema-version*
       "status" status "interaction_id" interaction-id
       "user_event_id" user-event-id
       "agent_event_id" (or agent-event-id :null)
       "content" (or content :null)
       "error_code" (or error-code :null)))

(defun %interaction-store-result (interaction-id result)
  (bt:with-lock-held (*conscious-interaction-lock*)
    (unless (gethash interaction-id *conscious-interaction-results*)
      (setf *conscious-interaction-result-order*
            (append *conscious-interaction-result-order*
                    (list interaction-id))))
    (setf (gethash interaction-id *conscious-interaction-results*) result)
    (loop while (> (length *conscious-interaction-result-order*)
                   *conscious-interaction-result-capacity*)
          for expired = (pop *conscious-interaction-result-order*)
          do (remhash expired *conscious-interaction-results*))
    (bt:condition-notify *conscious-interaction-condition*))
  result)

(defun conscious-interaction-configure
    (&key agent-id executor-fn metadata-fn observer-fn (queue-capacity 32))
  "Bind this process's one coordinator to one durable mind and executor."
  (unless (and (stringp agent-id) (plusp (length agent-id)))
    (error "Interaction coordinator requires a non-empty agent ID"))
  (unless (functionp executor-fn)
    (error "Interaction coordinator requires an executor function"))
  (unless (or (null metadata-fn) (functionp metadata-fn))
    (error "Interaction metadata provider must be a function or NIL"))
  (unless (or (null observer-fn) (functionp observer-fn))
    (error "Interaction observer must be a function or NIL"))
  (unless (and (integerp queue-capacity) (<= 1 queue-capacity 1024))
    (error "Interaction queue capacity must be between 1 and 1024"))
  (bt:with-lock-held (*conscious-interaction-lock*)
    (when (and *conscious-interaction-agent-id*
               (not (string= agent-id *conscious-interaction-agent-id*)))
      (error "Interaction coordinator is already bound to another mind"))
    (setf *conscious-interaction-agent-id* agent-id
          *conscious-interaction-executor-fn* executor-fn
          *conscious-interaction-metadata-fn* metadata-fn
          *conscious-interaction-observer-fn* observer-fn
          *conscious-interaction-queue-capacity* queue-capacity))
  (conscious-interaction-report))

(defun %interaction-admit-serialized (prompt channel)
  (unless (and (stringp prompt) (plusp (length prompt)))
    (error "Interaction input must be non-empty text"))
  (unless (and (stringp channel) (plusp (length channel)) (<= (length channel) 64))
    (error "Interaction channel must be bounded non-empty text"))
  (unless (and *conscious-interaction-agent-id*
               (functionp *conscious-interaction-executor-fn*))
    (error "Interaction coordinator is not configured"))
  (bt:with-lock-held (*conscious-interaction-lock*)
    (when (>= (+ (length *conscious-interaction-queue*)
                 (if *conscious-interaction-active* 1 0)
                 *conscious-interaction-admissions-in-progress*)
              *conscious-interaction-queue-capacity*)
      (error "Interaction queue capacity is exhausted"))
    (incf *conscious-interaction-admissions-in-progress*))
  (let* ((interaction-id (%interaction-id))
         (metadata
           (%interaction-copy-object
            (and (functionp *conscious-interaction-metadata-fn*)
                 (funcall *conscious-interaction-metadata-fn*)))))
    (setf (gethash "source" metadata) "q4.5-conversation"
          (gethash "interaction_id" metadata) interaction-id
          (gethash "interaction_runtime_revision" metadata)
          *conscious-interaction-runtime-revision*)
    (handler-case
        (let ((*public-inbound-channel* channel))
          (multiple-value-bind (ignored status user-event-id)
              (submit-stimulus prompt :kind :user-message :metadata metadata
                                      :wait-for-public-result nil)
            (declare (ignore ignored))
            (unless (and (eq status :accepted) user-event-id)
              (error "Interaction input was not durably admitted"))
            (let ((item (obj "interaction_id" interaction-id
                             "user_event_id" user-event-id
                             "prompt" prompt "channel" channel
                             "attempt" 0))
                  (position nil))
              (bt:with-lock-held (*conscious-interaction-lock*)
                (decf *conscious-interaction-admissions-in-progress*)
                (setf *conscious-interaction-queue*
                      (append *conscious-interaction-queue* (list item))
                      position (+ (length *conscious-interaction-queue*)
                                  (if *conscious-interaction-active* 1 0)))
                (bt:condition-notify *conscious-interaction-condition*))
              (let ((receipt-status (if (= position 1) "accepted" "queued")))
                (%interaction-notify receipt-status item)
                (return-from %interaction-admit-serialized
                  (obj "schema_version" 1 "status" receipt-status
                       "interaction_id" interaction-id
                       "user_event_id" user-event-id
                       "queue_position" position))))))
      (error (condition)
        (bt:with-lock-held (*conscious-interaction-lock*)
          (decf *conscious-interaction-admissions-in-progress*)
          (bt:condition-notify *conscious-interaction-condition*))
        (error condition)))))

(defun conscious-interaction-admit (prompt &key (channel "terminal"))
  "Durably admit one message and return without waiting for inference.

Only admission is serialized. This preserves the event authority's order
through queue insertion while the worker and provider remain independent."
  (bt:with-lock-held (*conscious-interaction-admission-lock*)
    (%interaction-admit-serialized prompt channel)))

(defun %interaction-run-item (item)
  (let* ((interaction-id (gethash "interaction_id" item))
         (user-event-id (gethash "user_event_id" item))
         (attempt (1+ (gethash "attempt" item 0))))
    (setf (gethash "attempt" item) attempt)
    (%interaction-append
     "conscious-interaction-claimed"
     (obj "interaction_id" interaction-id "user_event_id" user-event-id
          "attempt" attempt "runtime_revision"
          *conscious-interaction-runtime-revision*)
     :caused-by user-event-id)
    (%interaction-notify "thinking" item)
    (handler-case
        (let* ((result
                 (funcall *conscious-interaction-executor-fn*
                          (gethash "prompt" item)
                          :admitted-event-id user-event-id
                          :channel (gethash "channel" item)
                          :interaction-id interaction-id))
               (status (and (hash-table-p result) (gethash "status" result))))
          (unless (and (hash-table-p result) (stringp status))
            (error "Interaction executor returned no typed result"))
          (%interaction-append
           "conscious-interaction-completed"
           (obj "interaction_id" interaction-id "user_event_id" user-event-id
                "result_status" status "agent_event_id"
                (or (gethash "agent_event_id" result) :null))
           :caused-by user-event-id)
          (setf (gethash "interaction_id" result) interaction-id)
          (%interaction-store-result interaction-id result)
          (%interaction-notify status item result))
      (error (condition)
        (let ((result
                (%interaction-result
                 "failed" interaction-id user-event-id
                 :error-code "interaction-executor-failed")))
          (%interaction-append
           "conscious-interaction-failed"
           (obj "interaction_id" interaction-id "user_event_id" user-event-id
                "error_code" "interaction-executor-failed"
                "condition_type" (string-downcase
                                  (symbol-name (type-of condition))))
           :caused-by user-event-id)
          (%interaction-store-result interaction-id result)
          (%interaction-notify "failed" item result))))))

(defun %interaction-worker-loop ()
  (loop
    (let ((item nil))
      (bt:with-lock-held (*conscious-interaction-lock*)
        (loop while (and *conscious-interaction-running-p*
                         (null *conscious-interaction-queue*))
              do (bt:condition-wait *conscious-interaction-condition*
                                    *conscious-interaction-lock*))
        (unless *conscious-interaction-running-p* (return))
        (setf item (pop *conscious-interaction-queue*)
              *conscious-interaction-active* item))
      (unwind-protect (%interaction-run-item item)
        (bt:with-lock-held (*conscious-interaction-lock*)
          (setf *conscious-interaction-active* nil)
          (bt:condition-notify *conscious-interaction-condition*))))))

(defun %interaction-event-items (events)
  (let ((items (make-hash-table :test #'equal))
        (by-user (make-hash-table :test #'equal))
        (committed-pulses (make-hash-table :test #'equal)))
    (dolist (event events)
      (when (and (hash-table-p event)
                 (equal *conscious-interaction-agent-id*
                        (gethash "agent_id" event)))
        (let* ((type (gethash "type" event ""))
               (payload (gethash "payload" event))
               (metadata (and (hash-table-p payload)
                              (gethash "metadata" payload))))
          (when (and (string= type "pulse-committed")
                     (hash-table-p payload)
                     (stringp (gethash "pulse_id" payload)))
            (setf (gethash (gethash "pulse_id" payload) committed-pulses) t))
          (when (and (string= type "user-message")
                     (hash-table-p metadata)
                     (string= "q4.5-conversation"
                              (gethash "source" metadata ""))
                     (stringp (gethash "interaction_id" metadata)))
            (let* ((interaction-id (gethash "interaction_id" metadata))
                   (item (obj "interaction_id" interaction-id
                              "user_event_id" (gethash "id" event)
                              "prompt" (gethash "text" payload)
                              "channel" (gethash "channel" payload "internal")
                              "attempt" 0 "state" "admitted"
                              "agent_event_id" :null "reply" :null)))
              (setf (gethash interaction-id items) item
                    (gethash (gethash "id" event) by-user) item))))))
    (dolist (event events)
      (let* ((type (and (hash-table-p event) (gethash "type" event "")))
             (payload (and (hash-table-p event) (gethash "payload" event)))
             (interaction-id (and (hash-table-p payload)
                                  (gethash "interaction_id" payload)))
             (item (and (stringp interaction-id)
                        (gethash interaction-id items)))
             (caused-by (and (hash-table-p event) (gethash "caused_by" event)))
             (caused-item (gethash caused-by by-user)))
        (cond
          ((and item (string= type "conscious-interaction-claimed"))
           (setf (gethash "state" item) "claimed"
                 (gethash "attempt" item) (gethash "attempt" payload 1)))
          ((and item (member type *conscious-interaction-terminal-event-types*
                             :test #'string=))
           (setf (gethash "state" item)
                 (cond ((string= type "conscious-interaction-completed") "completed")
                       ((string= type "conscious-interaction-failed") "failed")
                       (t "outcome-unknown"))
                 (gethash "terminal_status" item)
                 (and (hash-table-p payload) (gethash "result_status" payload))
                 (gethash "terminal_agent_event_id" item)
                 (and (hash-table-p payload) (gethash "agent_event_id" payload))))
          ((and caused-item (string= type "model-request"))
           (let ((pulse-id (and (hash-table-p payload)
                                (gethash "pulse_id" payload))))
             (if (and (stringp pulse-id) (gethash pulse-id committed-pulses))
                 (setf (gethash "provider_outcome_committed" caused-item) t)
                 (setf (gethash "provider_started" caused-item) t))))
          ((and caused-item (string= type "agent-message"))
           (setf (gethash "agent_event_id" caused-item) (gethash "id" event)
                 (gethash "reply" caused-item)
                 (and (hash-table-p payload) (gethash "text" payload)))))))
    (sort (loop for item being the hash-values of items collect item)
          #'< :key (lambda (item) (gethash "user_event_id" item)))))

(defun conscious-interaction-recover ()
  "Rebuild pending/result state from authority before starting the worker."
  (unless *conscious-interaction-agent-id*
    (error "Interaction coordinator is not configured"))
  (when (and *conscious-interaction-worker*
             (bt:thread-alive-p *conscious-interaction-worker*))
    (error "Interaction recovery requires the worker to be stopped"))
  (let* ((events (funcall 'replay-events
                          :types (append '("user-message" "model-request"
                                           "pulse-committed"
                                           "agent-message"
                                           "conscious-interaction-claimed")
                                         *conscious-interaction-terminal-event-types*)))
         (items (%interaction-event-items events))
         (pending nil)
         (results (make-hash-table :test #'equal)))
    (dolist (item items)
      (let ((state (gethash "state" item))
            (interaction-id (gethash "interaction_id" item))
            (user-event-id (gethash "user_event_id" item)))
        (cond
          ((string= state "completed")
           (let ((terminal-status (gethash "terminal_status" item))
                 (terminal-agent (gethash "terminal_agent_event_id" item)))
             (setf (gethash interaction-id results)
                   (%interaction-result
                    (if (stringp terminal-status) terminal-status "completed")
                    interaction-id user-event-id
                    :content (let ((reply (gethash "reply" item)))
                               (and (stringp reply) reply))
                    :agent-event-id (cond ((integerp terminal-agent) terminal-agent)
                                          ((integerp (gethash "agent_event_id" item))
                                           (gethash "agent_event_id" item)))))))
          ((string= state "failed")
           (setf (gethash interaction-id results)
                 (%interaction-result "failed" interaction-id user-event-id
                                      :error-code "recovered-failure")))
          ((string= state "outcome-unknown")
           (setf (gethash interaction-id results)
                 (%interaction-result "outcome-unknown" interaction-id user-event-id
                                      :error-code "provider-outcome-unknown")))
          ((and (string= state "claimed") (gethash "agent_event_id" item)
                (integerp (gethash "agent_event_id" item)))
           (%interaction-append
            "conscious-interaction-completed"
            (obj "interaction_id" interaction-id
                 "user_event_id" user-event-id "result_status" "replied"
                 "agent_event_id" (gethash "agent_event_id" item)
                 "recovered" t)
            :caused-by user-event-id)
           (setf (gethash interaction-id results)
                 (%interaction-result
                  "replied" interaction-id user-event-id
                  :content (gethash "reply" item)
                  :agent-event-id (gethash "agent_event_id" item))))
          ((and (string= state "claimed") (gethash "provider_started" item))
           (%interaction-append
            "conscious-interaction-outcome-unknown"
            (obj "interaction_id" interaction-id
                 "user_event_id" user-event-id
                 "error_code" "provider-outcome-unknown")
            :caused-by user-event-id)
           (setf (gethash interaction-id results)
                 (%interaction-result "outcome-unknown" interaction-id user-event-id
                                      :error-code "provider-outcome-unknown")))
          (t (push item pending)))))
    (let ((result-order
            (loop for item in items
                  for interaction-id = (gethash "interaction_id" item)
                  when (gethash interaction-id results)
                    collect interaction-id)))
      (loop while (> (length result-order)
                     *conscious-interaction-result-capacity*)
            for expired = (pop result-order)
            do (remhash expired results))
    (bt:with-lock-held (*conscious-interaction-lock*)
      (setf *conscious-interaction-queue* (nreverse pending)
            *conscious-interaction-results* results
            *conscious-interaction-result-order* result-order
            *conscious-interaction-active* nil
            *conscious-interaction-admissions-in-progress* 0))
    (conscious-interaction-report))))

(defun conscious-interaction-start ()
  (unless (and *conscious-interaction-agent-id*
               (functionp *conscious-interaction-executor-fn*))
    (error "Interaction coordinator is not configured"))
  (unless (and *conscious-interaction-worker*
               (bt:thread-alive-p *conscious-interaction-worker*))
    (conscious-interaction-recover)
    (bt:with-lock-held (*conscious-interaction-lock*)
      (setf *conscious-interaction-running-p* t
            *conscious-interaction-worker*
            (bt:make-thread #'%interaction-worker-loop
                            :name "conscious-interaction"))
      (bt:condition-notify *conscious-interaction-condition*)))
  (conscious-interaction-report))

(defun conscious-interaction-stop ()
  (let ((worker nil))
    (bt:with-lock-held (*conscious-interaction-lock*)
      (setf *conscious-interaction-running-p* nil
            worker *conscious-interaction-worker*)
      (bt:condition-notify *conscious-interaction-condition*))
    (when (and worker (not (eq worker (bt:current-thread))))
      (ignore-errors (bt:join-thread worker)))
    (bt:with-lock-held (*conscious-interaction-lock*)
      (setf *conscious-interaction-worker* nil)))
  :stopped)

(defun conscious-interaction-wait (interaction-id &key (timeout 240))
  (unless (and (stringp interaction-id) (plusp (length interaction-id)))
    (error "Interaction wait requires an interaction ID"))
  (unless (and (realp timeout) (plusp timeout))
    (error "Interaction wait timeout must be positive"))
  (let ((deadline (+ (get-internal-real-time)
                     (* timeout internal-time-units-per-second))))
    (bt:with-lock-held (*conscious-interaction-lock*)
      (loop
        (let ((result (gethash interaction-id *conscious-interaction-results*)))
          (when result (return result)))
        (let ((remaining (/ (- deadline (get-internal-real-time))
                            (coerce internal-time-units-per-second
                                    'double-float))))
          (when (<= remaining 0)
            (return (%interaction-result "timeout" interaction-id :null
                                         :error-code "wait-timeout")))
          (bt:condition-wait *conscious-interaction-condition*
                             *conscious-interaction-lock*
                             :timeout remaining))))))

(defun conscious-interaction-submit-and-wait
    (prompt &key (channel "terminal") (timeout 240))
  (let ((receipt (conscious-interaction-admit prompt :channel channel)))
    (conscious-interaction-wait (gethash "interaction_id" receipt)
                                :timeout timeout)))

(defun conscious-interaction-report ()
  (bt:with-lock-held (*conscious-interaction-lock*)
    (obj "schema_version" *conscious-interaction-schema-version*
         "runtime_revision" *conscious-interaction-runtime-revision*
         "agent_id" (or *conscious-interaction-agent-id* :null)
         "running" (and *conscious-interaction-worker*
                         (bt:thread-alive-p *conscious-interaction-worker*))
         "active_interaction_id"
         (if *conscious-interaction-active*
             (gethash "interaction_id" *conscious-interaction-active*) :null)
         "queued_count" (length *conscious-interaction-queue*)
         "queue_capacity" *conscious-interaction-queue-capacity*
         "terminal_result_count" (hash-table-count
                                  *conscious-interaction-results*))))

(defun conscious-interaction-reset-for-tests ()
  (ignore-errors (conscious-interaction-stop))
  (bt:with-lock-held (*conscious-interaction-lock*)
    (setf *conscious-interaction-agent-id* nil
          *conscious-interaction-executor-fn* nil
          *conscious-interaction-metadata-fn* nil
          *conscious-interaction-observer-fn* nil
          *conscious-interaction-queue* nil
          *conscious-interaction-results* (make-hash-table :test #'equal)
          *conscious-interaction-result-order* nil
          *conscious-interaction-active* nil
          *conscious-interaction-admissions-in-progress* 0
          *conscious-interaction-sequence* 0
          *conscious-interaction-running-p* nil
          *conscious-interaction-worker* nil))
  t)
