;;;; cognitive-operation-executor.lisp -- separate durable tool-operation owner.
;;;;
;;;; The cognitive scheduler owns admission by selecting waiting-operation
;;;; work. This worker never calls a model. It reconstructs the exact committed
;;;; proposal and manifest from durable events, delegates execution to the
;;;; idempotent operation runtime, proves the projected result boundary, and
;;;; only then wakes cognition.

(in-package :agent)

(export '(conscious-operation-executor-configure
          conscious-operation-executor-run-one
          conscious-operation-executor-wake
          conscious-operation-executor-wait
          conscious-operation-executor-start
          conscious-operation-executor-stop
          conscious-operation-executor-report
          conscious-operation-executor-reset))

(declaim (ftype (function () t) conscious-operation-executor-report))

(defparameter *conscious-operation-executor-revision*
  "conscious-operation-executor-v1")
(defparameter *conscious-operation-executor-lock*
  (bt:make-lock "conscious operation executor state"))
(defparameter *conscious-operation-executor-owner-lock*
  (bt:make-lock "conscious operation executor owner"))
(defparameter *conscious-operation-executor-condition*
  (bt:make-condition-variable))
(defvar *conscious-operation-executor-agent-id* nil)
(defvar *conscious-operation-executor-events-fn* nil)
(defvar *conscious-operation-executor-projection-fn* nil)
(defvar *conscious-operation-executor-operation-fn* nil)
(defvar *conscious-operation-executor-transition-fn* nil)
(defvar *conscious-operation-executor-cognition-wake-fn* nil)
(defvar *conscious-operation-executor-observer-fn* nil)
(defvar *conscious-operation-executor-worker* nil)
(defvar *conscious-operation-executor-running-p* nil)
(defvar *conscious-operation-executor-pending-wakes* 0)
(defvar *conscious-operation-executor-in-progress-p* nil)
(defvar *conscious-operation-executor-boundaries* 0)
(defvar *conscious-operation-executor-failures* 0)
(defvar *conscious-operation-executor-last-work-id* nil)
(defvar *conscious-operation-executor-last-error* nil)
(defvar *conscious-operation-executor-results* (make-hash-table :test #'equal))

(defun %conscious-operation-executor-configured-p ()
  (and (stringp *conscious-operation-executor-agent-id*)
       (plusp (length *conscious-operation-executor-agent-id*))
       (every #'functionp
              (list *conscious-operation-executor-events-fn*
                    *conscious-operation-executor-projection-fn*
                    *conscious-operation-executor-operation-fn*
                    *conscious-operation-executor-transition-fn*
                    *conscious-operation-executor-cognition-wake-fn*))))

(defun %conscious-operation-executor-notify (status report)
  (let ((observer *conscious-operation-executor-observer-fn*))
    (when (functionp observer)
      (handler-case (funcall observer status report) (error () nil)))))

(defun %conscious-operation-executor-items (value)
  (cond ((vectorp value) (coerce value 'list))
        ((listp value) value)
        (t nil)))

(defun %conscious-operation-executor-proposal (events work proposal-id)
  (let ((matches nil))
    (dolist (event events)
      (let ((payload (and (hash-table-p event) (gethash "payload" event))))
        (when (and (hash-table-p payload)
                   (equal *conscious-operation-executor-agent-id*
                          (gethash "agent_id" event))
                   (string= "pulse-committed" (gethash "type" event ""))
                   (string= (gethash "work_id" work)
                            (gethash "work_id" payload "")))
          (dolist (proposal
                   (%conscious-operation-executor-items
                    (gethash "proposals" payload)))
            (when (and (hash-table-p proposal)
                       (string= proposal-id
                                (gethash "proposal_id" proposal "")))
              (push (cons proposal (gethash "context_manifest" payload))
                    matches))))))
    (unless (and (= 1 (length matches))
                 (hash-table-p (caar matches))
                 (hash-table-p (cdar matches)))
      (error "Waiting operation lacks one durable proposal and manifest"))
    (values (caar matches) (cdar matches))))

(defun %conscious-operation-executor-root-id (events work proposal)
  (let* ((context
           (conscious-work-context-build
            events (gethash "work_id" work)
            *conscious-operation-executor-agent-id*))
         (roots (coerce (gethash "root_event_ids" context) 'list))
         (evidence
           (%conscious-operation-executor-items
            (gethash "evidence_event_ids" proposal)))
         (eligible
           (sort (remove-if-not (lambda (id) (member id roots :test #'equal))
                                evidence)
                 #'<)))
    (unless (and eligible (every (lambda (id) (integerp id)) eligible))
      (error "Waiting operation cites no durable work root"))
    (first eligible)))

(defun %conscious-operation-executor-terminal-p (events work-id proposal-id type)
  (find-if
   (lambda (event)
     (let ((payload (and (hash-table-p event) (gethash "payload" event))))
       (and (hash-table-p payload)
            (equal *conscious-operation-executor-agent-id*
                   (gethash "agent_id" event))
            (string= type (gethash "type" event ""))
            (string= work-id (gethash "work_id" payload ""))
            (string= proposal-id (gethash "proposal_id" payload "")))))
   events :from-end t))

(defun %conscious-operation-executor-claim-active-p (claim &optional (now (get-universal-time)))
  (let* ((payload (and (hash-table-p claim) (gethash "payload" claim)))
         (expires (and (hash-table-p payload)
                       (gethash "lease_expires_at" payload))))
    (and (integerp expires) (> expires now))))

(defun conscious-operation-executor-configure
    (&key agent-id events-fn projection-fn
          (operation-fn #'conscious-tool-operation-run)
          (transition-fn #'conscious-work-runtime-transition)
          (cognition-wake-fn #'conscious-work-executor-wake)
          observer-fn)
  (unless (and (stringp agent-id) (plusp (length agent-id))
               (<= (length agent-id) 256))
    (error "Operation executor requires a bounded agent ID"))
  (dolist (function (list events-fn projection-fn operation-fn transition-fn
                          cognition-wake-fn))
    (unless (functionp function)
      (error "Operation executor requires complete function ports")))
  (unless (or (null observer-fn) (functionp observer-fn))
    (error "Operation executor observer must be a function or NIL"))
  (bt:with-lock-held (*conscious-operation-executor-owner-lock*)
    (bt:with-lock-held (*conscious-operation-executor-lock*)
      (when (or *conscious-operation-executor-running-p*
                *conscious-operation-executor-in-progress-p*)
        (error "An active operation executor cannot be reconfigured"))
      (when (and *conscious-operation-executor-agent-id*
                 (not (string= agent-id
                               *conscious-operation-executor-agent-id*)))
        (error "Operation executor is already bound to another mind"))
      (setf *conscious-operation-executor-agent-id* agent-id
            *conscious-operation-executor-events-fn* events-fn
            *conscious-operation-executor-projection-fn* projection-fn
            *conscious-operation-executor-operation-fn* operation-fn
            *conscious-operation-executor-transition-fn* transition-fn
            *conscious-operation-executor-cognition-wake-fn* cognition-wake-fn
            *conscious-operation-executor-observer-fn* observer-fn
            *conscious-operation-executor-last-error* nil)))
  (conscious-operation-executor-report))

(defun conscious-operation-executor-run-one ()
  "Run at most one scheduler-admitted durable operation boundary."
  (unless (%conscious-operation-executor-configured-p)
    (error "Operation executor is not configured"))
  (let ((notification nil))
    (bt:with-lock-held (*conscious-operation-executor-owner-lock*)
      (let* ((before-projection
               (funcall *conscious-operation-executor-projection-fn*))
             (selection (conscious-work-select-operation before-projection)))
        (when (string= "idle" (gethash "status" selection ""))
          (return-from conscious-operation-executor-run-one
            (obj "schema_version" 1 "status" "idle" "work_id" :null)))
        (let* ((work-id (gethash "work_id" selection))
               (work (gethash work-id (gethash "items" before-projection)))
               (proposal-id (and work
                                 (gethash "pending_tool_proposal_id" work)))
                (events (funcall *conscious-operation-executor-events-fn*))
                (existing-claim
                  (%conscious-operation-executor-terminal-p
                   events work-id proposal-id
                   "conscious-tool-operation-claimed")))
          (unless (and (hash-table-p work) (stringp proposal-id))
            (error "Selected operation has no pending proposal"))
          (bt:with-lock-held (*conscious-operation-executor-lock*)
            (setf *conscious-operation-executor-in-progress-p* t
                  *conscious-operation-executor-last-work-id* work-id
                  *conscious-operation-executor-last-error* nil))
          (unwind-protect
               (handler-case
                   (multiple-value-bind (proposal manifest)
                       (%conscious-operation-executor-proposal
                        events work proposal-id)
                     (let ((root-id
                             (%conscious-operation-executor-root-id
                              events work proposal)))
                       (funcall *conscious-operation-executor-operation-fn*
                                proposal manifest :work-id work-id
                                :user-event-id root-id
                                :max-result-characters
                                (gethash "max_tool_result_characters"
                                         (gethash "profile" work)))
                       (let* ((after
                                (funcall
                                 *conscious-operation-executor-projection-fn*))
                              (projected
                                (gethash work-id (gethash "items" after))))
                         (unless (and (hash-table-p projected)
                                      (string= "runnable"
                                               (gethash "state" projected ""))
                                      (= (1+ (gethash "tool_operations_used"
                                                     work))
                                         (gethash "tool_operations_used"
                                                  projected)))
                           (error "Durable operation result did not advance work"))
                          (incf *conscious-operation-executor-boundaries*)
                         (funcall *conscious-operation-executor-cognition-wake-fn*
                                  :reason "operation-result")
                          (setf notification
                                (let* ((terminal
                                         (%conscious-operation-executor-terminal-p
                                          (funcall
                                           *conscious-operation-executor-events-fn*)
                                          work-id proposal-id
                                          "conscious-tool-operation-result"))
                                       (outcome
                                         (conscious-boundary-outcome-make
                                          "succeeded"
                                          :claim-id
                                          (format nil "tool-operation:~a"
                                                  proposal-id)
                                          :terminal-event-id
                                          (and terminal (gethash "id" terminal))
                                          :reason-code "tool-operation-result")))
                                  (obj "schema_version" 1 "status" "advanced"
                                       "work_id" work-id
                                       "proposal_id" proposal-id
                                       "boundary_kind" "operation-result"
                                       "boundary_outcome" outcome))))))
                 (error (condition)
                   (let* ((after-events
                            (funcall *conscious-operation-executor-events-fn*))
                          (failed
                            (%conscious-operation-executor-terminal-p
                             after-events work-id proposal-id
                             "conscious-tool-operation-failed"))
                          (claimed
                            (%conscious-operation-executor-terminal-p
                             after-events work-id proposal-id
                             "conscious-tool-operation-claimed"))
                          (result
                            (%conscious-operation-executor-terminal-p
                             after-events work-id proposal-id
                             "conscious-tool-operation-result")))
                     (when result (error condition))
                      (let* ((leased
                               (and existing-claim
                                    (%conscious-operation-executor-claim-active-p
                                     existing-claim)))
                             (outcome-kind
                               (cond (failed "failed-after-claim")
                                     ((null claimed) "failed-before-claim")
                                     (t "outcome-unknown")))
                             (outcome
                               (and (not leased)
                                    (conscious-boundary-outcome-make
                                     outcome-kind
                                     :claim-id
                                     (and claimed
                                          (format nil "tool-operation:~a"
                                                  proposal-id))
                                     :terminal-event-id
                                     (and failed (gethash "id" failed))
                                     :reason-code
                                     (cond (failed "tool-operation-failed")
                                           ((null claimed)
                                            "tool-operation-rejected")
                                           (t
                                            "tool-operation-outcome-unknown")))))
                             (status
                               (if leased "leased"
                                   (or
                                    (conscious-boundary-outcome-work-transition
                                     outcome)
                                    "advanced")))
                             (reason
                              (cond (failed "tool-operation-failed")
                                    ((null claimed) "tool-operation-rejected")
                                    (t "tool-operation-outcome-unknown"))))
                        (unless leased
                          (let* ((current-projection
                                 (funcall
                                  *conscious-operation-executor-projection-fn*))
                               (current
                                 (gethash work-id
                                          (gethash "items" current-projection))))
                          (unless (hash-table-p current)
                            (error "Operation failure lost its work authority"))
                          (funcall *conscious-operation-executor-transition-fn*
                                   work-id status :reason-code reason
                                   :expected-state (gethash "state" current)
                                   :expected-revision
                                   (gethash "projection_revision" current))))
                        (unless leased
                          (incf *conscious-operation-executor-failures*))
                       (setf *conscious-operation-executor-last-error*
                             (string-downcase
                              (symbol-name (type-of condition)))
                             notification
                             (obj "schema_version" 1
                                  "status" status
                                  "work_id" work-id
                                  "proposal_id" proposal-id
                                  "boundary_kind" status
                                  "boundary_outcome" (or outcome :null)))))))
            (bt:with-lock-held (*conscious-operation-executor-lock*)
              (setf *conscious-operation-executor-in-progress-p* nil))))))
    (%conscious-operation-executor-notify
     (gethash "status" notification) notification)
    (bt:with-lock-held (*conscious-operation-executor-lock*)
      (setf (gethash (gethash "work_id" notification)
                     *conscious-operation-executor-results*)
            notification)
      (bt:condition-notify *conscious-operation-executor-condition*))
    notification))

(defun conscious-operation-executor-wake ()
  (unless (%conscious-operation-executor-configured-p)
    (error "Operation executor is not configured"))
  (bt:with-lock-held (*conscious-operation-executor-lock*)
    (incf *conscious-operation-executor-pending-wakes*)
    (bt:condition-notify *conscious-operation-executor-condition*))
  (obj "schema_version" 1 "status" "queued"))

(defun %conscious-operation-executor-next-lease-timeout ()
  "Seconds until the earliest unterminated operation claim, or NIL."
  (handler-case
      (let* ((events (funcall *conscious-operation-executor-events-fn*))
             (now (get-universal-time))
             (earliest nil))
        (dolist (claim events)
          (when (string= "conscious-tool-operation-claimed"
                         (gethash "type" claim ""))
            (let* ((payload (gethash "payload" claim))
                   (work-id (and (hash-table-p payload)
                                 (gethash "work_id" payload)))
                   (proposal-id (and (hash-table-p payload)
                                     (gethash "proposal_id" payload)))
                   (expires (and (hash-table-p payload)
                                 (gethash "lease_expires_at" payload))))
              (when (and (stringp work-id) (stringp proposal-id)
                         (integerp expires)
                         (not (%conscious-operation-executor-terminal-p
                               events work-id proposal-id
                               "conscious-tool-operation-result"))
                         (not (%conscious-operation-executor-terminal-p
                               events work-id proposal-id
                               "conscious-tool-operation-failed")))
                (setf earliest
                      (if earliest (min earliest expires) expires))))))
        (and earliest (max 0.05d0 (- earliest now))))
    ;; A transient authority read must not disable lease recovery forever.
    (error () 1)))

(defun conscious-operation-executor-wait (work-id &key (timeout 30))
  "Wait for this process's content-free operation receipt for WORK-ID."
  (unless (and (stringp work-id) (plusp (length work-id))
               (numberp timeout) (plusp timeout) (<= timeout 300))
    (error "Operation wait identity or timeout is invalid"))
  (let ((deadline (+ (get-internal-real-time)
                     (* timeout internal-time-units-per-second))))
    (bt:with-lock-held (*conscious-operation-executor-lock*)
      (loop
        (let ((result
                (gethash work-id *conscious-operation-executor-results*)))
          (when result
            (remhash work-id *conscious-operation-executor-results*)
            (return result)))
        (let ((remaining
                (/ (- deadline (get-internal-real-time))
                   (float internal-time-units-per-second 1.0d0))))
          (when (<= remaining 0)
            (return (obj "schema_version" 1 "status" "timeout"
                         "work_id" work-id)))
          (bt:condition-wait *conscious-operation-executor-condition*
                             *conscious-operation-executor-lock*
                             :timeout remaining))))))

(defun %conscious-operation-executor-loop ()
  (loop
    (let ((lease-timeout
            (%conscious-operation-executor-next-lease-timeout)))
      (bt:with-lock-held (*conscious-operation-executor-lock*)
        (loop while (and *conscious-operation-executor-running-p*
                         (zerop *conscious-operation-executor-pending-wakes*))
              do (if lease-timeout
                     (bt:condition-wait *conscious-operation-executor-condition*
                                        *conscious-operation-executor-lock*
                                        :timeout lease-timeout)
                     (bt:condition-wait *conscious-operation-executor-condition*
                                        *conscious-operation-executor-lock*))
                 (when (and lease-timeout
                            *conscious-operation-executor-running-p*
                            (zerop *conscious-operation-executor-pending-wakes*))
                   (incf *conscious-operation-executor-pending-wakes*)))
        (unless *conscious-operation-executor-running-p* (return))
        (decf *conscious-operation-executor-pending-wakes*)))
    (handler-case (conscious-operation-executor-run-one) (error () nil))))

(defun conscious-operation-executor-start ()
  (unless (%conscious-operation-executor-configured-p)
    (error "Operation executor is not configured"))
  (bt:with-lock-held (*conscious-operation-executor-lock*)
    (unless (and *conscious-operation-executor-worker*
                 (bt:thread-alive-p *conscious-operation-executor-worker*))
      (setf *conscious-operation-executor-running-p* t
            *conscious-operation-executor-worker*
            (bt:make-thread #'%conscious-operation-executor-loop
                            :name "pAI conscious operation executor"))))
  (conscious-operation-executor-report))

(defun conscious-operation-executor-stop ()
  (let ((worker nil))
    (bt:with-lock-held (*conscious-operation-executor-lock*)
      (setf *conscious-operation-executor-running-p* nil
            worker *conscious-operation-executor-worker*)
      (bt:condition-notify *conscious-operation-executor-condition*))
    (when (and worker (bt:thread-alive-p worker)
               (not (eq worker (bt:current-thread))))
      (bt:join-thread worker))
    (bt:with-lock-held (*conscious-operation-executor-lock*)
      (setf *conscious-operation-executor-worker* nil)))
  (conscious-operation-executor-report))

(defun conscious-operation-executor-report ()
  (bt:with-lock-held (*conscious-operation-executor-lock*)
    (obj "schema_version" 1
         "runtime_revision" *conscious-operation-executor-revision*
         "agent_id" (or *conscious-operation-executor-agent-id* :null)
         "configured" (not (null (%conscious-operation-executor-configured-p)))
         "running" (not (null *conscious-operation-executor-running-p*))
         "in_progress" (not (null *conscious-operation-executor-in-progress-p*))
         "pending_wakes" *conscious-operation-executor-pending-wakes*
         "committed_boundaries" *conscious-operation-executor-boundaries*
         "failures" *conscious-operation-executor-failures*
         "last_work_id" (or *conscious-operation-executor-last-work-id* :null)
         "last_error_type" (or *conscious-operation-executor-last-error* :null))))

(defun conscious-operation-executor-reset ()
  (ignore-errors (conscious-operation-executor-stop))
  (bt:with-lock-held (*conscious-operation-executor-owner-lock*)
    (bt:with-lock-held (*conscious-operation-executor-lock*)
      (setf *conscious-operation-executor-agent-id* nil
            *conscious-operation-executor-events-fn* nil
            *conscious-operation-executor-projection-fn* nil
            *conscious-operation-executor-operation-fn* nil
            *conscious-operation-executor-transition-fn* nil
            *conscious-operation-executor-cognition-wake-fn* nil
            *conscious-operation-executor-observer-fn* nil
            *conscious-operation-executor-pending-wakes* 0
            *conscious-operation-executor-in-progress-p* nil
            *conscious-operation-executor-boundaries* 0
            *conscious-operation-executor-failures* 0
            *conscious-operation-executor-last-work-id* nil
            *conscious-operation-executor-last-error* nil
            *conscious-operation-executor-results*
            (make-hash-table :test #'equal))))
  (conscious-operation-executor-report))
