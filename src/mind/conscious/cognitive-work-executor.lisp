;;;; cognitive-work-executor.lisp -- one-owner, safe-boundary cognition loop.
;;;;
;;;; The executor owns no private prompt or result state.  Its injected quantum
;;;; performs one durable unit of work.  The executor then rebuilds the work
;;;; projection and proves that the claimed boundary exists before scheduling
;;;; anything else.

(in-package :agent)

(export '(conscious-work-executor-configure conscious-work-executor-run-one
          conscious-work-executor-wake conscious-work-executor-start
          conscious-work-executor-stop conscious-work-executor-report
          conscious-work-executor-reset))

(defparameter *conscious-work-executor-schema-version* 1)
(defparameter *conscious-work-executor-revision* "conscious-work-executor-v1")
(defparameter *conscious-work-executor-boundary-kinds*
  '("pulse-committed" "operation-result" "suspended" "completed"
    "failed" "outcome-unknown"))
(defparameter *conscious-work-executor-wake-reasons*
  '("durable-stimulus" "runnable-boundary" "operation-result"
    "timer-fired" "operator-request" "startup-recovery"))
(defparameter *conscious-work-executor-state-lock*
  (bt:make-lock "conscious work executor state"))
(defparameter *conscious-work-executor-owner-lock*
  (bt:make-lock "conscious work executor owner"))
(defparameter *conscious-work-executor-condition*
  (bt:make-condition-variable))
(defvar *conscious-work-executor-agent-id* nil)
(defvar *conscious-work-executor-prepare-fn* nil)
(defvar *conscious-work-executor-projection-fn* nil)
(defvar *conscious-work-executor-quantum-fn* nil)
(defvar *conscious-work-executor-observer-fn* nil)
(defvar *conscious-work-executor-worker* nil)
(defvar *conscious-work-executor-running-p* nil)
(defvar *conscious-work-executor-in-progress-p* nil)
(defvar *conscious-work-executor-pending-wakes* 0)
(defvar *conscious-work-executor-boundaries* 0)
(defvar *conscious-work-executor-idle-passes* 0)
(defvar *conscious-work-executor-failures* 0)
(defvar *conscious-work-executor-last-work-id* nil)
(defvar *conscious-work-executor-last-error* nil)
(defvar *conscious-work-executor-observer-active-p* nil)
(declaim (ftype (function () t) conscious-work-executor-report))

(defun %conscious-work-executor-text-p (value maximum)
  (and (stringp value) (plusp (length value)) (<= (length value) maximum)))

(defun %conscious-work-executor-notify (status report)
  (let ((observer *conscious-work-executor-observer-fn*))
    (when (functionp observer)
      (handler-case
          (let ((*conscious-work-executor-observer-active-p* t))
            (funcall observer status report))
        (error () nil)))))

(defun %conscious-work-executor-projection ()
  (let ((projection (funcall *conscious-work-executor-projection-fn*)))
    (unless (and (hash-table-p projection)
                 (= *conscious-work-schema-version*
                    (gethash "schema_version" projection -1))
                 (equal *conscious-work-executor-agent-id*
                        (gethash "agent_id" projection))
                 (hash-table-p (gethash "items" projection)))
      (error "Cognitive work executor received an invalid projection"))
    projection))

(defun %conscious-work-executor-prepare ()
  (let* ((result (funcall *conscious-work-executor-prepare-fn*))
         (status (and (hash-table-p result) (gethash "status" result))))
    (unless (and (hash-table-p result)
                 (= 1 (gethash "schema_version" result -1))
                 (member status '("idle" "opened" "recovered") :test #'string=))
      (error "Cognitive work preparation returned an invalid receipt"))
    result))

(defun %conscious-work-executor-signature (work)
  ;; Only authoritative projected progress belongs here.  No stimulus, model,
  ;; tool or persona content is copied into executor state or diagnostics.
  (list (gethash "state" work)
        (gethash "waiting_reason" work)
        (gethash "parent_pulse_id" work)
        (gethash "model_calls_used" work)
        (gethash "tool_operations_used" work)
        (gethash "reasoning_continuations_used" work)
        (gethash "tool_result_characters_used" work)
        (gethash "service_count" work)))

(defun %conscious-work-executor-exact-keys-p (object keys)
  (and (hash-table-p object)
       (= (hash-table-count object) (length keys))
       (every (lambda (key) (nth-value 1 (gethash key object))) keys)))

(defun %conscious-work-executor-validate-result (result work-id)
  (unless (%conscious-work-executor-exact-keys-p
           result '("schema_version" "status" "work_id" "boundary_kind"))
    (error "Cognitive work quantum returned an invalid envelope"))
  (unless (and (= *conscious-work-executor-schema-version*
                  (gethash "schema_version" result -1))
               (string= "committed-boundary" (gethash "status" result ""))
               (string= work-id (gethash "work_id" result ""))
               (member (gethash "boundary_kind" result)
                       *conscious-work-executor-boundary-kinds*
                       :test #'string=))
    (error "Cognitive work quantum returned a mismatched boundary"))
  result)

(defun %conscious-work-executor-verify-boundary (before after kind)
  (when (equal (%conscious-work-executor-signature before)
               (%conscious-work-executor-signature after))
    (error "Cognitive work quantum produced no durable projected progress"))
  (cond
    ((string= kind "pulse-committed")
     (unless (and (= (1+ (gethash "service_count" before))
                     (gethash "service_count" after))
                  (= (1+ (gethash "model_calls_used" before))
                     (gethash "model_calls_used" after))
                  (not (equal (gethash "parent_pulse_id" before)
                              (gethash "parent_pulse_id" after))))
       (error "Claimed pulse boundary is absent from durable work state")))
    ((string= kind "operation-result")
     (unless (> (gethash "tool_operations_used" after)
                (gethash "tool_operations_used" before))
       (error "Claimed operation boundary is absent from durable work state")))
    ((member kind '("suspended" "completed" "failed" "outcome-unknown")
             :test #'string=)
     (unless (string= kind (gethash "state" after ""))
       (error "Claimed work transition is absent from durable work state"))))
  after)

(defun conscious-work-executor-configure
    (&key agent-id prepare-fn projection-fn quantum-fn observer-fn)
  "Bind the process's only cognitive executor to one mind and one quantum."
  (when *conscious-work-executor-observer-active-p*
    (error "A cognitive work observer cannot reconfigure its executor"))
  (unless (%conscious-work-executor-text-p agent-id 256)
    (error "Cognitive work executor requires a bounded agent ID"))
  (unless (functionp prepare-fn)
    (error "Cognitive work executor requires a stimulus preparation function"))
  (unless (functionp projection-fn)
    (error "Cognitive work executor requires a projection function"))
  (unless (functionp quantum-fn)
    (error "Cognitive work executor requires a quantum function"))
  (unless (or (null observer-fn) (functionp observer-fn))
    (error "Cognitive work executor observer must be a function or NIL"))
  ;; Match RUN-ONE's owner->state lock order so callbacks cannot be swapped
  ;; between selection and their durable boundary verification.
  (bt:with-lock-held (*conscious-work-executor-owner-lock*)
    (bt:with-lock-held (*conscious-work-executor-state-lock*)
      (when (or *conscious-work-executor-running-p*
                *conscious-work-executor-in-progress-p*)
        (error "An active cognitive work executor cannot be reconfigured"))
      (when (and *conscious-work-executor-agent-id*
                 (not (string= agent-id *conscious-work-executor-agent-id*)))
        (error "Cognitive work executor is already bound to another mind"))
      (setf *conscious-work-executor-agent-id* agent-id
            *conscious-work-executor-prepare-fn* prepare-fn
            *conscious-work-executor-projection-fn* projection-fn
            *conscious-work-executor-quantum-fn* quantum-fn
            *conscious-work-executor-observer-fn* observer-fn
            *conscious-work-executor-last-error* nil)))
  (conscious-work-executor-report))

(defun %conscious-work-executor-configured-p ()
  (and (%conscious-work-executor-text-p
       *conscious-work-executor-agent-id* 256)
       (functionp *conscious-work-executor-prepare-fn*)
       (functionp *conscious-work-executor-projection-fn*)
       (functionp *conscious-work-executor-quantum-fn*)))

(defun conscious-work-executor-run-one ()
  "Run at most one quantum, then rebuild and verify the durable boundary."
  (when *conscious-work-executor-observer-active-p*
    (error "A cognitive work observer cannot reenter its executor"))
  (unless (%conscious-work-executor-configured-p)
    (error "Cognitive work executor is not configured"))
  (let ((notifications nil)
        (result nil))
    (handler-case
        (setf result
              (bt:with-lock-held (*conscious-work-executor-owner-lock*)
                (%conscious-work-executor-prepare)
                (let* ((before-projection (%conscious-work-executor-projection))
           (selection (conscious-work-select before-projection)))
                  (when (string= "idle" (gethash "status" selection ""))
                    (bt:with-lock-held (*conscious-work-executor-state-lock*)
                      (incf *conscious-work-executor-idle-passes*))
                    (return-from conscious-work-executor-run-one
                      (obj "schema_version" 1 "status" "idle" "work_id" :null
                           "selection" selection)))
                  (let* ((work-id (gethash "work_id" selection))
             (before (gethash work-id (gethash "items" before-projection))))
                    (unless (hash-table-p before)
                      (error "Selected cognitive work is absent from its projection"))
                    (bt:with-lock-held (*conscious-work-executor-state-lock*)
                      (setf *conscious-work-executor-in-progress-p* t
                            *conscious-work-executor-last-work-id* work-id
                            *conscious-work-executor-last-error* nil))
                    (push (list "selected" selection) notifications)
                    (unwind-protect
                         (let* ((claimed
                           (%conscious-work-executor-validate-result
                           (funcall *conscious-work-executor-quantum-fn*
                                    (%conscious-work-public-item-copy before)
                                    selection)
                           work-id))
                        (ignored-preparation
                          (%conscious-work-executor-prepare))
                        (after-projection
                          (%conscious-work-executor-projection))
                        (after
                          (gethash work-id (gethash "items" after-projection)))
                        (kind (gethash "boundary_kind" claimed)))
                   (declare (ignore ignored-preparation))
                   (unless (hash-table-p after)
                     (error "Cognitive work disappeared after its quantum"))
                   (%conscious-work-executor-verify-boundary before after kind)
                           (let ((next (conscious-work-select after-projection)))
                             (bt:with-lock-held (*conscious-work-executor-state-lock*)
                               (incf *conscious-work-executor-boundaries*))
                             (let ((report
                             (obj "schema_version" 1 "status" "advanced"
                                  "work_id" work-id "boundary_kind" kind
                                  "work" (%conscious-work-public-item-copy after)
                                  "next_selection" next)))
                               (push (list "advanced" report) notifications)
                               report)))
                      (bt:with-lock-held (*conscious-work-executor-state-lock*)
                        (setf *conscious-work-executor-in-progress-p* nil)))))))
      (error (condition)
        (bt:with-lock-held (*conscious-work-executor-state-lock*)
          (incf *conscious-work-executor-failures*)
          (setf *conscious-work-executor-last-error*
                (string-downcase (symbol-name (type-of condition)))))
        (error condition)))
    ;; Observers are diagnostics, not executor authority. Run them only after
    ;; releasing the non-recursive owner lock so they cannot deadlock the mind.
    (dolist (notification (nreverse notifications))
      (%conscious-work-executor-notify (first notification)
                                       (second notification)))
    result))

(defun conscious-work-executor-wake (&key (reason "durable-stimulus"))
  "Queue one content-free scheduler wake.  Durable authority remains the input."
  (unless (member reason *conscious-work-executor-wake-reasons* :test #'string=)
    (error "Cognitive work wake reason is invalid"))
  (unless (%conscious-work-executor-configured-p)
    (error "Cognitive work executor is not configured"))
  (bt:with-lock-held (*conscious-work-executor-state-lock*)
    (incf *conscious-work-executor-pending-wakes*)
    (bt:condition-notify *conscious-work-executor-condition*))
  (obj "schema_version" 1 "status" "queued" "reason" reason))

(defun %conscious-work-executor-next-lease-timeout ()
  "Seconds until the earliest durable model lease, or NIL when none exists."
  (handler-case
      (let ((earliest nil)
            (now (get-universal-time)))
        (maphash
         (lambda (work-id work)
           (declare (ignore work-id))
           (let ((expires (and (hash-table-p work)
                               (gethash "pending_model_lease_expires_at"
                                        work))))
             (when (and (string= "deliberating"
                                 (gethash "state" work ""))
                        (integerp expires))
               (setf earliest (if earliest (min earliest expires) expires)))))
         (gethash "items" (%conscious-work-executor-projection)))
        (and earliest (max 0.05d0 (- earliest now))))
    ;; A transient authority read must not disable lease recovery forever.
    (error () 1)))

(defun %conscious-work-executor-worker-loop ()
  (loop
    (let ((lease-timeout (%conscious-work-executor-next-lease-timeout)))
      (bt:with-lock-held (*conscious-work-executor-state-lock*)
        (loop while (and *conscious-work-executor-running-p*
                         (zerop *conscious-work-executor-pending-wakes*))
              do (if lease-timeout
                     (bt:condition-wait *conscious-work-executor-condition*
                                        *conscious-work-executor-state-lock*
                                        :timeout lease-timeout)
                     (bt:condition-wait *conscious-work-executor-condition*
                                        *conscious-work-executor-state-lock*))
                 (when (and lease-timeout
                            *conscious-work-executor-running-p*
                            (zerop *conscious-work-executor-pending-wakes*))
                   (incf *conscious-work-executor-pending-wakes*)))
        (unless *conscious-work-executor-running-p* (return))
        (decf *conscious-work-executor-pending-wakes*)))
    (when (fboundp 'conscious-work-runtime-reap-expired-model-leases)
      (ignore-errors (conscious-work-runtime-reap-expired-model-leases)))
    (handler-case
        (let ((result (conscious-work-executor-run-one)))
          ;; A committed runnable continuation schedules another quantum, but
          ;; only after the durable replay/selection boundary above.  External
          ;; wakes can enter the same queue while the quantum is in flight.
          (when (and (string= "advanced" (gethash "status" result ""))
                     (string= "selected"
                              (gethash "status"
                                       (gethash "next_selection" result) "")))
            (conscious-work-executor-wake :reason "runnable-boundary")))
      (error ()
        ;; The failure is already counted and named by RUN-ONE.  Do not spin or
        ;; invent a retry; a fresh durable stimulus/operator action must wake it.
        nil))))

(defun conscious-work-executor-start ()
  (unless (%conscious-work-executor-configured-p)
    (error "Cognitive work executor is not configured"))
  (bt:with-lock-held (*conscious-work-executor-state-lock*)
    (unless (and *conscious-work-executor-worker*
                 (bt:thread-alive-p *conscious-work-executor-worker*))
      (setf *conscious-work-executor-running-p* t
            *conscious-work-executor-worker*
            (bt:make-thread #'%conscious-work-executor-worker-loop
                            :name "pAI cognitive work executor"))))
  (conscious-work-executor-report))

(defun conscious-work-executor-stop ()
  (when *conscious-work-executor-observer-active-p*
    (error "A cognitive work observer cannot stop its executor"))
  (let ((worker nil))
    (bt:with-lock-held (*conscious-work-executor-state-lock*)
      (setf *conscious-work-executor-running-p* nil
            worker *conscious-work-executor-worker*)
      (bt:condition-notify *conscious-work-executor-condition*))
    (when (and worker (bt:thread-alive-p worker)
               (not (eq worker (bt:current-thread))))
      (bt:join-thread worker))
    (bt:with-lock-held (*conscious-work-executor-state-lock*)
      (setf *conscious-work-executor-worker* nil)))
  (conscious-work-executor-report))

(defun conscious-work-executor-report ()
  (bt:with-lock-held (*conscious-work-executor-state-lock*)
    (obj "schema_version" 1
         "runtime_revision" *conscious-work-executor-revision*
         "agent_id" (or *conscious-work-executor-agent-id* :null)
         "configured" (not (null (%conscious-work-executor-configured-p)))
         "running" (not (null *conscious-work-executor-running-p*))
         "in_progress" (not (null *conscious-work-executor-in-progress-p*))
         "pending_wakes" *conscious-work-executor-pending-wakes*
         "committed_boundaries" *conscious-work-executor-boundaries*
         "idle_passes" *conscious-work-executor-idle-passes*
         "failures" *conscious-work-executor-failures*
         "last_work_id" (or *conscious-work-executor-last-work-id* :null)
         "last_error_type" (or *conscious-work-executor-last-error* :null))))

(defun conscious-work-executor-reset ()
  "Test/development reset.  A running worker is stopped before authority clears."
  (conscious-work-executor-stop)
  (bt:with-lock-held (*conscious-work-executor-owner-lock*)
    (bt:with-lock-held (*conscious-work-executor-state-lock*)
      (setf *conscious-work-executor-agent-id* nil
            *conscious-work-executor-prepare-fn* nil
            *conscious-work-executor-projection-fn* nil
            *conscious-work-executor-quantum-fn* nil
            *conscious-work-executor-observer-fn* nil
            *conscious-work-executor-pending-wakes* 0
            *conscious-work-executor-boundaries* 0
            *conscious-work-executor-idle-passes* 0
            *conscious-work-executor-failures* 0
            *conscious-work-executor-last-work-id* nil
            *conscious-work-executor-last-error* nil
            *conscious-work-executor-in-progress-p* nil)))
  (conscious-work-executor-report))
