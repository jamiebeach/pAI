;;;; lifecycle-runtime.lisp -- Q5 durable lifecycle append/reconciliation.
;;;;
;;;; This adapter owns event boundaries only. It executes no work and has no
;;;; provider, effect or publication route. Every append is reread before
;;;; success, and every in-memory value is rebuildable from replay.

(in-package :agent)

(export '(conscious-lifecycle-runtime-transition
          conscious-lifecycle-runtime-reconcile-result
          conscious-lifecycle-runtime-project conscious-lifecycle-runtime-restore
          conscious-lifecycle-runtime-report
          *conscious-lifecycle-runtime-projection*))

(define-condition conscious-lifecycle-transition-rejected (error)
  ((code :initarg :code :reader conscious-lifecycle-rejection-code)
   (reason :initarg :reason :reader conscious-lifecycle-rejection-reason))
  (:report (lambda (condition stream)
             (format stream "Lifecycle transition rejected (~a): ~a"
                     (conscious-lifecycle-rejection-code condition)
                     (conscious-lifecycle-rejection-reason condition)))))

(defun %lifecycle-runtime-reject (code format-control &rest arguments)
  (error 'conscious-lifecycle-transition-rejected
         :code code
         :reason (apply #'format nil format-control arguments)))

(defvar *conscious-lifecycle-runtime-lock*
  (bt:make-lock "conscious-lifecycle-runtime"))
(defvar *conscious-lifecycle-runtime-projection* nil)
(defvar *conscious-lifecycle-runtime-agent-id* nil)
(defvar *conscious-lifecycle-runtime-last-event-id* nil)
(defvar *conscious-lifecycle-runtime-last-error* nil)

(defparameter *conscious-lifecycle-result-event-types*
  '("agent-operation-terminal" "tool-result" "model-result"))

(defun %lifecycle-runtime-events ()
  (cond ((fboundp 'event-projection-events)
         (funcall 'event-projection-events))
        ((fboundp 'replay-events)
         (funcall 'replay-events))
        (t (error "Lifecycle runtime requires the event replay port"))))

(defun %lifecycle-runtime-event (events id &optional type agent-id)
  (find-if
   (lambda (event)
     (and (hash-table-p event)
          (equal id (gethash "id" event))
          (or (null type) (equal type (gethash "type" event)))
          (or (null agent-id) (equal agent-id (gethash "agent_id" event)))))
   events :from-end t))

(defun %lifecycle-runtime-request-event (events request-id agent-id)
  (find-if
   (lambda (event)
     (and (hash-table-p event)
          (equal agent-id (gethash "agent_id" event))
          (member (gethash "type" event)
                  '("conscious-lifecycle-transition"
                    "conscious-lifecycle-result-rejected"
                    "conscious-lifecycle-source-rejected")
                  :test #'string=)
          (let ((payload (gethash "payload" event)))
            (and (hash-table-p payload)
                 (equal request-id (gethash "request_id" payload))))))
   events :from-end t))

(defun %lifecycle-runtime-payload-identity-equal-p (left right)
  "Compare durable request identity while retaining actor revision as history.
The actor revision says which runtime first materialized the event; it is not
part of the deterministic request and therefore must survive runtime upgrades."
  (and (hash-table-p left) (hash-table-p right)
       (= (hash-table-count left) (hash-table-count right))
       (%lifecycle-text-p (gethash "actor_runtime_revision" left) 256)
       (%lifecycle-text-p (gethash "actor_runtime_revision" right) 256)
       (loop for key being the hash-keys of left using (hash-value value)
             always
             (multiple-value-bind (other present) (gethash key right)
               (and present
                    (or (string= key "actor_runtime_revision")
                        (equalp value other)))))))

(defun %lifecycle-runtime-append-readable (type payload agent-id &key caused-by)
  (unless (fboundp 'log-event)
    (error "Lifecycle runtime requires the event append port"))
  (let ((id (funcall 'log-event type payload :caused-by caused-by)))
    (unless id (error "Lifecycle append of ~a returned no event id" type))
    (let* ((events (%lifecycle-runtime-events))
           (stored (%lifecycle-runtime-event events id type agent-id)))
      (unless stored
        (error "Lifecycle ~a event ~s was not durably readable" type id))
      (values id stored events))))

(defun conscious-lifecycle-runtime-project (agent-id)
  (conscious-lifecycle-project (%lifecycle-runtime-events) :agent-id agent-id))

(defun conscious-lifecycle-runtime-restore (agent-id)
  "Rebuild and install lifecycle state from the event log."
  (let ((projection (conscious-lifecycle-runtime-project agent-id)))
    (setf *conscious-lifecycle-runtime-projection* projection
          *conscious-lifecycle-runtime-agent-id* agent-id
          *conscious-lifecycle-runtime-last-error* nil)
    projection))

(defun %lifecycle-runtime-preview-valid-p (events agent-id payload)
  (let ((preview
          (obj "schema_version" 1 "id" -1 "timestamp" 0
               "type" "conscious-lifecycle-transition" "agent_id" agent-id
               "caused_by" (gethash "source_event_id" payload)
               "payload" payload)))
    (let ((projected
            (conscious-lifecycle-project (append events (list preview))
                                         :agent-id agent-id)))
      (not (find -1 (gethash "invalid_event_ids" projected) :test #'equal)))))

(defun %lifecycle-runtime-transition-unlocked
    (lifecycle-id transition request-id lifecycle-kind origin-runtime-revision
     actor-runtime-revision source-event-id checkpoint-ref reason-code now
     agent-id)
  (let* ((payload
           (conscious-lifecycle-transition-payload
            lifecycle-id transition :request-id request-id
            :lifecycle-kind lifecycle-kind
            :origin-runtime-revision origin-runtime-revision
            :actor-runtime-revision actor-runtime-revision
            :source-event-id source-event-id :checkpoint-ref checkpoint-ref
            :reason-code reason-code :occurred-at now))
         (events (%lifecycle-runtime-events))
         (existing (%lifecycle-runtime-request-event events request-id agent-id)))
    (when existing
      (unless (and (equal "conscious-lifecycle-transition"
                          (gethash "type" existing))
                   (%lifecycle-runtime-payload-identity-equal-p
                    payload (gethash "payload" existing)))
        (%lifecycle-runtime-reject :request-conflict
         "request id ~s was reused with conflicting content" request-id))
      (setf *conscious-lifecycle-runtime-projection*
            (conscious-lifecycle-project events :agent-id agent-id)
            *conscious-lifecycle-runtime-agent-id* agent-id)
      (return-from %lifecycle-runtime-transition-unlocked
        (gethash "id" existing)))
    (unless (or (null source-event-id)
                (eq source-event-id :null)
                (%lifecycle-runtime-event events source-event-id nil agent-id))
      (error "Lifecycle source event ~s is not durably readable in partition ~s"
             source-event-id agent-id))
    (unless (%lifecycle-runtime-preview-valid-p events agent-id payload)
      (%lifecycle-runtime-reject :illegal-transition
       "transition ~a is invalid for ~a" transition lifecycle-id))
    (multiple-value-bind (id stored stored-events)
        (%lifecycle-runtime-append-readable
         "conscious-lifecycle-transition" payload agent-id
         :caused-by (if (or (null source-event-id)
                            (eq source-event-id :null))
                        nil source-event-id))
      (declare (ignore stored))
      (setf *conscious-lifecycle-runtime-projection*
            (conscious-lifecycle-project stored-events :agent-id agent-id)
            *conscious-lifecycle-runtime-agent-id* agent-id
            *conscious-lifecycle-runtime-last-event-id* id
            *conscious-lifecycle-runtime-last-error* nil)
      id)))

(defun conscious-lifecycle-runtime-transition
    (lifecycle-id transition &key request-id lifecycle-kind
                                  origin-runtime-revision actor-runtime-revision
                                  (source-event-id :null) (checkpoint-ref :null)
                                  (reason-code "explicit-transition") now
                                  (agent-id (and (boundp '*agent-id*)
                                                 (symbol-value '*agent-id*))))
  "Validate, durably append and rebuild one explicit Q5 lifecycle transition."
  (unless (%lifecycle-text-p agent-id 256)
    (error "Lifecycle transition requires a bounded agent partition"))
  (bt:with-lock-held (*conscious-lifecycle-runtime-lock*)
    (handler-case
        (%lifecycle-runtime-transition-unlocked
         lifecycle-id transition request-id lifecycle-kind
         origin-runtime-revision actor-runtime-revision source-event-id
         checkpoint-ref reason-code now agent-id)
      (conscious-lifecycle-transition-rejected (condition)
        (setf *conscious-lifecycle-runtime-last-error* "transition-rejected")
        (error condition))
      (error (condition)
        (setf *conscious-lifecycle-runtime-last-error* "operational-failure")
        (error condition)))))

(defun %lifecycle-runtime-result-correlation (payload)
  (or (gethash "operation_id" payload)
      (gethash "schedule_id" payload)
      (gethash "lifecycle_id" payload)
      :null))

(defun %lifecycle-runtime-result-success-p (payload)
  (member (gethash "status" payload) '("succeeded" "completed") :test #'string=))

(defun %lifecycle-runtime-rejection-payload
    (request-id lifecycle-id source-event-id claimed active reason now)
  (obj "schema_version" *conscious-lifecycle-schema-version*
       "request_id" request-id "lifecycle_id" lifecycle-id
       "source_event_id" source-event-id
       "claimed_runtime_revision" (or claimed :null)
       "active_runtime_revision" active "reason_code" reason
       "occurred_at" now))

(defun %lifecycle-runtime-existing-reconciliation
    (events request-id lifecycle-id source-event-id agent-id)
  (let ((existing (%lifecycle-runtime-request-event events request-id agent-id)))
    (when existing
      (let ((payload (gethash "payload" existing)))
        (unless (and (equal lifecycle-id (gethash "lifecycle_id" payload))
                     (equal source-event-id (gethash "source_event_id" payload)))
          (error "Lifecycle reconciliation request id was reused with conflicting content")))
      (values (gethash "id" existing) t))))

(defun conscious-lifecycle-runtime-reconcile-result
    (lifecycle-id source-event-id &key request-id current-runtime-revision now
                                      (agent-id (and (boundp '*agent-id*)
                                                     (symbol-value '*agent-id*))))
  "Reconcile one stored asynchronous result without executing or publishing it."
  (unless (and (%lifecycle-text-p agent-id 256)
               (%lifecycle-text-p request-id 256)
               (%lifecycle-text-p current-runtime-revision 256)
               (%lifecycle-present-id-p source-event-id))
    (error "Lifecycle result reconciliation identity is invalid"))
  (bt:with-lock-held (*conscious-lifecycle-runtime-lock*)
    (let ((events (%lifecycle-runtime-events)))
      (multiple-value-bind (existing-id found)
          (%lifecycle-runtime-existing-reconciliation
           events request-id lifecycle-id source-event-id agent-id)
        (when found
          (setf *conscious-lifecycle-runtime-projection*
                (conscious-lifecycle-project events :agent-id agent-id)
                *conscious-lifecycle-runtime-agent-id* agent-id)
          (return-from conscious-lifecycle-runtime-reconcile-result existing-id)))
      (let* ((projection (conscious-lifecycle-project events :agent-id agent-id))
             (lifecycle (conscious-lifecycle-current projection lifecycle-id))
             (source (%lifecycle-runtime-event events source-event-id nil agent-id)))
        (unless lifecycle (error "Unknown lifecycle ~s" lifecycle-id))
        (when (member (gethash "status" lifecycle)
                      *conscious-lifecycle-terminal-statuses* :test #'string=)
          (error "Lifecycle ~s is already terminal" lifecycle-id))
        (unless (and source
                     (member (gethash "type" source)
                             *conscious-lifecycle-result-event-types*
                             :test #'string=))
          (error "Source event is not a supported asynchronous result"))
        (let* ((payload (gethash "payload" source))
               (correlation (and (hash-table-p payload)
                                 (%lifecycle-runtime-result-correlation payload)))
               (claimed (and (hash-table-p payload)
                             (gethash "origin_runtime_revision" payload)))
               (origin (gethash "origin_runtime_revision" lifecycle))
               (rejection-reason
                 (cond
                   ((not (equal correlation lifecycle-id)) "correlation-mismatch")
                   ((not (%lifecycle-text-p claimed 256)) "unknown-runtime-revision")
                   ((or (not (string= claimed origin))
                        (not (string= claimed current-runtime-revision)))
                    "stale-runtime-revision")
                   (t nil))))
          (if rejection-reason
              (let ((rejected
                      (%lifecycle-runtime-rejection-payload
                       request-id lifecycle-id source-event-id claimed
                       current-runtime-revision rejection-reason now)))
                (multiple-value-bind (id stored all-events)
                    (%lifecycle-runtime-append-readable
                     "conscious-lifecycle-result-rejected" rejected agent-id
                     :caused-by source-event-id)
                  (declare (ignore stored))
                  (setf *conscious-lifecycle-runtime-projection*
                        (conscious-lifecycle-project all-events :agent-id agent-id)
                        *conscious-lifecycle-runtime-agent-id* agent-id
                        *conscious-lifecycle-runtime-last-event-id* id
                        *conscious-lifecycle-runtime-last-error* rejection-reason)
                  id))
              (%lifecycle-runtime-transition-unlocked
               lifecycle-id
               (if (%lifecycle-runtime-result-success-p payload)
                   "complete" "fail")
               request-id (gethash "lifecycle_kind" lifecycle) origin
               current-runtime-revision source-event-id
               (gethash "checkpoint_ref" lifecycle)
               (if (%lifecycle-runtime-result-success-p payload)
                   "result-succeeded" "result-failed")
               now agent-id)))))))

(defun conscious-lifecycle-runtime-report ()
  (let ((report (conscious-lifecycle-report
                 *conscious-lifecycle-runtime-projection*)))
    (setf (gethash "last_event_id" report)
          (or *conscious-lifecycle-runtime-last-event-id* :null)
          (gethash "last_error" report)
          (or *conscious-lifecycle-runtime-last-error* :null))
    report))
