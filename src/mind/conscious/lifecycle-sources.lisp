;;;; lifecycle-sources.lisp -- Q5 typed producer to lifecycle reconciliation.
;;;;
;;;; This adapter consumes already-durable source events. It creates no work,
;;;; calls no provider/effect/publication route and owns no worker.

(in-package :agent)

(export '(conscious-lifecycle-source-descriptor
          conscious-lifecycle-runtime-reconcile-producer-events
          conscious-lifecycle-context-records
          conscious-lifecycle-source-report))

(defparameter *conscious-lifecycle-near-term-revision*
  "near-term-intentions-v1")
(defparameter *conscious-lifecycle-near-term-states*
  '("seeded" "evolving" "ready" "blocked" "expired" "superseded"
    "expressed" "discarded"))
(defparameter *conscious-lifecycle-near-term-transition-map*
  '(("near-term-intention-created" "seeded" "open")
    ("near-term-intention-transition" "seeded" "checkpoint")
    ("near-term-intention-transition" "evolving" "checkpoint")
    ("near-term-intention-transition" "ready" "checkpoint")
    ("near-term-intention-transition" "expressed" "complete")
    ("near-term-intention-transition" "blocked" "fail")
    ("near-term-intention-transition" "expired" "cancel")
    ("near-term-intention-transition" "superseded" "cancel")
    ("near-term-intention-transition" "discarded" "cancel")))
(defparameter *conscious-lifecycle-source-diagnostic-bound* 32)
(defvar *conscious-lifecycle-source-lock*
  (bt:make-lock "conscious-lifecycle-sources"))
(defvar *conscious-lifecycle-source-last-report* nil)

(defun %lifecycle-source-event-type-p (type)
  (member type '("near-term-intention-created"
                 "near-term-intention-transition")
          :test #'string=))

(defun %lifecycle-source-transition-for-state (type state)
  (third
   (find-if (lambda (row)
              (and (string= type (first row))
                   (string= state (second row))))
            *conscious-lifecycle-near-term-transition-map*)))

(defun %lifecycle-source-fnv (text)
  (let ((hash #xcbf29ce484222325))
    (loop for character across text
          do (setf hash
                   (ldb (byte 64 0)
                        (* (logxor hash (char-code character))
                           #x100000001b3))))
    (format nil "~16,'0x" hash)))

(defun %lifecycle-source-request-id
    (event-id type intention-id receipt-id state)
  ;; Event IDs are unique going forward, but the preserved legacy log contains
  ;; duplicates. Include the bounded source envelope in a stable digest so two
  ;; old events cannot alias an idempotency identity merely by sharing an ID.
  (format nil "q5-near-term-source:~a"
          (%lifecycle-source-fnv
           (format nil "~a|~a|~a|~a|~a"
                   event-id type intention-id receipt-id state))))

(defun conscious-lifecycle-source-descriptor (event)
  "Return a bounded lifecycle transition descriptor for one source event.
NIL means the event is not a supported producer event or fails its envelope."
  (unless (hash-table-p event)
    (return-from conscious-lifecycle-source-descriptor nil))
  (let* ((type (gethash "type" event))
         (event-id (gethash "id" event))
         (payload (gethash "payload" event)))
    (unless (and (stringp type) (%lifecycle-source-event-type-p type)
                 (%lifecycle-present-id-p event-id)
                 (hash-table-p payload)
                 (eql 1 (gethash "schema_version" payload)))
      (return-from conscious-lifecycle-source-descriptor nil))
    (let* ((intention-id (gethash "intention_id" payload))
           (receipt-id (gethash "receipt_id" payload))
           (state (gethash "state" payload))
           (transition
             (and (%lifecycle-text-p state 32)
                  (member state *conscious-lifecycle-near-term-states*
                          :test #'string=)
                  (%lifecycle-source-transition-for-state type state)))
           (request-id
             (and (stringp intention-id) (stringp receipt-id) (stringp state)
                  (%lifecycle-source-request-id
                   event-id type intention-id receipt-id state))))
      (unless (and (%lifecycle-text-p intention-id 220)
                   (%lifecycle-text-p receipt-id 512)
                   transition request-id)
        (return-from conscious-lifecycle-source-descriptor nil))
      (let ((lifecycle-id (format nil "near-term:~a" intention-id)))
        (unless (%lifecycle-text-p lifecycle-id 256)
          (return-from conscious-lifecycle-source-descriptor nil))
        (obj "source_event_id" event-id
             "request_id" request-id
             "lifecycle_id" lifecycle-id
             "lifecycle_kind" "deferred-intention"
             "transition" transition
             "origin_runtime_revision"
             *conscious-lifecycle-near-term-revision*
             "checkpoint_ref"
             (if (string= transition "checkpoint") receipt-id :null)
             "reason_code" (format nil "near-term-~a" state))))))

(defun %lifecycle-source-safe-report
    (state examined appended recovered invalid rejected operational
     invalid-ids rejected-ids)
  (obj "schema_version" 1 "state" state
       "examined_count" examined "appended_count" appended
       "recovered_count" recovered "invalid_count" invalid
       "rejected_count" rejected
       "operational_failure_count" operational
       "diagnostic_id_bound" *conscious-lifecycle-source-diagnostic-bound*
       "invalid_ids_truncated"
       (if (> invalid (length invalid-ids)) t nil)
       "rejected_ids_truncated"
       (if (> rejected (length rejected-ids)) t nil)
       "invalid_source_event_ids" (coerce (nreverse invalid-ids) 'vector)
       "rejected_source_event_ids" (coerce (nreverse rejected-ids) 'vector)))

(defun %lifecycle-source-record-id (id ids)
  (if (< (length ids) *conscious-lifecycle-source-diagnostic-bound*)
      (cons id ids)
      ids))

(defun %lifecycle-source-rejection-request-id (request-id)
  (format nil "~a:rejected" request-id))

(defun %lifecycle-source-rejection-payload
    (descriptor actor-runtime-revision occurred-at)
  (obj "schema_version" 1
       "request_id"
       (%lifecycle-source-rejection-request-id
        (gethash "request_id" descriptor))
       "lifecycle_id" (gethash "lifecycle_id" descriptor)
       "source_event_id" (gethash "source_event_id" descriptor)
       "actor_runtime_revision" actor-runtime-revision
       "reason_code" "illegal-lifecycle-transition"
       "occurred_at" occurred-at))

(defun %lifecycle-source-index-events (events agent-id)
  (let ((requests (make-hash-table :test #'equal))
        (newest-sources (make-hash-table :test #'equal)))
    (dolist (event events)
      (when (and (hash-table-p event)
                 (equal agent-id (gethash "agent_id" event)))
        (let ((type (gethash "type" event))
              (payload (gethash "payload" event)))
          (when (%lifecycle-source-event-type-p type)
            (setf (gethash (gethash "id" event) newest-sources) event))
          (when (and (member type
                             '("conscious-lifecycle-transition"
                               "conscious-lifecycle-result-rejected"
                               "conscious-lifecycle-source-rejected")
                             :test #'string=)
                     (hash-table-p payload)
                     (stringp (gethash "request_id" payload)))
            (setf (gethash (gethash "request_id" payload) requests) event)))))
    (values requests newest-sources)))

(defun %lifecycle-source-existing-exact-p (existing type payload)
  (and (hash-table-p existing)
       (string= type (gethash "type" existing ""))
       (%lifecycle-runtime-payload-identity-equal-p
        payload (gethash "payload" existing))))

(defun conscious-lifecycle-runtime-reconcile-producer-events
    (agent-id &key actor-runtime-revision)
  "Materialize supported durable producer events as idempotent lifecycles."
  (unless (and (%lifecycle-text-p agent-id 256)
               (%lifecycle-text-p actor-runtime-revision 256))
    (error "Lifecycle source reconciliation identity is invalid"))
  (bt:with-lock-held (*conscious-lifecycle-source-lock*)
    ;; Read after acquiring the reconciliation lock. A snapshot supplied by a
    ;; caller before it waited for this lock could miss the preceding run's
    ;; transition/rejection and defeat idempotency under concurrency.
    (let ((events (%lifecycle-runtime-events))
          (examined 0) (appended 0) (recovered 0)
          (invalid 0) (rejected 0) (operational 0)
          (invalid-ids '()) (rejected-ids '()))
      (labels ((report (state)
                 (%lifecycle-source-safe-report
                  state examined appended recovered invalid rejected operational
                  invalid-ids rejected-ids))
               (fail (condition)
                 (incf operational)
                 (setf *conscious-lifecycle-source-last-report* (report "failed"))
                 (error condition)))
        (handler-case
            (multiple-value-bind (requests newest-sources)
                (%lifecycle-source-index-events events agent-id)
              (dolist (event events)
                (when (and (hash-table-p event)
                           (equal agent-id (gethash "agent_id" event))
                           (%lifecycle-source-event-type-p
                            (gethash "type" event)))
                  (incf examined)
                  (let* ((event-id (gethash "id" event))
                         (descriptor
                           (and (eq event (gethash event-id newest-sources))
                                (conscious-lifecycle-source-descriptor event))))
                    (if (null descriptor)
                        (progn
                          (incf invalid)
                          (setf invalid-ids
                                (%lifecycle-source-record-id event-id invalid-ids)))
                        (let* ((request-id (gethash "request_id" descriptor))
                               (transition-payload
                                 (conscious-lifecycle-transition-payload
                                  (gethash "lifecycle_id" descriptor)
                                  (gethash "transition" descriptor)
                                  :request-id request-id
                                  :lifecycle-kind
                                  (gethash "lifecycle_kind" descriptor)
                                  :origin-runtime-revision
                                  (gethash "origin_runtime_revision" descriptor)
                                  :actor-runtime-revision actor-runtime-revision
                                  :source-event-id
                                  (gethash "source_event_id" descriptor)
                                  :checkpoint-ref
                                  (gethash "checkpoint_ref" descriptor)
                                  :reason-code (gethash "reason_code" descriptor)
                                  :occurred-at (or (gethash "timestamp" event) 0)))
                               (rejection-payload
                                 (%lifecycle-source-rejection-payload
                                  descriptor actor-runtime-revision
                                  (or (gethash "timestamp" event) 0)))
                               (rejection-request
                                 (gethash "request_id" rejection-payload))
                               (existing (gethash request-id requests))
                               (existing-rejection
                                 (gethash rejection-request requests)))
                          (cond
                            (existing
                             (unless (%lifecycle-source-existing-exact-p
                                      existing "conscious-lifecycle-transition"
                                      transition-payload)
                               (error "Lifecycle source request ~s conflicts with durable history"
                                      request-id))
                             (incf recovered))
                            (existing-rejection
                             (unless (%lifecycle-source-existing-exact-p
                                      existing-rejection
                                      "conscious-lifecycle-source-rejected"
                                      rejection-payload)
                               (error "Lifecycle source rejection ~s conflicts with durable history"
                                      rejection-request))
                             ;; Existing durable dispositions are recovery,
                             ;; not new rejections in this run. The projection
                             ;; owns the cumulative rejection count.
                             (incf recovered))
                            (t
                             (handler-case
                                 (progn
                                   (conscious-lifecycle-runtime-transition
                                    (gethash "lifecycle_id" descriptor)
                                    (gethash "transition" descriptor)
                                    :request-id request-id
                                    :lifecycle-kind
                                    (gethash "lifecycle_kind" descriptor)
                                    :origin-runtime-revision
                                    (gethash "origin_runtime_revision" descriptor)
                                    :actor-runtime-revision actor-runtime-revision
                                    :source-event-id event-id
                                    :checkpoint-ref
                                    (gethash "checkpoint_ref" descriptor)
                                    :reason-code
                                    (gethash "reason_code" descriptor)
                                    :now (or (gethash "timestamp" event) 0)
                                    :agent-id agent-id)
                                   (incf appended))
                               (conscious-lifecycle-transition-rejected
                                   (condition)
                                 (unless (eq :illegal-transition
                                             (conscious-lifecycle-rejection-code
                                              condition))
                                   (error condition))
                                 (multiple-value-bind
                                       (id stored all-events)
                                     (%lifecycle-runtime-append-readable
                                      "conscious-lifecycle-source-rejected"
                                      rejection-payload agent-id
                                      :caused-by event-id)
                                   (declare (ignore id all-events))
                                   (setf (gethash rejection-request requests)
                                         stored)
                                   (incf appended)
                                   (incf rejected)
                                   (setf rejected-ids
                                         (%lifecycle-source-record-id
                                          event-id rejected-ids))))))))))))
              ;; The locked input snapshot remains authoritative when this
              ;; pass appended nothing. If it did append, reread once after
              ;; the last transition so callers receive the complete durable
              ;; boundary snapshot as a second value.
              (let ((all-events (if (plusp appended)
                                    (%lifecycle-runtime-events)
                                    events)))
                (setf *conscious-lifecycle-runtime-projection*
                      (conscious-lifecycle-project all-events :agent-id agent-id)
                      *conscious-lifecycle-runtime-agent-id* agent-id
                      *conscious-lifecycle-source-last-report*
                      (report "reconciled"))
                (values *conscious-lifecycle-source-last-report* all-events)))
          (conscious-lifecycle-transition-rejected (condition)
            ;; A typed rejection is consumed only around the one transition
            ;; attempt above. Reaching here is an adapter/programming failure.
            (fail condition))
          (error (condition) (fail condition)))))))

(defun %lifecycle-source-context-content (row)
  (let* ((checkpoint (gethash "checkpoint_ref" row))
         (phase (gethash "phase" row))
         (content
           (format nil "Lifecycle ~a (~a) is ~a~@[; phase ~a~]~@[; checkpoint ~a~]."
                   (gethash "lifecycle_id" row)
                   (gethash "lifecycle_kind" row)
                   (gethash "status" row)
                   (unless (or (null phase) (eq phase :null)) phase)
                   (unless (or (null checkpoint) (eq checkpoint :null))
                     checkpoint))))
    (subseq content 0 (min 768 (length content)))))

(defun conscious-lifecycle-context-records (awaiting)
  "Convert bounded awaited references to context-assembler records."
  (let ((rows (cond ((vectorp awaiting) (coerce awaiting 'list))
                    ((listp awaiting) awaiting)
                    (t nil))))
    (coerce
     (loop for row in rows
           when (and (hash-table-p row)
                     (%lifecycle-text-p (gethash "lifecycle_id" row) 256)
                     (%lifecycle-text-p (gethash "lifecycle_kind" row) 64)
                     (%lifecycle-text-p (gethash "status" row) 32)
                     (let ((phase (gethash "phase" row)))
                       (or (null phase) (eq phase :null)
                           (member phase *conscious-lifecycle-phases*
                                   :test #'string=)))
                     (%lifecycle-present-id-p (gethash "last_event_id" row)))
             collect
             (obj "source_id" (gethash "last_event_id" row)
                  "content" (%lifecycle-source-context-content row)))
     'vector)))

(defun conscious-lifecycle-source-report ()
  (or *conscious-lifecycle-source-last-report*
      (obj "schema_version" 1 "state" "unavailable"
           "examined_count" 0 "appended_count" 0 "recovered_count" 0
           "invalid_count" 0 "rejected_count" 0
           "operational_failure_count" 0 "diagnostic_id_bound" 32
           "invalid_ids_truncated" nil "rejected_ids_truncated" nil
           "invalid_source_event_ids" (vector)
           "rejected_source_event_ids" (vector))))
