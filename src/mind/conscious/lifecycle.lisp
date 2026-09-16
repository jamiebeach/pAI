;;;; lifecycle.lisp -- Q5 event-derived multi-pulse work lifecycles.
;;;;
;;;; Experiences and transitions are immutable events. Current lifecycle state
;;;; is a pure projection over those events. This file performs no append, I/O,
;;;; provider call, effect or publication and registers no worker.

(in-package :agent)

(export '(conscious-lifecycle-project conscious-lifecycle-current
          conscious-lifecycle-awaiting conscious-lifecycle-report
          conscious-lifecycle-transition-payload
          *conscious-lifecycle-schema-version*
          *conscious-lifecycle-kinds* *conscious-lifecycle-transitions*
          *conscious-lifecycle-phases*))

(defparameter *conscious-lifecycle-schema-version* 1)
(defparameter *conscious-lifecycle-kinds*
  '("tool-work" "model-work" "deferred-intention" "scheduled-wake"
    "private-exploration"))
(defparameter *conscious-lifecycle-transitions*
  '("open" "checkpoint" "suspend" "resume" "cancel" "complete" "fail"))
(defparameter *conscious-lifecycle-terminal-statuses*
  '("cancelled" "completed" "failed"))
(defparameter *conscious-lifecycle-phases*
  '("near-term-seeded" "near-term-evolving" "near-term-ready"
    "near-term-blocked" "near-term-expired" "near-term-superseded"
    "near-term-expressed" "near-term-discarded"))
(defparameter *conscious-lifecycle-awaiting-bound* 64)

(defun %lifecycle-present-id-p (value)
  (or (and (integerp value) (plusp value))
      (and (stringp value) (plusp (length value)) (<= (length value) 256))))

(defun %lifecycle-nullable-id-p (value)
  (or (eq value :null) (null value) (%lifecycle-present-id-p value)))

(defun %lifecycle-source-reference-valid-p (payload seen-event-ids)
  "A receipt is provenance only when its same-partition event preceded it."
  (let ((source (gethash "source_event_id" payload)))
    (or (null source)
        (eq source :null)
        (nth-value 1 (gethash source seen-event-ids)))))

(defun %lifecycle-text-p (value maximum)
  (and (stringp value) (plusp (length value)) (<= (length value) maximum)))

(defun %lifecycle-exact-keys-p (table keys)
  (and (hash-table-p table)
       (= (hash-table-count table) (length keys))
       (every (lambda (key) (nth-value 1 (gethash key table))) keys)))

(defun %lifecycle-transition-payload-p (payload)
  (and
   (%lifecycle-exact-keys-p
    payload '("schema_version" "request_id" "lifecycle_id" "lifecycle_kind"
              "transition" "origin_runtime_revision"
              "actor_runtime_revision" "source_event_id" "checkpoint_ref"
              "reason_code" "occurred_at"))
   (eql *conscious-lifecycle-schema-version* (gethash "schema_version" payload))
   (%lifecycle-text-p (gethash "request_id" payload) 256)
   (%lifecycle-text-p (gethash "lifecycle_id" payload) 256)
   (member (gethash "lifecycle_kind" payload) *conscious-lifecycle-kinds*
           :test #'string=)
   (member (gethash "transition" payload) *conscious-lifecycle-transitions*
           :test #'string=)
   (%lifecycle-text-p (gethash "origin_runtime_revision" payload) 256)
   (%lifecycle-text-p (gethash "actor_runtime_revision" payload) 256)
   (%lifecycle-nullable-id-p (gethash "source_event_id" payload))
   (let ((checkpoint (gethash "checkpoint_ref" payload)))
     (or (eq checkpoint :null) (null checkpoint)
         (%lifecycle-text-p checkpoint 512)))
   (%lifecycle-text-p (gethash "reason_code" payload) 128)
   (let ((occurred (gethash "occurred_at" payload)))
     (or (numberp occurred) (%lifecycle-text-p occurred 128)))))

(defun %lifecycle-rejection-payload-p (payload)
  (and
   (%lifecycle-exact-keys-p
    payload '("schema_version" "request_id" "lifecycle_id" "source_event_id"
              "claimed_runtime_revision" "active_runtime_revision"
              "reason_code" "occurred_at"))
   (eql *conscious-lifecycle-schema-version* (gethash "schema_version" payload))
   (%lifecycle-text-p (gethash "request_id" payload) 256)
   (%lifecycle-text-p (gethash "lifecycle_id" payload) 256)
   (%lifecycle-present-id-p (gethash "source_event_id" payload))
   (let ((claimed (gethash "claimed_runtime_revision" payload)))
     (or (eq claimed :null) (null claimed) (%lifecycle-text-p claimed 256)))
   (%lifecycle-text-p (gethash "active_runtime_revision" payload) 256)
   (member (gethash "reason_code" payload)
           '("stale-runtime-revision" "unknown-runtime-revision"
             "correlation-mismatch" "unsupported-result")
           :test #'string=)
   (let ((occurred (gethash "occurred_at" payload)))
     (or (numberp occurred) (%lifecycle-text-p occurred 128)))))

(defun %lifecycle-source-rejection-payload-p (payload)
  (and
   (%lifecycle-exact-keys-p
    payload '("schema_version" "request_id" "lifecycle_id"
              "source_event_id" "actor_runtime_revision" "reason_code"
              "occurred_at"))
   (eql *conscious-lifecycle-schema-version* (gethash "schema_version" payload))
   (%lifecycle-text-p (gethash "request_id" payload) 256)
   (%lifecycle-text-p (gethash "lifecycle_id" payload) 256)
   (%lifecycle-present-id-p (gethash "source_event_id" payload))
   (%lifecycle-text-p (gethash "actor_runtime_revision" payload) 256)
   (string= "illegal-lifecycle-transition" (gethash "reason_code" payload ""))
   (let ((occurred (gethash "occurred_at" payload)))
     (or (numberp occurred) (%lifecycle-text-p occurred 128)))))

(defun conscious-lifecycle-transition-payload
    (lifecycle-id transition &key request-id lifecycle-kind
                                  origin-runtime-revision actor-runtime-revision
                                  (source-event-id :null) (checkpoint-ref :null)
                                  (reason-code "explicit-transition") occurred-at)
  "Build and validate one detached Q5 transition payload."
  (let ((payload
          (obj "schema_version" *conscious-lifecycle-schema-version*
               "request_id" request-id "lifecycle_id" lifecycle-id
               "lifecycle_kind" lifecycle-kind "transition" transition
               "origin_runtime_revision" origin-runtime-revision
               "actor_runtime_revision" actor-runtime-revision
               "source_event_id" source-event-id
               "checkpoint_ref" checkpoint-ref "reason_code" reason-code
               "occurred_at" occurred-at)))
    (unless (%lifecycle-transition-payload-p payload)
      (error "Invalid conscious lifecycle transition payload"))
    (when (and (string= transition "checkpoint")
               (or (null checkpoint-ref) (eq checkpoint-ref :null)))
      (error "Lifecycle checkpoint requires a bounded checkpoint reference"))
    (when (and (member transition '("complete" "fail") :test #'string=)
               (or (null source-event-id) (eq source-event-id :null)))
      (error "Lifecycle ~a requires a source event receipt" transition))
    payload))

(defun %lifecycle-copy-row (row)
  (shasht:read-json (shasht:write-json row nil)))

(defun %lifecycle-open-row (payload event-id)
  (obj "schema_version" *conscious-lifecycle-schema-version*
       "lifecycle_id" (gethash "lifecycle_id" payload)
       "lifecycle_kind" (gethash "lifecycle_kind" payload)
       "status" "active"
       "origin_runtime_revision" (gethash "origin_runtime_revision" payload)
       "actor_runtime_revision" (gethash "actor_runtime_revision" payload)
       "checkpoint_ref" :null
       "opened_event_id" event-id "last_event_id" event-id
       "last_source_event_id" (gethash "source_event_id" payload)
       "terminal_source_event_id" :null
       "last_reason_code" (gethash "reason_code" payload)
       "transition_count" 1))

(defun %lifecycle-transition-allowed-p (status transition)
  (cond
    ((string= status "active")
     (member transition '("checkpoint" "suspend" "cancel" "complete" "fail")
             :test #'string=))
    ((string= status "suspended")
     (member transition '("resume" "cancel" "complete" "fail")
             :test #'string=))
    (t nil)))

(defun %lifecycle-apply-transition (row payload event-id)
  (let ((transition (gethash "transition" payload))
        (source (gethash "source_event_id" payload)))
    (unless (and (string= (gethash "lifecycle_kind" row)
                          (gethash "lifecycle_kind" payload))
                 (string= (gethash "origin_runtime_revision" row)
                          (gethash "origin_runtime_revision" payload))
                 (%lifecycle-transition-allowed-p (gethash "status" row)
                                                  transition))
      (return-from %lifecycle-apply-transition nil))
    (let ((next (%lifecycle-copy-row row)))
      (cond
        ((string= transition "checkpoint")
         (let ((checkpoint (gethash "checkpoint_ref" payload)))
           (when (or (null checkpoint) (eq checkpoint :null))
             (return-from %lifecycle-apply-transition nil))
           (setf (gethash "checkpoint_ref" next) checkpoint)))
        ((string= transition "suspend")
         (setf (gethash "status" next) "suspended"))
        ((string= transition "resume")
         (setf (gethash "status" next) "active"))
        ((string= transition "cancel")
         (setf (gethash "status" next) "cancelled"
               (gethash "terminal_source_event_id" next) source))
        ((string= transition "complete")
         (when (or (null source) (eq source :null))
           (return-from %lifecycle-apply-transition nil))
         (setf (gethash "status" next) "completed"
               (gethash "terminal_source_event_id" next) source))
        ((string= transition "fail")
         (when (or (null source) (eq source :null))
           (return-from %lifecycle-apply-transition nil))
         (setf (gethash "status" next) "failed"
               (gethash "terminal_source_event_id" next) source)))
      (setf (gethash "actor_runtime_revision" next)
            (gethash "actor_runtime_revision" payload)
            (gethash "last_event_id" next) event-id
            (gethash "last_source_event_id" next) source
            (gethash "last_reason_code" next) (gethash "reason_code" payload)
            (gethash "transition_count" next)
            (1+ (gethash "transition_count" row)))
      next)))

(defun %lifecycle-project-transition (payload event-id lifecycles requests)
  "Apply one already shape-valid transition, returning true on success."
  (let* ((request-id (gethash "request_id" payload))
         (lifecycle-id (gethash "lifecycle_id" payload))
         (transition (gethash "transition" payload))
         (row (gethash lifecycle-id lifecycles)))
    (cond
      ((gethash request-id requests) nil)
      ((string= transition "open")
       (when (null row)
         (setf (gethash request-id requests) event-id
               (gethash lifecycle-id lifecycles)
               (%lifecycle-open-row payload event-id))
         t))
      ((null row) nil)
      (t
       (let ((next (%lifecycle-apply-transition row payload event-id)))
         (when next
           (setf (gethash request-id requests) event-id
                 (gethash lifecycle-id lifecycles) next)
           t))))))

(defun conscious-lifecycle-project (events &key agent-id)
  "Purely rebuild Q5 lifecycle state from an authoritative event sequence."
  (let ((lifecycles (make-hash-table :test #'equal))
        (requests (make-hash-table :test #'equal))
        (seen-event-ids (make-hash-table :test #'equal))
        (invalid '())
        (rejected 0)
        (source-rejected 0)
        (highest 0))
    (dolist (event events)
      (when (and (hash-table-p event)
                 (equal agent-id (gethash "agent_id" event)))
        (let ((event-id (gethash "id" event))
              (type (gethash "type" event))
              (payload (gethash "payload" event)))
          (when (and (integerp event-id) (> event-id highest))
            (setf highest event-id))
          (cond
            ((equal type "conscious-lifecycle-transition")
             (unless (and (%lifecycle-transition-payload-p payload)
                          (%lifecycle-source-reference-valid-p
                           payload seen-event-ids)
                          (%lifecycle-project-transition
                           payload event-id lifecycles requests))
               (push event-id invalid)))
            ((equal type "conscious-lifecycle-result-rejected")
             (if (and (%lifecycle-rejection-payload-p payload)
                      (%lifecycle-source-reference-valid-p
                       payload seen-event-ids))
                 (incf rejected)
                 (push event-id invalid)))
            ((equal type "conscious-lifecycle-source-rejected")
             (if (and (%lifecycle-source-rejection-payload-p payload)
                      (null (gethash (gethash "request_id" payload) requests))
                      (%lifecycle-source-reference-valid-p
                       payload seen-event-ids))
                 (progn
                   (setf (gethash (gethash "request_id" payload) requests)
                         event-id)
                   (incf source-rejected))
                 (push event-id invalid))))
          (when (%lifecycle-present-id-p event-id)
            (setf (gethash event-id seen-event-ids) t)))))
    (let ((active 0) (terminal 0))
      (maphash
       (lambda (id row)
         (declare (ignore id))
         (if (member (gethash "status" row)
                     *conscious-lifecycle-terminal-statuses* :test #'string=)
             (incf terminal)
             (incf active)))
       lifecycles)
      (obj "schema_version" *conscious-lifecycle-schema-version*
           "agent_id" (or agent-id :null) "highest_event_id" highest
           "active_count" active "terminal_count" terminal
           "invalid_event_ids" (coerce (nreverse invalid) 'vector)
           "rejected_result_count" rejected
           "source_rejected_count" source-rejected
           "lifecycles" lifecycles))))

(defun conscious-lifecycle-current (projection lifecycle-id)
  "Return a detached current lifecycle row, or NIL."
  (let ((table (and (hash-table-p projection) (gethash "lifecycles" projection))))
    (let ((row (and (hash-table-p table) (gethash lifecycle-id table))))
      (and row (%lifecycle-copy-row row)))))

(defun conscious-lifecycle-awaiting
    (projection &key (bound *conscious-lifecycle-awaiting-bound*))
  "Return bounded non-terminal lifecycle references in stable identity order."
  (unless (and (integerp bound) (<= 0 bound 1024))
    (error "Lifecycle awaiting bound is invalid"))
  (let ((table (and (hash-table-p projection) (gethash "lifecycles" projection)))
        (ids '()))
    (when (hash-table-p table)
      (maphash
       (lambda (id row)
         (unless (member (gethash "status" row)
                         *conscious-lifecycle-terminal-statuses* :test #'string=)
           (push id ids)))
       table))
    (coerce
     (loop for id in (sort ids #'string<)
           repeat bound
           for row = (gethash id table)
           collect
           (obj "lifecycle_id" id
                "lifecycle_kind" (gethash "lifecycle_kind" row)
                "status" (gethash "status" row)
                "origin_runtime_revision"
                (gethash "origin_runtime_revision" row)
                "checkpoint_ref" (gethash "checkpoint_ref" row)
                "phase"
                (let ((reason (gethash "last_reason_code" row)))
                  (if (member reason *conscious-lifecycle-phases*
                              :test #'string=)
                      reason :null))
                "last_event_id" (gethash "last_event_id" row)))
     'vector)))

(defun conscious-lifecycle-report (projection)
  "Content-free lifecycle counts safe for runtime/operator diagnostics."
  (unless (hash-table-p projection)
    (return-from conscious-lifecycle-report
      (obj "schema_version" *conscious-lifecycle-schema-version*
           "state" "unavailable" "active_count" 0 "terminal_count" 0
           "invalid_event_count" 0 "rejected_result_count" 0
           "source_rejected_count" 0)))
  (obj "schema_version" (gethash "schema_version" projection)
       "state" "projected"
       "highest_event_id" (gethash "highest_event_id" projection)
       "active_count" (gethash "active_count" projection)
       "terminal_count" (gethash "terminal_count" projection)
       "invalid_event_count" (length (gethash "invalid_event_ids" projection))
       "rejected_result_count" (gethash "rejected_result_count" projection)
       "source_rejected_count" (gethash "source_rejected_count" projection 0)))
