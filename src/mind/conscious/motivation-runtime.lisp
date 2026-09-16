;;;; motivation-runtime.lisp -- Q5M2 durable curiosity candidate adapter.
;;;;
;;;; Reconciles the pure motivational projection into the ordinary durable
;;;; attention path. It owns no worker, provider, tool, effect or publication.

(in-package :agent)

(export '(conscious-motivation-codelet-install
          conscious-motivation-runtime-reconcile
          conscious-motivation-runtime-report))

(defparameter *conscious-motivation-candidate-event-type*
  "conscious-curiosity-candidate-raised")
(defparameter *conscious-motivation-candidate-phases*
  '("salient" "ready-for-opportunity" "reassessed"))
(defparameter *conscious-motivation-candidate-bands* '("salient" "ready"))
(defvar *conscious-motivation-runtime-lock*
  (bt:make-lock "conscious-motivation-runtime"))
(defvar *conscious-motivation-runtime-last-report* nil)

(defun %conscious-motivation-codelet (stimulus context)
  (declare (ignore context))
  (when (and (string= "intention-cue" (gethash "kind" stimulus ""))
             (string= "curiosity" (gethash "sub_kind" stimulus "")))
    (make-assessment
     :codelet "curiosity-motive" :concern "curiosity motive"
     :stimulus-id (gethash "stimulus_id" stimulus)
     :evidence-ids (coerce (gethash "source_event_ids" stimulus) 'list)
     :priority-class "ambient" :urgency "background"
     :explanation-code "curiosity-salient")))

(defun conscious-motivation-codelet-install ()
  "Install the bounded Q5M attention interpretation; starts no work."
  (register-codelet "curiosity-motive" 50
                    #'%conscious-motivation-codelet
                    :digest "q5m-curiosity-codelet-v1"))

(define-init :install conscious-motivation-codelet
  "Install the pure curiosity-candidate attention codelet."
  (conscious-motivation-codelet-install))

(defun %motivation-runtime-events ()
  (cond ((fboundp 'event-projection-events)
         (funcall 'event-projection-events))
        ((fboundp 'replay-events) (funcall 'replay-events))
        (t (error "Motivation runtime requires the event replay port"))))

(defun %motivation-runtime-request-id
    (candidate composition-hash origin-runtime-revision)
  (format nil "q5m-candidate:~a"
          (%motivation-fnv
           (with-output-to-string (out)
             (prin1 (list (gethash "candidate_id" candidate)
                          composition-hash origin-runtime-revision)
                    out)))))

(defun %motivation-runtime-payload-canonical (payload)
  (with-output-to-string (out)
    (prin1
     (list (gethash "schema_version" payload)
           (gethash "request_id" payload)
           (gethash "candidate_id" payload)
           (gethash "motive_id" payload)
           (gethash "motive_kind" payload)
           (gethash "phase" payload)
           (gethash "activation_band" payload)
           (coerce (gethash "source_event_ids" payload (vector)) 'list)
           (gethash "latest_event_id" payload)
           (gethash "expression_policy" payload)
           (gethash "projection_composition_hash" payload)
           (gethash "origin_runtime_revision" payload)
           (gethash "actor_runtime_revision" payload)
           (gethash "raised_at" payload))
     out)))

(defun %motivation-runtime-candidate-payload
    (candidate composition-hash origin-runtime-revision
     actor-runtime-revision raised-at)
  (let* ((request-id
           (%motivation-runtime-request-id
            candidate composition-hash origin-runtime-revision))
         (payload
           (obj "schema_version" 1 "request_id" request-id
                "candidate_id" (gethash "candidate_id" candidate)
                "motive_id" (gethash "motive_id" candidate)
                "motive_kind" (gethash "motive_kind" candidate)
                "phase" (gethash "phase" candidate)
                "activation_band" (gethash "activation_band" candidate)
                "source_event_ids"
                (copy-seq (gethash "source_event_ids" candidate))
                "latest_event_id" (gethash "latest_event_id" candidate)
                "expression_policy" (gethash "expression_policy" candidate)
                "projection_composition_hash" composition-hash
                "origin_runtime_revision" origin-runtime-revision
                "actor_runtime_revision" actor-runtime-revision
                "raised_at" raised-at "integrity_hash" :null)))
    (setf (gethash "integrity_hash" payload)
          (%motivation-fnv (%motivation-runtime-payload-canonical payload)))
    payload))

(defun %motivation-runtime-payload-valid-p (payload)
  (and
   (%lifecycle-exact-keys-p
    payload
    '("schema_version" "request_id" "candidate_id" "motive_id"
      "motive_kind" "phase" "activation_band" "source_event_ids"
      "latest_event_id" "expression_policy" "projection_composition_hash"
      "origin_runtime_revision" "actor_runtime_revision" "raised_at"
      "integrity_hash"))
   (eql 1 (gethash "schema_version" payload))
   (%lifecycle-text-p (gethash "request_id" payload) 256)
   (%lifecycle-text-p (gethash "candidate_id" payload) 512)
   (%lifecycle-text-p (gethash "motive_id" payload) 256)
   (string= "curiosity" (gethash "motive_kind" payload ""))
   (member (gethash "phase" payload)
           *conscious-motivation-candidate-phases* :test #'string=)
   (member (gethash "activation_band" payload)
           *conscious-motivation-candidate-bands* :test #'string=)
   ;; Candidate-event schema bound, independent of a mutable live policy.
   ;; The pure projector may use a stricter pinned evidence bound.
   (%motivation-id-items (gethash "source_event_ids" payload) 64)
   (%lifecycle-present-id-p (gethash "latest_event_id" payload))
   (find (gethash "latest_event_id" payload)
         (gethash "source_event_ids" payload) :test #'equal)
   (string= "private-consideration-only"
            (gethash "expression_policy" payload ""))
   (%lifecycle-text-p (gethash "projection_composition_hash" payload) 128)
   (%lifecycle-text-p (gethash "origin_runtime_revision" payload) 256)
   (%lifecycle-text-p (gethash "actor_runtime_revision" payload) 256)
   (integerp (gethash "raised_at" payload))
   (not (minusp (gethash "raised_at" payload)))
   (string=
    (gethash "integrity_hash" payload "")
    (%motivation-fnv (%motivation-runtime-payload-canonical payload)))))

(defun %motivation-runtime-identity-equal-p (left right)
  (and (%motivation-runtime-payload-valid-p left)
       (%motivation-runtime-payload-valid-p right)
       (loop for key being the hash-keys of left using (hash-value value)
             always
             (multiple-value-bind (other present) (gethash key right)
               (and present
                    (or (member key '("actor_runtime_revision" "raised_at"
                                      "integrity_hash")
                                :test #'string=)
                        (equalp value other)))))))

(defun %motivation-runtime-request-index (events agent-id)
  (let ((index (make-hash-table :test #'equal)))
    (dolist (event events index)
      (when (and (hash-table-p event)
                 (equal agent-id (gethash "agent_id" event))
                 (string= *conscious-motivation-candidate-event-type*
                          (gethash "type" event "")))
        (let ((payload (gethash "payload" event)))
          (when (and (hash-table-p payload)
                     (stringp (gethash "request_id" payload)))
            (let ((request-id (gethash "request_id" payload)))
              (when (gethash request-id index)
                (error "Duplicate durable motivation request ~s" request-id))
              (setf (gethash request-id index) event))))))))

(defun %motivation-runtime-stored-event (events id agent-id)
  (find-if (lambda (event)
             (and (hash-table-p event)
                  (equal id (gethash "id" event))
                  (equal agent-id (gethash "agent_id" event))
                  (string= *conscious-motivation-candidate-event-type*
                           (gethash "type" event ""))))
           events :from-end t))

(defun %motivation-runtime-safe-report
    (state examined appended recovered operational)
  (obj "schema_version" 1 "state" state
       "examined_count" examined "appended_count" appended
       "recovered_count" recovered
       "operational_failure_count" operational))

(defun conscious-motivation-runtime-reconcile
    (agent-id &key actor-runtime-revision origin-runtime-revision now)
  "Materialize eligible motive revisions as durable attention events."
  (unless (and (%lifecycle-text-p agent-id 256)
               (%lifecycle-text-p actor-runtime-revision 256)
               (%lifecycle-text-p origin-runtime-revision 256)
               (integerp now) (not (minusp now)))
    (error "Motivation reconciliation identity is invalid"))
  (bt:with-lock-held (*conscious-motivation-runtime-lock*)
    ;; Read after taking the lock. A caller-supplied snapshot can become stale
    ;; while waiting and miss a candidate appended by the preceding reconciler.
    (let ((events (%motivation-runtime-events))
          (examined 0) (appended 0) (recovered 0) (operational 0))
      (labels ((report (state)
                 (%motivation-runtime-safe-report
                  state examined appended recovered operational))
               (fail (condition)
                 (incf operational)
                 (setf *conscious-motivation-runtime-last-report*
                       (report "failed"))
                 (error condition)))
        (handler-case
            (let* ((projection
                     (conscious-motivation-project
                      events :now now :agent-id agent-id
                      :context
                      (make-projection-context
                       :now now :agent-id agent-id
                       :runtime-revision origin-runtime-revision)))
                   (composition-hash
                     (gethash "composition_hash" projection))
                   (candidates (conscious-motivation-candidates projection))
                   (requests (%motivation-runtime-request-index events agent-id)))
              (map nil
                   (lambda (candidate)
                     (incf examined)
                     (let* ((payload
                              (%motivation-runtime-candidate-payload
                               candidate composition-hash
                               origin-runtime-revision actor-runtime-revision
                               now))
                            (request-id (gethash "request_id" payload))
                            (existing (gethash request-id requests)))
                       (unless (%motivation-runtime-payload-valid-p payload)
                         (error "Derived motivation candidate payload is invalid"))
                       (if existing
                           (progn
                             (unless (%motivation-runtime-identity-equal-p
                                      payload (gethash "payload" existing))
                               (error "Motivation request ~s conflicts with durable history"
                                      request-id))
                             (incf recovered))
                           (progn
                             (unless (fboundp 'log-event)
                               (error "Motivation runtime requires the event append port"))
                             (let ((id
                                     (funcall
                                      'log-event
                                      *conscious-motivation-candidate-event-type*
                                      payload
                                      :caused-by
                                      (gethash "latest_event_id" candidate))))
                               (unless id
                                 (error "Motivation candidate append returned no id"))
                               (setf events (%motivation-runtime-events))
                               (let ((stored
                                       (%motivation-runtime-stored-event
                                        events id agent-id)))
                                 (unless (and stored
                                              (%motivation-runtime-identity-equal-p
                                               payload (gethash "payload" stored)))
                                   (error "Motivation candidate ~s was not durably readable"
                                          id))
                                 (setf (gethash request-id requests) stored)
                                 (incf appended)))))))
                   candidates)
              (setf *conscious-motivation-runtime-last-report*
                    (report "reconciled"))
              (values *conscious-motivation-runtime-last-report* events))
          (error (condition) (fail condition)))))))

(defun conscious-motivation-runtime-report ()
  (or *conscious-motivation-runtime-last-report*
      (%motivation-runtime-safe-report "unavailable" 0 0 0 0)))
