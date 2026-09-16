;;;; motivation.lisp -- Q5M rebuildable curiosity dynamics.
;;;;
;;;; Pure projection only. This file owns no append adapter, worker, model,
;;;; tool, publication route or runtime installation.

(in-package :agent)

(export '(conscious-curiosity-observation-payload
          conscious-curiosity-opportunity-payload
          conscious-curiosity-satisfaction-payload
          conscious-motivation-project conscious-motivation-candidates
          conscious-motivation-report
          *conscious-motivation-schema-version*))

(defparameter *conscious-motivation-schema-version* 1)
(defparameter *conscious-motivation-kind* "curiosity")
(defparameter *conscious-motivation-reinforcement-kinds*
  '("novel-observation" "unresolved-recurrence" "operator-interest"
    "contradictory-evidence"))
(defparameter *conscious-motivation-opportunity-states*
  '("suitable" "unsuitable"))
(defparameter *conscious-motivation-satisfaction-degrees*
  '("partial" "full"))

(defun %motivation-null-p (value)
  (or (null value) (eq value :null)))

(defun %motivation-policy-value (policy key)
  (cdr (assoc key policy)))

(defun %motivation-policy-valid-p (policy)
  (and (listp policy)
       (eql 1 (%motivation-policy-value policy :schema-version))
       (let ((weights (%motivation-policy-value
                       policy :reinforcement-weights)))
         (and (listp weights)
              (every (lambda (kind)
                       (let ((weight (cdr (assoc kind weights :test #'string=))))
                         (and (integerp weight) (plusp weight)
                              (<= weight 1000))))
                     *conscious-motivation-reinforcement-kinds*)))
       (every (lambda (key)
                (let ((value (%motivation-policy-value policy key)))
                  (and (integerp value) (not (minusp value)))))
              '(:decay-milliunits-per-hour :rising-threshold
                :salient-threshold :partial-satisfaction-drop
                :full-satisfaction-level :refractory-seconds
                :motive-bound :subject-ref-bound :evidence-root-bound
                :invalid-diagnostic-bound))
       (< (%motivation-policy-value policy :rising-threshold)
          (%motivation-policy-value policy :salient-threshold))
       (<= (%motivation-policy-value policy :salient-threshold) 1000)
       (string= "settled-then-lowest-nonsalient-v1"
                (%motivation-policy-value policy :active-retirement-policy))
       (%lifecycle-text-p
        (%motivation-policy-value policy :expression-policy) 128)))

(defun %motivation-fnv (text)
  (let ((hash #xcbf29ce484222325))
    (loop for character across text
          do (setf hash
                   (ldb (byte 64 0)
                        (* (logxor hash (char-code character))
                           #x100000001b3))))
    (format nil "~16,'0x" hash)))

(defun %motivation-items (value maximum)
  (let ((items (cond ((vectorp value) (coerce value 'list))
                     ((listp value) (copy-list value))
                     (t nil))))
    (and items (<= (length items) maximum) items)))

(defun %motivation-id-items (value maximum &key (nonempty t))
  (let ((items (%motivation-items value maximum)))
    (and items
         (or (not nonempty) (plusp (length items)))
         (every #'%lifecycle-present-id-p items)
         (= (length items) (length (remove-duplicates items :test #'equal)))
         items)))

(defun %motivation-string-items (value maximum)
  (let ((items (%motivation-items value maximum)))
    (and items (plusp (length items))
         (every (lambda (item) (%lifecycle-text-p item 256)) items)
         (= (length items) (length (remove-duplicates items :test #'string=)))
         items)))

(defun %motivation-sorted-strings (items)
  (sort (copy-list items) #'string<))

(defun %motivation-sorted-ids (items)
  (sort (copy-list items)
        (lambda (left right)
          (string< (princ-to-string left) (princ-to-string right)))))

(defun %motivation-derived-id
    (kind mind-identity-id subject-type subject-refs)
  (format nil "motive:~a:~a"
          kind
          (%motivation-fnv
           (with-output-to-string (out)
             (prin1 (list kind mind-identity-id subject-type
                          (%motivation-sorted-strings subject-refs)) out)))))

(defun %motivation-observation-canonical (payload)
  (with-output-to-string (out)
    (prin1
     (list (gethash "schema_version" payload)
           (gethash "motive_kind" payload)
           (gethash "mind_identity_id" payload)
           (gethash "subject_type" payload)
           (gethash "subject_label" payload)
           (coerce (gethash "subject_refs" payload (vector)) 'list)
           (gethash "reinforcement_kind" payload)
           (coerce (gethash "supporting_event_ids" payload (vector)) 'list)
           (gethash "source_revision" payload)
           (gethash "observed_at" payload)
           (gethash "motive_id" payload))
     out)))

(defun %motivation-transition-canonical (payload fields)
  (with-output-to-string (out)
    (prin1 (mapcar (lambda (field) (gethash field payload)) fields) out)))

(defun %motivation-observation-valid-p (payload policy)
  (let* ((ref-bound (%motivation-policy-value policy :subject-ref-bound))
         (evidence-bound (%motivation-policy-value policy :evidence-root-bound))
         (refs (%motivation-string-items
                (and (hash-table-p payload) (gethash "subject_refs" payload))
                ref-bound))
         (roots (%motivation-id-items
                 (and (hash-table-p payload)
                      (gethash "supporting_event_ids" payload))
                 evidence-bound)))
    (and
     (hash-table-p payload)
     (%lifecycle-exact-keys-p
      payload
      '("schema_version" "request_id" "motive_kind" "mind_identity_id"
        "subject_type" "subject_label" "subject_refs" "reinforcement_kind"
        "supporting_event_ids" "source_revision" "actor_runtime_revision"
        "observed_at" "motive_id" "integrity_hash"))
     (eql 1 (gethash "schema_version" payload))
     (%lifecycle-text-p (gethash "request_id" payload) 256)
     (string= *conscious-motivation-kind*
              (gethash "motive_kind" payload ""))
     (%lifecycle-text-p (gethash "mind_identity_id" payload) 128)
     (member (gethash "subject_type" payload)
             *conscious-lifecycle-semantic-subject-types* :test #'string=)
     (%lifecycle-text-p (gethash "subject_label" payload) 512)
     refs roots
     (member (gethash "reinforcement_kind" payload)
             *conscious-motivation-reinforcement-kinds* :test #'string=)
     (%lifecycle-text-p (gethash "source_revision" payload) 256)
     (%lifecycle-text-p (gethash "actor_runtime_revision" payload) 256)
     (integerp (gethash "observed_at" payload))
     (not (minusp (gethash "observed_at" payload)))
     (string=
      (gethash "motive_id" payload "")
      (%motivation-derived-id
       *conscious-motivation-kind* (gethash "mind_identity_id" payload)
       (gethash "subject_type" payload) refs))
     (string= (gethash "integrity_hash" payload "")
              (%motivation-fnv
               (%motivation-observation-canonical payload))))))

(defun conscious-curiosity-observation-payload
    (&key request-id mind-identity-id subject-type subject-label subject-refs
          reinforcement-kind supporting-event-ids source-revision
          actor-runtime-revision observed-at
          (policy *curiosity-motivation-policy*))
  "Build one detached, exact Q5M curiosity observation payload."
  (unless (%motivation-policy-valid-p policy)
    (error "Curiosity motivation policy is invalid"))
  (let* ((refs (%motivation-string-items
                subject-refs
                (%motivation-policy-value policy :subject-ref-bound)))
         (refs (and refs (%motivation-sorted-strings refs)))
         (motive-id (and refs (%motivation-derived-id
                              *conscious-motivation-kind* mind-identity-id
                              subject-type refs)))
         (payload
           (obj "schema_version" 1 "request_id" request-id
                "motive_kind" *conscious-motivation-kind*
                "mind_identity_id" mind-identity-id
                "subject_type" subject-type "subject_label" subject-label
                "subject_refs" (coerce (or refs '()) 'vector)
                "reinforcement_kind" reinforcement-kind
                "supporting_event_ids"
                (coerce (or (%motivation-id-items
                             supporting-event-ids
                             (%motivation-policy-value
                              policy :evidence-root-bound))
                            '()) 'vector)
                "source_revision" source-revision
                "actor_runtime_revision" actor-runtime-revision
                "observed_at" observed-at "motive_id" (or motive-id :null)
                "integrity_hash" :null)))
    (setf (gethash "integrity_hash" payload)
          (%motivation-fnv (%motivation-observation-canonical payload)))
    (unless (%motivation-observation-valid-p payload policy)
      (error "Curiosity observation does not match its closed schema"))
    payload))

(defun %motivation-opportunity-valid-p (payload policy)
  (let ((roots (%motivation-id-items
                (and (hash-table-p payload)
                     (gethash "supporting_event_ids" payload))
                (%motivation-policy-value policy :evidence-root-bound))))
    (and (hash-table-p payload)
         (%lifecycle-exact-keys-p
          payload
          '("schema_version" "request_id" "motive_id" "mind_identity_id"
            "opportunity_state" "reason_code" "supporting_event_ids"
            "actor_runtime_revision" "observed_at" "integrity_hash"))
         (eql 1 (gethash "schema_version" payload))
         (%lifecycle-text-p (gethash "request_id" payload) 256)
         (%lifecycle-text-p (gethash "motive_id" payload) 256)
         (%lifecycle-text-p (gethash "mind_identity_id" payload) 128)
         (member (gethash "opportunity_state" payload)
                 *conscious-motivation-opportunity-states* :test #'string=)
         (%lifecycle-text-p (gethash "reason_code" payload) 128)
         roots
         (%lifecycle-text-p (gethash "actor_runtime_revision" payload) 256)
         (integerp (gethash "observed_at" payload))
         (not (minusp (gethash "observed_at" payload)))
         (string=
          (gethash "integrity_hash" payload "")
          (%motivation-fnv
           (%motivation-transition-canonical
            payload '("schema_version" "motive_id" "mind_identity_id"
                      "opportunity_state" "reason_code"
                      "supporting_event_ids" "observed_at")))))))

(defun conscious-curiosity-opportunity-payload
    (&key request-id motive-id mind-identity-id opportunity-state reason-code
          supporting-event-ids actor-runtime-revision observed-at
          (policy *curiosity-motivation-policy*))
  (let ((payload
          (obj "schema_version" 1 "request_id" request-id
               "motive_id" motive-id "mind_identity_id" mind-identity-id
               "opportunity_state" opportunity-state "reason_code" reason-code
               "supporting_event_ids" (coerce supporting-event-ids 'vector)
               "actor_runtime_revision" actor-runtime-revision
               "observed_at" observed-at "integrity_hash" :null)))
    (setf (gethash "integrity_hash" payload)
          (%motivation-fnv
           (%motivation-transition-canonical
            payload '("schema_version" "motive_id" "mind_identity_id"
                      "opportunity_state" "reason_code"
                      "supporting_event_ids" "observed_at"))))
    (unless (%motivation-opportunity-valid-p payload policy)
      (error "Curiosity opportunity does not match its closed schema"))
    payload))

(defun %motivation-satisfaction-valid-p (payload)
  (and (hash-table-p payload)
       (%lifecycle-exact-keys-p
        payload
        '("schema_version" "request_id" "motive_id" "mind_identity_id"
          "degree" "receipt_event_id" "actor_runtime_revision"
          "observed_at" "integrity_hash"))
       (eql 1 (gethash "schema_version" payload))
       (%lifecycle-text-p (gethash "request_id" payload) 256)
       (%lifecycle-text-p (gethash "motive_id" payload) 256)
       (%lifecycle-text-p (gethash "mind_identity_id" payload) 128)
       (member (gethash "degree" payload)
               *conscious-motivation-satisfaction-degrees* :test #'string=)
       (%lifecycle-present-id-p (gethash "receipt_event_id" payload))
       (%lifecycle-text-p (gethash "actor_runtime_revision" payload) 256)
       (integerp (gethash "observed_at" payload))
       (not (minusp (gethash "observed_at" payload)))
       (string=
        (gethash "integrity_hash" payload "")
        (%motivation-fnv
         (%motivation-transition-canonical
          payload '("schema_version" "motive_id" "mind_identity_id"
                    "degree" "receipt_event_id" "observed_at"))))))

(defun conscious-curiosity-satisfaction-payload
    (&key request-id motive-id mind-identity-id degree receipt-event-id
          actor-runtime-revision observed-at)
  (let ((payload
          (obj "schema_version" 1 "request_id" request-id
               "motive_id" motive-id "mind_identity_id" mind-identity-id
               "degree" degree "receipt_event_id" receipt-event-id
               "actor_runtime_revision" actor-runtime-revision
               "observed_at" observed-at "integrity_hash" :null)))
    (setf (gethash "integrity_hash" payload)
          (%motivation-fnv
           (%motivation-transition-canonical
            payload '("schema_version" "motive_id" "mind_identity_id"
                      "degree" "receipt_event_id" "observed_at"))))
    (unless (%motivation-satisfaction-valid-p payload)
      (error "Curiosity satisfaction does not match its closed schema"))
    payload))

(defun %motivation-event-partition-p (event agent-id)
  (or (null agent-id)
      (and (stringp (gethash "agent_id" event))
           (string= agent-id (gethash "agent_id" event)))))

(defun %motivation-support-valid-p
    (payload seen ambiguous agent-id policy)
  (let ((roots (%motivation-id-items
                (gethash "supporting_event_ids" payload)
                (%motivation-policy-value policy :evidence-root-bound))))
    (and roots
         (every (lambda (id)
                  (let ((event (gethash id seen)))
                    (and event (not (gethash id ambiguous))
                         (%motivation-event-partition-p event agent-id))))
                roots))))

(defun %motivation-add-roots (row roots event-id bound)
  (let ((all (coerce (gethash "evidence_roots" row) 'list)))
    (dolist (root (append roots (list event-id)))
      (pushnew root all :test #'equal))
    (when (> (length all) bound)
      (return-from %motivation-add-roots nil))
    (setf (gethash "evidence_roots" row)
          (coerce (%motivation-sorted-ids all) 'vector))
    t))

(defun %motivation-decay (row at policy)
  (let ((last (gethash "last_updated_at" row))
        (activation (gethash "activation_milliunits" row)))
    (when (and (integerp at) (integerp last) (> at last))
      (let* ((hours (floor (- at last) 3600))
             (decay (* hours (%motivation-policy-value
                              policy :decay-milliunits-per-hour))))
        (setf (gethash "activation_milliunits" row)
              (max 0 (- activation decay))
              (gethash "last_updated_at" row) at)))
    row))

(defun %motivation-phase (row now policy)
  (let ((activation (gethash "activation_milliunits" row))
        (opportunity (gethash "opportunity_state" row))
        (satisfaction (gethash "satisfaction_state" row))
        (satisfied-at (gethash "satisfied_at" row))
        (refractory-until (gethash "refractory_until" row)))
    (cond
      ((and (string= satisfaction "full")
            (integerp satisfied-at) (= now satisfied-at)) "satisfied")
      ((and (string= satisfaction "full")
            (integerp refractory-until) (< now refractory-until)) "refractory")
      ((string= opportunity "unsuitable") "inhibited")
      ((string= (gethash "last_reinforcement_kind" row "")
                "contradictory-evidence") "reassessed")
      ((and (string= opportunity "suitable")
            (>= activation
                (%motivation-policy-value policy :salient-threshold)))
       "ready-for-opportunity")
      ((>= activation (%motivation-policy-value policy :salient-threshold))
       "salient")
      ((>= activation (%motivation-policy-value policy :rising-threshold))
       "rising")
      (t "latent"))))

(defun %motivation-row
    (payload event-id policy)
  (obj "schema_version" 1 "motive_id" (gethash "motive_id" payload)
       "motive_kind" *conscious-motivation-kind*
       "mind_identity_id" (gethash "mind_identity_id" payload)
       "subject_type" (gethash "subject_type" payload)
       "subject_label" (gethash "subject_label" payload)
       "subject_refs" (copy-seq (gethash "subject_refs" payload))
       "phase" "latent" "activation_milliunits" 0
       "reinforcement_count" 0 "evidence_roots" (vector)
       "latest_event_id" event-id
       "last_reinforcement_kind" :null
       "last_updated_at" (gethash "observed_at" payload)
       "opportunity_state" "unavailable" "inhibition_reason" :null
       "satisfaction_state" "none" "satisfied_at" :null
       "refractory_until" :null "confidence" "asserted"
       "expression_policy"
       (%motivation-policy-value policy :expression-policy)))

(defun %motivation-record-invalid (event-id ids count bound)
  (values (if (< (length ids) bound) (cons event-id ids) ids) (1+ count)))

(defun %motivation-completion-receipt-p
    (event seen-events agent-id)
  (and event (equal "conscious-lifecycle-transition" (gethash "type" event))
       (%motivation-event-partition-p event agent-id)
       (let* ((payload (gethash "payload" event))
              (lifecycle-id (and (hash-table-p payload)
                                 (gethash "lifecycle_id" payload))))
         (and (%lifecycle-transition-payload-p payload)
              (string= "complete" (gethash "transition" payload ""))
              (let* ((projection
                       (conscious-lifecycle-project
                        seen-events :agent-id agent-id))
                     (row (conscious-lifecycle-current projection lifecycle-id)))
                (and row (string= "completed" (gethash "status" row ""))
                     (equal (gethash "id" event)
                            (gethash "last_event_id" row))))))))

(defun %motivation-unique-prior-event
    (events event-id agent-id)
  (let ((matches
          (remove-if-not
           (lambda (event)
             (and (hash-table-p event)
                  (equal event-id (gethash "id" event))
                  (%motivation-event-partition-p event agent-id)))
           events)))
    (and (= 1 (length matches)) (first matches))))

(defun %motivation-result-review-receipt-p
    (event seen-events agent-id motive-id)
  "Accept one closed/refined recursive result review as a satisfaction receipt.

The receipt is authority only when its exact durable review chain already
exists in the same partition and the motive is explicitly inside both the
investigation result and the review disposition."
  (and event
       (string= "recursive-curiosity-result-review-completed"
                (gethash "type" event ""))
       (%motivation-event-partition-p event agent-id)
       (let* ((payload (gethash "payload" event))
              (source-motive-ids
                (and (hash-table-p payload)
                     (%motivation-string-items
                      (gethash "source_motive_ids" payload) 16)))
              (disposition
                (and (hash-table-p payload)
                     (gethash "disposition" payload)))
              (result-id
                (and (hash-table-p payload)
                     (gethash "result_event_id" payload)))
              (opened-id (gethash "caused_by" event))
              (opened (%motivation-unique-prior-event
                       seen-events opened-id agent-id))
              (opened-payload (and opened (gethash "payload" opened)))
              (result (%motivation-unique-prior-event
                       seen-events result-id agent-id))
              (result-payload (and result (gethash "payload" result)))
              (result-motive-ids
                (and (hash-table-p result-payload)
                     (%motivation-string-items
                      (gethash "source_motive_ids" result-payload) 16))))
         (and
          (hash-table-p payload)
          (%lifecycle-exact-keys-p
           payload
           '("schema_version" "result_event_id" "disposition"
             "source_motive_ids" "new_motive_id" "runtime_revision"
             "completed_at"))
          (eql 1 (gethash "schema_version" payload))
          (%lifecycle-present-id-p result-id)
          (member disposition '("closed" "refined") :test #'string=)
          source-motive-ids
          (find motive-id source-motive-ids :test #'string=)
          (if (string= disposition "closed")
              (%motivation-null-p (gethash "new_motive_id" payload))
              (%lifecycle-text-p (gethash "new_motive_id" payload) 256))
          (%lifecycle-text-p (gethash "runtime_revision" payload) 256)
          (integerp (gethash "completed_at" payload))
          (not (minusp (gethash "completed_at" payload)))
          opened
          (string= "recursive-curiosity-result-review-opened"
                   (gethash "type" opened ""))
          (equal result-id (gethash "caused_by" opened))
          (hash-table-p opened-payload)
          (%lifecycle-exact-keys-p
           opened-payload
           '("schema_version" "result_event_id" "runtime_revision"
             "opened_at"))
          (eql 1 (gethash "schema_version" opened-payload))
          (equal result-id (gethash "result_event_id" opened-payload))
          (%lifecycle-text-p
           (gethash "runtime_revision" opened-payload) 256)
          (integerp (gethash "opened_at" opened-payload))
          (not (minusp (gethash "opened_at" opened-payload)))
          result
          (string= "recursive-curiosity-result" (gethash "type" result ""))
          (hash-table-p result-payload)
          (string= "completed" (gethash "status" result-payload ""))
          (string= "private" (gethash "audience" result-payload ""))
          result-motive-ids
          (find motive-id result-motive-ids :test #'string=)))))

(defun %motivation-satisfaction-receipt-p
    (event seen-events agent-id motive-id)
  (or (%motivation-completion-receipt-p event seen-events agent-id)
      (%motivation-result-review-receipt-p
       event seen-events agent-id motive-id)))

(defun %motivation-retirement-candidate-id (motives at policy)
  "Return the deterministic retirement candidate for a bounded active view.

Fully satisfied motives retire first. If those are exhausted, the oldest
lowest-activation non-salient motive retires. Salient motives never yield
capacity here. Authority events remain untouched, and later reinforcement can
form a fresh active row from its complete new observation."
  (let ((settled nil) (latent nil)
        (salient-threshold
          (%motivation-policy-value policy :salient-threshold)))
    (maphash
     (lambda (motive-id row)
       (%motivation-decay row at policy)
       (let ((identity
               (list (gethash "activation_milliunits" row 0)
                     (gethash "latest_event_id" row 0) motive-id)))
         (cond
           ((string= "full" (gethash "satisfaction_state" row ""))
            (when (or (null settled)
                      (< (second identity) (second settled))
                      (and (= (second identity) (second settled))
                           (string< (third identity) (third settled))))
              (setf settled identity)))
           ((< (first identity) salient-threshold)
            (when (or (null latent)
                      (< (first identity) (first latent))
                      (and (= (first identity) (first latent))
                           (< (second identity) (second latent)))
                      (and (= (first identity) (first latent))
                           (= (second identity) (second latent))
                           (string< (third identity) (third latent))))
              (setf latent identity))))))
     motives)
    (third (or settled latent))))

(defun conscious-motivation-project
    (events &key now agent-id context
                 (policy *curiosity-motivation-policy*))
  "Pure Q5M curiosity fold over EVENTS under pinned POLICY and NOW."
  (let* ((ctx (or context
                  (make-projection-context :now now :agent-id agent-id
                                           :motivation-policy policy)))
         (policy (projection-context-policy
                  ctx "motivation_policy" policy))
         (now (let ((value (gethash "now" ctx)))
                (if (integerp value) value (or now 0))))
         (agent-id (let ((value (gethash "agent_id" ctx)))
                     (if (stringp value) value agent-id))))
    (unless (%motivation-policy-valid-p policy)
      (error "Pinned curiosity motivation policy is invalid"))
    (let ((motives (make-hash-table :test #'equal))
          (reinforcements (make-hash-table :test #'equal))
          (requests (make-hash-table :test #'equal))
          (seen (make-hash-table :test #'equal))
          (seen-count (make-hash-table :test #'equal))
          (ambiguous (make-hash-table :test #'equal))
          (seen-sequence nil)
          (invalid-ids nil) (invalid-count 0)
          (retired-ids nil) (retired-count 0)
          (bound (%motivation-policy-value policy :motive-bound))
          (root-bound (%motivation-policy-value policy :evidence-root-bound))
          (invalid-bound
            (%motivation-policy-value policy :invalid-diagnostic-bound)))
      (labels
          ((invalid (event-id)
             (multiple-value-setq (invalid-ids invalid-count)
               (%motivation-record-invalid event-id invalid-ids
                                            invalid-count invalid-bound)))
           (request-new-p (type payload)
             (let* ((request-id (gethash "request_id" payload))
                    (identity (list type (gethash "integrity_hash" payload)))
                    (existing (gethash request-id requests)))
               (cond ((null existing)
                      (setf (gethash request-id requests) identity) t)
                     ((equal existing identity) nil)
                     (t :conflict)))))
        (map nil
             (lambda (event)
               (when (hash-table-p event)
                 (let* ((event-id (gethash "id" event))
                        (type (gethash "type" event))
                        (payload (gethash "payload" event))
                        (partition-p (%motivation-event-partition-p
                                      event agent-id)))
                   (when (and partition-p
                              (member type
                                      '("conscious-curiosity-observed"
                                        "conscious-curiosity-opportunity-observed"
                                        "conscious-curiosity-satisfaction-observed")
                                      :test #'string=))
                     (let ((new-p
                             (cond
                               ((equal type "conscious-curiosity-observed")
                                (if (and (%motivation-observation-valid-p
                                          payload policy)
                                         (%motivation-support-valid-p
                                          payload seen ambiguous agent-id policy))
                                    (request-new-p type payload) :invalid))
                               ((equal type "conscious-curiosity-opportunity-observed")
                                (if (and (%motivation-opportunity-valid-p
                                          payload policy)
                                         (%motivation-support-valid-p
                                          payload seen ambiguous agent-id policy))
                                    (request-new-p type payload) :invalid))
                               (t
                                (if (%motivation-satisfaction-valid-p payload)
                                    (request-new-p type payload) :invalid)))))
                       (cond
                         ((or (eq new-p :invalid) (eq new-p :conflict))
                          (invalid event-id))
                         ((null new-p))
                         ((equal type "conscious-curiosity-observed")
                          (let* ((motive-id (gethash "motive_id" payload))
                                 (row (gethash motive-id motives)))
                            (when (and (null row)
                                       (>= (hash-table-count motives) bound))
                              (let ((retired-id
                                      (%motivation-retirement-candidate-id
                                       motives (gethash "observed_at" payload)
                                       policy)))
                                (if retired-id
                                    (progn
                                      (remhash retired-id motives)
                                      (remhash retired-id reinforcements)
                                      (incf retired-count)
                                      (when (< (length retired-ids) invalid-bound)
                                        (push retired-id retired-ids)))
                                    (progn
                                      (invalid event-id)
                                      (setf row :bounded)))))
                            (unless (eq row :bounded)
                              (unless row
                                (setf row (%motivation-row payload event-id policy)
                                      (gethash motive-id motives) row
                                      (gethash motive-id reinforcements)
                                      (make-hash-table :test #'equal)))
                              (if (or
                                   (not (string= (gethash "mind_identity_id" row)
                                                 (gethash "mind_identity_id" payload)))
                                   (not (string= (gethash "subject_type" row)
                                                 (gethash "subject_type" payload)))
                                   (not (equalp (gethash "subject_refs" row)
                                                (coerce
                                                 (%motivation-sorted-strings
                                                  (coerce
                                                   (gethash "subject_refs" payload)
                                                   'list))
                                                 'vector))))
                                (invalid event-id)
                                (let* ((roots (coerce
                                               (gethash "supporting_event_ids" payload)
                                               'list))
                                       (key (with-output-to-string (out)
                                              (prin1 (%motivation-sorted-ids roots) out)))
                                       (keys (gethash motive-id reinforcements)))
                                  (unless (gethash key keys)
                                    (%motivation-decay
                                     row (gethash "observed_at" payload) policy)
                                    (if (not (%motivation-add-roots
                                              row roots event-id root-bound))
                                        (invalid event-id)
                                        (progn
                                          (setf (gethash key keys) t)
                                          (incf (gethash "reinforcement_count" row))
                                          (setf
                                           (gethash "activation_milliunits" row)
                                           (min
                                            1000
                                            (+ (gethash
                                                "activation_milliunits" row)
                                               (cdr
                                                (assoc
                                                 (gethash "reinforcement_kind"
                                                          payload)
                                                 (%motivation-policy-value
                                                  policy
                                                  :reinforcement-weights)
                                                 :test #'string=))))
                                           (gethash "latest_event_id" row) event-id
                                           (gethash "last_reinforcement_kind" row)
                                           (gethash "reinforcement_kind" payload)
                                           (gethash "confidence" row)
                                           (if (> (gethash
                                                   "reinforcement_count" row) 1)
                                               "corroborated"
                                               "asserted"))))))))))
                         ((equal type "conscious-curiosity-opportunity-observed")
                          (let ((row (gethash (gethash "motive_id" payload) motives)))
                            (if (not (and row
                                          (string=
                                           (gethash "mind_identity_id" row)
                                           (gethash "mind_identity_id" payload))))
                                (invalid event-id)
                                (progn
                                  (%motivation-decay
                                   row (gethash "observed_at" payload) policy)
                                  (if (%motivation-add-roots
                                       row
                                       (coerce (gethash "supporting_event_ids" payload)
                                               'list)
                                       event-id root-bound)
                                      (setf (gethash "opportunity_state" row)
                                            (gethash "opportunity_state" payload)
                                            (gethash "inhibition_reason" row)
                                            (if (string=
                                                 "unsuitable"
                                                 (gethash "opportunity_state" payload))
                                                (gethash "reason_code" payload) :null)
                                            (gethash "latest_event_id" row) event-id)
                                      (invalid event-id))))))
                         (t
                          (let* ((row (gethash (gethash "motive_id" payload) motives))
                                 (receipt-id (gethash "receipt_event_id" payload))
                                 (receipt (and (not (gethash receipt-id ambiguous))
                                               (gethash receipt-id seen))))
                            (if (not (and row
                                          (string=
                                           (gethash "mind_identity_id" row)
                                           (gethash "mind_identity_id" payload))
                                          (%motivation-satisfaction-receipt-p
                                           receipt (reverse seen-sequence)
                                           agent-id
                                           (gethash "motive_id" payload))))
                                (invalid event-id)
                                (progn
                                  (%motivation-decay
                                   row (gethash "observed_at" payload) policy)
                                  (if (%motivation-add-roots
                                       row (list receipt-id) event-id root-bound)
                                      (let ((degree (gethash "degree" payload)))
                                        (setf (gethash "satisfaction_state" row) degree
                                              (gethash "satisfied_at" row)
                                              (gethash "observed_at" payload)
                                              (gethash "latest_event_id" row) event-id
                                              (gethash "opportunity_state" row)
                                              "unavailable")
                                        (if (string= degree "full")
                                            (setf
                                             (gethash "activation_milliunits" row)
                                             (%motivation-policy-value
                                              policy :full-satisfaction-level)
                                             (gethash "refractory_until" row)
                                             (+ (gethash "observed_at" payload)
                                                (%motivation-policy-value
                                                 policy :refractory-seconds)))
                                            (setf
                                             (gethash "activation_milliunits" row)
                                             (max
                                              0
                                              (- (gethash
                                                  "activation_milliunits" row)
                                                 (%motivation-policy-value
                                                  policy
                                                  :partial-satisfaction-drop))))))
                                      (invalid event-id)))))))))
                   (when (%lifecycle-present-id-p event-id)
                     (incf (gethash event-id seen-count 0))
                     (when (> (gethash event-id seen-count) 1)
                       (setf (gethash event-id ambiguous) t))
                     (setf (gethash event-id seen) event))
                   (push event seen-sequence))))
             events))
      (let ((rows nil))
        (maphash
         (lambda (id row)
           (declare (ignore id))
           (%motivation-decay row now policy)
           (setf (gethash "phase" row) (%motivation-phase row now policy))
           (push row rows))
         motives)
        (setf rows (sort rows #'string< :key
                         (lambda (row) (gethash "motive_id" row))))
        (obj "schema_version" 1 "status" "projected"
             "composition_hash" (projection-context-hash ctx)
             "projected_at" now "motive_count" (length rows)
             "motives" (coerce rows 'vector)
             "retired_motive_count" retired-count
             "retired_motive_ids_truncated"
             (> retired-count (length retired-ids))
             "retired_motive_ids" (coerce (nreverse retired-ids) 'vector)
             "invalid_count" invalid-count
             "invalid_ids_truncated" (> invalid-count (length invalid-ids))
             "invalid_event_ids" (coerce (nreverse invalid-ids) 'vector))))))

(defun conscious-motivation-candidates (projection)
  "Derive one content-free candidate per currently eligible motive revision."
  (unless (and (hash-table-p projection)
               (eql 1 (gethash "schema_version" projection)))
    (error "Motivational projection is invalid"))
  (let ((candidates nil))
    (map nil
         (lambda (row)
           (when (member (gethash "phase" row)
                         '("salient" "ready-for-opportunity" "reassessed")
                         :test #'string=)
             (push
              (obj "schema_version" 1
                   "candidate_id"
                   (format nil "candidate:~a:~a"
                           (gethash "motive_id" row)
                           (gethash "latest_event_id" row))
                   "motive_id" (gethash "motive_id" row)
                   "motive_kind" (gethash "motive_kind" row)
                   "phase" (gethash "phase" row)
                   "activation_band"
                   (if (string= "ready-for-opportunity" (gethash "phase" row))
                       "ready" "salient")
                   "source_event_ids" (copy-seq (gethash "evidence_roots" row))
                   "latest_event_id" (gethash "latest_event_id" row)
                   "expression_policy" (gethash "expression_policy" row))
              candidates)))
         (gethash "motives" projection))
    (coerce (nreverse candidates) 'vector)))

(defun conscious-motivation-report (projection)
  (obj "schema_version" 1 "status" (gethash "status" projection)
       "composition_hash" (gethash "composition_hash" projection)
       "projected_at" (gethash "projected_at" projection)
       "motive_count" (gethash "motive_count" projection)
       "retired_motive_count" (gethash "retired_motive_count" projection 0)
       "retired_motive_ids_truncated"
       (gethash "retired_motive_ids_truncated" projection nil)
       "retired_motive_ids" (gethash "retired_motive_ids" projection #())
       "candidate_count" (length (conscious-motivation-candidates projection))
       "invalid_count" (gethash "invalid_count" projection)
       "invalid_ids_truncated" (gethash "invalid_ids_truncated" projection)
       "invalid_event_ids" (gethash "invalid_event_ids" projection)))
