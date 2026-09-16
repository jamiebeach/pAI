;;;; lifecycle-semantics.lisp -- Q5S durable semantic lifecycle projection.
;;;;
;;;; Semantic meaning is immutable event data, projected independently from
;;;; generic Q5 lifecycle truth and joined only under explicit disclosure
;;;; policy. This module owns no worker, provider, effect, or publication path.

(in-package :agent)

(export '(conscious-lifecycle-semantic-payload
          conscious-lifecycle-semantic-integrity-hash
          conscious-lifecycle-semantic-project
          conscious-lifecycle-semantic-current
          conscious-lifecycle-semantic-context-records
          conscious-lifecycle-semantic-runtime-describe
          conscious-lifecycle-semantic-runtime-add-result
          *conscious-lifecycle-semantic-runtime-projection*
          *conscious-lifecycle-semantic-schema-version*
          *conscious-lifecycle-semantic-disclosure-policy*))

(defparameter *conscious-lifecycle-semantic-schema-version* 1)
(defparameter *conscious-lifecycle-semantic-subject-types*
  '("topic" "entity" "relationship" "project" "question"))
(defparameter *conscious-lifecycle-semantic-disclosure-classes*
  '("local-only" "private-provider-eligible" "operator-visible"
    "public-eligible"))
(defparameter *conscious-lifecycle-semantic-confidence-states*
  '("asserted" "corroborated" "uncertain"))
(defparameter *conscious-lifecycle-semantic-staleness-states*
  '("current" "stale" "superseded"))
(defparameter *conscious-lifecycle-semantic-invalid-bound* 32)

;; Declared policy, not a provider inference. :ANY phase means the row applies
;; to every already-valid closed lifecycle phase, including a null generic
;; phase. Publication eligibility never grants publication authority.
(defparameter *conscious-lifecycle-semantic-disclosure-policy*
  '((:policy "lifecycle-semantic-disclosure-v1"
     :purpose "respond" :audience "operator" :channel "terminal"
     :provider "local" :kind "deferred-intention" :phases :any
     :classes ("local-only" "private-provider-eligible" "operator-visible"
               "public-eligible"))
    (:policy "lifecycle-semantic-disclosure-v1"
     :purpose "respond" :audience "operator" :channel "terminal"
     :provider "remote-zdr" :kind "deferred-intention" :phases :any
     :classes ("private-provider-eligible" "public-eligible"))
    (:policy "lifecycle-semantic-disclosure-v1"
     :purpose "orient" :audience "operator" :channel "terminal"
     :provider "local" :kind "deferred-intention" :phases :any
     :classes ("local-only" "private-provider-eligible" "operator-visible"
               "public-eligible"))))

(defvar *conscious-lifecycle-semantic-runtime-lock*
  (bt:make-lock "conscious-lifecycle-semantics"))
(defvar *conscious-lifecycle-semantic-runtime-projection* nil)

(defun %lifecycle-semantic-items (value &optional (maximum 32))
  (let ((items (cond ((vectorp value) (coerce value 'list))
                     ((listp value) (copy-list value))
                     (t nil))))
    (and items (<= (length items) maximum) items)))

(defun %lifecycle-semantic-null-p (value)
  (or (null value) (eq value :null)))

(defun %lifecycle-semantic-canonical (payload)
  ;; Actor revision and request ID are provenance/retry envelopes, not semantic
  ;; identity. Keeping them out is what makes replay survive runtime upgrades.
  (with-output-to-string (out)
    (prin1
     (list (gethash "schema_version" payload)
           (gethash "descriptor_id" payload)
           (gethash "descriptor_revision" payload)
           (gethash "supersedes_event_id" payload)
           (gethash "lifecycle_id" payload)
           (gethash "lifecycle_kind" payload)
           (gethash "mind_identity_id" payload)
           (gethash "subject_type" payload)
           (gethash "subject_label" payload)
           (coerce (gethash "subject_refs" payload (vector)) 'list)
           (gethash "intended_outcome" payload)
           (gethash "result_summary" payload)
           (gethash "result_receipt_event_id" payload)
           (gethash "source_revision" payload)
           (gethash "disclosure_policy_ref" payload)
           (gethash "disclosure_class" payload)
           (gethash "confidence" payload)
           (gethash "staleness" payload)
           (coerce (gethash "supporting_event_ids" payload (vector)) 'list))
     out)))

(defun %lifecycle-semantic-fnv (text)
  (let ((hash #xcbf29ce484222325))
    (loop for character across text
          do (setf hash
                   (ldb (byte 64 0)
                        (* (logxor hash (char-code character))
                           #x100000001b3))))
    (format nil "~16,'0x" hash)))

(defun conscious-lifecycle-semantic-integrity-hash (payload)
  "Return the deterministic integrity digest for identity-bearing fields."
  (%lifecycle-semantic-fnv (%lifecycle-semantic-canonical payload)))

(defun %lifecycle-semantic-agent-id ()
  (if (and (boundp '*agent-id*) (stringp *agent-id*)
           (plusp (length *agent-id*)))
      *agent-id*
      (error "Semantic lifecycle runtime has no agent partition")))

(defun %lifecycle-semantic-payload-valid-p (payload)
  (and
   (%lifecycle-exact-keys-p
    payload
    '("schema_version" "request_id" "descriptor_id" "descriptor_revision"
      "supersedes_event_id" "lifecycle_id" "lifecycle_kind"
      "mind_identity_id" "subject_type" "subject_label" "subject_refs"
      "intended_outcome" "result_summary" "result_receipt_event_id"
      "source_revision" "actor_runtime_revision" "disclosure_policy_ref"
      "disclosure_class" "confidence" "staleness" "supporting_event_ids"
      "semantic_integrity_hash"))
   (eql *conscious-lifecycle-semantic-schema-version*
        (gethash "schema_version" payload))
   (%lifecycle-text-p (gethash "request_id" payload) 256)
   (%lifecycle-text-p (gethash "descriptor_id" payload) 256)
   (let ((revision (gethash "descriptor_revision" payload)))
     (and (integerp revision) (plusp revision) (<= revision 1000000)))
   (%lifecycle-nullable-id-p (gethash "supersedes_event_id" payload))
   (%lifecycle-text-p (gethash "lifecycle_id" payload) 256)
   (member (gethash "lifecycle_kind" payload) *conscious-lifecycle-kinds*
           :test #'string=)
   (%lifecycle-text-p (gethash "mind_identity_id" payload) 128)
   (member (gethash "subject_type" payload)
           *conscious-lifecycle-semantic-subject-types* :test #'string=)
   (%lifecycle-text-p (gethash "subject_label" payload) 512)
   (let* ((raw (gethash "subject_refs" payload))
          (refs (and (or (vectorp raw) (listp raw))
                     (%lifecycle-semantic-items raw 16))))
     (and (or (vectorp raw) (listp raw))
          (every (lambda (item) (%lifecycle-text-p item 256)) refs)
          (= (length refs) (length (remove-duplicates refs :test #'string=)))))
   (%lifecycle-text-p (gethash "intended_outcome" payload) 1024)
   (let ((summary (gethash "result_summary" payload))
         (receipt (gethash "result_receipt_event_id" payload)))
     (or (and (%lifecycle-semantic-null-p summary)
              (%lifecycle-semantic-null-p receipt))
         (and (%lifecycle-text-p summary 2048)
              (%lifecycle-present-id-p receipt))))
   (%lifecycle-text-p (gethash "source_revision" payload) 256)
   (%lifecycle-text-p (gethash "actor_runtime_revision" payload) 256)
   (string= "lifecycle-semantic-disclosure-v1"
            (gethash "disclosure_policy_ref" payload ""))
   (member (gethash "disclosure_class" payload)
           *conscious-lifecycle-semantic-disclosure-classes* :test #'string=)
   (member (gethash "confidence" payload)
           *conscious-lifecycle-semantic-confidence-states* :test #'string=)
   (member (gethash "staleness" payload)
           *conscious-lifecycle-semantic-staleness-states* :test #'string=)
   (let ((ids (%lifecycle-semantic-items
               (gethash "supporting_event_ids" payload) 32)))
     (and ids (plusp (length ids)) (every #'%lifecycle-present-id-p ids)
          (= (length ids) (length (remove-duplicates ids :test #'equal)))))
   (%lifecycle-text-p (gethash "semantic_integrity_hash" payload) 64)
   (string= (gethash "semantic_integrity_hash" payload)
            (conscious-lifecycle-semantic-integrity-hash payload))))

(defun conscious-lifecycle-semantic-payload
    (descriptor-id descriptor-revision supersedes-event-id
     lifecycle-id lifecycle-kind mind-identity-id subject-type subject-label
     subject-refs intended-outcome
     &key result-summary result-receipt-event-id source-revision
          actor-runtime-revision disclosure-policy-ref disclosure-class
          confidence staleness supporting-event-ids)
  "Build one exact detached Q5S semantic source payload."
  (let ((payload
          (obj "schema_version" *conscious-lifecycle-semantic-schema-version*
               "request_id" "pending"
               "descriptor_id" descriptor-id
               "descriptor_revision" descriptor-revision
               "supersedes_event_id" supersedes-event-id
               "lifecycle_id" lifecycle-id "lifecycle_kind" lifecycle-kind
               "mind_identity_id" mind-identity-id "subject_type" subject-type
               "subject_label" subject-label "subject_refs" subject-refs
               "intended_outcome" intended-outcome
               "result_summary" (or result-summary :null)
               "result_receipt_event_id" (or result-receipt-event-id :null)
               "source_revision" source-revision
               "actor_runtime_revision" actor-runtime-revision
               "disclosure_policy_ref" disclosure-policy-ref
               "disclosure_class" disclosure-class
               "confidence" confidence "staleness" staleness
               "supporting_event_ids" supporting-event-ids
               "semantic_integrity_hash" "pending")))
    (let ((digest (conscious-lifecycle-semantic-integrity-hash payload)))
      (setf (gethash "request_id" payload) (format nil "q5s:~a" digest)
            (gethash "semantic_integrity_hash" payload) digest))
    (unless (%lifecycle-semantic-payload-valid-p payload)
      (error "Invalid conscious lifecycle semantic payload"))
    payload))

(defun %lifecycle-semantic-event (events id &optional type agent-id)
  (find-if (lambda (event)
             (and (hash-table-p event) (equal id (gethash "id" event))
                  (or (null type) (equal type (gethash "type" event)))
                  (or (null agent-id)
                      (equal agent-id (gethash "agent_id" event)))))
           (reverse events)))

(defun %lifecycle-semantic-ready-receipt-p (event lifecycle-id)
  (and (hash-table-p event)
       (string= "near-term-intention-transition" (gethash "type" event ""))
       (let ((payload (gethash "payload" event)))
         (and (hash-table-p payload)
              (string= "ready" (gethash "state" payload ""))
              (string= lifecycle-id
                       (format nil "near-term:~a"
                               (gethash "intention_id" payload "")))))))

(defun %lifecycle-semantic-copy-row (payload event-id)
  (let ((row (shasht:read-json (shasht:write-json payload nil))))
    (setf (gethash "descriptor_event_id" row) event-id
          (gethash "evidence_event_ids" row)
          (coerce
           (remove-duplicates
            (append (coerce (gethash "supporting_event_ids" payload) 'list)
                    (let ((supersedes (gethash "supersedes_event_id" payload)))
                      (if (%lifecycle-semantic-null-p supersedes)
                          nil (list supersedes)))
                    (list event-id))
            :test #'equal)
           'vector))
    row))

(defun %lifecycle-semantic-joined-row
    (descriptors lifecycle-id mind-identity-id)
  (let ((found nil))
    (maphash
     (lambda (ignored row)
       (declare (ignore ignored))
       (when (and (string= lifecycle-id (gethash "lifecycle_id" row ""))
                  (string= mind-identity-id
                           (gethash "mind_identity_id" row "")))
         (setf found row)))
     descriptors)
    found))

(defun %lifecycle-semantic-update-valid-p (payload prior semantic-events)
  (let ((revision (gethash "descriptor_revision" payload))
        (supersedes (gethash "supersedes_event_id" payload)))
    (if (= revision 1)
        (and (%lifecycle-semantic-null-p supersedes) (null prior)
             (%lifecycle-semantic-null-p (gethash "result_summary" payload)))
        (and prior (%lifecycle-present-id-p supersedes)
             (equal supersedes (gethash "descriptor_event_id" prior))
             (gethash supersedes semantic-events)
             (= revision (1+ (gethash "descriptor_revision" prior)))
             (every (lambda (key)
                      (equal (gethash key payload) (gethash key prior)))
                    '("descriptor_id" "lifecycle_id" "lifecycle_kind"
                      "mind_identity_id"))))))

(defun conscious-lifecycle-semantic-project (events &key agent-id)
  "Purely rebuild bounded Q5S descriptors from an authoritative event stream."
  (let ((descriptors (make-hash-table :test #'equal))
        (requests (make-hash-table :test #'equal))
        (semantic-events (make-hash-table :test #'equal))
        (seen (make-hash-table :test #'equal))
        (invalid '()) (highest 0))
    (dolist (event events)
      (when (and (hash-table-p event)
                 (equal agent-id (gethash "agent_id" event)))
        (let ((id (gethash "id" event))
              (type (gethash "type" event))
              (payload (gethash "payload" event)))
          (when (and (integerp id) (> id highest)) (setf highest id))
          (when (equal type "conscious-lifecycle-semantic-described")
            (let* ((descriptor-id (and (hash-table-p payload)
                                       (gethash "descriptor_id" payload)))
                   (prior (and descriptor-id (gethash descriptor-id descriptors)))
                   (joined
                     (and (hash-table-p payload)
                          (%lifecycle-semantic-joined-row
                           descriptors (gethash "lifecycle_id" payload "")
                           (gethash "mind_identity_id" payload ""))))
                   (support (and (hash-table-p payload)
                                 (%lifecycle-semantic-items
                                  (gethash "supporting_event_ids" payload) 32)))
                   (result-receipt
                     (and (hash-table-p payload)
                          (gethash "result_receipt_event_id" payload)))
                   (valid
                     (and (%lifecycle-semantic-payload-valid-p payload)
                          (null (gethash (gethash "request_id" payload) requests))
                          (every (lambda (root) (gethash root seen)) support)
                          (if (= 1 (gethash "descriptor_revision" payload 0))
                              (null joined)
                              (eq joined prior))
                          (%lifecycle-semantic-update-valid-p
                           payload prior semantic-events)
                          (or (%lifecycle-semantic-null-p result-receipt)
                              (%lifecycle-semantic-ready-receipt-p
                               (gethash result-receipt seen)
                               (gethash "lifecycle_id" payload))))))
              (if valid
                  (let ((row (%lifecycle-semantic-copy-row payload id)))
                    (setf (gethash (gethash "request_id" payload) requests) id
                          (gethash descriptor-id descriptors) row
                          (gethash id semantic-events) row))
                  (when (< (length invalid)
                           *conscious-lifecycle-semantic-invalid-bound*)
                    (push id invalid)))))
          (when (%lifecycle-present-id-p id)
            ;; New writes are unique. Preserved duplicate IDs intentionally use
            ;; the newest preceding exact event, matching the Q5 receipt rule.
            (setf (gethash id seen) event)))))
    (obj "schema_version" *conscious-lifecycle-semantic-schema-version*
         "agent_id" (or agent-id :null) "highest_event_id" highest
         "descriptor_count" (hash-table-count descriptors)
         "invalid_event_ids" (coerce (nreverse invalid) 'vector)
         "invalid_event_ids_truncated"
         (> (count-if (lambda (event)
                        (and (hash-table-p event)
                             (equal agent-id (gethash "agent_id" event))
                             (equal "conscious-lifecycle-semantic-described"
                                    (gethash "type" event))))
                      events)
            (+ (hash-table-count semantic-events) (length invalid)))
         "descriptors" descriptors)))

(defun conscious-lifecycle-semantic-current
    (projection lifecycle-id mind-identity-id)
  "Return the current exact lifecycle/mind descriptor, or NIL."
  (let ((table (and (hash-table-p projection)
                    (gethash "descriptors" projection)))
        (matches '()))
    (when (hash-table-p table)
      (maphash
       (lambda (ignored row)
         (declare (ignore ignored))
         (when (and (string= lifecycle-id (gethash "lifecycle_id" row ""))
                    (string= mind-identity-id
                             (gethash "mind_identity_id" row "")))
           (push row matches)))
       table))
    (when matches
      (shasht:read-json
       (shasht:write-json
        (first (sort matches #'> :key (lambda (row)
                                       (gethash "descriptor_revision" row))))
        nil)))))

(defun %lifecycle-semantic-any-for-lifecycle-p (projection lifecycle-id)
  (let ((table (and (hash-table-p projection)
                    (gethash "descriptors" projection)))
        (found nil))
    (when (hash-table-p table)
      (maphash (lambda (ignored row)
                 (declare (ignore ignored))
                 (when (string= lifecycle-id (gethash "lifecycle_id" row ""))
                   (setf found t)))
               table))
    found))

(defun %lifecycle-semantic-policy-permits-p
    (descriptor purpose audience channel provider-class kind phase)
  (some
   (lambda (rule)
     (and (string= (getf rule :policy)
                   (gethash "disclosure_policy_ref" descriptor ""))
          (string= (getf rule :purpose) purpose)
          (string= (getf rule :audience) audience)
          (string= (getf rule :channel) channel)
          (string= (getf rule :provider) provider-class)
          (string= (getf rule :kind) kind)
          (or (eq :any (getf rule :phases))
              (member phase (getf rule :phases) :test #'string=))
          (member (gethash "disclosure_class" descriptor)
                  (getf rule :classes) :test #'string=)))
   *conscious-lifecycle-semantic-disclosure-policy*))

(defun %lifecycle-semantic-base-content (row suffix)
  (let ((phase (gethash "phase" row))
        (checkpoint (gethash "checkpoint_ref" row)))
    (subseq
     (format nil "Lifecycle ~a (~a) is ~a~@[; phase ~a~]~@[; checkpoint ~a~]. ~a"
             (gethash "lifecycle_id" row) (gethash "lifecycle_kind" row)
             (gethash "status" row)
             (unless (%lifecycle-semantic-null-p phase) phase)
             (unless (%lifecycle-semantic-null-p checkpoint) checkpoint)
             suffix)
     0 (min 4096
            (length
             (format nil "Lifecycle ~a (~a) is ~a~@[; phase ~a~]~@[; checkpoint ~a~]. ~a"
                     (gethash "lifecycle_id" row) (gethash "lifecycle_kind" row)
                     (gethash "status" row)
                     (unless (%lifecycle-semantic-null-p phase) phase)
                     (unless (%lifecycle-semantic-null-p checkpoint) checkpoint)
                     suffix))))))

(defun %lifecycle-semantic-record (row descriptor)
  (let* ((summary (gethash "result_summary" descriptor))
         (receipt (gethash "result_receipt_event_id" descriptor))
         (suffix
           (format nil
                   "Subject (~a): ~a. Intended outcome: ~a.~@[ Result summary: ~a; receipt ~a.~] Confidence: ~a; staleness: ~a."
                   (gethash "subject_type" descriptor)
                   (gethash "subject_label" descriptor)
                   (gethash "intended_outcome" descriptor)
                   (unless (%lifecycle-semantic-null-p summary) summary)
                   (unless (%lifecycle-semantic-null-p receipt) receipt)
                   (gethash "confidence" descriptor)
                   (gethash "staleness" descriptor))))
    (obj "source_id" (gethash "descriptor_event_id" descriptor)
         "content" (%lifecycle-semantic-base-content row suffix)
         "provenance"
         (obj "descriptor_id" (gethash "descriptor_id" descriptor)
              "descriptor_event_id" (gethash "descriptor_event_id" descriptor)
              "evidence_event_ids" (gethash "evidence_event_ids" descriptor)))))

(defun %lifecycle-semantic-unavailable-record (row reason descriptor-id)
  (values
   (obj "source_id" (gethash "last_event_id" row)
        "content"
        (%lifecycle-semantic-base-content
         row (if (string= reason "disclosure-policy-refused")
                 "Semantic description withheld by disclosure policy."
                 "Semantic description unavailable.")))
   (obj "source_id" (gethash "last_event_id" row)
        "section" "focus-lifecycles" "reason" reason
        "descriptor_id" (or descriptor-id :null))))

(defun conscious-lifecycle-semantic-context-records
    (awaiting projection &key mind-identity-id purpose audience channel
                              provider-class)
  "Return records, content-free refusals, and required evidence IDs."
  (unless (and (%lifecycle-text-p mind-identity-id 128)
               (%lifecycle-text-p purpose 64) (%lifecycle-text-p audience 64)
               (%lifecycle-text-p channel 64) (%lifecycle-text-p provider-class 64))
    (error "Semantic context selection identity or route is invalid"))
  (let ((rows (cond ((vectorp awaiting) (coerce awaiting 'list))
                    ((listp awaiting) awaiting) (t nil)))
        (records '()) (refusals '()) (evidence '()))
    (dolist (row rows)
      (when (and (hash-table-p row)
                 (%lifecycle-text-p (gethash "lifecycle_id" row) 256)
                 (%lifecycle-present-id-p (gethash "last_event_id" row)))
        (let* ((lifecycle-id (gethash "lifecycle_id" row))
               (descriptor (conscious-lifecycle-semantic-current
                            projection lifecycle-id mind-identity-id))
               (reason
                 (cond
                   ((null descriptor)
                    (if (%lifecycle-semantic-any-for-lifecycle-p
                         projection lifecycle-id)
                        "mind-identity-mismatch" "semantic-unavailable"))
                   ((not (%lifecycle-semantic-policy-permits-p
                          descriptor purpose audience channel provider-class
                          (gethash "lifecycle_kind" row)
                          (gethash "phase" row)))
                    "disclosure-policy-refused")
                   (t nil))))
          (if reason
              (multiple-value-bind (record refusal)
                  (%lifecycle-semantic-unavailable-record
                   row reason (and descriptor (gethash "descriptor_id" descriptor)))
                (push record records) (push refusal refusals)
                (pushnew (gethash "last_event_id" row) evidence :test #'equal))
              (let ((record (%lifecycle-semantic-record row descriptor)))
                (push record records)
                (dolist (id (coerce (gethash "evidence_event_ids" descriptor)
                                    'list))
                  (pushnew id evidence :test #'equal))
                (pushnew (gethash "last_event_id" row) evidence :test #'equal))))))
    (values (coerce (nreverse records) 'vector)
            (coerce (nreverse refusals) 'vector)
            (coerce (nreverse evidence) 'vector))))

(defun %lifecycle-semantic-runtime-existing-request (events request-id agent-id)
  (find-if (lambda (event)
             (and (hash-table-p event)
                  (equal agent-id (gethash "agent_id" event))
                  (string= "conscious-lifecycle-semantic-described"
                           (gethash "type" event ""))
                  (let ((payload (gethash "payload" event)))
                    (and (hash-table-p payload)
                         (string= request-id
                                  (gethash "request_id" payload ""))))))
           (reverse events)))

(defun %lifecycle-semantic-runtime-append
    (payload source-event-id agent-id &optional supplied-events)
  (let* ((events (or supplied-events (%lifecycle-runtime-events)))
         (source (%lifecycle-semantic-event events source-event-id nil agent-id))
         (existing (%lifecycle-semantic-runtime-existing-request
                    events (gethash "request_id" payload) agent-id)))
    (unless source (error "Semantic source receipt is absent from this partition"))
    (when existing
      (unless (string= (gethash "semantic_integrity_hash" payload)
                       (gethash "semantic_integrity_hash"
                                (gethash "payload" existing) ""))
        (error "Semantic request conflicts with durable history"))
      (return-from %lifecycle-semantic-runtime-append (gethash "id" existing)))
    (multiple-value-bind (id persisted receipt)
        (funcall 'log-event "conscious-lifecycle-semantic-described" payload
                 :caused-by source-event-id)
      (declare (ignore persisted))
      (unless id (error "Semantic lifecycle append returned no event ID"))
      ;; The production log supplies the exact flushed event receipt, avoiding
      ;; a second expanded replay while EVENTS is live. One-value test/legacy
      ;; ports retain a readback proof on their small streams.
      (let ((stored
              (if (and (hash-table-p receipt)
                       (equal id (gethash "id" receipt))
                       (string= "conscious-lifecycle-semantic-described"
                                (gethash "type" receipt ""))
                       (equal agent-id (gethash "agent_id" receipt))
                       (equalp payload (gethash "payload" receipt)))
                  receipt
                  (%lifecycle-semantic-event
                   (%lifecycle-runtime-events) id
                   "conscious-lifecycle-semantic-described" agent-id))))
        (unless stored
          (error "Semantic lifecycle event ~s was not durably readable" id))
        id))))

(defun conscious-lifecycle-semantic-runtime-describe
    (lifecycle-id lifecycle-kind mind-identity-id subject-type subject-label
     intended-outcome source-event-id
     &key (subject-refs (vector)) source-revision actor-runtime-revision
          (disclosure-policy-ref "lifecycle-semantic-disclosure-v1")
          (disclosure-class "local-only") (confidence "asserted"))
  "Append an initial descriptor at an explicit observation boundary."
  (bt:with-lock-held (*conscious-lifecycle-semantic-runtime-lock*)
    (let* ((agent-id (%lifecycle-semantic-agent-id))
           (events (%lifecycle-runtime-events))
           (source (%lifecycle-semantic-event
                    events source-event-id nil agent-id))
           (source-payload (and source (gethash "payload" source)))
           (descriptor-id (format nil "semantic:~a" lifecycle-id))
           (payload
             (conscious-lifecycle-semantic-payload
              descriptor-id 1 :null lifecycle-id lifecycle-kind
              mind-identity-id subject-type subject-label subject-refs
              intended-outcome :result-summary :null
              :result-receipt-event-id :null :source-revision source-revision
              :actor-runtime-revision actor-runtime-revision
              :disclosure-policy-ref disclosure-policy-ref
              :disclosure-class disclosure-class :confidence confidence
              :staleness "current"
              :supporting-event-ids (vector source-event-id))))
      (unless (and source-payload
                   (string= "near-term-intention-created"
                            (gethash "type" source ""))
                   (string= "seeded" (gethash "state" source-payload ""))
                   (string= lifecycle-id
                            (format nil "near-term:~a"
                                    (gethash "intention_id" source-payload ""))))
        (error "Initial semantic description requires its typed creation receipt"))
      (%lifecycle-semantic-runtime-append payload source-event-id agent-id
                                          events))))

(defun conscious-lifecycle-semantic-runtime-add-result
    (lifecycle-id mind-identity-id result-summary result-receipt-event-id
     &key actor-runtime-revision)
  "Append a superseding bounded result after a typed ready receipt."
  (bt:with-lock-held (*conscious-lifecycle-semantic-runtime-lock*)
    (let* ((agent-id (%lifecycle-semantic-agent-id))
           (events (%lifecycle-runtime-events))
           (projection (conscious-lifecycle-semantic-project
                        events :agent-id agent-id))
           (prior (conscious-lifecycle-semantic-current
                   projection lifecycle-id mind-identity-id))
           (receipt (%lifecycle-semantic-event
                     events result-receipt-event-id nil agent-id)))
      (unless prior (error "No semantic descriptor exists for result update"))
      (unless (%lifecycle-semantic-ready-receipt-p receipt lifecycle-id)
        (error "Semantic result requires a matching typed ready receipt"))
      (let ((payload
              (conscious-lifecycle-semantic-payload
               (gethash "descriptor_id" prior)
               (1+ (gethash "descriptor_revision" prior))
               (gethash "descriptor_event_id" prior)
               lifecycle-id (gethash "lifecycle_kind" prior) mind-identity-id
               (gethash "subject_type" prior) (gethash "subject_label" prior)
               (gethash "subject_refs" prior) (gethash "intended_outcome" prior)
               :result-summary result-summary
               :result-receipt-event-id result-receipt-event-id
               :source-revision (gethash "source_revision" prior)
               :actor-runtime-revision actor-runtime-revision
               :disclosure-policy-ref (gethash "disclosure_policy_ref" prior)
               :disclosure-class (gethash "disclosure_class" prior)
               :confidence (gethash "confidence" prior) :staleness "current"
               :supporting-event-ids
               (coerce
                (remove-duplicates
                 (append (coerce (gethash "supporting_event_ids" prior) 'list)
                         (list result-receipt-event-id))
                 :test #'equal)
                'vector))))
        (%lifecycle-semantic-runtime-append payload result-receipt-event-id
                                            agent-id events)))))
