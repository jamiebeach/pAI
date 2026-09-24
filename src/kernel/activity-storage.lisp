;;;; Bounded source-backed activity reads. No implementation or load effects.
(in-package :agent)

(define-condition activity-context-error (error)
  ((code :initarg :code :reader activity-context-error-code)
   (public-message :initarg :public-message :reader activity-context-error-public-message))
  (:report (lambda (condition stream)
             (write-string (activity-context-error-public-message condition) stream))))

(defgeneric storage-prepare-activity-index (backend)
  (:documentation "Explicit maintenance preparation; never implicit in a read."))
(defgeneric storage-activity-index-ready-p (backend)
  (:documentation "Report whether the explicit root index is present without creating it."))
(defgeneric storage-root-has-event-type-p
    (backend agent-id caused-by-root-id event-types
     &key through-position source-boundary newest-p)
  (:documentation "Return presence and a verified witness for one agent/root and
one or more event types at an inclusive physical frontier. Exactly one of
THROUGH-POSITION or SOURCE-BOUNDARY is required. SOURCE-BOUNDARY is an
authority boundary from STORAGE-AUTHORITY-BOUNDARY. An absent root index is
an error, including for a negative read; this operation never prepares it.
NEWEST-P selects the latest matching physical row instead of the first."))
(defgeneric storage-latest-activity-reference
    (backend agent-id persona-id channel resource-id &key activity-id)
  (:documentation "Return the latest scoped reference via an index, never full replay."))
(defgeneric storage-read-activity-context
    (backend reference-event-id &key agent-id through-event-id maximum-rows maximum-bytes)
  (:documentation "Return reference, complete original rows, coverage at a frozen frontier.
A read-limit-exceeded report returns no reference or rows. Never replay globally."))
(defgeneric storage-load-working-context-summary
    (backend agent-id activity-id policy-revision model-revision source-digest)
  (:documentation "Load one bounded derived summary projection by exact source identity."))
(defgeneric storage-publish-working-context-summary
    (backend agent-id activity-id policy-revision model-revision source-digest
             source-event-ids response provenance)
  (:documentation "Atomically publish one rebuildable, source-addressed summary projection."))

(defun validate-activity-reference (payload)
  "Validate a bounded journal reference, not permission to enroll any root."
  (labels ((text (key limit)
             (let ((v (gethash key payload)))
               (and (stringp v) (<= 1 (length v) limit))))
           (ids (value)
             (and (vectorp value) (not (stringp value))
                  (<= 1 (length value) 128)
                  (every (lambda (id) (and (integerp id) (plusp id))) value)
                  (= (length value) (length (remove-duplicates value))))))
    (unless (and (hash-table-p payload) (eql 1 (gethash "schema_version" payload))
                 (every (lambda (key) (text key 256))
                        '("activity_id" "persona_id" "channel" "resource_id"))
                 (text "reason" 2000)
                 (equal "high-bandwidth" (gethash "retention_policy" payload))
                 (member (gethash "actor" payload) '("operator" "model" "runtime") :test #'equal)
                 (member (gethash "attention_state" payload)
                         '("active" "interrupted" "parked") :test #'equal)
                 (member (gethash "completion_state" payload)
                         '("open" "provisionally-complete" "complete") :test #'equal)
                 (let ((confidence (gethash "completion_confidence" payload)))
                   (or (eq confidence :null)
                       (and (realp confidence) (<= 0 confidence 1))))
                 (ids (gethash "root_event_ids" payload))
                 (ids (gethash "evidence_event_ids" payload))
                 (let ((previous (gethash "previous_reference_event_id" payload)))
                   (or (eq previous :null) (and (integerp previous) (plusp previous)))))
      (error 'storage-error :operation :activity-reference :detail "Malformed activity reference")))
  payload)
