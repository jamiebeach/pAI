;;;; storage-projection.lisp -- SQLite projection checkpoint/tail path.

(in-package :agent)

(export '(conscious-storage-full-project
          conscious-storage-build-checkpoint
          conscious-storage-refresh-checkpoint
          conscious-storage-restore-checkpoint-tail
          conscious-storage-restore-event-sequence
          conscious-attention-shadow-policy-revision
          conscious-attention-shadow-select
          conscious-attention-shadow-events
          conscious-storage-indexed-state
          conscious-storage-refresh-indexed))

(defparameter *conscious-storage-checkpoint-name* "conscious-runtime")
(defparameter *conscious-storage-projector-revision*
  "conscious-storage-bounded-prefix-v6")

(defun %conscious-storage-runtime-revision ()
  (if (and (boundp '*conscious-cognition-runtime-revision*)
           (stringp (symbol-value '*conscious-cognition-runtime-revision*)))
      (symbol-value '*conscious-cognition-runtime-revision*)
      "conscious-q5-v2"))

(defun %conscious-storage-project-events (events agent-id now)
  (let* ((lifecycle (conscious-lifecycle-project events :agent-id agent-id))
         (semantics
           (conscious-lifecycle-semantic-project events :agent-id agent-id))
         (awaiting (conscious-lifecycle-awaiting lifecycle))
         (context
           (make-projection-context
            :now now :agent-id agent-id
            :runtime-revision (%conscious-storage-runtime-revision)
            :lifecycle awaiting))
         (state (conscious-state-project events :context context)))
    (%storage-object
     "schema_version" 1 "composition_hash" (projection-context-hash context)
     "state" state "lifecycle" lifecycle "semantics" semantics
     "context" (projection-context-report context))))

(defun %conscious-storage-canonical-hash (value)
  (%storage-sha256 (%stimulus-canonical-json value)))

(defun %conscious-storage-consumption-type-p (type)
  (member type *inbox-consumption-event-types* :test #'string=))

(defun %conscious-storage-retain-full-p (event)
  (let ((type (gethash "type" event "")))
    (or (stimulus-admissible-p type)
        (%conscious-storage-consumption-type-p type)
        (member type
                '("conscious-lifecycle-transition"
                  "conscious-lifecycle-result-rejected"
                  "conscious-lifecycle-source-rejected"
                  "conscious-lifecycle-semantic-described"
                  "near-term-intention-created"
                  "near-term-intention-transition")
                :test #'string=))))

(defun %conscious-storage-agent-message-root (event agent-id)
  (and (string= "agent-message" (gethash "type" event ""))
       (%inbox-authorized-conversation-reply-root event agent-id)))

(defun %conscious-storage-compact-agent-message (event)
  (let* ((payload (gethash "payload" event))
         (metadata (and (hash-table-p payload) (gethash "metadata" payload)))
         (compacted (%storage-object
                     "text_present"
                     (if (and (stringp (gethash "text" payload))
                              (plusp (length (gethash "text" payload))))
                         t nil))))
    (dolist (key '("authorization_kind" "authorization_id" "final"))
      (multiple-value-bind (value present-p) (gethash key payload)
        (when present-p (setf (gethash key compacted) value))))
    (when (hash-table-p metadata)
      (setf (gethash "metadata" compacted)
            (%storage-object
             "source" (gethash "source" metadata :null)
             "publication_validation"
             (gethash "publication_validation" metadata :null))))
    (%storage-object
     "schema_version" (gethash "schema_version" event 1)
     "id" (gethash "id" event) "timestamp" (gethash "timestamp" event)
     "type" "agent-message" "agent_id" (gethash "agent_id" event :null)
     "caused_by" (gethash "caused_by" event :null)
     "tick_id" (gethash "tick_id" event :null)
     "payload" compacted)))

(defun %conscious-storage-compact-causal-tool-call (event)
  (%storage-object
   "schema_version" (gethash "schema_version" event 1)
   "id" (gethash "id" event) "timestamp" (gethash "timestamp" event)
   "type" "tool-call" "agent_id" (gethash "agent_id" event :null)
   "caused_by" (gethash "caused_by" event :null)
   "payload" (%storage-object)))

(defun conscious-attention-shadow-policy-revision ()
  "Bind selected rows to the complete event-type admission set. A new
admitted type cannot silently reuse an older shadow that omitted it."
  (let ((types nil))
    (maphash (lambda (type ignored)
               (declare (ignore ignored))
               (push type types))
             *stimulus-kind-map*)
    (%storage-sha256
     (format nil "attention-selector-v3|~{~a~^|~}"
             (sort (append types *inbox-consumption-event-types*
                           (list "agent-message" "tool-call")) #'string<)))))

(defun conscious-attention-shadow-select (event)
  "Retain only facts needed by the pure inbox fold.  The authorized legacy
reply shape is compacted, but its causal and authorization fields remain."
  (let ((type (gethash "type" event "")))
    (cond ((or (stimulus-admissible-p type)
               (%conscious-storage-consumption-type-p type))
           event)
          ((%conscious-storage-agent-message-root
            event (gethash "agent_id" event))
           (%conscious-storage-compact-agent-message event))
          ((string= type "tool-call")
           (%conscious-storage-compact-causal-tool-call event)))))

(defun conscious-attention-shadow-events
    (derived source agent-id &key (limit 128) through-event-id)
  "Reconstruct an inbox-equivalent sparse ledger view from selected rows.
No ordinary journal body is read.  Neutral gap stubs retain the exact event
IDs needed for highest-observed and consumption-watermark semantics.  This
is a parity candidate, not yet the live attention reader."
  (let* ((revision (conscious-attention-shadow-policy-revision))
         (watermark (storage-shadow-attention-report
                     derived source :agent-id agent-id
                     :policy-revision revision))
         (boundary (storage-authority-boundary source :agent-id agent-id))
         (target-position
           (if through-event-id
               (storage-event-position source agent-id through-event-id)
               (gethash "through_storage_position" boundary)))
         (ordinal 0) (last-position 0) (events nil) (finished nil))
    (unless (and watermark
                 target-position
                 (<= target-position (gethash "through_position" watermark))
                 (= (gethash "through_position" watermark)
                    (gethash "through_storage_position" boundary)))
      (error 'storage-conflict-error :operation :attention-events
             :detail "attention rows are absent or behind source authority"))
    (labels ((stub (id)
               (obj "id" id "type" "projection-accounted"
                    "payload" (obj) "agent_id" agent-id)))
      (loop until finished do
        (multiple-value-bind (rows pinned)
            (storage-shadow-attention-read-page
             derived source :agent-id agent-id
             :policy-revision revision :after-ordinal ordinal :limit limit)
          (unless (equal (gethash "source_binding" watermark)
                         (gethash "source_binding" pinned))
            (error 'storage-conflict-error :operation :attention-events
                   :detail "attention cursor changed during page stream"))
          (dolist (row rows)
            (destructuring-bind (next-ordinal position previous-id event) row
              (when (> position target-position)
                (setf finished t)
                (return))
              (when (> position (1+ last-position))
                (push (stub previous-id) events))
              (push event events)
              (setf ordinal next-ordinal last-position position)))
          (when (= ordinal (gethash "selected_count" watermark))
            (setf finished t))))
      (when (> target-position last-position)
        (push (stub (or through-event-id
                        (gethash "through_event_id" watermark)))
              events))
      (unless (= (gethash "through_storage_position" boundary)
                 (gethash "through_storage_position"
                          (storage-authority-boundary
                           source :agent-id agent-id)))
        (error 'storage-conflict-error :operation :attention-events
               :detail "source authority advanced during attention read"))
      (values (nreverse events)
              (storage-max-event-id-through-position
               source agent-id target-position)))))

(defun conscious-storage-indexed-state
    (source derived agent-id &key (now (get-universal-time))
                                 (runtime-revision
                                   (%conscious-storage-runtime-revision))
                                 (maximum-selected-events 8192)
                                 through-event-id)
  "Project current conscious state from three source-bound row families.
This is an explicit candidate read, not yet the installed runtime route. It
requires each shadow at the same physical authority head and refuses an
oversized selected-event generation instead of recreating a heap spike.
For an earlier root, current lifecycle and pulse rows are usable only when
their event families have not changed after that physical boundary. The
semantic lifecycle view and conversation evidence remain separate gates."
  (unless (and (integerp maximum-selected-events)
               (plusp maximum-selected-events))
    (error "Indexed conscious state requires a positive event bound"))
  (let* ((boundary (storage-authority-boundary source :agent-id agent-id))
         (head (gethash "through_storage_position" boundary))
         (attention (storage-shadow-attention-report
                     derived source :agent-id agent-id
                     :policy-revision
                     (conscious-attention-shadow-policy-revision)))
         (lifecycle-watermark (storage-shadow-lifecycle-watermark
                               derived source :agent-id agent-id))
         (pulse (storage-shadow-conscious-pulse-report
                 derived source :agent-id agent-id))
         (target-position
           (if through-event-id
               (storage-event-position source agent-id through-event-id)
               head)))
    (unless (and attention lifecycle-watermark pulse
                 target-position (<= target-position head)
                 (= head (gethash "through_position" attention)
                    (gethash "through_position" lifecycle-watermark)
                    (gethash "through_position" pulse))
                 (<= (gethash "selected_count" attention)
                     maximum-selected-events))
      (error 'storage-conflict-error :operation :indexed-conscious-state
             :detail "indexed conscious inputs are absent, stale or over bound"))
    (when (< target-position head)
      (let ((changed nil))
        (multiple-value-bind (complete-p)
            (storage-map-events
             source
             (lambda (event position)
               (declare (ignore event position))
               (setf changed t))
             :agent-id agent-id :after-position target-position
             :through-position head
             :event-types
             '("conscious-lifecycle-transition"
               "conscious-lifecycle-result-rejected"
               "conscious-lifecycle-source-rejected"
               "pulse-committed"))
          (unless complete-p
            (error 'storage-conflict-error
                   :operation :indexed-conscious-state
                   :detail "as-of tail check did not complete")))
        (when changed
          (error 'storage-conflict-error :operation :indexed-conscious-state
                 :detail "lifecycle or pulse state changed after as-of root"))))
    (multiple-value-bind (attention-events highest)
        (conscious-attention-shadow-events
         derived source agent-id :through-event-id through-event-id)
      (let* ((lifecycle (storage-shadow-lifecycle-project
                         derived source :agent-id agent-id))
             (as-of-lifecycle
               (if through-event-id
                   (let ((copy (make-hash-table :test #'equal)))
                     (maphash (lambda (key value)
                                (setf (gethash key copy) value))
                              lifecycle)
                     (setf (gethash "highest_event_id" copy) highest)
                     copy)
                   lifecycle))
             (context (make-projection-context
                       :now now :agent-id agent-id
                       :runtime-revision runtime-revision
                       :lifecycle
                       (conscious-lifecycle-awaiting as-of-lifecycle)))
             (inbox (inbox-project
                     attention-events :context context
                     :observed-highest-event-id highest))
             (state (conscious-state-project
                     nil :context context :inbox-projection inbox
                     :committed-pulse-sequence
                     (gethash "max_sequence" pulse))))
      (unless (= head
                 (gethash "through_storage_position"
                          (storage-authority-boundary
                           source :agent-id agent-id)))
        (error 'storage-conflict-error :operation :indexed-conscious-state
               :detail "source authority advanced during indexed state read"))
      (values state context as-of-lifecycle inbox)))))

(defun conscious-storage-refresh-indexed
    (source derived agent-id &key (page-limit 128) (maximum-pages 16))
  "Advance prepared conscious row families through one captured ledger head.
This is incremental maintenance, never an implicit historical build. Each
family commits bounded pages independently; the indexed reader refuses an
interrupted or mixed-head set until a later call finishes all three."
  (unless (and (integerp page-limit) (<= 1 page-limit 512)
               (integerp maximum-pages) (plusp maximum-pages))
    (error "Indexed conscious refresh requires positive bounded pages"))
  (let* ((head (storage-head-position source :agent-id agent-id))
         (revision (conscious-attention-shadow-policy-revision))
         (attention (storage-shadow-attention-report
                     derived source :agent-id agent-id
                     :policy-revision revision))
         (lifecycle (storage-shadow-lifecycle-watermark
                     derived source :agent-id agent-id))
         (pulse (storage-shadow-conscious-pulse-report
                 derived source :agent-id agent-id)))
    (unless (and attention lifecycle pulse)
      (error 'storage-conflict-error :operation :indexed-conscious-refresh
             :detail "offline preparation of all three row families is required"))
    (labels ((advance (name initial thunk)
               (let ((position (gethash "through_position" initial)))
                 (when (> position head)
                   (error 'storage-conflict-error
                          :operation :indexed-conscious-refresh
                          :detail "derived cursor is ahead of captured authority"))
                 (loop repeat maximum-pages
                       while (< position head)
                       do (let* ((row (funcall thunk))
                                 (next (and row
                                            (gethash "through_position" row))))
                            (unless (and (integerp next)
                                         (> next position))
                              (error 'storage-conflict-error
                                     :operation :indexed-conscious-refresh
                                     :detail (format nil "~a cursor did not advance"
                                                     name)))
                            (setf position next)))
                 (unless (= position head)
                   (error 'storage-conflict-error
                          :operation :indexed-conscious-refresh
                          :detail (format nil "~a tail exceeds bounded page allowance"
                                          name))))))
      (advance "attention" attention
               (lambda ()
                 (storage-shadow-attention-apply-page
                  derived source #'conscious-attention-shadow-select
                  :agent-id agent-id :policy-revision revision
                  :limit page-limit :maximum-event-bytes 8388608)))
      (advance "lifecycle" lifecycle
               (lambda ()
                 (storage-shadow-lifecycle-apply-page
                  derived source #'conscious-lifecycle-shadow-step
                  :agent-id agent-id :limit page-limit)))
      (advance "pulse" pulse
               (lambda ()
                 (storage-shadow-conscious-pulse-apply-page
                  derived source :agent-id agent-id :limit page-limit))))
    (unless (= head (storage-head-position source :agent-id agent-id))
      (error 'storage-conflict-error :operation :indexed-conscious-refresh
             :detail "authority advanced during indexed refresh"))
    (conscious-storage-indexed-state source derived agent-id)))

(defun %conscious-storage-accounted-stub (event)
  ;; Preserve identity/order/partition facts used by highest-ID, watermark and
  ;; lifecycle source-reference folds. The neutral type is deliberately absent
  ;; from the stimulus census.
  (%storage-object
   "schema_version" (gethash "schema_version" event 1)
   "id" (gethash "id" event) "timestamp" (gethash "timestamp" event :null)
   "type" "projection-accounted"
   "agent_id" (gethash "agent_id" event :null)
   "caused_by" :null "tick_id" :null "payload" (%storage-object)))

(defun %conscious-storage-compact-event (event agent-id)
  (cond ((%conscious-storage-retain-full-p event) event)
        ((%conscious-storage-agent-message-root event agent-id)
         (%conscious-storage-compact-agent-message event))
        (t (%conscious-storage-accounted-stub event))))

(defun %conscious-storage-add-ids (value table)
  (map nil (lambda (id) (when id (setf (gethash id table) t)))
       (cond ((vectorp value) value)
             ((listp value) (coerce value 'vector))
             (t (vector)))))

(defun %conscious-storage-outstanding-ids (events agent-id now)
  "Return source IDs for every stimulus whose future interpretation is not
terminal. This includes candidates currently hidden by coalescing or bounds."
  (let* ((lifecycle (conscious-lifecycle-project events :agent-id agent-id))
         (context
           (make-projection-context
            :now now :agent-id agent-id
            :runtime-revision (%conscious-storage-runtime-revision)
            :lifecycle (conscious-lifecycle-awaiting lifecycle)))
         (inbox (inbox-project events :context context))
         (ids (make-hash-table :test #'equal)))
    (dolist (key '("admitted" "rejected" "deferred" "coalesced"))
      (map nil
           (lambda (row)
             (when (hash-table-p row)
               (%conscious-storage-add-ids
                (gethash "source_event_ids" row) ids)))
           (gethash key inbox (vector))))
    ids))

(defun %conscious-storage-reference-ids (events)
  "Return (values presence-ids full-receipt-ids) required by retained folds."
  (let ((presence (make-hash-table :test #'equal))
        (full (make-hash-table :test #'equal)))
    (dolist (event events)
      (let* ((type (gethash "type" event ""))
             (payload (gethash "payload" event)))
        (when (hash-table-p payload)
          (when (member type
                        '("conscious-lifecycle-transition"
                          "conscious-lifecycle-result-rejected"
                          "conscious-lifecycle-source-rejected")
                        :test #'string=)
            (let ((id (gethash "source_event_id" payload)))
              (when id (setf (gethash id presence) t))))
          (when (string= type "conscious-lifecycle-semantic-described")
            (%conscious-storage-add-ids
             (gethash "supporting_event_ids" payload) presence)
            (let ((supersedes (gethash "supersedes_event_id" payload))
                  (receipt (gethash "result_receipt_event_id" payload)))
              (unless (or (null supersedes) (eq supersedes :null))
                (setf (gethash supersedes presence) t))
              (unless (or (null receipt) (eq receipt :null))
                (setf (gethash receipt presence) t
                      (gethash receipt full) t)))))))
    (values presence full)))

(defun %conscious-storage-max-pulse-event (events)
  (let ((winner nil) (sequence 0))
    (dolist (event events winner)
      (when (string= "pulse-committed" (gethash "type" event ""))
        (let* ((payload (gethash "payload" event))
               (candidate (and (hash-table-p payload)
                               (gethash "pulse_sequence" payload))))
          (when (and (integerp candidate) (> candidate sequence))
            (setf sequence candidate winner event)))))))

(defun %conscious-storage-fold-event-p (event)
  (member (gethash "type" event "")
          '("conscious-lifecycle-transition"
            "conscious-lifecycle-result-rejected"
            "conscious-lifecycle-source-rejected"
            "conscious-lifecycle-semantic-described")
          :test #'string=))

(defun %conscious-storage-summarize-prefix (events agent-id now)
  "Remove history proven irrelevant to every future tail interpretation.

One neutral row represents each gap between retained facts. The result remains
an input to the real projectors; equality is checked before publication."
  (let ((outstanding (%conscious-storage-outstanding-ids events agent-id now))
        (pulse (%conscious-storage-max-pulse-event events))
        (output nil) (pending nil) (kept 0) (accounted 0))
    (multiple-value-bind (references full-receipts)
        (%conscious-storage-reference-ids events)
      (labels ((flush-gap ()
                 (when pending
                   (push (%conscious-storage-accounted-stub pending) output)
                   (incf accounted)
                   (setf pending nil))))
        (dolist (event events)
          (let* ((id (gethash "id" event))
                 (keep-full
                   (or (eq event pulse)
                       (%conscious-storage-fold-event-p event)
                       (gethash id outstanding)
                       (gethash id full-receipts)))
                 (keep-reference (and (not keep-full) (gethash id references))))
            (cond
              (keep-full
               (flush-gap)
               (push event output)
               (incf kept))
              (keep-reference
               (flush-gap)
               (push (%conscious-storage-accounted-stub event) output)
               (incf kept))
              (t (setf pending event)))))
        (flush-gap))
      (values (nreverse output) kept accounted))))

(defun %conscious-storage-events-json-bytes (events)
  (loop for event in events
        sum (length (sb-ext:string-to-octets
                     (%storage-json event) :external-format :utf-8))))

(defun %conscious-storage-tail-reference-hydration
    (backend prefix tail agent-id)
  "Point-read only old rows explicitly referenced by the new physical tail."
  (let ((seen (make-hash-table :test #'equal)) (hydrated nil))
    (dolist (event prefix) (setf (gethash (gethash "id" event) seen) t))
    (multiple-value-bind (references full-receipts)
        (%conscious-storage-reference-ids tail)
      (maphash
       (lambda (id ignored)
         (declare (ignore ignored))
         (unless (gethash id seen)
           (let ((source (storage-read-event backend id :agent-id agent-id)))
             (when source
               (push (if (gethash id full-receipts)
                         source
                         (%conscious-storage-accounted-stub source))
                     hydrated)
               (setf (gethash id seen) t)))))
       references))
    (nreverse hydrated)))

(defun %conscious-storage-stream
    (backend agent-id &key (after-position 0) collect-full-p)
  (let ((events nil) (source-bytes 0) (capsule-bytes 0)
        (retained 0) (stubbed 0) (last-position after-position))
    (multiple-value-bind (complete observed-position count observed-source-bytes)
        (storage-map-events
         backend
         (lambda (event position)
           (let* ((projected (if collect-full-p event
                                 (%conscious-storage-compact-event event agent-id)))
                  (projected-json (%storage-json projected)))
             (incf capsule-bytes
                   (length (sb-ext:string-to-octets
                            projected-json :external-format :utf-8)))
             (if (eq projected event) (incf retained) (incf stubbed))
             (push projected events)
             (setf last-position position)))
         :agent-id agent-id :after-position after-position)
      (declare (ignore observed-position))
      (unless complete
        (error 'storage-error :operation :projection-stream
               :detail "storage stream did not complete"))
      (setf source-bytes observed-source-bytes)
      (values (nreverse events) last-position count source-bytes capsule-bytes
              retained stubbed))))

(defun conscious-storage-full-project
    (backend &key (agent-id "default") (now (get-universal-time)))
  "Qualification path: materialize and project every exact SQLite event."
  (multiple-value-bind
        (events position count source-bytes projected-bytes retained stubbed)
      (%conscious-storage-stream backend agent-id :collect-full-p t)
    (declare (ignore projected-bytes retained stubbed))
    (values
     (%conscious-storage-project-events events agent-id now)
     (%storage-object "schema_version" 1 "route" "sqlite-full"
                      "event_count" count "through_storage_position" position
                      "source_json_bytes" source-bytes))))

(defun conscious-storage-build-checkpoint
    (backend &key checkpoint-backend (agent-id "default")
                  (now (get-universal-time)))
  "Stream, compact, project and publish one rebuildable checkpoint."
  (setf checkpoint-backend (or checkpoint-backend backend))
  (multiple-value-bind
        (events position count source-bytes capsule-event-bytes retained stubbed)
      (%conscious-storage-stream backend agent-id)
    (multiple-value-bind (summary-events summary-kept summary-accounted)
        (%conscious-storage-summarize-prefix events agent-id now)
      (let* ((bundle (%conscious-storage-project-events events agent-id now))
             (summary-bundle
               (%conscious-storage-project-events summary-events agent-id now))
           (composition (gethash "composition_hash" bundle))
           (projection-hash (%conscious-storage-canonical-hash bundle))
           (summary-hash (%conscious-storage-canonical-hash summary-bundle))
           (summary-bytes (%conscious-storage-events-json-bytes summary-events))
           ;; Bind both watermarks to the prefix actually observed. Asking the
           ;; backend for its maximum after streaming would let an append in
           ;; that gap claim an event absent from this capsule.
           (event-id
             (reduce #'max events :key (lambda (event) (gethash "id" event 0))
                     :initial-value 0))
           (source-binding
             (storage-checkpoint-source-binding
              backend :agent-id agent-id :through-event-id event-id
              :through-position position))
           (capsule
             (%storage-object
              "schema_version" 1 "checkpoint_now" (or now :null)
              "composition_hash" composition
              "projection_hash" projection-hash
              "event_source_binding" source-binding
              "source_event_count" count "retained_event_count" retained
              "stubbed_event_count" stubbed "source_json_bytes" source-bytes
              "stream_capsule_event_json_bytes" capsule-event-bytes
              "capsule_event_json_bytes" summary-bytes
              "capsule_event_count" (length summary-events)
              "summary_kept_event_count" summary-kept
              "summary_accounted_run_count" summary-accounted
              "events" (coerce summary-events 'vector))))
       (unless (string= projection-hash summary-hash)
         (error 'storage-integrity-error :operation :projection-checkpoint
                :detail "bounded prefix summary changed conscious projection"))
       (when (or (> event-id (storage-max-event-id backend :agent-id agent-id))
                 (> position (storage-head-position backend :agent-id agent-id)))
         (error 'storage-conflict-error :operation :projection-checkpoint
                :detail "checkpoint watermarks exceed durable event history"))
       (let ((checkpoint
               (storage-publish-checkpoint
                checkpoint-backend *conscious-storage-checkpoint-name* capsule
                :agent-id agent-id :through-event-id event-id
                :through-position position
                :projector-revision *conscious-storage-projector-revision*
                :policy-revision composition)))
         (values
          (%storage-object
           "schema_version" 1 "status" "checkpointed"
           "event_count" count "retained_event_count" retained
           "stubbed_event_count" stubbed "source_json_bytes" source-bytes
           "stream_capsule_event_json_bytes" capsule-event-bytes
           "capsule_event_json_bytes" summary-bytes
           "capsule_event_count" (length summary-events)
           "summary_kept_event_count" summary-kept
           "summary_accounted_run_count" summary-accounted
           "through_event_id" event-id "through_storage_position" position
           "composition_hash" composition "projection_hash" projection-hash
           "checkpoint_integrity_hash" (gethash "integrity_hash" checkpoint))
          bundle))))))

(defun conscious-storage-restore-event-sequence
    (backend &key checkpoint-backend (agent-id "default"))
  "Verify a bounded prefix and return it with only its physical tail."
  (setf checkpoint-backend (or checkpoint-backend backend))
  (let ((checkpoint
          (storage-load-checkpoint
           checkpoint-backend *conscious-storage-checkpoint-name*
           :agent-id agent-id)))
    (unless checkpoint
      (error 'storage-conflict-error :operation :projection-restore
             :detail "conscious projection checkpoint is absent"))
    (unless (string= *conscious-storage-projector-revision*
                     (gethash "projector_revision" checkpoint ""))
      (error 'storage-conflict-error :operation :projection-restore
             :detail "conscious projector revision changed"))
    (let* ((capsule (gethash "state" checkpoint))
           (events (coerce (gethash "events" capsule) 'list))
           (checkpoint-now (gethash "checkpoint_now" capsule))
           (checkpoint-bundle
             (%conscious-storage-project-events events agent-id checkpoint-now))
           (composition (gethash "composition_hash" checkpoint-bundle)))
      (unless
          (string=
           (gethash "event_source_binding" capsule "")
           (storage-checkpoint-source-binding
            backend :agent-id agent-id
            :through-event-id (gethash "through_event_id" checkpoint)
            :through-position (gethash "through_storage_position" checkpoint)))
        (error 'storage-integrity-error :operation :projection-restore
               :detail "checkpoint belongs to a different event source"))
      (unless (and (string= composition
                            (gethash "composition_hash" capsule ""))
                   (string= composition
                            (gethash "policy_revision" checkpoint "")))
        (error 'storage-conflict-error :operation :projection-restore
               :detail "conscious projection composition changed"))
      (unless (string= (%conscious-storage-canonical-hash checkpoint-bundle)
                       (gethash "projection_hash" capsule ""))
        (error 'storage-integrity-error :operation :projection-restore
               :detail "checkpoint projection hash mismatch"))
      (multiple-value-bind
            (tail tail-position tail-count source-bytes capsule-bytes
                  retained stubbed)
          (%conscious-storage-stream
           backend agent-id
           :after-position (gethash "through_storage_position" checkpoint))
        (let* ((hydrated
                 (%conscious-storage-tail-reference-hydration
                  backend events tail agent-id))
                (all-events (append events hydrated tail)))
           (values all-events
                   (%storage-object
             "schema_version" 1 "status" "restored"
            "prefix_event_count" (length events) "tail_event_count" tail-count
            "hydrated_prefix_reference_count" (length hydrated)
            "tail_source_json_bytes" source-bytes
            "tail_capsule_event_json_bytes" capsule-bytes
            "tail_retained_event_count" retained
            "tail_stubbed_event_count" stubbed
             "checkpoint_storage_position"
             (gethash "through_storage_position" checkpoint)
             "through_storage_position" tail-position)))))))

(defun conscious-storage-restore-checkpoint-tail
    (backend &key checkpoint-backend (agent-id "default")
                  (now (get-universal-time)))
  "Verify a checkpoint and project its bounded prefix plus physical tail."
  (multiple-value-bind (events report)
      (conscious-storage-restore-event-sequence
       backend :checkpoint-backend checkpoint-backend :agent-id agent-id)
    (let ((bundle (%conscious-storage-project-events events agent-id now)))
      (setf (gethash "composition_hash" report)
            (gethash "composition_hash" bundle)
            (gethash "projection_hash" report)
            (%conscious-storage-canonical-hash bundle))
      (values bundle report))))

(defun conscious-storage-refresh-checkpoint
    (backend &key checkpoint-backend (agent-id "default")
                  (now (get-universal-time)))
  "Advance a verified checkpoint from its bounded prefix and physical tail;
never reopen the pre-checkpoint storage range."
  (multiple-value-bind (events restore-report)
      (conscious-storage-restore-event-sequence
       backend :checkpoint-backend checkpoint-backend :agent-id agent-id)
    (multiple-value-bind (summary-events kept accounted)
        (%conscious-storage-summarize-prefix events agent-id now)
      (let* ((bundle (%conscious-storage-project-events events agent-id now))
             (summary-bundle
               (%conscious-storage-project-events summary-events agent-id now))
             (composition (gethash "composition_hash" bundle))
             (projection-hash (%conscious-storage-canonical-hash bundle))
             (summary-hash (%conscious-storage-canonical-hash summary-bundle))
             (position (gethash "through_storage_position" restore-report))
             (event-id
               (reduce #'max events
                       :key (lambda (event) (gethash "id" event 0))
                       :initial-value 0))
             (source-binding
               (storage-checkpoint-source-binding
                backend :agent-id agent-id :through-event-id event-id
                :through-position position))
             (capsule
               (%storage-object
                "schema_version" 1 "checkpoint_now" now
                "composition_hash" composition
                "projection_hash" projection-hash
                "event_source_binding" source-binding
                "source_event_count" (length events)
                "retained_event_count" kept
                "stubbed_event_count" accounted
                "source_json_bytes" 0
                "stream_capsule_event_json_bytes" 0
                "capsule_event_json_bytes"
                (%conscious-storage-events-json-bytes summary-events)
                "capsule_event_count" (length summary-events)
                "summary_kept_event_count" kept
                "summary_accounted_run_count" accounted
                "events" (coerce summary-events 'vector))))
        (unless (string= projection-hash summary-hash)
          (error 'storage-integrity-error :operation :projection-refresh
                 :detail "refreshed bounded prefix changed conscious projection"))
        (let ((target (or checkpoint-backend backend)))
          (when (or (> event-id
                       (storage-max-event-id backend :agent-id agent-id))
                    (> position
                       (storage-head-position backend :agent-id agent-id)))
            (error 'storage-conflict-error :operation :projection-refresh
                   :detail "checkpoint watermarks exceed durable event history"))
          (storage-publish-checkpoint
           target *conscious-storage-checkpoint-name* capsule
         :agent-id agent-id :through-event-id event-id
         :through-position position
         :projector-revision *conscious-storage-projector-revision*
           :policy-revision composition))
        (%storage-object
         "schema_version" 1 "status" "refreshed"
         "event_count" (length events)
         "capsule_event_count" (length summary-events)
         "through_event_id" event-id
         "through_storage_position" position
         "projection_hash" projection-hash)))))
