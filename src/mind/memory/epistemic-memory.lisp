;;;; epistemic-memory.lisp -- provenance, admission, and lineage.
;;;;
;;;; MEMORY-ADMIT-NODE validates typed evidence and writes the node plus its
;;;; DERIVED-FROM edges in one Postgres transaction. Legacy callers remain
;;;; compatible until *EPISTEMIC-MEMORY-MODE* is deliberately enforced.

(in-package :agent)

(export '(ensure-epistemic-memory-schema epistemic-memory-schema-report
          epistemic-admission-report memory-admit-node memory-lineage-roots
          memory-grounded-p memory-quarantine memory-supersede
          epistemic-admission-error))

(define-condition epistemic-admission-error (error)
  ((reasons :initarg :reasons :reader epistemic-admission-error-reasons))
  (:report (lambda (condition stream)
             (format stream "Epistemic admission rejected: ~{~a~^; ~}"
                     (epistemic-admission-error-reasons condition)))))

(defparameter *epistemic-origin-classes*
  '("lived-user" "lived-agent-action" "tool-result" "external-signal"
    "imported" "synthetic" "legacy-unclassified"))
(defparameter *epistemic-statuses*
  '("direct-event" "user-report" "agent-action" "supported-inference"
    "hypothesis" "prediction" "rejected" "legacy-unclassified"))
(defparameter *epistemic-grounding-statuses*
  '("grounded" "partially-grounded" "ungrounded" "unclassified"))
(defparameter *epistemic-self-process-event-types*
  '("tick-end" "tick-terminal" "reflection-pass" "self-model-revised"
    "prediction-outcome" "contradiction-detected" "episode-flushed"
    "attention-schema-update"))

(defvar *epistemic-event-resolver* nil
  "Optional deterministic test resolver. Production uses the event ledger.")
(defvar *epistemic-before-lineage-hook* nil
  "Test-only forced-failure seam called after node INSERT, before lineage INSERT.")
(defvar *epistemic-admission-stats* (make-hash-table :test #'equal))
(defvar *epistemic-stats-lock* (bt:make-lock "epistemic-admission-stats"))
(defvar *event-ring* nil)

(declaim (ftype function epistemic-memory-schema-report))

(defun %epistemic-normalize-string (value)
  (and value (string-downcase (string value))))

(defun %epistemic-id-list (value)
  "Return VALUE as a fresh list of ids; accept lists and decoded JSON vectors."
  (cond ((null value) nil)
        ((listp value) (copy-list value))
        ((vectorp value) (coerce value 'list))
        (t (list value))))

(defun %epistemic-record-stat (key)
  (bt:with-lock-held (*epistemic-stats-lock*)
    (incf (gethash key *epistemic-admission-stats* 0))))

(defun epistemic-admission-report ()
  (bt:with-lock-held (*epistemic-stats-lock*)
    (let ((counts (obj)))
      (maphash (lambda (key value) (setf (gethash key counts) value))
               *epistemic-admission-stats*)
      (obj "schema_version" 1
           "mode" (if (boundp '*epistemic-memory-mode*)
                      (%stabilization-json-mode *epistemic-memory-mode*)
                      "legacy")
           "counts" counts))))

(defun ensure-epistemic-memory-schema ()
  "Idempotently add the columns and indexes. Existing rows remain
intact and receive explicit legacy/unclassified defaults."
  (with-pg
    (pomo:with-transaction ()
      (dolist (ddl
                '("ALTER TABLE memory_nodes ADD COLUMN IF NOT EXISTS origin_class text NOT NULL DEFAULT 'legacy-unclassified'"
                  "ALTER TABLE memory_nodes ADD COLUMN IF NOT EXISTS epistemic_status text NOT NULL DEFAULT 'legacy-unclassified'"
                  "ALTER TABLE memory_nodes ADD COLUMN IF NOT EXISTS producer text"
                  "ALTER TABLE memory_nodes ADD COLUMN IF NOT EXISTS model_purpose text"
                  "ALTER TABLE memory_nodes ADD COLUMN IF NOT EXISTS confidence double precision"
                  "ALTER TABLE memory_nodes ADD COLUMN IF NOT EXISTS grounding_status text NOT NULL DEFAULT 'unclassified'"
                  "ALTER TABLE memory_nodes ADD COLUMN IF NOT EXISTS root_observation_ids jsonb NOT NULL DEFAULT '[]'::jsonb"
                  "ALTER TABLE memory_nodes ADD COLUMN IF NOT EXISTS generation_id text"
                  "ALTER TABLE memory_nodes ADD COLUMN IF NOT EXISTS supersedes_node_id text"
                  "ALTER TABLE memory_nodes ADD COLUMN IF NOT EXISTS quarantined boolean NOT NULL DEFAULT false"
                  "ALTER TABLE memory_nodes ADD COLUMN IF NOT EXISTS epistemic_metadata jsonb NOT NULL DEFAULT '{}'::jsonb"
                  "ALTER TABLE memory_nodes ADD COLUMN IF NOT EXISTS retrieval_embedding vector(768)"
                  "CREATE INDEX IF NOT EXISTS memory_nodes_epistemic_idx ON memory_nodes (origin_class, epistemic_status, grounding_status, quarantined)"
                  "CREATE INDEX IF NOT EXISTS memory_nodes_generation_idx ON memory_nodes (generation_id)"
                  "CREATE INDEX IF NOT EXISTS memory_nodes_retrieval_embedding_idx ON memory_nodes USING hnsw (retrieval_embedding vector_cosine_ops)"))
        (pomo:execute ddl))))
  (epistemic-memory-schema-report))

(defun epistemic-memory-schema-report ()
  (with-pg
    (let ((columns
            (pomo:query
             "SELECT column_name FROM information_schema.columns WHERE table_name='memory_nodes' AND column_name IN ('origin_class','epistemic_status','producer','model_purpose','confidence','grounding_status','root_observation_ids','generation_id','supersedes_node_id','quarantined','epistemic_metadata','retrieval_embedding') ORDER BY column_name"
             :column))
          (indexes
            (pomo:query
             "SELECT indexname FROM pg_indexes WHERE tablename='memory_nodes' AND indexname IN ('memory_nodes_epistemic_idx','memory_nodes_generation_idx','memory_nodes_retrieval_embedding_idx') ORDER BY indexname"
             :column)))
      (obj "schema_version" 1
           "provenance_column_count" (length columns)
           "provenance_columns" (coerce columns 'vector)
           "index_count" (length indexes)
           "indexes" (coerce indexes 'vector)))))

(defun %epistemic-event-id (source-event-id)
  (etypecase source-event-id
    (null nil)
    (integer source-event-id)
    (string (parse-integer source-event-id :junk-allowed t))))

(defun %epistemic-find-event (source-event-id)
  (let ((id (%epistemic-event-id source-event-id)))
    (when id
      (cond
        (*epistemic-event-resolver* (funcall *epistemic-event-resolver* id))
        ((and (boundp '*event-ring*)
              (find id *event-ring* :key (lambda (event) (gethash "id" event))))
         (find id *event-ring* :key (lambda (event) (gethash "id" event))))
        ((fboundp '%event-read-all-from-disk)
         (find id (funcall '%event-read-all-from-disk)
               :key (lambda (event) (gethash "id" event))))
        (t nil)))))

(defun %epistemic-required-event-type (origin status)
  (cond ((and (string= origin "lived-user") (string= status "user-report"))
         "user-message")
        ((and (string= origin "lived-agent-action") (string= status "agent-action"))
         "agent-message")
        ((string= origin "tool-result") "tool-result")
        ((string= origin "external-signal") "external-signal")
        (t nil)))

(defun %epistemic-direct-root-p (origin status)
  (or (and (string= origin "lived-user") (string= status "user-report"))
      (and (string= origin "lived-agent-action") (string= status "agent-action"))
      (and (string= origin "tool-result") (string= status "direct-event"))
      (and (string= origin "external-signal") (string= status "direct-event"))))

(defun %epistemic-node-row-current-connection (id)
  (pomo:query
   "SELECT id, origin_class, epistemic_status, grounding_status, quarantined FROM memory_nodes WHERE id=$1"
   id :row))

(defun %epistemic-parent-ids-current-connection (id)
  (pomo:query
   "SELECT to_id FROM memory_edges WHERE from_id=$1 AND edge_type='derived-from' ORDER BY to_id"
   id :column))

(defun %epistemic-lineage-roots-with-accessors
    (id node-row-fn parent-ids-fn &key reject-id)
  (let ((visiting (make-hash-table :test #'equal))
        (visited (make-hash-table :test #'equal)))
    (labels ((walk (node-id)
               (when (and reject-id (string= node-id reject-id))
                 (error 'epistemic-admission-error
                        :reasons (list (format nil "lineage cycle reaches candidate ~a" reject-id))))
               (when (gethash node-id visiting)
                 (error 'epistemic-admission-error
                        :reasons (list (format nil "lineage cycle at ~a" node-id))))
               (or (gethash node-id visited)
                   (let ((row (funcall node-row-fn node-id)))
                     (unless row
                       (error 'epistemic-admission-error
                              :reasons (list (format nil "missing lineage node ~a" node-id))))
                     (destructuring-bind (row-id origin status grounding quarantined) row
                       (declare (ignore grounding))
                       (when quarantined
                         (error 'epistemic-admission-error
                                :reasons (list (format nil "quarantined lineage node ~a" row-id))))
                       (setf (gethash node-id visiting) t)
                       (let ((roots
                               (if (%epistemic-direct-root-p origin status)
                                   (list row-id)
                                   (let ((parents (funcall parent-ids-fn row-id)))
                                     (unless parents
                                       (error 'epistemic-admission-error
                                              :reasons (list (format nil "lineage for ~a has no lived root" row-id))))
                                     (remove-duplicates (mapcan #'walk parents)
                                                        :test #'string=)))))
                         (remhash node-id visiting)
                         (setf (gethash node-id visited) roots)
                         roots))))))
      (walk id))))

(defun %epistemic-lineage-roots-current-connection (id &key reject-id)
  (%epistemic-lineage-roots-with-accessors
   id #'%epistemic-node-row-current-connection
   #'%epistemic-parent-ids-current-connection :reject-id reject-id))

(defun memory-lineage-roots (id)
  "Return the distinct lived root node ids reachable from ID. Signals on
missing, quarantined, rootless, or cyclic lineage rather than guessing."
  (when (and (boundp '*memory-cognitive-mutation-mode*)
             (eq *memory-cognitive-mutation-mode* :event-first))
    (return-from memory-lineage-roots
      (%epistemic-lineage-roots-with-accessors
       id
       (lambda (node-id)
         (let* ((envelope
                  (memory-storage-operation-node
                   *memory-search-storage-backend* node-id))
                (row (and envelope
                          (%memory-storage-json-read
                           (gethash "scalar_json" envelope)
                           :memory-lineage-roots))))
           (and row
                (list (gethash "id" row) (gethash "origin_class" row)
                      (gethash "epistemic_status" row)
                      (gethash "grounding_status" row)
                      (gethash "quarantined" row)))))
       (lambda (node-id)
         (mapcar
          (lambda (row-json)
            (gethash "to_id"
                     (%memory-storage-json-read row-json
                                                :memory-lineage-roots)))
          (memory-storage-operation-edges
           *memory-search-storage-backend* :from-id node-id
           :edge-type "derived-from"))))))
  (with-pg (%epistemic-lineage-roots-current-connection id)))

(defun %epistemic-metadata-copy (metadata)
  (let ((copy (obj)))
    (when (hash-table-p metadata)
      (maphash (lambda (key value) (setf (gethash key copy) value)) metadata))
    copy))

(defun %epistemic-valid-self-process-event-p (event-id)
  (let ((event (%epistemic-find-event event-id)))
    (and event
         (member (gethash "type" event) *epistemic-self-process-event-types*
                 :test #'string=))))

(defun %epistemic-validation-reasons-with-accessors
    (node-row-fn parent-ids-fn
     &key id kind source-event-id origin-class epistemic-status producer
          model-purpose confidence grounding-status lineage-parent-ids
          novelty-passed self-process-event-id epistemic-metadata)
  (declare (ignore model-purpose))
  (let* ((origin (%epistemic-normalize-string origin-class))
         (status (%epistemic-normalize-string epistemic-status))
         (grounding (%epistemic-normalize-string grounding-status))
         (kind-name (%epistemic-normalize-string (or kind "observation")))
         (parents (remove-duplicates (%epistemic-id-list lineage-parent-ids)
                                     :test #'string=))
         (reasons nil)
         (roots nil))
    (unless (member origin *epistemic-origin-classes* :test #'string=)
      (push "invalid or missing origin_class" reasons))
    (unless (member status *epistemic-statuses* :test #'string=)
      (push "invalid or missing epistemic_status" reasons))
    (unless (member grounding *epistemic-grounding-statuses* :test #'string=)
      (push "invalid or missing grounding_status" reasons))
    (when (or (null producer) (and (stringp producer) (zerop (length producer))))
      (push "producer is required" reasons))
    (when (and (member origin '("synthetic") :test #'string=)
               (member status '("direct-event" "user-report" "agent-action")
                       :test #'string=))
      (push "synthetic origin cannot claim a direct lived status" reasons))
    (let ((required-type (and origin status
                              (%epistemic-required-event-type origin status))))
      (when required-type
        (let ((event (%epistemic-find-event source-event-id)))
          (unless event (push "required source event is missing" reasons))
          (when (and event (not (string= (gethash "type" event "") required-type)))
            (push (format nil "source event type mismatch: expected ~a" required-type)
                  reasons)))))
    (when (and (string= (or origin "") "tool-result")
               (or (null producer) (string= producer "")))
      (push "tool-result requires tool identity as producer" reasons))
    (when (and (string= (or origin "") "imported")
               (not (and (hash-table-p epistemic-metadata)
                         (gethash "import_source" epistemic-metadata))))
      (push "imported memory requires import_source metadata" reasons))
    (when (%epistemic-direct-root-p (or origin "") (or status ""))
      (when parents
        (push "direct lived evidence cannot have derived-from parents" reasons))
      (unless (string= (or grounding "") "grounded")
        (push "direct lived evidence must be grounded" reasons))
      (when id (setf roots (list id))))
    (dolist (parent parents)
      (when (and id (string= parent id))
        (push "lineage cannot cite the candidate itself" reasons))
      (unless (and id (string= parent id))
        (handler-case
            (setf roots (nconc roots
                               (%epistemic-lineage-roots-with-accessors
                                parent node-row-fn parent-ids-fn
                                :reject-id id)))
          (epistemic-admission-error (e)
            (setf reasons (nconc (copy-list (epistemic-admission-error-reasons e))
                                 reasons))))))
    (setf roots (remove-duplicates roots :test #'string=))
    (when (string= (or origin "") "synthetic")
      (cond
        ((member kind-name '("thought" "prediction") :test #'string=)
         (unless (plusp (length roots))
           (push (format nil "~a requires at least one lived root" kind-name) reasons)))
        ((string= kind-name "reflection")
         (unless (or (>= (length roots) 2)
                     (and (plusp (length roots))
                          (%epistemic-valid-self-process-event-p self-process-event-id)))
           (push "reflection requires two lived roots or one root plus a typed self-process event"
                 reasons)))
        ((string= kind-name "worldview")
         (unless (>= (length roots) 2)
           (push "worldview requires at least two lived roots" reasons))
         (unless novelty-passed (push "worldview requires an explicit novelty pass" reasons))
         (unless (and (numberp confidence) (<= 0.0d0 confidence 1.0d0))
           (push "worldview requires confidence in [0,1]" reasons)))
        (t
         (unless (plusp (length roots))
           (push "synthetic memory requires at least one lived root" reasons))))
      (when (member grounding '("unclassified" "ungrounded") :test #'string=)
        (push "admitted synthetic memory must be grounded or partially-grounded" reasons)))
    (values (nreverse (remove-duplicates reasons :test #'string=)) roots)))

(defun %epistemic-validation-reasons-current-connection
    (&rest arguments &key &allow-other-keys)
  (apply #'%epistemic-validation-reasons-with-accessors
         #'%epistemic-node-row-current-connection
         #'%epistemic-parent-ids-current-connection arguments))

(defun %epistemic-admission-metadata (metadata novelty-passed self-process-event-id)
  (let ((result (%epistemic-metadata-copy metadata)))
    (when novelty-passed (setf (gethash "novelty_passed" result) t))
    (when self-process-event-id
      (setf (gethash "self_process_event_id" result) self-process-event-id))
    result))

(defun %epistemic-serialization-failure-p (condition)
  (let* ((symbol (find-symbol "SERIALIZATION-FAILURE" "CL-POSTGRES-ERROR"))
         (class (and symbol (find-class symbol nil))))
    (and class (typep condition class))))

(defun %epistemic-run-serializable (thunk &key (max-attempts 8))
  "Retry only PostgreSQL serialization failures. Each THUNK call must open a
fresh connection and transaction; validation and all writes are repeated."
  (loop for attempt from 1 to max-attempts
        do (handler-case (return (funcall thunk))
             (error (condition)
               (unless (and (< attempt max-attempts)
                            (%epistemic-serialization-failure-p condition))
                 (error condition))
               (%epistemic-record-stat "serialization-retry")
               (sleep (min 0.05d0 (* attempt 0.005d0)))))))

(defun memory-admit-node (&key kind content (importance nil) (valence 0.0)
                               (arousal 0.3) source-event-id id
                               origin-class epistemic-status producer model-purpose
                               confidence grounding-status generation-id
                               supersedes-node-id quarantined epistemic-metadata
                               lineage-parent-ids novelty-passed self-process-event-id)
  "Validate and transactionally admit a typed node plus DERIVED-FROM edges."
  (let* ((node-id (or id (format nil "node-~a-~4,'0x"
                                  (get-universal-time) (random 65536))))
         (importance-value (or importance (%score-importance (or content ""))))
         (embedding (embed-text (or content "")))
         (retrieval-embedding (embed-retrieval-document (or content "")))
         (metadata (%epistemic-admission-metadata
                    epistemic-metadata novelty-passed self-process-event-id))
         (roots nil))
    (handler-case
        (multiple-value-bind (committed-id committed-roots)
            (memory-cognitive-mutation-dispatch
             "admission"
             (obj "node_id" node-id "kind" kind "content" content
                  "importance" importance-value "valence" valence
                  "arousal" arousal "source_event_id" (or source-event-id :null)
                  "origin_class" (%epistemic-normalize-string origin-class)
                  "epistemic_status" (%epistemic-normalize-string epistemic-status)
                  "producer" (or producer :null)
                  "model_purpose" (or model-purpose :null)
                  "confidence" (or confidence :null)
                  "grounding_status" (%epistemic-normalize-string grounding-status)
                  "generation_id" (or generation-id :null)
                  "supersedes_node_id" (or supersedes-node-id :null)
                  "quarantined" (if quarantined t nil)
                  "epistemic_metadata" metadata
                  "lineage_parent_ids"
                  (coerce (%epistemic-id-list lineage-parent-ids) 'vector)
                  "novelty_passed" (if novelty-passed t nil)
                  "self_process_event_id" (or self-process-event-id :null)
                  "embedding" (coerce embedding 'vector)
                  "retrieval_embedding" (coerce retrieval-embedding 'vector))
             (lambda ()
               (%epistemic-run-serializable
                (lambda ()
                  (%call-with-memory-durable-event-buffer
                   (lambda ()
                     (with-pg
                       (pomo:with-transaction (:serializable)
                         (multiple-value-bind (reasons resolved-roots)
                             (%epistemic-validation-reasons-current-connection
                              :id node-id :kind kind
                              :source-event-id source-event-id
                              :origin-class origin-class
                              :epistemic-status epistemic-status
                              :producer producer :model-purpose model-purpose
                              :confidence confidence
                              :grounding-status grounding-status
                              :lineage-parent-ids lineage-parent-ids
                              :novelty-passed novelty-passed
                              :self-process-event-id self-process-event-id
                              :epistemic-metadata metadata)
                           (when reasons
                             (error 'epistemic-admission-error
                                    :reasons reasons))
                           (setf roots resolved-roots))
                         (%memory-insert-node-current-connection
                          node-id kind content embedding retrieval-embedding
                          importance-value valence arousal source-event-id
                          (%epistemic-normalize-string origin-class)
                          (%epistemic-normalize-string epistemic-status)
                          producer model-purpose confidence
                          (%epistemic-normalize-string grounding-status) roots
                          generation-id supersedes-node-id quarantined metadata)
                         ;; Re-admission replaces only lineage edges.
                         (%memory-delete-edges-current-connection
                          node-id "derived-from"
                          "admission-lineage-replacement")
                         (when *epistemic-before-lineage-hook*
                           (funcall *epistemic-before-lineage-hook* node-id))
                         (dolist (parent
                                  (remove-duplicates
                                   (%epistemic-id-list lineage-parent-ids)
                                   :test #'string=))
                           (%memory-insert-edge-current-connection
                            node-id parent "derived-from"
                            "admission-lineage"))))))))
                  (values node-id roots)))
          (unless (string= committed-id node-id)
            (error 'memory-storage-error :operation :route-cognitive-mutation
                   :detail "admission router changed the node identity"))
          (when committed-roots (setf roots committed-roots)))
      (epistemic-admission-error (e)
        (%epistemic-record-stat "rejected")
        (when (fboundp 'log-event)
          (ignore-errors
            (funcall 'log-event "epistemic-admission-rejected"
                     (obj "node_id" node-id "kind" (or kind :null)
                          "reasons" (coerce (epistemic-admission-error-reasons e)
                                            'vector)))))
        (error e)))
    (%memory-after-write node-id kind content importance-value)
    (%epistemic-record-stat "admitted")
    (when (fboundp 'log-event)
      (ignore-errors
        (funcall 'log-event "epistemic-admission-accepted"
                 (obj "node_id" node-id "kind" kind
                      "origin_class" (%epistemic-normalize-string origin-class)
                      "epistemic_status" (%epistemic-normalize-string epistemic-status)
                      "grounding_status" (%epistemic-normalize-string grounding-status)
                      "root_count" (length roots)))))
    node-id))

(defun %epistemic-shadow-assess-write
    (node-id &key kind source-event-id origin-class epistemic-status producer
                  model-purpose confidence grounding-status lineage-parent-ids
                  novelty-passed self-process-event-id)
  "Audit what enforced admission would do, without changing the legacy write."
  (multiple-value-bind (reasons roots)
      (handler-case
          (with-pg
            (%epistemic-validation-reasons-current-connection
             :id node-id :kind kind :source-event-id source-event-id
             :origin-class origin-class :epistemic-status epistemic-status
             :producer producer :model-purpose model-purpose :confidence confidence
             :grounding-status grounding-status
             :lineage-parent-ids lineage-parent-ids :novelty-passed novelty-passed
             :self-process-event-id self-process-event-id))
        (error (e) (values (list (format nil "shadow validator error: ~a" (type-of e))) nil)))
    (%epistemic-record-stat (if reasons "shadow-would-reject" "shadow-would-admit"))
    (when (fboundp 'log-event)
      (funcall 'log-event "epistemic-admission-shadow"
               (obj "node_id" node-id "kind" (or kind :null)
                    "decision" (if reasons "would-reject" "would-admit")
                    "reasons" (coerce reasons 'vector)
                    "root_count" (length roots))))
    (values (null reasons) reasons roots)))

(defun memory-grounded-p (id)
  (when (and (boundp '*memory-cognitive-mutation-mode*)
             (eq *memory-cognitive-mutation-mode* :event-first))
    (return-from memory-grounded-p
      (handler-case
          (let* ((envelope (memory-storage-operation-node
                            *memory-search-storage-backend* id))
                 (object (and envelope
                              (%memory-storage-json-read
                               (gethash "scalar_json" envelope)
                               :memory-grounded-p)))
                 (row (and object
                           (list (gethash "id" object)
                                 (gethash "origin_class" object)
                                 (gethash "epistemic_status" object)
                                 (gethash "grounding_status" object)
                                 (gethash "quarantined" object)))))
            (and row (not (fifth row))
                 (member (fourth row) '("grounded" "partially-grounded")
                         :test #'string=)
                 (plusp (length (memory-lineage-roots id)))))
        (error () nil))))
  (handler-case
      (with-pg
        (let ((row (%epistemic-node-row-current-connection id)))
          (and row
               (not (fifth row))
               (member (fourth row) '("grounded" "partially-grounded")
                       :test #'string=)
               (plusp (length (%epistemic-lineage-roots-current-connection id))))))
    (error () nil)))

(defun memory-quarantine (id &key (reason "operator quarantine")
                                  (actor "out-of-band"))
  "Reversibly exclude a node without deleting it or its lineage."
  (when (and (boundp '*memory-cognitive-mutation-mode*)
             (eq *memory-cognitive-mutation-mode* :event-first))
    (unless
        (eq t (memory-cognitive-mutation-dispatch
               "quarantine"
               (obj "node_id" id "reason" reason "actor" actor)
               (lambda () (error "PostgreSQL fallback is unavailable"))))
      (error 'memory-storage-error :operation :route-cognitive-mutation
             :detail "quarantine router did not preserve true"))
    (when (fboundp 'log-event)
      (funcall 'log-event "memory-quarantined"
               (obj "node_id" id "reason" reason "actor" actor)))
    (return-from memory-quarantine t))
  (%call-with-memory-durable-event-buffer
   (lambda ()
     (with-pg
       (pomo:with-transaction ()
         (unless (%epistemic-node-row-current-connection id)
           (error "Cannot quarantine missing memory node ~a" id))
         (pomo:execute
          "UPDATE memory_nodes SET quarantined=true, epistemic_metadata=epistemic_metadata || jsonb_build_object('quarantine_reason',$2::text,'quarantined_by',$3::text) WHERE id=$1"
          id reason actor)
         (%memory-queue-node-state-current-connection
          id "update" "quarantine")))))
  (when (fboundp 'log-event)
    (funcall 'log-event "memory-quarantined"
             (obj "node_id" id "reason" reason "actor" actor)))
  t)

(defun memory-supersede (old-id replacement-id &key (reason "superseded")
                                                   (actor "out-of-band"))
  "Record replacement lineage without deleting or automatically quarantining."
  (when (string= old-id replacement-id) (error "A node cannot supersede itself"))
  (unless
      (eq t
          (memory-cognitive-mutation-dispatch
           "supersession"
           (obj "old_node_id" old-id "replacement_node_id" replacement-id
                "reason" reason "actor" actor)
           (lambda ()
             (%call-with-memory-durable-event-buffer
              (lambda ()
                (with-pg
                  (pomo:with-transaction ()
                    (unless (%epistemic-node-row-current-connection old-id)
                      (error "Missing superseded node ~a" old-id))
                    (unless (%epistemic-node-row-current-connection replacement-id)
                      (error "Missing replacement node ~a" replacement-id))
                    (pomo:execute
                     "UPDATE memory_nodes SET supersedes_node_id=$1, epistemic_metadata=epistemic_metadata || jsonb_build_object('supersession_reason',$3::text,'superseded_by_actor',$4::text) WHERE id=$2"
                     old-id replacement-id reason actor)
                    (%memory-queue-node-state-current-connection
                     replacement-id "update" "supersession")
                    (%memory-insert-edge-current-connection
                     replacement-id old-id "supersedes" "supersession")))))
             t)))
    (error 'memory-storage-error :operation :route-cognitive-mutation
           :detail "supersession router did not preserve true"))
  (when (fboundp 'log-event)
    (funcall 'log-event "memory-superseded"
             (obj "old_node_id" old-id "replacement_node_id" replacement-id
                  "reason" reason "actor" actor)))
  t)

(define-init :restore epistemic-memory-restore
    "Restore durable state for epistemic-memory."
  (ensure-epistemic-memory-schema))
