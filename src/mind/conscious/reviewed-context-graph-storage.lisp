;;;; reviewed-context-graph-storage.lisp -- row-oriented durable CCG projection.
;;;;
;;;; The event ledger remains authoritative.  This is a disposable, source-bound
;;;; materialization: graph records are rows, while the checkpoint is only the
;;;; small consistency/watermark receipt for those rows.

(in-package :agent)

(export '(reviewed-context-graph-persist
          reviewed-context-graph-restore))

(defparameter *reviewed-context-graph-projection-name*
  "reviewed-context-graph-v1")
(defparameter *reviewed-context-graph-projector-revision*
  "reviewed-context-graph-sql-v1")
(defparameter *reviewed-context-graph-storage-policy-revision*
  "row-materialization-v1")

(defun %rcgs-fail (detail)
  (error 'storage-integrity-error :operation :reviewed-context-graph
         :detail detail))

(defun %rcgs-json (value)
  (%storage-json value))

(defun %rcgs-read-json (text)
  (%storage-json-read text :reviewed-context-graph))

(defun %rcgs-key (key)
  (cond ((listp key) (values "list" (%rcgs-json (coerce key 'vector))))
        (t (values "scalar" (%rcgs-json key)))))

(defun %rcgs-row-integrity
    (projection agent-id persona-id kind key key-kind payload-json)
  (%storage-sha256
   (format nil "reviewed-context-graph-row-v1|~a|~a|~a|~a|~a|~a|~a"
           projection agent-id persona-id kind key key-kind payload-json)))

(defun %rcgs-record-identity (kind key-json)
  (cons kind key-json))

(defun %rcgs-existing-record-integrities
    (handle projection agent-id persona-id)
  (let ((rows (make-hash-table :test #'equal)))
    (%with-sqlite-statement
        (statement handle
          "SELECT record_kind,record_key,integrity_hash FROM pai_reviewed_graph_records WHERE projection_name=?1 AND agent_id=?2 AND persona_id=?3"
          :reviewed-context-graph-persist)
      (loop for value in (list projection agent-id persona-id)
            for index from 1
            do (%sqlite-bind-text handle statement index value
                                  :reviewed-context-graph-persist))
      (loop for code = (%sqlite-step-raw statement)
            while (= code +sqlite-row+)
            do (setf (gethash
                       (%rcgs-record-identity
                        (%sqlite-column-text statement 0)
                        (%sqlite-column-text statement 1))
                       rows)
                     (%sqlite-column-text statement 2))
            finally (unless (= code +sqlite-done+)
                      (%sqlite-check code handle
                                     :reviewed-context-graph-persist))))
    rows))

(defun %rcgs-upsert-record
    (handle statement projection agent-id persona-id kind key value existing)
  "Write VALUE only when its independently verified row actually changed.
EXISTING is consumed as a seen-set; entries left behind are stale rows."
  (multiple-value-bind (key-kind key-json) (%rcgs-key key)
    (let* ((payload-json (%rcgs-json value))
           (integrity (%rcgs-row-integrity
                       projection agent-id persona-id kind key-json key-kind
                       payload-json))
           (identity (%rcgs-record-identity kind key-json))
           (prior (gethash identity existing)))
      (remhash identity existing)
      (unless (and prior (string= prior integrity))
        (%derived-reset-statement statement)
        (loop for value in (list projection agent-id persona-id kind key-json
                                 key-kind payload-json integrity)
              for index from 1
              do (%sqlite-bind-text handle statement index value
                                    :reviewed-context-graph-persist))
        (%sqlite-step handle statement :reviewed-context-graph-persist
                      +sqlite-done+)
        t))))

(defun %rcgs-index-pair (left right)
  (cons left right))

(defun %rcgs-load-index-pairs
    (handle select-sql projection agent-id persona-id)
  (let ((pairs (make-hash-table :test #'equal)))
    (%with-sqlite-statement
        (statement handle select-sql :reviewed-context-graph-persist)
      (loop for value in (list projection agent-id persona-id)
            for index from 1
            do (%sqlite-bind-text handle statement index value
                                  :reviewed-context-graph-persist))
      (loop for code = (%sqlite-step-raw statement)
            while (= code +sqlite-row+)
            do (setf (gethash
                       (%rcgs-index-pair
                        (%sqlite-column-text statement 0)
                        (%sqlite-column-text statement 1))
                       pairs)
                     t)
            finally (unless (= code +sqlite-done+)
                      (%sqlite-check code handle
                                     :reviewed-context-graph-persist))))
    pairs))

(defun %rcgs-sync-index-pairs
    (handle desired projection agent-id persona-id
     select-sql insert-sql delete-sql)
  "Apply only the set difference between DESIRED and one two-column index."
  (let ((existing (%rcgs-load-index-pairs
                   handle select-sql projection agent-id persona-id))
        (inserted 0)
        (deleted 0))
    (%with-sqlite-statement
        (statement handle insert-sql :reviewed-context-graph-persist)
      (maphash
       (lambda (pair ignored)
         (declare (ignore ignored))
         (if (gethash pair existing)
             (remhash pair existing)
             (progn
               (%derived-reset-statement statement)
               (loop for value in (list projection agent-id persona-id
                                        (car pair) (cdr pair))
                     for index from 1
                     do (%sqlite-bind-text handle statement index value
                                           :reviewed-context-graph-persist))
               (%sqlite-step handle statement :reviewed-context-graph-persist
                             +sqlite-done+)
               (incf inserted))))
       desired))
    (%with-sqlite-statement
        (statement handle delete-sql :reviewed-context-graph-persist)
      (maphash
       (lambda (pair ignored)
         (declare (ignore ignored))
         (%derived-reset-statement statement)
         (loop for value in (list projection agent-id persona-id
                                  (car pair) (cdr pair))
               for index from 1
               do (%sqlite-bind-text handle statement index value
                                     :reviewed-context-graph-persist))
         (%sqlite-step handle statement :reviewed-context-graph-persist
                       +sqlite-done+)
         (incf deleted))
       existing))
    (values inserted deleted)))

(defun %rcgs-alias-index (graph)
  (let ((pairs (make-hash-table :test #'equal)))
    (maphash
     (lambda (entity-id entity)
       (dolist (name
                (append
                 (loop for key in '("label" "name" "canonical_name")
                       for value = (gethash key entity)
                       when (and (stringp value) (plusp (length value)))
                         collect value)
                 (when (vectorp (gethash "aliases" entity))
                   (coerce (gethash "aliases" entity) 'list))))
         (when (and (stringp name) (plusp (length name)))
           (setf (gethash
                  (%rcgs-index-pair
                   entity-id (pai.context-graph::%cg-canonical name))
                  pairs)
                 t))))
     (pai.context-graph::context-graph-entities graph))
    pairs))

(defun %rcgs-adjacency-index (graph)
  (let ((pairs (make-hash-table :test #'equal)))
    (maphash
     (lambda (entity-id fact-ids)
       (when (vectorp fact-ids)
         (map nil
              (lambda (fact-id)
                (when (stringp fact-id)
                  (setf (gethash (%rcgs-index-pair entity-id fact-id) pairs)
                        t)))
              fact-ids)))
     (pai.context-graph::context-graph-entity-adjacency graph))
    pairs))

(defun %rcgs-compact-opening (event)
  "Keep settled retry identity without retaining its sealed source packet."
  (let* ((record (pai.context-graph::%cgro-record event))
         (compact (make-hash-table :test #'equal)))
    (dolist (key '("episode_event_id" "batch_index" "observed_at"
                   "formation_protocol" "fact_input_revision" "attempt"
                   "retry_of" "ontology_revision"))
      (multiple-value-bind (value present-p) (gethash key record)
        (when present-p (setf (gethash key compact) value))))
    (obj "id" (gethash "id" event)
         "agent_id" (gethash "agent_id" event)
         "type" (gethash "type" event)
         "timestamp" (gethash "timestamp" event)
         "caused_by" (gethash "caused_by" event)
         "payload"
         (obj "persona_id" (gethash "persona_id" (gethash "payload" event))
              "generation" (gethash "generation" (gethash "payload" event))
              "record_json" (%rcgs-json compact)))))

(defun %rcgs-write-checkpoint
    (handle agent-id through-event-id through-position state)
  (let* ((projection *reviewed-context-graph-projection-name*)
         (state-json (%storage-json state))
         (integrity
           (%storage-sha256
            (%storage-checkpoint-integrity-input
             projection agent-id through-event-id through-position
             *reviewed-context-graph-projector-revision*
             *reviewed-context-graph-storage-policy-revision* state-json))))
    (%with-sqlite-statement
        (statement handle
                   "INSERT INTO pai_projection_checkpoints(projection_name,agent_id,through_event_id,through_storage_position,projector_revision,policy_revision,state_json,integrity_hash) VALUES(?1,?2,?3,?4,?5,?6,?7,?8) ON CONFLICT(projection_name,agent_id) DO UPDATE SET through_event_id=excluded.through_event_id,through_storage_position=excluded.through_storage_position,projector_revision=excluded.projector_revision,policy_revision=excluded.policy_revision,state_json=excluded.state_json,integrity_hash=excluded.integrity_hash,created_at=CURRENT_TIMESTAMP"
                   :reviewed-context-graph-persist)
      (loop for value in
              (list projection agent-id through-event-id through-position
                    *reviewed-context-graph-projector-revision*
                    *reviewed-context-graph-storage-policy-revision*
                    state-json integrity)
            for index from 1
            do (if (member index '(3 4))
                   (%sqlite-bind-int64 handle statement index value
                                       :reviewed-context-graph-persist)
                   (%sqlite-bind-text handle statement index value
                                      :reviewed-context-graph-persist)))
      (%sqlite-step handle statement :reviewed-context-graph-persist
                    +sqlite-done+))))

(defun reviewed-context-graph-persist
    (backend graph owner &key through-event-id through-position
                           event-storage-id boundary-hash)
  "Atomically synchronize changed projection/index rows and its small watermark."
  (unless (and (typep backend 'sqlite-derived-storage)
               (pai.context-graph::context-graph-p graph)
               (pai.context-graph::cgi-owner-p owner)
               (integerp through-event-id) (<= 0 through-event-id)
               (integerp through-position) (<= 0 through-position)
               (stringp event-storage-id) (plusp (length event-storage-id))
               (stringp boundary-hash) (plusp (length boundary-hash)))
    (%rcgs-fail "reviewed graph persistence inputs are invalid"))
  (let* ((projection *reviewed-context-graph-projection-name*)
         (agent-id (pai.context-graph::cgi-owner-agent-id owner))
         (persona-id (pai.context-graph::cgi-owner-persona-id owner))
         (terminals (pai.context-graph::cgi-owner-terminals owner))
         (settled-exposure 0)
         (record-count 0)
         (written-record-count 0)
         (deleted-record-count 0)
         (alias-count 0)
         (written-alias-count 0)
         (deleted-alias-count 0)
         (adjacency-count 0)
         (written-adjacency-count 0)
         (deleted-adjacency-count 0))
    (bt:with-lock-held ((%sqlite-derived-lock backend))
      (%sqlite-derived-in-transaction
       backend :reviewed-context-graph-persist
       (lambda (handle)
         (let ((existing
                 (%rcgs-existing-record-integrities
                  handle projection agent-id persona-id)))
           (%with-sqlite-statement
               (statement handle
                 "INSERT INTO pai_reviewed_graph_records(projection_name,agent_id,persona_id,record_kind,record_key,key_kind,payload_json,integrity_hash) VALUES(?1,?2,?3,?4,?5,?6,?7,?8) ON CONFLICT(projection_name,agent_id,persona_id,record_kind,record_key) DO UPDATE SET key_kind=excluded.key_kind,payload_json=excluded.payload_json,integrity_hash=excluded.integrity_hash WHERE pai_reviewed_graph_records.integrity_hash<>excluded.integrity_hash"
                 :reviewed-context-graph-persist)
             (labels ((one (kind key value)
                        (when (%rcgs-upsert-record
                               handle statement projection agent-id persona-id
                               kind key value existing)
                          (incf written-record-count))
                        (incf record-count))
                    (table (kind value)
                      (maphash (lambda (key row) (one kind key row)) value)))
             (one "graph-meta" "state"
                  (obj "ontology" (pai.context-graph::context-graph-ontology graph)
                       "authority_profile" (or (pai.context-graph::context-graph-authority-profile graph) :null)
                       "authority_partition" (or (pai.context-graph::context-graph-authority-partition graph) :null)
                       "through_event_id" (pai.context-graph::context-graph-through-event-id graph)
                       "projection_digest" (or (pai.context-graph::context-graph-projection-digest graph) :null)))
             (table "entity" (pai.context-graph::context-graph-entities graph))
             (table "entity-index" (pai.context-graph::context-graph-entity-index graph))
             (table "fact" (pai.context-graph::context-graph-facts graph))
             (table "current-triple" (pai.context-graph::context-graph-current-triples graph))
             (table "episode" (pai.context-graph::context-graph-episodes graph))
             (table "correction" (pai.context-graph::context-graph-corrections graph))
             (table "entity-version" (pai.context-graph::context-graph-entity-versions graph))
             (table "current-entity-version" (pai.context-graph::context-graph-current-entity-versions graph))
             (table "revision-lineage" (pai.context-graph::context-graph-revision-lineage graph))
             (table "application-receipt" (pai.context-graph::context-graph-application-receipts graph))
             (table "adjacency" (pai.context-graph::context-graph-entity-adjacency graph))
             (maphash
              (lambda (id event)
                (one "owner-open" id
                     (if (gethash id terminals)
                         (%rcgs-compact-opening event) event)))
              (pai.context-graph::cgi-owner-opens owner))
             (table "owner-terminal" terminals)
             (maphash
              (lambda (key row)
                (if (gethash (first key) terminals)
                    (incf settled-exposure
                          (if (string= "request" (gethash "outcome" row ""))
                              (gethash "reserved_microusd" row 0)
                              (gethash "charged_microusd" row 0)))
                    (one "owner-phase" key row)))
              (pai.context-graph::cgi-owner-phases owner))
             (table "owner-task" (pai.context-graph::cgi-owner-tasks owner))
             (table "owner-application" (pai.context-graph::cgi-owner-applications owner))))
           (%with-sqlite-statement
               (statement handle
                 "DELETE FROM pai_reviewed_graph_records WHERE projection_name=?1 AND agent_id=?2 AND persona_id=?3 AND record_kind=?4 AND record_key=?5"
                 :reviewed-context-graph-persist)
             (maphash
              (lambda (identity ignored)
                (declare (ignore ignored))
                (%derived-reset-statement statement)
                (loop for value in (list projection agent-id persona-id
                                         (car identity) (cdr identity))
                      for index from 1
                      do (%sqlite-bind-text
                          handle statement index value
                          :reviewed-context-graph-persist))
                (%sqlite-step handle statement
                              :reviewed-context-graph-persist +sqlite-done+)
                (incf deleted-record-count))
              existing)))
         (let ((aliases (%rcgs-alias-index graph))
               (adjacency (%rcgs-adjacency-index graph)))
           (setf alias-count (hash-table-count aliases)
                 adjacency-count (hash-table-count adjacency))
           (multiple-value-setq (written-alias-count deleted-alias-count)
             (%rcgs-sync-index-pairs
              handle aliases projection agent-id persona-id
              "SELECT entity_id,alias_folded FROM pai_reviewed_graph_aliases WHERE projection_name=?1 AND agent_id=?2 AND persona_id=?3"
              "INSERT INTO pai_reviewed_graph_aliases(projection_name,agent_id,persona_id,entity_id,alias_folded) VALUES(?1,?2,?3,?4,?5)"
              "DELETE FROM pai_reviewed_graph_aliases WHERE projection_name=?1 AND agent_id=?2 AND persona_id=?3 AND entity_id=?4 AND alias_folded=?5"))
           (multiple-value-setq
               (written-adjacency-count deleted-adjacency-count)
             (%rcgs-sync-index-pairs
              handle adjacency projection agent-id persona-id
              "SELECT entity_id,fact_id FROM pai_reviewed_graph_adjacency WHERE projection_name=?1 AND agent_id=?2 AND persona_id=?3"
              "INSERT INTO pai_reviewed_graph_adjacency(projection_name,agent_id,persona_id,entity_id,fact_id) VALUES(?1,?2,?3,?4,?5)"
              "DELETE FROM pai_reviewed_graph_adjacency WHERE projection_name=?1 AND agent_id=?2 AND persona_id=?3 AND entity_id=?4 AND fact_id=?5")))
         (%rcgs-write-checkpoint
          handle agent-id through-event-id through-position
          (obj "schema_version" 1 "persona_id" persona-id
               "event_storage_id" event-storage-id
               "boundary_hash" boundary-hash
               "owner_protocol" (pai.context-graph::cgi-owner-protocol owner)
               "owner_revision" (pai.context-graph::cgi-owner-revision owner)
               "owner_last_event_id" (pai.context-graph::cgi-owner-last-id owner)
               "settled_exposure_microusd" settled-exposure
               "record_count" record-count
               "alias_count" alias-count
               "adjacency_count" adjacency-count)))))
    (obj "schema_version" 1 "status" "persisted"
         "record_count" record-count
         "written_record_count" written-record-count
         "deleted_record_count" deleted-record-count
         "alias_count" alias-count
         "written_alias_count" written-alias-count
         "deleted_alias_count" deleted-alias-count
         "adjacency_count" adjacency-count
         "written_adjacency_count" written-adjacency-count
         "deleted_adjacency_count" deleted-adjacency-count
         "through_event_id" through-event-id
         "through_storage_position" through-position)))

(defun %rcgs-decoded-key (key-kind key-json)
  (let ((key (%rcgs-read-json key-json)))
    (cond ((string= key-kind "list")
           (unless (vectorp key) (%rcgs-fail "list record key is invalid"))
           (coerce key 'list))
          ((string= key-kind "scalar") key)
          (t (%rcgs-fail "record key kind is invalid")))))

(defun %rcgs-target-table (graph owner kind)
  (cond
    ((string= kind "entity") (pai.context-graph::context-graph-entities graph))
    ((string= kind "entity-index") (pai.context-graph::context-graph-entity-index graph))
    ((string= kind "fact") (pai.context-graph::context-graph-facts graph))
    ((string= kind "current-triple") (pai.context-graph::context-graph-current-triples graph))
    ((string= kind "episode") (pai.context-graph::context-graph-episodes graph))
    ((string= kind "correction") (pai.context-graph::context-graph-corrections graph))
    ((string= kind "entity-version") (pai.context-graph::context-graph-entity-versions graph))
    ((string= kind "current-entity-version") (pai.context-graph::context-graph-current-entity-versions graph))
    ((string= kind "revision-lineage") (pai.context-graph::context-graph-revision-lineage graph))
    ((string= kind "application-receipt") (pai.context-graph::context-graph-application-receipts graph))
    ((string= kind "adjacency") (pai.context-graph::context-graph-entity-adjacency graph))
    ((string= kind "owner-open") (pai.context-graph::cgi-owner-opens owner))
    ((string= kind "owner-terminal") (pai.context-graph::cgi-owner-terminals owner))
    ((string= kind "owner-phase") (pai.context-graph::cgi-owner-phases owner))
    ((string= kind "owner-task") (pai.context-graph::cgi-owner-tasks owner))
    ((string= kind "owner-application") (pai.context-graph::cgi-owner-applications owner))
    (t nil)))

(defun reviewed-context-graph-restore
    (backend agent-id persona-id event-storage-id source-binding)
  "Restore rows verified by a small checkpoint; return NIL when none exists."
  (unless (typep backend 'sqlite-derived-storage)
    (%rcgs-fail "reviewed graph restore requires derived SQLite storage"))
  (let ((checkpoint
          (storage-load-checkpoint
           backend *reviewed-context-graph-projection-name*
           :agent-id agent-id)))
    (unless checkpoint (return-from reviewed-context-graph-restore nil))
    (let ((state (gethash "state" checkpoint)))
      (unless (and (hash-table-p state)
                   (= 1 (gethash "schema_version" state -1))
                   (string= persona-id (gethash "persona_id" state ""))
                   (string= event-storage-id
                            (gethash "event_storage_id" state ""))
                   (string= source-binding
                            (gethash "boundary_hash" state ""))
                   (string= *reviewed-context-graph-projector-revision*
                            (gethash "projector_revision" checkpoint ""))
                   (string= *reviewed-context-graph-storage-policy-revision*
                            (gethash "policy_revision" checkpoint ""))
                   (integerp (gethash "record_count" state))
                   (integerp (gethash "owner_last_event_id" state))
                   (integerp (gethash "settled_exposure_microusd" state)))
        (%rcgs-fail "reviewed graph checkpoint binding is invalid"))
      (let ((meta nil) (rows nil))
        (bt:with-lock-held ((%sqlite-derived-lock backend))
          (let ((handle (%sqlite-derived-handle
                         backend :reviewed-context-graph-restore)))
            (%with-sqlite-statement
                (statement handle
                  "SELECT record_kind,record_key,key_kind,payload_json,integrity_hash FROM pai_reviewed_graph_records WHERE projection_name=?1 AND agent_id=?2 AND persona_id=?3 ORDER BY CASE record_kind WHEN 'graph-meta' THEN 0 ELSE 1 END,record_kind,record_key"
                  :reviewed-context-graph-restore)
              (loop for value in (list *reviewed-context-graph-projection-name*
                                       agent-id persona-id)
                    for index from 1
                    do (%sqlite-bind-text handle statement index value
                                          :reviewed-context-graph-restore))
              (loop for code = (%sqlite-step-raw statement)
                    while (= code +sqlite-row+)
                    do (let* ((kind (%sqlite-column-text statement 0))
                              (key-json (%sqlite-column-text statement 1))
                              (key-kind (%sqlite-column-text statement 2))
                              (payload-json (%sqlite-column-text statement 3))
                              (integrity (%sqlite-column-text statement 4)))
                         (unless (string=
                                  integrity
                                  (%rcgs-row-integrity
                                   *reviewed-context-graph-projection-name*
                                   agent-id persona-id kind key-json key-kind
                                   payload-json))
                           (%rcgs-fail "reviewed graph row integrity mismatch"))
                         (let ((row (list kind
                                          (%rcgs-decoded-key key-kind key-json)
                                          (%rcgs-read-json payload-json))))
                           (if (string= kind "graph-meta")
                               (setf meta (third row))
                               (push row rows))))
                    finally (unless (= code +sqlite-done+)
                              (%sqlite-check code handle
                                             :reviewed-context-graph-restore))))))
        (unless (and (hash-table-p meta)
                     (= (1- (gethash "record_count" state)) (length rows)))
          (%rcgs-fail "reviewed graph row count is inconsistent"))
        (let* ((graph
                 (pai.context-graph::%make-context-graph
                  :ontology (gethash "ontology" meta)))
               (owner
                 (pai.context-graph::%cgi-owner-create
                  graph agent-id persona-id (gethash "owner_protocol" state))))
          (setf (pai.context-graph::cgi-owner-revision owner)
                (gethash "owner_revision" state)
                (pai.context-graph::cgi-owner-last-id owner)
                (gethash "owner_last_event_id" state)
                (pai.context-graph::context-graph-authority-profile graph)
                (let ((value (gethash "authority_profile" meta)))
                  (unless (eq value :null) value))
                (pai.context-graph::context-graph-authority-partition graph)
                (let ((value (gethash "authority_partition" meta)))
                  (unless (eq value :null) value))
                (pai.context-graph::context-graph-through-event-id graph)
                (gethash "through_event_id" meta)
                (pai.context-graph::context-graph-projection-digest graph)
                (let ((value (gethash "projection_digest" meta)))
                  (unless (eq value :null) value)))
          (dolist (row rows)
            (let ((table (%rcgs-target-table graph owner (first row))))
              (unless table (%rcgs-fail "reviewed graph record kind is unknown"))
              (setf (gethash (second row) table) (third row))))
          (setf (pai.context-graph::context-graph-entity-scan-index graph)
                (coerce
                 (sort (loop for id being the hash-keys of
                               (pai.context-graph::context-graph-entities graph)
                             collect id)
                       #'string<)
                 'vector)
                (pai.context-graph::context-graph-fact-scan-index graph)
                (coerce
                 (sort (loop for id being the hash-keys of
                               (pai.context-graph::context-graph-facts graph)
                             collect id)
                       #'string<)
                 'vector))
          (values graph owner checkpoint
                  (gethash "settled_exposure_microusd" state)))))))
