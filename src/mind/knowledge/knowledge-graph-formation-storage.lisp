;;;; knowledge-graph-formation-storage.lisp -- KG2 derived persistence owner.
;;;;
;;;; The event ledger remains authority.  This owner atomically persists and
;;;; independently verifies the rebuildable generic knowledge-graph partition.
;;;; Loading is inert; writes require an explicit derived backend and boundary.

(in-package :agent)

(export '(knowledge-graph-formation-persist
          knowledge-graph-formation-persist-tail
          knowledge-graph-formation-restore
          knowledge-graph-formation-inspect
          knowledge-graph-formation-current-candidates))

(defparameter *knowledge-graph-formation-storage-policy-revision*
  "generic-knowledge-graph-storage-v1")

(defun %kgfs-fail (detail)
  (error 'storage-integrity-error :operation :knowledge-graph-formation
         :detail detail))

(defun %kgfs-canonical-materialization (materialization)
  (unless (and (hash-table-p materialization)
               (= 1 (gethash "schema_version" materialization -1))
               (string= *knowledge-graph-formation-projection-name*
                        (gethash "projection_name" materialization ""))
               (string= *knowledge-graph-formation-revision*
                        (gethash "projection_revision" materialization "")))
    (%kgfs-fail "KG2 materialization contract is invalid"))
  (let* ((projection (gethash "projection_name" materialization))
         (agent-id (%cegs-required-string
                    (gethash "agent_id" materialization) "agent-id"))
         (persona-id (%cegs-required-string
                      (gethash "persona_id" materialization) "persona-id"))
         (node-ids (make-hash-table :test #'equal))
         (edge-ids (make-hash-table :test #'equal)))
    (dolist (row (%cegs-items (gethash "nodes" materialization)))
      (let* ((node-id (gethash "node_id" row))
             (kind (gethash "node_kind" row))
             (key (gethash "canonical_key" row))
             (json (gethash "payload_json" row))
             (expected (%kgf-integrity projection agent-id persona-id
                                       node-id kind key json)))
        (unless (and (string= projection (gethash "projection_name" row ""))
                     (string= agent-id (gethash "agent_id" row ""))
                     (string= persona-id (gethash "persona_id" row ""))
                     (stringp node-id) (stringp kind) (stringp key)
                     (stringp json) (string= key node-id)
                     (uiop:string-prefix-p "kgf:entity:" node-id)
                     (string= expected (gethash "integrity_hash" row ""))
                     (not (gethash node-id node-ids)))
          (%kgfs-fail "KG2 node row is invalid"))
        (let ((payload (%storage-json-read json :knowledge-graph-formation)))
          (unless (and (hash-table-p payload)
                       (string= node-id (gethash "node_id" payload ""))
                       (string= kind (gethash "node_kind" payload ""))
                       (string= persona-id (gethash "persona_id" payload "")))
            (%kgfs-fail "KG2 node payload is invalid")))
        (setf (gethash node-id node-ids) t)))
    (dolist (row (%cegs-items (gethash "edges" materialization)))
      (let* ((edge-id (gethash "edge_id" row))
             (from (gethash "from_node_id" row))
             (to (gethash "to_node_id" row))
             (predicate (gethash "predicate" row))
             (json (gethash "payload_json" row))
             (expected (%kgf-integrity projection agent-id persona-id
                                       edge-id from predicate to json)))
        (unless (and (string= projection (gethash "projection_name" row ""))
                     (string= agent-id (gethash "agent_id" row ""))
                     (string= persona-id (gethash "persona_id" row ""))
                     (stringp edge-id) (stringp from) (stringp to)
                     (stringp predicate) (plusp (length predicate))
                     (stringp json) (gethash from node-ids)
                     (gethash to node-ids)
                     (string= expected (gethash "integrity_hash" row ""))
                     (not (gethash edge-id edge-ids)))
          (%kgfs-fail "KG2 edge row is invalid"))
        (let ((payload (%storage-json-read json :knowledge-graph-formation)))
          (unless (and (hash-table-p payload)
                       (string= edge-id (gethash "edge_id" payload ""))
                       (string= from (gethash "from_node_id" payload ""))
                       (string= to (gethash "to_node_id" payload ""))
                       (string= predicate (gethash "predicate" payload ""))
                       (string= persona-id (gethash "persona_id" payload "")))
            (%kgfs-fail "KG2 edge payload is invalid")))
        (setf (gethash edge-id edge-ids) t)))
    (dolist (row (%cegs-items (gethash "evidence" materialization)))
      (let ((owner-kind (gethash "owner_kind" row ""))
            (owner-id (gethash "owner_id" row)))
        (unless (and (string= projection (gethash "projection_name" row ""))
                     (string= agent-id (gethash "agent_id" row ""))
                     (string= persona-id (gethash "persona_id" row ""))
                     (member owner-kind '("node" "edge") :test #'string=)
                     (stringp owner-id)
                     (if (string= owner-kind "node")
                         (gethash owner-id node-ids)
                         (gethash owner-id edge-ids))
                     (integerp (gethash "evidence_event_id" row))
                     (member (gethash "evidence_role" row "")
                             '("descriptor" "source") :test #'string=)
                     (integerp (gethash "evidence_ordinal" row)))
          (%kgfs-fail "KG2 evidence row is invalid"))))
    materialization))

(defun %kgfs-write-checkpoint
    (handle projection agent-id through-event-id through-position state)
  (let* ((state-json (%storage-json state))
         (projector (gethash "projection_revision" state))
         (policy *knowledge-graph-formation-storage-policy-revision*)
         (integrity
           (%storage-sha256
            (%storage-checkpoint-integrity-input
             projection agent-id through-event-id through-position
             projector policy state-json))))
    (%with-sqlite-statement
        (statement handle
                   "INSERT INTO pai_projection_checkpoints(projection_name,agent_id,through_event_id,through_storage_position,projector_revision,policy_revision,state_json,integrity_hash) VALUES(?1,?2,?3,?4,?5,?6,?7,?8) ON CONFLICT(projection_name,agent_id) DO UPDATE SET through_event_id=excluded.through_event_id,through_storage_position=excluded.through_storage_position,projector_revision=excluded.projector_revision,policy_revision=excluded.policy_revision,state_json=excluded.state_json,integrity_hash=excluded.integrity_hash,created_at=CURRENT_TIMESTAMP"
                   :knowledge-graph-formation-persist)
      (%sqlite-bind-text handle statement 1 projection
                         :knowledge-graph-formation-persist)
      (%sqlite-bind-text handle statement 2 agent-id
                         :knowledge-graph-formation-persist)
      (%sqlite-bind-int64 handle statement 3 through-event-id
                          :knowledge-graph-formation-persist)
      (%sqlite-bind-int64 handle statement 4 through-position
                          :knowledge-graph-formation-persist)
      (%sqlite-bind-text handle statement 5 projector
                         :knowledge-graph-formation-persist)
      (%sqlite-bind-text handle statement 6 policy
                         :knowledge-graph-formation-persist)
      (%sqlite-bind-text handle statement 7 state-json
                         :knowledge-graph-formation-persist)
      (%sqlite-bind-text handle statement 8 integrity
                         :knowledge-graph-formation-persist)
      (%sqlite-step handle statement :knowledge-graph-formation-persist
                    +sqlite-done+))))

(defun knowledge-graph-formation-persist
    (backend materialization &key through-event-id through-position
       event-storage-id boundary-hash)
  "Atomically replace one rebuildable KG2 partition and its checkpoint."
  (unless (typep backend 'sqlite-derived-storage)
    (error 'storage-error :operation :knowledge-graph-formation-persist
           :detail "KG2 persistence requires derived SQLite storage"))
  (%storage-positive-integer through-event-id "through-event-id"
                             :zero-allowed t)
  (%storage-positive-integer through-position "through-position"
                             :zero-allowed t)
  (%cegs-required-string event-storage-id "event-storage-id")
  (%cegs-required-string boundary-hash "boundary-hash")
  (let* ((canonical (%kgfs-canonical-materialization materialization))
         (projection (gethash "projection_name" canonical))
         (agent-id (gethash "agent_id" canonical))
         (persona-id (gethash "persona_id" canonical))
         (nodes (gethash "nodes" canonical))
         (edges (gethash "edges" canonical))
         (evidence (gethash "evidence" canonical)))
    (multiple-value-bind (digest xor sum node-count edge-count evidence-count)
        (%cegs-graph-integrity-state canonical)
      (let ((state
              (obj "schema_version" 1 "persona_id" persona-id
                   "event_storage_id" event-storage-id
                   "boundary_hash" boundary-hash
                   "projection_revision" *knowledge-graph-formation-revision*
                   "node_count" node-count "edge_count" edge-count
                   "evidence_count" evidence-count
                   "row_xor" (%cegs-accumulator-hex xor)
                   "row_sum" (%cegs-accumulator-hex sum)
                   "graph_digest" digest)))
        (bt:with-lock-held ((%sqlite-derived-lock backend))
          (%sqlite-derived-in-transaction
           backend :knowledge-graph-formation-persist
           (lambda (handle)
             (multiple-value-bind (current-event current-position)
                 (%cegs-current-watermark handle projection agent-id)
               (when (or (and current-event (< through-event-id current-event))
                         (and current-position (< through-position current-position)))
                 (error 'storage-conflict-error
                        :operation :knowledge-graph-formation-persist
                        :detail "KG2 checkpoint would move backwards")))
             (%cegs-delete-partition handle projection agent-id persona-id)
             (%cegs-insert-nodes handle nodes)
             (%cegs-insert-edges handle edges)
             (%cegs-insert-evidence handle evidence)
             (%kgfs-write-checkpoint handle projection agent-id
                                     through-event-id through-position state))))
        (obj "schema_version" 1 "status" "persisted"
             "node_count" node-count "edge_count" edge-count
             "evidence_count" evidence-count "graph_digest" digest
             "through_storage_position" through-position
             "event_write_count" 0 "memory_write_count" 0)))))

(defun %kgfs-checkpoint (backend agent-id persona-id event-storage-id)
  (let* ((checkpoint
           (storage-load-checkpoint
            backend *knowledge-graph-formation-projection-name*
            :agent-id agent-id))
         (state (and checkpoint (gethash "state" checkpoint))))
    (unless (and checkpoint (hash-table-p state)
                 (= 1 (gethash "schema_version" state -1))
                 (string= persona-id (gethash "persona_id" state ""))
                 (string= event-storage-id
                          (gethash "event_storage_id" state ""))
                 (string= *knowledge-graph-formation-revision*
                          (gethash "projection_revision" state ""))
                 (string= *knowledge-graph-formation-storage-policy-revision*
                          (gethash "policy_revision" checkpoint ""))
                 (every #'integerp
                        (mapcar (lambda (key) (gethash key state))
                                '("node_count" "edge_count"
                                  "evidence_count")))
                 (every (lambda (key) (%cegs-hex256-p (gethash key state)))
                        '("row_xor" "row_sum" "graph_digest")))
      (%kgfs-fail "KG2 checkpoint binding is invalid"))
    (values checkpoint state)))

(defun %kgfs-read-materialization (backend agent-id persona-id checkpoint)
  (let* ((state (gethash "state" checkpoint))
         (projection *knowledge-graph-formation-projection-name*)
         (nodes nil) (edges nil) (evidence nil))
    (bt:with-lock-held ((%sqlite-derived-lock backend))
      (let ((handle (%sqlite-derived-handle backend
                                            :knowledge-graph-formation-restore)))
        (labels ((partitioned (select order)
                   (format nil
                           "~a WHERE projection_name=?1 AND agent_id=?2 AND persona_id=?3 ORDER BY ~a"
                           select order)))
          (setf nodes
                (%cegs-read-rows
                 handle
                 (partitioned
                  "SELECT projection_name,agent_id,persona_id,node_id,node_kind,canonical_key,payload_json,integrity_hash FROM pai_knowledge_graph_nodes"
                  "node_id")
                 '("projection_name" "agent_id" "persona_id" "node_id"
                   "node_kind" "canonical_key" "payload_json" "integrity_hash")
                 nil :knowledge-graph-formation-restore
                 projection agent-id persona-id)
                edges
                (%cegs-read-rows
                 handle
                 (partitioned
                  "SELECT projection_name,agent_id,persona_id,edge_id,from_node_id,predicate,to_node_id,payload_json,integrity_hash FROM pai_knowledge_graph_edges"
                  "edge_id")
                 '("projection_name" "agent_id" "persona_id" "edge_id"
                   "from_node_id" "predicate" "to_node_id" "payload_json"
                   "integrity_hash")
                 nil :knowledge-graph-formation-restore
                 projection agent-id persona-id)
                evidence
                (%cegs-read-rows
                 handle
                 (partitioned
                  "SELECT projection_name,agent_id,persona_id,owner_kind,owner_id,evidence_event_id,evidence_role,evidence_ordinal FROM pai_knowledge_graph_evidence"
                  "owner_kind,owner_id,evidence_event_id,evidence_role,evidence_ordinal")
                 '("projection_name" "agent_id" "persona_id" "owner_kind"
                   "owner_id" "evidence_event_id" "evidence_role"
                   "evidence_ordinal")
                 '("evidence_event_id" "evidence_ordinal")
                 :knowledge-graph-formation-restore
                 projection agent-id persona-id)))))
    (obj "schema_version" 1 "projection_name" projection
         "projection_revision" (gethash "projection_revision" state)
         "agent_id" agent-id "persona_id" persona-id
         "nodes" nodes "edges" edges "evidence" evidence)))

(defun %kgfs-node-row (handle projection agent-id persona-id node-id)
  (let ((rows
          (%cegs-read-rows
           handle
           "SELECT projection_name,agent_id,persona_id,node_id,node_kind,canonical_key,payload_json,integrity_hash FROM pai_knowledge_graph_nodes WHERE projection_name=?1 AND agent_id=?2 AND persona_id=?3 AND node_id=?4"
           '("projection_name" "agent_id" "persona_id" "node_id"
             "node_kind" "canonical_key" "payload_json" "integrity_hash")
           nil :knowledge-graph-formation-tail
           projection agent-id persona-id node-id)))
    (when (plusp (length rows)) (aref rows 0))))

(defun %kgfs-edge-row (handle projection agent-id persona-id edge-id)
  (let ((rows
          (%cegs-read-rows
           handle
           "SELECT projection_name,agent_id,persona_id,edge_id,from_node_id,predicate,to_node_id,payload_json,integrity_hash FROM pai_knowledge_graph_edges WHERE projection_name=?1 AND agent_id=?2 AND persona_id=?3 AND edge_id=?4"
           '("projection_name" "agent_id" "persona_id" "edge_id"
             "from_node_id" "predicate" "to_node_id" "payload_json"
             "integrity_hash")
           nil :knowledge-graph-formation-tail
           projection agent-id persona-id edge-id)))
    (when (plusp (length rows)) (aref rows 0))))

(defun %kgfs-upsert-node (handle row)
  (%with-sqlite-statement
      (statement handle
                 "INSERT INTO pai_knowledge_graph_nodes(projection_name,agent_id,persona_id,node_id,node_kind,canonical_key,payload_json,integrity_hash) VALUES(?1,?2,?3,?4,?5,?6,?7,?8) ON CONFLICT(projection_name,agent_id,persona_id,node_id) DO UPDATE SET node_kind=excluded.node_kind,canonical_key=excluded.canonical_key,payload_json=excluded.payload_json,integrity_hash=excluded.integrity_hash"
                 :knowledge-graph-formation-tail)
    (loop for key in '("projection_name" "agent_id" "persona_id" "node_id"
                       "node_kind" "canonical_key" "payload_json"
                       "integrity_hash")
          for index from 1
          do (%sqlite-bind-text handle statement index (gethash key row)
                                :knowledge-graph-formation-tail))
    (%sqlite-step handle statement :knowledge-graph-formation-tail
                  +sqlite-done+)))

(defun %kgfs-upsert-edge (handle row)
  (%with-sqlite-statement
      (statement handle
                 "INSERT INTO pai_knowledge_graph_edges(projection_name,agent_id,persona_id,edge_id,from_node_id,predicate,to_node_id,payload_json,integrity_hash) VALUES(?1,?2,?3,?4,?5,?6,?7,?8,?9) ON CONFLICT(projection_name,agent_id,persona_id,edge_id) DO UPDATE SET from_node_id=excluded.from_node_id,predicate=excluded.predicate,to_node_id=excluded.to_node_id,payload_json=excluded.payload_json,integrity_hash=excluded.integrity_hash"
                 :knowledge-graph-formation-tail)
    (loop for key in '("projection_name" "agent_id" "persona_id" "edge_id"
                       "from_node_id" "predicate" "to_node_id"
                       "payload_json" "integrity_hash")
          for index from 1
          do (%sqlite-bind-text handle statement index (gethash key row)
                                :knowledge-graph-formation-tail))
    (%sqlite-step handle statement :knowledge-graph-formation-tail
                  +sqlite-done+)))

(defun %kgfs-select-owners-evidence (rows node-ids edge-ids)
  (remove-if-not
   (lambda (row)
     (if (string= "node" (gethash "owner_kind" row ""))
         (find (gethash "owner_id" row) node-ids :test #'string=)
         (find (gethash "owner_id" row) edge-ids :test #'string=)))
   (%cegs-items rows)))

(defun knowledge-graph-formation-persist-tail
    (backend materialization changed-node-ids changed-edge-ids
     &key through-event-id through-position event-storage-id boundary-hash)
  "Atomically apply one bounded KG2 row delta and advance its signed checkpoint.

MATERIALIZATION may contain unchanged endpoint rows needed to validate changed
edges. Only explicitly changed IDs are read or written; the lifetime generation
is neither read nor rewritten."
  (%storage-positive-integer through-event-id "through-event-id"
                             :zero-allowed t)
  (%storage-positive-integer through-position "through-position"
                             :zero-allowed t)
  (%cegs-required-string event-storage-id "event-storage-id")
  (%cegs-required-string boundary-hash "boundary-hash")
  (let* ((canonical (%kgfs-canonical-materialization materialization))
         (projection (gethash "projection_name" canonical))
         (revision (gethash "projection_revision" canonical))
         (agent-id (gethash "agent_id" canonical))
         (persona-id (gethash "persona_id" canonical))
         (node-ids (remove-duplicates (%cegs-items changed-node-ids)
                                      :test #'string=))
         (edge-ids (remove-duplicates (%cegs-items changed-edge-ids)
                                      :test #'string=))
         (nodes (remove-if-not
                 (lambda (row) (find (gethash "node_id" row) node-ids
                                     :test #'string=))
                 (%cegs-items (gethash "nodes" canonical))))
         (edges (remove-if-not
                 (lambda (row) (find (gethash "edge_id" row) edge-ids
                                     :test #'string=))
                 (%cegs-items (gethash "edges" canonical))))
         (evidence (%kgfs-select-owners-evidence
                    (gethash "evidence" canonical) node-ids edge-ids)))
    (unless (and (= (length nodes) (length node-ids))
                 (= (length edges) (length edge-ids)))
      (%kgfs-fail "KG2 tail changed IDs are absent from materialization"))
    (multiple-value-bind (checkpoint prior)
        (%kgfs-checkpoint backend agent-id persona-id event-storage-id)
      (let ((xor (parse-integer (gethash "row_xor" prior) :radix 16))
            (sum (parse-integer (gethash "row_sum" prior) :radix 16))
            (node-count (gethash "node_count" prior))
            (edge-count (gethash "edge_count" prior))
            (evidence-count (gethash "evidence_count" prior))
            (updated 0) (inserted 0))
        (labels ((fold-row (collection row remove-p)
                   (multiple-value-setq (xor sum)
                     (%cegs-accumulate-row xor sum collection row remove-p))))
          (bt:with-lock-held ((%sqlite-derived-lock backend))
            (%sqlite-derived-in-transaction
             backend :knowledge-graph-formation-tail
             (lambda (handle)
               (multiple-value-bind (current-event current-position)
                   (%cegs-current-watermark handle projection agent-id)
                 (unless (and current-position
                              (= current-event
                                 (gethash "through_event_id" checkpoint))
                              (= current-position
                                 (gethash "through_storage_position" checkpoint)))
                   (error 'storage-conflict-error
                          :operation :knowledge-graph-formation-tail
                          :detail "KG2 checkpoint changed during tail fold"))
                 (when (or (< through-event-id current-event)
                           (< through-position current-position))
                   (error 'storage-conflict-error
                          :operation :knowledge-graph-formation-tail
                          :detail "KG2 checkpoint would move backwards")))
               (dolist (row nodes)
                 (let ((old (%kgfs-node-row
                             handle projection agent-id persona-id
                             (gethash "node_id" row))))
                   (if old
                       (progn
                         (fold-row "nodes" old t) (incf updated)
                         (let ((old-evidence
                                 (%cegs-owner-evidence-rows
                                  handle projection agent-id persona-id "node"
                                  (gethash "node_id" row)))
                               (owned (%kgfs-select-owners-evidence
                                       evidence
                                       (list (gethash "node_id" row)) nil)))
                           (dolist (e (%cegs-items old-evidence))
                             (fold-row "evidence" e t)
                             (decf evidence-count))
                           (%cegs-delete-owner-evidence
                            handle projection agent-id persona-id "node"
                            (gethash "node_id" row))
                           (%cegs-insert-evidence handle owned)
                           (dolist (e owned)
                             (fold-row "evidence" e nil)
                             (incf evidence-count))))
                       (progn
                         (incf node-count) (incf inserted)
                         (let ((owned (%kgfs-select-owners-evidence
                                       evidence
                                       (list (gethash "node_id" row)) nil)))
                           (%cegs-insert-evidence handle owned)
                           (dolist (e owned)
                             (fold-row "evidence" e nil)
                             (incf evidence-count)))))
                   (%kgfs-upsert-node handle row)
                   (fold-row "nodes" row nil)))
               (dolist (row edges)
                 (let ((old (%kgfs-edge-row
                             handle projection agent-id persona-id
                             (gethash "edge_id" row))))
                   (if old
                       (progn
                         (fold-row "edges" old t) (incf updated)
                         (let ((old-evidence
                                 (%cegs-owner-evidence-rows
                                  handle projection agent-id persona-id "edge"
                                  (gethash "edge_id" row)))
                               (owned (%kgfs-select-owners-evidence
                                       evidence nil
                                       (list (gethash "edge_id" row)))))
                           (dolist (e (%cegs-items old-evidence))
                             (fold-row "evidence" e t)
                             (decf evidence-count))
                           (%cegs-delete-owner-evidence
                            handle projection agent-id persona-id "edge"
                            (gethash "edge_id" row))
                           (%cegs-insert-evidence handle owned)
                           (dolist (e owned)
                             (fold-row "evidence" e nil)
                             (incf evidence-count))))
                       (progn
                         (incf edge-count) (incf inserted)
                         (let ((owned (%kgfs-select-owners-evidence
                                       evidence nil
                                       (list (gethash "edge_id" row)))))
                           (%cegs-insert-evidence handle owned)
                           (dolist (e owned)
                             (fold-row "evidence" e nil)
                             (incf evidence-count)))))
                   (%kgfs-upsert-edge handle row)
                   (fold-row "edges" row nil)))
               (let* ((digest
                        (%cegs-digest-from-components
                         projection revision agent-id persona-id
                         node-count edge-count evidence-count xor sum))
                      (state
                        (obj "schema_version" 1 "persona_id" persona-id
                             "event_storage_id" event-storage-id
                             "boundary_hash" boundary-hash
                             "projection_revision" revision
                             "node_count" node-count "edge_count" edge-count
                             "evidence_count" evidence-count
                             "row_xor" (%cegs-accumulator-hex xor)
                             "row_sum" (%cegs-accumulator-hex sum)
                             "graph_digest" digest)))
                 (%kgfs-write-checkpoint
                  handle projection agent-id through-event-id through-position
                  state)))))
          (obj "schema_version" 1 "status" "tail-persisted"
               "changed_node_count" (length nodes)
               "changed_edge_count" (length edges)
               "updated_row_count" updated "inserted_row_count" inserted
               "node_count" node-count "edge_count" edge-count
               "evidence_count" evidence-count
               "graph_digest"
               (%cegs-digest-from-components
                projection revision agent-id persona-id
                node-count edge-count evidence-count xor sum)
               "full_generation_rows_read" 0
               "through_storage_position" through-position
               "event_write_count" 0 "memory_write_count" 0))))))

(defun knowledge-graph-formation-restore
    (backend agent-id persona-id &key event-storage-id)
  "Restore and independently verify one persisted KG2 partition."
  (%cegs-required-string agent-id "agent-id")
  (%cegs-required-string persona-id "persona-id")
  (%cegs-required-string event-storage-id "event-storage-id")
  (multiple-value-bind (checkpoint state)
      (%kgfs-checkpoint backend agent-id persona-id event-storage-id)
    (let* ((stored (%kgfs-read-materialization
                    backend agent-id persona-id checkpoint))
           (canonical (%kgfs-canonical-materialization stored)))
      (multiple-value-bind (digest xor sum node-count edge-count evidence-count)
          (%cegs-graph-integrity-state canonical)
        (unless (and (= node-count (gethash "node_count" state -1))
                     (= edge-count (gethash "edge_count" state -1))
                     (= evidence-count (gethash "evidence_count" state -1))
                     (string= (%cegs-accumulator-hex xor)
                              (gethash "row_xor" state ""))
                     (string= (%cegs-accumulator-hex sum)
                              (gethash "row_sum" state ""))
                     (string= digest (gethash "graph_digest" state "")))
          (%kgfs-fail "KG2 checkpoint digest or counts do not match"))
        (values canonical
                (obj "schema_version" 1 "status" "restored"
                     "node_count" node-count "edge_count" edge-count
                     "evidence_count" evidence-count "graph_digest" digest
                     "event_storage_id" (gethash "event_storage_id" state)
                     "boundary_hash" (gethash "boundary_hash" state)
                     "through_event_id" (gethash "through_event_id" checkpoint)
                     "through_storage_position"
                     (gethash "through_storage_position" checkpoint)
                     "database_write_count" 0))))))

(defun knowledge-graph-formation-inspect
    (backend agent-id persona-id &key event-storage-id)
  "Return content-free verified KG2 health without durable writes."
  (multiple-value-bind (materialization report)
      (knowledge-graph-formation-restore
       backend agent-id persona-id :event-storage-id event-storage-id)
    (declare (ignore materialization))
    report))

(defun %kgfs-like-pattern (text)
  (with-output-to-string (stream)
    (write-char #\% stream)
    (loop for character across (string-downcase text)
          do (when (find character "\\%_" :test #'char=)
               (write-char #\\ stream))
             (write-char character stream))
    (write-char #\% stream)))

(defun knowledge-graph-formation-current-candidates
    (backend agent-id persona-id cues
     &key event-storage-id (maximum 64))
  "Return bounded current KG2 descriptors whose payload matches lexical CUES.

This is candidate generation, never merge authority. The provider may only
choose among these verified IDs and the pure projector independently confirms
that a chosen node is current when replay applies the sealed receipt."
  (unless (and (typep backend 'sqlite-derived-storage)
               (%kgf-required-string-p agent-id 180)
               (%kgf-required-string-p persona-id 120)
               (%kgf-required-string-p event-storage-id 240)
               (vectorp cues) (<= (length cues) 24)
               (every (lambda (cue) (%kgf-required-string-p cue 240)) cues)
               (integerp maximum) (<= 1 maximum 64))
    (error 'storage-error :operation :knowledge-graph-formation-candidates
           :detail "KG2 candidate query inputs are invalid"))
  ;; Refuse candidate IDs from an unbound or stale generation.
  (%kgfs-checkpoint backend agent-id persona-id event-storage-id)
  (let* ((participant-score
           "CASE WHEN EXISTS (SELECT 1 FROM json_each(payload_json,'$.classifications') WHERE lower(value) IN ('operator','active-persona')) THEN 128 ELSE 0 END")
         (score-parts
           (loop for index from 4 below (+ 4 (length cues))
                 append
                 (list
                  (format nil
                          "CASE WHEN lower(COALESCE(json_extract(payload_json,'$.label'),''))=lower(?~d) THEN 32 WHEN instr(lower(COALESCE(json_extract(payload_json,'$.label'),'')),lower(?~d))>0 THEN 16 ELSE 0 END"
                          index index)
                  (format nil
                          "CASE WHEN instr(lower(COALESCE(json_extract(payload_json,'$.aliases'),'')),lower(?~d))>0 THEN 12 ELSE 0 END"
                          index)
                  (format nil
                          "CASE WHEN instr(lower(COALESCE(json_extract(payload_json,'$.classifications'),'')),lower(?~d))>0 THEN 8 ELSE 0 END"
                          index))))
         (lexical-score (if score-parts
                            (format nil "~{~a~^ + ~}" score-parts)
                            "0"))
         (score (format nil "(~a + ~a)" participant-score lexical-score))
         (sql
           (format nil
                   "SELECT projection_name,agent_id,persona_id,node_id,node_kind,canonical_key,payload_json,integrity_hash FROM pai_knowledge_graph_nodes WHERE projection_name=?1 AND agent_id=?2 AND persona_id=?3 AND ~a>0 ORDER BY ~a DESC,node_id LIMIT 256"
                   score score))
         (rows nil))
    (bt:with-lock-held ((%sqlite-derived-lock backend))
      (let ((handle (%sqlite-derived-handle
                     backend :knowledge-graph-formation-candidates)))
        (setf rows
              (apply #'%cegs-read-rows handle sql
                     '("projection_name" "agent_id" "persona_id" "node_id"
                       "node_kind" "canonical_key" "payload_json"
                       "integrity_hash")
                      nil :knowledge-graph-formation-candidates
                      *knowledge-graph-formation-projection-name*
                      agent-id persona-id
                      (coerce cues 'list)))))
    (let ((candidates nil))
      (loop for row across rows
            while (< (length candidates) maximum)
            for payload = (%storage-json-read
                           (gethash "payload_json" row "")
                           :knowledge-graph-formation-candidates)
            do
               ;; Validate every returned row against its canonical integrity
               ;; contract before exposing the ID outside storage.
               (%kgfs-canonical-materialization
                (obj "schema_version" 1
                     "projection_name"
                     *knowledge-graph-formation-projection-name*
                     "projection_revision" *knowledge-graph-formation-revision*
                     "agent_id" agent-id "persona_id" persona-id
                     "nodes" (vector row) "edges" #() "evidence" #()))
               (when (and (hash-table-p payload)
                          (string= "current" (gethash "status" payload "")))
                 (push
                  (obj "node_id" (gethash "node_id" payload)
                       "kind" (gethash "node_kind" payload)
                       "label" (gethash "label" payload)
                       "aliases" (copy-seq (gethash "aliases" payload))
                       "classifications"
                       (copy-seq (gethash "classifications" payload #()))
                       "participant_role"
                       (or (gethash "participant_role" payload) :null))
                  candidates)))
      (coerce (nreverse candidates) 'vector))))
