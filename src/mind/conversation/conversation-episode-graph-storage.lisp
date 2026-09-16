;;;; conversation-episode-graph-storage.lisp -- KG1 persisted episode graph.
;;;;
;;;; The ledger remains authority.  This owner stores and verifies one
;;;; disposable episode/concept graph partition in derived SQLite.  Loading the
;;;; file is inert; every write requires an explicit backend and boundary.

(in-package :agent)

(export '(conversation-episode-graph-persist
          conversation-episode-graph-persist-tail
          conversation-episode-graph-restore
          conversation-episode-graph-inspect))

(defparameter *conversation-episode-graph-storage-policy-revision*
  "conversation-episode-graph-storage-v2")

(defconstant +cegs-accumulator-modulus+ (ash 1 256))

(defun %cegs-required-string (value name)
  (unless (and (stringp value) (plusp (length value)))
    (error 'storage-error :operation :conversation-episode-graph
           :detail (format nil "~a must be a non-empty string" name)))
  value)

(defun %cegs-items (value)
  (cond ((null value) nil)
        ((vectorp value) (coerce value 'list))
        ((listp value) value)
        (t (error 'storage-integrity-error
                  :operation :conversation-episode-graph
                  :detail "graph row collection is not a sequence"))))

(defun %cegs-episodes (materialization)
  (let ((episodes nil))
    (dolist (row (%cegs-items (gethash "nodes" materialization)))
      (when (string= "episode" (gethash "node_kind" row ""))
        (let ((payload
                (%storage-json-read
                 (gethash "payload_json" row "")
                 :conversation-episode-graph)))
          (unless (hash-table-p payload)
            (error 'storage-integrity-error
                   :operation :conversation-episode-graph
                   :detail "episode graph payload is not an object"))
          (push payload episodes))))
    (coerce (sort episodes #'<
                  :key (lambda (episode)
                         (gethash "last_event_id" episode 0)))
            'vector)))

(defun %cegs-canonical-materialization (materialization)
  (unless (and (hash-table-p materialization)
               (= 1 (gethash "schema_version" materialization -1))
               (string=
                *conversation-episode-graph-storage-projection-name*
                (gethash "projection_name" materialization ""))
               (string= *conversation-episode-projection-revision*
                        (gethash "projection_revision" materialization "")))
    (error 'storage-integrity-error
           :operation :conversation-episode-graph
           :detail "graph materialization contract is invalid"))
  (let* ((projection (gethash "projection_name" materialization))
         (agent-id (%cegs-required-string
                    (gethash "agent_id" materialization) "agent-id"))
         (persona-id (%cegs-required-string
                      (gethash "persona_id" materialization) "persona-id"))
         (node-ids (make-hash-table :test #'equal))
         (edge-ids (make-hash-table :test #'equal)))
    ;; Validate each bounded row independently.  Do not reconstruct and JSON
    ;; serialize a second whole graph: that made integrity checking require
    ;; several times the resident graph size at realistic lifetime depth.
    (dolist (row (%cegs-items (gethash "nodes" materialization)))
      (let* ((node-id (gethash "node_id" row))
             (kind (gethash "node_kind" row ""))
             (key (gethash "canonical_key" row))
             (payload-json (gethash "payload_json" row))
             (expected
               (%conversation-episode-graph-integrity
                projection agent-id persona-id node-id kind key payload-json)))
        (unless (and (string= projection (gethash "projection_name" row ""))
                     (string= agent-id (gethash "agent_id" row ""))
                     (string= persona-id (gethash "persona_id" row ""))
                     (member kind '("episode" "concept") :test #'string=)
                     (stringp node-id) (stringp key) (stringp payload-json)
                     (string= node-id
                              (%conversation-episode-graph-id kind key))
                     (string= expected (gethash "integrity_hash" row ""))
                     (not (gethash node-id node-ids)))
          (error 'storage-integrity-error
                 :operation :conversation-episode-graph
                 :detail "graph node row is invalid"))
        (let ((payload (%storage-json-read payload-json
                                           :conversation-episode-graph)))
          (unless (and (hash-table-p payload)
                       (if (string= kind "episode")
                           (and (string= key (gethash "episode_id" payload ""))
                                (string= persona-id
                                         (gethash "persona_id" payload "")))
                           (string= key (gethash "label" payload ""))))
            (error 'storage-integrity-error
                   :operation :conversation-episode-graph
                   :detail "graph node payload is invalid")))
        (setf (gethash node-id node-ids) kind)))
    (dolist (row (%cegs-items (gethash "edges" materialization)))
      (let* ((edge-id (gethash "edge_id" row))
             (from (gethash "from_node_id" row))
             (to (gethash "to_node_id" row))
             (predicate (gethash "predicate" row ""))
             (payload-json (gethash "payload_json" row))
             (expected
               (%conversation-episode-graph-integrity
                projection agent-id persona-id edge-id from predicate to
                payload-json)))
        (unless (and (string= projection (gethash "projection_name" row ""))
                     (string= agent-id (gethash "agent_id" row ""))
                     (string= persona-id (gethash "persona_id" row ""))
                     (string= predicate "has-concept")
                     (string= "episode" (gethash from node-ids ""))
                     (string= "concept" (gethash to node-ids ""))
                     (string= edge-id
                              (%conversation-episode-graph-id
                               "edge"
                               (format nil "~a|has-concept|~a" from to)))
                     (string= expected (gethash "integrity_hash" row "")))
          (error 'storage-integrity-error
                 :operation :conversation-episode-graph
                 :detail "graph edge row is invalid"))
        (when (gethash edge-id edge-ids)
          (error 'storage-integrity-error
                 :operation :conversation-episode-graph
                 :detail "graph edge identity is duplicated"))
        (setf (gethash edge-id edge-ids) t)))
    (dolist (row (%cegs-items (gethash "evidence" materialization)))
      (unless (and (string= projection (gethash "projection_name" row ""))
                   (string= agent-id (gethash "agent_id" row ""))
                   (string= persona-id (gethash "persona_id" row ""))
                   (member (gethash "owner_kind" row "") '("node" "edge")
                           :test #'string=)
                   (stringp (gethash "owner_id" row))
                   (if (string= "node" (gethash "owner_kind" row ""))
                       (gethash (gethash "owner_id" row) node-ids)
                       (gethash (gethash "owner_id" row) edge-ids))
                   (integerp (gethash "evidence_event_id" row))
                   (member (gethash "evidence_role" row "")
                           '("descriptor" "source") :test #'string=)
                   (integerp (gethash "evidence_ordinal" row)))
        (error 'storage-integrity-error
               :operation :conversation-episode-graph
               :detail "graph evidence row is invalid")))
    materialization))

(defun %cegs-row-token (collection row)
  (let ((json (shasht:write-json row nil)))
    (parse-integer
     (%storage-sha256
      (format nil "~d:~a~d:~a"
              (length collection) collection (length json) json))
     :radix 16)))

(defun %cegs-accumulate-row (xor sum collection row &optional remove-p)
  (let ((token (%cegs-row-token collection row)))
    (values (logxor xor token)
            (mod (+ sum (if remove-p (- token) token))
                 +cegs-accumulator-modulus+))))

(defun %cegs-materialization-accumulators (materialization)
  (let ((xor 0) (sum 0))
    (dolist (collection '("nodes" "edges" "evidence"))
      (dolist (row (%cegs-items (gethash collection materialization)))
        (multiple-value-setq (xor sum)
          (%cegs-accumulate-row xor sum collection row))))
    (values xor sum)))

(defun %cegs-accumulator-hex (value)
  (string-downcase (format nil "~64,'0x" value)))

(defun %cegs-hex256-p (value)
  (and (stringp value) (= 64 (length value))
       (every (lambda (character) (digit-char-p character 16)) value)))

(defun %cegs-digest-from-components
    (projection revision agent-id persona-id node-count edge-count
     evidence-count xor sum)
  (%storage-sha256
   (format nil "~a|~a|~a|~a|~d|~d|~d|~a|~a"
           projection revision agent-id persona-id
           node-count edge-count evidence-count
           (%cegs-accumulator-hex xor) (%cegs-accumulator-hex sum))))

(defun %cegs-graph-integrity-state (materialization)
  "Return a composable row-set integrity state for MATERIALIZATION."
  (let ((nodes (%cegs-items (gethash "nodes" materialization)))
        (edges (%cegs-items (gethash "edges" materialization)))
        (evidence (%cegs-items (gethash "evidence" materialization))))
    (multiple-value-bind (xor sum)
        (%cegs-materialization-accumulators materialization)
      (values
       (%cegs-digest-from-components
        (gethash "projection_name" materialization)
        (gethash "projection_revision" materialization)
        (gethash "agent_id" materialization)
        (gethash "persona_id" materialization)
        (length nodes) (length edges) (length evidence) xor sum)
       xor sum (length nodes) (length edges) (length evidence)))))

(defun %cegs-delete-partition (handle projection agent-id persona-id)
  (dolist (table '("pai_knowledge_graph_evidence"
                   "pai_knowledge_graph_edges"
                   "pai_knowledge_graph_nodes"))
    (%with-sqlite-statement
        (statement handle
                   (format nil
                           "DELETE FROM ~a WHERE projection_name=?1 AND agent_id=?2 AND persona_id=?3"
                           table)
                   :conversation-episode-graph-persist)
      (%sqlite-bind-text handle statement 1 projection
                         :conversation-episode-graph-persist)
      (%sqlite-bind-text handle statement 2 agent-id
                         :conversation-episode-graph-persist)
      (%sqlite-bind-text handle statement 3 persona-id
                         :conversation-episode-graph-persist)
      (%sqlite-step handle statement :conversation-episode-graph-persist
                    +sqlite-done+))))

(defun %cegs-insert-nodes (handle rows)
  (dolist (row (%cegs-items rows))
    (%with-sqlite-statement
        (statement handle
                   "INSERT INTO pai_knowledge_graph_nodes(projection_name,agent_id,persona_id,node_id,node_kind,canonical_key,payload_json,integrity_hash) VALUES(?1,?2,?3,?4,?5,?6,?7,?8)"
                   :conversation-episode-graph-persist)
      (loop for key in '("projection_name" "agent_id" "persona_id" "node_id"
                         "node_kind" "canonical_key" "payload_json"
                         "integrity_hash")
            for index from 1
            do (%sqlite-bind-text handle statement index (gethash key row)
                                  :conversation-episode-graph-persist))
      (%sqlite-step handle statement :conversation-episode-graph-persist
                    +sqlite-done+))))

(defun %cegs-insert-edges (handle rows)
  (dolist (row (%cegs-items rows))
    (%with-sqlite-statement
        (statement handle
                   "INSERT INTO pai_knowledge_graph_edges(projection_name,agent_id,persona_id,edge_id,from_node_id,predicate,to_node_id,payload_json,integrity_hash) VALUES(?1,?2,?3,?4,?5,?6,?7,?8,?9)"
                   :conversation-episode-graph-persist)
      (loop for key in '("projection_name" "agent_id" "persona_id" "edge_id"
                         "from_node_id" "predicate" "to_node_id"
                         "payload_json" "integrity_hash")
            for index from 1
            do (%sqlite-bind-text handle statement index (gethash key row)
                                  :conversation-episode-graph-persist))
      (%sqlite-step handle statement :conversation-episode-graph-persist
                    +sqlite-done+))))

(defun %cegs-insert-evidence (handle rows)
  (dolist (row (%cegs-items rows))
    (%with-sqlite-statement
        (statement handle
                   "INSERT INTO pai_knowledge_graph_evidence(projection_name,agent_id,persona_id,owner_kind,owner_id,evidence_event_id,evidence_role,evidence_ordinal) VALUES(?1,?2,?3,?4,?5,?6,?7,?8)"
                   :conversation-episode-graph-persist)
      (loop for key in '("projection_name" "agent_id" "persona_id"
                         "owner_kind" "owner_id")
            for index from 1
            do (%sqlite-bind-text handle statement index (gethash key row)
                                  :conversation-episode-graph-persist))
      (%sqlite-bind-int64 handle statement 6 (gethash "evidence_event_id" row)
                          :conversation-episode-graph-persist)
      (%sqlite-bind-text handle statement 7 (gethash "evidence_role" row)
                         :conversation-episode-graph-persist)
      (%sqlite-bind-int64 handle statement 8 (gethash "evidence_ordinal" row)
                          :conversation-episode-graph-persist)
      (%sqlite-step handle statement :conversation-episode-graph-persist
                    +sqlite-done+))))

(defun %cegs-delete-owner-evidence (handle projection agent-id persona-id
                                    owner-kind owner-id)
  (%with-sqlite-statement
      (statement handle
                 "DELETE FROM pai_knowledge_graph_evidence WHERE projection_name=?1 AND agent_id=?2 AND persona_id=?3 AND owner_kind=?4 AND owner_id=?5"
                 :conversation-episode-graph-tail)
    (loop for value in (list projection agent-id persona-id owner-kind owner-id)
          for index from 1
          do (%sqlite-bind-text handle statement index value
                                :conversation-episode-graph-tail))
    (%sqlite-step handle statement :conversation-episode-graph-tail
                  +sqlite-done+)))

(defun %cegs-delete-edge (handle projection agent-id persona-id edge-id)
  (%cegs-delete-owner-evidence handle projection agent-id persona-id
                               "edge" edge-id)
  (%with-sqlite-statement
      (statement handle
                 "DELETE FROM pai_knowledge_graph_edges WHERE projection_name=?1 AND agent_id=?2 AND persona_id=?3 AND edge_id=?4"
                 :conversation-episode-graph-tail)
    (loop for value in (list projection agent-id persona-id edge-id)
          for index from 1
          do (%sqlite-bind-text handle statement index value
                                :conversation-episode-graph-tail))
    (%sqlite-step handle statement :conversation-episode-graph-tail
                  +sqlite-done+)))

(defun %cegs-delete-node (handle projection agent-id persona-id node-id)
  (%cegs-delete-owner-evidence handle projection agent-id persona-id
                               "node" node-id)
  (%with-sqlite-statement
      (statement handle
                 "DELETE FROM pai_knowledge_graph_nodes WHERE projection_name=?1 AND agent_id=?2 AND persona_id=?3 AND node_id=?4"
                 :conversation-episode-graph-tail)
    (loop for value in (list projection agent-id persona-id node-id)
          for index from 1
          do (%sqlite-bind-text handle statement index value
                                :conversation-episode-graph-tail))
    (%sqlite-step handle statement :conversation-episode-graph-tail
                  +sqlite-done+)))

(defun %cegs-orphan-concept-p (handle projection agent-id persona-id node-id)
  (%with-sqlite-statement
      (statement handle
                 "SELECT 1 FROM pai_knowledge_graph_edges WHERE projection_name=?1 AND agent_id=?2 AND persona_id=?3 AND to_node_id=?4 LIMIT 1"
                 :conversation-episode-graph-tail)
    (loop for value in (list projection agent-id persona-id node-id)
          for index from 1
          do (%sqlite-bind-text handle statement index value
                                :conversation-episode-graph-tail))
    (let ((code (%sqlite-step-raw statement)))
      (cond ((= code +sqlite-row+) nil)
            ((= code +sqlite-done+) t)
            (t (%sqlite-check code handle
                              :conversation-episode-graph-tail))))))

(defun %cegs-current-watermark (handle projection agent-id)
  (%with-sqlite-statement
      (statement handle
                 "SELECT through_event_id,through_storage_position FROM pai_projection_checkpoints WHERE projection_name=?1 AND agent_id=?2"
                 :conversation-episode-graph-persist)
    (%sqlite-bind-text handle statement 1 projection
                       :conversation-episode-graph-persist)
    (%sqlite-bind-text handle statement 2 agent-id
                       :conversation-episode-graph-persist)
    (let ((code (%sqlite-step-raw statement)))
      (cond ((= code +sqlite-row+)
             (values (%sqlite-column-int64 statement 0)
                     (%sqlite-column-int64 statement 1)))
            ((= code +sqlite-done+) (values nil nil))
            (t (%sqlite-check code handle
                              :conversation-episode-graph-persist))))))

(defun %cegs-write-checkpoint
    (handle projection agent-id through-event-id through-position state)
  (let* ((state-json (%storage-json state))
         (projector (gethash "projection_revision" state))
         (policy *conversation-episode-graph-storage-policy-revision*)
         (integrity
           (%storage-sha256
            (%storage-checkpoint-integrity-input
             projection agent-id through-event-id through-position
             projector policy state-json))))
    (%with-sqlite-statement
        (statement handle
                   "INSERT INTO pai_projection_checkpoints(projection_name,agent_id,through_event_id,through_storage_position,projector_revision,policy_revision,state_json,integrity_hash) VALUES(?1,?2,?3,?4,?5,?6,?7,?8) ON CONFLICT(projection_name,agent_id) DO UPDATE SET through_event_id=excluded.through_event_id,through_storage_position=excluded.through_storage_position,projector_revision=excluded.projector_revision,policy_revision=excluded.policy_revision,state_json=excluded.state_json,integrity_hash=excluded.integrity_hash,created_at=CURRENT_TIMESTAMP"
                   :conversation-episode-graph-persist)
      (%sqlite-bind-text handle statement 1 projection
                         :conversation-episode-graph-persist)
      (%sqlite-bind-text handle statement 2 agent-id
                         :conversation-episode-graph-persist)
      (%sqlite-bind-int64 handle statement 3 through-event-id
                          :conversation-episode-graph-persist)
      (%sqlite-bind-int64 handle statement 4 through-position
                          :conversation-episode-graph-persist)
      (%sqlite-bind-text handle statement 5 projector
                         :conversation-episode-graph-persist)
      (%sqlite-bind-text handle statement 6 policy
                         :conversation-episode-graph-persist)
      (%sqlite-bind-text handle statement 7 state-json
                         :conversation-episode-graph-persist)
      (%sqlite-bind-text handle statement 8 integrity
                         :conversation-episode-graph-persist)
      (%sqlite-step handle statement :conversation-episode-graph-persist
                    +sqlite-done+))))

(defun conversation-episode-graph-persist
    (backend materialization &key through-event-id through-position
       event-storage-id boundary-hash)
  "Atomically replace one derived episode graph partition and its checkpoint."
  (unless (typep backend 'sqlite-derived-storage)
    (error 'storage-error :operation :conversation-episode-graph-persist
           :detail "KG1 persistence requires derived SQLite storage"))
  (%storage-positive-integer through-event-id "through-event-id"
                             :zero-allowed t)
  (%storage-positive-integer through-position "through-position"
                             :zero-allowed t)
  (%cegs-required-string event-storage-id "event-storage-id")
  (%cegs-required-string boundary-hash "boundary-hash")
  (let* ((canonical (%cegs-canonical-materialization materialization))
         (projection (gethash "projection_name" canonical))
         (agent-id (gethash "agent_id" canonical))
         (persona-id (gethash "persona_id" canonical))
         (nodes (gethash "nodes" canonical))
         (edges (gethash "edges" canonical))
         (evidence (gethash "evidence" canonical)))
    (multiple-value-bind (digest xor sum node-count edge-count evidence-count)
        (%cegs-graph-integrity-state canonical)
      (let ((state
              (obj "schema_version" 2 "persona_id" persona-id
                   "event_storage_id" event-storage-id
                   "boundary_hash" boundary-hash
                   "projection_revision"
                   (gethash "projection_revision" canonical)
                   "episode_count"
                   (count "episode" (%cegs-items nodes)
                          :key (lambda (row) (gethash "node_kind" row ""))
                          :test #'string=)
                   "node_count" node-count "edge_count" edge-count
                   "evidence_count" evidence-count
                   "row_xor" (%cegs-accumulator-hex xor)
                   "row_sum" (%cegs-accumulator-hex sum)
                   "graph_digest" digest)))
        (bt:with-lock-held ((%sqlite-derived-lock backend))
          (%sqlite-derived-in-transaction
           backend :conversation-episode-graph-persist
           (lambda (handle)
             (multiple-value-bind (current-event current-position)
                 (%cegs-current-watermark handle projection agent-id)
               (when (or (and current-event (< through-event-id current-event))
                         (and current-position
                              (< through-position current-position)))
                 (error 'storage-conflict-error
                        :operation :conversation-episode-graph-persist
                        :detail "graph checkpoint would move backwards")))
             (%cegs-delete-partition handle projection agent-id persona-id)
             (%cegs-insert-nodes handle nodes)
             (%cegs-insert-edges handle edges)
             (%cegs-insert-evidence handle evidence)
             (%cegs-write-checkpoint handle projection agent-id
                                     through-event-id through-position state))))
        (obj "schema_version" 1 "status" "persisted"
             "node_count" node-count "edge_count" edge-count
             "evidence_count" evidence-count "graph_digest" digest
             "through_storage_position" through-position
             "event_write_count" 0 "memory_write_count" 0)))))

(defun %cegs-checkpoint-generation (backend agent-id persona-id event-storage-id)
  "Load only the signed checkpoint state, never graph rows."
  (let* ((checkpoint
           (storage-load-checkpoint
            backend *conversation-episode-graph-storage-projection-name*
            :agent-id agent-id))
         (state (and checkpoint (gethash "state" checkpoint))))
    (unless (and checkpoint (hash-table-p state)
                 (= 2 (gethash "schema_version" state -1))
                 (string= persona-id (gethash "persona_id" state ""))
                 (string= event-storage-id
                          (gethash "event_storage_id" state ""))
                 (string= *conversation-episode-projection-revision*
                          (gethash "projection_revision" state ""))
                 (string= *conversation-episode-graph-storage-policy-revision*
                          (gethash "policy_revision" checkpoint ""))
                 (every #'integerp
                        (mapcar (lambda (key) (gethash key state))
                                '("episode_count" "node_count" "edge_count"
                                  "evidence_count")))
                 (every (lambda (key)
                          (%cegs-hex256-p (gethash key state)))
                        '("row_xor" "row_sum" "graph_digest")))
      (error 'storage-integrity-error
             :operation :conversation-episode-graph-restore
             :detail "graph checkpoint binding is invalid"))
    (values checkpoint state)))

(defun %cegs-node-row (handle projection agent-id persona-id node-id)
  (let ((rows
          (%cegs-read-rows
           handle
           "SELECT projection_name,agent_id,persona_id,node_id,node_kind,canonical_key,payload_json,integrity_hash FROM pai_knowledge_graph_nodes WHERE projection_name=?1 AND agent_id=?2 AND persona_id=?3 AND node_id=?4"
           '("projection_name" "agent_id" "persona_id" "node_id" "node_kind"
             "canonical_key" "payload_json" "integrity_hash")
           nil :conversation-episode-graph-tail
           projection agent-id persona-id node-id)))
    (when (plusp (length rows)) (aref rows 0))))

(defun %cegs-outgoing-edge-rows
    (handle projection agent-id persona-id node-id)
  (%cegs-read-rows
   handle
   "SELECT projection_name,agent_id,persona_id,edge_id,from_node_id,predicate,to_node_id,payload_json,integrity_hash FROM pai_knowledge_graph_edges WHERE projection_name=?1 AND agent_id=?2 AND persona_id=?3 AND from_node_id=?4 ORDER BY edge_id"
   '("projection_name" "agent_id" "persona_id" "edge_id" "from_node_id"
     "predicate" "to_node_id" "payload_json" "integrity_hash")
   nil :conversation-episode-graph-tail
   projection agent-id persona-id node-id))

(defun %cegs-owner-evidence-rows
    (handle projection agent-id persona-id owner-kind owner-id)
  (%cegs-read-rows
   handle
   "SELECT projection_name,agent_id,persona_id,owner_kind,owner_id,evidence_event_id,evidence_role,evidence_ordinal FROM pai_knowledge_graph_evidence WHERE projection_name=?1 AND agent_id=?2 AND persona_id=?3 AND owner_kind=?4 AND owner_id=?5 ORDER BY evidence_event_id,evidence_role,evidence_ordinal"
   '("projection_name" "agent_id" "persona_id" "owner_kind" "owner_id"
     "evidence_event_id" "evidence_role" "evidence_ordinal")
   '("evidence_event_id" "evidence_ordinal")
   :conversation-episode-graph-tail
   projection agent-id persona-id owner-kind owner-id))

(defun conversation-episode-graph-persist-tail
    (backend materialization changed-episode-ids
     &key through-event-id through-position event-storage-id boundary-hash)
  "Advance a verified generation from only the changed episode slice.

MATERIALIZATION contains no unchanged episodes.  The signed row XOR, modular
sum and counts are advanced by removing the prior changed rows and adding the
new rows. Full warm restore remains the independent whole-generation verifier."
  (unless (typep backend 'sqlite-derived-storage)
    (error 'storage-error :operation :conversation-episode-graph-tail
           :detail "KG1 tail persistence requires derived SQLite storage"))
  (%storage-positive-integer through-event-id "through-event-id"
                             :zero-allowed t)
  (%storage-positive-integer through-position "through-position"
                             :zero-allowed t)
  (%cegs-required-string event-storage-id "event-storage-id")
  (%cegs-required-string boundary-hash "boundary-hash")
  (let* ((canonical (%cegs-canonical-materialization materialization))
         (projection (gethash "projection_name" canonical))
         (revision (gethash "projection_revision" canonical))
         (agent-id (gethash "agent_id" canonical))
         (persona-id (gethash "persona_id" canonical))
         (changed (remove-duplicates (%cegs-items changed-episode-ids)
                                     :test #'string=))
         (changed-node-ids
           (mapcar (lambda (id)
                     (%conversation-episode-graph-id "episode" id))
                   changed))
         (nodes (%cegs-items (gethash "nodes" canonical)))
         (edges (%cegs-items (gethash "edges" canonical)))
         (evidence (%cegs-items (gethash "evidence" canonical)))
         (new-episode-nodes
           (remove-if-not (lambda (row)
                            (string= "episode"
                                     (gethash "node_kind" row "")))
                          nodes))
         (new-concept-nodes
           (remove-if-not (lambda (row)
                            (string= "concept"
                                     (gethash "node_kind" row "")))
                          nodes)))
    (multiple-value-bind (checkpoint prior)
        (%cegs-checkpoint-generation
         backend agent-id persona-id event-storage-id)
      (let ((xor (parse-integer (gethash "row_xor" prior) :radix 16))
            (sum (parse-integer (gethash "row_sum" prior) :radix 16))
            (node-count (gethash "node_count" prior))
            (edge-count (gethash "edge_count" prior))
            (evidence-count (gethash "evidence_count" prior))
            (episode-count (gethash "episode_count" prior))
            (removed-nodes 0) (removed-edges 0) (removed-evidence 0)
            (inserted-nodes 0) (inserted-edges 0) (inserted-evidence 0))
        (labels ((fold-row (collection row remove-p)
                   (multiple-value-setq (xor sum)
                     (%cegs-accumulate-row xor sum collection row remove-p))
                   (cond
                     ((string= collection "nodes")
                      (if remove-p (incf removed-nodes)
                          (incf inserted-nodes)))
                     ((string= collection "edges")
                      (if remove-p (incf removed-edges)
                          (incf inserted-edges)))
                     ((string= collection "evidence")
                      (if remove-p (incf removed-evidence)
                          (incf inserted-evidence)))
                     (t (error "Unknown graph row collection ~a"
                               collection))))
                 (fold-rows (collection rows remove-p)
                   (dolist (row (%cegs-items rows))
                     (fold-row collection row remove-p))))
          (bt:with-lock-held ((%sqlite-derived-lock backend))
            (%sqlite-derived-in-transaction
             backend :conversation-episode-graph-tail
             (lambda (handle)
               (multiple-value-bind (current-event current-position)
                   (%cegs-current-watermark handle projection agent-id)
                 (unless (and current-position
                              (= current-event
                                 (gethash "through_event_id" checkpoint))
                              (= current-position
                                 (gethash "through_storage_position"
                                          checkpoint)))
                   (error 'storage-conflict-error
                          :operation :conversation-episode-graph-tail
                          :detail "graph checkpoint changed during tail fold"))
                 (when (or (< through-event-id current-event)
                           (< through-position current-position))
                   (error 'storage-conflict-error
                          :operation :conversation-episode-graph-tail
                          :detail "graph checkpoint would move backwards")))
               (let ((old-concepts nil)
                     (existing-concepts (make-hash-table :test #'equal)))
                 ;; Capture and remove only rows owned by changed episodes.
                 (dolist (node-id changed-node-ids)
                   (let ((old-node (%cegs-node-row
                                    handle projection agent-id persona-id
                                    node-id)))
                     (when old-node
                       (decf episode-count)
                       (fold-row "nodes" old-node t)
                       (fold-rows
                        "evidence"
                        (%cegs-owner-evidence-rows
                         handle projection agent-id persona-id "node" node-id)
                        t))
                     (dolist (edge (%cegs-items
                                    (%cegs-outgoing-edge-rows
                                     handle projection agent-id persona-id
                                     node-id)))
                       (pushnew (gethash "to_node_id" edge) old-concepts
                                :test #'string=)
                       (fold-row "edges" edge t)
                       (fold-rows
                        "evidence"
                        (%cegs-owner-evidence-rows
                         handle projection agent-id persona-id "edge"
                         (gethash "edge_id" edge))
                        t)
                       (%cegs-delete-edge handle projection agent-id persona-id
                                          (gethash "edge_id" edge)))
                     (when old-node
                       (%cegs-delete-node handle projection agent-id persona-id
                                          node-id))))
                 ;; Existing concepts are immutable canonical label rows.
                 (dolist (row new-concept-nodes)
                   (let ((node-id (gethash "node_id" row)))
                     (when (%cegs-node-row handle projection agent-id persona-id
                                           node-id)
                       (setf (gethash node-id existing-concepts) t))))
                 (%cegs-insert-nodes handle new-episode-nodes)
                 (fold-rows "nodes" new-episode-nodes nil)
                 (incf episode-count (length new-episode-nodes))
                 (dolist (row new-concept-nodes)
                   (unless (gethash (gethash "node_id" row) existing-concepts)
                     (%cegs-insert-nodes handle (list row))
                     (fold-row "nodes" row nil)))
                 (%cegs-insert-edges handle edges)
                 (fold-rows "edges" edges nil)
                 (%cegs-insert-evidence handle evidence)
                 (fold-rows "evidence" evidence nil)
                 ;; A concept is retained precisely while an incoming edge
                 ;; exists. Its evidence is already carried by those edges.
                 (dolist (concept-id old-concepts)
                   (when (%cegs-orphan-concept-p
                          handle projection agent-id persona-id concept-id)
                     (let ((row (%cegs-node-row
                                 handle projection agent-id persona-id
                                 concept-id)))
                       (when row
                         (fold-row "nodes" row t)
                         (%cegs-delete-node handle projection agent-id persona-id
                                            concept-id)))))
                 (incf node-count (- inserted-nodes removed-nodes))
                 (incf edge-count (- inserted-edges removed-edges))
                 (incf evidence-count
                       (- inserted-evidence removed-evidence))
                 (let* ((digest
                          (%cegs-digest-from-components
                           projection revision agent-id persona-id
                           node-count edge-count evidence-count xor sum))
                        (state
                          (obj "schema_version" 2 "persona_id" persona-id
                               "event_storage_id" event-storage-id
                               "boundary_hash" boundary-hash
                               "projection_revision" revision
                               "episode_count" episode-count
                               "node_count" node-count
                               "edge_count" edge-count
                               "evidence_count" evidence-count
                               "row_xor" (%cegs-accumulator-hex xor)
                               "row_sum" (%cegs-accumulator-hex sum)
                               "graph_digest" digest)))
                   (%cegs-write-checkpoint
                    handle projection agent-id through-event-id
                    through-position state))))))
          (let ((digest
                  (%cegs-digest-from-components
                   projection revision agent-id persona-id
                   node-count edge-count evidence-count xor sum)))
            (obj "schema_version" 1 "status" "tail-persisted"
                 "changed_episode_count" (length changed)
                 "episode_count" episode-count
                 "node_count" node-count "edge_count" edge-count
                 "evidence_count" evidence-count "graph_digest" digest
                 "removed_row_count"
                 (+ removed-nodes removed-edges removed-evidence)
                 "inserted_row_count"
                 (+ inserted-nodes inserted-edges inserted-evidence)
                 "full_generation_rows_read" 0
                 "through_storage_position" through-position
                 "event_write_count" 0 "memory_write_count" 0)))))))

(defun %cegs-read-rows
    (handle sql keys integer-keys operation &rest text-bindings)
  (let ((rows nil))
    (%with-sqlite-statement (statement handle sql operation)
      (loop for value in text-bindings for index from 1
            do (%sqlite-bind-text handle statement index value operation))
      (loop for code = (%sqlite-step-raw statement)
            while (= code +sqlite-row+)
            do (let ((row (make-hash-table :test #'equal)))
                 (loop for key in keys for index from 0
                       do (setf (gethash key row)
                                (if (member key integer-keys :test #'string=)
                                    (%sqlite-column-int64 statement index)
                                    (%sqlite-column-text statement index))))
                 (push row rows))
            finally (unless (= code +sqlite-done+)
                      (%sqlite-check code handle operation))))
    (coerce (nreverse rows) 'vector)))

(defun %cegs-read-materialization (backend agent-id persona-id checkpoint)
  (let* ((state (gethash "state" checkpoint))
         (projection *conversation-episode-graph-storage-projection-name*)
         (nodes nil) (edges nil) (evidence nil))
    (bt:with-lock-held ((%sqlite-derived-lock backend))
      (let ((handle (%sqlite-derived-handle backend
                                            :conversation-episode-graph-restore)))
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
                 nil :conversation-episode-graph-restore
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
                 nil :conversation-episode-graph-restore
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
                 :conversation-episode-graph-restore
                 projection agent-id persona-id)))))
    (obj "schema_version" 1 "projection_name" projection
         "projection_revision" (gethash "projection_revision" state)
         "agent_id" agent-id "persona_id" persona-id
         "nodes" nodes "edges" edges "evidence" evidence)))

(defun conversation-episode-graph-restore
    (backend agent-id persona-id &key event-storage-id)
  "Restore and independently verify one persisted episode graph partition."
  (%cegs-required-string agent-id "agent-id")
  (%cegs-required-string persona-id "persona-id")
  (%cegs-required-string event-storage-id "event-storage-id")
  (multiple-value-bind (checkpoint state)
      (%cegs-checkpoint-generation backend agent-id persona-id event-storage-id)
    (let* ((stored (%cegs-read-materialization
                    backend agent-id persona-id checkpoint))
           (canonical (%cegs-canonical-materialization stored)))
      (multiple-value-bind
            (digest xor sum node-count edge-count evidence-count)
          (%cegs-graph-integrity-state canonical)
        (unless (and (= node-count (gethash "node_count" state -1))
                     (= edge-count (gethash "edge_count" state -1))
                     (= evidence-count (gethash "evidence_count" state -1))
                     (= (count "episode" (%cegs-items
                                           (gethash "nodes" canonical))
                               :key (lambda (row)
                                      (gethash "node_kind" row ""))
                               :test #'string=)
                        (gethash "episode_count" state -1))
                     (string= (%cegs-accumulator-hex xor)
                              (gethash "row_xor" state ""))
                     (string= (%cegs-accumulator-hex sum)
                              (gethash "row_sum" state ""))
                     (string= digest (gethash "graph_digest" state "")))
          (error 'storage-integrity-error
                 :operation :conversation-episode-graph-restore
                 :detail "graph checkpoint digest or counts do not match"))
        (values
         (%cegs-episodes canonical)
         (obj "schema_version" 1 "status" "restored"
              "episode_count" (gethash "episode_count" state)
              "node_count" node-count "edge_count" edge-count
              "evidence_count" evidence-count "graph_digest" digest
              "event_storage_id" (gethash "event_storage_id" state)
              "boundary_hash" (gethash "boundary_hash" state)
              "through_event_id" (gethash "through_event_id" checkpoint)
              "through_storage_position"
              (gethash "through_storage_position" checkpoint)
              "database_write_count" 0))))))

(defun conversation-episode-graph-inspect
    (backend agent-id persona-id &key event-storage-id)
  "Content-free verified graph health. This read performs no durable writes."
  (multiple-value-bind (episodes report)
      (conversation-episode-graph-restore
       backend agent-id persona-id :event-storage-id event-storage-id)
    (declare (ignore episodes))
    report))
