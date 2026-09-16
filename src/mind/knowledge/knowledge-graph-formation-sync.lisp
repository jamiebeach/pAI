;;;; knowledge-graph-formation-sync.lisp -- event-bound KG2 maintenance.

(in-package :agent)

(export '(knowledge-graph-formation-synchronize))

(defun %kgf-sync-events (event-backend agent-id after-position boundary)
  (let ((receipts nil)
        (storage-id (gethash "storage_id" boundary)))
    (multiple-value-bind (complete-p ignored-last count)
        (storage-map-event-receipts
         event-backend
         (lambda (receipt)
           (unless (string= storage-id (gethash "storage_id" receipt ""))
             (%kgfs-fail "KG2 tail receipt belongs to another event store"))
           (push receipt receipts))
         :agent-id agent-id :after-position after-position
         :through-position (gethash "through_storage_position" boundary)
         :event-types '("knowledge-graph-formation-sealed"))
      (declare (ignore ignored-last))
      (unless complete-p
        (error 'storage-unavailable-error
               :operation :knowledge-graph-formation-sync
               :detail "bounded KG2 receipt stream did not complete"))
      (values (nreverse receipts) count))))

(defun %kgf-sync-cold (event-backend derived-backend agent-id persona-id
                       boundary fallback-reason)
  (multiple-value-bind (receipts count)
      (%kgf-sync-events event-backend agent-id 0 boundary)
    (let* ((events
             (mapcar (lambda (receipt)
                       (%storage-json-read (gethash "event_json" receipt "")
                                           :knowledge-graph-formation-sync))
                     receipts))
           (state (knowledge-graph-formation-project
                   events agent-id persona-id))
           (materialization (knowledge-graph-formation-materialization state))
           (persist
             (knowledge-graph-formation-persist
              derived-backend materialization
              :through-event-id (gethash "through_event_id" boundary)
              :through-position (gethash "through_storage_position" boundary)
              :event-storage-id (gethash "storage_id" boundary)
              :boundary-hash (gethash "source_binding" boundary))))
      (obj "schema_version" 1 "status" "synchronized"
           "mode" "cold-rebuild"
           "fallback_reason" (or fallback-reason :null)
           "formation_event_count" count
           "through_event_id" (gethash "through_event_id" boundary)
           "through_storage_position"
           (gethash "through_storage_position" boundary)
           "node_count" (gethash "node_count" persist)
           "edge_count" (gethash "edge_count" persist)
           "evidence_count" (gethash "evidence_count" persist)
           "event_write_count" 0 "memory_write_count" 0))))

(defun %kgf-sync-row-payload (row operation)
  (let ((payload (%storage-json-read (gethash "payload_json" row "")
                                     operation)))
    (unless (hash-table-p payload) (%kgfs-fail "KG2 row payload is invalid"))
    payload))

(defun %kgf-sync-matching-edges
    (handle projection agent-id persona-id from predicate to)
  (%cegs-read-rows
   handle
   "SELECT projection_name,agent_id,persona_id,edge_id,from_node_id,predicate,to_node_id,payload_json,integrity_hash FROM pai_knowledge_graph_edges WHERE projection_name=?1 AND agent_id=?2 AND persona_id=?3 AND from_node_id=?4 AND predicate=?5 AND to_node_id=?6 ORDER BY edge_id"
   '("projection_name" "agent_id" "persona_id" "edge_id" "from_node_id"
     "predicate" "to_node_id" "payload_json" "integrity_hash")
   nil :knowledge-graph-formation-sync
   projection agent-id persona-id from predicate to))

(defun %kgf-sync-ref-ids (event)
  (let* ((payload (gethash "payload" event))
         (persona-id (gethash "persona_id" payload))
         (event-id (gethash "id" event))
         (refs (make-hash-table :test #'equal)))
    (loop for entity across
          (gethash "entities" (gethash "proposal" payload))
          for ordinal from 0
          do (setf (gethash (gethash "local_ref" entity) refs)
                   (if (string= "LINK_EXISTING"
                                (gethash "identity_action" entity))
                       (gethash "existing_node_id" entity)
                       (%kgf-id "entity" persona-id event-id ordinal))))
    refs))

(defun %kgf-sync-partial-state
    (derived-backend event agent-id persona-id)
  "Read only explicitly referenced nodes and retirement candidates."
  (let* ((payload (gethash "payload" event))
         (proposal (gethash "proposal" payload))
         (projection *knowledge-graph-formation-projection-name*)
         (refs (%kgf-sync-ref-ids event))
         (nodes nil) (edges nil))
    (bt:with-lock-held ((%sqlite-derived-lock derived-backend))
      (let ((handle (%sqlite-derived-handle
                     derived-backend :knowledge-graph-formation-sync)))
        (loop for entity across (gethash "entities" proposal)
              for action = (gethash "identity_action" entity)
              unless (string= action "NEW")
                do (let* ((id (gethash "existing_node_id" entity))
                          (row (%kgfs-node-row handle projection agent-id
                                               persona-id id)))
                     (unless row (%kgfs-fail "KG2 referenced node is absent"))
                     (pushnew (%kgf-sync-row-payload
                               row :knowledge-graph-formation-sync)
                              nodes :key (lambda (node)
                                           (gethash "node_id" node))
                              :test #'string=)))
        (loop for relation across (gethash "relationships" proposal)
              do (let* ((from (gethash (gethash "subject_ref" relation)
                                         refs))
                          (to (gethash (gethash "object_ref" relation) refs))
                          (predicate (gethash "predicate" relation)))
                     (dolist (row (%cegs-items
                                   (%kgf-sync-matching-edges
                                    handle projection agent-id persona-id
                                    from predicate to)))
                       (let ((edge (%kgf-sync-row-payload
                                    row :knowledge-graph-formation-sync)))
                         (pushnew edge edges
                                  :key (lambda (item)
                                         (gethash "edge_id" item))
                                  :test #'string=)))))))
    (%kgf-state agent-id persona-id
                (%kgf-index nodes "node_id") (%kgf-index edges "edge_id"))))

(defun %kgf-sync-changed-ids (before after collection id-key)
  (let ((prior (%kgf-index (gethash collection before) id-key))
        (changed nil))
    (dolist (row (%kgf-items (gethash collection after)))
      (let* ((id (gethash id-key row))
             (old (gethash id prior)))
        (unless (and old
                     (string= (shasht:write-json old nil)
                              (shasht:write-json row nil)))
          (push id changed))))
    (nreverse changed)))

(defun %kgf-sync-tail (event-backend derived-backend agent-id persona-id
                       boundary)
  (multiple-value-bind (checkpoint prior)
      (%kgfs-checkpoint derived-backend agent-id persona-id
                        (gethash "storage_id" boundary))
    (let* ((old-event (gethash "through_event_id" checkpoint))
           (old-position (gethash "through_storage_position" checkpoint))
           (old-binding
             (storage-checkpoint-source-binding
              event-backend :agent-id agent-id
              :through-event-id old-event :through-position old-position)))
      (unless (and (<= old-position
                       (gethash "through_storage_position" boundary))
                   (<= old-event (gethash "through_event_id" boundary))
                   (string= old-binding (gethash "boundary_hash" prior "")))
        (%kgfs-fail "KG2 checkpoint is not a verified ledger prefix"))
      (multiple-value-bind (receipts scanned-count)
          (%kgf-sync-events event-backend agent-id old-position boundary)
        (let* ((receipt (first receipts))
               (event (and receipt
                           (%storage-json-read
                            (gethash "event_json" receipt "")
                            :knowledge-graph-formation-sync)))
               (through-position
                 (if receipt (gethash "storage_position" receipt)
                     (gethash "through_storage_position" boundary)))
               (through-event
                 (if receipt (gethash "event_id" receipt)
                     (gethash "through_event_id" boundary)))
               (binding
                 (storage-checkpoint-source-binding
                  event-backend :agent-id agent-id
                  :through-event-id through-event
                  :through-position through-position))
               (before
                 (if event
                     (%kgf-sync-partial-state derived-backend event
                                              agent-id persona-id)
                     (%kgf-state agent-id persona-id
                                 (make-hash-table :test #'equal)
                                 (make-hash-table :test #'equal))))
               (after (if event
                          (knowledge-graph-formation-fold
                           before event agent-id persona-id)
                          before))
               (node-ids (%kgf-sync-changed-ids before after
                                                "nodes" "node_id"))
               (edge-ids (%kgf-sync-changed-ids before after
                                                "edges" "edge_id"))
               (persist
                 (knowledge-graph-formation-persist-tail
                  derived-backend
                  (knowledge-graph-formation-materialization after)
                  node-ids edge-ids
                  :through-event-id through-event
                  :through-position through-position
                  :event-storage-id (gethash "storage_id" boundary)
                  :boundary-hash binding)))
          (obj "schema_version" 1 "status" "synchronized"
               "mode" "incremental-tail" "fallback_reason" :null
               "formation_event_count" (if receipt 1 0)
               "formation_events_scanned" scanned-count
               "changed_node_count" (length node-ids)
               "changed_edge_count" (length edge-ids)
               "node_count" (gethash "node_count" persist)
               "edge_count" (gethash "edge_count" persist)
               "evidence_count" (gethash "evidence_count" persist)
               "full_generation_rows_read" 0
               "through_event_id" through-event
               "through_storage_position" through-position
               "event_write_count" 0 "memory_write_count" 0))))))

(defun knowledge-graph-formation-synchronize
    (event-backend derived-backend agent-id persona-id)
  "Synchronize KG2 to a captured ledger generation, one tail receipt at a time."
  (%cegs-required-string agent-id "agent-id")
  (%cegs-required-string persona-id "persona-id")
  (let ((boundary (storage-authority-boundary
                   event-backend :agent-id agent-id)))
    (handler-case
        (%kgf-sync-tail event-backend derived-backend agent-id persona-id
                        boundary)
      (storage-error (condition)
        (%kgf-sync-cold event-backend derived-backend agent-id persona-id
                        boundary
                        (string-downcase
                         (symbol-name (type-of condition))))))))
