;;;; knowledge-graph-formation-sync-tests.lisp -- KG2 bounded rebuild/tail.

(in-package :agent)

(ql:quickload '(:cffi :shasht :ironclad :bordeaux-threads) :silent t)

(dolist (file '("storage-substrate.lisp" "memory-storage.lisp"
                "sqlite-storage.lisp" "sqlite-derived-storage.lisp"
                "conversation-episode-graph.lisp"
                "conversation-episode-graph-storage.lisp"
                "knowledge-graph-ontology.lisp"
                "knowledge-graph-formation.lisp"
                "knowledge-graph-formation-storage.lisp"
                "knowledge-graph-formation-sync.lisp"))
  (load (test-source file)))

(defvar *kgfx-pass* 0)
(defvar *kgfx-fail* 0)

(defun kgfx-check (name condition)
  (if condition
      (progn (incf *kgfx-pass*) (format t "PASS ~a~%" name))
      (progn (incf *kgfx-fail*) (format t "FAIL ~a~%" name))))

(defun kgfx-delete-db (path)
  (dolist (candidate
           (list path
                 (pathname (concatenate 'string (namestring path) "-wal"))
                 (pathname (concatenate 'string (namestring path) "-shm"))))
    (when (probe-file candidate) (delete-file candidate))))

(defun kgfx-entity (ref kind label &key (action "NEW") (existing :null))
  (obj "local_ref" ref "kind" kind "label" label "aliases" #()
       "classifications" #() "identity_action" action
       "existing_node_id" existing "evidence_status" "direct"
       "evidence_note" "fixture evidence"))

(defun kgfx-payload (entities relationships &optional (eligible #()))
  (obj "schema_version" 1 "persona_id" "kgfx-persona"
       "disclosure_class" "private"
       "formation_revision" *knowledge-graph-formation-revision*
       "source_event_ids" #(9001) "source_memory_node_ids" #()
       "source_episode_ids" #("episode:fixture")
       "source_evidence"
       (vector (obj "source_id" "event:fixture" "speaker_id" "operator"
                    "kind" "original-utterance" "timestamp" 4000000000
                    "text" "Fixture directly states this graph fact."
                    "text_sha256"
                    (%kgf-sha256 "Fixture directly states this graph fact.")))
       "eligible_existing_node_ids" eligible
       "proposal"
       (obj "schema_version" 3
            "ontology_revision" *knowledge-graph-ontology-revision*
            "entities" entities
            "relationships" relationships)))

(defun kgfx-relation (from predicate to &optional (action "ASSERT"))
  (obj "subject_ref" from "predicate" predicate "object_ref" to
       "relationship_action" action
       "fact" "Fixture directly states this graph fact."
       "grounding"
       (obj "schema_version" 1 "scope" "assertion" "polarity" "positive"
            "attributed_to_ref" :null
            "evidence" (vector (obj "source_id" "event:fixture"
                                    "quote" "Fixture directly states this graph fact.")))
       "temporal"
       (obj "schema_version" 1 "character" "unspecified"
            "occurred_at" :null "valid_from" :null "valid_until" :null)
       "evidence_status" "direct" "evidence_note" "fixture evidence"))

(defun kgfx-full-materialization (events)
  (let ((all nil))
    (storage-map-events
     events (lambda (event position)
              (declare (ignore position))
              (when (string= "knowledge-graph-formation-sealed"
                             (gethash "type" event ""))
                (push event all)))
     :agent-id "kgfx-agent")
    (knowledge-graph-formation-materialization
     (knowledge-graph-formation-project
      (nreverse all) "kgfx-agent" "kgfx-persona"))))

(format t "~%== KG2 event-bound synchronization ==~%")

(let ((event-db #p"/agent/state/kgfx-events.sqlite3")
      (derived-db #p"/agent/state/kgfx-derived.sqlite3")
      (events nil) (derived nil))
  (kgfx-delete-db event-db)
  (kgfx-delete-db derived-db)
  (unwind-protect
      (progn
        (setf events (make-sqlite-storage event-db)
              derived (make-sqlite-derived-storage derived-db))
        (storage-append-event
         events "knowledge-graph-formation-sealed"
         (kgfx-payload
          (vector (kgfx-entity "operator" "person" "Operator")
                  (kgfx-entity "need" "concept" "Accessible visuals"))
          (vector (kgfx-relation "operator" "related_to" "need")))
         :agent-id "kgfx-agent")
        (let ((report (knowledge-graph-formation-synchronize
                       events derived "kgfx-agent" "kgfx-persona")))
          (kgfx-check "absent generation cold-rebuilds at captured boundary"
                      (and (string= "cold-rebuild" (gethash "mode" report))
                           (= 2 (gethash "node_count" report))
                           (= 1 (gethash "edge_count" report)))))
        (multiple-value-bind (first ignored)
            (knowledge-graph-formation-restore
             derived "kgfx-agent" "kgfx-persona"
             :event-storage-id
             (gethash "storage_id"
                      (storage-authority-boundary
                       events :agent-id "kgfx-agent")))
          (declare (ignore ignored))
          (let ((operator-id
                  (gethash "node_id"
                           (find "person"
                                 (coerce (gethash "nodes" first) 'list)
                                 :key (lambda (row)
                                        (gethash "node_kind" row ""))
                                 :test #'string=))))
            (storage-append-event events "operator-note"
                                  (obj "text" "irrelevant")
                                  :agent-id "kgfx-agent")
            (storage-append-event
             events "knowledge-graph-formation-sealed"
             (kgfx-payload
              (vector
               (kgfx-entity "operator" "person" "Operator"
                            :action "LINK_EXISTING" :existing operator-id)
               (kgfx-entity "preference" "concept"
                            "High contrast palettes"))
              (vector (kgfx-relation "operator" "related_to" "preference"))
              (vector operator-id))
             :agent-id "kgfx-agent")))
        (let ((report (knowledge-graph-formation-synchronize
                       events derived "kgfx-agent" "kgfx-persona")))
          (kgfx-check "one receipt advances through only a selective row tail"
                      (and (string= "incremental-tail" (gethash "mode" report))
                           (= 1 (gethash "formation_event_count" report))
                           (= 1 (gethash "changed_node_count" report))
                           (= 1 (gethash "changed_edge_count" report))
                           (zerop (gethash "full_generation_rows_read"
                                           report -1))
                           (= 3 (gethash "through_storage_position" report)))))
        (let* ((boundary (storage-authority-boundary
                          events :agent-id "kgfx-agent"))
               (storage-id (gethash "storage_id" boundary)))
          (multiple-value-bind (restored report)
              (knowledge-graph-formation-restore
               derived "kgfx-agent" "kgfx-persona"
               :event-storage-id storage-id)
            (kgfx-check "tail generation independently warm-restores"
                        (and (= 3 (gethash "node_count" report))
                             (= 2 (gethash "edge_count" report))))
            (kgfx-check "incremental tail equals authoritative full replay"
                        (string=
                         (shasht:write-json restored nil)
                         (shasht:write-json
                          (kgfx-full-materialization events) nil)))))
        (multiple-value-bind (current ignored)
            (knowledge-graph-formation-restore
             derived "kgfx-agent" "kgfx-persona"
             :event-storage-id
             (gethash "storage_id"
                      (storage-authority-boundary
                       events :agent-id "kgfx-agent")))
          (declare (ignore ignored))
          (labels ((node-id (label)
                      (let ((row
                              (find label
                                    (coerce (gethash "nodes" current) 'list)
                                    :key (lambda (candidate)
                                           (gethash
                                            "label"
                                            (shasht:read-json
                                             (gethash "payload_json" candidate))
                                            ""))
                                    :test #'string=)))
                        (unless row
                          (error "Missing fixture node ~a among ~s" label
                                 (map 'list
                                      (lambda (candidate)
                                        (gethash
                                         "label"
                                         (shasht:read-json
                                          (gethash "payload_json" candidate))
                                         ""))
                                      (gethash "nodes" current))))
                        (gethash "node_id" row))))
            (let ((operator-id (node-id "Operator"))
                  (need-id (node-id "Accessible visuals"))
                  (preference-id (node-id "High contrast palettes")))
              (storage-append-event
               events "knowledge-graph-formation-sealed"
               (kgfx-payload
                (vector
                 (kgfx-entity "preference" "concept"
                              "Accessible high-contrast palettes"
                              :action "REVISE_EXISTING"
                              :existing preference-id))
                #() (vector preference-id))
               :agent-id "kgfx-agent")
              (let ((report (knowledge-graph-formation-synchronize
                             events derived "kgfx-agent" "kgfx-persona")))
                (kgfx-check "revision tail updates old row and adds lineage"
                            (and (= 2 (gethash "changed_node_count" report))
                                 (= 1 (gethash "changed_edge_count" report))
                                 (zerop (gethash "full_generation_rows_read"
                                                 report -1)))))
              (storage-append-event
               events "knowledge-graph-formation-sealed"
               (kgfx-payload
                (vector
                 (kgfx-entity "operator" "person" "Operator"
                              :action "LINK_EXISTING" :existing operator-id)
                 (kgfx-entity "need" "concept" "Accessible visuals"
                              :action "LINK_EXISTING" :existing need-id))
                (vector (kgfx-relation "operator" "related_to" "need"
                                       "RETIRE"))
                (vector operator-id need-id))
               :agent-id "kgfx-agent")
              (let ((report (knowledge-graph-formation-synchronize
                             events derived "kgfx-agent" "kgfx-persona")))
                (kgfx-check "retirement tail updates only its current edge"
                            (and (zerop (gethash "changed_node_count" report))
                                 (= 1 (gethash "changed_edge_count" report))
                                 (zerop (gethash "full_generation_rows_read"
                                                 report -1)))))
              (storage-append-event
               events "knowledge-graph-formation-sealed"
               (kgfx-payload
                (vector
                 (kgfx-entity "operator" "person" "Operator"
                              :action "LINK_EXISTING" :existing operator-id)
                 (kgfx-entity "need" "concept" "Accessible visuals"
                              :action "LINK_EXISTING" :existing need-id))
                (vector (kgfx-relation "operator" "related_to" "need"))
                (vector operator-id need-id))
               :agent-id "kgfx-agent")
              (let ((report (knowledge-graph-formation-synchronize
                             events derived "kgfx-agent" "kgfx-persona")))
                (kgfx-check "reassertion reopens one edge with new evidence"
                            (and (zerop (gethash "changed_node_count" report))
                                 (= 1 (gethash "changed_edge_count" report))
                                 (= 3 (gethash "edge_count" report))))))))
        (storage-append-event
         events "knowledge-graph-formation-sealed"
          (kgfx-payload (vector (kgfx-entity "a" "concept" "First queued"))
                       #())
         :agent-id "kgfx-agent")
        (storage-append-event
         events "knowledge-graph-formation-sealed"
          (kgfx-payload (vector (kgfx-entity "b" "concept" "Second queued"))
                       #())
         :agent-id "kgfx-agent")
        (let ((first (knowledge-graph-formation-synchronize
                      events derived "kgfx-agent" "kgfx-persona"))
              (second nil))
          (setf second (knowledge-graph-formation-synchronize
                        events derived "kgfx-agent" "kgfx-persona"))
          (kgfx-check "queued receipts settle one atomic event per recursion"
                      (and (= 2 (gethash "formation_events_scanned" first))
                           (= 1 (gethash "formation_event_count" first))
                           (= 7 (gethash "through_storage_position" first))
                           (= 8 (gethash "through_storage_position" second)))))
        (multiple-value-bind (restored ignored)
            (knowledge-graph-formation-restore
             derived "kgfx-agent" "kgfx-persona"
             :event-storage-id
             (gethash "storage_id"
                      (storage-authority-boundary
                       events :agent-id "kgfx-agent")))
          (declare (ignore ignored))
          (kgfx-check "revision retirement reopening and queue remain replay exact"
                      (string=
                       (shasht:write-json restored nil)
                       (shasht:write-json
                        (kgfx-full-materialization events) nil))))
        (storage-append-event events "operator-note" (obj "text" "advance")
                              :agent-id "kgfx-agent")
        (let ((report (knowledge-graph-formation-synchronize
                       events derived "kgfx-agent" "kgfx-persona")))
          (kgfx-check "irrelevant tail advances without graph row changes"
                      (and (string= "incremental-tail" (gethash "mode" report))
                           (zerop (gethash "formation_event_count" report))
                           (zerop (gethash "changed_node_count" report))
                           (= 9 (gethash "through_storage_position" report)))))
        (%sqlite-exec
         (%sqlite-derived-handle derived :kgfx-corrupt)
         "UPDATE pai_projection_checkpoints SET state_json='{}' WHERE projection_name='grounded-knowledge-graph' AND agent_id='kgfx-agent'"
         :kgfx-corrupt)
        (let ((report (knowledge-graph-formation-synchronize
                       events derived "kgfx-agent" "kgfx-persona")))
          (kgfx-check "corrupt generation falls back to bounded cold rebuild"
                      (and (string= "cold-rebuild" (gethash "mode" report))
                           (stringp (gethash "fallback_reason" report))
                           (= 6 (gethash "node_count" report))))))
    (when events (ignore-errors (storage-close events)))
    (when derived (ignore-errors (storage-close derived)))
    (kgfx-delete-db event-db)
    (kgfx-delete-db derived-db)))

(format t "~%KG2 sync: ~d passed, ~d failed.~%" *kgfx-pass* *kgfx-fail*)
(when (plusp *kgfx-fail*) (uiop:quit 1))
