;;;; knowledge-graph-formation-storage-tests.lisp -- KG2 derived owner fixtures.

(in-package :agent)

(ql:quickload '(:cffi :shasht :ironclad :bordeaux-threads) :silent t)

(dolist (file '("storage-substrate.lisp" "memory-storage.lisp"
                "sqlite-storage.lisp" "sqlite-derived-storage.lisp"
                "conversation-episode-graph.lisp"
                "conversation-episode-graph-storage.lisp"
                "knowledge-graph-ontology.lisp"
                "knowledge-graph-formation.lisp"
                "knowledge-graph-formation-storage.lisp"))
  (load (test-source file)))

(defvar *kgfs-pass* 0)
(defvar *kgfs-fail* 0)

(defun kgfs-check (name condition)
  (if condition
      (progn (incf *kgfs-pass*) (format t "PASS ~a~%" name))
      (progn (incf *kgfs-fail*) (format t "FAIL ~a~%" name))))

(defun kgfs-signals-p (type thunk)
  (handler-case (progn (funcall thunk) nil)
    (condition (actual) (typep actual type))))

(defun kgfs-delete-db (path)
  (dolist (candidate
           (list path
                 (pathname (concatenate 'string (namestring path) "-wal"))
                 (pathname (concatenate 'string (namestring path) "-shm"))))
    (when (probe-file candidate) (delete-file candidate))))

(defun kgfs-event (id)
  (obj "id" id "type" "knowledge-graph-formation-sealed"
       "agent_id" "kg2-agent" "timestamp" (+ 4000000000 id)
       "payload"
       (obj "schema_version" 1 "persona_id" "kg2-persona"
            "disclosure_class" "private"
            "formation_revision" *knowledge-graph-formation-revision*
            "source_event_ids" (vector (+ id 100))
            "source_memory_node_ids" (vector (format nil "memory:~d" id))
            "source_episode_ids" (vector (format nil "episode:~d" id))
            "source_evidence"
            (vector (obj "source_id" "event:fixture" "speaker_id" "operator"
                         "kind" "original-utterance" "timestamp" 4000000000
                         "text" "The operator requires color-accessible artifacts."
                         "text_sha256"
                         (%kgf-sha256
                          "The operator requires color-accessible artifacts.")))
            "eligible_existing_node_ids" #()
            "proposal"
            (obj "schema_version" 3
                 "ontology_revision" *knowledge-graph-ontology-revision*
                 "entities"
                 (vector
                  (obj "local_ref" "operator" "kind" "person"
                       "label" "Operator" "aliases" #()
                       "classifications" #("operator")
                       "identity_action" "NEW" "existing_node_id" :null
                       "evidence_status" "direct" "evidence_note" "fixture")
                  (obj "local_ref" "requirement" "kind" "concept"
                       "label" "Color-accessible artifacts" "aliases" #()
                       "classifications" #("accessibility")
                       "identity_action" "NEW" "existing_node_id" :null
                       "evidence_status" "direct" "evidence_note" "fixture"))
                 "relationships"
                 (vector
                  (obj "subject_ref" "operator" "predicate" "related_to"
                       "object_ref" "requirement"
                       "relationship_action" "ASSERT"
                       "fact" "The operator requires color-accessible artifacts."
                       "grounding"
                       (obj "schema_version" 1 "scope" "assertion"
                            "polarity" "positive" "attributed_to_ref" :null
                            "evidence"
                            (vector (obj "source_id" "event:fixture"
                                         "quote" "The operator requires color-accessible artifacts.")))
                       "temporal"
                       (obj "schema_version" 1 "character" "standing-disposition"
                            "occurred_at" :null "valid_from" :null
                            "valid_until" :null)
                       "evidence_status" "direct" "evidence_note" "fixture"))))))

(format t "~%== KG2 generic graph storage ==~%")

(let* ((database #p"/agent/state/kg2-graph.sqlite3")
       (backend nil)
       (state (knowledge-graph-formation-project
               (list (kgfs-event 10)) "kg2-agent" "kg2-persona"))
       (materialization (knowledge-graph-formation-materialization state)))
  (kgfs-delete-db database)
  (unwind-protect
      (progn
        (setf backend (make-sqlite-derived-storage database))
        (let ((report
                (knowledge-graph-formation-persist
                 backend materialization
                 :through-event-id 10 :through-position 20
                 :event-storage-id "kg2-fixture-ledger"
                 :boundary-hash "kg2-fixture-boundary")))
          (kgfs-check "atomic persistence reports content-free counts"
                      (and (string= "persisted" (gethash "status" report))
                           (= 2 (gethash "node_count" report))
                           (= 1 (gethash "edge_count" report))
                           (= 0 (gethash "event_write_count" report -1))
                           (= 0 (gethash "memory_write_count" report -1)))))
        (multiple-value-bind (restored report)
            (knowledge-graph-formation-restore
             backend "kg2-agent" "kg2-persona"
             :event-storage-id "kg2-fixture-ledger")
          (kgfs-check "warm restore returns exact canonical materialization"
                      (string= (shasht:write-json materialization nil)
                               (shasht:write-json restored nil)))
          (kgfs-check "warm restore verifies checkpoint and digest"
                      (and (string= "restored" (gethash "status" report))
                           (= 20 (gethash "through_storage_position" report))
                           (= 0 (gethash "database_write_count" report -1)))))
        (let ((candidates
                (knowledge-graph-formation-current-candidates
                 backend "kg2-agent" "kg2-persona"
                 #( "color-accessible" "operator")
                 :event-storage-id "kg2-fixture-ledger")))
          (kgfs-check "bounded lexical lookup returns verified current candidates"
                      (and (= 2 (length candidates))
                           (every (lambda (row)
                                    (and (stringp (gethash "node_id" row))
                                         (stringp (gethash "kind" row))
                                         (stringp (gethash "label" row))
                                         (vectorp (gethash "aliases" row))
                                         (vectorp
                                          (gethash "classifications" row))
                                         (nth-value
                                          1 (gethash "participant_role" row))))
                                  candidates))))
        (let ((candidates
                (knowledge-graph-formation-current-candidates
                 backend "kg2-agent" "kg2-persona" #()
                 :event-storage-id "kg2-fixture-ledger")))
          (kgfs-check "known participants remain candidates without lexical cues"
                      (and (= 1 (length candidates))
                           (string= "operator"
                                    (gethash "participant_role"
                                             (aref candidates 0))))))
        (let ((candidates
                (knowledge-graph-formation-current-candidates
                 backend "kg2-agent" "kg2-persona"
                 #( "accessibility" "operator")
                 :event-storage-id "kg2-fixture-ledger" :maximum 1)))
          (kgfs-check
           "exact label outranks a broad classification before candidate bound"
           (and (= 1 (length candidates))
                (string= "Operator"
                         (gethash "label" (aref candidates 0))))))
        (kgfs-check "candidate lookup refuses a foreign generation binding"
                    (kgfs-signals-p
                     'storage-integrity-error
                     (lambda ()
                       (knowledge-graph-formation-current-candidates
                        backend "kg2-agent" "kg2-persona" #( "operator")
                        :event-storage-id "foreign-ledger"))))
        (let ((before
                (knowledge-graph-formation-inspect
                 backend "kg2-agent" "kg2-persona"
                 :event-storage-id "kg2-fixture-ledger")))
          (kgfs-check
           "backward checkpoint fails without replacing persisted rows"
           (and
            (kgfs-signals-p
             'storage-conflict-error
             (lambda ()
               (knowledge-graph-formation-persist
                backend materialization
                :through-event-id 9 :through-position 19
                :event-storage-id "kg2-fixture-ledger"
                :boundary-hash "older")))
            (string= (gethash "graph_digest" before)
                     (gethash
                      "graph_digest"
                      (knowledge-graph-formation-inspect
                       backend "kg2-agent" "kg2-persona"
                       :event-storage-id "kg2-fixture-ledger"))))))
        (%sqlite-exec
         (%sqlite-derived-handle backend :kg2-fixture-corrupt)
         "UPDATE pai_knowledge_graph_nodes SET payload_json='{}' WHERE rowid=(SELECT rowid FROM pai_knowledge_graph_nodes WHERE projection_name='grounded-knowledge-graph' ORDER BY rowid LIMIT 1)"
         :kg2-fixture-corrupt)
        (kgfs-check "row corruption fails warm restore closed"
                    (kgfs-signals-p
                     'storage-integrity-error
                     (lambda ()
                       (knowledge-graph-formation-restore
                        backend "kg2-agent" "kg2-persona"
                        :event-storage-id "kg2-fixture-ledger")))))
    (when backend (ignore-errors (storage-close backend)))
    (kgfs-delete-db database)))

(format t "~%KG2 storage: ~d passed, ~d failed.~%"
        *kgfs-pass* *kgfs-fail*)
(when (plusp *kgfs-fail*) (uiop:quit 1))
