;;;; knowledge-graph-search-storage-tests.lisp -- disposable KG3 derived reads.
;;;; harness: full-system

(in-package :agent)

(defvar *kgss-pass* 0)
(defvar *kgss-fail* 0)

(defun kgss-check (name condition)
  (if condition
      (progn (incf *kgss-pass*) (format t "PASS ~a~%" name))
      (progn (incf *kgss-fail*) (format t "FAIL ~a~%" name))))

(defun kgss-signals-p (type thunk)
  (handler-case (progn (funcall thunk) nil)
    (condition (actual) (typep actual type))))

(defun kgss-delete-db (path)
  (dolist (candidate
           (list path
                 (pathname (concatenate 'string (namestring path) "-wal"))
                 (pathname (concatenate 'string (namestring path) "-shm"))))
    (when (probe-file candidate) (delete-file candidate))))

(defun kgss-count (handle table-name)
  (let ((rows (%cegs-read-rows
               handle (format nil "SELECT COUNT(*) AS row_count FROM ~a"
                              table-name)
               '("row_count") '("row_count") :kg3-count)))
    (gethash "row_count" (aref rows 0))))

(defun kgss-episode-event ()
  (obj "id" 20 "type" "conversation-episode-sealed"
       "agent_id" "kg3-agent" "timestamp" "2026-08-31T12:00:00Z"
       "payload"
       (obj "schema_version" 1 "episode_id" "episode:color"
            "persona_id" "kg3-persona" "first_event_id" 10
            "last_event_id" 11 "first_timestamp" 10 "last_timestamp" 11
            "source_event_ids" #(10 11)
            "synopsis" "Operator discussed color-accessible artifacts."
            "subjects" #() "entities" #()
            "retrieval_cues" #( "color accessibility" "artifact design")
            "broader_categories" #( "operator requirements")
            "unresolved_threads" #())))

(defun kgss-formation-event ()
  (obj "id" 30 "type" "knowledge-graph-formation-sealed"
       "agent_id" "kg3-agent" "timestamp" 4000000030
       "payload"
       (obj "schema_version" 1 "persona_id" "kg3-persona"
            "disclosure_class" "private"
            "formation_revision" *knowledge-graph-formation-revision*
            "source_event_ids" #(10 11)
            "source_memory_node_ids" #( "memory:color")
            "source_episode_ids" #( "episode:color")
            "source_evidence"
            (vector (obj "source_id" "migrated-event:source-agent:10"
                         "speaker_id" "operator"
                         "kind" "original-utterance"
                         "timestamp" "2026-08-31T12:00:00Z"
                         "text" "I have red-green color vision deficiency."
                         "text_sha256"
                         (%kgf-sha256
                          "I have red-green color vision deficiency.")))
            "eligible_existing_node_ids" #()
            "proposal"
            (obj "schema_version" 3
                 "ontology_revision" *knowledge-graph-ontology-revision*
                 "entities"
                 (vector
                  (obj "local_ref" "operator" "kind" "person"
                       "label" "Operator" "aliases" #() "classifications" #()
                       "identity_action" "NEW" "existing_node_id" :null
                       "evidence_status" "direct"
                       "evidence_note" "operator is named in evidence")
                  (obj "local_ref" "requirement" "kind" "condition"
                       "label" "Red-green color vision deficiency" "aliases" #()
                       "classifications" #("color-vision")
                       "identity_action" "NEW" "existing_node_id" :null
                       "evidence_status" "direct"
                       "evidence_note" "condition is stated in evidence")
                  (obj "local_ref" "hypothesis" "kind" "concept"
                       "label" "Speculative presentation preference"
                       "aliases" #() "classifications" #()
                       "identity_action" "NEW"
                       "existing_node_id" :null
                       "evidence_status" "inference"
                       "evidence_note" "plausible but not stated"))
                 "relationships"
                 (vector
                  (obj "subject_ref" "operator" "predicate" "has_condition"
                       "object_ref" "requirement"
                       "relationship_action" "ASSERT"
                       "fact" "The operator has red-green color vision deficiency."
                       "grounding"
                       (obj "schema_version" 1 "scope" "assertion"
                            "polarity" "positive" "attributed_to_ref" :null
                            "evidence"
                            (vector (obj "source_id" "migrated-event:source-agent:10"
                                         "quote" "I have red-green color vision deficiency.")))
                       "temporal"
                       (obj "schema_version" 1 "character" "standing-disposition"
                            "occurred_at" :null "valid_from" :null
                            "valid_until" :null)
                       "evidence_status" "direct"
                       "evidence_note" "condition is attributed to operator")
                  (obj "subject_ref" "operator" "predicate" "proposed"
                       "object_ref" "hypothesis"
                       "relationship_action" "ASSERT"
                       "fact" "The operator proposed a presentation preference."
                       "grounding"
                       (obj "schema_version" 1 "scope" "hypothesis"
                            "polarity" "unknown" "attributed_to_ref" :null
                            "evidence"
                            (vector (obj "source_id" "migrated-event:source-agent:10"
                                         "quote" "I have red-green color vision deficiency.")))
                       "temporal"
                       (obj "schema_version" 1 "character" "unspecified"
                            "occurred_at" :null "valid_from" :null
                            "valid_until" :null)
                       "evidence_status" "inference"
                       "evidence_note" "plausible but not stated"))))))

(format t "~%== KG3 verified graph storage search ==~%")

(defun kgss-noise-events ()
  (loop for index below 24
        for event = (kgss-formation-event)
        for payload = (gethash "payload" event)
        for proposal = (gethash "proposal" payload)
        for node = (aref (gethash "entities" proposal) 1)
        do (setf (gethash "id" event) (+ 31 index)
                 (gethash "label" node) (format nil "Color runtime issue ~d" index)
                 (gethash "evidence_note" node) "A technical issue, not a personal requirement."
                 (gethash "entities" proposal) (vector node)
                 (gethash "relationships" proposal) #())
        collect event))

(let* ((directory #p"/tmp/pai-kg3-tests/")
       (database (merge-pathnames "graph-search.sqlite3" directory))
       (backend nil)
       (episode-state
         (conversation-episode-project
          (list (kgss-episode-event)) "kg3-agent" "kg3-persona"))
       (episode-materialization
         (conversation-episode-graph-materialization
          episode-state "kg3-agent" "kg3-persona"))
       (formation-state
         (knowledge-graph-formation-project
          (cons (kgss-formation-event) (kgss-noise-events)) "kg3-agent" "kg3-persona"))
       (formation-materialization
         (knowledge-graph-formation-materialization formation-state)))
  (ensure-directories-exist database)
  (kgss-delete-db database)
  (unwind-protect
      (progn
        (setf backend (make-sqlite-derived-storage database))
        (conversation-episode-graph-persist
         backend episode-materialization :through-event-id 20
         :through-position 40 :event-storage-id "kg3-ledger"
         :boundary-hash "kg1-boundary")
        (knowledge-graph-formation-persist
         backend formation-materialization :through-event-id 54
         :through-position 64 :event-storage-id "kg3-ledger"
         :boundary-hash "kg2-boundary")
        (let* ((handle (%sqlite-derived-handle backend :kg3-count))
               (before-nodes
                 (kgss-count handle "pai_knowledge_graph_nodes"))
               (before-edges
                 (kgss-count handle "pai_knowledge_graph_edges"))
               (result
                 (knowledge-graph-search-storage
                  backend "kg3-agent" "kg3-persona"
                  (knowledge-graph-search-request
                   :query "color requirement" :maximum-depth 1)
                  :event-storage-id "kg3-ledger")))
          (kgss-check "ordinary search excludes the legacy episode concept graph"
                      (and (string= "available" (gethash "status" result))
                           (= 1 (length (gethash "searched_projections" result)))
                           (notany
                            (lambda (path)
                              (string= "conversation-episode-graph"
                                       (gethash "projection_name" path "")))
                            (gethash "paths" result))
                           (find "grounded-knowledge-graph"
                                 (coerce (gethash "paths" result) 'list)
                                 :test #'string=
                                 :key (lambda (path)
                                        (gethash "projection_name" path "")))))
          (kgss-check "paths expose source evidence and generation watermarks"
                      (and
                       (= 1 (length (gethash "generation_watermarks" result)))
                       (every
                        (lambda (path)
                          (vectorp
                           (gethash "evidence_event_ids"
                                    (aref (gethash "nodes" path) 0))))
                        (gethash "paths" result))))
          (kgss-check "verified graph search performs zero derived writes"
                      (and (= 0 (gethash "database_write_count" result -1))
                           (= before-nodes
                              (kgss-count handle
                                          "pai_knowledge_graph_nodes"))
                           (= before-edges
                              (kgss-count handle
                              "pai_knowledge_graph_edges")))))
        (let* ((handle (%sqlite-derived-handle backend :rank-test))
               (*knowledge-graph-search-maximum-seeds* 1)
               (rows (%kgss-read-query-nodes handle "grounded-knowledge-graph"
                        "kg3-agent" "kg3-persona" "color vision" "verified")))
          (kgss-check "SQL ranks semantic fields before its bounded candidate limit"
            (and (plusp (length rows))
                 (search "Red-green color vision" (gethash "payload_json" (aref rows 0)))))
          (kgss-check "JSON metadata keys are not lexical graph evidence"
            (zerop (length (%kgss-read-query-nodes handle "grounded-knowledge-graph"
                        "kg3-agent" "kg3-persona" "node_id" "all")))))
        (let* ((result
                 (knowledge-graph-search-storage
                  backend "kg3-agent" "kg3-persona"
                  (knowledge-graph-search-request :query "color-vision")
                  :event-storage-id "kg3-ledger"))
               (nodes
                 (loop for path across (gethash "paths" result)
                       append (coerce (gethash "nodes" path) 'list)))
               (condition
                 (find "Red-green color vision deficiency" nodes
                       :test #'string=
                       :key (lambda (node) (gethash "label" node "")))))
          (kgss-check "classifications are searchable and visible to traversal"
                      (and condition
                           (plusp (gethash "query_match_count" result))
                           (find "Red-green color vision deficiency"
                                 (gethash "query_match_nodes" result)
                                 :test #'string=
                                 :key (lambda (node)
                                        (gethash "label" node "")))
                           (find "color-vision"
                                 (gethash "classifications" condition)
                                 :test #'string=))))
        (let* ((result (knowledge-graph-search-storage backend "kg3-agent" "kg3-persona"
                         (knowledge-graph-search-request :query "color accessibility")
                         :event-storage-id "kg3-ledger"))
               (episode (loop for path across (gethash "paths" result)
                          thereis (find "episode" (gethash "nodes" path)
                                    :key (lambda (node) (gethash "node_kind" node))
                                    :test #'string=))))
          (kgss-check "episodic summaries stay on the separate retrieval surface"
                      (null episode)))
        (let ((verified
                (knowledge-graph-search-storage
                 backend "kg3-agent" "kg3-persona"
                 (knowledge-graph-search-request
                  :query "speculative presentation preference"
                  :maximum-depth 1)
                 :event-storage-id "kg3-ledger"))
              (inferred
                (knowledge-graph-search-storage
                 backend "kg3-agent" "kg3-persona"
                 (knowledge-graph-search-request
                  :query "speculative presentation preference"
                  :maximum-depth 1 :evidence-policy "inferred")
                 :event-storage-id "kg3-ledger"))
              (hypothesis
                (knowledge-graph-search-storage
                 backend "kg3-agent" "kg3-persona"
                 (knowledge-graph-search-request
                  :query "speculative presentation preference"
                  :maximum-depth 1 :evidence-policy "all")
                 :event-storage-id "kg3-ledger")))
          (kgss-check "verified default excludes model inference"
                      (notany
                       (lambda (path)
                         (find "inference" (gethash "nodes" path)
                               :test #'string=
                               :key (lambda (node)
                                      (gethash "evidence_status" node ""))))
                       (gethash "paths" verified)))
          (kgss-check "explicit inferred policy exposes reviewed inference"
                      (find-if
                       (lambda (path)
                         (find "inference" (gethash "nodes" path)
                               :test #'string=
                               :key (lambda (node)
                                      (gethash "evidence_status" node ""))))
                       (gethash "paths" inferred)))
          (kgss-check "explicit all policy exposes labeled inference"
                      (find-if
                       (lambda (path)
                         (find "inference" (gethash "nodes" path)
                               :test #'string=
                               :key (lambda (node)
                                      (gethash "evidence_status" node ""))))
                       (gethash "paths" hypothesis))))
        (let* ((operator
                 (find "person" (coerce (gethash "nodes" formation-state) 'list)
                       :test #'string=
                       :key (lambda (row) (gethash "node_kind" row ""))))
               (result
                 (knowledge-graph-search-storage
                  backend "kg3-agent" "kg3-persona"
                  (knowledge-graph-search-request
                   :starting-node-id (gethash "node_id" operator) :query :null
                   :predicates #( "has_condition") :direction "outgoing"
                   :maximum-depth 1)
                  :event-storage-id "kg3-ledger")))
          (kgss-check "exact start, predicate and direction traverse the KG2 edge"
                      (and (= 2 (gethash "path_count" result))
                           (string= "has_condition"
                                    (gethash
                                     "predicate"
                                     (aref
                                      (gethash
                                       "edges"
                                       (aref (gethash "paths" result) 1))
                                     0)))
                           (find "source-agent"
                                 (gethash
                                  "origin_agent_ids"
                                  (aref
                                   (gethash
                                    "edges"
                                    (aref (gethash "paths" result) 1))
                                   0)
                                  #())
                                 :test #'string=))))
        (let ((result
                (knowledge-graph-search-storage
                 backend "kg3-agent" "kg3-persona"
                 (knowledge-graph-search-request
                  :starting-node-id "FixtureOperator" :query "color requirement"
                  :maximum-depth 1)
                 :event-storage-id "kg3-ledger")))
          (kgss-check "unknown start with query falls back to lexical seeds"
                      (and (string= "unresolved-query-fallback"
                                    (gethash "starting_node_status" result))
                           (plusp (gethash "path_count" result))
                           (string= "FixtureOperator"
                                    (gethash "unresolved_starting_node_id"
                                             result)))))
        (let ((result
                (knowledge-graph-search-storage
                 backend "kg3-agent" "kg3-persona"
                 (knowledge-graph-search-request
                  :starting-node-id "FixtureOperator" :query :null
                  :maximum-depth 1)
                 :event-storage-id "kg3-ledger")))
          (kgss-check "unknown exact-only start returns typed empty evidence"
                      (and (string= "unresolved"
                                    (gethash "starting_node_status" result))
                           (string= "empty" (gethash "status" result))
                           (zerop (gethash "path_count" result)))))
        (let ((result
                (knowledge-graph-search-storage
                 backend "kg3-agent" "kg3-persona"
                 (knowledge-graph-search-request
                  :query "zzzxxyyqq" :maximum-depth 0)
                 :event-storage-id "kg3-ledger"
                 :linked-source-ids #( "memory:color" "episode:color")
                 :linked-evidence-event-ids #(10))))
          (kgss-check "exact KG4 memory, episode and evidence links seed graph rows"
                      (and (plusp (gethash "hybrid_link_seed_count" result))
                           (plusp (gethash "path_count" result))
                           (every (lambda (path)
                                    (= 0 (gethash "depth" path)))
                                  (gethash "paths" result)))))
        (let ((result
                (knowledge-graph-search-storage
                 backend "kg3-agent" "kg3-persona"
                 (knowledge-graph-search-request
                  :query "zzzxxyyqq" :maximum-depth 0)
                 :event-storage-id "kg3-ledger"
                 :linked-source-ids #( "memory:not-present")
                 :linked-evidence-event-ids #(999999))))
          (kgss-check "unrelated hybrid candidates do not manufacture graph paths"
                      (and (= 0 (gethash "hybrid_link_seed_count" result))
                           (= 0 (gethash "path_count" result)))))
        (kgss-check "foreign event authority binding fails closed"
                    (kgss-signals-p
                     'storage-integrity-error
                     (lambda ()
                       (knowledge-graph-search-storage
                        backend "kg3-agent" "kg3-persona"
                        (knowledge-graph-search-request :query "color")
                        :event-storage-id "foreign-ledger")))))
    (when backend (ignore-errors (storage-close backend)))
    (kgss-delete-db database)))

(format t "~%KG3 storage search: ~d passed, ~d failed.~%"
        *kgss-pass* *kgss-fail*)
(when (plusp *kgss-fail*) (uiop:quit 1))
