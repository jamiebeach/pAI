;;;; harness: full-system
(in-package :agent)

(dolist (type '("context-graph-identity-opened" "context-graph-identity-phase"
                "context-graph-identity-completed" "context-graph-identity-failed"))
  (assert (member type *conscious-context-graph-journal-event-types*
                  :test #'equal))
  (assert (not (member type *conscious-recursive-thread-event-types*
                       :test #'equal))))

(assert (not (knowledge-graph-ontology-kind-p "attribute_value")))
(assert (knowledge-graph-ontology-kind-p
         "attribute_value" *knowledge-graph-family-ontology-revision*))
(assert (knowledge-graph-ontology-signature-valid-p
         "parent_of" "person" "person"
         *knowledge-graph-family-ontology-revision*))
(assert (knowledge-graph-ontology-signature-valid-p
         "has_age" "person" "attribute_value"
         *knowledge-graph-family-ontology-revision*))
(assert (not (knowledge-graph-ontology-signature-valid-p
              "has_age" "attribute_value" "person"
              *knowledge-graph-family-ontology-revision*)))

;; The checkpoint is metadata; graph and owner values are independently
;; integrity-checked rows. Settled source packets do not survive restoration.
(let* ((path (merge-pathnames "reviewed-context-graph.sqlite3" (test-state-dir)))
       (backend nil) (agent-id "row-agent") (persona-id "row-persona")
       (runtime (pai.context-graph:context-graph-runtime-create
                 (%ccg-ontology) (%ccg-runtime-ontology-revision)
                 agent-id persona-id))
       (graph (pai.context-graph::context-graph-runtime-graph runtime))
       (owner (%ccg-create-owner graph agent-id persona-id))
       (opening
         (obj "id" 10 "agent_id" agent-id
              "type" "context-graph-identity-opened" "timestamp" 10
              "caused_by" 3 "payload"
              (obj "persona_id" persona-id
                   "generation" *conscious-context-graph-owner-generation*
                   "record_json"
                   (pai.context-graph:context-graph-runtime-json
                    (obj "episode_event_id" 3 "batch_index" 0
                         "observed_at" 10
                         "source_context"
                         (obj "private" (make-string 10000 :initial-element #\x))
                         "ontology_revision" (%ccg-runtime-ontology-revision)
                         "formation_protocol" *conscious-context-graph-formation-protocol*
                         "fact_input_revision" "selected-signatures-v4"
                         "budget_microusd" 1000 "request_ceiling_microusd" 500
                         "attempt" 1 "retry_of" :null)))))
       (terminal
         (obj "id" 11 "agent_id" agent-id
              "type" "context-graph-identity-failed" "timestamp" 11
              "caused_by" 10 "payload"
              (obj "persona_id" persona-id
                   "generation" *conscious-context-graph-owner-generation*
                   "record_json"
                   (pai.context-graph:context-graph-runtime-json
                    (obj "reason" "provider" "failure_class" "provider"
                         "retryable" :false "attempt" 1 "failed_at" 11
                         "next_retry_at" :null))))))
  (labels ((clean ()
             (dolist (candidate
                      (list path
                            (pathname (concatenate 'string (namestring path) "-wal"))
                            (pathname (concatenate 'string (namestring path) "-shm"))))
               (when (probe-file candidate) (delete-file candidate)))))
    (clean)
    (unwind-protect
         (progn
           (setf backend (make-sqlite-derived-storage path)
                 (gethash 10 (pai.context-graph::cgi-owner-opens owner)) opening
                 (gethash 10 (pai.context-graph::cgi-owner-terminals owner)) terminal
                 (gethash '(3 0) (pai.context-graph::cgi-owner-tasks owner)) 10
                 (gethash '(10 "facts") (pai.context-graph::cgi-owner-phases owner))
                 (obj "outcome" "response" "charged_microusd" 17)
                 (pai.context-graph::cgi-owner-last-id owner) 11
                 (gethash "entity:fixture"
                          (pai.context-graph::context-graph-entities graph))
                 (obj "entity_id" "entity:fixture" "node_id" "entity:fixture"
                      "kind" "concept" "label" "Fixture"
                      "aliases" #("Fixture Alias")
                      "classifications" #() "participant_role" :null
                      "status" "current")
                 (gethash "entity:fixture"
                          (pai.context-graph::context-graph-entity-adjacency
                           graph))
                 #("fact:fixture"))
           (let ((first
                   (reviewed-context-graph-persist
                    backend graph owner :through-event-id 20
                    :through-position 30 :event-storage-id "fixture-ledger"
                    :boundary-hash "fixture-binding")))
             (assert (string= "persisted" (gethash "status" first)))
             (assert (plusp (gethash "written_record_count" first)))
             (assert (= 2 (gethash "alias_count" first)))
             (assert (= 2 (gethash "written_alias_count" first)))
             (assert (= 1 (gethash "adjacency_count" first)))
             (assert (= 1 (gethash "written_adjacency_count" first))))
           ;; Advancing only the source watermark updates the tiny checkpoint;
           ;; identical graph/index rows perform no database writes.
           (let ((unchanged
                   (reviewed-context-graph-persist
                    backend graph owner :through-event-id 21
                    :through-position 31 :event-storage-id "fixture-ledger"
                    :boundary-hash "fixture-binding-2")))
             (assert (zerop (gethash "written_record_count" unchanged)))
             (assert (zerop (gethash "deleted_record_count" unchanged)))
             (assert (zerop (gethash "written_alias_count" unchanged)))
             (assert (zerop (gethash "deleted_alias_count" unchanged)))
             (assert (zerop (gethash "written_adjacency_count" unchanged)))
             (assert (zerop (gethash "deleted_adjacency_count" unchanged))))
           ;; A changed entity and adjacency update only their rows and index
           ;; set differences; a removed owner row is deleted explicitly.
           (setf (gethash "aliases"
                          (gethash
                           "entity:fixture"
                           (pai.context-graph::context-graph-entities graph)))
                 #("Changed Alias")
                 (gethash "entity:fixture"
                          (pai.context-graph::context-graph-entity-adjacency
                           graph))
                 #())
           (remhash '(3 0) (pai.context-graph::cgi-owner-tasks owner))
           (let ((changed
                   (reviewed-context-graph-persist
                    backend graph owner :through-event-id 22
                    :through-position 32 :event-storage-id "fixture-ledger"
                    :boundary-hash "fixture-binding-3")))
             (assert (= 2 (gethash "written_record_count" changed)))
             (assert (= 1 (gethash "deleted_record_count" changed)))
             (assert (= 1 (gethash "written_alias_count" changed)))
             (assert (= 1 (gethash "deleted_alias_count" changed)))
             (assert (zerop (gethash "written_adjacency_count" changed)))
             (assert (= 1 (gethash "deleted_adjacency_count" changed))))
           (multiple-value-bind (restored restored-owner checkpoint exposure)
               (reviewed-context-graph-restore
                backend agent-id persona-id "fixture-ledger"
                "fixture-binding-3")
             (assert restored)
             (assert (= 32 (gethash "through_storage_position" checkpoint)))
             (assert (= 17 exposure))
             (assert (gethash "entity:fixture"
                              (pai.context-graph::context-graph-entities restored)))
             (assert
              (not (nth-value
                    1 (gethash
                       "source_context"
                       (pai.context-graph::%cgro-record
                        (gethash 10 (pai.context-graph::cgi-owner-opens
                                     restored-owner)))))))))
      (when backend (ignore-errors (storage-close backend)))
      (clean))))


;; No graph journals is a valid projection, not an incomplete rebuild. Exercise
;; the actual authority and recursive cache, and prove warm/cold reads reuse it.
(let* ((database (merge-pathnames "empty-graph-events.sqlite3" (test-state-dir)))
       (derived (merge-pathnames "empty-graph-derived.sqlite3" (test-state-dir)))
       (*agent-id* "empty-graph-agent")
       (persona-id "empty-graph-persona")
       (*conscious-context-graph-runtime* nil)
       (*conscious-context-graph-formation-owner* nil)
       (*conscious-context-graph-runtime-key* nil)
       (*conscious-context-graph-journal-position* nil)
       (*conscious-context-graph-maintenance-replay-p* t)
       (*conscious-recursive-thread-events-cache-key* nil)
       (original-persist (symbol-function 'reviewed-context-graph-persist))
       (writes 0))
  (unwind-protect
       (progn
         (sqlite-event-authority-prepare
          database nil :derived-database derived :agent-id *agent-id*
          :initialize-p t)
         (log-event "heap-health" (obj "status" "synthetic-empty-graph"))
         (setf (symbol-function 'reviewed-context-graph-persist)
               (lambda (&rest arguments)
                 (incf writes)
                 (apply original-persist arguments)))
         (let* ((backend *sqlite-event-authority-backend*)
                (checkpoint-backend *sqlite-event-authority-checkpoint-backend*)
                (head (storage-head-position backend :agent-id *agent-id*)))
           (%ccg-sync backend *agent-id* persona-id checkpoint-backend)
           (let ((checkpoint
                   (storage-load-checkpoint
                    checkpoint-backend *reviewed-context-graph-projection-name*
                    :agent-id *agent-id*)))
             (assert checkpoint)
             (assert (= head (gethash "through_storage_position" checkpoint))))
           (assert (= 1 writes))
           (%ccg-sync backend *agent-id* persona-id checkpoint-backend)
           (assert (= 1 writes))
           ;; A fresh generation must restore the source-bound empty graph.
           (setf *conscious-context-graph-runtime-key* nil
                 *conscious-context-graph-runtime* nil
                 *conscious-context-graph-formation-owner* nil)
           (%ccg-sync backend *agent-id* persona-id checkpoint-backend)
           (assert (= 1 writes))
           (assert (= head (storage-head-position backend :agent-id *agent-id*)))))
    (setf (symbol-function 'reviewed-context-graph-persist) original-persist)
    (when *event-authority-port* (event-authority-clear))))

(let ((profile-path (merge-pathnames "graph-provider-profiles.json"
                                     (test-state-dir))))
  (with-open-file (stream profile-path :direction :output
                          :if-exists :supersede :if-does-not-exist :create)
    (shasht:write-json
     (obj "experiment_routing" (obj "graph_profile" "fixture-graph")
          "profiles"
          (obj "fixture-graph"
               (obj "provider" "openrouter"
                    "endpoint" "https://openrouter.ai/api/v1/chat/completions"
                    "model" "fixture/graph-model"
                    "publication_role" "proposal-only"
                    "allowed_purposes" #("knowledge-graph-formation")
                    "requires_native_tool_calls" t
                    "provider_routing"
                    (obj "zdr" t "data_collection" "deny"
                         "only" #("fixture-provider")
                         "allow_fallbacks" nil
                         "max_price_usd_per_million"
                         (obj "prompt" 0.33d0 "completion" 1.21d0)))))
     stream))
  ;; Later end-to-end cases invoke the same provider selector. This process is
  ;; isolated, so retain the fixture document for the full suite lifetime.
  (setf *conscious-context-graph-provider-profiles-path* profile-path)
  (let* ((conversation *conscious-conversation-provider-profile*)
         (profile (%ccg-provider-profile)))
    (let* ((*conscious-conversation-provider-profile* profile)
           (policy (%conversation-openrouter-provider-policy)))
      (assert (equal "fixture/graph-model" (gethash "model" profile)))
      (assert (equalp #("fixture-provider") (gethash "only" policy)))
      (assert (eq t (gethash "zdr" policy)))
      (assert (equal "deny" (gethash "data_collection" policy)))
      (assert (nth-value 1 (gethash "allow_fallbacks" policy)))
      (assert (null (gethash "allow_fallbacks" policy))))
    (assert (eq conversation *conscious-conversation-provider-profile*))))

(let* ((agent-id "migration-destination-agent")
       (persona-id "migration-destination-persona")
       (index (make-hash-table :test #'eql))
       (ontology (%ccg-ontology))
       (runtime (pai.context-graph:context-graph-runtime-create
                 ontology *knowledge-graph-ontology-revision*
                 agent-id persona-id))
       (graph (pai.context-graph::context-graph-runtime-graph runtime)))
  (labels ((historical (id source-id type text &optional caused-by)
             (obj "id" id "agent_id" agent-id "type"
                  (if (string= type "user-message")
                      "historical-user-message-imported"
                      "historical-agent-message-imported")
                  "timestamp" (+ 100 id) "caused_by" (or caused-by :null)
                  "payload"
                  (obj "text" text "metadata"
                       (obj "source" "historical-agent-migration-v1"
                            "persona_id" persona-id
                            "source_agent_id" "migration-source-agent"
                            "source_event_id" source-id
                            "source_event_type" type
                            "source_event_sha256"
                            "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
                            "source_caused_by" :null)))))
    (setf (gethash 10 index)
          (historical 10 1 "user-message" "Historical operator evidence.")
          (gethash 11 index)
          (historical 11 2 "agent-message" "Historical assistant evidence." 10)
          (gethash 12 index)
          (obj "id" 12 "agent_id" agent-id "type" "conversation-episode-sealed"
               "timestamp" 112 "caused_by" 11 "payload"
               (obj "schema_version" 1 "episode_id" "episode:migration:10:11"
                    "persona_id" persona-id "first_event_id" 10
                    "last_event_id" 11 "first_timestamp" 110
                    "last_timestamp" 111 "source_event_ids" #(10 11)
                    "synopsis" "Historical exchange." "subjects" #()
                    "entities" #() "retrieval_cues" #()
                    "broader_categories" #() "unresolved_threads" #())))
    (let* ((context (%ccg-source-context
                     graph 12 200 index agent-id persona-id))
           (sources (gethash "sources" (gethash "source_packet" context)))
           (operator-source (aref sources 0))
           (source-agent-source (aref sources 1)))
      (assert (equal "operator"
                     (gethash "role" (gethash "identity" operator-source))))
      (assert (equal "other"
                     (gethash "role" (gethash "identity" source-agent-source))))
      (assert (search "migrated-event:migration-source-agent:2"
                      (gethash "source_id" source-agent-source)))
      (assert (equalp #("migration-source-agent")
                      (%kgs-origin-agent-ids
                       #("migrated-event:migration-source-agent:2"
                         "migrated-memory:migration-source-agent:41"
                         "event:99"))))
      (assert (not (equal
                    (gethash "principal_id"
                             (gethash "identity" source-agent-source))
                    (gethash "principal_id"
                             (gethash "identity" operator-source))))))
    ;; A large prior-agent receipt must not prevent the smaller operator source
    ;; or all later episodes from reaching the owner. It is omitted wholesale,
    ;; never truncated into false exact evidence; its event ID remains bound by
    ;; the authenticated episode root.
    (setf (gethash 13 index)
          (historical 13 3 "agent-message"
                      (make-string 30001 :initial-element #\x) 10)
          (gethash 14 index)
          (obj "id" 14 "agent_id" agent-id
               "type" "conversation-episode-sealed" "timestamp" 114
               "caused_by" 13 "payload"
               (obj "schema_version" 1
                    "episode_id" "episode:migration:10:13"
                    "persona_id" persona-id "first_event_id" 10
                    "last_event_id" 13 "first_timestamp" 110
                    "last_timestamp" 113 "source_event_ids" #(10 13)
                    "synopsis" "Bounded source test." "subjects" #()
                    "entities" #() "retrieval_cues" #()
                    "broader_categories" #() "unresolved_threads" #())))
    (let* ((context (%ccg-source-context
                     graph 14 200 index agent-id persona-id))
           (sources (gethash "sources" (gethash "source_packet" context)))
           (batches (pai.context-graph::%cgro-source-batches context)))
      (assert (= 1 (length sources)))
      (assert (equal "original-utterance" (gethash "kind" (aref sources 0))))
      (assert (= 1 (length batches)))
      (assert (= 1 (length (aref batches 0)))))))

(let* ((agent-id "runtime-fixture-agent") (persona-id "runtime-fixture-persona")
       (index (make-hash-table :test #'eql)) (events nil) (sequence 3)
       (ontology (%ccg-ontology))
       (runtime (pai.context-graph:context-graph-runtime-create ontology *knowledge-graph-ontology-revision* agent-id persona-id))
       (simple (obj "new_entities" (vector (obj "name" "Mina" "kind" "organism" "alternate_names" #() "categories" #("cat")))
                    "facts" (vector (obj "subject" "operator" "object" "new_1" "predicate" "owns"
                                "statement" "The operator owns a cat." "scope" "assertion" "polarity" "positive" "attributed_to" "operator"
                                "time" (obj "character" "ongoing-state" "occurred_at" :null "valid_from" :null "valid_until" :null)
                                "evidence" (vector (obj "source" "source_1" "quote" "I own a cat named Mina."))))
                    "name_corrections" #())))
  (setf (gethash 1 index) (obj "id" 1 "agent_id" agent-id "type" "user-message" "timestamp" 100
                              "payload" (obj "text" "I own a cat named Mina." "metadata"
                                             (obj "persona_id" persona-id "source" "recursive-mind-v1")))
        (gethash 2 index) (obj "id" 2 "agent_id" agent-id "type" "agent-message" "timestamp" 101
                              "payload" (obj "text" "Thanks for telling me." "metadata"
                                             (obj "persona_id" persona-id "source" "recursive-mind-v1")))
        (gethash 3 index) (obj "id" 3 "agent_id" agent-id "type" "conversation-episode-sealed" "caused_by" nil
                              "transport_score" 0.25d0
                              "payload" (obj "schema_version" 1 "episode_id" "episode:fixture" "persona_id" persona-id
                                   "first_event_id" 1 "last_event_id" 2 "first_timestamp" 100 "last_timestamp" 101
                                   "source_event_ids" #(1 2) "synopsis" "An animal was discussed."
                                   "subjects" #() "entities" #() "retrieval_cues" #() "broader_categories" #() "unresolved_threads" #())))
  (labels ((source (graph id now) (%ccg-source-context graph id now index agent-id persona-id))
           (append-event (type payload cause)
             (let ((e (obj "id" (incf sequence) "agent_id" agent-id "type" type "payload" payload "caused_by" cause)))
               (push e events) e))
           (model (phase spec digest opened)
             (declare (ignore spec digest opened))
             (cond ((equal phase "entities") (obj "new_entities" (gethash "new_entities" simple)))
                   ((equal phase "facts") (obj "facts" (gethash "facts" simple) "name_corrections" #()))
                   ((equal phase "review")
                    (let* ((context (source (pai.context-graph::context-graph-runtime-graph runtime) 3 200))
                           (raw (pai.context-graph::%cgs-expand context ontology *knowledge-graph-ontology-revision* simple))
                           (prepared (gethash "proposal" (gethash "value" (pai.context-graph::%cgm-prepare context raw *knowledge-graph-ontology-revision*)))))
                      (obj "schema_version" 2 "revision_reviews" #()
                           "claim_reviews" (map 'vector (lambda (claim)
                                 (obj "claim_ref" (gethash "claim_ref" claim) "verdict" "DIRECTLY_EVIDENCED"
                                      "evidence" "The exact operator utterance directly supports this claim."))
                                 (pai.context-graph::%cgm-claims prepared))))))))
    (let* ((guide (%ccg-descriptor-guide))
           (pai.context-graph::*cgf-protocol* "identity-formation-v8")
           (context (source (pai.context-graph::context-graph-runtime-graph runtime) 3 200))
           (spec (gethash "value" (pai.context-graph::%cgf-mention-spec context)))
           (reservation (%ccg-model-reservation "mentions" spec nil 4
                                                 *conscious-context-graph-request-ceiling-microusd*)))
      (assert (= 2500000 *conscious-context-graph-generation-budget-microusd*))
      (assert (equal "direct-v6" *conscious-context-graph-runtime-profile*))
      (assert (equal "identity-formation-owner-v6"
                     *conscious-context-graph-owner-generation*))
      (assert (equal "identity-formation-v9"
                     *conscious-context-graph-formation-protocol*))
      (assert (= 2048 (%ccg-phase-output-tokens "mentions")))
      (assert (= 4096 (%ccg-phase-output-tokens "identity-page-1")))
      (assert (= 4096 (%ccg-phase-output-tokens "new-identity-groups")))
      (assert (= 8192 (%ccg-phase-output-tokens "facts")))
      (assert (= 4096 (%ccg-phase-output-tokens "review")))
      (assert (<= 1 reservation *conscious-context-graph-request-ceiling-microusd*))
      (assert (equal "condition" (gethash "name" (find "condition" guide :test #'equal
                                                        :key (lambda (row) (gethash "name" row)))))))
    (let* ((boundary (obj "storage_id" "fixture-storage"))
           (direct-key (%ccg-runtime-cache-key boundary agent-id persona-id))
           (*conscious-context-graph-runtime-profile* "reviewed-inference-v7")
           (*conscious-context-graph-owner-generation* "identity-formation-owner-v7")
           (*conscious-context-graph-formation-protocol* "identity-formation-v10")
           (owner (%ccg-create-owner
                   (pai.context-graph::context-graph-runtime-graph runtime)
                   agent-id persona-id))
           (reviewed-key (%ccg-runtime-cache-key boundary agent-id persona-id)))
      (assert (equal "identity-formation-owner-v7"
                     (pai.context-graph::cgi-owner-protocol owner)))
      (assert (not (equal direct-key reviewed-key))))
    (let* ((boundary (obj "storage_id" "fixture-storage"))
           (direct-key (%ccg-runtime-cache-key boundary agent-id persona-id))
           (*conscious-context-graph-runtime-profile* "reviewed-inference-v8")
           (*conscious-context-graph-owner-generation* "identity-formation-owner-v8")
           (*conscious-context-graph-formation-protocol* "identity-formation-v13")
           (owner (%ccg-create-owner
                   (pai.context-graph::context-graph-runtime-graph runtime)
                   agent-id persona-id))
           (reviewed-key (%ccg-runtime-cache-key boundary agent-id persona-id)))
      (assert (equal "identity-formation-owner-v8"
                     (pai.context-graph::cgi-owner-protocol owner)))
      (assert (not (equal direct-key reviewed-key))))
    (let* ((boundary (obj "storage_id" "fixture-storage"))
           (direct-key (%ccg-runtime-cache-key boundary agent-id persona-id))
           (*conscious-context-graph-runtime-profile* "reviewed-inference-v9")
           (*conscious-context-graph-owner-generation* "identity-formation-owner-v9")
           (*conscious-context-graph-formation-protocol* "identity-formation-v14")
           (owner (%ccg-create-owner
                   (pai.context-graph::context-graph-runtime-graph runtime)
                   agent-id persona-id))
           (reviewed-key (%ccg-runtime-cache-key boundary agent-id persona-id))
           (descriptor (knowledge-graph-ontology-provider-descriptor
                        (%ccg-runtime-ontology-revision))))
      (assert (equal "identity-formation-owner-v9"
                     (pai.context-graph::cgi-owner-protocol owner)))
      (assert (equal *knowledge-graph-family-ontology-revision*
                     (pai.context-graph::cgi-owner-revision owner)))
      (assert (find "attribute_value" (gethash "entity_types" descriptor)
                    :test #'equal))
      (assert (find "parent_of" (gethash "predicate_signatures" descriptor)
                    :test #'equal :key (lambda (row)
                                         (gethash "predicate" row))))
      (assert (not (equal direct-key reviewed-key))))
    (let ((*conscious-context-graph-owner-generation*
            "identity-formation-owner-v7")
          (*conscious-context-graph-formation-protocol*
            "identity-formation-v9"))
      (assert (handler-case
                  (progn
                    (%ccg-create-owner
                     (pai.context-graph::context-graph-runtime-graph runtime)
                     agent-id persona-id)
                    nil)
                (error () t))))
    (let ((*conscious-context-graph-owner-generation*
            "identity-formation-owner-v8")
          (*conscious-context-graph-formation-protocol*
            "identity-formation-v10"))
      (assert (handler-case
                  (progn
                    (%ccg-create-owner
                     (pai.context-graph::context-graph-runtime-graph runtime)
                     agent-id persona-id)
                    nil)
                (error () t))))
    (let ((owner (pai.context-graph::%cgf-owner-create-v2
                   (pai.context-graph::context-graph-runtime-graph runtime)
                   agent-id persona-id *knowledge-graph-ontology-revision*)))
      (setf (gethash '(10 "mentions") (pai.context-graph::cgi-owner-phases owner))
              (obj "outcome" "response" "charged_microusd" 2300)
            (gethash '(11 "facts") (pai.context-graph::cgi-owner-phases owner))
              (obj "outcome" "request" "reserved_microusd" 4700))
      (assert (= 7000 (%ccg-owner-exposure-microusd owner)))
      (assert (= 2493000 (%ccg-owner-budget-remaining-microusd owner)))
      (let ((*conscious-context-graph-prior-exposure-microusd* 1200000))
        (assert (= 1293000 (%ccg-owner-budget-remaining-microusd owner)))))
    ;; Provider latency must not monopolize the stable graph projection.  The
    ;; formation lock remains the single-writer guard; this helper releases only
    ;; the reader lock and restores it before the owner resumes.
    (let* ((lock (bt:make-lock "graph-io-release-fixture"))
           (*conscious-context-graph-lock* lock)
           (acquired nil))
      (bt:with-lock-held (lock)
        (multiple-value-bind (response charge)
            (%ccg-call-with-graph-unlocked
             (lambda (phase spec digest opened ceiling)
               (declare (ignore phase spec digest opened ceiling))
               (setf acquired (bt:acquire-lock lock nil))
               (when acquired (bt:release-lock lock))
               (values (obj "ok" :true) 17))
             "fixture" (obj) "digest" 1 100)
          (assert (eq t acquired))
          (assert (eq :true (gethash "ok" response)))
          (assert (= 17 charge))))
      (assert (bt:acquire-lock lock nil))
      (bt:release-lock lock))
    ;; A real thread barrier proves the same property across workers: formation
    ;; provider IO remains blocked while a public reader acquires the graph lock
    ;; and reads the last stable projection.  The provider is released only
    ;; after that read completes.
    (let* ((lock (bt:make-lock "graph-provider-barrier-fixture"))
           (provider-entered (sb-thread:make-semaphore :count 0))
           (release-provider (sb-thread:make-semaphore :count 0))
           (worker-result :not-finished)
           (worker
             (bt:make-thread
              (lambda ()
                (let ((*conscious-context-graph-lock* lock))
                  (handler-case
                      (bt:with-lock-held (lock)
                        (multiple-value-bind (response charge)
                            (%ccg-call-with-graph-unlocked
                             (lambda (phase spec digest opened ceiling)
                               (declare (ignore phase spec digest opened ceiling))
                               (sb-thread:signal-semaphore provider-entered)
                               (sb-thread:wait-on-semaphore release-provider)
                               (values (obj "ok" :true) 23))
                             "fixture" (obj) "digest" 2 100)
                          (setf worker-result
                                (and (eq :true (gethash "ok" response))
                                     (= 23 charge)))))
                    (error (condition)
                      (setf worker-result condition)))))
              :name "blocked graph formation provider fixture")))
      (assert (sb-thread:wait-on-semaphore provider-entered :timeout 5))
      (let ((*conscious-context-graph-lock* lock))
        (bt:with-lock-held (lock)
          (let ((stable (pai.context-graph::context-graph-runtime-graph runtime)))
            (assert (integerp
                     (pai.context-graph:context-graph-entity-count stable)))
            (assert (integerp
                     (pai.context-graph:context-graph-fact-count stable))))))
      (assert (eq :not-finished worker-result))
      (sb-thread:signal-semaphore release-provider)
      (assert (not (eq :timed-out
                       (sb-thread:join-thread worker :timeout 10
                                              :default :timed-out))))
      (assert (eq t worker-result)))
    (let* ((owner (pai.context-graph::%cgf-owner-create-v6
                    (pai.context-graph::context-graph-runtime-graph runtime)
                    agent-id persona-id *knowledge-graph-ontology-revision*))
           (failure (obj "reason" "IDENTITY_CALL_FAILED" "failure_class" "provider-transient"
                         "retryable" :true "attempt" 1 "failed_at" 100 "next_retry_at" 130))
           (opening (obj "id" 10 "agent_id" agent-id "type" "context-graph-identity-opened"
                         "caused_by" 3 "payload"
                         (obj "persona_id" persona-id "generation" "identity-formation-owner-v6"
                              "record_json" (pai.context-graph:context-graph-runtime-json
                                              (obj "attempt" 1)))))
           (event (obj "id" 20 "agent_id" agent-id "type" "context-graph-identity-failed"
                       "caused_by" 10 "payload"
                       (obj "persona_id" persona-id "generation" "identity-formation-owner-v6"
                            "record_json" (pai.context-graph:context-graph-runtime-json failure)))))
      (setf (gethash '(3 0) (pai.context-graph::cgi-owner-tasks owner)) 10
            (gethash 10 (pai.context-graph::cgi-owner-opens owner)) opening
            (gethash 10 (pai.context-graph::cgi-owner-terminals owner)) event)
      (multiple-value-bind (retryable unresolved next-retry-at)
          (%ccg-owner-failure-summary owner 110)
        (assert (= 1 retryable)) (assert (zerop unresolved)) (assert (= 130 next-retry-at)))
      (multiple-value-bind (retryable unresolved next-retry-at)
          (%ccg-owner-failure-summary owner 130)
        (assert (= 1 retryable)) (assert (zerop unresolved)) (assert (null next-retry-at)))
      (assert (null (%ccg-owner-next-task owner #'source (list (gethash 3 index))
                                          agent-id persona-id 129)))
      (assert (equal '(3 0 2 10)
                     (%ccg-owner-next-task owner #'source (list (gethash 3 index))
                                           agent-id persona-id 130)))
      (setf (gethash "retryable" failure) :false (gethash "next_retry_at" failure) :null
            (gethash "record_json" (gethash "payload" event))
              (pai.context-graph:context-graph-runtime-json failure))
      (multiple-value-bind (retryable unresolved next-retry-at)
          (%ccg-owner-failure-summary owner 130)
        (assert (zerop retryable)) (assert (= 1 unresolved)) (assert (null next-retry-at))))
    ;; A future retry time is a generation-wide provider circuit.  A later
    ;; untouched episode must not be opened while the provider is known down.
    (let* ((owner (pai.context-graph::%cgf-owner-create-v8
                    (pai.context-graph::context-graph-runtime-graph runtime)
                    agent-id persona-id *knowledge-graph-ontology-revision*))
           (opening-id 77)
           (opening (obj "id" opening-id "agent_id" agent-id
                         "type" "context-graph-identity-opened"
                         "caused_by" 3 "payload"
                         (obj "persona_id" persona-id
                              "generation" "identity-formation-owner-v8"
                              "record_json"
                              (pai.context-graph:context-graph-runtime-json
                               (obj "attempt" 1)))))
           (failure-record
             (obj "reason" "IDENTITY_CALL_FAILED"
                  "failure_class" "provider-transient"
                  "retryable" :true "attempt" 1
                  "failed_at" 100 "next_retry_at" 130))
           (failure (obj "id" 78 "agent_id" agent-id
                         "type" "context-graph-identity-failed"
                         "caused_by" opening-id "payload"
                         (obj "persona_id" persona-id
                              "generation" "identity-formation-owner-v8"
                              "record_json"
                              (pai.context-graph:context-graph-runtime-json
                               failure-record))))
           (old-sync (symbol-function '%ccg-sync))
           (*conscious-recursive-mind-operator-pending-p* nil)
           (*conscious-recursive-mind-operator-waiters* 0)
           (*conscious-context-graph-lock*
             (bt:make-lock "provider-circuit-fixture"))
           (*conscious-context-graph-now-fn* (lambda () 110)))
      (setf (gethash '(3 0) (pai.context-graph::cgi-owner-tasks owner))
              opening-id
            (gethash opening-id (pai.context-graph::cgi-owner-opens owner))
              opening
            (gethash opening-id (pai.context-graph::cgi-owner-terminals owner))
              failure)
      (unwind-protect
           (progn
             (setf (symbol-function '%ccg-sync)
                     (lambda (event-backend selected-agent selected-persona
                              &optional derived-backend)
                       (declare (ignore event-backend selected-agent
                                        selected-persona derived-backend))
                       (values runtime #'source
                               (list (gethash 3 index) (gethash 12 index))
                               owner)))
             (let ((report (conscious-context-graph-formation-step
                            nil nil agent-id persona-id)))
               (assert (equal "paused-provider" (gethash "status" report)))
               (assert (eq :null (gethash "opened_event_id" report)))
               (assert (= 1 (hash-table-count
                             (pai.context-graph::cgi-owner-tasks owner))))))
        (setf (symbol-function '%ccg-sync) old-sync)))
    ;; Source inspection is per episode.  An unauthentic or malformed earlier
    ;; seal is reported as blocked coverage, but cannot hide a later valid seal.
    (let* ((owner (pai.context-graph::%cgf-owner-create-v6
                    (pai.context-graph::context-graph-runtime-graph runtime)
                    agent-id persona-id *knowledge-graph-ontology-revision*))
           (malformed (obj "id" 0 "agent_id" agent-id
                           "type" "conversation-episode-sealed"
                           "payload" (obj "persona_id" persona-id))))
      (multiple-value-bind (task blocked)
          (%ccg-owner-next-task
           owner
           (lambda (selected-graph id now)
             (if (zerop id)
                 (error "malformed sealed source fixture")
                 (source selected-graph id now)))
           (list malformed (gethash 3 index)) agent-id persona-id 200)
        (assert (equal '(3 0 1 :null) task))
        (assert (= 1 blocked))))
    ;; A cold process can restore an opened task that has no terminal receipt.
    ;; The adapter must resume that owner boundary; it must not restrict owner
    ;; execution to the branch that creates a fresh opening.
    (let* ((owner (pai.context-graph::%cgf-owner-create-v6
                    (pai.context-graph::context-graph-runtime-graph runtime)
                    agent-id persona-id *knowledge-graph-ontology-revision*))
           (opening-id 77)
           (calls 0)
           (old-sync (symbol-function '%ccg-sync))
           (old-run (symbol-function 'pai.context-graph::%cgi-owner-run))
           (*conscious-recursive-mind-operator-pending-p* nil)
           (*conscious-recursive-mind-operator-waiters* 0)
           (*conscious-context-graph-lock* (bt:make-lock "cold-open-fixture")))
      (setf (gethash opening-id (pai.context-graph::cgi-owner-opens owner))
              (obj "id" opening-id "type" "context-graph-identity-opened"))
      (unwind-protect
           (progn
             (setf (symbol-function '%ccg-sync)
                     (lambda (event-backend selected-agent selected-persona
                              &optional derived-backend)
                       (declare (ignore event-backend selected-agent
                                        selected-persona derived-backend))
                       (values runtime #'source nil owner))
                   (symbol-function 'pai.context-graph::%cgi-owner-run)
                     (lambda (selected-owner source-fn append-fn call-fn opened
                              &optional reservation-fn now-fn)
                       (declare (ignore selected-owner source-fn append-fn call-fn
                                        reservation-fn now-fn))
                       (incf calls)
                       (assert (= opening-id opened))
                       (obj "status" "complete")))
             (let ((report (conscious-context-graph-formation-step
                            nil nil agent-id persona-id)))
               (assert (= 1 calls))
               (assert (equal "sealed" (gethash "status" report)))
               (assert (= opening-id (gethash "opened_event_id" report)))))
        (setf (symbol-function '%ccg-sync) old-sync
              (symbol-function 'pai.context-graph::%cgi-owner-run) old-run)))
    (assert (equal "sealed" (gethash "status" (pai.context-graph:context-graph-runtime-step runtime '(3) #'source #'model #'append-event :now 200))))
    (assert (= 1 (pai.context-graph::context-graph-runtime-applications runtime)))
    (let* ((graph (pai.context-graph::context-graph-runtime-graph runtime))
           (fact (loop for value being the hash-values of
                       (pai.context-graph::context-graph-facts graph)
                       return value))
           (original-source-ids (copy-seq (gethash "accepted_source_ids" fact)))
           (search
             (unwind-protect
                  (progn
                    (setf (gethash "accepted_source_ids" fact)
                          #("migrated-memory:migration-source-agent:41"))
                    (%ccg-search runtime
                                 (knowledge-graph-search-request
                                  :query "MINA cat")))
               (setf (gethash "accepted_source_ids" fact)
                     original-source-ids)))
           (compact (knowledge-graph-search-compact-result search))
           (node (find "Mina" (gethash "nodes" compact) :test #'equal :key (lambda (n) (gethash "label" n)))))
      (assert node) (assert (= 1 (gethash "edge_count" compact)))
      (assert (equalp #("migration-source-agent")
                      (gethash "origin_agent_ids" node)))
      (assert (equalp #("migration-source-agent")
                      (gethash "origin_agent_ids"
                               (aref (gethash "edges" compact) 0))))
      (assert (zerop (gethash "edge_count" (knowledge-graph-search-compact-result
                        (%ccg-search runtime (knowledge-graph-search-request :starting-node-id (gethash "node_id" node) :direction "outgoing"))))))
      (assert (= 1 (gethash "edge_count" (knowledge-graph-search-compact-result
                        (%ccg-search runtime (knowledge-graph-search-request :starting-node-id (gethash "node_id" node) :direction "incoming"))))))
      (assert (zerop (gethash "edge_count" (knowledge-graph-search-compact-result
                        (%ccg-search runtime (knowledge-graph-search-request :query "Mina" :predicates #("works_at"))))))))
    ;; A reviewed inference is durable graph state but remains opt-in at the
    ;; retrieval boundary.  Exercise the actual runtime traversal rather than
    ;; only the standalone evidence predicate.
    (let* ((graph (pai.context-graph::context-graph-runtime-graph runtime))
           (fact (loop for value being the hash-values of
                       (pai.context-graph::context-graph-facts graph)
                       return value))
           (prior-status (gethash "evidence_status" fact))
           (prior-lifecycle (gethash "status" fact))
           (start (gethash "subject_id" fact)))
      (unwind-protect
           (progn
              (setf (gethash "evidence_status" fact) "inference")
              (let ((verified (knowledge-graph-search-compact-result
                               (%ccg-search runtime
                                 (knowledge-graph-search-request :query "Mina")))))
                (assert (zerop (gethash "edge_count" verified)))
                (assert (= 1 (gethash "omitted_reviewed_inference_count" verified)))
                (assert (search "consider one inferred search"
                                (gethash "retrieval_hint" verified))))
              (let ((answer (knowledge-graph-search-compact-result
                            (%ccg-search runtime
                              (knowledge-graph-search-request :query "Mina" :evidence-policy "inferred")))))
               (assert (= 1 (gethash "edge_count" answer)))
               (assert (equal "identity-with-relations" (gethash "answer_status" answer)))
               ;; Traversed inference is not a verified query answer.
               (assert (zerop (gethash "matched_fact_count" answer)))
               (assert (gethash "confirmation_recommended" (aref (gethash "edges" answer) 0))))
             (let* ((before
                      (pai.context-graph::%cg-authority-watermark
                       graph agent-id persona-id))
                    (candidate (%ccg-confirmation-candidate
                                runtime (gethash "fact_id" fact))))
               (assert (equal "inference"
                              (gethash "evidence_status" candidate)))
               (assert (equal (gethash "identity_sha256" fact)
                              (gethash "identity_sha256" candidate)))
               (assert (equal (gethash "fact" fact)
                              (gethash "statement" candidate)))
               (assert
                (pai.context-graph::%cg-authority-equal-p
                 before
                 (pai.context-graph::%cg-authority-watermark
                  graph agent-id persona-id)))
               (assert (handler-case
                           (progn
                             (%ccg-confirmation-candidate runtime "missing-fact")
                             nil)
                         (error () t))))
             (assert
              (zerop
               (gethash
                "edge_count"
                (knowledge-graph-search-compact-result
                 (%ccg-search
                  runtime
                  (knowledge-graph-search-request
                   :starting-node-id start :evidence-policy "verified"))))))
             (assert
              (= 1
                 (gethash
                  "edge_count"
                  (knowledge-graph-search-compact-result
                   (%ccg-search
                    runtime
                     (knowledge-graph-search-request
                      :starting-node-id start
                      :evidence-policy "inferred"))))))
             (setf (gethash "status" fact) "retired")
             (assert
              (zerop
               (gethash
                "edge_count"
                (knowledge-graph-search-compact-result
                 (%ccg-search
                  runtime
                  (knowledge-graph-search-request
                   :starting-node-id start
                   :evidence-policy "inferred")))))))
        (setf (gethash "evidence_status" fact) prior-status
              (gethash "status" fact) prior-lifecycle))
      (assert (handler-case
                  (progn
                    (%ccg-confirmation-candidate runtime (gethash "fact_id" fact))
                    nil)
                (error () t))))
    ;; Exact operator identity discovery is independent of factual-context
    ;; selection.  Simulate an admitted current entity with no traversable edge;
    ;; an exact label still appears and bypasses semantic suggestions.
    (let* ((graph (pai.context-graph::context-graph-runtime-graph runtime))
           (entity (find "Mina" (loop for row being the hash-values of
                                         (pai.context-graph::context-graph-entities graph) collect row)
                         :test #'equal :key (lambda (row) (gethash "label" row))))
           (id (gethash "entity_id" entity))
           (adjacency (gethash id (pai.context-graph::context-graph-entity-adjacency graph))))
      (unwind-protect
           (progn
             (setf (gethash id (pai.context-graph::context-graph-entity-adjacency graph)) #())
             (let* ((*conscious-context-graph-semantic-similarity-fn*
                      (lambda (query document) (declare (ignore query document)) 0.66d0))
                    (compact (knowledge-graph-search-compact-result
                              (%ccg-search runtime (knowledge-graph-search-request :query "mina")))))
               (assert (equal "exact-identity" (gethash "query_match_kind" compact)))
               (assert (equal "identity-only" (gethash "answer_status" compact)))
               (assert (zerop (gethash "matched_fact_count" compact)))
               (assert (= 1 (count "Mina" (gethash "nodes" compact)
                                   :test #'equal :key (lambda (node) (gethash "label" node)))))
               (assert (zerop (gethash "edge_count" compact)))))
        (setf (gethash id (pai.context-graph::context-graph-entity-adjacency graph)) adjacency)))
    (let* ((graph (pai.context-graph::context-graph-runtime-graph runtime))
           (context (source graph 3 200))
           (refs (make-hash-table :test #'equal)))
      ;; Exact persona lookup is a runtime identity binding over an admitted
      ;; participant node; it must not turn arbitrary metadata into graph data.
      (pai.context-graph::%cg-authority-new-entity
       graph
       (obj "local_ref" "runtime:active-persona" "kind" "agent"
            "label" "active-persona" "aliases" #()
            "classifications" #("active-persona"))
       context (gethash "participants" context) refs)
      (setf (pai.context-graph::context-graph-entity-scan-index graph)
            (coerce (sort (loop for id being the hash-keys of
                                (pai.context-graph::context-graph-entities graph)
                                collect id)
                          #'string<)
                    'vector)
            (pai.context-graph::context-graph-projection-digest graph) nil))
    (let* ((graph (pai.context-graph::context-graph-runtime-graph runtime))
           (compact
             (knowledge-graph-search-compact-result
              (%ccg-search
               runtime
               (knowledge-graph-search-request
                :exact-queries #( "Mina" "Unlisted Fixture Name"
                                  "runtime-fixture-persona" )))))
           (audits (gethash "exact_query_results" compact)))
      (assert (equal "available" (gethash "status" compact)))
      (assert (equal "exact-audit" (gethash "answer_status" compact)))
      (assert (= (pai.context-graph:context-graph-entity-count graph)
                 (gethash "graph_entity_count" compact)))
      (assert (= (pai.context-graph:context-graph-fact-count graph)
                 (gethash "graph_fact_count" compact)))
      (assert (= (gethash "node_count" compact)
                 (gethash "returned_node_count" compact)))
      (assert (= 1 (gethash "match_count" (aref audits 0))))
      (assert (null (gethash "absence_confirmed" (aref audits 0))))
      (assert (eq t (gethash "scan_complete" (aref audits 1))))
      (assert (eq t (gethash "absence_confirmed" (aref audits 1))))
      (assert (= 1 (gethash "match_count" (aref audits 2))))
      (assert (equal "active-persona"
                     (gethash "label" (aref (gethash "matches" (aref audits 2)) 0))))
      (assert (null (gethash "absence_confirmed" (aref audits 2)))))
    (let ((*conscious-context-graph-semantic-similarity-fn*
            (lambda (query document) (declare (ignore query document)) 0.66d0)))
      (let ((compact (knowledge-graph-search-compact-result
                      (%ccg-search runtime
                                   (knowledge-graph-search-request :query "Unlisted Fixture Name")))))
        (assert (equal "related-suggestions" (gethash "query_match_kind" compact)))
        (assert (member (gethash "answer_status" compact)
                        '("suggestions-only" "query-facts") :test #'equal))
        (assert (not (find "Unlisted Fixture Name" (gethash "nodes" compact)
                           :test #'equal :key (lambda (node) (gethash "label" node)))))))
    (multiple-value-bind (records report metadata)
        (%ccg-context-records runtime (knowledge-graph-attention-frame :attention-kind "conversation" :stimulus "What cat do I own?") 1600)
      (assert (plusp (length records)))
      (assert (<= (gethash "used_characters" report) 1600))
      (assert (some (lambda (r) (search "Mina" (gethash "content" r))) records))
      (assert (some (lambda (r) (search "The operator owns a cat." (gethash "content" r))) records))
      (assert (= (length records) (length metadata)))
      (assert
       (every
        (lambda (row)
          (if (string= "graph-fact" (gethash "source_kind" row ""))
              (eq t (gethash "operator_support" row))
              (null (gethash "operator_support" row))))
        metadata)))
    (let* ((snapshot (append (list (gethash 1 index) (gethash 2 index)
                                    (gethash 3 index))
                              (reverse events)))
           (coverage (conscious-context-graph-coverage-inspect
                      snapshot agent-id persona-id :now 200))
           (serialized (shasht:write-json coverage nil)))
      (assert (equal "context-graph-coverage-inspector-v1"
                     (gethash "inspector_revision" coverage)))
      ;; Legacy runtime receipts remain audit evidence but are not reinterpreted
      ;; as destination work for the active v6 formation owner.
      (assert (= 1 (gethash "queued" (gethash "stage_counts" coverage))))
      (assert (= 0 (gethash "database_write_count" coverage)))
      (assert (null (search "Mina" serialized)))
      (assert (null (search "cat" serialized))))
    (let ((retrieved (%ccg-retrieve runtime "What cat do I own?")))
      (assert (equal "focused-kg-v5" (gethash "retrieval_revision" retrieved)))
      (assert (plusp (length (gethash "facts" (gethash "context" retrieved))))))
    ;; Cold semantic graph reads batch-fill every missing document once. Later
    ;; queries reuse the digest-bound vectors and embed only the new query.
    (let* ((query-calls 0)
           (batch-calls 0)
           (document-count 0)
           (*conscious-context-graph-semantic-similarity-fn* nil)
           (*conscious-context-graph-semantic-query-embed-fn*
             (lambda (query)
               (declare (ignore query))
               (incf query-calls)
               '(1d0 0d0)))
           (*conscious-context-graph-semantic-documents-embed-fn*
             (lambda (documents)
               (incf batch-calls)
               (incf document-count (length documents))
               (loop repeat (length documents) collect '(1d0 0d0)))))
      (clrhash *conscious-context-graph-semantic-vectors*)
      (let ((first (%ccg-retrieve runtime "feline companion")))
        (assert (plusp (length (gethash "facts" (gethash "context" first)))))
        (assert (= 1 query-calls))
        (assert (= 1 batch-calls))
        (assert (= 1 document-count)))
      (let ((second (%ccg-retrieve runtime "companion animal")))
        (assert (plusp (length (gethash "facts" (gethash "context" second)))))
        (assert (= 2 query-calls))
        (assert (= 1 batch-calls))
        (assert (= 1 document-count)))
      (clrhash *conscious-context-graph-semantic-vectors*))
    (let ((*ollama-endpoint* "http://127.0.0.1:11434/api/embeddings"))
      (assert (equal "http://127.0.0.1:11434/api/embed"
                     (%ollama-batch-embedding-endpoint))))
    ;; The production identity owner stores the authenticated episode under
    ;; source_context (the legacy runtime owner used context).  Prove that the
    ;; read facade actually discovers source-only terms through the new shape.
    (let* ((graph (pai.context-graph::context-graph-runtime-graph runtime))
           (source-context (source graph 3 200))
           (packet (gethash "source_packet" source-context))
           (extra (pai.context-graph::%cg-detach (aref (gethash "sources" packet) 0)))
           (opening nil))
      (setf (gethash "source_id" extra) "event:998"
            (gethash "text" extra) "Mina is playful."
            (gethash "text_sha256" extra) (pai.context-graph::%cg-sha256 "Mina is playful.")
            (gethash "resource_id" (gethash "resource_ref" extra)) "event:998"
            (gethash "version_id" (gethash "resource_ref" extra))
              (pai.context-graph::%cg-sha256 "Mina is playful.")
            (gethash "sources" packet)
              (concatenate 'vector (gethash "sources" packet) (vector extra)))
      (setf opening (obj "id" 999 "payload"
                         (obj "record_json"
                              (pai.context-graph:context-graph-runtime-json
                                (obj "source_context" source-context)))))
      (setf (gethash 999 (pai.context-graph::context-graph-runtime-opens runtime)) opening)
      (let ((retrieved (%ccg-retrieve runtime "playful")))
        (assert (= 1 (length (gethash "sources" (gethash "context" retrieved)))))
        (assert (search "Mina is playful."
                        (gethash "text" (gethash "value"
                          (aref (gethash "sources" (gethash "context" retrieved)) 0)))))))
    (let ((*conscious-context-graph-semantic-similarity-fn*
            (lambda (query document) (declare (ignore query document)) 0.45d0)))
      (multiple-value-bind (records report)
          (%ccg-context-records runtime (knowledge-graph-attention-frame :attention-kind "conversation" :stimulus "What is my blood type?") 1600)
        (assert (zerop (length records)))
        (assert (zerop (gethash "used_characters" report)))))
    (let ((*conscious-context-graph-semantic-similarity-fn*
            (lambda (query document)
              (declare (ignore query))
              (if (search "cat" document :test #'char-equal) 0.66d0 0.45d0))))
      (let* ((graph (pai.context-graph::context-graph-runtime-graph runtime))
             (entity (find "Mina" (loop for row being the hash-values of
                                          (pai.context-graph::context-graph-entities graph) collect row)
                           :test #'equal :key (lambda (row) (gethash "label" row))))
             (document (%ccg-semantic-document graph (gethash "entity_id" entity) entity)))
        (assert (not (search "Mina" document :test #'char-equal)))
        (assert (search "cat" document :test #'char-equal)))
      (let ((semantic (%ccg-retrieve runtime "feline companion")))
        (assert (equal "separated-nonparticipant-top-cluster-v2"
                       (gethash "semantic_selection_revision" semantic)))
        (assert (= 1 (length (gethash "facts" (gethash "context" semantic))))))
      ;; A weak lexical hit must not suppress the semantic selector when it
      ;; covers only one part of the question.
      (let ((partial (%ccg-retrieve runtime "feline cat")))
        (assert (equal "separated-nonparticipant-top-cluster-v2"
                       (gethash "semantic_selection_revision" partial)))
        (assert (= 1 (length (gethash "facts" (gethash "context" partial))))))
      ;; Fully covered lexical retrieval avoids needless embedding work.
      (let ((lexical (%ccg-retrieve runtime "cat")))
        (assert (eq :null (gethash "semantic_selection_revision" lexical))))
      ;; Restored lifecycle openings may have no source packet; retrieval must
      ;; skip them without treating a missing context as a hash table.
      (setf (gethash 1000 (pai.context-graph::context-graph-runtime-opens runtime))
            (obj "id" 1000 "payload"
                 (obj "record_json"
                      (pai.context-graph:context-graph-runtime-json
                       (obj "reason" "settled-lifecycle-record")))))
      (let ((*conscious-context-graph-semantic-similarity-fn*
              (lambda (query document) (declare (ignore query document)) 0.45d0)))
        (assert (zerop (length (gethash "facts" (gethash "context"
                                      (%ccg-retrieve runtime "unrelated paraphrase"))))))))
    (let ((*conscious-context-graph-semantic-similarity-fn*
            (lambda (query document) (declare (ignore query document)) 0.45d0)))
      (assert (equal "empty" (gethash "status" (%ccg-search runtime (knowledge-graph-search-request :query "unrelated-asteroid"))))))
    (let ((before (pai.context-graph::%cg-authority-watermark (pai.context-graph::context-graph-runtime-graph runtime) agent-id persona-id)))
      (setf runtime (pai.context-graph:context-graph-runtime-create ontology *knowledge-graph-ontology-revision* agent-id persona-id))
      (dolist (e (reverse events)) (pai.context-graph:context-graph-runtime-consume runtime e #'source))
      (assert (pai.context-graph::%cg-authority-equal-p before
                (pai.context-graph::%cg-authority-watermark (pai.context-graph::context-graph-runtime-graph runtime) agent-id persona-id))))
    ;; Same text under a different principal/persona never authenticates by hash.
    (setf (gethash "agent_id" (gethash 1 index)) "foreign-agent")
    (assert (handler-case (progn (source (pai.context-graph::context-graph-runtime-graph runtime) 3 200) nil) (error () t)))
    (setf (gethash "agent_id" (gethash 1 index)) agent-id)
    (remhash 2 index)
    (assert (handler-case (progn (source (pai.context-graph::context-graph-runtime-graph runtime) 3 200) nil) (error () t)))))
(format t "RUNTIME-ADAPTER source authority, shared generation, useful bounded context, search/explorer traversal and replay passed~%")
(format t "PASS context-graph-runtime-adapter-tests~%")
