;;;; knowledge-graph-formation-source-tests.lisp -- pure KG2 source selection.

(in-package :agent)

(ql:quickload '(:shasht :ironclad) :silent t)
(load (test-source "conversation-episode-graph.lisp"))
(load (test-source "knowledge-graph-ontology.lisp"))
(load (test-source "knowledge-graph-formation.lisp"))
(load (test-source "knowledge-graph-formation-owner.lisp"))
(load (test-source "knowledge-graph-formation-source.lisp"))

(defvar *kgfs-pass* 0)
(defvar *kgfs-fail* 0)

(defun kgfs-check (name condition)
  (if condition
      (progn (incf *kgfs-pass*) (format t "PASS ~a~%" name))
      (progn (incf *kgfs-fail*) (format t "FAIL ~a~%" name))))

(defun kgfs-event (id type payload &optional caused-by)
  (obj "id" id "type" type "agent_id" "source-agent"
       "timestamp" (+ 4000000000 id) "caused_by" (or caused-by :null)
       "payload" payload))

(defun kgfs-episode (id episode-id synopsis &optional (persona "source-persona"))
  (kgfs-event
   id "conversation-episode-sealed"
   (obj "schema_version" 1 "episode_id" episode-id "persona_id" persona
        "first_event_id" (- id 2) "last_event_id" (- id 1)
        "first_timestamp" (+ 3999999900 id)
        "last_timestamp" (+ 3999999950 id)
        "source_event_ids" (vector (- id 2) (- id 1))
        "synopsis" synopsis "subjects" #("continuity")
        "entities" #("operator") "retrieval_cues" #("bounded source")
        "broader_categories" #("knowledge") "unresolved_threads" #()
        "protocol_revision" "recursive-conversation-episode-v1"
        "projection_revision" "conversation-episodes-v2"
        "sealed_at" (+ 4000000000 id))))

(defun kgfs-open (id episode-id &optional (persona "source-persona"))
  (kgfs-event
   id "knowledge-graph-formation-opened"
   (obj "persona_id" persona "source_episode_ids" (vector episode-id))))

(defun kgfs-terminal (id opened-id proposal-version &optional failed-p)
  (kgfs-event
   id (if failed-p "knowledge-graph-formation-failed"
          "knowledge-graph-formation-sealed")
   (if failed-p
       (obj "reason" "bounded failure")
       (obj "formation_revision"
            (if (= proposal-version 3)
                *knowledge-graph-formation-revision* "legacy-formation")
            "proposal" (obj "schema_version" proposal-version)))
   opened-id))

(defun kgfs-attempt (id opened-id &optional
                        (revision *knowledge-graph-formation-attempt-revision*))
  (kgfs-event
   id "model-request"
   (obj "knowledge_graph_formation" t "protocol_revision" revision)
   opened-id))

(defun kgfs-memory (event-id memory-id content
                    &key (origin "lived-user") (grounding "grounded")
                      (role "user"))
  (kgfs-event
   event-id "memory-baseline-node"
   (obj "schema_version" 1 "session_id" "fixture" "ordinal" event-id
        "node"
        (obj "scalar_json"
             (shasht:write-json
              (obj "id" memory-id "content" content
                   "created_at" "2026-09-03T12:00:00Z"
                   "origin_class" origin "grounding_status" grounding
                   "epistemic_metadata" (obj "role" role))
              nil)
             "embedding_binary_hex" "" "retrieval_embedding_binary_hex" ""))
   nil))

(defun kgfs-supplemental-candidate
    (event memory-id lexical semantic episodic graph)
  (obj "event" event "event_id" (gethash "id" event)
       "memory_node_id" memory-id
       "lexical_score_milli" lexical
       "semantic_score_milli" semantic
       "episodic_score_milli" episodic
       "graph_score_milli" graph))

(defun kgfs-select (events &optional (candidates #()))
  (let ((expanded (copy-list events)))
    (dolist (event events)
      (when (string= "conversation-episode-sealed" (gethash "type" event ""))
        (loop for source-id across
                (gethash "source_event_ids" (gethash "payload" event))
              unless (find source-id expanded :key (lambda (row) (gethash "id" row))
                          :test #'eql)
                do (push (kgfs-event
                          source-id
                          (if (oddp source-id) "agent-message" "user-message")
                          (obj "text" (format nil "Original utterance ~d." source-id)))
                         expanded))))
    (knowledge-graph-formation-select-episode-source
   expanded "source-agent" "source-persona"
   (lambda (event) (declare (ignore event)) candidates))))

(format t "~%== KG2 episode source selection ==~%")

(let* ((source-ids (coerce (loop for id from 1000 below 1080 collect id)
                           'vector))
       (episode
         (kgfs-event
          1100 "conversation-episode-sealed"
          (obj "schema_version" 1 "episode_id" "episode:wide"
               "persona_id" "source-persona"
               "first_event_id" 1000 "last_event_id" 1079
               "first_timestamp" 4000001000 "last_timestamp" 4000001079
               "source_event_ids" source-ids
               "synopsis" "Wide historical episode." "subjects" #("history")
               "entities" #() "retrieval_cues" #("wide source")
               "broader_categories" #("knowledge")
               "unresolved_threads" #()
               "protocol_revision" "recursive-conversation-episode-v1"
               "projection_revision" "conversation-episodes-v2"
               "sealed_at" 4000001100)))
       (packet (kgfs-select (list episode))))
  (kgfs-check "wide historical packet retains root plus eighty utterance ids"
              (= 81 (length (gethash "source_event_ids" packet))))
  (kgfs-check "wide historical packet retains all eighty evidence records"
              (= 80 (length (gethash "evidence_records" packet)))))

(let* ((newer (kgfs-episode 20 "episode:newer" "Newer episode."))
       (older (kgfs-episode 10 "episode:older" "Older episode."))
       (packet (kgfs-select (list newer older))))
  (kgfs-check "oldest uncovered sealed episode is selected deterministically"
              (and (equalp #(10 8 9) (gethash "source_event_ids" packet))
                   (equalp #( "episode:older")
                           (gethash "source_episode_ids" packet))))
  (kgfs-check "provider evidence is exact original dialogue rather than synopsis"
              (let ((evidence (gethash "evidence_records" packet)))
                (and (= 2 (length evidence))
                     (every (lambda (row)
                              (member (gethash "kind" row)
                                      '("original-utterance"
                                        "prior-agent-utterance")
                                      :test #'string=))
                            evidence)
                     (notany (lambda (row)
                               (string= "Older episode."
                                        (gethash "text" row "")))
                             evidence)))))

(let* ((older (kgfs-episode 10 "episode:older" "Older episode."))
       (newer (kgfs-episode 20 "episode:newer" "Newer episode."))
       (opened (kgfs-open 30 "episode:older"))
       (packet (kgfs-select (list newer opened older))))
  (kgfs-check "any prior durable open prevents repeated source churn"
              (equalp #( "episode:newer")
                      (gethash "source_episode_ids" packet))))

(let* ((episode (kgfs-episode 10 "episode:legacy" "Legacy episode."))
       (opened (kgfs-open 20 "episode:legacy"))
       (sealed-v1 (kgfs-terminal 30 20 1))
       (packet (kgfs-select (list episode opened sealed-v1))))
  (kgfs-check "successful legacy formation is reconsidered by v2"
              (equalp #( "episode:legacy")
                      (gethash "source_episode_ids" packet))))

(let* ((episode (kgfs-episode 10 "episode:verified" "Verified episode."))
       (opened (kgfs-open 20 "episode:verified"))
       (sealed-v3 (kgfs-terminal 30 20 3)))
  (kgfs-check "successful current formation remains durably covered"
              (null (kgfs-select (list episode opened sealed-v3)))))

(let* ((episode (kgfs-episode 10 "episode:failed" "Failed episode."))
       (opened (kgfs-open 20 "episode:failed"))
       (attempt (kgfs-attempt 21 20))
       (failed (kgfs-terminal 30 20 2 t)))
  (kgfs-check "one current provider failure remains retryable"
              (hash-table-p
               (kgfs-select (list episode opened attempt failed)))))

(let* ((episode (kgfs-episode 10 "episode:bounded-failure" "Failed episode."))
       (events
         (list episode
               (kgfs-open 20 "episode:bounded-failure")
               (kgfs-attempt 21 20) (kgfs-terminal 22 20 3 t)
               (kgfs-open 30 "episode:bounded-failure")
               (kgfs-attempt 31 30) (kgfs-terminal 32 30 3 t)
               (kgfs-open 40 "episode:bounded-failure")
               (kgfs-attempt 41 40) (kgfs-terminal 42 40 3 t))))
  (kgfs-check "three current failures bound quiet-loop retry churn"
              (null (kgfs-select events))))

(let* ((episode (kgfs-episode 10 "episode:old-failure" "Old failure."))
       (opened (kgfs-open 20 "episode:old-failure"))
       (attempt (kgfs-attempt 21 20 "recursive-knowledge-graph-formation-v2"))
       (failed (kgfs-terminal 30 20 2 t))
       (packet (kgfs-select (list episode opened attempt failed))))
  (kgfs-check "older protocol failure is reconsidered once after repair"
              (equalp #( "episode:old-failure")
                      (gethash "source_episode_ids" packet))))

(let ((packet
        (kgfs-select
         (list (kgfs-episode 10 "episode:foreign" "Foreign." "other-persona")
               (kgfs-episode 20 "episode:local" "Local.")))))
  (kgfs-check "foreign persona episodes are ineligible"
              (equalp #( "episode:local")
                      (gethash "source_episode_ids" packet))))

(let* ((candidate
       (obj "node_id" "kgf:entity:current" "kind" "person"
              "label" "Operator" "aliases" #("operator")
              "classifications" #("operator")
              "participant_role" "operator"))
       (packet (kgfs-select
                (list (kgfs-episode 10 "episode:candidate" "Candidate."))
                (vector candidate))))
  (kgfs-check "verified candidate descriptors pass through unchanged"
              (and (knowledge-graph-formation-source-packet-valid-p packet)
                   (eq candidate
                       (aref (gethash "eligible_existing_nodes" packet) 0)))))

(let* ((episode (kgfs-episode 10 "episode:supplemented" "Supplemented."))
       (memory (kgfs-memory 40 "memory:40" "The operator stated an exact fact."))
       (events (list episode memory))
       (packet
         (let ((expanded (copy-list events)))
           (loop for source-id across
                   (gethash "source_event_ids" (gethash "payload" episode))
                 do (push (kgfs-event source-id "user-message"
                                      (obj "text" "Related exact utterance."))
                          expanded))
           (knowledge-graph-formation-select-episode-source
            expanded "source-agent" "source-persona" (lambda (event)
              (declare (ignore event)) #())
            :supplemental-memory-selector-fn
            (lambda (selected all-events)
              (declare (ignore selected all-events)) (vector memory))))))
  (kgfs-check "grounded lived memory can enter through the bounded source seam"
              (and (equalp #( "memory:40")
                           (gethash "source_memory_node_ids" packet))
                   (find 40 (gethash "source_event_ids" packet) :test #'eql)
                   (= 3 (length (gethash "evidence_records" packet)))
                   (string= "memory-event:40"
                            (gethash "source_id"
                                     (aref (gethash "evidence_records" packet)
                                           2))))))

(let* ((episode (kgfs-episode 10 "episode:semantic-bridge"
                              "Planning for a long-distance outing."))
       (relevant (kgfs-memory 70 "memory:touring-equipment"
                              "The operator owns a basalt-grey touring bicycle named Northstar."))
       (unrelated (kgfs-memory 71 "memory:tea-preference"
                               "The operator prefers jasmine tea."))
       (report
         (knowledge-graph-formation-select-supplemental-memories
          episode
          (vector
           (kgfs-supplemental-candidate relevant "memory:touring-equipment"
                                        0 910 120 0)
           (kgfs-supplemental-candidate unrelated "memory:tea-preference"
                                        30 80 0 0)))))
  (kgfs-check "semantic vocabulary gap selects the useful grounded candidate"
              (let ((selected (gethash "selected" report)))
                (and (string= "complete" (gethash "status" report))
                     (= 1 (length selected))
                     (= 70 (gethash "event_id" (aref selected 0)))
                     (= 940 (gethash "relevance_score_milli"
                                     (aref selected 0))))))
  (kgfs-check "unrelated personal memory is not attached"
              (= 1 (gethash "selected_count" report))))

(let* ((episode (kgfs-episode 10 "episode:discovery"
                              "Planning for a long-distance outing."))
       (source (kgfs-event 8 "user-message"
                           (obj "text" "What should I pack for the long ride?")))
       (memory (kgfs-memory 70 "memory:touring-equipment"
                            "The operator owns a touring bicycle."))
       (captured nil))
  (multiple-value-bind (candidates receipt)
      (knowledge-graph-formation-discover-supplemental-memory-candidates
       episode (list episode source memory)
       (lambda (query &key k mode candidate-strategy require-grounded)
         (setf captured (list query k mode candidate-strategy require-grounded))
         (values
          (list (obj "id" "memory:touring-equipment" "similarity" 0.91d0
                     "candidate_sources" #( "semantic" )))
          (obj "strategy" "hybrid-explicit" "union_candidate_count" 1
               "returned_count" 1 "database_write_count" 0))))
    (kgfs-check "discovery derives a general episode query and uses safe hybrid search"
                (and (search "long ride" (first captured) :test #'char-equal)
                     (= 50 (second captured))
                     (eq :conversation (third captured))
                     (eq :hybrid-explicit (fourth captured))
                     (fifth captured)))
    (kgfs-check "discovery maps semantic retrieval to the baseline event"
                (and (string= "complete" (gethash "status" receipt))
                     (= 1 (length candidates))
                     (= 70 (gethash "event_id" (aref candidates 0)))
                     (= 910 (gethash "semantic_score_milli"
                                     (aref candidates 0)))))))

(let* ((episode (kgfs-episode 10 "episode:incomplete-discovery" "Recall."))
       (memory (kgfs-memory 72 "memory:known" "Known memory.")))
  (multiple-value-bind (candidates receipt)
      (knowledge-graph-formation-discover-supplemental-memory-candidates
       episode (list episode memory)
       (lambda (query &rest arguments)
         (declare (ignore query arguments))
         (values
          (list (obj "id" "memory:known" "similarity" 0.1d0
                     "candidate_sources" #( "lexical" )
                     "lexical_tier" 1 "lexical_coverage" 0.5d0)
                (obj "id" "memory:not-in-baseline" "similarity" 0.9d0
                     "candidate_sources" #( "semantic" )))
          (obj "union_candidate_count" 3 "returned_count" 2
               "database_write_count" 0))))
    (kgfs-check "lexical evidence becomes an explicit relevance signal"
                (= 500 (gethash "lexical_score_milli" (aref candidates 0))))
    (kgfs-check "truncation and unmapped rows make discovery incomplete"
                (and (string= "incomplete" (gethash "status" receipt))
                     (= 1 (gethash "unmapped_candidate_count" receipt))))))

(kgfs-check
 "discovery rejects search without a complete zero-write receipt"
 (handler-case
     (progn
       (knowledge-graph-formation-discover-supplemental-memory-candidates
        (kgfs-episode 10 "episode:writes" "Recall.") nil
        (lambda (query &rest arguments)
          (declare (ignore query arguments))
          (values nil (obj "union_candidate_count" 0 "returned_count" 0
                           "database_write_count" 1))))
       nil)
   (error () t)))

(let* ((episode (kgfs-episode 10 "episode:bounded" "Bounded selection."))
       (one (kgfs-memory 80 "memory:a" "First relevant fact."))
       (two (kgfs-memory 81 "memory:b" "Second relevant fact."))
       (report
         (knowledge-graph-formation-select-supplemental-memories
          episode
          (vector (kgfs-supplemental-candidate two "memory:b" 700 0 0 0)
                  (kgfs-supplemental-candidate one "memory:a" 700 0 0 0))
          :candidate-status "incomplete" :maximum 1)))
  (kgfs-check "stable ordering and clipping are explicit"
              (and (string= "incomplete" (gethash "status" report))
                   (equalp #( "candidate-discovery-incomplete"
                              "selection-bound-clipped")
                           (gethash "incompleteness_reasons" report))
                   (= 80 (gethash "event_id"
                                  (aref (gethash "selected" report) 0))))))

(let* ((episode (kgfs-episode 10 "episode:separation" "Authority separation."))
       (ungrounded (kgfs-memory 90 "memory:untrusted" "Plausible but ungrounded."
                                :grounding "unclassified"))
       (report
         (knowledge-graph-formation-select-supplemental-memories
          episode
          (vector (kgfs-supplemental-candidate
                   ungrounded "memory:untrusted" 900 0 0 0))))
       (selected-event
         (gethash "event" (aref (gethash "selected" report) 0))))
  (kgfs-check "relevance selection does not impersonate provenance authority"
              (handler-case
                  (progn
                    (knowledge-graph-formation-grounded-memory-evidence
                     selected-event "source-agent" "source-persona")
                    nil)
                (error () t))))

(let ((bad (kgfs-memory 50 "memory:50" "An unsupported memory."
                         :grounding "unclassified")))
  (kgfs-check "ungrounded supplemental memory fails closed"
              (handler-case
                  (progn
                    (knowledge-graph-formation-grounded-memory-evidence
                     bad "source-agent" "source-persona")
                    nil)
                (error () t))))

(let ((packet (kgfs-select nil)))
  (kgfs-check "empty authority is idle" (null packet)))

(handler-case
    (progn
      (kgfs-select
       (list (kgfs-episode 10 "episode:bad-candidate" "Bad candidate."))
       (vector (obj "node_id" "invented")))
      (kgfs-check "malformed candidate set fails closed" nil))
  (error () (kgfs-check "malformed candidate set fails closed" t)))

(format t "~%KG2 source selection: ~d passed, ~d failed.~%"
        *kgfs-pass* *kgfs-fail*)
(when (plusp *kgfs-fail*) (uiop:quit 1))
