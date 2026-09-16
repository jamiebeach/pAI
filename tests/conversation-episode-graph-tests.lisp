;;;; conversation-episode-graph-tests.lisp -- pure V5 Slice 2 substrate.

(in-package :agent)

(ql:quickload '(:ironclad) :silent t)

(load (test-source "conversation-episode-graph.lisp"))

(defvar *ceg-passed* 0)
(defvar *ceg-failed* 0)

(defun ceg-check (name condition)
  (if condition
      (progn (incf *ceg-passed*) (format t "PASS ~a~%" name))
      (progn (incf *ceg-failed*) (format t "FAIL ~a~%" name))))

(defun ceg-ts (hour minute)
  (format nil "2026-08-20T~2,'0d:~2,'0d:00Z" hour minute))

(defun ceg-event (id type text timestamp &key caused-by (persona "fixture"))
  (obj "id" id "type" type "agent_id" "episode-fixture"
       "caused_by" (or caused-by :null) "timestamp" timestamp
       "payload"
       (obj "text" text
            "metadata"
            (obj "source" "recursive-mind-v1"
                 "persona_id" persona
                 "thread_id" (format nil "thread:~d" id)))))

(defun ceg-sealed (id episode-id first-id last-id synopsis categories cues
                   &key (persona "fixture") (last-time 1000))
  (obj "id" id "type" "conversation-episode-sealed"
       "agent_id" "episode-fixture" "caused_by" (1- id)
       "timestamp" (ceg-ts 12 0)
       "payload"
       (obj "schema_version" 1 "episode_id" episode-id
            "persona_id" persona "first_event_id" first-id
            "last_event_id" last-id "first_timestamp" (- last-time 100)
            "last_timestamp" last-time
            "source_event_ids" (vector first-id last-id)
            "synopsis" synopsis
            "subjects" (vector) "entities" (vector)
            "retrieval_cues" (coerce cues 'vector)
            "broader_categories" (coerce categories 'vector)
            "unresolved_threads" (vector))))

(defun ceg-historical-event (id source-id type text timestamp &key caused-by)
  (obj "id" id "type"
       (if (string= type "user-message")
           "historical-user-message-imported"
           "historical-agent-message-imported")
       "agent_id" "episode-fixture" "caused_by" (or caused-by :null)
       "timestamp" timestamp "payload"
       (obj "text" text "metadata"
            (obj "source" "historical-agent-migration-v1"
                 "persona_id" "fixture" "source_agent_id" "source-agent"
                 "source_event_id" source-id "source_event_type" type
                 "source_event_sha256"
                 "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef"
                 "source_caused_by" :null))))

(format t "~%== conversation episode graph ==~%")

(let* ((events
         (list (ceg-historical-event 101 7 "user-message"
                                     "Historical operator statement."
                                     (ceg-ts 7 0))
               (ceg-historical-event 102 8 "agent-message"
                                     "Historical source-agent response."
                                     (ceg-ts 7 1) :caused-by 101)))
       (episodes
         (conversation-episode-candidates
          events "episode-fixture" "fixture")))
  (ceg-check "closed migration receipts form a historical episode"
             (and (= 1 (length episodes))
                  (equalp #(101 102)
                          (gethash "source_event_ids" (aref episodes 0)))
                  (equalp #("operator" "assistant")
                          (map 'vector
                               (lambda (row) (gethash "role" row))
                               (gethash "turns" (aref episodes 0))))))
  (setf (gethash "source_event_sha256"
                 (gethash "metadata" (gethash "payload" (first events))))
        "invalid")
  (ceg-check "malformed migration provenance is not public dialogue"
             (zerop (length
                     (conversation-episode-candidates
                      events "episode-fixture" "fixture")))))

(let* ((events
         (list
          (ceg-event 1 "user-message" "early question" (ceg-ts 8 0))
          (ceg-event 2 "agent-message" "early answer" (ceg-ts 8 1)
                     :caused-by 1)
          ;; More than two hours creates a new candidate without phrase or
          ;; model-based boundary detection.
          (ceg-event 3 "user-message" "later question" (ceg-ts 10 30))
          (ceg-event 4 "agent-message" "later answer" (ceg-ts 10 31)
                     :caused-by 3)
          ;; An unanswered message is durable but not sealed as a completed
          ;; conversational episode.
          (ceg-event 5 "user-message" "unanswered" (ceg-ts 10 32))))
       (episodes
         (conversation-episode-candidates
          events "episode-fixture" "fixture")))
  (ceg-check "elapsed gap partitions completed dialogue deterministically"
             (and (= 2 (length episodes))
                  (equalp #(1 2) (gethash "source_event_ids"
                                          (aref episodes 0)))
                  (equalp #(3 4) (gethash "source_event_ids"
                                          (aref episodes 1)))))
  (ceg-check "unanswered user evidence is not laundered into an episode"
             (not (find 5
                        (loop for episode across episodes
                              append (coerce (gethash "source_event_ids" episode)
                                             'list)))))
  (ceg-check "different persona is excluded from first-person projection"
             (zerop (length
                     (conversation-episode-candidates
                      events "episode-fixture" "other"))))
  (let ((candidate
          (conversation-episode-next-sealable
           events "episode-fixture" "fixture"
           (encode-universal-time 0 0 11 20 8 2026 0))))
    (ceg-check "oldest closed range is selected for quiet sealing"
               (and candidate
                    (string= "episode:fixture:1:2"
                             (gethash "episode_id" candidate)))))
  (let* ((sealed
           (ceg-sealed 6 "episode:fixture:1:2" 1 2 "early synopsis"
                       '("general") '("early")))
         (candidate
           (conversation-episode-next-sealable
            (append events (list sealed)) "episode-fixture" "fixture"
            (encode-universal-time 0 0 13 20 8 2026 0))))
    (ceg-check "sealed range is idempotently skipped"
               (and candidate
                    (string= "episode:fixture:3:4"
                             (gethash "episode_id" candidate)))))
  (let* ((failed
           (obj "id" 6 "type" "conversation-episode-seal-failed"
                "agent_id" "episode-fixture" "caused_by" 5
                "payload" (obj "episode_id" "episode:fixture:1:2"
                               "persona_id" "fixture")))
         (candidate
           (conversation-episode-next-sealable
            (append events (list failed)) "episode-fixture" "fixture"
            (encode-universal-time 0 0 13 20 8 2026 0))))
    (ceg-check "failed range remains eligible for a later quiet retry"
               (and candidate
                    (string= "episode:fixture:1:2"
                             (gethash "episode_id" candidate))))))

(let* ((events
         (loop for pair from 0 below 3
               for user-id = (+ 10 (* pair 2))
               for agent-id = (1+ user-id)
               append
               (list
                (ceg-event user-id "user-message"
                           (format nil "bounded topic ~d" pair)
                           (ceg-ts 12 (* pair 2)))
                (ceg-event agent-id "agent-message"
                           (format nil "bounded answer ~d" pair)
                           (ceg-ts 12 (1+ (* pair 2)))
                           :caused-by user-id))))
       (episodes
         (conversation-episode-candidates
          events "episode-fixture" "fixture"
          :max-public-messages 4)))
  (ceg-check "shape pressure partitions only between completed pairs"
             (and (= 2 (length episodes))
                  (equalp #(10 11 12 13)
                          (gethash "source_event_ids" (aref episodes 0)))
                  (equalp #(14 15)
                          (gethash "source_event_ids" (aref episodes 1))))))

(let* ((events
         (list
          (ceg-event 30 "user-message" "earlier renovation"
                     (ceg-ts 8 0))
          (ceg-event 31 "agent-message" "earlier answer"
                     (ceg-ts 8 1) :caused-by 30)
          (ceg-event 32 "user-message"
                     "I am beside a campground fire and smell like campfire smoke"
                     (ceg-ts 10 30))
          (ceg-event 33 "agent-message"
                     "Enjoy the fire. Go smell less like campfire when you get back."
                     (ceg-ts 10 31) :caused-by 32)
          (ceg-sealed 34 "episode:fixture:30:31" 30 31
                      "Earlier home renovation discussion."
                      '("home renovation") '("renovation")
                      :last-time
                      (encode-universal-time 0 1 8 20 8 2026 0))))
       (before (shasht:write-json events nil)))
  (multiple-value-bind (records ids report)
      (conversation-unsealed-dialogue-context-records
       events "episode-fixture" "fixture" "smoke by the fire")
    (ceg-check "unsealed raw fallback recovers exact completed dialogue"
               (and (= 1 (length records))
                    (search "Go smell less like campfire"
                            (gethash "content" (aref records 0)))
                    ;; Selection identities remain one-for-one with records;
                    ;; context admission later derives eligible provenance.
                    (equal '("conversation-raw:32:33") ids)
                    (let ((record (aref records 0)))
                      (and (= 3 (hash-table-count record))
                           (equalp #(32 33)
                                   (gethash "evidence_event_ids"
                                            (gethash "provenance" record)))))
                    (= 31 (gethash "sealed_through_event_id" report))
                    (= 1 (gethash "pending_episode_count" report))
                    (zerop (gethash "database_write_count" report)))))
  (multiple-value-bind (records ids report)
      (conversation-unsealed-dialogue-context-records
       events "episode-fixture" "other" "smoke by the fire")
    (declare (ignore ids report))
    (ceg-check "unsealed raw fallback is persona scoped"
               (zerop (length records))))
  (ceg-check "unsealed raw fallback is a zero-mutation read"
             (string= before (shasht:write-json events nil))))

(let ((events
        (list
         (ceg-event 40 "user-message" "fixture concise question"
                    (ceg-ts 11 0))
         (ceg-event 41 "agent-message" "fixture concise answer"
                    (ceg-ts 11 1) :caused-by 40)
         (ceg-event 42 "user-message"
                    (format nil "fixture oversized question ~a"
                            (make-string 700 :initial-element #\x))
                    (ceg-ts 11 2))
         (ceg-event 43 "agent-message" "fixture oversized answer"
                    (ceg-ts 11 3) :caused-by 42))))
  (multiple-value-bind (records ids report)
      (conversation-unsealed-dialogue-context-records
       events "episode-fixture" "fixture" "fixture"
       :maximum 1 :character-budget 800 :record-character-limit 500)
    (ceg-check "raw recall refuses an oversized whole pair and keeps scanning"
               (and (= 1 (length records))
                    (equal '("conversation-raw:40:41") ids)
                    (search "fixture concise answer"
                            (gethash "content" (aref records 0)))
                    (= 1 (gethash "budget_refusal_count" report))))))

(let* ((health
         (ceg-sealed
          20 "episode:health" 10 11
          "The operator discussed iron deficiency, improving lab values, and tendon recovery."
          '("health issue" "physical wellbeing")
          '("iron" "hemoglobin" "tendon recovery") :last-time 1000))
       (related
         (ceg-sealed
          21 "episode:recovery" 12 13
          "A later conversation discussed gradual recovery and training load."
          '("physical wellbeing") '("training" "recovery") :last-time 1100))
       (transport
         (ceg-sealed
          22 "episode:transport" 14 15
          "The operator arranged a family member's transportation schedule."
          '("family logistics") '("ride" "schedule") :last-time 9000))
       (technical
         (ceg-sealed
          23 "episode:compiler" 16 17
          "A private technical discussion covered compiler warnings."
          '("software engineering") '("compiler" "warnings") :last-time 8000))
       (events (list health related transport technical))
       (episodes
         (conversation-episode-project
          events "episode-fixture" "fixture"))
       (before (shasht:write-json episodes nil))
       (public (conversation-episode-recall episodes "that health issue"))
       (private (conversation-episode-recall episodes "compiler warnings")))
  (ceg-check "projected episode keeps exact source provenance"
             (equalp #(10 11)
                     (gethash "source_event_ids" (aref episodes 0))))
  (ceg-check "broad human phrasing bridges to a specific episode"
             (string= "episode:health"
                      (gethash "episode_id" (aref public 0))))
  (ceg-check "one-hop concept graph admits a related prior episode"
             (find "episode:recovery" public
                   :key (lambda (row) (gethash "episode_id" row))
                   :test #'string=))
  (ceg-check "unrelated recent episode does not win by recency"
             (not (find "episode:transport" public
                        :key (lambda (row) (gethash "episode_id" row))
                        :test #'string=)))
  (ceg-check "a private technical frame selects different recollection"
             (and (plusp (length private))
                  (string= "episode:compiler"
                           (gethash "episode_id" (aref private 0)))))
  (ceg-check "graph recall is a zero-mutation read"
             (string= before (shasht:write-json episodes nil)))
  (let ((semantic
          (conversation-episode-recall
           episodes "general complaint" :maximum 1
           :semantic-score-fn
           (lambda (cue episode)
             (declare (ignore cue))
             (if (string= "episode:health" (gethash "episode_id" episode))
                 1d0 0d0)))))
    (ceg-check "read-only semantic scorer composes with lexical graph recall"
               (and (= 1 (length semantic))
                    (string= "episode:health"
                             (gethash "episode_id" (aref semantic 0))))))
  (multiple-value-bind (records ids report)
      (conversation-episode-context-records
       episodes "health issue" :character-budget 800 :record-character-limit 600)
    (ceg-check "bounded context labels recollection as non-exhaustive"
               (and (plusp (length records))
                    (search "non-exhaustive"
                            (gethash "content" (aref records 0)))
                    (= (length records) (length ids))
                    (= (length records) (gethash "selected_count" report))))))

(let* ((short
         (ceg-sealed
          50 "episode:budget:short" 44 45
          "Fixture concise recollection."
          '("fixture") '("fixture") :last-time 1000))
       (long
         (ceg-sealed
          51 "episode:budget:long" 46 47
          (format nil "Fixture oversized recollection ~a"
                  (make-string 700 :initial-element #\x))
          '("fixture") '("fixture") :last-time 1100))
       (episodes
         (conversation-episode-project
          (list short long) "episode-fixture" "fixture")))
  (multiple-value-bind (records ids report)
      (conversation-episode-context-records
       episodes "fixture" :maximum 1
       :character-budget 800 :record-character-limit 500)
    (ceg-check "sealed recall refuses an oversized whole episode and keeps scanning"
               (and (= 1 (length records))
                    (= 1 (length ids))
                    (search "Fixture concise recollection"
                            (gethash "content" (aref records 0)))
                    (= 1 (gethash "budget_refusal_count" report))))))

(let* ((first
         (ceg-sealed
          80 "episode:graph:first" 70 71
          "First graph materialization fixture."
          '("shared category") '("unique cue" "shared category")
          :last-time 1200))
       (second
         (ceg-sealed
          81 "episode:graph:second" 72 73
          "Second graph materialization fixture."
          '("shared category") '("second cue")
          :last-time 1300))
       (episodes
         (conversation-episode-project
          (list first second) "episode-fixture" "fixture"))
       (rows
         (conversation-episode-graph-materialization
          episodes "episode-fixture" "fixture"))
       (nodes (gethash "nodes" rows))
       (edges (gethash "edges" rows))
       (evidence (gethash "evidence" rows))
       (again
         (conversation-episode-graph-materialization
          episodes "episode-fixture" "fixture")))
  (ceg-check "materialization emits two episode and three concept nodes"
             (and (= 5 (length nodes))
                  (= 2 (count "episode" nodes
                              :key (lambda (row) (gethash "node_kind" row))
                              :test #'string=))
                  (= 3 (count "concept" nodes
                              :key (lambda (row) (gethash "node_kind" row))
                              :test #'string=))))
  (ceg-check "materialization deduplicates episode-concept edges"
             (= 4 (length edges)))
  (ceg-check "materialization retains descriptor and ordered source evidence"
             (and (= 18 (length evidence))
                  (= 6 (count "descriptor" evidence
                              :key (lambda (row) (gethash "evidence_role" row))
                              :test #'string=))))
  (ceg-check "materialization identities and ordering are deterministic"
             (string= (shasht:write-json rows nil)
                      (shasht:write-json again nil))))

(format t "~%~d passed, ~d failed~%" *ceg-passed* *ceg-failed*)
(when (plusp *ceg-failed*) (uiop:quit 1))
