;;;; personal-recall-integration-tests.lisp -- first-provider recall path.
;;;; harness: full-system

(in-package :agent)

(defvar *personal-recall-test-pass* 0)
(defvar *personal-recall-test-fail* 0)
(defvar *personal-recall-provider-calls* 0)
(defvar *personal-recall-graph-query* nil)
(defvar *agent-id* "personal-recall-fixture-agent")

(defun personal-recall-check (name condition)
  (if condition
      (progn (incf *personal-recall-test-pass*) (format t "PASS ~a~%" name))
      (progn (incf *personal-recall-test-fail*) (format t "FAIL ~a~%" name))))

(defun personal-recall-record (id content evidence)
  (obj "source_id" id "content" content
       "provenance"
       (obj "descriptor_id" id "descriptor_event_id" (aref evidence 0)
            "evidence_event_ids" evidence)))

(defun personal-recall-budget-profile ()
  (let* ((document
           (shasht:read-json
            (uiop:read-file-string
             (merge-pathnames "config/conscious-context-profiles.json"
                               (symbol-value '*pai-root*))))))
    (gethash "solicited-conversation-dev" (gethash "profiles" document))))

(defun personal-recall-message-event (id type text timestamp)
  (let ((event
          (obj "id" id "agent_id" *agent-id* "type" type
               "timestamp" timestamp "payload"
               (obj "text" text "channel" "terminal"
                    "metadata"
                    (obj "source" "q4.5-conversation"
                         "persona_id" "personal-recall-fixture-persona")))))
    (when (string= type "agent-message")
      (setf (gethash "caused_by" event) (1- id)))
    event))

(defun personal-recall-sealed-event
    (id episode-id first-id last-id synopsis cues timestamp)
  (obj "id" id "agent_id" *agent-id*
       "type" "conversation-episode-sealed" "timestamp" timestamp
       "payload"
       (obj "schema_version" 1 "episode_id" episode-id
            "persona_id" "personal-recall-fixture-persona"
            "first_event_id" first-id "last_event_id" last-id
            "first_timestamp" (- timestamp 60) "last_timestamp" timestamp
            "source_event_ids" (vector first-id last-id)
            "synopsis" synopsis
            "subjects" cues "entities" cues "retrieval_cues" cues
            "broader_categories" cues "unresolved_threads" #())))

(defun personal-recall-empty-projection ()
  (obj "relevant_shared_memory" #()
       "relevant_shared_memory_candidates" #()
       "memory_retrieval"
       (obj "candidate_count" 0 "eligible_count" 0
            "evidence_candidate_count" 0 "database_write_count" 0)))

(defun personal-recall-memory-projection (rows)
  (obj "relevant_shared_memory" (subseq rows 0 (min 3 (length rows)))
       "relevant_shared_memory_candidates" rows
       "memory_retrieval"
       (obj "candidate_count" (length rows) "eligible_count" (length rows)
            "evidence_candidate_count" (length rows)
            "database_write_count" 0)))

(defun personal-recall-memory-row (id content origin event-id rank)
  (obj "id" id "kind" "observation" "content" content
       "origin_class" origin
       "epistemic_status"
       (if (string= origin "lived-user") "user-report" "external-source")
       "grounding_status" "grounded" "label" "Grounded fixture evidence"
       "similarity" (- 0.99d0 (* rank 0.01d0))
       "lexical_tier" 1 "source_event_id" event-id))

(defparameter *personal-recall-graph-records*
  (vector
   (personal-recall-record
    "graph-context:facts:pet"
    "Reviewed graph fact: the operator's pets are named Pet-A and Pet-B."
    #(101 11))
   (personal-recall-record
    "graph-context:facts:spouse"
    "Reviewed graph fact: the operator's spouse is named Partner-A."
    #(102 12))
   (personal-recall-record
    "graph-context:facts:health"
    "Reviewed historical graph fact: the operator reported health condition Condition-A earlier this year; current validity is unknown."
    #(103 13))
   (personal-recall-record
    "graph-context:facts:children"
    "Reviewed graph facts: the operator's children are named Child-A and Child-B."
    #(104 14))
   (personal-recall-record
    "graph-context:entities:partner"
    "Reviewed entity reference Partner-A; this is not a relationship assertion."
    #(105 15))))

(defparameter *personal-recall-graph-metadata*
  (vector
   (obj "source_id" "graph-context:facts:pet" "source_kind" "graph-fact"
        "operator_support" t "speaker_basis" "reviewed-graph-fact"
        "local_rank" 1)
   (obj "source_id" "graph-context:facts:spouse" "source_kind" "graph-fact"
        "operator_support" t "speaker_basis" "reviewed-graph-fact"
        "local_rank" 2)
   (obj "source_id" "graph-context:facts:health" "source_kind" "graph-fact"
        "operator_support" t "speaker_basis" "reviewed-graph-fact"
        "local_rank" 3)
   (obj "source_id" "graph-context:facts:children" "source_kind" "graph-fact"
        "operator_support" t "speaker_basis" "reviewed-graph-fact"
        "local_rank" 4)
   (obj "source_id" "graph-context:entities:partner"
        "source_kind" "graph-entity" "operator_support" nil
        "speaker_basis" "reviewed-graph-discovery" "local_rank" 1)))

(defun personal-recall-graph-port (records metadata)
  (lambda (frame semantic episode character-budget)
    (declare (ignore semantic episode character-budget))
    (setf *personal-recall-graph-query* (%kgac-query frame))
    (values records
            (obj "schema_version" 1
                 "status" (if (plusp (length records)) "selected" "empty")
                 "selected_count" (length records)
                 "rendered_characters"
                 (loop for row across records sum (length (gethash "content" row)))
                 "database_write_count" 0 "provider_calls" 0)
            metadata)))

(defun personal-recall-assemble (question &key graph-records graph-metadata
                                           memory-projection episodic-events)
  (let* ((profile (personal-recall-budget-profile))
         (current-id 900)
         (current (personal-recall-message-event
                   current-id "user-message" question 3994416000))
         (events (list current))
         (*conscious-conversation-memory-projection-fn*
           (lambda (prompt)
             (declare (ignore prompt))
             (or memory-projection (personal-recall-empty-projection))))
         (*conscious-conversation-graph-context-fn*
           (personal-recall-graph-port (or graph-records #())
                                       (or graph-metadata #())))
         (*conscious-recursive-mind-episodic-memory-enabled-p* t)
         (*turn-capture-context* (obj "as_of" 3994416000))
         (spec
           (%conversation-assembly-spec
            events current-id question *agent-id* profile "local" "terminal"
            nil nil (or episodic-events events)))
         (context
           (make-conscious-assembly-context
            :pulse-id "pulse:personal-recall" :purpose "respond"
            :audience (gethash "audience" spec)
            :runtime-revision "personal-recall-fixture"
            :conscious-state-revision 1 :clock-identity "fixture-clock"
            :total-character-budget (gethash "total_character_budget" spec)
            :section-character-budgets
            (gethash "section_character_budgets" spec)
            :sections (gethash "sections" spec)
            :eligible-evidence-ids (gethash "eligible_evidence_ids" spec)
            :available-tools (gethash "available_tools" spec)
            :permitted-proposal-kinds
            (gethash "permitted_proposal_kinds" spec)
            :publication-constraints (gethash "publication_constraints" spec)
            :remaining-budget (gethash "remaining_budget" spec)
            :pre-render-refusals (gethash "pre_render_refusals" spec #())))
         (assembled
           (conscious-context-assemble
            (obj "state_revision" 1
                 "composition_hash" "personal-recall-fixture")
            context))
         (messages (%conversation-model-messages assembled nil question))
         (payload (%conversation-http-request-payload
                   messages "fixture-model" 0.0d0)))
    (values spec assembled payload)))

(defun personal-recall-payload-memory-rows (payload)
  (let* ((messages (gethash "messages" payload))
         (envelope (shasht:read-json (gethash "content" (aref messages 1))))
         (rows (gethash "context_data" envelope)))
    (remove-if-not
     (lambda (row) (string= "memory-bundles" (gethash "section" row "")))
     (coerce rows 'list))))

(defun personal-recall-payload-memory-text (payload)
  (shasht:write-json (personal-recall-payload-memory-rows payload) nil))

(format t "~%== Personal recall first-provider qualification ==~%")

(conscious-conversation-set-persona-profile
 "personal-recall-fixture-persona" 1 "Fixture persona identity."
 "Fixture persona voice: concise and evidence-grounded." :source "test-fixture")

(let ((recent-original (and (fboundp 'event-recent-conversation-events)
                            (symbol-function 'event-recent-conversation-events)))
      (private-original
        (and (fboundp 'conscious-recursive-private-cognition-context-records)
             (symbol-function
              'conscious-recursive-private-cognition-context-records))))
  (unwind-protect
       (progn
         ;; Keep every read on the supplied disposable fixture authority.
         (setf (symbol-function 'event-recent-conversation-events)
               (lambda (&rest ignored) (declare (ignore ignored)) nil)
               (symbol-function
                'conscious-recursive-private-cognition-context-records)
               (lambda (&key &allow-other-keys)
                 (vector
                  (obj "source_id" 777
                       "content"
                       "Private motive about an unrelated spouse hypothesis."))))

         (dolist (case
                  '(("What are my pets' names?" "Pet-A" "Pet-B")
                    ("What is my spouse's name?" "Partner-A" nil)
                    ("What health condition did I have earlier this year?"
                     "Condition-A" "current validity is unknown")
                    ("What are my children's names?" "Child-A" "Child-B")))
           (destructuring-bind (question first second) case
             (multiple-value-bind (spec assembled payload)
                 (personal-recall-assemble
                  question :graph-records *personal-recall-graph-records*
                  :graph-metadata *personal-recall-graph-metadata*)
               (declare (ignore assembled))
               (let ((wire (personal-recall-payload-memory-text payload))
                     (report *conscious-conversation-turn-memory-report*))
                  (personal-recall-check
                   (format nil "preformed graph exposes ~a in first provider payload"
                           first)
                   (and (search first wire :test #'char-equal)
                       (or (null second)
                           (search second wire :test #'char-equal))
                       (= 1 (gethash "graph_context_selected_count" report))
                        (zerop (gethash "database_write_count" report -1))
                        (zerop *personal-recall-provider-calls*)
                        (zerop (length (gethash "available_tools" spec)))))
                  (format t (concatenate 'string
                                         "FIRST-PROVIDER case=~a selected=~d "
                                         "graph=~d chars=~d tools=~d "
                                         "provider_calls=~d writes=~d~%")
                          first
                          (gethash "recall_selected_count" report 0)
                          (gethash "graph_context_selected_count" report 0)
                          (gethash "rendered_characters" report 0)
                          (length (gethash "available_tools" spec))
                          *personal-recall-provider-calls*
                          (gethash "database_write_count" report -1))))))

         (personal-recall-check
          "public graph query excludes unrelated private motive"
          (and (search "child" *personal-recall-graph-query*)
               (null (search "hypothesis" *personal-recall-graph-query*))))

         ;; The production raw and sealed selectors run against one synthetic
         ;; event history. Eight recent failed-answer echoes cannot consume the
         ;; sealed source's discovery or final relevance allowance.
         (let ((history nil))
           (loop for index from 0 below 8
                 for user-id = (+ 200 (* index 2))
                 for assistant-id = (1+ user-id)
                 do (push (personal-recall-message-event
                           user-id "user-message"
                           "What are my children's names?" (+ 3994400000 index))
                          history)
                    (push (personal-recall-message-event
                           assistant-id "agent-message"
                           "I could not find your children's names."
                           (+ 3994400100 index))
                          history))
           (push (personal-recall-sealed-event
                  500 "episode:children-positive" 50 51
                  "Earlier, the operator reported that their children are Child-A and Child-B."
                  #( "children" "child" "names") 3994300000)
                 history)
           (multiple-value-bind (spec assembled payload)
               (personal-recall-assemble
                "What are my children's names?"
                :episodic-events (nreverse history))
             (declare (ignore spec assembled))
             (let ((wire (personal-recall-payload-memory-text payload))
                   (report *conscious-conversation-turn-memory-report*))
               (personal-recall-check
                "sealed positive survives a full raw pool and reaches first payload"
                (and (search "Child-A" wire)
                     (= 8 (gethash "episodic_raw_selected_count" report))
                     (= 1 (gethash "episodic_sealed_selected_count" report))
                     (<= 1 (gethash "episodic_selected_count" report) 4)))))))

         ;; Missing graph: the production semantic projection adapter and
         ;; global selector retain the historical operator report, while the
         ;; recent assistant-side absence echo supplies no operator fact.
         (let ((projection
                 (personal-recall-memory-projection
                  (vector
                   (personal-recall-memory-row
                    "assistant-echo"
                    "A recent assistant said it could not find the health condition."
                    "external-source" 601 1)
                   (personal-recall-memory-row
                    "operator-condition"
                    "The operator reported historical health condition Condition-B; current validity is unknown."
                    "lived-user" 602 2)))))
           (multiple-value-bind (spec assembled payload)
               (personal-recall-assemble
                "What health condition did I have earlier?"
                :memory-projection projection)
             (declare (ignore spec assembled))
             (let ((wire (personal-recall-payload-memory-text payload))
                   (report *conscious-conversation-turn-memory-report*))
               (personal-recall-check
                "missing graph uses semantic operator evidence without graph claim"
                (and (search "Condition-B" wire :test #'char-equal)
                     (null (search "could not find" wire :test #'char-equal))
                     (= 1 (gethash "selected_count" report))
                     (zerop (gethash "graph_context_selected_count" report)))))))

         (multiple-value-bind (spec assembled payload)
             (personal-recall-assemble
              "What is their spouse's name?"
              :graph-records *personal-recall-graph-records*
              :graph-metadata *personal-recall-graph-metadata*)
           (declare (ignore spec assembled))
           (personal-recall-check
            "ambiguous third-person spouse query exposes no operator fact"
            (null (search "Partner-A"
                          (personal-recall-payload-memory-text payload)
                          :test #'char-equal))))

         (multiple-value-bind (spec assembled payload)
             (personal-recall-assemble
              "What is my blood type?"
              :graph-records *personal-recall-graph-records*
              :graph-metadata *personal-recall-graph-metadata*)
           (declare (ignore spec assembled))
           (personal-recall-check
            "unknown sensitive query exposes no unrelated personal dossier"
            (null (personal-recall-payload-memory-rows payload))))
    (if recent-original
        (setf (symbol-function 'event-recent-conversation-events)
              recent-original)
        (fmakunbound 'event-recent-conversation-events))
    (if private-original
        (setf (symbol-function
               'conscious-recursive-private-cognition-context-records)
              private-original)
        (fmakunbound 'conscious-recursive-private-cognition-context-records))))

(format t "~%PERSONAL-RECALL INTEGRATION TESTS: ~d passed, ~d failed.~%"
        *personal-recall-test-pass* *personal-recall-test-fail*)
(when (plusp *personal-recall-test-fail*) (uiop:quit 1))
