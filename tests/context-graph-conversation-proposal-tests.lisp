;;;; context-graph-conversation-proposal-tests.lisp -- immediate append-only writes.
;;;; harness: full-system

(in-package :agent)

(defun cg-conversation-proposal-arguments (&optional (quote "Child Alpha is my child."))
  (obj
   "entities"
   (vector
    (obj "local_ref" "child_alpha" "kind" "person"
         "label" "Child Alpha" "aliases" #() "classifications" #()
         "identity_action" "NEW" "existing_node_id" :null
         "evidence_status" "direct"
         "evidence_note" "Explicit operator statement in the current turn."))
   "relationships"
   (vector
    (obj "subject_ref" "runtime:operator" "predicate" "parent_of"
         "object_ref" "child_alpha"
         "fact" "The operator is the parent of Child Alpha."
         "quote" quote "polarity" "positive"
         "temporal_character" "standing-disposition"
         "evidence_status" "direct"
         "evidence_note" "Explicit operator statement in the current turn."))))

(assert (member "context-graph-update-proposed"
                *conscious-recursive-thread-event-types* :test #'equal))

(let* ((*conscious-recursive-mind-graph-proposal-fn* (lambda (id) id))
       (schemas (%recursive-tool-schemas nil nil nil))
       (schema
         (find "propose-graph-update" schemas :test #'equal
               :key (lambda (row)
                      (gethash "name" (gethash "function" row)))))
       (arguments (cg-conversation-proposal-arguments)))
  (assert schema)
  (assert (eq arguments
              (%recursive-validate-tool-arguments
               "propose-graph-update" arguments)))
  (assert
   (handler-case
       (progn
         (%recursive-validate-tool-arguments
          "propose-graph-update"
          (obj "entities" #() "relationships" #()))
         nil)
     (error () t))))

(let* ((*conscious-recursive-mind-graph-proposal-fn* (lambda (id) id))
       (bad (cg-conversation-proposal-arguments))
       (entity (aref (gethash "entities" bad) 0)))
  (setf (gethash "identity_action" entity) "LINK_EXISTING"
        (gethash "existing_node_id" entity) :null)
  (handler-case
      (progn
        (%recursive-validate-tool-arguments "propose-graph-update" bad)
        (error "Missing LINK_EXISTING validation refusal"))
    (error (condition)
      (assert (search "LINK_EXISTING"
                      (format nil "~a" condition)))
      (assert (search "runtime:operator"
                      (format nil "~a" condition))))))

;; A validation refusal orders the transcript but is not a completed effect;
;; the model may retry even when it initially repeats the same wire object.
(let* ((arguments (cg-conversation-proposal-arguments))
       (arguments-json (shasht:write-json arguments nil))
       (call
         (obj "id" "tool:retry" "type" "function"
              "function"
              (obj "name" "propose-graph-update"
                   "arguments" arguments-json)))
       (projection
         (obj "tool_name" "propose-graph-update"
              "tool_arguments" arguments
              "transcript"
              (vector
               (obj "role" "assistant" "content" :null
                    "tool_calls" (vector call))
               (obj "role" "tool" "tool_call_id" "tool:retry"
                    "content"
                    "NOT EXECUTED: invalid parameters. Correct the parameters and try again.")))))
  (assert (not (%recursive-consecutive-duplicate-tool-p projection))))

(let* ((agent-id "agent:fixture")
       (persona-id "fixture-persona")
       (*conscious-context-graph-runtime-profile* "reviewed-inference-v9")
       (runtime
         (pai.context-graph:context-graph-runtime-create
          (%ccg-ontology) *knowledge-graph-family-ontology-revision*
          agent-id persona-id))
       (graph (pai.context-graph::context-graph-runtime-graph runtime))
       (source
         (obj "id" 10 "agent_id" agent-id "type" "user-message"
              "timestamp" "2026-09-12T12:00:00Z"
              "payload"
              (obj "text" "Child Alpha is my child."
                   "metadata"
                   (obj "source" "recursive-mind-v1"
                        "persona_id" persona-id))))
       (proposal
         (obj "id" 11 "agent_id" agent-id
              "type" "context-graph-update-proposed" "caused_by" 10
              "payload"
              (obj "schema_version" 1 "source_user_event_id" 10
                   "proposal" (cg-conversation-proposal-arguments)
                   "thread_id" "thread:fixture"
                   "model_call_id" "model:fixture"
                   "tool_call_id" "tool:fixture"
                   "runtime_revision"
                   *conscious-recursive-mind-runtime-revision*
                   "proposed_at" 100)))
       (index (make-hash-table :test #'eql)))
  (setf (gethash 10 index) source (gethash 11 index) proposal)
  (let ((result
          (%ccg-apply-conversation-proposal
           runtime proposal index agent-id persona-id)))
    (assert (equal "applied" (gethash "status" result))
            (result) "~a" (gethash "reason" result))
    (assert (equal "admitted" (gethash "formation_outcome" result)))
    (assert (= 3 (pai.context-graph:context-graph-entity-count graph)))
    (assert (= 1 (pai.context-graph:context-graph-fact-count graph)))
    (assert
     (loop for row being the hash-values of
             (pai.context-graph::context-graph-entities graph)
           thereis (equal "Child Alpha" (gethash "label" row))))
    (let ((self
            (loop for row being the hash-values of
                    (pai.context-graph::context-graph-entities graph)
                  when (equal "active-persona"
                              (gethash "participant_role" row))
                    return row)))
      (assert self)
      (assert (equal "active-persona" (gethash "label" self)))
      (assert (zerop (length (gethash "aliases" self)))))
    (let* ((before (pai.context-graph:context-graph-fact-count graph))
           (foreign (pai.context-graph::%cg-detach proposal)))
      (setf (gethash "id" foreign) 12
            (gethash "agent_id" foreign) "agent:foreign")
      (setf (gethash 12 index) foreign)
      (let ((rejected
              (%ccg-apply-conversation-proposal
               runtime foreign index agent-id persona-id)))
        (assert (equal "rejected" (gethash "status" rejected)))
        (assert (= before
                   (pai.context-graph:context-graph-fact-count graph)))))))

;; An authenticated model proposal is itself a durable active-persona
;; observation.  It can ground an inference without pretending the operator
;; said it, and the reserved persona handle avoids a duplicate self node.
(let* ((agent-id "agent:observation-fixture")
       (persona-id "observation-persona")
       (*conscious-context-graph-runtime-profile* "reviewed-inference-v9")
       (runtime
         (pai.context-graph:context-graph-runtime-create
          (%ccg-ontology) *knowledge-graph-family-ontology-revision*
          agent-id persona-id))
       (graph (pai.context-graph::context-graph-runtime-graph runtime))
       (quote "I observe that I am working on Project Alpha.")
       (arguments
         (obj
          "entities"
          (vector
           (obj "local_ref" "project_alpha" "kind" "project"
                "label" "Project Alpha" "aliases" #()
                "classifications" #() "identity_action" "NEW"
                "existing_node_id" :null "evidence_status" "inference"
                "evidence_note" "Current active-persona observation."))
          "relationships"
          (vector
           (obj "subject_ref" "runtime:active-persona"
                "predicate" "works_on" "object_ref" "project_alpha"
                "fact" "The active persona works on Project Alpha."
                "quote" quote "polarity" "positive"
                "temporal_character" "ongoing-state"
                "evidence_status" "inference"
                "evidence_note" "Reasoned current observation."))))
       (arguments-json (shasht:write-json arguments nil))
       (source
         (obj "id" 20 "agent_id" agent-id "type" "user-message"
              "timestamp" "2026-09-12T13:00:00Z"
              "payload"
              (obj "text" "Use your own observations when appropriate."
                   "metadata"
                   (obj "source" "recursive-mind-v1"
                        "persona_id" persona-id))))
       (model
         (obj "id" 21 "agent_id" agent-id "type" "model-response"
              "caused_by" 20 "timestamp" "2026-09-12T13:00:01Z"
              "payload"
              (obj "model_call_id" "model:observation"
                   "assistant_message"
                   (obj "role" "assistant" "content" :null
                        "tool_calls"
                        (vector
                         (obj "id" "tool:observation" "type" "function"
                              "function"
                              (obj "name" "propose-graph-update"
                                   "arguments" arguments-json)))))))
       (proposal
         (obj "id" 22 "agent_id" agent-id
              "type" "context-graph-update-proposed" "caused_by" 20
              "payload"
              (obj "schema_version" 1 "source_user_event_id" 20
                   "proposal" arguments "thread_id" "thread:observation"
                   "model_call_id" "model:observation"
                   "tool_call_id" "tool:observation"
                   "runtime_revision"
                   *conscious-recursive-mind-runtime-revision*
                   "proposed_at" 200)))
       (index (make-hash-table :test #'eql)))
  (setf (gethash 20 index) source
        (gethash 21 index) model
        (gethash 22 index) proposal)
  (let ((result
          (%ccg-apply-conversation-proposal
           runtime proposal index agent-id persona-id)))
    (assert (equal "applied" (gethash "status" result)))
    (assert (= 3 (pai.context-graph:context-graph-entity-count graph)))
    (assert (= 1 (pai.context-graph:context-graph-fact-count graph))))
  (let* ((bad-arguments (pai.context-graph::%cg-detach arguments))
         (relationship (aref (gethash "relationships" bad-arguments) 0))
         (bad-json nil)
         (bad-model (pai.context-graph::%cg-detach model))
         (bad-proposal (pai.context-graph::%cg-detach proposal)))
    (setf (gethash "evidence_status" relationship) "direct"
          bad-json (shasht:write-json bad-arguments nil)
          (gethash "id" bad-model) 23
          (gethash "model_call_id" (gethash "payload" bad-model))
          "model:bad-direct"
          (gethash "arguments"
                   (gethash "function"
                            (aref
                             (gethash "tool_calls"
                                      (gethash "assistant_message"
                                               (gethash "payload" bad-model)))
                             0)))
          bad-json
          (gethash "id" bad-proposal) 24
          (gethash "proposal" (gethash "payload" bad-proposal)) bad-arguments
          (gethash "model_call_id" (gethash "payload" bad-proposal))
          "model:bad-direct")
    (setf (gethash 23 index) bad-model (gethash 24 index) bad-proposal)
    (let ((rejected
            (%ccg-apply-conversation-proposal
             runtime bad-proposal index agent-id persona-id)))
      (assert (equal "rejected" (gethash "status" rejected)))
      (assert (search "must be inference"
                      (gethash "reason" rejected))))))

(format t "CONTEXT-GRAPH-CONVERSATION-PROPOSAL operator/tool/agent observation authority, reserved persona endpoint, and specific closed rejection passed~%")
(format t "PASS context-graph-conversation-proposal-tests~%")
