;;;; harness: full-system
;;;; Focused append-only graph confirmation qualification.
(in-package :agent)

(assert (member "context-graph-confirmation-requested"
                *conscious-recursive-thread-event-types* :test #'equal))

(let* ((fact-id "cgf:fixture-inference")
       (identity
         "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")
       (candidate
         (obj "schema_version" 1 "fact_id" fact-id
              "identity_sha256" identity
              "statement" "The operator is the parent of Child A."
              "predicate" "parent_of"
              "subject" (obj "node_id" "entity:operator"
                             "label" "Operator" "kind" "person")
              "object" (obj "node_id" "entity:child-a"
                            "label" "Child A" "kind" "person")
              "evidence_status" "inference" "through_event_id" 41
              "ontology_revision" "personal-context-core-glm53-v1.3"))
       (captured nil)
       (old-append (symbol-function '%conversation-append-readable))
       (old-confirmation *conscious-recursive-mind-graph-confirmation-fn*))
  (unwind-protect
       (progn
         (setf *conscious-recursive-mind-graph-confirmation-fn*
               (lambda (requested-id)
                 (assert (string= fact-id requested-id))
                 candidate)
               (symbol-function '%conversation-append-readable)
               (lambda (type payload &key caused-by)
                 (push (list type payload caused-by) captured)
                 (values (length captured)
                         (obj "id" (length captured) "type" type
                              "payload" payload "caused_by" caused-by))))
         (let* ((schemas (%recursive-tool-schemas nil nil nil))
                (schema
                  (find "request-graph-confirmation" schemas :test #'equal
                        :key (lambda (row)
                               (gethash "name" (gethash "function" row))))))
           (assert schema)
           (assert (equal '("fact_id")
                          (%recursive-object-keys
                           (%recursive-validate-tool-arguments
                            "request-graph-confirmation"
                            (obj "fact_id" fact-id))))))
           (assert (handler-case
                       (progn
                         (%recursive-validate-tool-arguments
                          "request-graph-confirmation"
                          (obj "fact_id" fact-id "statement" "model supplied"))
                         nil)
                     (error () t))))
         (let* ((content
                  (%recursive-request-graph-confirmation
                   fact-id 7 "thread:fixture" "model:fixture" "tool:fixture"))
                (result (shasht:read-json content))
                (receipt (first captured))
                (payload (second receipt)))
           (assert (equal "confirmation-requested"
                          (gethash "status" result)))
           (assert (equal "context-graph-confirmation-requested"
                          (first receipt)))
           (assert (= 7 (third receipt)))
           (assert (equal identity
                          (gethash "fact_identity_sha256" payload)))
           (assert (search "Do not claim the graph changed"
                           (gethash "next_step" result)))))
    (setf (symbol-function '%conversation-append-readable) old-append
          *conscious-recursive-mind-graph-confirmation-fn* old-confirmation))

(let* ((root
         (obj "id" 7 "agent_id" "agent:fixture" "type" "user-message"
              "payload"
              (obj "text" "Check one inference."
                   "metadata"
                   (obj "source" "recursive-mind-v1"
                        "thread_id" "thread:fixture"))))
       (receipt
         (obj "id" 8 "agent_id" "agent:fixture"
              "type" "context-graph-confirmation-requested" "caused_by" 7
              "payload"
              (obj "schema_version" 1 "thread_id" "thread:fixture"
                   "fact_id" "cgf:fixture-inference"
                   "fact_identity_sha256"
                   "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
                   "statement" "The operator is the parent of Child A."
                   "predicate" "parent_of"
                   "subject" (obj "node_id" "entity:operator")
                   "object" (obj "node_id" "entity:child-a")
                   "ontology_revision" "personal-context-core-glm53-v1.3"
                   "graph_through_event_id" 41
                   "model_call_id" "model:fixture"
                   "tool_call_id" "tool:fixture"
                   "runtime_revision" *conscious-recursive-mind-runtime-revision*
                   "requested_at" 42)))
       (projection
         (conscious-recursive-thread-project
          (vector root receipt) 7 "agent:fixture")))
  (assert (equal "model-ready" (gethash "state" projection)))
  (assert (= 1 (gethash "confirmation_request_count" projection)))
  (assert (= 0 (gethash "confirmation_resolution_count" projection))))

(let* ((statement "The operator is the parent of Child A.")
       (identity
         "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")
       (request
         (obj "id" 8 "agent_id" "agent:fixture"
              "type" "context-graph-confirmation-requested" "caused_by" 7
              "payload"
              (obj "fact_id" "cgf:fixture-inference"
                   "fact_identity_sha256" identity
                   "statement" statement
                   "ontology_revision"
                   "personal-context-core-glm53-v1.3")))
       (publication
         (obj "id" 9 "agent_id" "agent:fixture" "type" "agent-message"
              "caused_by" 7 "payload"
              (obj "text" (format nil "Is this correct: ~a" statement))))
       (user
         (obj "id" 10 "agent_id" "agent:fixture" "type" "user-message"
              "payload" (obj "text" "Yes")))
       (events (list request publication user))
       (captured nil)
       (old-events (symbol-function '%recursive-thread-events))
       (old-append (symbol-function '%conversation-append-readable))
       (old-confirmation *conscious-recursive-mind-graph-confirmation-fn*))
  (unwind-protect
       (progn
         (setf (symbol-function '%recursive-thread-events)
               (lambda () events)
               (symbol-function '%conversation-append-readable)
               (lambda (type payload &key caused-by)
                 (setf captured (list type payload caused-by))
                 (values 11 (obj "id" 11 "type" type "payload" payload
                                 "caused_by" caused-by)))
               *conscious-recursive-mind-graph-confirmation-fn*
               (lambda (fact-id)
                 (assert (equal fact-id "cgf:fixture-inference"))
                 (obj "fact_id" fact-id "identity_sha256" identity
                      "evidence_status" "inference")))
         (%recursive-maybe-resolve-graph-confirmation 10 "Yes")
         (assert (equal "context-graph-confirmation-resolved"
                        (first captured)))
         (assert (equal "confirm"
                        (gethash "decision" (second captured))))
         (assert (= 8 (gethash "request_event_id" (second captured))))
         (assert (= 10 (third captured)))
         (assert (null (%recursive-graph-confirmation-decision
                        "Yes, but let me explain"))))
    (setf (symbol-function '%recursive-thread-events) old-events
          (symbol-function '%conversation-append-readable) old-append
          *conscious-recursive-mind-graph-confirmation-fn* old-confirmation)))

(format t "RECURSIVE-GRAPH-CONFIRMATION request, exact short-answer resolution, append-only receipts and per-root counts passed~%")
(format t "PASS recursive-graph-confirmation-tests~%")
