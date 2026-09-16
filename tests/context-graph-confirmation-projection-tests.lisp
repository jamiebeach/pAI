;;;; context-graph-confirmation-projection-tests.lisp -- append-only resolution fold.
;;;; harness: full-system

(in-package :agent)

(defun cgcp-fixture (decision answer)
  (let* ((agent-id "agent:fixture")
         (persona-id "persona:fixture")
         (identity (make-string 64 :initial-element #\a))
         (fact-id "cgf:fixture-inference")
         (identity-key "fixture-claim-identity")
         (statement "The operator is the parent of Child A.")
         (ontology
           (let ((*conscious-context-graph-runtime-profile*
                   "reviewed-inference-v9"))
             (%ccg-ontology)))
         (runtime
           (pai.context-graph:context-graph-runtime-create
            ontology *knowledge-graph-family-ontology-revision*
            agent-id persona-id))
         (graph
           (pai.context-graph::context-graph-runtime-graph runtime))
         (fact
           (obj "fact_id" fact-id "identity_key" identity-key
                "identity_sha256" identity
                "subject_id" "entity:operator"
                "object_id" "entity:child-a" "predicate" "parent_of"
                "fact" statement "status" "current"
                "grounding"
                (obj "scope" "assertion" "polarity" "positive"
                     "attributed_entity_id" :null
                     "source_basis" "reported")
                "temporal"
                (obj "schema_version" 1 "character" "state"
                     "occurred_at" :null "valid_from" :null
                     "valid_until" :null)
                "observed_at" 30 "through_event_id" 41
                "evidence_status" "inference"
                "evidence_records" #("premise")
                "evidence_digests" #("premise-digest")
                "accepted_source_ids" #("event:30")
                "accepted_evidence_digest" "premise-evidence"))
         (request
           (obj "id" 42 "agent_id" agent-id
                "type" "context-graph-confirmation-requested"
                "caused_by" 40
                "payload"
                (obj "fact_id" fact-id
                     "fact_identity_sha256" identity
                     "statement" statement
                     "ontology_revision"
                     *knowledge-graph-family-ontology-revision*)))
         (publication
           (obj "id" 43 "agent_id" agent-id "type" "agent-message"
                "caused_by" 40
                "payload"
                (obj "text" (format nil "Is this correct: ~a" statement)
                     "metadata"
                     (obj "source" "recursive-mind-v1"
                          "persona_id" persona-id))))
         (source
           (obj "id" 44 "agent_id" agent-id "type" "user-message"
                "timestamp" "2026-01-01T00:00:00Z"
                "payload"
                (obj "text" answer
                     "metadata"
                     (obj "source" "recursive-mind-v1"
                          "persona_id" persona-id))))
         (resolution
           (obj "id" 45 "agent_id" agent-id
                "type" "context-graph-confirmation-resolved"
                "caused_by" 44
                "payload"
                (obj "schema_version" 1 "request_event_id" 42
                     "request_root_event_id" 40 "fact_id" fact-id
                     "fact_identity_sha256" identity
                     "decision" decision "source_user_event_id" 44
                     "source_quote" answer
                     "ontology_revision"
                     *knowledge-graph-family-ontology-revision*
                     "resolved_at" 50)))
         (index (make-hash-table :test #'eql)))
    (setf
     (pai.context-graph::context-graph-authority-partition graph)
     (obj "agent_id" agent-id "persona_id" persona-id)
     (gethash fact-id (pai.context-graph::context-graph-facts graph)) fact
     (gethash identity-key
              (pai.context-graph::context-graph-current-triples graph)) fact-id
     (pai.context-graph::context-graph-through-event-id graph) 41)
    (dolist (event (list request publication source resolution))
      (setf (gethash (gethash "id" event) index) event))
    (values runtime graph resolution index fact-id identity-key
            agent-id persona-id)))

(multiple-value-bind (runtime graph resolution index fact-id identity-key
                      agent-id persona-id)
    (cgcp-fixture "confirm" "Yes")
  (assert (equal "confirm"
                 (%ccg-apply-confirmation-resolution
                  runtime resolution index agent-id persona-id)))
  (let ((fact (gethash fact-id
                       (pai.context-graph::context-graph-facts graph))))
    (assert (equal "direct" (gethash "evidence_status" fact)))
    (assert (equal "current" (gethash "status" fact)))
    (assert (= 2 (length (gethash "evidence_records" fact))))
    (assert (find "event:44" (gethash "accepted_source_ids" fact)
                  :test #'equal))
    (assert (equal fact-id
                   (gethash identity-key
                            (pai.context-graph::context-graph-current-triples
                             graph))))
    (assert (= 45 (pai.context-graph::context-graph-through-event-id graph)))
    (assert (gethash "event:45"
                     (pai.context-graph::context-graph-application-receipts
                      graph)))))

(multiple-value-bind (runtime graph resolution index fact-id identity-key
                      agent-id persona-id)
    (cgcp-fixture "reject" "No")
  (assert (equal "reject"
                 (%ccg-apply-confirmation-resolution
                  runtime resolution index agent-id persona-id)))
  (let ((fact (gethash fact-id
                       (pai.context-graph::context-graph-facts graph))))
    (assert (equal "retired" (gethash "status" fact)))
    (assert (equal "operator-confirmation-rejection-v1"
                   (gethash "retirement_basis" fact)))
    (assert (null
             (gethash identity-key
                      (pai.context-graph::context-graph-current-triples
                       graph))))))

(multiple-value-bind (runtime graph resolution index fact-id identity-key
                      agent-id persona-id)
    (cgcp-fixture "confirm" "Yes")
  (declare (ignore identity-key))
  (let* ((publication (gethash 43 index))
         (metadata (gethash "metadata" (gethash "payload" publication))))
    (setf (gethash "persona_id" metadata) "persona:foreign")
    (assert
     (handler-case
         (progn
           (%ccg-apply-confirmation-resolution
            runtime resolution index agent-id persona-id)
           nil)
       (error () t)))
    (assert (equal "inference"
                   (gethash "evidence_status"
                            (gethash fact-id
                                     (pai.context-graph::context-graph-facts
                                      graph)))))))

(format t "CONTEXT-GRAPH-CONFIRMATION exact yes promotes, exact no retires, and foreign publication fails closed through append-only replay passed~%")
(format t "PASS context-graph-confirmation-projection-tests~%")
