;;;; harness: full-system
(in-package :agent)
(load (asdf:system-relative-pathname :pai "scripts/context-graph-episode-lab-core.lisp"))
(load (asdf:system-relative-pathname :pai "scripts/context-graph-lab-checkpoint.lisp"))
(load (asdf:system-relative-pathname :pai "scripts/context-graph-lab-prepare.lisp"))
(load (asdf:system-relative-pathname :pai "scripts/context-graph-lab-case.lisp"))

(let* ((contract (obj "profile" "reviewed-inference-v9" "generation" "identity-formation-owner-v9"
                      "protocol" "identity-formation-v14" "ontology_revision" "personal-context-core-glm53-v1.3"
                      "agent_id" "fixture-agent" "persona_id" "fixture-persona" "cutoff" 0 "recovery_position" 0))
       (input (obj "kind" "private-preparation-input" "contract" contract "origin_digest" "fixture" "events" #()))
       (frame nil))
  (context-graph-lab-prepare input '(0)
    (lambda (envelope selection status)
      (declare (ignore status))
      (setf frame (obj "envelope" envelope "digest" (gethash "digest" envelope) "contract" selection))))
  (let ((result (context-graph-lab-replay frame #() (vector (obj "exact_queries" #("Missing Entity"))))))
    (assert (equal "complete" (gethash "status" result)))
    (assert (= 0 (gethash "historical_fold_count" result)))
    (assert (= 0 (length (gethash "nodes" (gethash "delta" result))))))
  (let* ((bad (obj "id" 1 "type" "context-graph-identity-phase" "agent_id" "foreign"
                   "payload" (obj "persona_id" "fixture-persona" "generation" "identity-formation-owner-v9")))
         (result (context-graph-lab-replay frame (vector bad))))
    (assert (equal "refused" (gethash "status" result)))
    (assert (= 1 (gethash "event_id" (gethash "failure" result)))))
  (assert (handler-case
              (progn (context-graph-lab-replay frame #() #() (obj "event_id" 99 "response" (obj))) nil)
            (error () t)))
  (assert (equal "complete" (gethash "status" (context-graph-lab-replay frame #())))))
(format t "LAB-CASE checkpoint-only replay, exact query, partition refusal and baseline isolation passed~%")

(let* ((base (obj "status" "complete" "mode" "recorded" "baseline_digest" "fixture"
                  "input_queries_digest" "fixture-queries" "contract" (obj "profile" "fixture")
                  "input_events_digest" "fixture-input" "after" (obj "nodes" #() "edges" #() "evidence" #())))
       (changed (pai.context-graph::%cg-detach base)))
  (assert (equal "passed" (gethash "status" (context-graph-lab-compare base base #()))))
  (remhash "input_queries_digest" changed)
  (assert (handler-case (progn (context-graph-lab-compare base changed #()) nil) (error () t)))
  (setf (gethash "input_queries_digest" changed) "fixture-queries"
        (gethash "contract" changed) (obj "profile" "other"))
  (assert (handler-case (progn (context-graph-lab-compare base changed #()) nil) (error () t)))
  (setf (gethash "contract" changed) (obj "profile" "fixture")
        (gethash "admission_trace" changed) (vector (obj "rejected" "fixture-claim")))
  (assert (equal "failed" (gethash "status" (context-graph-lab-compare base changed #()))))
  (remhash "admission_trace" changed)
  (setf (gethash "nodes" (gethash "after" changed)) (vector (obj "node_id" "fixture-node" "label" "neutral")))
  (assert (equal "failed" (gethash "status" (context-graph-lab-compare base changed #()))))
  (assert (equal "passed" (gethash "status" (context-graph-lab-compare base changed #("nodes:fixture-node:added")))))
  (setf (gethash "input_events_digest" changed) "other-input")
  (assert (handler-case (progn (context-graph-lab-compare base changed #()) nil) (error () t))))
(format t "LAB-COMPARISON explicit empty/nonempty oracle and input mismatch refusal passed~%")

(let* ((base (obj "status" "complete" "baseline_digest" "fixture"
                  "input_events_digest" "events" "input_queries_digest" "queries"
                  "contract" (obj "profile" "fixture")
                  "after" (obj "nodes" #() "edges" #()
                               "evidence" (vector (obj "fact_id" "fact-1" "source_ids" #("source-1" "source-2"))))))
       (candidate (pai.context-graph::%cg-detach base)))
  (setf (gethash "source_ids" (aref (gethash "evidence" (gethash "after" candidate)) 0)) #("source-2"))
  (let* ((comparison (context-graph-lab-compare base candidate #()))
         (loss (gethash "evidence_loss" comparison)))
    (assert (equal "failed" (gethash "status" comparison)))
    (assert (= 1 (length loss)))
    (assert (equalp #("source-1") (gethash "removed_source_ids" (aref loss 0))))))
(format t "LAB-COMPARISON admission-only changes and explicit evidence loss passed~%")

(let* ((graph (pai.context-graph::%make-context-graph))
       (relationship (obj "predicate" "owns" "fact" "Fixture person owns fixture pet."))
       (result (obj "proposal" (obj "relationships" (vector relationship))
                    "review" (obj "claim_reviews" (vector (obj "claim_ref" "relationship:0" "verdict" "UNSUPPORTED")))))
       (application (obj "value" (obj "selection_receipt"
                          (obj "omissions" (vector (obj "claim_ref" "relationship:0" "reason" "REVIEW_NOT_DIRECT")))))))
  (let ((trace (aref (%cgl-linked-claims result application graph 50) 0)))
    (assert (equal "omitted" (gethash "selection" trace)))
    (assert (eq :null (gethash "applied_fact_id" trace)))
    (assert (equal "UNSUPPORTED" (gethash "verdict" (gethash "review" trace)))))
  (assert (equal "unknown" (gethash "selection" (aref (%cgl-linked-claims result nil graph 50) 0))))
  (loop for id in '("fact-a" "fact-b") do
    (setf (gethash id (pai.context-graph::context-graph-facts graph))
          (obj "fact_id" id "predicate" "owns" "fact" (gethash "fact" relationship)
               "evidence_records" (vector (obj "application_event_id" 50)))))
  (let ((trace (aref (%cgl-linked-claims result nil graph 50) 0)))
    (assert (equal "ambiguous-provenance-join" (gethash "unknown_reason" trace)))
    (assert (eq :null (gethash "applied_fact_id" trace)))
    (assert (= 2 (length (gethash "candidate_fact_ids" trace))))))
(format t "LAB-CLAIM-TRACE explicit omission, unknown and ambiguous provenance passed~%")

(let* ((graph (pai.context-graph::%make-context-graph))
       (identity (obj "bindings" (vector (obj "mention" "mention-1" "entity" "known-1"))
                      "mentions" (vector (obj "mention" "mention-1" "designation" "Fixture child"
                                              "source" "source-1" "quote" "My child is here."))
                      "resolutions" (vector (obj "mention" "mention-1" "candidate" "candidate-1"))
                      "candidates" (vector (obj "candidate" "candidate-1" "name" "Fixture child"
                                                "kind" "person"))
                      "page_responses" (vector (obj "mentions" (vector
                        (obj "mention" "mention-1" "status" "possible"
                             "candidates" #("candidate-1")))))))
       (relationship (obj "subject_ref" "known-1" "predicate" "child_of"
                          "object_ref" "runtime:operator" "fact" "Fixture child is a child."
                          "grounding" (obj "attributed_to_ref" "runtime:active-persona")))
       (result (obj "identity_trace" identity
                    "proposal" (obj "relationships" (vector relationship))))
       (endpoints (gethash "identity_endpoints"
                           (aref (%cgl-linked-claims result nil graph 50) 0)))
       (subject (gethash "subject" endpoints))
       (mention (aref (gethash "mentions" subject) 0)))
  (assert (equal "resolved" (gethash "status" subject)))
  (assert (equal "source-1" (gethash "source" (gethash "mention" mention))))
  (assert (equal "candidate-1" (gethash "candidate" (gethash "resolution" mention))))
  (assert (= 1 (length (gethash "page_decisions" mention))))
  (assert (equal "reserved-participant" (gethash "status" (gethash "object" endpoints))))
  (assert (equal "operator" (gethash "participant_role" (gethash "object" endpoints))))
  (assert (equal "active-persona"
                 (gethash "participant_role" (gethash "attributed_to" endpoints)))))
(format t "LAB-CLAIM-TRACE endpoint mentions, candidate resolution and reserved participants joined~%")

(let* ((claim (obj "applied_fact_id" "fact-a"))
       (result (obj "admission_trace" (vector (obj "claim_trace" (vector claim)))
                    "queries" (vector (obj "request" (obj "evidence_policy" "inferred")
                                            "before" (obj "edges" #())
                                            "after" (obj "edges" (vector (obj "edge_id" "fact-a"))))))))
  (%cgl-add-query-links result)
  (let ((visibility (aref (gethash "query_visibility" claim) 0)))
    (assert (eq :false (gethash "returned_before" visibility)))
    (assert (eq :true (gethash "returned_after" visibility)))))
(format t "LAB-CLAIM-TRACE bounded before/after query visibility passed~%")

(let* ((large (make-array 200 :initial-element (obj "quote" (make-string 1000 :initial-element #\a))))
       (copy (pai.context-graph::%cg-detach large)))
  (assert (%cgl-result-equal-p large copy))
  (setf (gethash "quote" (aref copy 0)) "Different")
  (assert (not (%cgl-result-equal-p large copy)))
  (assert (not (%cgl-result-equal-p "person-a" "Person-A")))
  (assert (not (%cgl-result-equal-p (obj "missing" nil) (obj)))))
(format t "LAB-COMPARISON large trace, case sensitivity and missing-key distinction passed~%")

(let* ((edge (obj "edge_id" "fixture-fact" "query_eligible" nil))
       (rows (vector edge)))
  (assert (equal "unchanged"
                 (gethash "change" (aref (%cgel-mark-delta rows rows "edge_id" #'%cgl-result-equal-p) 0)))))
(format t "LAB-COMPARISON saved JSON false values are data, not authority inputs, passed~%")

(let* ((graph (pai.context-graph::%make-context-graph))
       (runtime (pai.context-graph::%make-cg-runtime :graph graph))
       (sources (map 'vector (lambda (id) (obj "source_id" id "quote" "Original evidence"))
                     #("s1" "s2" "s3" "s4" "s5"))))
  (setf (gethash "fact-a" (pai.context-graph::context-graph-facts graph))
        (obj "fact_id" "fact-a" "accepted_source_ids" #("s1" "s2" "s3" "s4")
             "evidence_records" (vector (obj "accepted_sources" sources))))
  (let* ((inventory (%cgl-evidence-inventory runtime))
         (base (obj "status" "complete" "baseline_digest" "fixture"
                    "input_events_digest" "events" "input_queries_digest" "queries"
                    "contract" (obj "profile" "fixture")
                    "after" (obj "nodes" #() "edges" #() "evidence" inventory)))
         (candidate (pai.context-graph::%cg-detach base)))
    (assert (= 5 (length (gethash "source_ids" (aref inventory 0)))))
    (setf (gethash "quote" (aref (gethash "accepted_sources"
             (aref (gethash "records" (aref (gethash "evidence" (gethash "after" candidate)) 0)) 0)) 4))
          "Changed fifth-source evidence")
    (let ((comparison (context-graph-lab-compare base candidate #())))
      (assert (equal "failed" (gethash "status" comparison)))
      (assert (find "evidence:fact-a:changed" (gethash "unexpected" comparison) :test #'equal)))
    (remhash "evidence" (gethash "after" candidate))
    (assert (handler-case (progn (context-graph-lab-compare base candidate #()) nil) (error () t)))))
(format t "LAB-COMPARISON full fifth-source evidence and legacy-inventory refusal passed~%")

(let ((*cgl-run-cache* (make-hash-table :test #'equal))
      (*cgl-run-order* nil))
  (loop for number from 1 to 5
        for id = (format nil "fixture-run-~D" number)
        for graph = (pai.context-graph::%make-context-graph)
        for runtime = (pai.context-graph::%make-cg-runtime :graph graph)
        for lab = (%cgl-case-lab runtime
                                  (pai.context-graph::%cgi-owner-create
                                   graph "fixture-agent" "fixture-persona")
                                  number)
        do (%cgl-cache-run id lab lab number number))
  (assert (= 4 (hash-table-count *cgl-run-cache*)))
  (assert (null (gethash "fixture-run-1" *cgl-run-cache*)))
  (assert (gethash "fixture-run-5" *cgl-run-cache*))
  (assert (handler-case
              (progn (context-graph-lab-cached-query "fixture-run-1" "after" (obj)) nil)
            (error (condition)
              (search "no automatic replay" (princ-to-string condition))))))
(format t "LAB-RUN-CACHE four-entry eviction refuses without automatic replay passed~%")
(format t "PASS context-graph-lab-case-tests~%")
