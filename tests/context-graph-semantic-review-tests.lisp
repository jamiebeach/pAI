;;;; harness: bare
(load (merge-pathnames "context-graph-runtime-generation-tests.lisp" *load-truename*))
(in-package :pai.context-graph)

(dolist (value '(:null "2024-02-29" "2026-09-06T12:15:30Z")) (assert (%cgq-time-p value)))
(dolist (value '("source_5" "before source_3" "yesterday" "2025-02-29" "2026-04-31"
                 "2026-13-01" "2026-01-00" "2026-09-06T25:00:00Z" "2026-9-6"))
  (assert (not (%cgq-time-p value))))

(let* ((ontology (gethash "ontology" (as-fixture))) (graph (make-context-graph ontology))
       (revision "personal-context-core-glm53-v1.2")
       (episode (sm-episode "temporal-gate-fixture" "I own a cat named Mina." 100)))
  (multiple-value-bind (context boundary) (lab-authority-context graph episode 0)
    (declare (ignore boundary))
    (let ((raw (%cgs-expand context ontology revision (sm-proposal))))
      (setf (gethash "occurred_at" (gethash "temporal" (aref (gethash "relationships" raw) 0))) "before source_3")
      (assert (equal "accepted" (gethash "status" (%cgm-review-input context raw revision))))
      (assert (sm-error (lambda () (%cgq-review-input context raw revision)) "QUALITY_TEMPORAL_INVALID")))))

(dolist (failure '(nil "endpoint_identity" "direction" "scope" "statement_fidelity" "time" "joke" "hypothesis" "uncertain" "retained-hypothesis"))
  (let* ((ontology (gethash "ontology" (as-fixture))) (graph (make-context-graph ontology))
         (revision "personal-context-core-glm53-v1.2")
         (episode (sm-episode "semantic-review-fixture" (if (equal failure "retained-hypothesis")
                                                          "I might own a cat named Mina." "I own a cat named Mina.") 100))
         (simple (sm-proposal)))
    ;; Same-kind endpoints pass ontology signatures despite reversed meaning.
    ;; The scripted independent verdict is deliberately based on this conflict.
    (when (equal failure "direction")
      (setf (gethash "entity_types" ontology) (concatenate 'vector (gethash "entity_types" ontology) #("artifact"))
            (gethash "edge_types" ontology) (concatenate 'vector (gethash "edge_types" ontology)
                                             (vector (%cg-object "name" "has_part" "subject_types" #("artifact") "object_types" #("artifact"))))
            episode (sm-episode "semantic-direction-fixture" "The dashboard contains a drawer." 100)
            (gethash "new_entities" simple)
            (vector (%cg-object "name" "dashboard" "kind" "artifact" "alternate_names" #() "categories" #("interface"))
                    (%cg-object "name" "drawer" "kind" "artifact" "alternate_names" #() "categories" #("interface"))))
      (let ((fact (aref (gethash "facts" simple) 0)))
        (setf (gethash "subject" fact) "new_2" (gethash "object" fact) "new_1"
              (gethash "predicate" fact) "has_part" (gethash "statement" fact) "The dashboard contains a drawer."
              (gethash "quote" (aref (gethash "evidence" fact) 0)) "The dashboard contains a drawer.")))
    (when (equal failure "retained-hypothesis")
      (let ((fact (aref (gethash "facts" simple) 0)))
        (setf (gethash "scope" fact) "hypothesis" (gethash "statement" fact) "The operator might own a cat."
              (gethash "quote" (aref (gethash "evidence" fact) 0)) "I might own a cat named Mina.")))
    (multiple-value-bind (context boundary) (lab-authority-context graph episode 0)
      (let* ((before (%cg-authority-projection-digest graph "lab-agent" "lab-persona"))
             (result
               (context-graph-generate-reviewed context ontology revision
                 (lambda (phase spec digest)
                   (declare (ignore digest))
                   (cond ((equal phase "entities") (%cg-object "new_entities" (gethash "new_entities" simple)))
                         ((equal phase "facts")
                          (assert (search "never emit source handles" (gethash "system" spec)))
                          (%cg-object "facts" (gethash "facts" simple) "name_corrections" #()))
                         ((equal phase "review")
                          (assert (equal "kg-semantic-review-v2" (gethash "adapter_revision" spec)))
                          (let ((review (sm-review context (%cgs-expand context ontology revision simple))))
                            (loop for row across (gethash "claim_reviews" review)
                                  for entity = (uiop:string-prefix-p "entity:" (gethash "claim_ref" row)) do
                              (setf (gethash "quality_checks" row)
                                    (apply #'%cg-object (loop for key in +cgq-checks+
                                                            append (list key (if (and entity (not (equal key "endpoint_identity")))
                                                                                 "not-applicable" "supported"))))
                                    (gethash "source_reading" row) (if entity "not-applicable"
                                                                      (if (equal failure "retained-hypothesis") "hypothesis" "assertion")))
                              (when (and failure (not (equal failure "retained-hypothesis")) (not entity))
                                (if (member failure +cgq-checks+ :test #'equal)
                                    (setf (gethash failure (gethash "quality_checks" row)) "unsupported")
                                    (setf (gethash "source_reading" row) failure))))
                            review)))) :quality-review t))
             (envelope (gethash "value" result)))
        (assert (equal before (%cg-authority-projection-digest graph "lab-agent" "lab-persona")))
        (assert (equal "kg-runtime-reviewed-v2" (gethash "generation_revision" envelope)))
        (context-graph-apply-reviewed-generation graph boundary envelope)
        (let ((replayed (make-context-graph ontology)))
          (context-graph-apply-reviewed-generation replayed boundary envelope)
          (assert (equal (%cg-authority-projection-digest graph "lab-agent" "lab-persona")
                         (%cg-authority-projection-digest replayed "lab-agent" "lab-persona"))))
        (assert (= (if (or (null failure) (equal failure "retained-hypothesis")) 1 0)
                   (hash-table-count (context-graph-facts graph))))
        ;; Rejected batches do not establish an authority projection, so the
        ;; private authority retrieval API correctly refuses those graphs.
        ;; Exercise retrieval only for admitted assertion/hypothesis batches:
        ;; the assertion is useful, while the hypothesis is not factual.
        (when (or (null failure) (equal failure "retained-hypothesis"))
          (assert (= (if failure 0 1)
                     (length (gethash "facts" (gethash "context" (%cg-authority-retrieve graph "lab-agent" "lab-persona" "Mina")))))))
        (let ((tampered (%cg-detach envelope)))
          (setf (gethash "response_digest" (gethash "review_binding" tampered)) (%cg-sha256 "tampered"))
          (assert (sm-error (lambda () (context-graph-apply-reviewed-generation graph boundary tampered)) "QUALITY_REVIEW_INVALID")))
        (let* ((missing (%cg-detach envelope)) (review (gethash "review" missing)))
          (remhash "quality_checks" (aref (gethash "claim_reviews" review) 0))
          (setf (gethash "response_digest" (gethash "review_binding" missing)) (%cg-authority-digest "model-review-output" review))
          (assert (sm-error (lambda () (context-graph-apply-reviewed-generation graph boundary missing)) "QUALITY_REVIEW_INVALID")))
        (format t "PASS semantic review gate ~a~%" (or failure "positive useful retrieval"))))))
(format t "SEMANTIC-REVIEW temporal, dimensional admission, useful retrieval and binding checks passed~%")
