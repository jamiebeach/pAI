;;;; harness: bare
(load (merge-pathnames "context-graph-source-reference-tests.lisp" *load-truename*))
(in-package :pai.context-graph)
(defvar *runtime-candidate-checks* 0)
(defun rc-check (name value)
  (unless value (error "FAIL ~a" name))
  (incf *runtime-candidate-checks*) (format t "PASS ~a~%" name))
(let* ((ontology (gethash "ontology" (as-fixture)))
       (revision "personal-context-core-glm53-v1.2"))
  (setf (gethash "entity_types" ontology) (concatenate 'vector (gethash "entity_types" ontology) #("artifact")))
  (let* ((graph (make-context-graph ontology))
         (episode (sm-episode "candidate-first" "I own a cat named Mina." 100)))
    (multiple-value-bind (context boundary) (lab-authority-context graph episode 0)
      (let* ((raw (%cgs-expand context ontology revision (sm-proposal)))
             (review (sm-review context raw)) (spec (%cgm-review-input context raw revision)))
        (%cgm-apply-reviewed graph boundary context raw review revision
          (%cg-object "request_digest" (%cg-authority-digest "model-review-input" (gethash "value" spec))
                      "response_digest" (%cg-authority-digest "model-review-output" review))))
      ;; Construct valid unrelated projection descriptors; no provider fixtures or private IDs.
      (dotimes (i 70)
        (%cg-authority-new-entity graph
          (%cg-object "local_ref" (format nil "artifact-~d" i) "kind" "artifact"
                      "label" (format nil "Telescope ~d" i) "aliases" #() "classifications" #())
          context #() (make-hash-table :test #'equal)))
      (setf (context-graph-entity-scan-index graph)
            (coerce (sort (loop for id being the hash-keys of (context-graph-entities graph) collect id) #'string<) 'vector)
            (context-graph-projection-digest graph) nil))
    (multiple-value-bind (base boundary) (lab-authority-context graph (sm-episode "candidate-next" "The cat Mina is actually named Mira." 200) 1)
      (let* ((before (%cg-authority-projection-digest graph "lab-agent" "lab-persona"))
             (base-json (%cg-authority-canonical-json base))
             (selected (context-graph-select-runtime-candidates graph base "telescope"))
             (scope (aref (gethash "correction_scopes" selected) 0)))
        (rc-check "larger graph uses distinct scope context" (= 2 (gethash "schema_version" selected)))
        (rc-check "ordinary ranked candidates truthfully incomplete" (eq :false (gethash "complete" (gethash "candidate_scan" selected))))
        (rc-check "complete correction survives more than 64 entities" (eq :true (gethash "complete" scope)))
        (rc-check "one actual correction target retained" (= 1 (length (gethash "candidate_entity_ids" scope))))
        (rc-check "reserved target plus bounded ranked candidates" (= 49 (length (gethash "eligible_entities" selected))))
        (rc-check "scope scan verified against actual graph" (%cg-runtime-verify-correction-scans graph selected))
        (rc-check "selection leaves owner input unchanged" (equal base-json (%cg-authority-canonical-json base)))
        (rc-check "selection writes no graph state" (equal before (%cg-authority-projection-digest graph "lab-agent" "lab-persona")))
        (let ((bad (%cg-detach selected)))
          (setf (gethash "candidate_entity_ids" (aref (gethash "correction_scans" bad) 0)) #())
          (rc-check "omitted correction candidate refused" (sm-error (lambda () (%cg-runtime-verify-correction-scans graph bad)) "CORRECTION_SCAN_MISMATCH")))
        (let ((bad (%cg-detach base)))
          (setf (gethash "correction_scopes" bad) #()
                (gethash "state_digest" (gethash "projection_watermark" bad)) (%cg-sha256 "stale"))
          (rc-check "stale selection context refused" (sm-error (lambda () (context-graph-select-runtime-candidates graph bad "telescope")) "RUNTIME_CANDIDATES_INVALID")))
        (let* ((overflow (context-graph-select-runtime-candidates graph base "Mina" :target-kinds #("artifact")))
               (overflow-scope (aref (gethash "correction_scopes" overflow) 0)))
          (rc-check "more than sixteen alternatives stays incomplete" (eq :false (gethash "complete" overflow-scope)))
          (rc-check "overflow retains bounded diagnostic candidates" (= 16 (length (gethash "candidate_entity_ids" overflow-scope)))))
        (let* ((handle (loop for h being the hash-keys of (%cgs-handles selected) using (hash-value e)
                             when (equal "Mina" (gethash "label" e)) return h))
               (simple (%cg-object "new_entities" #() "facts" #() "name_corrections"
                         (vector (%cg-object "target" handle "corrected_name" "Mira" "interpretation" "error-correction"
                           "evidence" (%cg-object "source" "source_1" "quote" "The cat Mina is actually named Mira.")))))
               (raw (%cgs-expand selected ontology revision simple))
               (prepared (sr-prepared selected raw))
               (review (%cg-object "schema_version" 2
                         "claim_reviews" (map 'vector (lambda (claim)
                           (%cg-object "claim_ref" (gethash "claim_ref" claim) "verdict" "DIRECTLY_EVIDENCED"
                                       "evidence" "Exact synthetic correction.")) (%cgm-claims prepared))
                         "revision_reviews" (gethash "reviews" (sr-review selected prepared))))
               (spec (%cgm-review-input selected raw revision)))
          (let ((bad (%cg-detach selected)))
            (setf (gethash "complete" (aref (gethash "correction_scans" bad) 0)) :false
                  (gethash "complete" (aref (gethash "correction_scopes" bad) 0)) :false)
            (rc-check "application rechecks scope against actual projection"
                      (sm-error (lambda () (%cg-apply-authority-episode graph boundary raw bad :null)) "CORRECTION_SCAN_MISMATCH")))
          (rc-check "large graph correction applies through shared admission"
                    (equal "admitted" (gethash "formation_outcome" (gethash "application" (gethash "value"
                      (%cgm-apply-reviewed graph boundary selected raw review revision
                        (%cg-object "request_digest" (%cg-authority-digest "model-review-input" (gethash "value" spec))
                                    "response_digest" (%cg-authority-digest "model-review-output" review))))))))
          (rc-check "corrected name retrieves the existing grounded fact"
                    (= 1 (length (gethash "rows" (%cg-authority-search graph "lab-agent" "lab-persona" "Mira"))))))))))
(format t "RUNTIME-CANDIDATES ~d passed, 0 failed~%" *runtime-candidate-checks*)
