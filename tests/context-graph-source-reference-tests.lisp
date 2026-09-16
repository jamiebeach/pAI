;;;; harness: bare
(load (merge-pathnames "context-graph-simple-model-tests.lisp" *load-truename*))
(in-package :pai.context-graph)
(defvar *source-reference-checks* 0)
(defun sr-check (name value)
  (unless value (error "FAIL ~a" name))
  (incf *source-reference-checks*) (format t "PASS ~a~%" name))
(defun sr-prepared (context raw)
  (gethash "proposal" (gethash "value" (context-graph-prepare-authority context raw))))
(defun sr-review (context prepared)
  (let* ((request (gethash "request" (gethash "value" (context-graph-build-revision-review-input context prepared))))
         (rows (gethash "revisions" request)))
    (%cg-object "schema_version" 1 "reviews"
      (map 'vector (lambda (input)
                     (let ((target (gethash "target_entity_id" (gethash "proposed_revision" input))))
                       (%cg-object "revision_ref" (gethash "revision_ref" input) "proposal_digest" (gethash "proposal_digest" input)
                                   "interpretation" "error-correction" "source_reading" "operator-assertion"
                                   "target_scope_fit" "supported" "same_entity" "supported" "replacement_supported" "supported"
                                   "competing_interpretation" "none-found" "candidate_assessments"
                                   (map 'vector (lambda (id)
                                                  (%cg-object "entity_id" id "assessment" (if (equal id target) "target" "not-target")
                                                              "anchor_fact_ids" #() "reason_code"
                                                              (if (equal id target) "context-and-correction-agree" "context-excludes")))
                                        (gethash "candidate_entity_ids" (gethash "target_scope" input)))))) rows))))
(defun sr-fixture (&key (quote "You remember the cat as Mina. Her name is actually Mira.") (count 2) duplicate incomplete)
  (multiple-value-bind (context raw view) (at-fixture count)
    (when (> count 16)
      (multiple-value-bind (partition policy participants) (as-fixture count)
        (declare (ignore policy participants))
        (setf (gethash "eligible_entities" context)
              (coerce (sort (loop for e being the hash-values of (gethash "entities" partition) collect e)
                            #'string< :key (lambda (e) (gethash "entity_id" e))) 'vector))))
    (let* ((source (aref (gethash "sources" (gethash "source_packet" context)) 0))
           (revision (aref (gethash "entity_revisions" raw) 0))
           (target (aref (gethash "eligible_entities" context) 0)))
      (setf (gethash "text" source) quote (gethash "text_sha256" source) (%cg-sha256 quote)
            (gethash "quote" (aref (gethash "evidence" (gethash "grounding" revision)) 0)) quote)
      (loop for e across (gethash "eligible_entities" context) for i from 0 do
        (setf (gethash "label" e) (if (or (zerop i) duplicate) "Mina" (format nil "Other ~d" i))
              (gethash "revision_digest" e) (%cg-revision-descriptor-digest e)))
      (setf (gethash "label" view) "Mina" (gethash "revision_digest" view) (%cg-revision-descriptor-digest view)
            (gethash "expected_revision_digest" revision) (gethash "revision_digest" target))
      (when incomplete (setf (gethash "complete" (gethash "candidate_scan" context)) :false))
      (setf context (context-graph-enable-source-reference-corrections context)
            (gethash "target_scope_id" revision) (gethash "scope_id" (aref (gethash "correction_scopes" context) 0)))
      (values context raw view))))
(multiple-value-bind (context raw view) (sr-fixture)
  (let* ((prepared (sr-prepared context raw)) (review (sr-review context prepared))
         (receipt (at-review-receipt context prepared review))
         (before (%cg-authority-canonical-json (vector context prepared receipt view)))
         (decision (context-graph-decide-entity-revision context prepared "revision:one" receipt view))
         (grant (gethash "grant" decision)))
    (sr-check "explicit source reaches reviewed admission without ownership" (equal "accept" (gethash "outcome" decision)))
    (sr-check "distinct valid grant with zero invented anchors"
              (and (%cg-revision-grant-p grant) (equal "reviewed-explicit-source-reference" (gethash "admission_basis" grant))
                   (zerop (length (gethash "anchor_claim_digests" grant)))))
    (sr-check "mechanical planner accepts admitted source-reference grant"
              (equal "accepted" (gethash "status" (context-graph-plan-entity-revision context grant view))))
    (sr-check "decision preserves frozen inputs" (equal before (%cg-authority-canonical-json (vector context prepared receipt view))))
    (sr-check "no review still defers" (equal "REVIEW_UNAVAILABLE"
      (gethash "reason_code" (context-graph-decide-entity-revision context prepared "revision:one" :null view))))
    (setf (gethash "request_digest" receipt) (%cg-sha256 "drift"))
    (sr-check "review binding drift rejected" (equal "REVIEW_BINDING_INVALID"
      (gethash "reason_code" (context-graph-decide-entity-revision context prepared "revision:one" receipt view))))))
(dolist (args '((:quote "Her name is actually Mira.") (:quote "Mina is actually Mira.")
                (:quote "The cat Minaret is actually Mira.") (:duplicate t) (:incomplete t) (:count 17)))
  (multiple-value-bind (context raw view) (apply #'sr-fixture args)
    (let ((decision (context-graph-decide-entity-revision context (sr-prepared context raw) "revision:one" :null view)))
      (sr-check (format nil "reference prerequisite refuses ~s" args)
                (and (equal "defer" (gethash "outcome" decision))
                     (member (gethash "reason_code" decision) '("EXPLICIT_SOURCE_REFERENCE_UNRESOLVED" "TARGET_CONTEXT_INCOMPLETE") :test #'equal)
                     (eq :null (gethash "grant" decision)))))))
(dolist (spec '(("source_reading" "reported") ("source_reading" "joke") ("source_reading" "hypothetical")
                ("same_entity" "uncertain") ("interpretation" "actual-name-change") ("competing_interpretation" "present")))
  (multiple-value-bind (context raw view) (sr-fixture)
    (let* ((prepared (sr-prepared context raw)) (review (sr-review context prepared)))
      (setf (gethash (first spec) (aref (gethash "reviews" review) 0)) (second spec))
      (sr-check (format nil "semantic review refuses ~s" spec)
                (not (equal "accept" (gethash "outcome" (context-graph-decide-entity-revision
                       context prepared "revision:one" (at-review-receipt context prepared review) view))))))))
(multiple-value-bind (context raw view) (sr-fixture)
  (let* ((prepared (sr-prepared context raw)) (review (sr-review context prepared))
         (assessments (gethash "candidate_assessments" (aref (gethash "reviews" review) 0))))
    (setf (gethash "assessment" (aref assessments 1)) "uncertain")
    (sr-check "uncertain alternative defers" (equal "TARGET_AMBIGUOUS" (gethash "reason_code"
      (context-graph-decide-entity-revision context prepared "revision:one" (at-review-receipt context prepared review) view))))
    (setf (gethash "anchor_fact_ids" (aref assessments 0)) #("fabricated"))
    (sr-check "fabricated anchor rejected" (handler-case (progn (at-review-receipt context prepared review) nil)
                                            (context-graph-authority-input-error () t)))))
(multiple-value-bind (context raw view) (sr-fixture)
  (declare (ignore raw view))
  (setf (gethash "candidate_entity_ids" (aref (gethash "correction_scopes" context) 0))
        (subseq (gethash "candidate_entity_ids" (aref (gethash "correction_scopes" context) 0)) 0 1))
  (sr-check "complete scope cannot omit an eligible alternative"
            (handler-case (progn (%cg-validate-authority-context context) nil) (context-graph-authority-input-error () t))))

(multiple-value-bind (context raw view) (sr-fixture)
  (let* ((prepared (sr-prepared context raw)) (receipt (at-review-receipt context prepared (sr-review context prepared))))
    (setf (gethash "label" view) "Newer name" (gethash "revision_digest" view) (%cg-revision-descriptor-digest view))
    (sr-check "stale current version defers" (equal "TARGET_STALE" (gethash "reason_code"
      (context-graph-decide-entity-revision context prepared "revision:one" receipt view))))))
(multiple-value-bind (context raw view) (sr-fixture)
  (let* ((source (aref (gethash "sources" (gethash "source_packet" context)) 0))
         (persona (find "active-persona" (gethash "participants" context) :test #'equal :key (lambda (p) (gethash "role" p)))))
    (setf (gethash "speaker_id" source) (gethash "speaker_id" persona) (gethash "kind" source) "prior-agent-utterance"
          (gethash "role" (gethash "identity" source)) "active-persona"
          (gethash "principal_id" (gethash "identity" source)) (gethash "principal_id" persona)
          (gethash "binding_id" (gethash "identity" source)) (gethash "identity_binding_id" persona))
    (sr-check "assistant exact quotation cannot authorize correction" (equal "CORRECTION_SOURCE_INVALID" (gethash "reason_code"
      (context-graph-decide-entity-revision context (sr-prepared context raw) "revision:one" :null view))))))
(multiple-value-bind (context raw view) (at-fixture)
  (declare (ignore raw view))
  (let ((before (%cg-authority-canonical-json context)))
    (context-graph-enable-source-reference-corrections context)
    (sr-check "opt-in composition leaves original policy unchanged" (equal before (%cg-authority-canonical-json context))))
  (setf (gethash "enabled" (gethash "correction_policy" context)) :false)
  (sr-check "disabled policy cannot be silently enabled" (sm-error
            (lambda () (context-graph-enable-source-reference-corrections context)) "POLICY_UNAVAILABLE")))

;; Actual production application and fresh lab subprocess retrieval. The only
;; ownership fact is about the assistant, never an operator correction anchor.
(multiple-value-bind (partition unused participants) (as-fixture)
  (declare (ignore unused participants))
  (let* ((ontology (gethash "ontology" partition)) (revision "personal-context-core-glm53-v1.2")
         (graph (make-context-graph ontology)) (simple (sm-proposal))
         (quote "The assistant owns a cat named Mina.")
         (episode (sm-episode "sr-first" quote 100))
         (step (%cg-object "episode" episode "proposal" simple "review" :null "request_digest" :null)))
    (setf (gethash "subject" (aref (gethash "facts" simple) 0)) "active_persona"
          (gethash "statement" (aref (gethash "facts" simple) 0)) quote
          (gethash "quote" (aref (gethash "evidence" (aref (gethash "facts" simple) 0)) 0)) quote)
    (multiple-value-bind (context boundary) (lab-authority-context graph episode 0 t)
      (declare (ignore boundary))
      (setf (gethash "review" step) (sm-review context (%cgs-expand context ontology revision simple))
            (gethash "request_digest" step) (gethash "request_digest"
              (lab-authority-step graph (%cg-detach (%cg-object "episode" episode "proposal" simple "review" :null "request_digest" :null)) 0 revision t t)))
      (sr-check "initial nonoperator fact applied" (equal "accepted" (gethash "status" (lab-authority-step graph step 0 revision t t)))))
    (let* ((quote2 "You remember the cat as Mina. Her name is actually Mira.")
           (episode2 (sm-episode "sr-second" quote2 200))
           (correction (%cg-object "new_entities" #() "facts" #() "name_corrections"
                        (vector (%cg-object "target" "known_1" "corrected_name" "Mira" "interpretation" "error-correction"
                                            "evidence" (%cg-object "source" "source_1" "quote" quote2)))))
           (next (%cg-object "episode" episode2 "proposal" correction "review" :null "request_digest" :null)))
      (multiple-value-bind (context boundary) (lab-authority-context graph episode2 1 t)
        (declare (ignore boundary))
        (sr-check "source-reference extraction input invents no operator relationship"
          (let* ((spec (%cgs-extraction-input context ontology revision))
                 (known (gethash "known_entities" (gethash "input" (gethash "value" spec)))))
            (and (stringp (%cg-authority-canonical-json spec))
                 (zerop (length (gethash "operator_relations" (aref known 0)))))))
        (sr-check "staged source-reference entity and fact requests serialize"
          (and (stringp (%cg-authority-canonical-json (%cgt-entity-input context ontology revision)))
               (stringp (%cg-authority-canonical-json (%cgt-fact-input context ontology revision (%cg-object "new_entities" #()))))))
        (let* ((raw (%cgs-expand context ontology revision correction))
               (prepared (gethash "proposal" (gethash "value" (%cgm-prepare context raw revision))))
               (review (%cg-object "schema_version" 2 "claim_reviews" #()
                                   "revision_reviews" (gethash "reviews" (sr-review context prepared)))))
          (setf (gethash "review" next) review
                (gethash "request_digest" next) (gethash "request_digest"
                  (lab-authority-step graph (%cg-object "episode" episode2 "proposal" correction "review" :null "request_digest" :null) 1 revision t t)))
          (let* ((bundle (%cg-object "schema_version" 2 "authority_operation" "simple-source-reference-session"
                                    "source_kind" "synthetic-controlled" "ontology" ontology "ontology_revision" revision
                                    "history" (vector step) "step" next "queries" #("Mira" "cat" "Mina" "tea")))
                 (result (al-run bundle)) (queries (gethash "queries" result)))
            (sr-check "fresh production lab applies ownership-independent correction"
                      (equal "applied" (gethash "status" (gethash "application" (gethash "value" result)))))
            (sr-check "corrected name and category retrieve useful fact"
                      (and (= 1 (length (gethash "rows" (aref queries 0)))) (= 1 (length (gethash "rows" (aref queries 1))))))
            (sr-check "old name and unrelated query excluded"
                      (and (zerop (length (gethash "rows" (aref queries 2)))) (zerop (length (gethash "rows" (aref queries 3))))))
            (let* ((focused (%cg-object "name_corrections" (%cg-detach (gethash "name_corrections" correction))))
                   (before (%cg-authority-canonical-json focused))
                   (spec (%cgt-correction-input context ontology revision))
                   (focused-step (%cg-object "episode" episode2 "proposal" focused "review" :null "request_digest" :null))
                   (focused-bundle (%cg-detach bundle)))
              (sr-check "correction-only ask exposes no ordinary extraction fields"
                        (equal '("name_corrections") (%cgs-keys (gethash "properties" (gethash "schema" (gethash "value" spec))))))
              (sr-check "correction expansion preserves model response"
                        (progn (%cgt-correction-expand context ontology revision focused)
                               (equal before (%cg-authority-canonical-json focused))))
              (let ((bad (%cg-detach focused)))
                (setf (gethash "facts" bad) #())
                (sr-check "ordinary facts cannot enter correction-only response"
                          (sm-error (lambda () (%cgt-correction-expand context ontology revision bad)) "CORRECTION_ONLY_PROPOSAL_INVALID")))
              (setf (gethash "request_digest" focused-step)
                    (gethash "request_digest" (lab-authority-step graph focused-step 1 revision :correction t))
                    (gethash "review" focused-step) (%cg-detach review)
                    (gethash "step" focused-bundle) focused-step
                    (gethash "authority_operation" focused-bundle) "correction-only-session")
              (sr-check "fresh correction-only lab wire retrieves corrected fact"
                        (= 1 (length (gethash "rows" (aref (gethash "queries" (al-run focused-bundle)) 0))))))
            (sr-check "fact retains same enduring object identity"
                      (equal (gethash "entity_id" (aref (gethash "eligible_entities" context) 0))
                             (gethash "entity_id" (gethash "object" (aref (gethash "rows" (aref queries 0)) 0)))))))))))
(format t "SOURCE-REFERENCE ~d passed, 0 failed~%" *source-reference-checks*)
