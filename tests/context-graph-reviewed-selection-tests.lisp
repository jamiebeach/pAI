;;;; harness: bare
(load (merge-pathnames "context-graph-simple-model-tests.lisp" *load-truename*))
(in-package :pai.context-graph)
(defvar *selection-checks* 0)
(defun rs-check (name value)
  (unless value (error "FAIL ~a" name))
  (incf *selection-checks*) (format t "PASS ~a~%" name))
(multiple-value-bind (partition unused participants) (as-fixture)
  (declare (ignore unused participants))
  (let* ((ontology (gethash "ontology" partition)) (revision "personal-context-core-glm53-v1.2"))
    (labels ((trial (mode)
               (let* ((graph (make-context-graph ontology))
                      (episode (sm-episode "selection-fixture" "I own a cat named Mina." 100))
                      (simple (sm-proposal)))
                 (when (member mode '(:assistant :assistant-inference :reported))
                   (setf (gethash "speaker_id" (aref (gethash "sources" episode) 0)) "active-persona"
                         (gethash "kind" (aref (gethash "sources" episode) 0)) "prior-agent-utterance"))
                 (when (eq mode :reported)
                   (setf (gethash "scope" (aref (gethash "facts" simple) 0)) "reported-speech"))
                 (when (member mode '(:optional :attribution))
                   (setf (gethash "new_entities" simple)
                         (concatenate 'vector (gethash "new_entities" simple)
                                      (vector (%cg-object "name" "Unsupported extra" "kind" "person" "alternate_names" #() "categories" #())))))
                 (when (eq mode :attribution)
                   (setf (gethash "attributed_to" (aref (gethash "facts" simple) 0)) "new_2"))
                 (multiple-value-bind (context boundary) (lab-authority-context graph episode 0)
                   (let* ((raw (%cgs-expand context ontology revision simple))
                          (review (sm-review context raw))
                          (before (%cg-authority-canonical-json raw)))
                     (when (member mode '(:optional :attribution :dependency))
                       (setf (gethash "verdict" (find (if (member mode '(:optional :attribution)) "entity:new_2" "entity:new_1")
                                                     (gethash "claim_reviews" review) :test #'equal :key (lambda (r) (gethash "claim_ref" r))))
                             "UNSUPPORTED"))
                     (let* ((*cgm-review-admission-policy*
                              (if (eq mode :assistant-inference)
                                  "reviewed-inference-v1"
                                  *cgm-review-admission-policy*))
                            (*cg-claim-identity-protocol*
                              (if (eq mode :assistant-inference)
                                  "claim-identity-v2"
                                  *cg-claim-identity-protocol*))
                            (spec (%cgm-review-input context raw revision))
                            (binding (%cg-object "request_digest" (%cg-authority-digest "model-review-input" (gethash "value" spec))
                                                 "response_digest" (%cg-authority-digest "model-review-output" review)))
                            (result (%cgm-apply-reviewed graph boundary context raw review revision binding)))
                       (rs-check "selection leaves original proposal immutable" (equal before (%cg-authority-canonical-json raw)))
                       (values graph result context raw)))))))
      (multiple-value-bind (graph result) (trial :optional)
        (rs-check "unsupported optional entity does not discard useful fact"
                  (and (equal "accepted" (gethash "status" result)) (= 1 (context-graph-fact-count graph))))
        (rs-check "omission is explicit and bound"
                  (= 1 (length (gethash "omissions" (gethash "selection_receipt" (gethash "value" result)))))))
      (multiple-value-bind (graph result) (trial :dependency)
        (rs-check "rejected endpoint removes dependent fact"
                  (and (zerop (context-graph-fact-count graph))
                       (= 2 (length (gethash "omissions" (gethash "selection_receipt" (gethash "value" result))))))))
      (multiple-value-bind (graph result) (trial :attribution)
        (declare (ignore result))
        (rs-check "rejected attribution removes dependent fact" (zerop (context-graph-fact-count graph))))
      (multiple-value-bind (graph result context raw) (trial :assistant)
        (rs-check "positive reviewer cannot promote assistant assertion" (zerop (context-graph-fact-count graph)))
        (rs-check "unsupported incident facts do not leave an orphan entity"
                  (and (zerop (context-graph-entity-count graph))
                       (find "NO_RETAINED_RELATIONSHIP"
                             (gethash "omissions" (gethash "selection_receipt" (gethash "value" result)))
                             :test #'equal :key (lambda (row) (gethash "reason" row)))))
        (rs-check "projection independently enforces factual authority"
                  (sm-error (lambda () (%cg-authority-apply-claim graph (aref (gethash "relationships" raw) 0) context nil nil))
                            "ASSERTION_SOURCE_AUTHORITY_INVALID")))
      (multiple-value-bind (graph result) (trial :assistant-inference)
        (let ((fact (loop for value being the hash-values of
                          (context-graph-facts graph)
                          return value)))
          (rs-check "inference policy downgrades a direct verdict over a prior-agent premise"
                    (and (equal "accepted" (gethash "status" result))
                         (= 1 (context-graph-fact-count graph))
                         (equal "inference" (gethash "evidence_status" fact))))))
      (multiple-value-bind (graph result) (trial :reported)
        (rs-check "assistant report is retained without factual retrieval"
                  (and (equal "accepted" (gethash "status" result)) (= 1 (context-graph-fact-count graph))
                       (zerop (length (gethash "rows" (%cg-authority-search graph "lab-agent" "lab-persona" "Mina"))))))))
    (let* ((graph (make-context-graph ontology))
           (episode (sm-episode "reviewed-inference-fixture"
                                "The operator has a cat named Mina." 100)))
      (setf (gethash "speaker_id" (aref (gethash "sources" episode) 0))
            "active-persona"
            (gethash "kind" (aref (gethash "sources" episode) 0))
            "prior-agent-utterance")
      (let ((simple (sm-proposal)))
        (setf (gethash "quote"
                       (aref (gethash "evidence"
                                     (aref (gethash "facts" simple) 0))
                             0))
              "The operator has a cat named Mina."
              (gethash "attributed_to" (aref (gethash "facts" simple) 0))
              "operator")
        (multiple-value-bind (context boundary)
            (lab-authority-context graph episode 0)
          (let* ((raw (%cgs-expand context ontology revision simple))
                 (review (sm-review context raw)))
            (loop for row across (gethash "claim_reviews" review)
                  do (setf (gethash "verdict" row) "REASONABLE_INFERENCE"
                           (gethash "evidence" row)
                           "The authenticated agent utterance is a premise, not direct authority."))
            (let* ((spec (%cgm-review-input context raw revision))
                   (binding
                     (%cg-object
                      "request_digest"
                      (%cg-authority-digest "model-review-input"
                                            (gethash "value" spec))
                      "response_digest"
                      (%cg-authority-digest "model-review-output" review))))
              (rs-check "legacy admission still omits reasonable inference"
                        (let ((result (%cgm-apply-reviewed
                                      graph boundary context raw review revision
                                      binding)))
                          (and (equal "rejected" (gethash "status" result))
                               (zerop (context-graph-fact-count graph)))))
              (let* ((*cgm-review-admission-policy* "reviewed-inference-v1")
                     (*cg-claim-identity-protocol* "claim-identity-v2")
                     (result (%cgm-apply-reviewed
                              graph boundary context raw review revision binding))
                     (fact (loop for value being the hash-values of
                                 (context-graph-facts graph)
                                 return value)))
                (rs-check "versioned admission retains reviewed inference"
                          (and (equal "accepted" (gethash "status" result))
                               (= 1 (context-graph-fact-count graph))
                               (equal "inference"
                                      (gethash "evidence_status" fact))
                               (equal "derived"
                                      (gethash "source_basis"
                                               (gethash "grounding" fact)))))
                (rs-check "verified retrieval excludes reviewed inference"
                          (not (%cg-evidence-policy-allows-p fact "verified")))
                (rs-check "inferred retrieval includes reviewed inference"
                          (%cg-evidence-policy-allows-p fact "inferred"))
                (let* ((direct-episode
                         (sm-episode "reviewed-inference-confirmation"
                                     "I own a cat named Mina." 200))
                       (direct-simple (sm-proposal)))
                  (setf (gethash "statement"
                                 (aref (gethash "facts" direct-simple) 0))
                        "Mina belongs to the operator.")
                  (multiple-value-bind (direct-context direct-boundary)
                      (lab-authority-context graph direct-episode 1)
                    (let* ((direct-raw
                             (%cgs-expand direct-context ontology revision
                                          direct-simple))
                           (descriptor (aref (gethash "entities" direct-raw) 0))
                           (existing
                             (find "Mina"
                                   (gethash "eligible_entities" direct-context)
                                   :test #'equal
                                   :key (lambda (row) (gethash "label" row)))))
                      (setf (gethash "identity_action" descriptor)
                            "LINK_EXISTING"
                            (gethash "existing_node_id" descriptor)
                            (gethash "node_id" existing))
                      (let* ((direct-review
                               (sm-review direct-context direct-raw))
                             (direct-spec
                               (%cgm-review-input direct-context direct-raw
                                                  revision))
                             (direct-binding
                               (%cg-object
                                "request_digest"
                                (%cg-authority-digest
                                 "model-review-input"
                                 (gethash "value" direct-spec))
                                "response_digest"
                                (%cg-authority-digest
                                 "model-review-output" direct-review)))
                             (direct-result
                               (%cgm-apply-reviewed
                                graph direct-boundary direct-context direct-raw
                                direct-review revision direct-binding))
                             (promoted
                               (loop for value being the hash-values of
                                     (context-graph-facts graph)
                                     return value)))
                        (rs-check "later direct evidence promotes rather than duplicates a typed inference"
                                  (and (equal "accepted"
                                              (gethash "status" direct-result))
                                       (= 1 (context-graph-fact-count graph))
                                       (equal "direct"
                                              (gethash "evidence_status"
                                                       promoted))
                                       (= 2 (length
                                             (gethash "evidence_records"
                                                      promoted)))))))))))))))
    (let* ((graph (make-context-graph ontology))
           (episode (sm-episode "reviewed-inference-correction-premise"
                                "The operator has a cat named Mina." 100))
           (simple (sm-proposal)))
      (setf (gethash "speaker_id" (aref (gethash "sources" episode) 0))
            "active-persona"
            (gethash "kind" (aref (gethash "sources" episode) 0))
            "prior-agent-utterance"
            (gethash "quote"
                     (aref (gethash "evidence"
                                   (aref (gethash "facts" simple) 0))
                           0))
            "The operator has a cat named Mina."
            (gethash "attributed_to" (aref (gethash "facts" simple) 0))
            "operator")
      (multiple-value-bind (context boundary)
          (lab-authority-context graph episode 0)
        (let* ((raw (%cgs-expand context ontology revision simple))
               (review (sm-review context raw)))
          (loop for row across (gethash "claim_reviews" review)
                do (setf (gethash "verdict" row) "REASONABLE_INFERENCE"
                         (gethash "evidence" row)
                         "The authenticated agent utterance is a premise, not direct authority."))
          (let* ((spec (%cgm-review-input context raw revision))
                 (binding
                   (%cg-object
                    "request_digest"
                    (%cg-authority-digest "model-review-input"
                                          (gethash "value" spec))
                    "response_digest"
                    (%cg-authority-digest "model-review-output" review))))
            (let ((*cgm-review-admission-policy* "reviewed-inference-v1")
                  (*cg-claim-identity-protocol* "claim-identity-v2"))
              (%cgm-apply-reviewed graph boundary context raw review revision
                                   binding)))))
      (let* ((denial-episode
               (sm-episode "reviewed-inference-correction"
                           "I do not own a cat named Mina." 200))
             (denial-simple (sm-proposal)))
        (setf (gethash "polarity" (aref (gethash "facts" denial-simple) 0))
              "negative"
              (gethash "quote"
                       (aref (gethash "evidence"
                                     (aref (gethash "facts" denial-simple) 0))
                             0))
              "I do not own a cat named Mina.")
        (multiple-value-bind (context boundary)
            (lab-authority-context graph denial-episode 1)
          (let* ((raw (%cgs-expand context ontology revision denial-simple))
                 (descriptor (aref (gethash "entities" raw) 0))
                 (existing
                   (find "Mina" (gethash "eligible_entities" context)
                         :test #'equal
                         :key (lambda (row) (gethash "label" row)))))
            (setf (gethash "identity_action" descriptor) "LINK_EXISTING"
                  (gethash "existing_node_id" descriptor)
                  (gethash "node_id" existing))
            (let* ((review (sm-review context raw))
                   (spec (%cgm-review-input context raw revision))
                   (binding
                     (%cg-object
                      "request_digest"
                      (%cg-authority-digest "model-review-input"
                                            (gethash "value" spec))
                      "response_digest"
                      (%cg-authority-digest "model-review-output" review))))
              (let* ((*cgm-review-admission-policy* "reviewed-inference-v1")
                     (*cg-claim-identity-protocol* "claim-identity-v2")
                     (result
                       (%cgm-apply-reviewed graph boundary context raw review
                                            revision binding))
                     (retired
                       (find "retired"
                             (loop for value being the hash-values of
                                   (context-graph-facts graph)
                                   collect value)
                             :test #'equal
                             :key (lambda (row) (gethash "status" row))))
                     (current
                       (find "current"
                             (loop for value being the hash-values of
                                   (context-graph-facts graph)
                                   collect value)
                             :test #'equal
                             :key (lambda (row) (gethash "status" row))))
                     (before-replay
                       (%cg-authority-canonical-json
                        (context-graph-facts graph)))
                     (replay
                       (%cgm-apply-reviewed graph boundary context raw review
                                            revision binding)))
                (rs-check "later direct contradiction retires only the matching inference"
                          (and (equal "accepted" (gethash "status" result))
                               (= 2 (context-graph-fact-count graph))
                               retired current
                               (equal "inference"
                                      (gethash "evidence_status" retired))
                               (equal "direct-contradiction-v1"
                                      (gethash "retirement_basis" retired))
                               (equal (gethash "fact_id" current)
                                      (gethash "replacement_fact_id" retired))
                               (equal "negative"
                                      (gethash "polarity"
                                               (gethash "grounding" current)))
                               (equal "direct"
                                      (gethash "evidence_status" current))))
                (rs-check "retired inference is absent from the current proposition index"
                          (and (null (gethash (gethash "identity_key" retired)
                                              (context-graph-current-triples graph)))
                               (equal (gethash "fact_id" current)
                                      (gethash (gethash "identity_key" current)
                                               (context-graph-current-triples graph)))))
                (rs-check "correction application is idempotent under replay"
                          (and (equal "already-applied"
                                      (gethash "status"
                                               (gethash "application"
                                                        (gethash "value" replay))))
                               (equal before-replay
                                      (%cg-authority-canonical-json
                                       (context-graph-facts graph)))))))))))
    (let* ((graph (make-context-graph ontology)) (episode (sm-episode "names" "My name is Rowan." 100)))
      (multiple-value-bind (context boundary) (lab-authority-context graph episode 0)
        (declare (ignore boundary))
        (setf (gethash "label" (aref (gethash "participants" context) 0)) "Rowan")
        (let* ((input (gethash "input" (gethash "value" (%cgt-entity-input context ontology revision))))
               (table (%cgt-table context ontology (%cg-object "new_entities" #()))))
          (rs-check "entity stage receives trusted participant name"
                    (equal "Rowan" (gethash "name" (aref (gethash "participants" input) 0))))
          (rs-check "fact stage receives same trusted name" (equal "Rowan" (gethash "name" (gethash "operator" table)))))
        (let* ((selection (%cg-object "new_entities" (vector (%cg-object "name" "Rowan" "kind" "person" "alternate_names" #() "categories" #()))))
               (table (%cgt-table context ontology selection)))
          (rs-check "same-name third party is not merged by label"
                    (and (gethash "new_1" table) (gethash "operator" table))))))))
(rs-check "short greeting is not durable relatedness"
          (%cgm-transient-greeting-p
           (%cg-object "predicate" "related_to" "fact" "Operator greets Mina"
                       "grounding" (%cg-object "evidence"
                                     (vector (%cg-object "source_id" "event:1" "quote" "Hey Mina"))))))

(let* ((*cgm-review-admission-policy* "reviewed-inference-v1")
       (*cgq-durable-relevance-policy* "cross-turn-v1")
       (*cg-claim-identity-protocol* "claim-identity-v2")
       (ontology (gethash "ontology" (as-fixture)))
       (revision "personal-context-core-glm53-v1.2"))
  (labels ((review-for (context raw durability)
             (let* ((prepared (gethash "proposal"
                                       (gethash "value"
                                                (%cgm-prepare context raw revision))))
                    (claims (%cgm-claims prepared)))
               (%cg-object
                "schema_version" 2 "revision_reviews" #()
                "claim_reviews"
                (map 'vector
                     (lambda (claim)
                       (let ((entity (equal "entity"
                                            (gethash "claim_kind" claim))))
                         (%cg-object
                          "claim_ref" (gethash "claim_ref" claim)
                          "verdict" "DIRECTLY_EVIDENCED"
                          "evidence" "Authenticated prior-agent premise."
                          "source_reading" (if entity "not-applicable"
                                               "reported-speech")
                          "quality_checks"
                          (apply #'%cg-object
                                 (loop for key in (%cgq-review-checks)
                                       append
                                       (list key
                                             (cond
                                               ((and entity
                                                     (not (equal key
                                                                 "endpoint_identity")))
                                                "not-applicable")
                                               ((equal key "durable_relevance")
                                                durability)
                                               (t "supported"))))))))
                     claims)))))
    (let* ((graph (make-context-graph ontology))
           (episode (sm-episode "reported-premise"
                                "I own a cat named Mina." 100)))
      (setf (gethash "speaker_id" (aref (gethash "sources" episode) 0))
            "active-persona"
            (gethash "kind" (aref (gethash "sources" episode) 0))
            "prior-agent-utterance")
      (multiple-value-bind (context boundary)
          (lab-authority-context graph episode 0)
        (let* ((raw (%cgs-expand context ontology revision (sm-proposal)))
               (review (review-for context raw "supported"))
               (spec (%cgq-review-input context raw revision))
               (binding
                 (%cg-object
                  "request_digest"
                  (%cg-authority-digest "model-review-input"
                                        (gethash "value" spec))
                  "response_digest"
                  (%cg-authority-digest "model-review-output" review)))
               (result (%cgq-apply-reviewed graph boundary context raw review
                                             revision binding))
               (fact (loop for value being the hash-values of
                           (context-graph-facts graph) return value)))
          (rs-check "reported prior-agent premise remains an explicit inference"
                    (and (equal "accepted" (gethash "status" result))
                         fact
                         (equal "inference"
                                (gethash "evidence_status" fact)))))))
    (let* ((graph (make-context-graph ontology))
           (episode (sm-episode "transient-premise"
                                "I own a cat named Mina." 200)))
      (setf (gethash "speaker_id" (aref (gethash "sources" episode) 0))
            "active-persona"
            (gethash "kind" (aref (gethash "sources" episode) 0))
            "prior-agent-utterance")
      (multiple-value-bind (context boundary)
          (lab-authority-context graph episode 0)
        (let* ((raw (%cgs-expand context ontology revision (sm-proposal)))
               (review (review-for context raw "unsupported"))
               (spec (%cgq-review-input context raw revision))
               (binding
                 (%cg-object
                  "request_digest"
                  (%cg-authority-digest "model-review-input"
                                        (gethash "value" spec))
                  "response_digest"
                  (%cg-authority-digest "model-review-output" review)))
               (result (%cgq-apply-reviewed graph boundary context raw review
                                             revision binding)))
          (rs-check "unsupported durable relevance removes relationship and orphan"
                    (and (equal "rejected" (gethash "status" result))
                         (zerop (context-graph-fact-count graph))
                         (zerop (context-graph-entity-count graph))
                         (find "REVIEW_NOT_DIRECT"
                               (gethash "omissions"
                                        (gethash "selection_receipt"
                                                 (gethash "value" result)))
                               :test #'equal
                               :key (lambda (row)
                                      (gethash "reason" row))))))))))
(let ((*cgm-review-admission-policy* "direct-only-v1"))
  (rs-check "legacy review schema keeps its durable inference vocabulary"
            (search "REASONABLE_INFERENCE"
                    (%cg-authority-canonical-json
                     (%cgm-review-schema 1 0)))))
(let ((*cgm-review-admission-policy* "reviewed-inference-v1"))
  (rs-check "inference review schema retains the explicit inference verdict"
            (search "REASONABLE_INFERENCE"
                    (%cg-authority-canonical-json
                     (%cgm-review-schema 1 0)))))
(format t "REVIEWED-SELECTION ~d passed, 0 failed~%" *selection-checks*)
