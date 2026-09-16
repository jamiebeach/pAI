;;;; harness: bare
(load (merge-pathnames "context-graph-runtime-candidates-tests.lisp" *load-truename*))
(in-package :pai.context-graph)
(let* ((ontology (gethash "ontology" (as-fixture))) (graph (make-context-graph ontology))
       (revision "personal-context-core-glm53-v1.2")
       (first (sm-episode "full-source-first" "I own a cat named Mina." 100)))
  (multiple-value-bind (context boundary) (lab-authority-context graph first 0)
    (let* ((raw (%cgs-expand context ontology revision (sm-proposal))) (review (sm-review context raw)))
      (%cgm-apply-reviewed graph boundary context raw review revision
        (%cg-object "request_digest" (%cg-authority-digest "model-review-input" (gethash "value" (%cgm-review-input context raw revision)))
                    "response_digest" (%cg-authority-digest "model-review-output" review)))))
  (let* ((text (concatenate 'string (make-string 1600 :initial-element #\x) " I own a cat named Mina."))
         (episode (sm-episode "full-source-late" text 200)))
    (multiple-value-bind (full boundary) (lab-authority-context graph episode 1)
      (let* ((before (%cg-authority-projection-digest graph "lab-agent" "lab-persona"))
             (old (%cgro-batch-context graph full 0 "staged" "bounded-v3"))
             (new (%cgro-batch-context graph full 0 "staged" "bounded-v4"))
             (handle (first (%cgs-keys (%cgs-handles new)))))
        (assert (zerop (length (gethash "eligible_entities" old))))
        (assert (= 1 (length (gethash "eligible_entities" new))))
        (assert (%cg-authority-equal-p (gethash "source_packet" old) (gethash "source_packet" new)))
        (assert (equal before (%cg-authority-projection-digest graph "lab-agent" "lab-persona")))
        (let* ((*cgt-protocol* "bounded-v4")
               (simple (sm-proposal)) (fact (aref (gethash "facts" simple) 0)))
          (setf (gethash "new_entities" simple) #() (gethash "object" fact) handle)
          (let ((facts (%cg-detach simple)))
            (remhash "new_entities" facts)
            (setf simple (%cgt-combine new ontology
                           (%cg-object "new_entities" #() "reuse_entities" (vector handle)) facts)))
          (let* ((raw (%cgs-expand new ontology revision simple)) (review (sm-review new raw))
                 (bound (%cg-detach boundary)) (count (context-graph-entity-count graph)))
            (setf (gethash "episode_id" bound) (gethash "episode_id" new))
            (loop for row across (gethash "claim_reviews" review)
                  for entity = (uiop:string-prefix-p "entity:" (gethash "claim_ref" row)) do
              (setf (gethash "source_reading" row) (if entity "not-applicable" "assertion")
                    (gethash "quality_checks" row)
                    (apply #'%cg-object (loop for key in +cgq-checks+ append
                      (list key (if (and entity (not (equal key "endpoint_identity"))) "not-applicable" "supported"))))))
            (%cgq-apply-reviewed graph bound new raw review revision
              (%cg-object "request_digest" (%cg-authority-digest "model-review-input" (gethash "value" (%cgq-review-input new raw revision)))
                          "response_digest" (%cg-authority-digest "model-review-output" review)))
            (assert (= count (context-graph-entity-count graph)))
            (assert (plusp (length (gethash "facts" (gethash "context" (%cg-authority-retrieve graph "lab-agent" "lab-persona" "Mina")))))))))))
)
(format t "FULL-SOURCE late mention, source fidelity, unchanged read state and existing-identity reuse passed~%")

(let* ((ontology (gethash "ontology" (as-fixture))) (graph (make-context-graph ontology))
       (episode (sm-episode "identity-alternatives" "The cat Mina was mentioned." 100)))
  (multiple-value-bind (seed boundary) (lab-authority-context graph episode 0)
    (declare (ignore boundary))
    (dotimes (i 13)
      (%cg-authority-new-entity graph
        (%cg-object "local_ref" (format nil "alternative-~d" i) "kind" "organism"
                    "label" "Mina" "aliases" #("Little Cat") "classifications" #("cat"))
        seed #() (make-hash-table :test #'equal))
      (when (member i '(1 12))
        (setf (context-graph-entity-scan-index graph)
              (coerce (sort (loop for id being the hash-keys of (context-graph-entities graph) collect id) #'string<) 'vector)
              (context-graph-projection-digest graph) nil)
        (multiple-value-bind (full ignored) (lab-authority-context graph episode 1)
          (declare (ignore ignored))
          (let ((before (%cg-authority-projection-digest graph "lab-agent" "lab-persona")))
            (if (= i 1)
                (progn
                  (assert (= 2 (length (gethash "eligible_entities"
                    (context-graph-select-runtime-candidates graph full "little cat" :target-kinds #() :ordinary-limit 12 :full-source-p t)))))
                  (assert (zerop (length (gethash "eligible_entities"
                    (context-graph-select-runtime-candidates graph full "minaret" :target-kinds #() :ordinary-limit 12 :full-source-p t))))))
                (assert (sm-error (lambda ()
                          (context-graph-select-runtime-candidates graph full "Mina" :target-kinds #() :ordinary-limit 12 :full-source-p t))
                                  "IDENTITY_CANDIDATE_LIMIT")))
            (assert (equal before (%cg-authority-projection-digest graph "lab-agent" "lab-persona")))))))))
(format t "FULL-SOURCE same-name alternatives, case-insensitive aliases, negative substring and overflow passed~%")

(let* ((ontology (gethash "ontology" (as-fixture))) (graph (make-context-graph ontology))
       (episode (sm-episode "selection-bounds" "I own a cat named Mina." 100)))
  (multiple-value-bind (seed boundary) (lab-authority-context graph episode 0)
    (declare (ignore boundary))
    (dotimes (i 48)
      (%cg-authority-new-entity graph
        (%cg-object "local_ref" (format nil "discovery-~d" i) "kind" "organism"
                    "label" "Mina" "aliases" #() "classifications" #("cat"))
        seed #() (make-hash-table :test #'equal)))
    (setf (context-graph-entity-scan-index graph)
          (coerce (sort (loop for id being the hash-keys of (context-graph-entities graph) collect id) #'string<) 'vector)
          (context-graph-projection-digest graph) nil)
    (multiple-value-bind (base ignored) (lab-authority-context graph episode 1)
      (declare (ignore ignored))
      (let* ((*cgt-protocol* "bounded-v4")
             (context (%cgro-batch-context graph base 0 "staged" *cgt-protocol*))
             (handles (%cgs-keys (%cgs-handles context)))
             (selection (%cg-object "new_entities" #() "reuse_entities" (coerce (subseq handles 0 12) 'vector))))
        (assert (= 48 (length handles)))
        (assert (= (+ 12 (length (%cgs-participants context))) (hash-table-count (%cgt-table context ontology selection))))
        (let ((bad (%cg-detach selection)))
          (setf (gethash "reuse_entities" bad) (vector (first handles) (first handles)))
          (assert (sm-error (lambda () (%cgt-table context ontology bad)) "STAGED_ENTITIES_INVALID"))
          (setf (gethash "reuse_entities" bad) (coerce (subseq handles 0 13) 'vector))
          (assert (sm-error (lambda () (%cgt-table context ontology bad)) "STAGED_ENTITIES_INVALID"))
          (setf (gethash "reuse_entities" bad) #("unknown_handle"))
          (assert (sm-error (lambda () (%cgt-table context ontology bad)) "STAGED_ENTITIES_INVALID")))
        (let* ((facts (sm-proposal)) (fact (aref (gethash "facts" facts) 0)))
          (remhash "new_entities" facts)
          (setf (gethash "object" fact) (first handles))
          (assert (%cgs-schema-valid-p facts (%cgt-fact-schema context ontology selection)))
          (setf (gethash "object" fact) (nth 12 handles))
          (assert (sm-error (lambda () (%cgt-combine context ontology selection facts)) "STAGED_FACTS_INVALID")))
        (setf (gethash "new_entities" selection)
              (map 'vector (lambda (i) (let ((entity (%cg-detach (aref (gethash "new_entities" (sm-proposal)) 0))))
                                        (setf (gethash "name" entity) (format nil "New cat ~d" i)) entity))
                   #(1 2 3 4 5 6 7 8 9 10 11 12)))
        (assert (= (+ 24 (length (%cgs-participants context))) (hash-table-count (%cgt-table context ontology selection))))))))
(format t "FULL-SOURCE 48 discovery / 12 reuse / 12 new bounds, duplicate and unselected-handle rejection passed~%")

(let* ((ontology (gethash "ontology" (as-fixture)))
       (revision "personal-context-core-glm53-v1.2")
       (graph (make-context-graph ontology))
       (episode (sm-episode "authority-shaped-facts" "I own a cat named Mina." 100)))
  (setf (gethash "sources" episode)
        (concatenate 'vector (gethash "sources" episode)
          (vector (%cg-object "source_id" "assistant-restatement"
                              "speaker_id" "active-persona"
                              "kind" "prior-agent-utterance"
                              "text" "The operator owns a cat named Mina."))))
  (multiple-value-bind (context ignored) (lab-authority-context graph episode 0)
    (declare (ignore ignored))
    (let* ((tool-text "Authenticated inventory observation: the active persona owns a cat named Mina.")
           (tool-source
             (%cg-object "source_id" "tool-inventory-observation"
                         "speaker_id" "tool:inventory"
                         "kind" "tool-observation" "timestamp" 100
                         "text" tool-text "text_sha256" (%cg-sha256 tool-text)
                         "identity" (%cg-object "principal_id" "tool:inventory"
                                                "binding_id" "binding:inventory"
                                                "conversation_id" "lab-conversation"
                                                "role" "other")
                         "resource_ref" (%cg-object "store" "event"
                                                    "resource_id" "event:inventory"
                                                    "version_id" "version:inventory:one"
                                                    "component" "content")))
           (selection (%cg-object "new_entities" (gethash "new_entities" (sm-proposal))
                                  "reuse_entities" #()))
           (operator-facts (%cg-detach (sm-proposal)))
           (assistant-facts (%cg-detach (sm-proposal)))
           (tool-facts (%cg-detach (sm-proposal))))
      (setf (gethash "sources" (gethash "source_packet" context))
            (concatenate 'vector
                         (gethash "sources" (gethash "source_packet" context))
                         (vector tool-source))
            (gethash "primary_source_ids" context)
            (concatenate 'vector (gethash "primary_source_ids" context)
                         #("tool-inventory-observation")))
      (remhash "new_entities" operator-facts)
      (remhash "new_entities" assistant-facts)
      (remhash "new_entities" tool-facts)
      (setf (gethash "source" (aref (gethash "evidence"
                                      (aref (gethash "facts" assistant-facts) 0)) 0))
            "source_2"
            (gethash "quote" (aref (gethash "evidence"
                                     (aref (gethash "facts" assistant-facts) 0)) 0))
            "The operator owns a cat named Mina.")
      (let ((fact (aref (gethash "facts" tool-facts) 0)))
        (setf (gethash "subject" fact) "active_persona"
              (gethash "attributed_to" fact) "active_persona"
              (gethash "statement" fact) tool-text
              (gethash "source" (aref (gethash "evidence" fact) 0)) "source_3"
              (gethash "quote" (aref (gethash "evidence" fact) 0)) tool-text))
      (let ((*cgt-protocol* "bounded-v4"))
        (let ((system (gethash "system"
                               (gethash "value"
                                        (%cgt-fact-input context ontology revision
                                                         selection)))))
          ;; Prior protocol prompts are durable receipt input. A newer policy
          ;; must not silently change their request digests.
          (assert (search "Assertion facts about the operator or external world require exact original operator utterance evidence"
                          system))
          (assert (not (search "Under bounded-v5" system))))
        (assert (%cgs-schema-valid-p assistant-facts
                                    (%cgt-fact-schema context ontology selection))))
      (let ((*cgt-protocol* "bounded-v5"))
        (let ((system (gethash "system"
                               (gethash "value"
                                        (%cgt-fact-input context ontology revision
                                                         selection)))))
          (assert (search "Under bounded-v5" system)))
        (assert (%cgs-schema-valid-p operator-facts
                                    (%cgt-fact-schema context ontology selection)))
        (assert (%cgs-schema-valid-p tool-facts
                                    (%cgt-fact-schema context ontology selection)))
        (assert (not (%cgs-schema-valid-p assistant-facts
                                         (%cgt-fact-schema context ontology selection))))
        (let* ((revision "personal-context-core-glm53-v1.2")
               (raw (%cgs-expand context ontology revision
                                 (%cgt-combine context ontology selection
                                                tool-facts)))
               (prepared (gethash "proposal"
                                  (gethash "value"
                                           (%cgm-prepare context raw revision))))
               (relationship (aref (gethash "relationships" prepared) 0)))
          (let ((*cg-assertion-evidence-policy* "operator-utterance-v1"))
            (assert (not (%cg-authority-assertion-evidence-p context relationship))))
          (let ((*cg-assertion-evidence-policy* "direct-observation-v2"))
            (assert (%cg-authority-assertion-evidence-p context relationship))
            (assert (equal "original"
                           (%cg-authority-source-basis
                            (vector (%cg-citation-exact-span
                                     context
                                     (aref (gethash "evidence"
                                                   (gethash "grounding"
                                                            relationship)) 0)))))))
          (let* ((assistant-raw
                   (let ((*cgt-protocol* "bounded-v4"))
                     (%cgs-expand context ontology revision
                                  (%cgt-combine context ontology selection
                                                 assistant-facts))))
                 (assistant-prepared
                   (gethash "proposal"
                            (gethash "value"
                                     (%cgm-prepare context assistant-raw
                                                   revision))))
                 (assistant-relationship
                   (aref (gethash "relationships" assistant-prepared) 0))
                 (*cg-assertion-evidence-policy* "direct-observation-v2"))
            (assert (not (%cg-authority-assertion-evidence-p
                          context assistant-relationship)))))
        (setf (gethash "scope" (aref (gethash "facts" assistant-facts) 0))
              "reported-speech")
        (assert (%cgs-schema-valid-p assistant-facts
                                    (%cgt-fact-schema context ontology selection)))))))
(let* ((ontology (gethash "ontology" (as-fixture)))
       (revision "personal-context-core-glm53-v1.2")
       (episode (sm-episode "bounded-v6-inference-prompt"
                            "The operator owns a cat named Mina." 300))
       (selection (%cg-object "new_entities" (gethash "new_entities"
                                                       (sm-proposal))
                              "reuse_entities" #())))
  (multiple-value-bind (context boundary)
      (lab-authority-context (make-context-graph ontology) episode 0)
    (declare (ignore boundary))
    (let ((*cgt-protocol* "bounded-v6")
          (*cgt-fact-input-revision* "selected-signatures-v4"))
      (let* ((spec (%cgt-fact-input context ontology revision selection))
             (value (gethash "value" spec))
             (input (gethash "input" value))
             (system (gethash "system" value)))
        (assert (not (nth-value 1 (gethash "known_entities" input))))
        (assert (not (nth-value 1 (gethash "candidate_scan" input))))
        (assert (nth-value 1 (gethash "entity_table" input)))
        (assert (%cg-closed-keys-p (gethash "ontology" input)
                                   '("edge_types")))
        (assert (plusp (length (gethash "edge_types"
                                        (gethash "ontology" input)))))
        (assert (search "Assertions may be either directly supported or useful reasonable inferences"
                        system))
        (assert (search "Prior-agent prose can be an inference premise but never direct authority"
                        system))))))
(format t "FULL-SOURCE bounded-v6 explicitly separates direct authority from reviewed inference premises passed~%")
(let* ((table (make-hash-table :test #'equal))
       (ontology
         (%cg-object
          "edge_types"
          (vector
           (%cg-object "name" "parent_of" "subject_types" '("person")
                       "object_types" '("person"))
           (%cg-object "name" "daughter_of" "subject_types" '("person")
                       "object_types" '("person"))
           (%cg-object "name" "has_age" "subject_types" '("person")
                       "object_types" '("attribute_value"))))))
  (setf (gethash "person_1" table)
        (%cg-object "kind" "person")
        (gethash "person_2" table)
        (%cg-object "kind" "person")
        (gethash "age_1" table)
        (%cg-object "kind" "attribute_value"))
  (let ((*cgt-fact-input-revision* "selected-signatures-v4"))
    (let ((groups (%cgt-fact-choice-groups table ontology)))
      (assert (= 2 (length groups)))
      (assert (equal '("parent_of" "daughter_of")
                     (gethash "predicates" (aref groups 0))))
      (assert (equal '("has_age")
                     (gethash "predicates" (aref groups 1))))))
  (let ((*cgt-fact-input-revision* "selected-signatures-v3"))
    (let ((groups (%cgt-fact-choice-groups table ontology)))
      (assert (= 3 (length groups)))
      (assert (every (lambda (group)
                       (= 1 (length (gethash "predicates" group))))
                     groups)))))
(format t "FULL-SOURCE identical endpoint signatures share one closed schema branch passed~%")
(format t "FULL-SOURCE v9 assertion authority follows authenticated evidence class, independent of operator subject, while reports remain representable passed~%")
