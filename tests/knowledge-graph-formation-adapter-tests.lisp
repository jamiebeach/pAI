;;;; knowledge-graph-formation-adapter-tests.lisp -- strict KG2 wire boundary.
;;;; harness: full-system

(in-package :agent)

(defvar *kgfa-pass* 0)
(defvar *kgfa-fail* 0)

(defun kgfa-check (name condition)
  (if condition
      (progn (incf *kgfa-pass*) (format t "PASS ~a~%" name))
      (progn (incf *kgfa-fail*) (format t "FAIL ~a~%" name))))

(defun kgfa-signals-p (thunk)
  (handler-case (progn (funcall thunk) nil) (error () t)))

(defun kgfa-opened ()
  (obj "schema_version" 1 "persona_id" "adapter-persona"
       "disclosure_class" "private"
       "formation_revision" *knowledge-graph-formation-revision*
       "owner_revision" *knowledge-graph-formation-owner-revision*
       "source_event_ids" #(10) "source_memory_node_ids" #()
       "source_episode_ids" #( "episode:10")
       "evidence_records"
       (vector (obj "source_id" "event:10" "speaker_id" "operator"
                    "kind" "original-utterance" "timestamp" "2026-09-03T12:00:00Z"
                    "text" "Yesterday the operator said accessible visuals are required."
                    "text_sha256"
                    (%kgf-sha256
                     "Yesterday the operator said accessible visuals are required.")))
       "eligible_existing_nodes"
       (vector (obj "node_id" "kgf:entity:existing" "kind" "person"
                    "label" "Operator" "aliases" #( "FixtureOperator")
                    "classifications" #( "operator")
                    "participant_role" "operator"))
       "opened_at" 4000000000))

(defun kgfa-message (proposal &optional (name "write-knowledge-graph-formation"))
  (obj "role" "assistant" "content" :null
       "tool_calls"
       (vector
        (obj "id" "kgfa-call" "type" "function" "function"
             (obj "name" name "arguments"
                  (shasht:write-json proposal nil))))))

(defun kgfa-proposal (&optional (existing "kgf:entity:existing"))
  (obj "schema_version" 3
       "ontology_revision" *knowledge-graph-ontology-revision*
       "entities"
       (vector
        (obj "local_ref" "operator" "kind" "person" "label" "Operator"
             "aliases" #( "FixtureOperator") "classifications" #( "operator")
             "identity_action" "LINK_EXISTING"
             "existing_node_id" existing)
        (obj "local_ref" "need" "kind" "concept"
             "label" "Accessible visuals" "aliases" #()
             "classifications" #("accessibility")
             "identity_action" "NEW" "existing_node_id" :null))
       "relationships"
       (vector (obj "subject_ref" "operator" "predicate" "related_to"
                    "object_ref" "need" "relationship_action" "ASSERT"
                    "fact" "The operator requires accessible visuals."
                    "grounding"
                    (obj "schema_version" 1 "scope" "assertion"
                         "polarity" "positive" "attributed_to_ref" :null
                         "evidence"
                         (vector
                          (obj "source_id" "event:10"
                               "quote" "Yesterday the operator said accessible visuals are required.")))
                    "temporal"
                    (obj "schema_version" 1 "character" "standing-disposition"
                         "occurred_at" "invented" "valid_from" :null
                         "valid_until" :null)))))

(defun kgfa-review-message (rows)
  (kgfa-message (obj "schema_version" 1 "claim_reviews" rows)
                "review-knowledge-graph-evidence"))

(format t "~%== KG2 recursive provider adapter ==~%")

(let* ((schema (%recursive-kg-formation-schema))
       (function (gethash "function" (aref schema 0))))
  (kgfa-check "adapter advertises one strict native semantic function"
              (and (= 1 (length schema))
                   (gethash "strict" function)
                   (string= "write-knowledge-graph-formation"
                            (gethash "name" function)))))

(kgfa-check "valid native proposal crosses the wire boundary"
            (hash-table-p
             (%recursive-kg-formation-proposal
              (kgfa-message (kgfa-proposal)) (kgfa-opened))))

(let* ((proposal
         (%recursive-kg-formation-proposal
          (kgfa-message (kgfa-proposal "kgf:invented")) (kgfa-opened)))
       (operator (aref (gethash "entities" proposal) 0)))
  (kgfa-check "runtime replaces a guessed operator ID with the canonical participant"
              (and (string= "LINK_EXISTING"
                            (gethash "identity_action" operator))
                   (string= "kgf:entity:existing"
                            (gethash "existing_node_id" operator)))))

(let* ((proposal (kgfa-proposal))
       (duplicate
         (obj "local_ref" "operator-again" "kind" "person"
              "label" "Operator duplicate" "aliases" #()
              "classifications" #( "operator")
              "identity_action" "NEW" "existing_node_id" :null)))
  (setf (gethash "entities" proposal)
        (concatenate 'vector (gethash "entities" proposal)
                     (vector duplicate))
        (gethash "attributed_to_ref"
                 (gethash "grounding"
                          (aref (gethash "relationships" proposal) 0)))
        "operator-again")
  (let* ((normalized
           (%recursive-kg-formation-proposal
            (kgfa-message proposal) (kgfa-opened)))
         (entities (gethash "entities" normalized))
         (relationship (aref (gethash "relationships" normalized) 0)))
    (kgfa-check "duplicate proposed operator roles collapse before admission"
                (= 2 (length entities)))
    (kgfa-check "collapsed participant references use canonical runtime node"
                (let ((operator (find "operator" entities :test #'string=
                                      :key (lambda (row)
                                             (gethash "local_ref" row)))))
                  (and operator
                       (string= "kgf:entity:existing"
                                (gethash "existing_node_id" operator))
                       (string= "operator"
                                (gethash "attributed_to_ref"
                                         (gethash "grounding" relationship))))))))

(let* ((proposal (kgfa-proposal))
       (need (aref (gethash "entities" proposal) 1)))
  (setf (gethash "identity_action" need) "LINK_EXISTING"
        (gethash "existing_node_id" need) "kgf:invented")
  (kgfa-check "invented ordinary identity fails at the wire boundary"
              (kgfa-signals-p
               (lambda ()
                 (%recursive-kg-formation-proposal
                  (kgfa-message proposal) (kgfa-opened))))))

(kgfa-check "wrong native function name fails closed"
            (kgfa-signals-p
             (lambda ()
               (%recursive-kg-formation-proposal
                (kgfa-message (kgfa-proposal) "other-function")
                (kgfa-opened)))))

(kgfa-check "prose completion cannot masquerade as graph authority"
            (kgfa-signals-p
             (lambda ()
               (%recursive-kg-formation-proposal
                (obj "role" "assistant" "content" "I found a relationship.")
                (kgfa-opened)))))

(let* ((proposal (kgfa-proposal))
       (invalid (aref (gethash "relationships" proposal) 0)))
  (setf (gethash "predicate" invalid) "part_of")
  (kgfa-check "ontology-invalid relationship is dropped without losing entities"
              (let ((filtered
                      (%recursive-kg-formation-proposal
                       (kgfa-message proposal) (kgfa-opened))))
                (and (= 2 (length (gethash "entities" filtered)))
                     (zerop (length (gethash "relationships" filtered)))))))

(let* ((proposal (kgfa-proposal))
       (message
         (kgfa-review-message
          (vector
           (obj "claim_ref" "entity:operator"
                "verdict" "DIRECTLY_EVIDENCED" "evidence" "named operator")
           (obj "claim_ref" "entity:need" "verdict" "UNSUPPORTED"
                "evidence" "not stated as an independent entity")
           (obj "claim_ref" "relationship:0"
                "verdict" "REASONABLE_INFERENCE"
                "evidence" "plausible association only"))))
       (reviews (%recursive-kg-evidence-review message proposal))
       (reviewed (%recursive-kg-reviewed-proposal
                  proposal reviews (kgfa-opened))))
  (kgfa-check "unsupported entities and dependent relationships are removed"
              (and (= 1 (length (gethash "entities" reviewed)))
                   (zerop (length (gethash "relationships" reviewed)))
                   (string= "direct"
                            (gethash "evidence_status"
                                     (aref (gethash "entities" reviewed) 0))))))

(let* ((proposal (kgfa-proposal))
       (reviews
         (%recursive-kg-evidence-review
          (kgfa-review-message
           (vector
            (obj "claim_ref" "entity:operator" "verdict" "DIRECTLY_EVIDENCED"
                 "evidence" "operator is named")
            (obj "claim_ref" "entity:need" "verdict" "DIRECTLY_EVIDENCED"
                 "evidence" "requirement is stated")
            (obj "claim_ref" "relationship:0" "verdict" "DIRECTLY_EVIDENCED"
                 "evidence" "relationship is stated")))
          proposal))
       (reviewed (%recursive-kg-reviewed-proposal
                  proposal reviews (kgfa-opened)))
       (relationship (aref (gethash "relationships" reviewed) 0))
       (temporal (gethash "temporal" relationship)))
  (kgfa-check "relative time is derived from cited utterance rather than model text"
              (and (string= "2026-09-02" (gethash "occurred_at" temporal ""))
                   (not (string= "invented"
                                 (gethash "occurred_at" temporal "")))))
  (kgfa-check "grounding scope and exact quote survive evidence review"
              (let ((grounding (gethash "grounding" relationship)))
                (and (string= "assertion" (gethash "scope" grounding ""))
                     (string=
                      "Yesterday the operator said accessible visuals are required."
                      (gethash "quote"
                               (aref (gethash "evidence" grounding) 0)))))))

(let* ((proposal (kgfa-proposal))
       (relationship (aref (gethash "relationships" proposal) 0))
       (citation (aref (gethash "evidence" (gethash "grounding" relationship)) 0)))
  (setf (gethash "quote" citation) "This quote was never in the ledger.")
  (kgfa-check "one inexact relationship cannot discard valid entities"
              (let ((filtered
                      (%recursive-kg-formation-proposal
                       (kgfa-message proposal) (kgfa-opened))))
                (and (= 2 (length (gethash "entities" filtered)))
                     (zerop (length (gethash "relationships" filtered)))))))

(let* ((opened (kgfa-opened))
       (source (aref (gethash "evidence_records" opened) 0))
       (text "Yesterday the operator said **accessible visuals** are required.")
       (proposal (kgfa-proposal)))
  (setf (gethash "text" source) text
        (gethash "text_sha256" source) (%kgf-sha256 text))
  (let* ((filtered
           (%recursive-kg-formation-proposal (kgfa-message proposal) opened))
         (relationship (aref (gethash "relationships" filtered) 0))
         (quote
           (gethash "quote"
                    (aref (gethash "evidence"
                                   (gethash "grounding" relationship))
                          0))))
    (kgfa-check "markdown-cleaned excerpt maps back to one exact source span"
                (and (search quote text :test #'char=)
                     (search "**" quote :test #'char=)))))

(let* ((proposal (kgfa-proposal))
       (reviews
         (%recursive-kg-evidence-review
          (kgfa-review-message
           (vector
            (obj "claim_ref" "entity:operator" "verdict" "DIRECTLY_EVIDENCED"
                 "evidence" "operator is named")
            (obj "claim_ref" "entity:need" "verdict" "DIRECTLY_EVIDENCED"
                 "evidence" "requirement is stated")
            (obj "claim_ref" "relationship:0" "verdict" "REASONABLE_INFERENCE"
                 "evidence" "plausible but not stated")))
          proposal))
       (reviewed (%recursive-kg-reviewed-proposal
                  proposal reviews (kgfa-opened))))
  (kgfa-check "reasonable inference remains outside the authoritative graph"
              (zerop (length (gethash "relationships" reviewed)))))

(kgfa-check "incomplete evidence review fails closed"
            (kgfa-signals-p
             (lambda ()
               (%recursive-kg-evidence-review
                (kgfa-review-message
                 (vector
                  (obj "claim_ref" "entity:operator"
                       "verdict" "DIRECTLY_EVIDENCED"
                       "evidence" "named operator")))
                (kgfa-proposal)))))

(kgfa-check "invented entity cannot acquire exact-prior authority"
            (kgfa-signals-p
             (lambda ()
               (let* ((proposal (kgfa-proposal))
                      (reviews
                        (%recursive-kg-evidence-review
                         (kgfa-review-message
                          (vector
                           (obj "claim_ref" "entity:operator"
                                "verdict" "UNSUPPORTED"
                                "evidence" "not needed for this fixture")
                           (obj "claim_ref" "entity:need"
                                "verdict" "EXACT_PRIOR_GRAPH"
                                "evidence" "claimed prior identity")
                           (obj "claim_ref" "relationship:0"
                                "verdict" "UNSUPPORTED" "evidence" "absent")))
                         proposal)))
                 (%recursive-kg-reviewed-proposal
                  proposal reviews (kgfa-opened))))))

(kgfa-check "relationship cannot acquire unsupplied exact-prior authority"
            (kgfa-signals-p
             (lambda ()
               (let* ((proposal (kgfa-proposal))
                      (reviews
                        (%recursive-kg-evidence-review
                         (kgfa-review-message
                          (vector
                           (obj "claim_ref" "entity:operator"
                                "verdict" "DIRECTLY_EVIDENCED" "evidence" "named")
                           (obj "claim_ref" "entity:need"
                                "verdict" "DIRECTLY_EVIDENCED" "evidence" "named")
                           (obj "claim_ref" "relationship:0"
                                "verdict" "EXACT_PRIOR_GRAPH"
                                "evidence" "claimed prior fact")))
                         proposal)))
                 (%recursive-kg-reviewed-proposal
                  proposal reviews (kgfa-opened))))))

(let* ((payload
         (obj "entities" #( "Operator" "Operator" "Visual access")
              "subjects" #( "artifacts") "retrieval_cues" #( "color vision")
              "broader_categories" #() "unresolved_threads" #()))
       (event (obj "payload" payload))
       (cues (%recursive-kg-formation-cues event)))
  (kgfa-check "episode cues are bounded and case-insensitively distinct"
              (and (= 4 (length cues))
                   (<= (length cues) 24))))

(format t "~%KG2 adapter: ~d passed, ~d failed.~%"
        *kgfa-pass* *kgfa-fail*)
(when (plusp *kgfa-fail*) (uiop:quit 1))
