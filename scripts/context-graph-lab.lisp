;;;; context-graph-lab.lisp -- disposable graph creation/query harness.

(require :asdf)
(load (uiop:getenv "PAI_QUICKLISP_SETUP"))
(asdf:load-asd (pathname (uiop:getenv "PAI_CONTEXT_GRAPH_ASD")))
(asdf:load-system :pai-context-graph :force t)

(in-package :pai.context-graph)
(load (merge-pathnames "context-graph-authority-session.lisp" *load-truename*))

(defun lab-read-json (path)
  (let* ((text (uiop:read-file-string path :external-format :utf-8))
         (legacy (shasht:read-json text)))
    (if (and (hash-table-p legacy) (gethash "authority_operation" legacy))
        (let ((shasht:*read-default-true-value* :true)
              (shasht:*read-default-false-value* :false)
              (shasht:*read-default-null-value* :null))
          (shasht:read-json text))
        legacy)))

(defun lab-write-json (path value)
  (with-open-file (out path :direction :output :if-exists :supersede
                            :if-does-not-exist :create
                            :external-format :utf-8)
    (write-string (if (and (hash-table-p value) (gethash "authority_revision" value))
                      (%cg-authority-canonical-json value)
                      (shasht:write-json value nil)) out)))

(let* ((bundle (lab-read-json (uiop:getenv "PAI_CONTEXT_GRAPH_BUNDLE")))
       (output (pathname (uiop:getenv "PAI_CONTEXT_GRAPH_OUTPUT")))
       (normalization (gethash "participant_normalization" bundle)))
  ;; Schema 2 authority primitives are explicit modes, never silently selected
  ;; by a legacy formation. Projection replay is synthetic/offline qualification;
  ;; it is not a live authority-context or memory-access boundary.
  (when (gethash "authority_operation" bundle)
    (unless (eql 2 (gethash "schema_version" bundle))
      (error "Authority lab requires schema 2"))
    (let ((keys (cond ((member (gethash "authority_operation" bundle) '("identity-formation-session" "identity-formation-v2-session" "identity-formation-v3-session" "identity-formation-v4-session" "identity-formation-v5-session" "identity-formation-v6-session") :test #'equal)
                      (append '("source_kind" "ontology" "ontology_revision" "history" "step" "queries")
                              (loop for key in '("retrieval_lexicon" "retrieval_policy" "descriptor_guide")
                                    when (nth-value 1 (gethash key bundle)) collect key)))
                     ((equal "retrieval-session" (gethash "authority_operation" bundle))
                      (append '("source_kind" "ontology" "ontology_revision" "history" "queries" "retrieval_lexicon")
                              (when (nth-value 1 (gethash "retrieval_policy" bundle)) '("retrieval_policy"))))
                      ((equal "source-span" (gethash "authority_operation" bundle)) '("source" "proposed_quote"))
                      ((equal "participants" (gethash "authority_operation" bundle)) '("proposal" "authority_context"))
                      ((equal "revision-plan" (gethash "authority_operation" bundle)) '("authority_context" "grant" "current_view"))
                      ((equal "prepare" (gethash "authority_operation" bundle)) '("authority_context" "proposal"))
                      ((equal "formation-authority" (gethash "authority_operation" bundle)) '("authority_context" "proposal" "raw_review" "response_binding" "current_views"))
                      ((equal "projection-replay" (gethash "authority_operation" bundle)) '("ontology" "agent_id" "persona_id" "formations" "queries"))
                      ((member (gethash "authority_operation" bundle) '("model-session" "simple-model-session" "staged-model-session" "simple-source-reference-session" "staged-source-reference-session" "correction-only-session") :test #'equal)
                       '("source_kind" "ontology" "ontology_revision" "history" "step" "queries"))
                      ((equal "correction-scopes" (gethash "authority_operation" bundle)) '("partition_view" "policy" "participants")))))
      (unless (and keys (%cg-closed-keys-p bundle (append '("schema_version" "authority_operation") keys)))
        (error "Invalid authority lab envelope")))
    (let ((result
            (cond
              ((member (gethash "authority_operation" bundle) '("identity-formation-session" "identity-formation-v2-session" "identity-formation-v3-session" "identity-formation-v4-session" "identity-formation-v5-session" "identity-formation-v6-session") :test #'equal)
               (lab-authority-input-result (lambda () (lab-identity-session bundle))))
              ((equal "retrieval-session" (gethash "authority_operation" bundle))
               (lab-authority-input-result (lambda () (lab-authority-retrieval-session bundle))))
              ((member (gethash "authority_operation" bundle) '("model-session" "simple-model-session" "staged-model-session" "simple-source-reference-session" "staged-source-reference-session" "correction-only-session") :test #'equal)
               (lab-authority-input-result (lambda () (lab-authority-session bundle
                 (cond ((equal "correction-only-session" (gethash "authority_operation" bundle)) :correction)
                       ((member (gethash "authority_operation" bundle) '("staged-model-session" "staged-source-reference-session") :test #'equal) :staged)
                       ((member (gethash "authority_operation" bundle) '("simple-model-session" "simple-source-reference-session") :test #'equal) t))
                 (member (gethash "authority_operation" bundle) '("simple-source-reference-session" "staged-source-reference-session" "correction-only-session") :test #'equal)))))
              ((equal "projection-replay" (gethash "authority_operation" bundle))
               (unless (and (%cg-authority-array-p (gethash "formations" bundle) 32)
                            (plusp (length (gethash "formations" bundle)))
                            (%cg-authority-strings-p (gethash "queries" bundle) 32 1000))
                 (error "Invalid projection replay bounds"))
               (let ((graph (make-context-graph (gethash "ontology" bundle))) (applications nil))
                 (loop for formation across (gethash "formations" bundle)
                       do (unless (%cg-closed-keys-p formation '("boundary" "authority_context" "proposal" "raw_review" "response_binding"))
                            (error "Invalid projection formation envelope"))
                          (let* ((context (gethash "authority_context" formation))
                                 (proposal (gethash "proposal" formation))
                                 (prepared (gethash "proposal" (gethash "value" (context-graph-prepare-authority context proposal))))
                                 (review (if (eq :null (gethash "raw_review" formation)) :null
                                             (gethash "value" (context-graph-validate-revision-review
                                                               context prepared (gethash "raw_review" formation) (gethash "response_binding" formation))))))
                            (push (%cg-apply-authority-episode graph (gethash "boundary" formation) proposal context review) applications)))
                 (%cg-authority-result "accepted"
                   (%cg-object "applications" (coerce (nreverse applications) 'vector)
                               "entity_count" (context-graph-entity-count graph) "fact_count" (context-graph-fact-count graph)
                               "version_count" (hash-table-count (context-graph-entity-versions graph))
                               "watermark" (%cg-authority-watermark graph (gethash "agent_id" bundle) (gethash "persona_id" bundle))
                               "queries" (map 'vector (lambda (query) (%cg-authority-search graph (gethash "agent_id" bundle) (gethash "persona_id" bundle) query))
                                              (gethash "queries" bundle))))))
              ((equal "source-span" (gethash "authority_operation" bundle))
               (context-graph-resolve-source-span
                (gethash "source" bundle) (gethash "proposed_quote" bundle)))
              ((equal "participants" (gethash "authority_operation" bundle))
               (context-graph-normalize-participants
                (gethash "proposal" bundle) (gethash "authority_context" bundle)))
              ((equal "revision-plan" (gethash "authority_operation" bundle))
               (context-graph-plan-entity-revision
                (gethash "authority_context" bundle) (gethash "grant" bundle) (gethash "current_view" bundle)))
              ((equal "prepare" (gethash "authority_operation" bundle))
               (context-graph-prepare-authority (gethash "authority_context" bundle) (gethash "proposal" bundle)))
              ((equal "formation-authority" (gethash "authority_operation" bundle))
               (let* ((context (gethash "authority_context" bundle))
                      (preparation (context-graph-prepare-authority context (gethash "proposal" bundle))))
                 (if (not (equal "accepted" (gethash "status" preparation))) preparation
                     (let* ((prepared (gethash "proposal" (gethash "value" preparation)))
                            (review (if (eq :null (gethash "raw_review" bundle)) :null
                                        (gethash "value" (context-graph-validate-revision-review
                                                          context prepared (gethash "raw_review" bundle) (gethash "response_binding" bundle)))))
                            (batch (%cg-decide-revision-batch context prepared review (gethash "current_views" bundle))))
                       (%cg-authority-result "accepted" (%cg-object "prepared_authority" (gethash "value" preparation)
                                                                     "review_receipt" review "batch" batch))))))
              ((equal "correction-scopes" (gethash "authority_operation" bundle))
               (context-graph-build-correction-scopes
                (gethash "partition_view" bundle) (gethash "policy" bundle) (gethash "participants" bundle)))
              (t (error "Unknown authority lab operation")))))
      (lab-write-json output result)
      (format t "CONTEXT-GRAPH-AUTHORITY-OK~%")
      (uiop:quit 0)))
  (if normalization
      (multiple-value-bind (proposal repairs)
          (context-graph-normalize-legacy-participants
           (gethash "proposal" normalization)
           (gethash "descriptors" normalization))
        (lab-write-json output
                        (%cg-object "schema_version" 1
                                    "proposal" proposal
                                    "repairs" repairs))
        (format t "CONTEXT-GRAPH-NORMALIZATION-OK repairs=~d~%"
                (length repairs)))
      (let* ((graph (make-context-graph (gethash "ontology" bundle)))
             (evidence-policy (gethash "evidence_policy" bundle "all"))
             (applications nil)
             (results nil))
        (loop for formation across (gethash "formations" bundle)
              do (push (context-graph-apply-episode
                        graph (gethash "episode" formation)
                        (gethash "proposal" formation)
                        :source-packet (gethash "source_packet" formation)
                        :reuse-exact-identities-p
                        (not (equal "explicit" (gethash "identity_policy" bundle))))
                       applications))
        (loop for correction across (gethash "corrections" bundle #())
              do (context-graph-apply-correction graph correction))
        (loop for query across (gethash "queries" bundle)
              do (push (context-graph-search
                        graph query :evidence-policy evidence-policy
                        :claim-policy (gethash "claim_policy" bundle "factual"))
                       results))
        (lab-write-json
         output
         (%cg-object
          "schema_version" 1
          "source" (gethash "source" bundle)
          "applications" (coerce (nreverse applications) 'vector)
          "graph" (context-graph-snapshot graph)
          "queries" (coerce (nreverse results) 'vector)
          "compact_queries"
          (map 'vector (lambda (query)
                         (context-graph-compact-search
                          graph query
                          :claim-policy (gethash "claim_policy" bundle "factual")))
               (gethash "queries" bundle))
          "candidate_sets"
          (map 'vector (lambda (entity)
                         (context-graph-entity-candidates graph entity))
               (gethash "candidate_entities" bundle #()))))
        (format t "CONTEXT-GRAPH-LAB-OK formations=~d entities=~d facts=~d queries=~d~%"
                (length (gethash "formations" bundle))
                (context-graph-entity-count graph)
                (context-graph-fact-count graph)
                (length (gethash "queries" bundle))))))
