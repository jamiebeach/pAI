;;;; harness: bare
(require :asdf)
(unless (find-package :ql) (load (or (uiop:getenv "PAI_QUICKLISP_SETUP") "/opt/quicklisp/setup.lisp")))
(asdf:load-asd (merge-pathnames "../pai-context-graph.asd" *load-truename*))
(asdf:load-system :pai-context-graph)
(in-package :pai.context-graph)
(load (merge-pathnames "fixtures/context-graph-authority-fixtures.lisp" *load-truename*))
(load (merge-pathnames "fixtures/context-graph-authority-lab-driver.lisp" *load-truename*))
(defvar *authority-projection-checks* 0)
(defun ap-check (name value)
  (unless value (error "FAIL ~a" name))
  (incf *authority-projection-checks*) (format t "PASS ~a~%" name))
(defun ap-error-p (thunk)
  (handler-case (progn (funcall thunk) nil) (context-graph-authority-input-error () t)))
(defun ap-initial-fixture ()
  (multiple-value-bind (partition policy participants) (as-fixture)
    (declare (ignore policy participants))
    (multiple-value-bind (context proposal view) (at-fixture)
      (declare (ignore view))
      (let* ((graph (make-context-graph (gethash "ontology" partition)))
             (entity (aref (gethash "entities" proposal) 0))
             (source (aref (gethash "sources" (gethash "source_packet" context)) 0)))
        (setf (gethash "projection_watermark" context) (%cg-authority-watermark graph "agent:one" "persona:one")
              (gethash "eligible_entities" context) #() (gethash "correction_scopes" context) #()
              (gethash "candidate_scan" context) (%cg-object "complete" :true "examined_count" 0)
              (gethash "entity_revisions" proposal) #()
              (gethash "identity_action" entity) "NEW" (gethash "existing_node_id" entity) :null
              (gethash "label" entity) "Mina"
              (gethash "text" source) "The operator owns a cat named Mina."
              (gethash "text_sha256" source) (%cg-sha256 (gethash "text" source))
              (gethash "relationships" proposal)
              (vector (%cg-object "subject_ref" "runtime:operator" "predicate" "owns" "object_ref" "corrected"
                                  "relationship_action" "ASSERT" "fact" "The operator owns a cat named Mina."
                                  "grounding" (%cg-object "schema_version" 2 "scope" "assertion" "polarity" "positive"
                                                          "attributed_to_ref" "runtime:operator" "evidence"
                                                          (vector (%cg-object "source_id" "source:correction" "quote" (gethash "text" source))))
                                  "temporal" (%cg-object "schema_version" 1 "character" "standing-disposition"
                                                         "occurred_at" :null "valid_from" :null "valid_until" :null)
                                  "evidence_status" "direct" "evidence_note" "synthetic utterance judgment")))
        (values graph context proposal (%cg-object "episode_id" (gethash "episode_id" context)
                                                  "application_event_id" 11 "opened_boundary_id" 10 "observed_at" 200))))))
(multiple-value-bind (graph context proposal boundary) (ap-initial-fixture)
  (let ((before (%cg-authority-projection-digest graph "agent:one" "persona:one")))
    ;; A valid entity kind but invalid owns endpoint fails after staged creation.
    (setf (gethash "kind" (aref (gethash "entities" proposal) 0)) "person")
    (ap-check "late claim signature failure rejects the application"
              (ap-error-p (lambda () (%cg-apply-authority-episode graph boundary proposal context :null))))
    (ap-check "late failure publishes neither participant nodes nor watermark"
              (and (equal before (%cg-authority-projection-digest graph "agent:one" "persona:one"))
                   (zerop (context-graph-through-event-id graph)) (null (context-graph-authority-profile graph))))))
(multiple-value-bind (graph context proposal boundary) (ap-initial-fixture)
  (let ((result (%cg-apply-authority-episode graph boundary proposal context :null)))
    (ap-check "ordinary authority episode applies" (equal "admitted" (gethash "formation_outcome" result)))
    (ap-check "runtime participants and ordinary entity installed" (= 3 (context-graph-entity-count graph)))
    (ap-check "grounded claim installed" (= 1 (context-graph-fact-count graph)))
    (ap-check "event watermark advances" (= 11 (context-graph-through-event-id graph)))
    (dolist (operation (list (lambda () (context-graph-apply-episode graph nil nil))
                            (lambda () (context-graph-apply-correction graph nil))
                            (lambda () (context-graph-search graph "cat"))
                            (lambda () (context-graph-snapshot graph))
                            (lambda () (context-graph-entity-candidates graph nil))
                            (lambda () (context-graph-reassessment-candidates graph))))
      (ap-check "legacy API cannot mix or misread a new-profile projection"
                (handler-case (progn (funcall operation) nil) (error () t))))
    (ap-check "replay checks sealed input before stale watermark" (equal "already-applied" (gethash "status" (%cg-apply-authority-episode graph boundary proposal context :null))))
    (let* ((repeat-graph (%cg-stage-authority-state graph)) (repeat-context (%cg-detach context))
           (repeat-proposal (%cg-detach proposal)) (repeat-boundary (%cg-detach boundary))
           (target (find "organism" (loop for value being the hash-values of (context-graph-entities graph) collect value)
                         :test #'equal :key (lambda (row) (gethash "kind" row))))
           (descriptor (aref (gethash "entities" repeat-proposal) 0)))
      (setf (gethash "episode_id" repeat-context) "episode:reinforcement"
            (gethash "projection_watermark" repeat-context) (%cg-authority-watermark graph "agent:one" "persona:one")
            (gethash "episode_id" repeat-boundary) "episode:reinforcement"
            (gethash "application_event_id" repeat-boundary) 13 (gethash "opened_boundary_id" repeat-boundary) 12)
      (%cg-apply-authority-episode repeat-graph repeat-boundary repeat-proposal repeat-context :null)
      (ap-check "NEW with same label never implicitly reuses identity"
                (and (= 4 (context-graph-entity-count repeat-graph)) (= 2 (context-graph-fact-count repeat-graph))))
      (ap-check "bounded retrieval exposes an incomplete scan"
                (eq :false (gethash "scan_complete" (%cg-authority-search repeat-graph "agent:one" "persona:one" "cat" :scan-limit 1))))
      (setf repeat-graph (%cg-stage-authority-state graph)
            (gethash "eligible_entities" repeat-context) (vector (%cg-detach target))
            (gethash "candidate_scan" repeat-context) (%cg-object "complete" :true "examined_count" 1)
            (gethash "identity_action" descriptor) "LINK_EXISTING"
            (gethash "existing_node_id" descriptor) (gethash "node_id" target)
            (gethash "label" descriptor) "Untrusted different name")
      (%cg-apply-authority-episode repeat-graph repeat-boundary repeat-proposal repeat-context :null)
      (ap-check "explicit link reinforces the same shared claim identity"
                (and (= 3 (context-graph-entity-count repeat-graph)) (= 1 (context-graph-fact-count repeat-graph))))
      (ap-check "link cannot mutate current label or aliases"
                (%cg-authority-equal-p target (%cg-authority-current-descriptor repeat-graph (gethash "entity_id" target))))
      (ap-check "reinforcement accumulates complete source evidence"
                (= 2 (length (gethash "evidence_records" (gethash (aref (context-graph-fact-scan-index repeat-graph) 0) (context-graph-facts repeat-graph)))))))
    (let ((before (%cg-authority-projection-digest graph "agent:one" "persona:one")))
      (setf (gethash "observed_at" boundary) 201)
      (ap-check "changed replay rejected" (ap-error-p (lambda () (%cg-apply-authority-episode graph boundary proposal context :null))))
      (setf (gethash "application_event_id" boundary) 13 (gethash "opened_boundary_id" boundary) 12)
      (ap-check "stale context rejected" (ap-error-p (lambda () (%cg-apply-authority-episode graph boundary proposal context :null))))
      (ap-check "failed applications leave projection unchanged" (equal before (%cg-authority-projection-digest graph "agent:one" "persona:one"))))))
(multiple-value-bind (graph context proposal boundary) (ap-initial-fixture)
  (%cg-apply-authority-episode graph boundary proposal context :null)
  (multiple-value-bind (correction raw unused) (at-fixture 1)
    (declare (ignore unused))
    (let* ((partition (%cg-authority-partition-view graph "agent:one" "persona:one" "entity:operator"))
           (scoped (gethash "value" (context-graph-build-correction-scopes partition (gethash "correction_policy" correction) (gethash "participants" correction))))
           (target (aref (gethash "eligible_entities" scoped) 0))
           (scope (aref (gethash "scopes" scoped) 0))
           (entity (aref (gethash "entities" raw) 0))
           (revision (aref (gethash "entity_revisions" raw) 0))
           (old-node (gethash "node_id" target))
           (facts-before (%cg-authority-canonical-json (context-graph-facts graph))))
      (setf (gethash "episode_id" correction) "episode:two"
            (gethash "projection_watermark" correction) (gethash "projection_watermark" partition)
            (gethash "eligible_entities" correction) (gethash "eligible_entities" scoped)
            (gethash "correction_scopes" correction) (gethash "scopes" scoped)
            (gethash "existing_node_id" entity) old-node
            (gethash "target_node_id" revision) old-node
            (gethash "target_entity_id" revision) (gethash "entity_id" target)
            (gethash "expected_revision_digest" revision) (gethash "revision_digest" target)
            (gethash "target_scope_id" revision) (gethash "scope_id" scope))
      (let* ((prepared (gethash "proposal" (gethash "value" (context-graph-prepare-authority correction raw))))
             (review (at-review-receipt correction prepared (at-raw-review correction prepared)))
             (second-boundary (%cg-object "episode_id" "episode:two" "application_event_id" 13 "opened_boundary_id" 12 "observed_at" 201))
             (missing-review-graph (%cg-stage-authority-state graph))
             (missing-review-result (%cg-apply-authority-episode missing-review-graph second-boundary raw correction :null))
             (result (%cg-apply-authority-episode graph second-boundary raw correction review))
             (current (%cg-authority-current-descriptor graph (gethash "entity_id" target))))
        (ap-check "missing semantic review defers with no active delta"
                  (and (equal "deferred" (gethash "formation_outcome" missing-review-result))
                       (zerop (length (gethash "revision_deltas" (gethash "batch" missing-review-result))))))
        (ap-check "deferred application retains audit but changes no knowledge"
                  (and (= 2 (hash-table-count (context-graph-application-receipts missing-review-graph)))
                       (= 3 (hash-table-count (context-graph-entity-versions missing-review-graph)))
                       (equal "Mina" (gethash "label" (%cg-authority-current-descriptor missing-review-graph (gethash "entity_id" target))))
                       (equal facts-before (%cg-authority-canonical-json (context-graph-facts missing-review-graph)))))
        (ap-check "correction through recomputed shared admission applies" (equal "admitted" (gethash "formation_outcome" result)))
        (ap-check "current descriptor has replacement label" (equal "Mira" (gethash "label" current)))
        (ap-check "old version retained without becoming current" (and (not (equal old-node (gethash "node_id" current)))
                                                                     (equal "superseded" (gethash "status" (%cg-authority-entity graph old-node)))))
        (ap-check "correction never rewrites historical claim evidence" (equal facts-before (%cg-authority-canonical-json (context-graph-facts graph))))
        (ap-check "one identity with two versions" (and (= 3 (context-graph-entity-count graph)) (= 4 (hash-table-count (context-graph-entity-versions graph)))))
        (let* ((before (%cg-authority-projection-digest graph "agent:one" "persona:one"))
               (named (%cg-authority-search graph "agent:one" "persona:one" "Mira"))
               (broad (%cg-authority-search graph "agent:one" "persona:one" "cat"))
               (rows (gethash "rows" named)))
          (ap-check "held-out corrected name retrieves the enduring target"
                    (and (= 1 (length rows)) (equal (gethash "entity_id" target) (gethash "entity_id" (gethash "object" (aref rows 0))))))
          (ap-check "held-out category query finds same fact"
                    (equal (gethash "fact_id" (aref rows 0)) (gethash "fact_id" (aref (gethash "rows" broad) 0))))
          (ap-check "old primary name in historical evidence is not current support"
                    (zerop (length (gethash "rows" (%cg-authority-search graph "agent:one" "persona:one" "Mina")))))
          (ap-check "unrelated query has no accidental support"
                    (zerop (length (gethash "rows" (%cg-authority-search graph "agent:one" "persona:one" "tea")))))
          (ap-check "foreign partition is denied"
                    (ap-error-p (lambda () (%cg-authority-search graph "agent:one" "persona:other" "Mira"))))
          (setf (gethash "label" (gethash "object" (aref rows 0))) "Changed by caller")
          (ap-check "retrieval is detached and performs zero projection writes"
                    (equal before (%cg-authority-projection-digest graph "agent:one" "persona:one"))))
        (ap-check "correction replay does not add a third version" (and (equal "already-applied" (gethash "status" (%cg-apply-authority-episode graph second-boundary raw correction review)))
                                                                      (= 4 (hash-table-count (context-graph-entity-versions graph)))))
        (let* ((queries #("Mira" "cat" "Mina" "tea"))
               (wire (gethash "value"
                       (al-run (%cg-object "schema_version" 2 "authority_operation" "projection-replay"
                                           "ontology" (context-graph-ontology graph) "agent_id" "agent:one" "persona_id" "persona:one"
                                           "formations" (vector
                                                         (%cg-object "boundary" boundary "authority_context" context "proposal" proposal "raw_review" :null "response_binding" :null)
                                                         (%cg-object "boundary" second-boundary "authority_context" correction "proposal" raw
                                                                     "raw_review" (at-raw-review correction prepared)
                                                                     "response_binding" (%cg-object "opened_boundary_id" 12 "request_digest" (gethash "request_digest" review)
                                                                                                    "response_digest" (gethash "response_digest" review))))
                                           "queries" queries)))))
          (ap-check "actual lab replay installs one correction version" (= 4 (gethash "version_count" wire)))
          (ap-check "actual lab replay has identical projection digest"
                    (%cg-authority-equal-p (gethash "watermark" wire) (%cg-authority-watermark graph "agent:one" "persona:one")))
          (ap-check "actual lab held-out retrieval equals direct production Lisp"
                    (%cg-authority-equal-p (gethash "queries" wire)
                                          (map 'vector (lambda (query) (%cg-authority-search graph "agent:one" "persona:one" query)) queries))))))))
(format t "AUTHORITY-PROJECTION ~d passed, 0 failed~%" *authority-projection-checks*)
