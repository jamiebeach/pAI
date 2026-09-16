;;;; lifecycle.lisp -- bounded correction and reassessment for the standalone lab.

(in-package :pai.context-graph)

(defun %cg-install-staged-state (target source)
  (setf (context-graph-entities target) (context-graph-entities source)
        (context-graph-entity-index target) (context-graph-entity-index source)
        (context-graph-facts target) (context-graph-facts source)
        (context-graph-current-triples target) (context-graph-current-triples source)
        (context-graph-episodes target) (context-graph-episodes source)
        (context-graph-corrections target) (context-graph-corrections source)
        (context-graph-authority-profile target) (context-graph-authority-profile source)
        (context-graph-authority-partition target) (context-graph-authority-partition source)
        (context-graph-entity-versions target) (context-graph-entity-versions source)
        (context-graph-current-entity-versions target) (context-graph-current-entity-versions source)
        (context-graph-revision-lineage target) (context-graph-revision-lineage source)
        (context-graph-application-receipts target) (context-graph-application-receipts source)
        (context-graph-entity-scan-index target) (context-graph-entity-scan-index source)
        (context-graph-fact-scan-index target) (context-graph-fact-scan-index source)
        (context-graph-entity-adjacency target) (context-graph-entity-adjacency source)
        (context-graph-through-event-id target) (context-graph-through-event-id source)
        (context-graph-projection-digest target) (context-graph-projection-digest source)))

(defun %cg-install-revision-delta (staged delta)
  "Mechanical installer, called only after recomputing an admitted batch.
Descriptor rows use EligibleEntity fields. The complete delta retains observation
and correction evidence separately in the append-only revision-lineage index."
  (unless (and (context-graph-p staged)
               (%cg-closed-keys-p delta '("operation_id" "entity_id" "expected_node_id" "expected_revision_digest"
                                          "close_version" "create_version" "lineage" "current_version" "evidence")))
    (%cg-authority-fail "REVISION_DELTA_INVALID"))
  (let* ((entity-id (gethash "entity_id" delta)) (old-id (gethash "expected_node_id" delta))
         (old (gethash old-id (context-graph-entity-versions staged)))
         (new (gethash "create_version" delta)) (new-id (and (hash-table-p new) (gethash "node_id" new)))
         (close (gethash "close_version" delta)) (lineage (gethash "lineage" delta))
         (index (gethash "current_version" delta)) (operation-id (gethash "operation_id" delta)))
    (unless (and (%cg-authority-digest-p operation-id)
                 (%cg-eligible-entity-p old)
                 (equal entity-id (gethash "entity_id" old))
                 (equal old-id (gethash entity-id (context-graph-current-entity-versions staged)))
                 (%cg-authority-equal-p old (gethash entity-id (context-graph-entities staged)))
                 (equal (gethash "expected_revision_digest" delta) (gethash "revision_digest" old))
                 (%cg-closed-keys-p close '("node_id" "status" "observed_at"))
                 (equal old-id (gethash "node_id" close)) (equal "superseded" (gethash "status" close))
                 (%cg-closed-keys-p new '("node_id" "entity_id" "kind" "label" "aliases" "classifications" "participant_role"
                                         "status" "supersedes_node_id" "correction_grant_id" "observed_at"))
                 (%cg-authority-string-p new-id 180) (not (equal old-id new-id))
                 (not (gethash new-id (context-graph-entity-versions staged)))
                 (not (gethash operation-id (context-graph-revision-lineage staged)))
                 (equal entity-id (gethash "entity_id" new)) (equal old-id (gethash "supersedes_node_id" new))
                 (equal "current" (gethash "status" new))
                 (%cg-closed-keys-p lineage '("new_node_id" "old_node_id" "relation" "correction_grant_id"))
                 (equal new-id (gethash "new_node_id" lineage)) (equal old-id (gethash "old_node_id" lineage))
                 (equal "supersedes" (gethash "relation" lineage))
                 (equal (gethash "correction_grant_id" new) (gethash "correction_grant_id" lineage))
                 (%cg-closed-keys-p index '("entity_id" "node_id"))
                 (equal entity-id (gethash "entity_id" index)) (equal new-id (gethash "node_id" index)))
      (%cg-authority-fail "REVISION_DELTA_INVALID"))
    (let ((replacement (%cg-detach old)) (closed (%cg-detach old)))
      (dolist (key '("node_id" "entity_id" "kind" "label" "aliases" "classifications" "participant_role" "status"))
        (setf (gethash key replacement) (%cg-detach (gethash key new))))
      (setf (gethash "revision_digest" replacement) (%cg-revision-descriptor-digest replacement)
            (gethash "status" closed) "superseded"
            (gethash "revision_digest" closed) (%cg-revision-descriptor-digest closed))
      (unless (%cg-eligible-entity-p replacement) (%cg-authority-fail "REVISION_DELTA_INVALID"))
      ;; Caller stages the full formation. No fact endpoint or evidence changes.
      (setf (gethash old-id (context-graph-entity-versions staged)) closed
            (gethash new-id (context-graph-entity-versions staged)) replacement
            (gethash entity-id (context-graph-entities staged)) (%cg-detach replacement)
            (gethash entity-id (context-graph-current-entity-versions staged)) (copy-seq new-id)
            (gethash operation-id (context-graph-revision-lineage staged)) (%cg-detach delta)))
    staged))

(defun %cg-stage-authority-state (graph)
  "Detach every mutable projection index before any proposed transition."
  (let ((staged (copy-context-graph graph)))
    (setf (context-graph-ontology staged) (%cg-detach (context-graph-ontology graph))
          (context-graph-entities staged) (%cg-detach (context-graph-entities graph))
          (context-graph-entity-index staged) (%cg-detach (context-graph-entity-index graph))
          (context-graph-facts staged) (%cg-detach (context-graph-facts graph))
          (context-graph-current-triples staged) (%cg-detach (context-graph-current-triples graph))
          (context-graph-episodes staged) (%cg-detach (context-graph-episodes graph))
          (context-graph-corrections staged) (%cg-detach (context-graph-corrections graph))
          (context-graph-authority-partition staged) (%cg-detach (context-graph-authority-partition graph))
          (context-graph-entity-versions staged) (%cg-detach (context-graph-entity-versions graph))
          (context-graph-current-entity-versions staged) (%cg-detach (context-graph-current-entity-versions graph))
          (context-graph-revision-lineage staged) (%cg-detach (context-graph-revision-lineage graph))
          (context-graph-application-receipts staged) (%cg-detach (context-graph-application-receipts graph))
          (context-graph-entity-scan-index staged) (%cg-detach (context-graph-entity-scan-index graph))
          (context-graph-fact-scan-index staged) (%cg-detach (context-graph-fact-scan-index graph))
          (context-graph-entity-adjacency staged) (%cg-detach (context-graph-entity-adjacency graph))
          (context-graph-projection-digest staged) (%cg-detach (context-graph-projection-digest graph)))
    staged))

(defun context-graph-apply-correction (graph correction)
  "Apply one explicit correction without deleting its historical target.

A replacement fact must already exist through the grounded episode path. The
correction closes the mistaken fact and records why; it never invents knowledge."
  (%cg-require-legacy-profile graph)
  (let ((keys '("schema_version" "correction_id" "observed_at" "action"
                "target_fact_id" "replacement_fact_id" "reason"
                "source_episode_id")))
    (unless (and (context-graph-p graph) (%cg-closed-keys-p correction keys)
                 (eql 1 (gethash "schema_version" correction))
                 (%cg-present-string-p (gethash "correction_id" correction) 180)
                 (%cg-present-string-p (gethash "observed_at" correction) 80)
                 (member (gethash "action" correction)
                         '("withdraw" "supersede") :test #'string=)
                 (%cg-present-string-p (gethash "target_fact_id" correction) 180)
                 (%cg-present-string-p (gethash "reason" correction) 1000)
                 (%cg-present-string-p (gethash "source_episode_id" correction) 180))
      (error "Context graph correction is invalid")))
  (let* ((id (gethash "correction_id" correction))
         (digest (%cg-sha256
                  (shasht:write-json (%cg-canonical-tree correction) nil)))
         (prior (gethash id (context-graph-corrections graph))))
    (when prior
      (unless (string= digest (gethash "input_sha256" prior))
        (error "Context graph correction ID conflicts with prior input"))
      (return-from context-graph-apply-correction
        (%cg-object "status" "already-applied" "correction_id" id)))
    (let* ((target-id (gethash "target_fact_id" correction))
           (target (gethash target-id (context-graph-facts graph)))
           (replacement-id (gethash "replacement_fact_id" correction))
           (action (gethash "action" correction))
           (source-episode-id (gethash "source_episode_id" correction))
           (source-episode (gethash source-episode-id
                                    (context-graph-episodes graph)))
           (staged (copy-context-graph graph)))
      (unless (and target (gethash "current" target))
        (error "Context graph correction target is absent or not current"))
      (unless (and source-episode
                   (gethash "grounded_input_sha256" source-episode))
        (error "Context graph correction source episode is absent or ungrounded"))
      (cond
        ((string= action "withdraw")
         (unless (%cg-null-p replacement-id)
           (error "Withdraw correction may not name a replacement")))
        ((unless (and (%cg-present-string-p replacement-id 180)
                      (not (string= target-id replacement-id))
                      (gethash replacement-id (context-graph-facts graph))
                      (gethash "current"
                               (gethash replacement-id
                                        (context-graph-facts graph)))
                      (gethash "grounding"
                               (gethash replacement-id
                                        (context-graph-facts graph))))
           (error "Supersede correction requires an existing grounded current replacement"))))
      (setf (context-graph-entities staged)
            (%cg-detach (context-graph-entities graph))
            (context-graph-entity-index staged)
            (%cg-detach (context-graph-entity-index graph))
            (context-graph-facts staged)
            (%cg-detach (context-graph-facts graph))
            (context-graph-current-triples staged)
            (%cg-detach (context-graph-current-triples graph))
            (context-graph-episodes staged)
            (%cg-detach (context-graph-episodes graph))
            (context-graph-corrections staged)
            (%cg-detach (context-graph-corrections graph)))
      (let ((copy (gethash target-id (context-graph-facts staged))))
        (setf (gethash "current" copy) nil
              (gethash "invalid_at" copy) (gethash "observed_at" correction)
              (gethash "expired_at" copy) (gethash "observed_at" correction)
              (gethash "corrected_by" copy) id
              (gethash "replacement_fact_id" copy)
              (if (string= action "supersede") replacement-id :null))
        (remhash (gethash "identity_key" copy)
                 (context-graph-current-triples staged)))
      (let ((record (%cg-detach correction)))
        (setf (gethash "input_sha256" record) digest
              (gethash id (context-graph-corrections staged)) record))
      (%cg-install-staged-state graph staged)
      (%cg-object "status" "applied" "correction_id" id
                  "target_fact_id" target-id "action" action))))

(defun %cg-entity-degrees (graph)
  (let ((degrees (make-hash-table :test #'equal)))
    (maphash (lambda (id ignored)
               (declare (ignore ignored))
               (setf (gethash id degrees) 0))
             (context-graph-entities graph))
    (maphash (lambda (ignored fact)
               (declare (ignore ignored))
               (when (gethash "current" fact)
                 (incf (gethash (gethash "subject_id" fact) degrees 0))
                 (incf (gethash (gethash "object_id" fact) degrees 0))))
             (context-graph-facts graph))
    degrees))

(defun context-graph-reassessment-candidates
    (graph &key (maximum 16) (offset 0) (high-degree-threshold 12))
  "Return structural audit candidates without changing graph authority.

OFFSET lets a quiet caller rotate through the bounded queue. Random selection
is safe at that caller boundary because any later correction is explicit."
  (%cg-require-legacy-profile graph)
  (unless (and (context-graph-p graph) (integerp maximum) (<= 1 maximum 64)
               (integerp offset) (not (minusp offset))
               (integerp high-degree-threshold) (plusp high-degree-threshold))
    (error "Invalid graph reassessment request"))
  (let ((rows nil)
        (degrees (%cg-entity-degrees graph))
        (names (make-hash-table :test #'equal)))
    (maphash (lambda (id entity)
               (push id (gethash (gethash "canonical_name" entity) names)))
             (context-graph-entities graph))
    (maphash
     (lambda (id fact)
       (when (and (gethash "current" fact)
                  (string= "related_to" (gethash "predicate" fact)))
         (push (%cg-object "target_kind" "fact" "target_id" id
                           "priority" 40 "reason" "generic-edge") rows)))
     (context-graph-facts graph))
    (maphash
     (lambda (id entity)
       (let ((degree (gethash id degrees 0))
             (same-name
               (gethash (gethash "canonical_name" entity) names)))
         (when (zerop degree)
           (push (%cg-object "target_kind" "entity" "target_id" id
                             "priority" 20 "reason" "isolated-node") rows))
         (when (>= degree high-degree-threshold)
           (push (%cg-object "target_kind" "entity" "target_id" id
                             "priority" 30 "reason" "high-degree-hub") rows))
         (when (> (length same-name) 1)
           (push (%cg-object "target_kind" "entity" "target_id" id
                             "priority" 35 "reason" "same-name-collision") rows))))
     (context-graph-entities graph))
    (setf rows
          (sort rows
                (lambda (left right)
                  (or (> (gethash "priority" left)
                         (gethash "priority" right))
                      (and (= (gethash "priority" left)
                              (gethash "priority" right))
                           (string< (gethash "target_id" left)
                                    (gethash "target_id" right)))))))
    (let* ((count (length rows))
           (start (if (zerop count) 0 (mod offset count)))
           (rotated (append (subseq rows start) (subseq rows 0 start)))
           (selected (subseq rotated 0 (min maximum count))))
      (%cg-object "schema_version" 1 "candidate_count" count
                  "non_exhaustive" (> count maximum)
                  "candidates" (coerce selected 'vector)))))
