;;;; Complete correction sets are independent of ranked ordinary candidates.
(in-package :pai.context-graph)

(defun %cg-runtime-kind-candidates (graph kind)
  (let* ((index (context-graph-entity-scan-index graph)) (count (min 1024 (length index))) (matches nil))
    (dotimes (i count)
      (let ((entity (gethash (aref index i) (context-graph-entities graph))))
        (when (and (equal kind (gethash "kind" entity)) (eq :null (gethash "participant_role" entity)))
          (push (gethash "entity_id" entity) matches))))
    (setf matches (sort matches #'string<))
    (values (coerce (subseq matches 0 (min 16 (length matches))) 'vector)
            (and (= count (length index)) (<= (length matches) 16)) count)))

(defun %cg-runtime-verify-correction-scans (graph context)
  ;; An owner receipt is not enough: application rechecks the actual pre-state.
  (when (eql 2 (gethash "schema_version" context))
    (loop for scan across (gethash "correction_scans" context) do
      (multiple-value-bind (ids complete count) (%cg-runtime-kind-candidates graph (gethash "target_kind" scan))
        (unless (and (= count (gethash "examined_count" scan))
                     (%cg-authority-equal-p ids (gethash "candidate_entity_ids" scan))
                     (eq (if complete :true :false) (gethash "complete" scan)))
          (%cg-authority-fail "CORRECTION_SCAN_MISMATCH")))))
  t)

(defun context-graph-select-runtime-candidates (graph context query &key (target-kinds #("organism")) (ordinary-limit 48) (full-source-p nil))
  "Compose schema-2 candidates on an already-authorized owner context.
Never grants access. Complete kind scans reserve slots before ordinary relevance.
Sixteen alternatives per kind, 64 total descriptors, 1024 inspected graph rows;
overflow fails closed, never masquerades as a complete candidate universe."
  (%cg-validate-authority-context context)
  (unless (and (member full-source-p '(t nil)) (%cg-authority-string-p query (if full-source-p 70000 1000)) (%cg-authority-strings-p target-kinds 8 80)
               (integerp ordinary-limit) (<= 0 ordinary-limit 48)
               (%cg-authority-equal-p (gethash "projection_watermark" context)
                 (%cg-authority-watermark graph (gethash "agent_id" context) (gethash "persona_id" context))))
    (%cg-authority-fail "RUNTIME_CANDIDATES_INVALID"))
  (let* ((copy (%cg-detach context)) (reserved nil) (scans nil) (ordinary nil) (nonparticipants 0) (exact-count 0)
         (index (context-graph-entity-scan-index graph)) (count (min 1024 (length index)))
         (lexicon (%cg-object "entity_types" (make-hash-table :test #'equal))) (terms (%cgr-tokens query)))
    (when (and full-source-p (< count (length index))) (%cg-authority-fail "IDENTITY_SCAN_LIMIT"))
    (loop for kind in (sort (remove-duplicates (coerce target-kinds 'list) :test #'equal) #'string<) do
      (unless (%cg-type-declared-p (context-graph-ontology graph) kind) (%cg-authority-fail "ENTITY_KIND_INVALID"))
      (multiple-value-bind (ids complete examined) (%cg-runtime-kind-candidates graph kind)
        (loop for id across ids do (pushnew id reserved :test #'equal))
        (push (%cg-object "target_kind" kind "complete" (if complete :true :false)
                          "examined_count" examined "candidate_entity_ids" ids) scans)))
    (when (> (length reserved) 64) (%cg-authority-fail "CORRECTION_CONTEXT_LIMIT"))
    (dotimes (i count)
      (let* ((id (aref index i)) (entity (gethash id (context-graph-entities graph))))
        (when (eq :null (gethash "participant_role" entity)) (incf nonparticipants))
        (when (and (eq :null (gethash "participant_role" entity)) (not (member id reserved :test #'equal)))
          (let* ((exact (and full-source-p
                             (some (lambda (name) (%cgr-term-in-range-p name query 0 (length query)))
                                   (cons (gethash "label" entity) (coerce (gethash "aliases" entity) 'list)))))
                 (score (%cgr-score terms (%cgr-entity-fields entity lexicon))))
            (when exact (incf exact-count))
            (when (or exact (plusp score))
              (push (%cg-object "id" id "score" score "exact" (if exact :true :false)) ordinary))))))
    ;; Matching names nominate alternatives; never select one by label alone.
    (when (and full-source-p (> exact-count (min ordinary-limit (- 64 (length reserved)))))
      (%cg-authority-fail "IDENTITY_CANDIDATE_LIMIT"))
    (setf ordinary (sort ordinary
                        (lambda (a b)
                          (if (and full-source-p (not (eq (gethash "exact" a) (gethash "exact" b))))
                              (eq :true (gethash "exact" a)) (%cgr-before-p a b)))))
    (let ((selected (append reserved (mapcar (lambda (row) (gethash "id" row))
                        (subseq ordinary 0 (min ordinary-limit (- 64 (length reserved)) (length ordinary)))))))
      (setf (gethash "schema_version" copy) 2
            (gethash "correction_scans" copy) (coerce (nreverse scans) 'vector)
            ;; Source scopes below are rebuilt; stale scope sets are not reused.
            (gethash "correction_scopes" copy) #()
            (gethash "eligible_entities" copy)
            (coerce (mapcar (lambda (id) (%cg-detach (gethash id (context-graph-entities graph))))
                           (sort selected #'string<)) 'vector)
            (gethash "candidate_scan" copy)
            (%cg-object "complete" (if (and (= count (length index)) (= (length selected) nonparticipants)) :true :false)
                        "examined_count" count))
      (context-graph-enable-source-reference-corrections copy target-kinds))))
