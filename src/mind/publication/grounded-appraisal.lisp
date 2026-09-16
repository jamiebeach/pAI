;;;; grounded-appraisal.lisp -- Slice D deterministic bounded appraisal.
;;;;
;;;; Appraisals describe current architecture state. They are derived from
;;;; durable authority and bounded modulators, never from model output.

(in-package :agent)

(export '(agent-appraisal-compute agent-appraisal-current
          grounded-appraisal-report))

(defparameter *grounded-appraisal-version* "story-appraisal-v1")
(defparameter *grounded-appraisal-ttl-seconds* 7200)
(defparameter *grounded-appraisal-tie-order*
  '("uncertain" "reluctant" "disappointed" "curious" "satisfied" "excited"))
(defvar *grounded-appraisal-modulator-fn*
  (lambda (name)
    (and (fboundp 'modulator-value)
         (ignore-errors (funcall 'modulator-value name))))
  "Injected bounded modulator resolver; NIL means unavailable.")
(defvar *grounded-appraisal-stats* (make-hash-table :test #'equal))
(defvar *grounded-appraisal-stats-lock* (bt:make-lock "grounded-appraisal-stats"))

(defun %appraisal-clamp (value)
  (min 1.0d0 (max 0.0d0 (float value 1.0d0))))

(defun %appraisal-stat (key)
  (bt:with-lock-held (*grounded-appraisal-stats-lock*)
    (incf (gethash key *grounded-appraisal-stats* 0))))

(defun %appraisal-modulator (name minimum maximum fallback)
  (let ((value (funcall *grounded-appraisal-modulator-fn* name)))
    (if (and (numberp value) (<= minimum value maximum))
        (values (float value 1.0d0) t)
        (values fallback nil))))

(defun %appraisal-scores (c p n v u a l b o)
  (let ((scores (obj)))
    (setf
     (gethash "excited" scores)
     (%appraisal-clamp (+ (* 0.25d0 c) (* 0.15d0 p) (* 0.20d0 n)
                          (* 0.15d0 v) (* 0.15d0 a) (* 0.10d0 l)
                          (* -0.20d0 u) (* -0.35d0 b) (* -0.35d0 o)))
     (gethash "satisfied" scores)
     (%appraisal-clamp (+ (* 0.30d0 c) (* 0.20d0 p) (* 0.10d0 n)
                          (* 0.25d0 v) (* 0.15d0 l)
                          (* -0.15d0 u) (* -0.40d0 b) (* -0.40d0 o)))
     (gethash "curious" scores)
     (%appraisal-clamp (+ (* 0.25d0 (- 1.0d0 c)) (* 0.20d0 p)
                          (* 0.20d0 n) (* 0.20d0 u) (* 0.15d0 a)
                          (* -0.25d0 b) (* -0.20d0 o)))
     (gethash "uncertain" scores)
     (%appraisal-clamp (+ (* 0.55d0 u) (* 0.20d0 (- 1.0d0 v))
                          (* 0.15d0 (- 1.0d0 p)) (* 0.10d0 b)))
     (gethash "reluctant" scores)
     (%appraisal-clamp (+ (* 0.35d0 b) (* 0.25d0 o)
                          (* 0.20d0 (- 1.0d0 l)) (* 0.20d0 u)))
     (gethash "disappointed" scores)
     (%appraisal-clamp (+ (* 0.35d0 b) (* 0.25d0 o)
                          (* 0.25d0 (- 1.0d0 v)) (* 0.15d0 (- 1.0d0 l)))))
    scores))

(defun %appraisal-winner (scores)
  (let ((winner (first *grounded-appraisal-tie-order*))
        (winning-score -1.0d0))
    (dolist (label *grounded-appraisal-tie-order*)
      (let ((score (gethash label scores)))
        (when (> score winning-score)
          (setf winner label winning-score score))))
    (values winner winning-score)))

(defun %appraisal-budget-exhausted-p (process usage)
  (let ((budget (gethash "budget" process)))
    (or (>= (gethash "attempt_count" usage)
            (%fpe-budget-value budget "max_operations"))
        (>= (gethash "model_operation_count" usage)
            (%fpe-budget-value budget "max_model_operations"))
        (>= (gethash "prompt_tokens_used" usage)
            (%fpe-budget-value budget "max_prompt_tokens"))
        (>= (gethash "completion_tokens_used" usage)
            (%fpe-budget-value budget "max_completion_tokens"))
        (>= (gethash "cost_used" usage)
            (%fpe-budget-value budget "max_cost")))))

(defun %appraisal-input-current (process-id)
  (let* ((process (%fpe-process-current process-id :for-update t))
         (project (and process (%fpe-project-current process-id :for-update t))))
    (unless (and process project)
      (%fpe-reject "missing-project" "process/project ~a does not exist" process-id))
    (let* ((artifact-id (gethash "current_artifact_id" project))
           (artifact (and (stringp artifact-id)
                          (%fpe-artifact-current artifact-id :for-update t)))
           (artifact-version (and artifact (gethash "current_version" artifact)))
           (validation-row
             (and artifact
                  (pomo:query
                   "SELECT id,facts::text FROM agent_attestations WHERE process_id=$1 AND artifact_id=$2 AND artifact_version=$3 AND attestation_type='artifact-validated' AND superseded_at IS NULL ORDER BY issued_at DESC LIMIT 1"
                   process-id artifact-id artifact-version :row)))
           (validation-facts
             (and validation-row (%fpe-json-read (second validation-row) (obj))))
           (meaningful
             (pomo:query
              "SELECT count(*) FROM agent_process_operations WHERE process_id=$1 AND status='completed' AND meaningful_progress=true"
              process-id :single))
           (usage (%fpe-operation-usage-current process-id))
           (complete (if (and artifact
                              (string= (gethash "state" process) "completed")
                              (string= (gethash "status" artifact) "completed"))
                         1.0d0 0.0d0))
           (progress (min 1.0d0 (/ meaningful 3.0d0)))
           (maximum-similarity
             (let ((value (and validation-facts
                               (gethash "maximum_prior_version_similarity"
                                        validation-facts))))
               (if (numberp value) (%appraisal-clamp value) 0.0d0)))
           (novelty (%appraisal-clamp (/ (- 1.0d0 maximum-similarity) 0.50d0)))
           (validated (if validation-row 1.0d0 0.0d0))
           (uncertainty (%appraisal-clamp (gethash "uncertainty" project)))
           (blocked (if (string= (gethash "state" process) "blocked") 1.0d0 0.0d0))
           (over-budget (if (%appraisal-budget-exhausted-p process usage) 1.0d0 0.0d0)))
      (values process project artifact validation-row
              complete progress novelty validated uncertainty blocked over-budget
              maximum-similarity meaningful usage))))

(defun %appraisal-attestation-object (row)
  (%fpe-attestation-row-object row))

(defparameter *appraisal-attestation-select*
  "SELECT id,process_id,operation_id,attestation_type,epistemic_status,issued_by,issued_at::text,valid_from::text,valid_until::text,subject,facts::text,source_event_ids::text,evidence_node_ids::text,artifact_id,artifact_version,derivation_version,supersedes_attestation_id,superseded_at::text FROM agent_attestations")

(defun agent-appraisal-compute (process-id &key (now (get-universal-time)))
  (%fpe-require-write-mode)
  (unless (and (integerp now) (plusp now))
    (%fpe-reject "invalid-time" "appraisal NOW must be a positive universal time"))
  (multiple-value-bind (arousal arousal-present)
      (%appraisal-modulator "arousal" 0.0d0 1.0d0 0.5d0)
    (multiple-value-bind (valence valence-present)
        (%appraisal-modulator "valence" -1.0d0 1.0d0 0.0d0)
      (let* ((l (%appraisal-clamp (/ (+ valence 1.0d0) 2.0d0)))
             (valid-until (+ now *grounded-appraisal-ttl-seconds*))
             (attestation-id (%fpe-id "att"))
             (superseded-ids nil)
             (result nil))
        (%fpe-run-serializable
         (lambda ()
           (with-pg
             (%fpe-with-transaction (:serializable)
               (multiple-value-bind
                     (process project artifact validation-row c p n v u b o
                              maximum-similarity meaningful usage)
                   (%appraisal-input-current process-id)
                 (declare (ignore validation-row))
                 (let* ((scores (%appraisal-scores c p n v u arousal l b o))
                        (confidence
                          (%appraisal-clamp
                           (+ 0.45d0 (* 0.10d0 c) (* 0.10d0 v) (* 0.10d0 p)
                              (* 0.10d0 (- 1.0d0 u))
                              (if valence-present 0.075d0 0.0d0)
                              (if arousal-present 0.075d0 0.0d0))))
                        (inputs
                          (obj "C" c "P" p "N" n "V" v "U" u "A" arousal
                               "L" l "B" b "O" o
                               "arousal_present" (if arousal-present t nil)
                               "valence_present" (if valence-present t nil)
                               "maximum_prior_version_similarity" maximum-similarity
                               "meaningful_operation_count" meaningful
                               "operation_usage" usage
                               "process_state" (gethash "state" process)
                               "artifact_status" (if artifact
                                                     (gethash "status" artifact) :null)))
                        (label nil) (intensity nil))
                   (multiple-value-setq (label intensity) (%appraisal-winner scores))
                   (setf superseded-ids
                         (pomo:query
                          "UPDATE agent_attestations SET superseded_at=to_timestamp($2-2208988800) WHERE process_id=$1 AND attestation_type='subjective-appraisal' AND derivation_version=$3 AND superseded_at IS NULL RETURNING id"
                          process-id now *grounded-appraisal-version* :column))
                   (pomo:execute
                    "INSERT INTO agent_attestations(id,process_id,attestation_type,epistemic_status,issued_by,issued_at,valid_from,valid_until,subject,facts,source_event_ids,evidence_node_ids,artifact_id,artifact_version,derivation_version) VALUES($1,$2,'subjective-appraisal','derived-current-state','grounded-appraisal',to_timestamp($3-2208988800),to_timestamp($3-2208988800),to_timestamp($4-2208988800),'the agent',$5::jsonb,'[]'::jsonb,'[]'::jsonb,$6,$7,$8)"
                    attestation-id process-id now valid-until
                    (%fpe-json
                     (obj "label" label "intensity" intensity
                          "confidence" confidence "scores" scores
                          "input_snapshot" inputs
                          "derivation_version" *grounded-appraisal-version*
                          "valid_from_universal" now
                          "valid_until_universal" valid-until
                          "project_id" (gethash "id" project)))
                    (%fpe-sql-null (and artifact (gethash "id" artifact)))
                    (%fpe-sql-null (and artifact (gethash "current_version" artifact)))
                    *grounded-appraisal-version*)
                   (setf result
                         (%appraisal-attestation-object
                          (pomo:query
                           (format nil "~a WHERE id=$1" *appraisal-attestation-select*)
                           attestation-id :row)))))))))
        (%appraisal-stat "computed")
        (%fpe-log
         "agent-appraisal-derived"
         (let ((facts (gethash "facts" result)))
           (obj "schema_version" 1 "attestation_id" attestation-id
                "process_id" process-id "label" (gethash "label" facts)
                "intensity" (gethash "intensity" facts)
                "confidence" (gethash "confidence" facts)
                "derivation_version" *grounded-appraisal-version*
                "valid_until" (gethash "valid_until" result))))
        (dolist (superseded-id superseded-ids)
          (%fpe-emit-attestation-state superseded-id))
        (%fpe-emit-attestation-state attestation-id)
        result))))

(defun agent-appraisal-current (process-id &key (now (get-universal-time)))
  (with-pg
    (%appraisal-attestation-object
     (pomo:query
      (format nil "~a WHERE process_id=$1 AND attestation_type='subjective-appraisal' AND derivation_version=$2 AND superseded_at IS NULL AND valid_from<=to_timestamp($3-2208988800) AND valid_until>to_timestamp($3-2208988800) ORDER BY issued_at DESC LIMIT 1" *appraisal-attestation-select*)
      process-id *grounded-appraisal-version* now :row))))

(defun grounded-appraisal-report ()
  (let ((stats (obj)))
    (bt:with-lock-held (*grounded-appraisal-stats-lock*)
      (maphash (lambda (key value) (setf (gethash key stats) value))
               *grounded-appraisal-stats*))
    (obj "schema_version" 1 "derivation_version" *grounded-appraisal-version*
         "ttl_seconds" *grounded-appraisal-ttl-seconds* "stats" stats)))
