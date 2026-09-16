;;;; stabilization-evals.lisp -- deterministic release gates.

(in-package :agent)

(export '(stabilization-evaluate-deterministic stabilization-evaluate-online
          stabilization-promotion-readiness stabilization-eval-report))

(defparameter *stabilization-eval-required-dimensions*
  '("thought_variation" "worldview_development" "reciprocity" "outreach"
    "memory" "responsiveness" "reliability" "cost"))
(defparameter *stabilization-eval-max-projection-chars* 5800)
(defparameter *stabilization-eval-min-memory-precision* 0.80d0)
(defvar *stabilization-eval-last-report* nil)

(defun %stab12-list (value)
  (cond ((null value) nil) ((listp value) value)
        ((vectorp value) (coerce value 'list)) (t (list value))))
(defun %stab12-result (name passed &optional observed required)
  (obj "gate" name "passed" (if passed t nil)
       "observed" (or observed :null) "required" (or required :null)))
(defun %stab12-all (items predicate) (every predicate (%stab12-list items)))

(defun stabilization-evaluate-deterministic (data)
  "Evaluate deterministic release evidence. Missing fields fail closed."
  (let* ((projection (%stab12-list (gethash "authoritative_projection_records" data)))
         (identity (gethash "identity" data (obj)))
         (ticks (gethash "ticks" data (obj)))
         (synthetic (%stab12-list (gethash "accepted_synthetic_records" data)))
         (pairs (%stab12-list (gethash "accepted_same_topic_pairs" data)))
         (memory (gethash "conversational_memory" data (obj)))
         (temporal (%stab12-list (gethash "temporal_fixtures" data)))
         (initiative (%stab12-list (gethash "initiative_decisions" data)))
         (recovery (gethash "recovery" data (obj)))
         (scorecard (gethash "scorecard" data (obj)))
         (latency (gethash "latency" data (obj)))
         (dimensions (gethash "dimensions" scorecard (obj)))
         (results nil))
    (flet ((gate (name passed &optional observed required)
             (push (%stab12-result name passed observed required) results)))
      (gate "authoritative-projection-grounded"
            (and (gethash "authoritative_projection_records" data)
                 (%stab12-all projection
                              (lambda (record)
                                (and (member (gethash "grounding_status" record)
                                             '("grounded" "partially-grounded") :test #'string=)
                                     (not (member (gethash "epistemic_status" record)
                                                  '("unclassified" "legacy-unclassified")
                                                  :test #'string=)))))))
      (gate "identity-zero-failures"
            (and (zerop (gethash "fixture_failures" identity -1))
                 (= 100 (gethash "shadow_calls" identity -1))
                 (zerop (gethash "shadow_failures" identity -1))))
      (gate "one-terminal-per-tick"
            (and (numberp (gethash "starts" ticks))
                 (= (gethash "starts" ticks) (gethash "terminals" ticks))
                 (= (gethash "starts" ticks)
                    (gethash "unique_terminal_generations" ticks))))
      (gate "synthetic-lineage-valid"
            (and (gethash "accepted_synthetic_records" data)
                 (%stab12-all synthetic
                              (lambda (record)
                                (plusp (length (%stab12-list
                                                (gethash "root_observation_ids" record))))))))
      (gate "novelty-or-new-evidence"
            (and (gethash "accepted_same_topic_pairs" data)
                 (%stab12-all pairs
                              (lambda (pair)
                                (or (< (gethash "similarity" pair 1.0d0) 0.90d0)
                                    (gethash "new_evidence" pair)
                                    (gethash "supersession" pair))))))
      (let ((precision (gethash "precision" memory)))
        (gate "memory-precision"
              (and (numberp precision)
                   (>= precision *stabilization-eval-min-memory-precision*)
                   (numberp (gethash "recall" memory)))
              precision *stabilization-eval-min-memory-precision*))
      (gate "temporal-fixtures-truthful"
            (and temporal
                 (%stab12-all temporal
                              (lambda (fixture)
                                (and (gethash "activity_accurate" fixture)
                                     (gethash "limitations_accurate" fixture))))))
      (gate "multipart-capture-exact" (eq t (gethash "multipart_capture_exact" data)))
      (gate "initiative-auditable"
            (and initiative
                 (%stab12-all initiative
                              (lambda (decision)
                                (and (plusp (length (%stab12-list
                                                    (gethash "candidate_ids" decision))))
                                     (plusp (length (%stab12-list
                                                    (gethash "evidence_node_ids" decision))))
                                     (not (find "malformed-structured-score"
                                                (%stab12-list (gethash "gates" decision))
                                                :test #'string=)))))))
      (gate "shadow-no-delivery"
            (and (gethash "shadow_delivery_count" data)
                 (zerop (gethash "shadow_delivery_count" data))))
      (let ((chars (gethash "projection_dynamic_chars" data))
            (production (gethash "production_replay_dynamic_chars" data)))
        (gate "projection-size"
              (and (numberp chars) (numberp production)
                   (<= chars *stabilization-eval-max-projection-chars*)
                   (< chars production)) chars *stabilization-eval-max-projection-chars*))
      (gate "fresh-executable-recovery"
            (and (gethash "fresh" recovery) (gethash "executable_ok" recovery)
                 (gethash "fingerprint_equal" recovery)))
      (gate "forced-failure-chat-survival"
            (and (gethash "forced_failure_results" data)
                 (%stab12-all (gethash "forced_failure_results" data)
                              (lambda (result) (gethash "ordinary_chat_survived" result)))))
      (gate "complete-standard-scorecard"
            (and (stringp (gethash "benchmark_version" scorecard))
                 (hash-table-p (gethash "before" scorecard))
                 (hash-table-p (gethash "after" scorecard))
                 (%stab12-all *stabilization-eval-required-dimensions*
                              (lambda (name) (not (null (gethash name dimensions)))))))
      (gate "local-latency-within-rule"
            (and (numberp (gethash "local_regression_percent" latency))
                 (numberp (gethash "promotion_limit_percent" latency))
                 (<= (gethash "local_regression_percent" latency)
                     (gethash "promotion_limit_percent" latency))
                 (eq t (gethash "external_variance_separated" latency))))
      (let* ((ordered (nreverse results))
             (passed (count-if (lambda (result) (gethash "passed" result)) ordered)))
        (setf *stabilization-eval-last-report*
              (obj "kind" "deterministic" "passed" (= passed (length ordered))
                   "passed_count" passed "gate_count" (length ordered)
                   "gates" (coerce ordered 'vector)))
        *stabilization-eval-last-report*))))

(defun stabilization-evaluate-online (data)
  "Report stochastic online suites separately; these never override gates."
  (let ((suites (obj)))
    (dolist (name '("epistemic_qa" "identity_perspective" "reciprocity"
                    "memory_relevance" "novelty" "initiative" "recovery" "cost"))
      (setf (gethash name suites) (or (gethash name data) :null)))
    (obj "kind" "stochastic-online" "promotion_authority" nil "suites" suites)))

(defun stabilization-promotion-readiness (deterministic evidence)
  "Return staged readiness only. This function never changes a mode."
  (let* ((decisions (gethash "initiative_shadow_decisions" evidence 0))
         (ordinary-soak (gethash "ordinary_use_soak_hours" evidence 0))
         (post-recovery-soak (gethash "post_recovery_soak_hours" evidence 0))
         (cold-recovery (gethash "cold_recovery_ok" evidence nil))
         (ready (and (gethash "passed" deterministic)
                     (>= ordinary-soak 24) (>= decisions 20)
                     cold-recovery (>= post-recovery-soak 24))))
    (obj "ready" (if ready t nil) "mode_changes_performed" 0
         "deterministic_passed" (if (gethash "passed" deterministic) t nil)
         "ordinary_use_soak_hours" ordinary-soak
         "initiative_shadow_decisions" decisions
         "cold_recovery_ok" (if cold-recovery t nil)
         "post_recovery_soak_hours" post-recovery-soak
         "rollback" "mode-change-no-restart")))

(defun stabilization-eval-report ()
  (or *stabilization-eval-last-report*
      (obj "kind" "deterministic" "passed" :null "gates" (vector))))
