(in-package :agent)

(defvar *stab12-pass* 0)
(defvar *stab12-fail* 0)
(defun stab12-check (name condition)
  (if condition
      (progn (incf *stab12-pass*) (format t "  ok   ~a~%" name))
      (progn (incf *stab12-fail*) (format t "  FAIL ~a~%" name))))

(load (test-source "stabilization-evals.lisp"))

(defun stab12-good-data ()
  (let ((dimensions (obj)))
    (dolist (name *stabilization-eval-required-dimensions*)
      (setf (gethash name dimensions) (obj "before" 1 "after" 1)))
    (obj
     "authoritative_projection_records"
     (vector (obj "grounding_status" "grounded" "epistemic_status" "user-report"))
     "identity" (obj "fixture_failures" 0 "shadow_calls" 100 "shadow_failures" 0)
     "ticks" (obj "starts" 3 "terminals" 3 "unique_terminal_generations" 3)
     "accepted_synthetic_records" (vector (obj "root_observation_ids" (vector "root-1")))
     "accepted_same_topic_pairs" (vector (obj "similarity" 0.92d0 "new_evidence" t))
     "conversational_memory" (obj "precision" 0.85d0 "recall" 0.70d0)
     "temporal_fixtures" (vector (obj "activity_accurate" t "limitations_accurate" t))
     "multipart_capture_exact" t
     "initiative_decisions"
     (vector (obj "candidate_ids" (vector "candidate-1")
                  "evidence_node_ids" (vector "evidence-1") "gates" (vector)))
     "shadow_delivery_count" 0 "projection_dynamic_chars" 5000
     "production_replay_dynamic_chars" 7000
     "recovery" (obj "fresh" t "executable_ok" t "fingerprint_equal" t)
     "forced_failure_results" (vector (obj "ordinary_chat_survived" t))
     "scorecard" (obj "benchmark_version" "v1" "before" (obj) "after" (obj)
                      "dimensions" dimensions)
     "latency" (obj "local_regression_percent" 3.0d0 "promotion_limit_percent" 5.0d0
                    "external_variance_separated" t))))

(format t "~%== deterministic release gate ==~%")
(let ((report (stabilization-evaluate-deterministic (stab12-good-data))))
  (stab12-check "complete evidence passes every deterministic gate"
                (and (gethash "passed" report)
                     (= (gethash "passed_count" report) (gethash "gate_count" report))))
  (let ((readiness (stabilization-promotion-readiness
                    report (obj "ordinary_use_soak_hours" 0
                                "initiative_shadow_decisions" 0
                                "cold_recovery_ok" nil
                                "post_recovery_soak_hours" 0))))
    (stab12-check "passing code does not bypass soak or change modes"
                  (and (not (gethash "ready" readiness))
                       (zerop (gethash "mode_changes_performed" readiness)))))
  (let ((readiness (stabilization-promotion-readiness
                    report (obj "ordinary_use_soak_hours" 24
                                "initiative_shadow_decisions" 20
                                "cold_recovery_ok" t
                                "post_recovery_soak_hours" 24))))
    (stab12-check "all staged evidence reports readiness without promotion"
                  (and (gethash "ready" readiness)
                       (zerop (gethash "mode_changes_performed" readiness))))))

(let ((bad (stab12-good-data)))
  (setf (gethash "shadow_delivery_count" bad) 1)
  (remhash "latency" bad)
  (let* ((report (stabilization-evaluate-deterministic bad))
         (failed (remove-if (lambda (gate) (gethash "passed" gate))
                            (coerce (gethash "gates" report) 'list))))
    (stab12-check "missing evidence and shadow delivery fail closed"
                  (and (not (gethash "passed" report))
                       (find "shadow-no-delivery" failed
                             :key (lambda (gate) (gethash "gate" gate)) :test #'string=)
                       (find "local-latency-within-rule" failed
                             :key (lambda (gate) (gethash "gate" gate)) :test #'string=)))))

(let ((online (stabilization-evaluate-online (obj "cost" (obj "usd" 0.01d0)))))
  (stab12-check "online suites are reported without promotion authority"
                (and (not (gethash "promotion_authority" online))
                     (gethash "cost" (gethash "suites" online)))))

(format t "~%~a passed, ~a failed~%" *stab12-pass* *stab12-fail*)
(when (plusp *stab12-fail*) (sb-ext:exit :code 1))
