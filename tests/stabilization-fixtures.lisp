;;;; stabilization-fixtures.lisp -- deterministic known-failure gate.
;;;; Load after stabilization-baseline.lisp. The gate intentionally exits
;;;; non-zero while the frozen baseline exhibits the defects this sprint owns.

(in-package :agent)

(export '(stabilization-baseline-failures run-stabilization-baseline-gate))

(defun %fixture-json-file (path)
  (shasht:read-json (uiop:read-file-string path)))

(defun %fixture-ref (object &rest keys)
  (reduce (lambda (value key) (and (hash-table-p value) (gethash key value)))
          keys :initial-value object))

(defun stabilization-baseline-failures (scorecard)
  "Return stable release-gate failures; no model calls and no state writes."
  (let* ((canonical (gethash "scorecard" scorecard))
         (worldview (%fixture-ref canonical "worldview_development"
                                  "raw_worldview_variation"))
         (reliability (%fixture-ref canonical "reliability_cost" "reliability"))
         (reciprocity (%fixture-ref canonical "reciprocity"))
         (outreach (%fixture-ref canonical "outreach"))
         (memory (%fixture-ref canonical "memory"))
         (safety (%fixture-ref canonical "epistemic_safety"))
         (failures nil))
    (flet ((require-gate (name passed evidence)
             (unless passed
               (push (obj "gate" name "status" "fail" "evidence" evidence)
                     failures))))
      (require-gate
       "worldview-near-duplicate-rate<=0.25"
       (let ((rate (and worldview (gethash "near_duplicate_node_rate" worldview))))
         (and (numberp rate) (<= rate 0.25d0)))
       (or (and worldview (gethash "near_duplicate_node_rate" worldview)) :null))
      (require-gate "tick-missing-terminal=0"
                    (eql 0 (gethash "tick_missing_terminal" reliability))
                    (gethash "tick_missing_terminal" reliability :null))
      (require-gate "reciprocity-fixed-rubric-present"
                    (numberp (gethash "fixed_rubric_score" reciprocity))
                    (gethash "fixed_rubric_score" reciprocity :null))
      (require-gate "outreach-labelled-recall-present"
                    (numberp (gethash "labelled_recall" outreach))
                    (gethash "labelled_recall" outreach :null))
      (require-gate "memory-labelled-recall-present"
                    (numberp (gethash "labelled_retrieval_recall" memory))
                    (gethash "labelled_retrieval_recall" memory :null))
      (require-gate "epistemic-ungrounded-admission-measured"
                    (numberp (gethash "ungrounded_admission" safety))
                    (gethash "ungrounded_admission" safety :null)))
    (nreverse failures)))

(defun run-stabilization-baseline-gate (&rest baseline-args)
  "Write the baseline scorecard, print known failures, then fail the process."
  ;; Touch each immutable fixture here so missing or malformed corpus files
  ;; fail before a result can be mistaken for a complete benchmark.
  (dolist (path '(#P"/agent/state/evals/fixtures/v1/identity.json"
                  #P"/agent/state/evals/fixtures/v1/reciprocity.json"
                  #P"/agent/state/evals/fixtures/v1/initiative.json"
                  #P"/agent/state/evals/fixtures/v1/latency-paths.json"
                  #P"/agent/state/evals/fixtures/v1/worldview-novelty.json"
                  #P"/agent/state/evals/fixtures/v1/memory-retrieval.json"
                  #P"/agent/state/evals/fixtures/v1/recovery.json"
                  #P"/agent/state/evals/fixtures/v1/tick-schedule.json"))
    (%fixture-json-file path))
  (let* ((scorecard (apply #'run-stabilization-baseline baseline-args))
         (failures (stabilization-baseline-failures scorecard)))
    (format t "~&known-failure gate: ~a failure(s).~%" (length failures))
    (dolist (failure failures)
      (format t "  FAIL ~a: ~s~%"
              (gethash "gate" failure) (gethash "evidence" failure)))
    (when failures
      (error "Stabilization release gate failed with ~a known defect(s)."
             (length failures)))
    scorecard))
