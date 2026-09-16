;;;; Repeatable, content-free projection microbenchmark.

(in-package :agent)

(ql:quickload :bordeaux-threads :silent t)

(defvar *context-projection-mode* :legacy)
(defvar *last-self-mod-history* nil)
(defvar *timing-installed-wrappers* nil)

(defun memory-search (&rest arguments) (declare (ignore arguments)) nil)
(defun replay-events (&rest arguments) (declare (ignore arguments)) nil)
(defun modulator-state () (obj "certainty" 0.75d0 "arousal" 0.25d0))
(defun drive-state () (obj "connection" (obj "current" 0.30d0)))
(defun log-event (&rest arguments) (declare (ignore arguments)) nil)
(defun auto-turn (prompt) prompt)

(load (test-source "context-projection.lisp"))

(defun %context-benchmark-percentile (sorted fraction)
  (when sorted
    (nth (min (1- (length sorted))
              (floor (* fraction (length sorted))))
         sorted)))

(defun %context-benchmark-legacy-fixture ()
  (with-output-to-string (stream)
    (format stream
            "<!-- CONTINUITY:BEGIN -->~%Uncited narrative continuity repeated across independent prompt writers and presented without a shared provenance contract.~%<!-- CONTINUITY:END -->~%")
    (dolist (name '("AFFECT" "INTRUSIONS" "WANTS" "SOUL" "BRINGUP"
                    "LATENT" "SHARED-MEMORY"))
      (format stream
              "<!-- ~a:BEGIN -->~%Legacy dynamic state repeats context, interpretations, relationship framing, and possible memories from an independent source. This fixture contains no private production text.~%<!-- ~a:END -->~%"
              name name))
    (format stream
            "RELATIONSHIP CONTEXT~%Unmarked journal and legacy graph suffix.~%=== PAI'S MEMORY ===~%")))

(let* ((output (pathname
                (or (uiop:getenv "CONTEXT_BENCHMARK_OUTPUT")
                    (namestring (merge-pathnames "evals/results/STAB-06-context-projection-offline.sanitized.json" *pai-root*)))))
       (batches 101)
       (projections-per-batch 100)
       (prompt "What happened while I was away?")
       (rows (list
              (obj "id" "fixture-user-1" "kind" "observation"
                   "origin_class" "lived-user" "epistemic_status" "user-report"
                   "grounding_status" "grounded"
                   "content" "the operator asked for measured progress." "quarantined" nil)
              (obj "id" "fixture-unsafe-1" "kind" "thought"
                   "origin_class" "legacy-unclassified"
                   "epistemic_status" "legacy-unclassified"
                   "grounding_status" "unclassified"
                   "content" "Fictional all-night narrative." "quarantined" nil)))
       (events (list
                (obj "type" "user-message" "timestamp" "2026-07-30T01:00:00Z"
                     "payload" (obj))
                (obj "type" "tick-terminal" "timestamp" "2026-07-30T02:00:00Z"
                     "payload" (obj "status" "error" "reason" "model-timeout"
                                    "tick_type" "reflect"))
                (obj "type" "reflection-no-novelty"
                     "timestamp" "2026-07-30T03:00:00Z"
                     "payload" (obj "reason" "near-repeat"))))
       (*context-projection-memory-search-fn*
         (lambda (query k) (declare (ignore query k)) rows))
       (*context-projection-events-fn*
         (lambda (from to) (declare (ignore from to)) events))
       (*context-projection-soul-fn* (lambda () nil))
       (legacy (%context-benchmark-legacy-fixture))
       (projection (build-context-projection prompt :now 3994416000 :mode :enforced))
       (rendered (render-context-projection projection))
       (samples nil))
  (dotimes (warmup 100)
    (declare (ignorable warmup))
    (render-context-projection
     (build-context-projection prompt :now 3994416000 :mode :enforced)))
  (dotimes (batch batches)
    (declare (ignorable batch))
    (let ((start (get-internal-real-time)))
      (dotimes (projection-number projections-per-batch)
        (declare (ignorable projection-number))
        (render-context-projection
         (build-context-projection prompt :now 3994416000 :mode :enforced)))
      (push (* 1000.0d0
               (/ (- (get-internal-real-time) start)
                  internal-time-units-per-second
                  projections-per-batch))
            samples)))
  (setf samples (sort samples #'<))
  (let ((result
          (obj "schema_version" 1
               "change_id" "STAB-06-context-projection-offline"
               "source_revision" (or (uiop:getenv "CONTEXT_SOURCE_REVISION")
                                     "working-tree")
               "fixture" "inventoried-eight-writers-plus-unmarked-suffix"
               "batches" batches
               "projections_per_batch" projections-per-batch
               "iterations" (* batches projections-per-batch)
               "legacy_dynamic_chars" (length legacy)
               "unified_projection_chars" (length rendered)
               "character_reduction_fraction"
               (- 1.0d0 (/ (length rendered) (float (length legacy) 1.0d0)))
               "projection_ms_p50" (%context-benchmark-percentile samples 0.50d0)
               "projection_ms_p95" (%context-benchmark-percentile samples 0.95d0)
               "projection_ms_max" (car (last samples))
               "temporal_query" (if (gethash "temporal_query" projection) t nil)
               "grounded_source_count" (length (gethash "source_node_ids" projection))
               "unsafe_fixture_present" (if (search "fixture-unsafe-1" rendered) t nil)
               "error_visible" (if (search "status=error" rendered) t nil)
               "repetition_visible" (if (search "reflection-no-novelty" rendered) t nil))))
    (ensure-directories-exist output)
    (with-open-file (stream output :direction :output :if-exists :supersede
                                   :if-does-not-exist :create
                                   :external-format :utf-8)
      (write-string (shasht:write-json result nil) stream)
      (terpri stream))
    (format t "context benchmark written to ~a.~%" output)))
