;;;; Read-only current-window entry point for the canonical STAB scorecard.
;;;; Load stabilization-baseline.lisp first. Unlike the frozen release fixture,
;;;; this entry point deliberately uses the current timestamp and no event cap.

(in-package :agent)

(let* ((events-file
         (pathname (or (uiop:getenv "CURRENT_SCORECARD_EVENTS_FILE")
                       "/agent/state/events.jsonl")))
       (output-file
         (pathname (or (uiop:getenv "CURRENT_SCORECARD_OUTPUT_FILE")
                       "/agent/state/evals/results/current-window.json")))
       (hours
         (parse-integer (or (uiop:getenv "CURRENT_SCORECARD_HOURS") "24")))
       (snapshot-end
         (or (uiop:getenv "CURRENT_SCORECARD_END_SQL")
             (error "CURRENT_SCORECARD_END_SQL is required for a reproducible current-window run.")))
       (change-id
         (or (uiop:getenv "BASELINE_CHANGE_ID")
             "current-observability-window"))
       (source-revision
         (or (uiop:getenv "BASELINE_SOURCE_REVISION") "unknown")))
  (run-stabilization-baseline
   :events-file events-file
   :output-file output-file
   :hours hours
   :snapshot-event-id nil
   :snapshot-end-sql snapshot-end
   :change-id change-id
   :source-revision source-revision))
