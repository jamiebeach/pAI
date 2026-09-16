;;;; Command-line entry point for the frozen scorecard. Load baseline and
;;;; fixtures first; paths are environment-driven to avoid shell quoting drift.

(in-package :agent)

(run-stabilization-baseline-gate
 :events-file (pathname (or (uiop:getenv "BASELINE_EVENTS_FILE")
                            "/agent/state/events.jsonl"))
 :output-file (pathname (or (uiop:getenv "BASELINE_OUTPUT_FILE")
                            "/agent/state/evals/results/current-baseline.json")))
