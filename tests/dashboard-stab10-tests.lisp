(in-package :agent)

(ql:quickload '(:hunchentoot :bordeaux-threads :shasht) :silent t)

(unless (fboundp 'obj)
  (defun obj (&rest pairs)
    (loop with table = (make-hash-table :test #'equal)
          for (key value) on pairs by #'cddr
          do (setf (gethash key table) value)
          finally (return table))))

(defvar *dash10-pass* 0)
(defvar *dash10-fail* 0)
(defvar *dash10-events* nil)
(defvar *dash10-replay-calls* 0)
(defun dash10-check (name condition)
  (if condition
      (progn (incf *dash10-pass*) (format t "  ok   ~a~%" name))
      (progn (incf *dash10-fail*) (format t "  FAIL ~a~%" name))))

(setf *dash10-events*
      (list
       (obj "id" 1 "timestamp" 100 "type" "tick-start"
            "payload" (obj "generation_id" "gen-1" "tick_type" "reflect"))
       (obj "id" 2 "timestamp" 101 "type" "memory-admission-accepted"
            "payload" (obj "generation_id" "gen-1" "node_id" "bad-1"
                           "origin_class" "synthetic" "epistemic_status" "hypothesis"
                           "grounding_status" "ungrounded" "kind" "thought"
                           "producer" "fixture"))
       (obj "id" 3 "timestamp" 102 "type" "cognitive-call-end"
            "payload" (obj "generation_id" "gen-1" "status" "duplicate"))
       (obj "id" 4 "timestamp" 103 "type" "cognitive-call-end"
            "payload" (obj "generation_id" "gen-1" "status" "duplicate"))
       (obj "id" 5 "timestamp" 104 "type" "cognitive-call-end"
            "payload" (obj "generation_id" "gen-1" "status" "accepted"))
       (obj "id" 6 "timestamp" 105 "type" "cognitive-call-end"
            "payload" (obj "generation_id" "gen-1" "status" "accepted"))
       (obj "id" 7 "timestamp" 106 "type" "timing-trace"
            "payload" (obj "trace_id" "trace-1" "turn_id" "turn-1"
                           "generation_id" "gen-1" "duration_ms" 1000.0d0
                           "spans" (vector
                                    (obj "span_id" "s1" "name" "turn.queue_wait"
                                         "start_offset_ms" 0.0d0 "duration_ms" 100.0d0
                                         "attributes" (obj))
                                    (obj "span_id" "s2" "name" "model.public_pipeline"
                                         "start_offset_ms" 100.0d0 "duration_ms" 600.0d0
                                         "attributes" (obj "prompt_tokens" 100))
                                    (obj "span_id" "s3" "name" "conversation.persist"
                                         "start_offset_ms" 700.0d0 "duration_ms" 50.0d0
                                         "attributes" (obj)))))))
(setf (fdefinition 'replay-events)
      (lambda (&key from to limit types exclude-types)
        (declare (ignore from to limit))
        (incf *dash10-replay-calls*)
        (remove-if-not
         (lambda (event)
           (let ((type (gethash "type" event)))
             (and (or (null types) (member type types :test #'string=))
                  (or (null exclude-types)
                      (not (member type exclude-types :test #'string=))))))
         *dash10-events*)))
(setf (fdefinition 'memory-recall)
      (lambda (&rest arguments) (declare (ignore arguments)) nil))
(setf (fdefinition 'conversation-context-budget-report)
      (lambda ()
        (obj "mode" "shadow" "candidate_records" 20
             "candidate_chars" 33600 "leading_briefs" 17)))
(setf (fdefinition 'reciprocity-canary-report)
      (lambda ()
        (obj "mode" "shadow" "record_count" 1 "open_unanswered" 0
             "delivery_capable" nil
             "status_counts" (obj "would-send" 1)
             "records" (vector
                        (obj "status" "would-send"
                             "source" "explore-development"
                             "reason" "eligible"
                             "content_preview" "A concrete grounded thought."
                             "topic" "fixture"
                             "initiative_decision_id" "decision-1")))))
(setf (fdefinition 'pull-reciprocity-report)
      (lambda () (obj "schema_version" 1 "mode" "enabled"
                      "reviewed_candidates" 20 "label_batches" 1
                      "delivery_capable" nil)))
(setf (fdefinition 'runtime-truth-manifest)
      (lambda () (obj "schema_version" 1 "source_of_truth" "live-bound-values"
                      "public_outbound" (obj "mode" "observe" "unclassified" 3))))
(setf (fdefinition 'replay-capsule-report)
      (lambda () (obj "schema_version" 2 "records" 2 "autostart" t
                      "worker_alive" t "file_bytes" 100
                      "disk_max_bytes" 8388608
                      "coverage" (obj "trigger_type" (obj "user-reply" 1)
                                      "fidelity" (obj "exact" 2)
                                      "day" (obj "2026-08-04" 2)
                                      "outcome" (obj "observed" 2))
                      "pruning" (obj "record_cap" 0))))

(setf *conscious-conversation-cost-ceiling-usd* 2d0
      *conscious-conversation-provider-spent-usd* 0.25d0
      *conscious-conversation-provider-attempts* 3
      *conscious-conversation-private-provider-spent-usd* 0.1d0
      *conscious-conversation-private-provider-attempts* 2
      *conscious-recursive-mind-private-budget-percent* 50)

(load (test-source "near-term-workspace.lisp"))
(load (test-source "near-term-workspace-adapters.lisp"))
(setf *near-term-workspace-latent-source-fn*
      (lambda ()
        (list (obj "id" "dash-latent" "state" "ready"
                   "content" "A bounded dashboard workspace fixture."
                   "evidence_ids" (vector "dash-evidence")
                   "source_event_ids" (vector "dash-event")
                   "updated_at" 100 "expires_at" 9999999999)))
      *near-term-workspace-question-source-fn* (lambda () nil)
      *near-term-workspace-scheduler-source-fn* (lambda () nil)
      *near-term-workspace-initiative-source-fn* (lambda () nil))

(load (test-source "turn-trace-projection.lisp"))
(load (test-source "assets.lisp"))
(load (test-source "dashboard.lisp"))

(format t "~%== correlations, timing and alerts ==~%")
(setf *dash10-replay-calls* 0)
(let* ((report (dashboard-report :hours 1))
       (timing (gethash "timing" report))
       (alerts (coerce (gethash "alerts" report) 'list)))
  (dash10-check "one ledger replay supplies rows and aggregates"
                (= 1 *dash10-replay-calls*))
  (dash10-check "event table remains parsed and bounded"
                (= 7 (length (gethash "events" report))))
  (dash10-check "correlation groups expose linked generation"
                (plusp (length (gethash "gen-1" (gethash "correlations" report)))))
  (dash10-check "timing view reports percentiles and sample count"
                (and (= 1 (gethash "sample_count" timing))
                     (= 1000.0d0 (gethash "p95_ms" timing))))
  (dash10-check "critical path separates queue/model/persistence"
                (let ((totals (gethash "critical_path_totals_ms" timing)))
                  (and (= 100.0d0 (gethash "queue" totals))
                       (= 600.0d0 (gethash "model" totals))
                       (= 50.0d0 (gethash "persistence" totals)))))
  (dash10-check "slowest span identifies injected model delay"
                (string= "model.public_pipeline"
                         (gethash "name" (aref (gethash "slowest_spans" timing) 0))))
  (dash10-check "missing terminal and ungrounded admission alert"
                (and (find "missing-tick-terminal" alerts
                           :key (lambda (item) (gethash "kind" item)) :test #'string=)
                     (find "ungrounded-admission" alerts
                           :key (lambda (item) (gethash "kind" item)) :test #'string=)))
  (dash10-check "duplicate-rate threshold alerts"
                (find "duplicate-rate" alerts
                      :key (lambda (item) (gethash "kind" item)) :test #'string=))
  (dash10-check "memory health is classified by producer and grounding"
                (let ((health (gethash "memory_health" report)))
                  (and (= 1 (gethash "accepted" health))
                       (= 1 (gethash "fixture" (gethash "by_producer" health))))))
  (dash10-check "dashboard report includes bounded near-term shadow"
                (let ((workspace (gethash "near_term_workspace" report)))
                  (and (string= "dashboard-shadow"
                                (gethash "mode" workspace))
                       (= 1 (gethash "active_items" workspace))
                       (null (gethash "prompt_integration" workspace)))))
  (dash10-check "dashboard report includes conversation prompt telemetry"
                (let ((context (gethash "conversation_context" report)))
                  (and (string= "shadow" (gethash "mode" context))
                       (= 20 (gethash "candidate_records" context)))))
  (dash10-check "dashboard exposes reciprocity canary evidence"
                (let ((canary (gethash "reciprocity_canary" report)))
                  (and (string= "shadow" (gethash "mode" canary))
                       (= 1 (gethash "record_count" canary)))))
  (dash10-check "dashboard exposes delivery-incapable pull/label status"
                (let ((pull (gethash "pull_reciprocity" report)))
                  (and (string= "enabled" (gethash "mode" pull))
                       (= 20 (gethash "reviewed_candidates" pull))
                       (null (gethash "delivery_capable" pull)))))
  (dash10-check "dashboard exposes runtime truth"
                (string= "live-bound-values"
                         (gethash "source_of_truth" (gethash "runtime_truth" report))))
  (dash10-check "dashboard exposes bounded replay capsule status"
                (= 2 (gethash "records" (gethash "replay_capsules" report)))))

(dash10-check "dashboard escapes rendered event content"
              (and (search "replace(/[&<>]/g" *dashboard-html*)
                   (search "encodeURIComponent" *dashboard-html*)))

(dash10-check "dashboard HTML reaches its closing document sentinel"
              (and (search "No capture files</option>" *dashboard-html*)
                   (search "</script></body></html>" *dashboard-html*)))

(dash10-check "dashboard reports browser-side data failures visibly"
              (and (search "showStatus" *dashboard-html*)
                   (search "Dashboard data failed:" *dashboard-html*)
                   (search "response.ok" *dashboard-html*)))

(dash10-check "dashboard renders bounded feedback-loop status"
              (search "feedback_loop?.projected_active_question_count"
                      *dashboard-html*))

(dash10-check "dashboard renders bounded near-term workspace data"
              (and (search "function workspace" *dashboard-html*)
                   (search "bounded working state" *dashboard-html*)
                   (search "workspace(d.near_term_workspace)"
                           *dashboard-html*)))

(dash10-check "dashboard renders context record and character budget"
              (and (search "conversation_context" *dashboard-html*)
                   (search "candidate_records" *dashboard-html*)
                   (search "candidate_chars" *dashboard-html*)))

(dash10-check "dashboard renders outbound audit and capsule counts"
              (and (search "function governance" *dashboard-html*)
                   (search "Unclassified sends" *dashboard-html*)
                   (search "Replay capsules" *dashboard-html*)
                   (search "Replay triggers" *dashboard-html*)
                   (search "Replay fidelity" *dashboard-html*)
                   (search "Replay days" *dashboard-html*)
                   (search "Replay outcomes" *dashboard-html*)
                   (search "Replay pruned" *dashboard-html*)))

(dash10-check "dashboard renders reciprocity lifecycle records"
              (and (search "function reciprocity" *dashboard-html*)
                   (search "reciprocity(d.reciprocity_canary)"
                           *dashboard-html*)
                   (search "Canary unanswered" *dashboard-html*)))

(dash10-check "dashboard JSON validator rejects partial documents"
              (and (%dash-valid-json-p "{\"ok\":true}")
                   (not (%dash-valid-json-p "{\"broken\":"))))

(let ((compact (%dashboard-browser-report 1 "all" nil)))
  (dash10-check "browser report stays bounded and omits full trace detail"
                (and (<= (length (gethash "events" compact)) 100)
                     (zerop (hash-table-count (gethash "correlations" compact)))
                     (zerop (length (gethash "traces" (gethash "timing" compact)))))))

(let ((trace (dashboard-turn-trace-report :turn-id "turn-1" :hours 24)))
  (dash10-check "deliberate trace read uses the content-free shared projection"
                (and (string= "turn-1" (gethash "turn_id" trace))
                     (string= "legacy-unlinked"
                              (gethash "physical_attempts_status" trace))
                     (null (gethash "private_content_included" trace)))))

(dash10-check "dashboard exposes deliberate exact-ID trace controls"
              (and (search "/api/dashboard/turn-trace" *dashboard-html*)
                   (search "Exact turn ID" *dashboard-html*)
                   (search "traceTurn" *dashboard-html*)
                   (search "Loaded only on request" *dashboard-html*)))

(dash10-check "observability is a separate shell-backed page"
              (and (search "PAI OBSERVABILITY" *observability-dashboard-html*)
                   (search "/app-shell.js" *observability-dashboard-html*)
                   (search "View exact context" *observability-dashboard-js*)
                   (search "FULL PROVIDER REQUEST" *observability-dashboard-js*)
                   (search "FULL PROVIDER RESPONSE" *observability-dashboard-js*)
                   (search "/api/dashboard/observability/live"
                           *observability-dashboard-js*)
                   (search "/api/dashboard/observability/history"
                           *observability-dashboard-js*)
                   (search "json-scroll" *observability-dashboard-html*)
                   (search "white-space:pre-wrap" *observability-dashboard-html*)))

(let ((budget (%dashboard-session-budget-report)))
  (dash10-check "observability budget snapshot does not need the mind lock"
                (and (string= "non-blocking-observability"
                              (gethash "snapshot_consistency" budget))
                     (= 1d0 (gethash "private_cost_ceiling_usd" budget))
                     (< (abs (- 0.9d0
                                (gethash "private_remaining_usd" budget)))
                        1d-12))))

(setf *dash10-replay-calls* 0)
(let ((history (%dashboard-observability-history-report 1)))
  (dash10-check "observability history uses bounded type batches"
                (and (= (ceiling (length *dashboard-activity-event-types*)
                                 *dashboard-event-query-type-limit*) *dash10-replay-calls*)
                     (vectorp (gethash "events" history))
                     (hash-table-p (gethash "activity" history)))))

(let* ((now (get-universal-time))
       (events (list
                (obj "id" 20 "timestamp" now
                     "type" "model-request"
                     "payload" (obj "model_call_id" "model:test:1"
                                    "model" "fixture/model"))
                (obj "id" 21 "timestamp" now
                     "type" "recursive-tool-execution"
                     "payload" (obj "tool_name" "search-memory"))))
       (activity (%dashboard-activity-report events 1))
       (bucket (aref (gethash "buckets" activity)
                     (1- (length (gethash "buckets" activity))))))
  (dash10-check "activity projection counts model and tool boundaries"
                (and (= 1 (gethash "model_requests" bucket))
                     (= 1 (gethash "tool_calls" bucket))
                     (= 1 (gethash "search-memory"
                                   (gethash "tool_usage" activity))))))

(let* ((event (obj "id" 22 "type" "peer-message-received"
                   "payload" (obj "sender_name" "Synthetic Peer" "thread_id" "fixture-thread"
                                  "text" "Synthetic private body")))
       (payload (gethash "payload" (%dashboard-event-project event))))
  (dash10-check "peer history exposes provenance without message bodies"
                (and (equal "Synthetic Peer" (gethash "sender_name" payload))
                     (equal "fixture-thread" (gethash "thread_id" payload))
                     (not (nth-value 1 (gethash "text" payload))))))

(let ((original (symbol-function 'replay-events)) (calls nil))
  (unwind-protect
       (progn
         (setf (symbol-function 'replay-events)
               (lambda (&key from limit types)
                 (declare (ignore from))
                 (push types calls)
                 (unless (and (<= (length types) 32) (= limit 2))
                   (error "Unbounded dashboard query"))
                 (list (obj "id" (length calls)))))
         (let ((rows (%dashboard-replay-activity-events 0 :limit 2)))
           (dash10-check "activity vocabulary is batched without duplicate type queries"
                         (and (= 2 (length calls))
                              (= (length (apply #'append calls))
                                 (length (remove-duplicates (apply #'append calls) :test #'equal)))
                              (equal '(1 2) (mapcar (lambda (row) (gethash "id" row)) rows))))))
    (setf (symbol-function 'replay-events) original)))

(let ((calls 0))
  (setf (fdefinition 'conscious-recursive-peer-message-inspect)
        (lambda (&optional limit) (declare (ignore limit))
          (incf calls)
          (obj "scope" "retained-recursive-projection" "pending_count" 2 "items" #())))
  (let ((report (%dashboard-observability-live-report)))
    (dash10-check "live inbox is read once and shared with attention"
                  (and (= calls 1)
                       (eq (gethash "peer_inbox" report)
                           (gethash "peer_inbox" (gethash "attention" report))))))
  (dash10-check "peer inbox UI is wired with explicit retained scope"
                (and (search "peer-inbox" *observability-dashboard-html*)
                     (search "renderPeerInbox(live.peer_inbox)" *observability-dashboard-js*)
                     (search "not a lifetime ledger count" *observability-dashboard-js*))))

(format t "~%~a passed, ~a failed~%" *dash10-pass* *dash10-fail*)
(when (plusp *dash10-fail*) (sb-ext:exit :code 1))
