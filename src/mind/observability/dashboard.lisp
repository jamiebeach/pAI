;;;; dashboard.lisp -- small read-only operator dashboard, 2026-07-29.
(in-package :agent)
(export '(dashboard-report dashboard-correlations dashboard-timing-view
          dashboard-alerts dashboard-turn-trace-report))
(defvar *llm-debug-capture-mode* :off)
(defvar *llm-debug-dashboard-reveal-p* nil)

(defun %dash-json (value) (with-output-to-string (s) (shasht:write-json value s)))
(defun %dash-valid-json-p (text)
  (and (stringp text)
       (handler-case (progn (shasht:read-json text) t)
         (error () nil))))
(defun %dash-json-validated (producer &key (attempts 3))
  "Build, encode, and parse-check a response before Hunchentoot sends it.
Mutable diagnostic structures can change during traversal; retry from a fresh
bounded report rather than sending a syntactically partial JSON document."
  (dotimes (index attempts
                  (values "{\"error\":\"dashboard snapshot unavailable\"}" nil
                          attempts))
    (handler-case
        ;; WITH-OUTPUT-TO-STRING returns only after SHASHT has encoded the
        ;; complete graph. An encoder error unwinds to the retry below, so
        ;; reparsing the completed response merely builds a second large JSON
        ;; object graph and cannot make the returned string more atomic.
        (let ((encoded (%dash-json (funcall producer))))
          (return (values encoded t (1+ index))))
      (error () nil))))
(defun %dash-hours (raw) (or (and raw (ignore-errors (parse-integer raw))) 1))
(defun %dash-events (hours type limit)
  (let* ((cutoff (- (get-universal-time) (* 3600 (max 0 hours))))
         (selected-type (and type (plusp (length type))
                             (not (string= type "all")) type)))
    (replay-events :from cutoff :limit (or limit 250)
                   :types (and selected-type (list selected-type)))))

(defun dashboard-turn-trace-report (&key turn-id trace-id (hours 24))
  "Deliberately project one exactly identified trace without returning bodies."
  (unless (fboundp 'turn-trace-project)
    (error "Turn-trace projection is unavailable."))
  (unless (or (and turn-id (turn-trace-safe-id-p turn-id))
              (and trace-id (turn-trace-safe-id-p trace-id)))
    (error "A safe turn_id or trace_id is required."))
  (let* ((bounded-hours (min 168 (max 1 hours)))
         (cutoff (- (get-universal-time) (* 3600 bounded-hours)))
         ;; Physical rows contain full request/response bodies in the sacred
         ;; ledger. Keep this deliberate read small and project them away
         ;; immediately; timing rows are content-free and may use a wider cap.
         (events (append
                  (replay-events :from cutoff :limit 1000
                                 :types '("timing-trace"))
                  (replay-events :from cutoff :limit 96
                                 :types '("model-request" "model-response")))))
    (turn-trace-project events :turn-id turn-id :trace-id trace-id)))
(defun %dash-types (events)
  (let ((counts (make-hash-table :test #'equal)))
    (dolist (e events) (incf (gethash (gethash "type" e) counts 0))) counts))

(defun %dash-list (value)
  (cond ((null value) nil) ((listp value) value)
        ((vectorp value) (coerce value 'list)) (t (list value))))
(defun %dash-payload (event) (or (gethash "payload" event) (obj)))
(defun %dash-correlation-id (event)
  (let ((payload (%dash-payload event)))
    (or (gethash "trace_id" payload) (gethash "turn_id" payload)
        (gethash "generation_id" payload) (gethash "candidate_id" payload)
        (gethash "decision_id" payload) (gethash "id" event))))
(defun %dash-percentile (numbers fraction)
  (when numbers
    (let* ((sorted (sort (copy-list numbers) #'<))
           (index (min (1- (length sorted))
                       (floor (* fraction (1- (length sorted)))))))
      (nth index sorted))))
(defun %dash-span-owner (name)
  (cond ((or (search "model" name) (search "tool" name) (search "network" name)) "external")
        (t *agent-id*)))
(defun %dash-span-category (name)
  (cond ((search "queue" name) "queue")
        ((or (search "retriev" name) (search "memory" name) (search "embed" name)) "retrieval_embedding")
        ((search "model" name) "model") ((search "tool" name) "tools")
        ((or (search "persist" name) (search "commit" name)) "persistence")
        ((search "broadcast" name) "broadcast") (t "local_processing")))

(defun dashboard-correlations (events)
  (let ((groups (obj)))
    (dolist (event events)
      (let* ((id (%dash-correlation-id event))
             (current (%dash-list (and id (gethash id groups)))))
        (when id
          (setf (gethash id groups)
                (coerce (append current
                                (list (obj "event_id" (or (gethash "id" event) :null)
                                           "type" (gethash "type" event)
                                           "timestamp" (or (gethash "timestamp" event) :null))))
                        'vector)))))
    groups))

(defun dashboard-timing-view (events)
  (let ((durations nil) (span-rows nil) (categories (obj)) (owners (obj))
        (traces nil))
    (dolist (event events)
      (when (string= (gethash "type" event "") "timing-trace")
        (let ((payload (%dash-payload event)))
          (push payload traces)
          (when (numberp (gethash "duration_ms" payload))
            (push (gethash "duration_ms" payload) durations))
          (dolist (span (%dash-list (gethash "spans" payload)))
            (let* ((name (gethash "name" span "unknown"))
                   (duration (gethash "duration_ms" span 0.0d0))
                   (category (%dash-span-category name))
                   (owner (%dash-span-owner name)))
              (incf (gethash category categories 0.0d0) duration)
              (incf (gethash owner owners 0.0d0) duration)
              (push (obj "trace_id" (gethash "trace_id" payload)
                         "span_id" (gethash "span_id" span) "name" name
                         "duration_ms" duration "category" category "owner" owner
                         "start_offset_ms" (gethash "start_offset_ms" span 0.0d0)
                         "attributes" (or (gethash "attributes" span) (obj)))
                    span-rows))))))
    (setf span-rows (sort span-rows #'> :key (lambda (row) (gethash "duration_ms" row))))
    (obj "sample_count" (length durations)
         "median_ms" (or (%dash-percentile durations 0.50d0) :null)
         "p75_ms" (or (%dash-percentile durations 0.75d0) :null)
         "p95_ms" (or (%dash-percentile durations 0.95d0) :null)
         "critical_path_totals_ms" categories "owner_totals_ms" owners
         "slowest_spans" (coerce (subseq span-rows 0 (min 20 (length span-rows))) 'vector)
         "traces" (coerce (nreverse traces) 'vector))))

(defun dashboard-alerts (events)
  (let ((tick-starts (make-hash-table :test #'equal))
        (tick-terminals (make-hash-table :test #'equal))
        (alerts nil) (cognitive-total 0) (duplicates 0))
    (flet ((alert (kind event &optional details)
             (push (obj "kind" kind "event_id" (or (gethash "id" event) :null)
                        "details" (or details :null)) alerts)))
      (dolist (event events)
        (let ((type (gethash "type" event "")) (payload (%dash-payload event)))
          (cond ((string= type "tick-start")
                 (setf (gethash (gethash "generation_id" payload) tick-starts) event))
                ((string= type "tick-terminal")
                 (setf (gethash (gethash "generation_id" payload) tick-terminals) event))
                ((and (member type '("memory-admission-accepted" "epistemic-admission-accepted") :test #'string=)
                      (member (gethash "grounding_status" payload)
                              '("unclassified" "ungrounded") :test #'string=))
                 (alert "ungrounded-admission" event))
                ((and (string= type "cognitive-call-end")
                      (search "identity" (string-downcase (format nil "~a" (gethash "reason" payload "")))))
                 (alert "identity-rejection" event))
                ((member type '("recovery-failed" "recovery-failure") :test #'string=)
                 (alert "recovery-failure" event))
                ((and (string= type "conversation-persistence-status")
                      (> (gethash "lag_seconds" payload 0) 120))
                 (alert "persistence-lag" event (gethash "lag_seconds" payload)))
                ((and (string= type "memory-use-recorded")
                      (> (gethash "activation" payload 0.0d0) 0.85d0))
                 (alert "high-activation" event))
                ((and (string= type "context-projection-built")
                      (plusp (gethash "forbidden_node_count" payload 0)))
                 (alert "forbidden-projection-node" event))
                ((and (string= type "tick-terminal")
                      (string= (gethash "reason" payload "") "write-cap-breach"))
                 (alert "write-cap-breach" event)))
          (when (string= type "cognitive-call-end")
            (incf cognitive-total)
            (when (string= (gethash "status" payload "") "duplicate") (incf duplicates)))))
      (maphash (lambda (generation event)
                 (unless (gethash generation tick-terminals)
                   (alert "missing-tick-terminal" event generation))) tick-starts)
      (when (and (plusp cognitive-total) (> (/ duplicates cognitive-total) 0.30d0))
        (push (obj "kind" "duplicate-rate" "event_id" :null
                   "details" (obj "duplicates" duplicates "total" cognitive-total)) alerts)))
    (coerce (nreverse alerts) 'vector)))

(defun %dash-memory-health (events)
  (let ((origin (obj)) (status (obj)) (grounding (obj)) (kind (obj)) (producer (obj))
        (accepted 0) (rejected 0))
    (dolist (event events)
      (let ((type (gethash "type" event "")) (payload (%dash-payload event)))
        (cond ((member type '("memory-admission-accepted" "epistemic-admission-accepted") :test #'string=)
               (incf accepted)
               (dolist (entry (list (list origin "origin_class") (list status "epistemic_status")
                                    (list grounding "grounding_status") (list kind "kind")
                                    (list producer "producer")))
                 (incf (gethash (format nil "~a" (gethash (second entry) payload "unknown"))
                                (first entry) 0))))
              ((member type '("memory-admission-rejected" "epistemic-admission-rejected") :test #'string=)
               (incf rejected)))))
    (obj "accepted" accepted "rejected" rejected
         "grounded_rate" (if (zerop accepted) :null
                              (/ (gethash "grounded" grounding 0) (float accepted 1.0d0)))
         "by_origin" origin "by_status" status "by_grounding" grounding
         "by_kind" kind "by_producer" producer)))

(defun %dash-runtime-symbol-value (package-name symbol-name &optional default)
  (let* ((package (find-package package-name))
         (symbol (and package (find-symbol symbol-name package))))
    (if (and symbol (boundp symbol)) (symbol-value symbol) default)))

(defun %dash-call-report (package-name symbol-name &rest arguments)
  "Call an optional runtime report without creating a compile-time back-edge."
  (let* ((package (find-package package-name))
         (symbol (and package (find-symbol symbol-name package))))
    (if (and symbol (fboundp symbol))
        (apply (symbol-function symbol) arguments)
        :null)))

(defun %dashboard-session-budget-report ()
  "Read scalar accounting telemetry without waiting for the cognition lock.

The authoritative admission path remains lock-protected.  Dashboard reads are
observational and may span one accounting update, which is preferable to
blocking the web server behind an open-ended provider call."
  (let* ((cost-ceiling
           (%dash-runtime-symbol-value
            :agent "*CONSCIOUS-CONVERSATION-COST-CEILING-USD*" 0d0))
         (spent
           (%dash-runtime-symbol-value
            :agent "*CONSCIOUS-CONVERSATION-PROVIDER-SPENT-USD*" 0d0))
         (private-percent
           (%dash-runtime-symbol-value
            :agent "*CONSCIOUS-RECURSIVE-MIND-PRIVATE-BUDGET-PERCENT*" 0))
         (private-ceiling (* cost-ceiling (/ private-percent 100d0)))
         (private-spent
           (%dash-runtime-symbol-value
            :agent "*CONSCIOUS-CONVERSATION-PRIVATE-PROVIDER-SPENT-USD*" 0d0))
         (anomaly
           (%dash-runtime-symbol-value
            :agent
            "*CONSCIOUS-CONVERSATION-MOST-RECENT-ACCOUNTING-ANOMALY*")))
    (obj "schema_version" 3 "status" "ok"
         "snapshot_consistency" "non-blocking-observability"
         "request_attempts"
         (%dash-runtime-symbol-value
          :agent "*CONSCIOUS-CONVERSATION-PROVIDER-ATTEMPTS*" 0)
         "request_limit_enforced" nil "request_limit" :null
         "remaining_requests" :null
         "spent_usd" spent "cost_ceiling_usd" cost-ceiling
         "remaining_usd" (max 0d0 (- cost-ceiling spent))
         "private_budget_percent" private-percent
         "private_request_attempts"
         (%dash-runtime-symbol-value
          :agent "*CONSCIOUS-CONVERSATION-PRIVATE-PROVIDER-ATTEMPTS*" 0)
         "private_request_limit_enforced" nil
         "private_request_limit" :null "private_remaining_requests" :null
         "private_spent_usd" private-spent
         "private_cost_ceiling_usd" private-ceiling
         "private_remaining_usd" (max 0d0 (- private-ceiling private-spent))
         "accounting_uncertain"
         (if (%dash-runtime-symbol-value
              :agent
              "*CONSCIOUS-CONVERSATION-PROVIDER-BUDGET-UNCERTAIN-P*")
             t nil)
         "accounting_anomaly_count"
         (%dash-runtime-symbol-value
          :agent "*CONSCIOUS-CONVERSATION-ACCOUNTING-ANOMALY-COUNT*" 0)
         "pending_generation_settlement_count"
         (length
          (%dash-runtime-symbol-value
           :agent
           "*CONSCIOUS-CONVERSATION-PENDING-GENERATION-SETTLEMENTS*"
           nil))
         "most_recent_accounting_anomaly"
         (if (hash-table-p anomaly) anomaly :null))))

(defun %dashboard-runtime-parameters-report ()
  "Project effective live values only; command-line secrets are never read."
  (let* ((profile (%dash-runtime-symbol-value
                   :agent "*CONSCIOUS-CONVERSATION-PROVIDER-PROFILE*"))
         (selected (and (hash-table-p profile) profile))
         (capture-mode (%dash-runtime-symbol-value
                        :agent "*LLM-DEBUG-CAPTURE-MODE*" :off)))
    (obj
     "schema_version" 1 "source_of_truth" "event-sourced-runtime-settings-and-live-bound-values"
     "settings_revision"
     (%dash-runtime-symbol-value :agent "*RUNTIME-SETTINGS-REVISION*" :null)
     "captured_at" (get-universal-time)
     "model"
     (obj "provider" (if (hash-table-p profile)
                          (gethash "provider" profile :null) :null)
          "profile_id" (if (hash-table-p profile)
                             (gethash "profile_id" profile :null) :null)
          "model" (%dash-runtime-symbol-value
                     :agent "*CONSCIOUS-RECURSIVE-MIND-MODEL*" :null)
          "endpoint" (%dash-runtime-symbol-value
                        :agent "*CONSCIOUS-RECURSIVE-MIND-ENDPOINT*" :null)
          ;; Temperature is request-local for specialist cognition.  This is
          ;; only the ordinary solicited-turn default, not a global claim.
          "ordinary_turn_default_temperature" 0.3d0
          "max_output_tokens"
          (or (%dash-runtime-symbol-value
               :agent "*CONSCIOUS-CONVERSATION-MAX-OUTPUT-TOKENS*") :null)
          "provider_call_timeout_seconds"
          (%dash-runtime-symbol-value
           :agent "*CONSCIOUS-CONVERSATION-PROVIDER-CALL-TIMEOUT-SECONDS*"
           :null)
          "provider_streaming"
          (if (%dash-runtime-symbol-value
               :agent "*CONSCIOUS-CONVERSATION-PROVIDER-STREAMING-P*")
              t nil)
          "provider_connect_timeout_seconds"
          (%dash-runtime-symbol-value
           :agent "*CONSCIOUS-CONVERSATION-PROVIDER-CONNECT-TIMEOUT-SECONDS*"
           :null)
          "provider_inactivity_timeout_seconds"
          (%dash-runtime-symbol-value
           :agent
           "*CONSCIOUS-CONVERSATION-PROVIDER-INACTIVITY-TIMEOUT-SECONDS*"
           :null)
          "reasoning" (if (hash-table-p selected)
                            (or (gethash "reasoning" selected) "model-default")
                            :null)
          "provider_routing" (if (hash-table-p selected)
                                   (or (gethash "provider_routing" selected)
                                       :null)
                                   :null))
     "cognition"
     (obj "loop_mode" (%dash-runtime-symbol-value
                        :cl-user "*CONVERSATION-LOOP-MODE*" :null)
          "recursive_tools" (if (%dash-runtime-symbol-value
                                  :agent "*CONSCIOUS-RECURSIVE-MIND-TOOLS-ENABLED-P*")
                                 t nil)
          "private_reasoning_effort"
          (%dash-runtime-symbol-value
           :agent "*CONSCIOUS-RECURSIVE-MIND-PRIVATE-REASONING-EFFORT*" :null)
          "max_model_boundaries"
          (%dash-runtime-symbol-value
           :agent "*CONSCIOUS-RECURSIVE-MIND-MAX-MODEL-BOUNDARIES*" :null)
          "max_tool_boundaries"
          (%dash-runtime-symbol-value
           :agent "*CONSCIOUS-RECURSIVE-MIND-MAX-TOOL-BOUNDARIES*" :null)
          "max_tool_result_characters"
          (%dash-runtime-symbol-value
           :agent "*CONSCIOUS-RECURSIVE-MIND-MAX-TOOL-RESULT-CHARACTERS*" :null)
          "max_total_tool_result_characters"
          (%dash-runtime-symbol-value
           :agent "*CONSCIOUS-RECURSIVE-MIND-MAX-TOTAL-TOOL-RESULT-CHARACTERS*"
           :null)
          "runtime_revision"
          (%dash-runtime-symbol-value
           :agent "*CONSCIOUS-RECURSIVE-MIND-RUNTIME-REVISION*" :null))
     "continuous_life"
     (obj "wake_seconds" (%dash-runtime-symbol-value
                          :cl-user "*CONVERSATION-CURIOSITY-WAKE-SECONDS*" 0)
          "deliberate_curiosity" (if (%dash-runtime-symbol-value
                                      :cl-user "*CONVERSATION-DELIBERATE-CURIOSITY-P*")
                                     t nil)
          "reach_out" (if (%dash-runtime-symbol-value
                            :cl-user "*CONVERSATION-CURIOSITY-REACH-OUT-P*") t nil)
          "briefing" (if (%dash-runtime-symbol-value
                           :cl-user "*CONVERSATION-CURIOSITY-BRIEFING-P*") t nil)
          "consolidation" (if (%dash-runtime-symbol-value
                                :cl-user "*CONVERSATION-CURIOSITY-CONSOLIDATION-P*")
                               t nil)
          "episodic_memory" (if (%dash-runtime-symbol-value
                                  :cl-user "*CONVERSATION-EPISODIC-MEMORY-P*") t nil)
          "knowledge_graph_formation"
          (if (%dash-runtime-symbol-value
               :cl-user "*CONVERSATION-KNOWLEDGE-GRAPH-FORMATION-P*") t nil)
          "quiescent_reappraisal_seconds"
          (%dash-runtime-symbol-value
           :agent "*CONSCIOUS-RECURSIVE-CURIOSITY-QUIESCENT-REAPPRAISAL-SECONDS*"
           :null))
     "budgets"
     (%dashboard-session-budget-report)
     "observability"
     (obj "loop_trace" (%dash-runtime-symbol-value
                         :cl-user "*CONVERSATION-LOOP-TRACE-MODE*" :null)
          "context_trace" (string-downcase (symbol-name capture-mode))
          "context_trace_retention_seconds"
          (%dash-runtime-symbol-value
           :agent "*LLM-DEBUG-CAPTURE-RETENTION-SECONDS*" :null)
          "credential_redaction" t)
     "web"
     (obj "enabled" (if (%dash-runtime-symbol-value :agent "*ACCEPTOR*") t nil)
          "listen_address" (%dash-runtime-symbol-value
                             :agent "*WEB-LISTEN-ADDRESS*" :null)
          "authentication_configured"
          (if (eq t (%dash-call-report :agent
                                       "WEB-AUTHENTICATION-CONFIGURED-P"))
              t nil)))))

(defun %dashboard-public-progress-report ()
  (let* ((active (%dash-runtime-symbol-value
                  :cl-user "*CONVERSATION-PROGRESS-ACTIVE-P*" nil))
         (stage (%dash-runtime-symbol-value
                 :cl-user "*CONVERSATION-PROGRESS-STAGE*" nil))
         (started (%dash-runtime-symbol-value
                   :cl-user "*CONVERSATION-PROGRESS-STAGE-STARTED*" 0)))
    (obj "active" (if active t nil)
         "interaction_id" (or (%dash-runtime-symbol-value
                                :cl-user
                                "*CONVERSATION-PROGRESS-INTERACTION-ID*") :null)
         "stage" (or stage :null)
         "stage_age_seconds"
         (if (and active (numberp started) (plusp started))
             (/ (- (get-internal-real-time) started)
                (coerce internal-time-units-per-second 'double-float))
             0))))

(defun %dashboard-attention-report (&optional supplied-docket)
  (let* ((public (%dashboard-public-progress-report))
         (private (%dash-call-report :agent
                                     "CONSCIOUS-RECURSIVE-ATTENTION-INSPECT" 20))
         ;; The observability response also exposes this same projection at
         ;; top level.  Accept it from that caller so an exhaustive durable
         ;; docket replay happens once per refresh, not twice.
         (docket (or supplied-docket
                     (%dash-call-report :agent
                                        "CONSCIOUS-WORK-DOCKET-INSPECT" 64)))
         (docket-items (and (hash-table-p docket)
                            (gethash "items" docket)))
         (eligible-work
           (and (vectorp docket-items)
                (find-if
                 (lambda (row)
                   (and (member (gethash "state" row "")
                                '("active" "waiting") :test #'string=)
                        (<= (gethash "next_eligible_at" row most-positive-fixnum)
                            (get-universal-time))))
                 (coerce docket-items 'list))))
         (focuses (and (hash-table-p private) (gethash "focuses" private)))
         (pending (and (vectorp focuses)
                       (find "pending" focuses
                             :key (lambda (row) (gethash "status" row ""))
                             :test #'string=)))
         (state (cond ((eq t (gethash "active" public)) "operator-turn")
                      (pending "private-focus")
                      (eligible-work "maintained-work-ready")
                      ((hash-table-p private) "between-cognitive-cycles")
                      (t "runtime-unavailable")))
         (description
           (cond ((string= state "operator-turn")
                  (format nil "Handling an operator turn: ~a"
                          (gethash "stage" public "working")))
                 (pending
                  (format nil "Pursuing private focus: ~a"
                          (gethash "question" pending "unspecified focus")))
                 (eligible-work
                  (format nil "Maintained work is ready: ~a"
                          (gethash "title" eligible-work "unspecified work")))
                 ((string= state "between-cognitive-cycles")
                  "Waiting between continuous-life cycles; durable attention remains available.")
                 (t "The selected mind is not currently observable."))))
    (obj "schema_version" 1 "as_of" (get-universal-time)
         "state" state "description" description
         "public_turn" public "private_attention" private
         "work_docket" docket
         "current_focus" (or pending :null))))

(defparameter *dashboard-activity-event-types*
  '("model-request" "model-response" "recursive-tool-execution"
    "recursive-tool-result" "user-message" "agent-message"
    "conscious-curiosity-observed" "recursive-curiosity-focus-opened"
    "conscious-work-docket-opened"
    "conscious-work-docket-transitioned"
    "recursive-work-docket-focus-opened"
    "recursive-work-docket-result"
    "recursive-curiosity-focus-failed" "recursive-curiosity-result"
    "recursive-curiosity-attention-opened"
    "recursive-curiosity-attention-completed"
    "recursive-curiosity-attention-declined"
    "recursive-curiosity-attention-quiescent"
    "recursive-curiosity-result-review-opened"
    "recursive-curiosity-result-review-completed"
    "recursive-curiosity-incorporation-opened"
    "recursive-curiosity-incorporation-completed"
    "recursive-private-briefing-opened"
    "recursive-private-briefing-completed"
    "recursive-curiosity-consolidation-opened"
    "recursive-curiosity-consolidation-completed"
    "memory-admission-accepted" "memory-admission-rejected"))

(defun %dashboard-event-category (type)
  (cond ((string= type "model-request") "model_requests")
        ((string= type "model-response") "model_responses")
        ((string= type "recursive-tool-execution") "tool_calls")
        ((string= type "recursive-tool-result") "tool_results")
        ((string= type "user-message") "operator_messages")
        ((string= type "agent-message") "agent_messages")
        ((or (search "curiosity" type) (search "private" type)
             (search "work-docket" type))
         "private_cognition")
        ((search "memory-admission" type) "memory_writes")
        (t "other")))

(defun %dashboard-activity-report (events hours)
  (let* ((bucket-seconds (cond ((<= hours 1) 300)
                               ((<= hours 6) 900)
                               ((<= hours 24) 3600)
                               (t 21600)))
         (now (get-universal-time))
         (cutoff (- now (round (* hours 3600))))
         (first (* (floor cutoff bucket-seconds) bucket-seconds))
         (buckets (make-hash-table :test #'eql))
         (tools (make-hash-table :test #'equal))
         (models (make-hash-table :test #'equal)))
    (loop for start from first to now by bucket-seconds
          do (setf (gethash start buckets)
                   (obj "start" start "model_requests" 0 "model_responses" 0
                        "tool_calls" 0 "tool_results" 0
                        "operator_messages" 0 "agent_messages" 0
                        "private_cognition" 0 "memory_writes" 0 "other" 0)))
    (dolist (event events)
      (let* ((raw-timestamp (gethash "timestamp" event 0))
             ;; Durable events use ISO8601; focused projection fixtures may
             ;; supply universal time directly.
             (timestamp (if (numberp raw-timestamp)
                            raw-timestamp
                            (%event-parse-ts event)))
             (type (gethash "type" event ""))
             (payload (%dash-payload event))
             (start (and (plusp timestamp)
                         (* (floor timestamp bucket-seconds) bucket-seconds)))
             (bucket (and start (gethash start buckets))))
        (when bucket
          (incf (gethash (%dashboard-event-category type) bucket 0)))
        (when (string= type "recursive-tool-execution")
          (incf (gethash (gethash "tool_name" payload "unknown") tools 0)))
        (when (string= type "model-request")
          (incf (gethash (gethash "model" payload "unknown") models 0)))))
    (obj "schema_version" 1 "hours" hours
         "bucket_seconds" bucket-seconds
         "buckets" (coerce
                    (loop for start being the hash-keys of buckets
                            using (hash-value bucket)
                          collect bucket into rows
                          finally (return (sort rows #'< :key
                                                (lambda (row)
                                                  (gethash "start" row)))))
                    'vector)
         "tool_usage" tools "model_usage" models)))

(defparameter *dashboard-event-summary-keys*
  '("thread_id" "model_call_id" "tool_call_id" "tool_name" "model"
    "status" "runtime_revision" "protocol_revision" "tools_advertised"
    "final_synthesis" "reasoning_recovery" "content_persisted" "usage"
    "question" "subject_label" "motive_id" "source_motive_ids"
    "work_id" "title" "purpose" "operator_benefit" "next_step"
    "priority" "state" "next_eligible_at" "note" "work_revision"
    "focus_event_id" "register_revision" "page_revision" "decision"
    "disposition" "summary" "error_code" "reason" "observation_count"
    "thread_count" "opened_at" "completed_at"))

(defun %dashboard-event-project (event)
  (let ((payload (%dash-payload event)) (projected (obj)))
    (dolist (key *dashboard-event-summary-keys*)
      (multiple-value-bind (value present-p) (gethash key payload)
        (when present-p (setf (gethash key projected) value))))
    (obj "id" (gethash "id" event :null)
         "timestamp" (gethash "timestamp" event :null)
         "type" (gethash "type" event "unknown")
         "caused_by" (gethash "caused_by" event :null)
         "agent_id" (gethash "agent_id" event :null)
         "payload" projected)))

(defun %dashboard-observability-live-report ()
  "Return the process-local and durable attention state needed for first paint."
  (let ((docket (%dash-call-report :agent
                                   "CONSCIOUS-WORK-DOCKET-INSPECT" 64)))
    (obj "schema_version" 1 "as_of" (get-universal-time)
         "attention" (%dashboard-attention-report docket)
         "work_docket" docket
         "runtime_parameters" (%dashboard-runtime-parameters-report))))

(defun %dashboard-observability-history-report (hours)
  "Return the independently loadable bounded activity and event history."
  (let* ((bounded-hours (min 168 (max 1 hours)))
         (cutoff (- (get-universal-time) (* 3600 bounded-hours)))
         (events (replay-events :from cutoff :limit 5000
                                :types *dashboard-activity-event-types*)))
    (obj "schema_version" 1 "as_of" (get-universal-time)
         "activity" (%dashboard-activity-report events bounded-hours)
         "events" (coerce (mapcar #'%dashboard-event-project
                                  (last events (min 300 (length events))))
                          'vector))))

(defun %dashboard-observability-report (hours)
  "Retain the original combined report for API compatibility."
  (let ((live (%dashboard-observability-live-report))
        (history (%dashboard-observability-history-report hours)))
    (setf (gethash "activity" live) (gethash "activity" history)
          (gethash "events" live) (gethash "events" history))
    live))
(defun dashboard-report (&key (hours 1) type (limit 250) (all-limit 5000))
  ;; One streaming ledger pass supplies both the selected rows and aggregate
  ;; rows.  The previous two calls each reparsed the complete durable ledger
  ;; every 30-second browser poll, creating avoidable multi-GiB allocation
  ;; bursts as the ledger grew.
  (let* ((cutoff (- (get-universal-time) (* 3600 (max 0 hours))))
         ;; Model request/response bodies contain full prompts, tool schemas,
         ;; and provider payloads. They remain explicitly queryable by type,
         ;; but default aggregation must not materialize them every poll.
         (all (replay-events :from cutoff :limit (or all-limit 5000)
                             :exclude-types '("model-request"
                                              "model-response")))
         (selected-type (and type (plusp (length type))
                             (not (string= type "all")) type))
         (events (if selected-type
                     (replay-events :from cutoff :limit (or limit 250)
                                    :types (list selected-type))
                     (last all (min (or limit 250) (length all)))))
         (ticks (%dash-types
                 (remove-if-not
                  (lambda (e) (member (gethash "type" e)
                                      '("tick-end" "tick-terminal")
                                      :test #'string=)) all)))
         (tick-types (make-hash-table :test #'equal)) (funnel (make-hash-table :test #'equal)) (cost 0.0d0))
    (dolist (e all)
      (when (member (gethash "type" e) '("tick-end" "tick-terminal")
                    :test #'string=)
        (let ((p (gethash "payload" e)))
          (incf (gethash (gethash "type" p "unknown") tick-types 0))
          (incf cost (or (gethash "cost" p) 0.0d0)))))
    (dolist (e all)
      (when (string= (gethash "type" e) "initiative-decision")
        (let ((decision (gethash "decision" (gethash "payload" e) "unknown")))
          (incf (gethash "triggers" funnel 0))
          (incf (gethash decision funnel 0)))))
    (when (fboundp 'initiative-candidates)
      (dolist (candidate (initiative-candidates))
        (when (>= (gethash "created_at" candidate 0) (- (get-universal-time) (* 3600 hours)))
          (incf (gethash (format nil "status:~a" (gethash "status" candidate)) funnel 0)))))
    (obj "hours" hours "events" (coerce events 'vector) "event_types" (%dash-types all)
         "runtime_truth" (if (fboundp 'runtime-truth-manifest)
                               (runtime-truth-manifest) :null)
         "replay_capsules" (if (fboundp 'replay-capsule-report)
                                (replay-capsule-report) :null)
         "correlations" (dashboard-correlations all)
         "timing" (dashboard-timing-view all)
         "alerts" (dashboard-alerts all)
         "memory_health" (%dash-memory-health all)
         "tick_types" tick-types "initiative_funnel" funnel "tick_cost" cost
         "initiative_candidates"
         (if (fboundp 'initiative-candidates)
             (coerce (remove-if-not
                      (lambda (candidate)
                        (and (string= (gethash "kind" candidate) "share-thought")
                             (>= (gethash "created_at" candidate 0) (- (get-universal-time) (* 3600 hours)))))
                      (initiative-candidates)) 'vector)
             (vector))
         "memory_nodes" (if (fboundp '%memory-node-count) (ignore-errors (%memory-node-count)) :null)
         "heap" (if (fboundp 'heap-health-report) (heap-health-report) :null)
         "latent" (if (fboundp 'latent-thought-report) (latent-thought-report) :null)
         "latent_v2" (if (fboundp 'latent-v2-report) (latent-v2-report) :null)
         "legacy_audit" (if (fboundp 'legacy-memory-audit-report)
                            (legacy-memory-audit-report) :null)
         "stabilization_eval" (if (fboundp 'stabilization-eval-report)
                                  (stabilization-eval-report) :null)
         "grounded_agency" (if (fboundp 'grounded-agency-report)
                                (grounded-agency-report) :null)
         "feedback_loop" (if (fboundp 'feedback-loop-containment-report)
                              (feedback-loop-containment-report) :null)
         "conversation_context"
         (if (fboundp 'conversation-context-budget-report)
             (conversation-context-budget-report) :null)
         "reciprocity_canary"
         (if (fboundp 'reciprocity-canary-report)
             (reciprocity-canary-report) :null)
         "pull_reciprocity"
         (if (fboundp 'pull-reciprocity-report)
             (pull-reciprocity-report) :null)
         "near_term_workspace"
         (if (fboundp 'near-term-workspace-shadow-snapshot)
             (handler-case
                 (near-term-workspace-shadow-snapshot)
               (error (condition)
                 (obj "schema_version" 1 "mode" "dashboard-shadow"
                      "status" "unavailable" "active_items" 0
                      "items" (vector) "rejections" (vector)
                      "error_type"
                      (string-downcase
                       (symbol-name (type-of condition))))))
             :null)
         "initiative" (if (fboundp 'initiative-policy-report) (initiative-policy-report) :null))))

(defun %dashboard-browser-report (hours type detail)
  "Bound the browser's default response. Full correlations and timing traces
remain available only through DETAIL=full for deliberate operator inspection."
  (let* ((full-p (string-equal (or detail "") "full"))
         (report (dashboard-report :hours hours :type type
                                   :limit (if full-p 250 100)
                                   :all-limit (if full-p 5000 1000))))
    (unless full-p
      (setf (gethash "correlations" report) (obj))
      (let ((timing (gethash "timing" report)))
        (when (hash-table-p timing)
          (setf (gethash "traces" timing) (vector)))))
    report))

(defparameter *dashboard-html* "<!doctype html><html><head><meta charset='utf-8'><title>the agent · Dashboard</title><style>
body{margin:0;background:#10121a;color:#e8eaf0;font:14px system-ui;padding:24px}h1{margin:0 0 16px}.controls,.cards{display:flex;gap:10px;flex-wrap:wrap;margin:12px 0}button,select,input{background:#202535;color:#e8eaf0;border:1px solid #3a425b;border-radius:6px;padding:8px}.card{background:#181c28;border:1px solid #30384d;border-radius:9px;padding:12px;min-width:130px}.muted{color:#9ba5bf}.error{color:#ff9f9f}.bars{display:flex;gap:7px;align-items:end;height:125px}.bar{background:#6d8dff;min-width:28px;text-align:center;font-size:11px;padding-top:4px}.event{border-top:1px solid #2a3042;padding:9px 0}.type{color:#98b7ff}.time{color:#9ba5bf;font-size:12px}.payload{white-space:pre-wrap;word-break:break-word;color:#c6cada;margin-top:4px}#memory{width:300px}a{color:#98b7ff}</style></head><body><h1>the agent <span class='muted'>operator dashboard</span></h1><div class='controls'><select id='hours'><option value='.0833'>5 minutes</option><option value='.5'>30 minutes</option><option value='1' selected>1 hour</option><option value='6'>6 hours</option><option value='24'>24 hours</option></select><select id='type'><option value='all'>all event types</option></select><button id='refresh'>Refresh</button><input id='memory' placeholder='Memory search'><button id='search'>Recall</button></div><div id='cards' class='cards'></div><h3>Tick mix</h3><div id='bars' class='bars'></div><h3>Events</h3><div id='events'></div><h3>Memory recall</h3><div id='memout' class='payload muted'>Enter a query to inspect retrieved memory.</div><h3>Private LLM diagnostics</h3><div class='controls'><select id='debugfiles'><option>No capture files</option></select><button id='debugmeta'>Load metadata</button><button id='debugreveal'>Reveal private context</button></div><div class='muted'>Context is never loaded automatically. Reveal may contain private conversation.</div><pre id='debugout' class='payload'></pre><script>
const $=id=>document.getElementById(id), esc=s=>String(s??'').replace(/[&<>]/g,c=>({'&':'&amp;','<':'&lt;','>':'&gt;'}[c]));
function showStatus(message,error=false){let el=$('status');if(!el){el=document.createElement('div');el.id='status';$('cards').before(el)}el.className=error?'error':'muted';el.textContent=message}
async function json(url){let response=await fetch(url);if(!response.ok)throw new Error(`HTTP ${response.status}`);return response.json()}
function cards(d){let c=d.conversation_context||{},r=d.reciprocity_canary||{};$('cards').innerHTML=[['Events',d.events.length],['Memory nodes',d.memory_nodes??'—'],['Heap',d.heap?`${(100*d.heap.ratio).toFixed(1)}%`:'—'],['Tick cost','$'+(+d.tick_cost).toFixed(4)],['Context',c.candidate_records!=null?`${c.candidate_records} rec / ${c.candidate_chars} chars`:(c.status||'—')],['Context mode',c.mode||'—'],['Latent',d.latent?.records??0],['Open questions',d.feedback_loop?.projected_active_question_count??'—'],['Workspace',d.near_term_workspace?.active_items??'—'],['Canary mode',r.mode||'—'],['Would send',r.status_counts?.['would-send']??0],['Canary sent',r.status_counts?.sent??0],['Canary unanswered',r.open_unanswered??0]].map(x=>`<div class=card><div class=muted>${x[0]}</div><b>${x[1]}</b></div>`).join('')}
function reciprocity(r){let el=$('reciprocity');if(!el){let h=document.createElement('h3');h.textContent='Reciprocity canary';el=document.createElement('div');el.id='reciprocity';el.className='cards';$('bars').before(h,el)}if(!r||!r.mode){el.innerHTML='<span class=muted>Canary unavailable.</span>';return}let rows=r.records||[];el.innerHTML=`<div class=card><div class=muted>Mode / delivery</div><b>${esc(r.mode)} / ${r.delivery_capable?'capable':'blocked'}</b></div>`+(rows.length?rows.slice(0,8).map(x=>`<div class=card><div class=muted>${esc(x.status)} · ${esc(x.source)} · ${esc(x.reason)}</div><div>${esc(x.content_preview||'')}</div><div class=muted>${esc(x.topic||'')} · ${esc(x.initiative_decision_id||'')}</div></div>`).join(''):'<span class=muted>No canary observations yet.</span>')}
function workspace(w){let el=$('workspace');if(!el){let h=document.createElement('h3');h.innerHTML='Near-term workspace <span class=muted>bounded working state</span>';el=document.createElement('div');el.id='workspace';el.className='cards';$('bars').before(h,el)}if(!w||w.mode!=='dashboard-shadow'){el.innerHTML='<span class=muted>Workspace unavailable.</span>';return}let rows=w.items||[],sources=w.sources||[],errors=sources.filter(x=>x.status!=='ok'),latest=w.conversational_intention;el.innerHTML=rows.length?rows.map(x=>`<div class=card><div class=muted>${esc(x.item_type)} · ${esc(x.source)} · ${esc(x.state)}</div><div>${esc(x.summary)}</div>${x.artifact_summary?`<div class=muted>Result: ${esc(x.artifact_summary)}</div>`:''}${x.pass_count!=null?`<div class=muted>Passes: ${esc(x.pass_count)}/${esc(x.max_passes)} · ${esc(x.latest_transition||'')}</div>`:''}<div class=muted>${esc(x.id)}</div></div>`).join(''):'<span class=muted>No active items in the bounded workspace.</span>';if(latest&&!rows.some(x=>x.id===latest.id))el.innerHTML+=`<div class=card><div class=muted>latest conversational intention · ${esc(latest.state)}</div><div>${esc(latest.subject)}</div>${latest.artifact_summary?`<div class=muted>Result: ${esc(latest.artifact_summary)}</div>`:''}<div class=muted>Passes: ${esc(latest.pass_count)}/${esc(latest.max_passes)} · ${esc(latest.latest_transition||'')}${latest.failure_code?` · ${esc(latest.failure_code)}`:''}</div></div>`;if((w.rejections||[]).length||errors.length)el.innerHTML+=`<div class='card error'>${(w.rejections||[]).length} rejected event(s); ${errors.length} source error(s). Inspect API JSON for diagnostics.</div>`}
function governance(d){let el=$('governance');if(!el){let h=document.createElement('h3');h.textContent='Runtime governance';el=document.createElement('div');el.id='governance';el.className='cards';$('bars').before(h,el)}let o=d.runtime_truth?.public_outbound||{},p=d.replay_capsules||{},c=p.coverage||{},fmt=x=>Object.entries(x||{}).map(([k,v])=>`${k}:${v}`).join(' · ')||'none';el.innerHTML=[['Outbound audit',o.mode||'unavailable'],['Unclassified sends',o.unclassified??'unavailable'],['Would withhold',o.would_withhold??'unavailable'],['Replay capsules',p.records??'unavailable'],['Capsule worker',p.worker_alive?'alive':(p.autostart?'waiting':'off')],['Replay triggers',fmt(c.trigger_type)],['Replay fidelity',fmt(c.fidelity)],['Replay days',fmt(c.day)],['Replay outcomes',fmt(c.outcome)],['Replay bytes',`${p.file_bytes??'unavailable'} / ${p.disk_max_bytes??'unavailable'}`],['Replay pruned',fmt(p.pruning)]].map(x=>`<div class=card><div class=muted>${x[0]}</div><b>${esc(x[1])}</b></div>`).join('')}
function bars(o){let es=Object.entries(o||{}),m=Math.max(1,...es.map(x=>x[1]));$('bars').innerHTML=es.map(([k,v])=>`<div><div class=bar style='height:${18+90*v/m}px'>${v}</div><div class=muted>${esc(k)}</div></div>`).join('')}
function funnel(o){let el=$('funnel');if(!el){let h=document.createElement('h3');h.textContent='Initiative funnel';el=document.createElement('div');el.id='funnel';el.className='cards';$('bars').after(h,el)}let es=Object.entries(o||{});el.innerHTML=es.length?es.map(([k,v])=>`<div class=card><div class=muted>${esc(k.replace('status:',''))}</div><b>${v}</b></div>`).join(''):'<span class=muted>No initiative decisions in this window.</span>'}
function summary(e,candidates){let p=e.payload||{};if(e.type==='initiative-decision'){let c=candidates[p.candidate_id]||{};let thought=c.reason||c.provenance||p.topic||'(candidate content unavailable)';return `${thought.slice(0,260)} — ${p.decision||''}; ${p.reason||''}; value ${p.user_value??'—'}/${p.agent_outcome??'—'}`}if(p.text)return p.text.slice(0,180);if(p.content)return p.content.slice(0,180);return JSON.stringify(p).slice(0,220)}
function localTime(value){let d=new Date(value);return Number.isNaN(d.getTime())?value:d.toLocaleString(undefined,{dateStyle:'medium',timeStyle:'medium'})}
function events(a,candidates){$('events').innerHTML=`<table style='width:100%;border-collapse:collapse'><thead><tr><th align=left>Time</th><th align=left>Type</th><th align=left>Summary / candidate</th><th align=left>Raw</th></tr></thead><tbody>${a.slice().reverse().map(e=>{let p=e.payload||{},id=p.candidate_id||'';let c=candidates[id];return `<tr class=event><td class=time title='${esc(e.timestamp)}'>${esc(localTime(e.timestamp))}</td><td class=type>${esc(e.type)}</td><td>${esc(summary(e,candidates))}${id?`<div class=muted>candidate: ${esc(id)} · topic: ${esc(c?.topic||p.topic||'')}</div>`:''}</td><td><details><summary>JSON</summary><pre class=payload>${esc(JSON.stringify(p,null,2))}</pre></details></td></tr>`}).join('')}</tbody></table>`}
async function load(){showStatus('Loading dashboard data...');try{let h=$('hours').value,t=$('type').value,d=await json(`/api/dashboard/data?hours=${h}&type=${encodeURIComponent(t)}`);let rows=d.events||[],candidates=Object.fromEntries((d.initiative_candidates||[]).map(c=>[c.id,c]));cards({...d,events:rows});governance(d);workspace(d.near_term_workspace);reciprocity(d.reciprocity_canary);bars(d.tick_types);funnel(d.initiative_funnel);events(rows,candidates);let cur=$('type').value;$('type').innerHTML='<option value=all>all event types</option>'+Object.keys(d.event_types||{}).sort().map(x=>`<option ${x===cur?'selected':''}>${esc(x)}</option>`).join('');showStatus(`Loaded ${rows.length} events.`)}catch(error){showStatus(`Dashboard data failed: ${error.message}`,true)}}
$('refresh').onclick=load;$('hours').onchange=load;$('type').onchange=load;$('search').onclick=async()=>{let q=$('memory').value;if(!q)return;let d=await(await fetch('/api/dashboard/memory?q='+encodeURIComponent(q))).json();$('memout').textContent=JSON.stringify(d,null,2)};
async function debugIndex(){let d=await(await fetch('/api/dashboard/llm-debug')).json(),files=d.files||[];$('debugfiles').innerHTML=files.length?files.map(f=>`<option value='${esc(f.name)}'>${esc(f.name)} (${f.bytes} bytes)</option>`).join(''):`<option value=''>No capture files</option>`}
async function debugRead(reveal){let name=$('debugfiles').value;if(!name)return;$('debugout').textContent=reveal?'Loading private contextâ€¦':'Loading metadataâ€¦';let d=await(await fetch(`/api/dashboard/llm-debug/read?name=${encodeURIComponent(name)}&reveal=${reveal?'true':'false'}`)).json();$('debugout').textContent=JSON.stringify(d,null,2)}
$('debugmeta').onclick=()=>debugRead(false);$('debugreveal').onclick=()=>debugRead(true);load();debugIndex();setInterval(load,30000);
</script></body></html>")

(defun %dashboard-html-replace-one (text old new)
  (let ((position (search old text)))
    (unless position (error "Dashboard HTML insertion point is absent."))
    (concatenate 'string (subseq text 0 position) new
                 (subseq text (+ position (length old))))))

(setf *dashboard-html*
      (%dashboard-html-replace-one
       *dashboard-html* "<h3>Memory recall</h3>"
       "<h3>Turn trace explorer</h3><div class='controls'><input id='traceid' placeholder='Exact turn ID'><button id='loadtrace'>Load trace</button></div><div class='muted'>Loaded only on request. The projection excludes prompts, messages, tool arguments, response bodies and error text.</div><div id='traceviz'></div><pre id='traceout' class='payload'>Select Trace on a correlated event or enter an exact turn ID.</pre><h3>Memory recall</h3>"))

(setf *dashboard-html*
      (%dashboard-html-replace-one
       *dashboard-html* "<td><details><summary>JSON</summary>"
       "<td>${p.turn_id&&/^[A-Za-z0-9_.:-]{1,160}$/.test(p.turn_id)?`<button onclick='traceTurn(${JSON.stringify(p.turn_id)})'>Trace</button>`:''}<details><summary>JSON</summary>"))

(setf *dashboard-html*
      (%dashboard-html-replace-one
       *dashboard-html* "async function debugIndex()"
       "function renderDashboardTrace(d){let duration=Math.max(1,+d.duration_ms),attempts=d.provider_attempts||[],repeated=d.repeated_tools||[];$('traceviz').innerHTML=`<h4>Waterfall</h4>${(d.spans||[]).map(s=>`<div style='margin:5px 0'><span class=muted>${esc(s.name)} - ${(+s.duration_ms).toFixed(1)} ms</span><div style='height:8px;margin-left:${Math.min(90,100*(+s.start_offset_ms)/duration)}%;width:${Math.max(1,Math.min(100,100*(+s.duration_ms)/duration))}%;background:#6d8dff;border-radius:4px'></div></div>`).join('')}<h4>Physical provider attempts</h4>${attempts.length?`<table><tr><th>Provider / model</th><th>Status</th><th>Duration</th><th>Tokens</th><th>Cost</th></tr>${attempts.map(a=>`<tr><td>${esc(a.provider)} / ${esc(a.model)}</td><td>${esc(a.status)}</td><td>${esc(a.duration_ms)} ms</td><td>${esc(a.total_tokens)}</td><td>$${(+a.cost_usd||0).toFixed(6)}</td></tr>`).join('')}</table>`:'<div class=muted>Historical physical attempts are not exactly linked.</div>'}${repeated.length?'<h4>Repeated tools</h4>'+repeated.map(x=>`<div class=error>${esc(x.tool)} x ${x.count}</div>`).join(''):''}`;}async function traceTurn(id){$('traceid').value=id;$('traceout').textContent='Loading exact trace...';try{let h=Math.max(1,Math.ceil(+$('hours').value)),d=await json(`/api/dashboard/turn-trace?turn_id=${encodeURIComponent(id)}&hours=${h}`),t=d.totals||{},g=d.growth||{};renderDashboardTrace(d);$('traceout').textContent=`Duration ${(+d.duration_ms).toFixed(1)} ms - first output ${d.first_public_output_ms} ms - ${g.public_model_call_count} model calls - ${t.provider_attempt_total_tokens} physical tokens - $${(+t.provider_attempt_cost_usd).toFixed(6)}\n\n${JSON.stringify(d,null,2)}`}catch(e){$('traceout').textContent=e.message}}$('loadtrace').onclick=()=>traceTurn($('traceid').value);async function debugIndex()"))

(defparameter *observability-dashboard-html*
  (asset "../../adapters/web/assets/observability.html"))
(defparameter *observability-dashboard-js*
  (asset "../../adapters/web/assets/observability.js"))

(hunchentoot:define-easy-handler (dashboard-page :uri "/dashboard") ()
  (setf (hunchentoot:content-type*) "text/html; charset=utf-8")
  *observability-dashboard-html*)
(hunchentoot:define-easy-handler (dashboard-script :uri "/observability.js") ()
  (setf (hunchentoot:content-type*) "text/javascript; charset=utf-8")
  *observability-dashboard-js*)
(hunchentoot:define-easy-handler (dashboard-data :uri "/api/dashboard/data") (hours type detail)
  (setf (hunchentoot:content-type*) "application/json; charset=utf-8")
  (multiple-value-bind (encoded valid-p attempts)
      (%dash-json-validated
       (lambda ()
         (%dashboard-browser-report (%dash-hours hours) (or type "all") detail)))
    (declare (ignore attempts))
    (unless valid-p (setf (hunchentoot:return-code*) 503))
    encoded))
(hunchentoot:define-easy-handler
    (dashboard-observability :uri "/api/dashboard/observability") (hours)
  (setf (hunchentoot:content-type*) "application/json; charset=utf-8"
        (hunchentoot:header-out "Cache-Control") "no-store")
  (handler-case
      (%dash-json (%dashboard-observability-report
                   (min 168 (max 1 (%dash-hours hours)))))
    (error (condition)
      (setf (hunchentoot:return-code*) 503)
      (%dash-json (obj "error" (princ-to-string condition))))))
(hunchentoot:define-easy-handler
    (dashboard-observability-live :uri "/api/dashboard/observability/live") ()
  (setf (hunchentoot:content-type*) "application/json; charset=utf-8"
        (hunchentoot:header-out "Cache-Control") "no-store")
  (handler-case
      (%dash-json (%dashboard-observability-live-report))
    (error (condition)
      (setf (hunchentoot:return-code*) 503)
      (%dash-json (obj "error" (princ-to-string condition))))))
(hunchentoot:define-easy-handler
    (dashboard-observability-history :uri "/api/dashboard/observability/history")
    (hours)
  (setf (hunchentoot:content-type*) "application/json; charset=utf-8"
        (hunchentoot:header-out "Cache-Control") "no-store")
  (handler-case
      (%dash-json (%dashboard-observability-history-report
                   (min 168 (max 1 (%dash-hours hours)))))
    (error (condition)
      (setf (hunchentoot:return-code*) 503)
      (%dash-json (obj "error" (princ-to-string condition))))))
(hunchentoot:define-easy-handler
    (dashboard-model-context :uri "/api/dashboard/model-context")
    (model_call_id)
  (setf (hunchentoot:content-type*) "application/json; charset=utf-8"
        (hunchentoot:header-out "Cache-Control") "no-store")
  (handler-case
      (%dash-json
       (if (fboundp 'llm-debug-capture-find-model-call)
           (llm-debug-capture-find-model-call model_call_id :reveal t)
           (obj "status" "unavailable"
                "reason" "context-capture-reader-unavailable")))
    (error (condition)
      (setf (hunchentoot:return-code*) 400)
      (%dash-json (obj "error" (princ-to-string condition))))))
(hunchentoot:define-easy-handler
    (dashboard-turn-trace :uri "/api/dashboard/turn-trace")
    (turn_id trace_id hours)
  (setf (hunchentoot:content-type*) "application/json; charset=utf-8"
        (hunchentoot:header-out "Cache-Control") "no-store")
  (handler-case
      (%dash-json
       (dashboard-turn-trace-report
        :turn-id (and turn_id (plusp (length turn_id)) turn_id)
        :trace-id (and trace_id (plusp (length trace_id)) trace_id)
        :hours (min 168 (max 1 (%dash-hours hours)))))
    (error (condition)
      (setf (hunchentoot:return-code*) 400)
      (%dash-json (obj "error" (princ-to-string condition))))))
(hunchentoot:define-easy-handler (dashboard-memory :uri "/api/dashboard/memory") (q)
  (setf (hunchentoot:content-type*) "application/json; charset=utf-8")
  (%dash-json (obj "query" (or q "") "results" (if (and q (plusp (length q))) (coerce (memory-recall q :k 8 :debug t) 'vector) (vector)))))
(hunchentoot:define-easy-handler (dashboard-llm-debug :uri "/api/dashboard/llm-debug") ()
  (setf (hunchentoot:content-type*) "application/json; charset=utf-8")
  (%dash-json
   (obj "enabled" (if (and (boundp '*llm-debug-capture-mode*)
                            (member *llm-debug-capture-mode*
                                    '(:metadata :full :on))) t nil)
        "mode" (if (boundp '*llm-debug-capture-mode*)
                   (string-downcase
                    (symbol-name *llm-debug-capture-mode*))
                   "off")
        "reveal_enabled" (if *llm-debug-dashboard-reveal-p* t nil)
        "files" (if (fboundp 'llm-debug-capture-index)
                    (llm-debug-capture-index) (vector)))))
(hunchentoot:define-easy-handler
    (dashboard-llm-debug-read :uri "/api/dashboard/llm-debug/read")
    (name reveal)
  (setf (hunchentoot:content-type*) "application/json; charset=utf-8")
  (handler-case
      (let ((requested (string-equal (or reveal "") "true")))
        (when (and requested (not *llm-debug-dashboard-reveal-p*))
          (error "Private debug reveal is disabled"))
        (%dash-json
         (obj "name" (or name "") "revealed" (if requested t nil)
              "records"
              (if (fboundp 'llm-debug-capture-read)
                  (llm-debug-capture-read name :reveal requested)
                  (vector)))))
    (error (condition)
      (setf (hunchentoot:return-code*) 404)
      (%dash-json (obj "error" (princ-to-string condition))))))
