;;;; stabilization-baseline.lisp -- read-only scorecard.
;;;;
;;;; Run in a separate SBCL process with SHASHT and POSTMODERN loaded. It reads
;;;; events.jsonl and memory_nodes, never calls a model and never writes the agent's
;;;; state. The only write is the requested result artifact.

(in-package :agent)

(export '(run-stabilization-baseline))

(defparameter *baseline-metric-version* "1.1.0")
(defparameter *baseline-default-hours* 24)
(defparameter *baseline-v1-snapshot-event-id* 100)
(defparameter *baseline-v1-snapshot-end-sql* "2030-01-01 00:00:00+00")

(defun %baseline-read-events (path)
  (with-open-file (in path :external-format :utf-8)
    (loop for line = (read-line in nil nil)
          while line
          when (plusp (length (string-trim '(#\Space #\Tab #\Return) line)))
            collect (handler-case (shasht:read-json line) (error () nil)) into rows
          finally (return (remove nil rows)))))

(defun %baseline-event-time (event)
  (handler-case
      (let ((s (gethash "timestamp" event)))
        (encode-universal-time
         (parse-integer s :start 17 :end 19)
         (parse-integer s :start 14 :end 16)
         (parse-integer s :start 11 :end 13)
         (parse-integer s :start 8 :end 10)
         (parse-integer s :start 5 :end 7)
         (parse-integer s :start 0 :end 4) 0))
    (error () 0)))

(defun %baseline-percentile (values fraction)
  (if (null values)
      :null
      (let* ((sorted (sort (copy-list values) #'<))
             (index (floor (* fraction (1- (length sorted))))))
        (nth index sorted))))

(defun %baseline-mean (values)
  (if values (/ (reduce #'+ values) (float (length values) 1.0d0)) :null))

(defun %baseline-count-by (items key-fn)
  (let ((counts (obj)))
    (dolist (item items counts)
      (let ((key (or (funcall key-fn item) "unknown")))
        (incf (gethash key counts 0))))))

(defun %baseline-event-metrics (events hours snapshot-event-id)
  (let* ((frozen-events
           (if snapshot-event-id
               (remove-if (lambda (e)
                            (> (or (gethash "id" e) most-positive-fixnum)
                               snapshot-event-id))
                          events)
               events))
         (latest (reduce #'max frozen-events :key #'%baseline-event-time :initial-value 0))
         (cutoff (- latest (* hours 3600)))
         (window (remove-if (lambda (e) (< (%baseline-event-time e) cutoff)) frozen-events))
         (users (make-hash-table :test #'eql))
         (latencies nil)
         (turn-character-ratios nil)
         (question-turns 0)
         (premature-closes 0)
         (tick-starts (make-hash-table :test #'eql))
         (tick-terminal-ids (make-hash-table :test #'eql))
         (tick-cost 0.0d0)
         (initiative-candidate-count 0)
         (initiative-decisions nil))
    (dolist (event window)
      (let ((type (gethash "type" event)))
        (cond
          ((string= type "user-message")
           (setf (gethash (gethash "id" event) users) event))
          ((string= type "agent-message")
           (let* ((cause (gethash "caused_by" event))
                  (user (and (integerp cause) (gethash cause users))))
             (when user
               (let* ((seconds (- (%baseline-event-time event) (%baseline-event-time user)))
                      (user-text (gethash "text" (gethash "payload" user) ""))
                      (agent-text (gethash "text" (gethash "payload" event) "")))
                 (when (>= seconds 0) (push (float seconds 1.0d0) latencies))
                 (when (and (stringp user-text) (plusp (length user-text)) (stringp agent-text))
                   (push (/ (length agent-text) (float (length user-text) 1.0d0)) turn-character-ratios)
                   (when (find #\? agent-text) (incf question-turns))
                   (let ((lower (string-downcase agent-text)))
                     (when (some (lambda (phrase) (search phrase lower))
                                 '("i'm here whenever" "i'll be here"
                                   "whenever you need" "go do that"))
                       (incf premature-closes))))))))
          ((string= type "tick-start")
           (setf (gethash (gethash "id" event) tick-starts) event))
          ((or (string= type "tick-end") (string= type "tick-terminal"))
           (let ((cause (gethash "caused_by" event)))
             (when (integerp cause) (setf (gethash cause tick-terminal-ids) t)))
           (incf tick-cost (or (gethash "cost" (gethash "payload" event)) 0)))
          ((string= type "initiative-candidate-scored")
           (incf initiative-candidate-count))
          ((string= type "initiative-decision") (push event initiative-decisions)))))
    (let ((missing-ticks 0))
      (maphash (lambda (id event) (declare (ignore event))
                 (unless (gethash id tick-terminal-ids) (incf missing-ticks)))
               tick-starts)
      (obj
       "window_hours" hours
       "snapshot_event_id" (or snapshot-event-id :null)
       "latest_event_timestamp" (if window (gethash "timestamp" (car (last window))) :null)
       "event_count" (length window)
       "event_types" (%baseline-count-by window (lambda (e) (gethash "type" e)))
       "responsiveness"
       (obj "paired_turns" (length latencies)
            "latency_seconds_p50" (%baseline-percentile latencies 0.50d0)
            "latency_seconds_p95" (%baseline-percentile latencies 0.95d0)
            "latency_seconds_max" (if latencies (reduce #'max latencies) :null)
            "agent_user_character_ratio_median" (%baseline-percentile turn-character-ratios 0.50d0))
       "reciprocity_proxies"
       (obj "paired_turns" (length latencies)
            "question_turn_rate" (if latencies (/ question-turns (float (length latencies) 1.0d0)) :null)
            "premature_close_rate" (if latencies (/ premature-closes (float (length latencies) 1.0d0)) :null)
            "fixed_rubric_status" "pending-v1-online-evaluation")
       "reliability"
       (obj "tick_starts" (hash-table-count tick-starts)
            "tick_terminals" (hash-table-count tick-terminal-ids)
            "tick_missing_terminal" missing-ticks
            "tick_missing_terminal_rate"
            (if (plusp (hash-table-count tick-starts))
                (/ missing-ticks (float (hash-table-count tick-starts) 1.0d0))
                :null))
       "cost" (obj "tick_cost_usd" tick-cost)
       "initiative"
       (obj "candidate_count" initiative-candidate-count
            "decision_count" (length initiative-decisions)
            "distinct_topic_count"
            (length (remove-duplicates
                     (remove nil (mapcar (lambda (e)
                                           (gethash "topic" (gethash "payload" e)))
                                         initiative-decisions))
                     :test #'string=))
            "execute_now_count"
            (count-if (lambda (e)
                        (string= (or (gethash "decision" (gethash "payload" e)) "")
                                 "execute-now"))
                      initiative-decisions)
            "decisions_by_outcome"
            (%baseline-count-by initiative-decisions
                                (lambda (e) (gethash "decision" (gethash "payload" e)))))))))

(defun %baseline-pg-config ()
  (list (or (uiop:getenv "PAI_PG_DATABASE") "pai_memory")
        (or (uiop:getenv "PAI_PG_USER") "pai")
        (or (uiop:getenv "PAI_PG_PASSWORD") "pai_local_dev_only")
        (or (uiop:getenv "PAI_PG_HOST") "pai-postgres")
        :port (parse-integer (or (uiop:getenv "PAI_PG_PORT") "5432"))))

(defun %baseline-find-root (parent id)
  (let ((p (gethash id parent id)))
    (if (equal p id) id
        (setf (gethash id parent) (%baseline-find-root parent p)))))

(defun %baseline-union (parent a b)
  (let ((ra (%baseline-find-root parent a)) (rb (%baseline-find-root parent b)))
    (unless (equal ra rb) (setf (gethash rb parent) ra))))

(defun %baseline-memory-kind-metrics (kind ids similar-pairs)
  (let ((parent (make-hash-table :test #'equal))
        (duplicate-ids (make-hash-table :test #'equal))
        (near-pairs 0))
    (dolist (id ids) (setf (gethash id parent) id))
    (dolist (pair similar-pairs)
      (destructuring-bind (a pair-kind b similarity) pair
        (when (string= pair-kind kind)
          (%baseline-union parent a b)
          (when (>= similarity 0.90d0)
            (incf near-pairs)
            (setf (gethash a duplicate-ids) t (gethash b duplicate-ids) t)))))
    (let ((clusters (make-hash-table :test #'equal)))
      (dolist (id ids)
        (incf (gethash (%baseline-find-root parent id) clusters 0)))
      (let* ((n (length ids))
             (cluster-count (hash-table-count clusters))
             ;; Hash-table iteration order is implementation-dependent.  Sort
             ;; cluster sizes before summing so a frozen snapshot serializes
             ;; identically across fresh SBCL processes.
             (cluster-sizes
               (sort (loop for size being the hash-values of clusters
                           collect size)
                     #'<))
             (entropy
               (if (or (zerop n) (<= cluster-count 1))
                   0.0d0
                   (/ (- (loop for size in cluster-sizes
                               for p = (/ size (float n 1.0d0))
                               sum (* p (log p))))
                      (log (float cluster-count 1.0d0))))))
        (obj "node_count" n
             "near_duplicate_pairs" near-pairs
             "near_duplicate_node_rate" (if (plusp n) (/ (hash-table-count duplicate-ids) (float n 1.0d0)) :null)
             "semantic_topic_clusters" cluster-count
             "normalized_topic_entropy" entropy)))))

(defun %baseline-memory-metrics (hours snapshot-end-sql)
  (handler-case
      (pomo:with-connection (%baseline-pg-config)
        (let* ((hours-int (max 1 (round hours)))
               (nodes (pomo:query
                       (format nil "SELECT id, kind FROM memory_nodes WHERE created_at >= timestamptz '~a' - interval '~a hours' AND created_at <= timestamptz '~a' AND kind IN ('thought','reflection','prediction','worldview','observation','episode')"
                               snapshot-end-sql hours-int snapshot-end-sql)))
               (pairs (pomo:query
                       (format nil "SELECT a.id, a.kind, b.id, 1 - (a.embedding <=> b.embedding) AS similarity FROM memory_nodes a JOIN memory_nodes b ON a.kind=b.kind AND a.id < b.id WHERE a.created_at >= timestamptz '~a' - interval '~a hours' AND b.created_at >= timestamptz '~a' - interval '~a hours' AND a.created_at <= timestamptz '~a' AND b.created_at <= timestamptz '~a' AND a.kind IN ('thought','reflection','prediction','worldview','observation','episode') AND 1 - (a.embedding <=> b.embedding) >= 0.78"
                               snapshot-end-sql hours-int snapshot-end-sql hours-int
                               snapshot-end-sql snapshot-end-sql)))
               (by-kind (make-hash-table :test #'equal))
               (provenance-present
                 (pomo:query
                  "SELECT EXISTS (SELECT 1 FROM information_schema.columns WHERE table_name='memory_nodes' AND column_name='origin_class')"
                  :single))
               (epistemic
                 (if provenance-present
                     (destructuring-bind
                         (typed generated ungrounded rootless laundering quarantined)
                         (pomo:query
                          (format nil "SELECT
                             count(*) FILTER (WHERE origin_class <> 'legacy-unclassified'),
                             count(*) FILTER (WHERE origin_class = 'synthetic'),
                             count(*) FILTER (WHERE origin_class = 'synthetic' AND grounding_status NOT IN ('grounded','partially-grounded')),
                             count(*) FILTER (WHERE origin_class = 'synthetic' AND jsonb_array_length(root_observation_ids) = 0),
                             count(*) FILTER (WHERE origin_class = 'synthetic' AND epistemic_status IN ('direct-event','user-report','agent-action')),
                             count(*) FILTER (WHERE quarantined)
                           FROM memory_nodes
                           WHERE created_at >= timestamptz '~a' - interval '~a hours'
                             AND created_at <= timestamptz '~a'"
                                  snapshot-end-sql hours-int snapshot-end-sql)
                          :row)
                       (obj "status" "available"
                            "typed_node_count" typed
                            "synthetic_node_count" generated
                            "ungrounded_admission" ungrounded
                            "rootless_generated_admission" rootless
                            "synthetic_evidence_laundering" laundering
                            "quarantined_node_count" quarantined))
                     (obj "status" "unavailable-pre-STAB-01"
                          "typed_node_count" :null
                          "synthetic_node_count" :null
                          "ungrounded_admission" :null
                          "rootless_generated_admission" :null
                          "synthetic_evidence_laundering" :null
                          "quarantined_node_count" :null))))
          (dolist (row nodes) (push (first row) (gethash (second row) by-kind)))
          (let ((result (obj)))
            (dolist (kind '("thought" "reflection" "prediction" "worldview" "observation" "episode"))
              (setf (gethash kind result)
                    (%baseline-memory-kind-metrics kind (gethash kind by-kind) pairs)))
            (obj "status" "ok"
                 "variation_by_kind" result
                 "epistemic" epistemic
                 "grounded_rate" :null
                 "grounded_rate_note" "unavailable before provenance"
                 "root_combination_diversity" :null
                 "root_combination_note" "unavailable before provenance"))))
    (error (e)
      (obj "status" "unavailable" "error" (format nil "~a" e)))))

(defun %baseline-canonical-scorecard (event-metrics memory-metrics)
  "Always emit every standard dimension. Metrics unavailable before later
STAB packages are explicit nulls rather than silently absent fields."
  (let* ((variation (gethash "variation_by_kind" memory-metrics))
         (epistemic (gethash "epistemic" memory-metrics))
         (worldview (and variation (gethash "worldview" variation)))
         (reciprocity (gethash "reciprocity_proxies" event-metrics))
         (initiative (gethash "initiative" event-metrics)))
    (obj
     "thought_variation"
     (obj "variation_by_kind" (or variation :null)
          "distinct_lived_root_combinations" :null
          "accepted_novel_thoughts_per_model_call" :null)
     "worldview_development"
     (obj "raw_worldview_variation" (or worldview :null)
          "grounded_distinct_nodes" :null
          "median_novelty_distance" :null
          "distinct_root_coverage" :null
          "supersession_evolution_rate" :null
          "unsupported_rejection_rate" :null)
     "reciprocity"
     (obj "fixed_rubric_score" :null
          "question_turn_rate" (gethash "question_turn_rate" reciprocity :null)
          "premature_close_rate" (gethash "premature_close_rate" reciprocity :null)
          "agent_user_character_ratio_median"
          (gethash "agent_user_character_ratio_median"
                   (gethash "responsiveness" event-metrics) :null)
          "status" "fixed online evaluator pending")
     "outreach"
     (obj "candidate_count" (gethash "candidate_count" initiative 0)
          "decision_count" (gethash "decision_count" initiative 0)
          "execute_now_count" (gethash "execute_now_count" initiative 0)
          "distinct_topic_count" (gethash "distinct_topic_count" initiative 0)
          "labelled_precision" :null "labelled_recall" :null
          "false_outreach_rate" :null "missed_high_value_rate" :null
          "same_topic_repeat_rate" :null "unanswered_followup_rate" :null)
     "memory"
     (obj "labelled_retrieval_precision" :null "labelled_retrieval_recall" :null
          "grounded_result_rate" :null "source_diversity" :null
          "quarantined_leakage" :null "complete_turn_capture" :null)
     "epistemic_safety"
     (obj "ungrounded_admission"
          (if epistemic (gethash "ungrounded_admission" epistemic :null) :null)
          "rootless_generated_admission"
          (if epistemic
              (gethash "rootless_generated_admission" epistemic :null)
              :null)
          "identity_swap" :null
          "unsupported_temporal_claim" :null
          "synthetic_evidence_laundering"
          (if epistemic
              (gethash "synthetic_evidence_laundering" epistemic :null)
              :null)
          "malformed_output_escape" :null)
     "responsiveness" (gethash "responsiveness" event-metrics)
     "reliability_cost"
     (obj "reliability" (gethash "reliability" event-metrics)
          "cost" (gethash "cost" event-metrics)
          "model_calls" :null "db_calls" :null "fallback_rate" :null))))

(defun run-stabilization-baseline (&key
                                     (events-file #P"/agent/state/events.jsonl")
                                     (output-file #P"/agent/state/evals/results/current-baseline.json")
                                     (hours *baseline-default-hours*)
                                     (snapshot-event-id *baseline-v1-snapshot-event-id*)
                                     (snapshot-end-sql *baseline-v1-snapshot-end-sql*)
                                     (change-id (or (uiop:getenv "BASELINE_CHANGE_ID")
                                                    "STAB-00-pre-instrumentation"))
                                     (source-revision (or (uiop:getenv "BASELINE_SOURCE_REVISION")
                                                          "unknown")))
  "Create a read-only, machine-readable stabilization scorecard."
  (let* ((events (%baseline-read-events events-file))
         (event-metrics (%baseline-event-metrics events hours snapshot-event-id))
         (memory-metrics (%baseline-memory-metrics hours snapshot-end-sql))
         (result
           (obj "schema_version" 1
                "benchmark_version" "1.0.0"
                "metric_version" *baseline-metric-version*
                "state_snapshot" (format nil "event-~a@~a"
                                         snapshot-event-id snapshot-end-sql)
                "change_id" change-id
                "source_revision" source-revision
                "generated_at_universal" (get-universal-time)
                "events" event-metrics
                "memory" memory-metrics
                "scorecard" (%baseline-canonical-scorecard event-metrics memory-metrics)
                "epistemic_safety" (gethash "epistemic" memory-metrics))))
    (ensure-directories-exist output-file)
    (with-open-file (out output-file :direction :output :if-exists :supersede
                                  :if-does-not-exist :create :external-format :utf-8)
      (let ((*print-pretty* t)) (write-string (shasht:write-json result nil) out))
      (terpri out))
    (format t "~&baseline scorecard written to ~a (~a frozen-window events).~%"
            output-file (gethash "event_count" event-metrics))
    result))
