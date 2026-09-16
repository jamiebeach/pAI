;;;; conscious-latency-qualification-addendum.lisp -- scaling and invalidation evidence.

(in-package :cl-user)

(load (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname)))
(push #P"/pai/" asdf:*central-registry*)
(let ((*standard-output* (make-broadcast-stream)))
  (asdf:load-system :pai))

(in-package :agent)

(defun %clqa-ms (thunk)
  (let ((started (get-internal-real-time)))
    (values (funcall thunk)
            (* 1000.0d0
               (/ (- (get-internal-real-time) started)
                  internal-time-units-per-second)))))

(defun %clqa-percentile (samples fraction)
  (let* ((ordered (sort (copy-list samples) #'<))
         (index (min (1- (length ordered))
                     (floor (* fraction (length ordered))))))
    (nth index ordered)))

(defun %clqa-summary (samples)
  (obj "iterations" (length samples)
       "minimum_ms" (reduce #'min samples)
       "p50_ms" (%clqa-percentile samples 0.50d0)
       "p95_ms" (%clqa-percentile samples 0.95d0)
       "maximum_ms" (reduce #'max samples)))

(defun %clqa-sample (count thunk)
  (let ((samples nil) (last-value nil))
    (dotimes (index count)
      (declare (ignore index))
      (multiple-value-bind (value elapsed) (%clqa-ms thunk)
        (setf last-value value)
        (push elapsed samples)))
    (values (%clqa-summary samples) last-value)))

(defun %clqa-profile ()
  (obj "profile_id" "latency-qualification" "revision" 1
       "max_model_calls" 8 "max_tool_operations" 6
       "max_reasoning_continuations" 3
       "max_tool_result_characters" 12000
       "permitted_proposal_kinds"
       (vector "tool-call-proposal" "publication-candidate"
               "request-continuation" "yield" "abstain")
       "permitted_tools" (vector "search-files")
       "budget_exhaustion" "suspend" "renewal_policy" "explicit-only"))

(defun %clqa-work-events (count agent-id)
  (let ((events nil) (event-id 0))
    (dotimes (index count (nreverse events))
      (let ((work-id (format nil "work:latency:~d" index)))
        (incf event-id)
        (push
         (obj "id" event-id "timestamp" (write-to-string event-id)
              "type" "conscious-work-opened"
              "agent_id" agent-id "payload"
              (obj "schema_version" 1 "work_id" work-id
                   "concern_identity" (format nil "concern:latency:~d" index)
                   "stimulus_ids" (vector (format nil "stimulus:~d" event-id))
                   "purpose" "qualification" "priority_class" "ambient"
                   "urgency_class" "background" "deadline" :null
                   "opened_at" event-id "profile" (%clqa-profile)))
         events)
        (incf event-id)
        (push
         (obj "id" event-id "timestamp" (write-to-string event-id)
              "type" "conscious-work-failed"
              "agent_id" agent-id "payload"
              (obj "schema_version" 1 "work_id" work-id
                   "reason_code" "qualification-terminal"))
         events)))))

(defun %clqa-turn-events (after-id agent-id)
  (let ((work-id "work:latency:turn"))
    (labels ((event (offset type payload)
               (let ((id (+ after-id offset)))
                 (obj "id" id "timestamp" (write-to-string id)
                      "type" type "agent_id" agent-id "payload" payload))))
      (list
       (event
        1 "conscious-work-opened"
        (obj "schema_version" 1 "work_id" work-id
             "concern_identity" "concern:latency:turn"
             "stimulus_ids" (vector "stimulus:latency:turn")
             "purpose" "qualification" "priority_class" "direct"
             "urgency_class" "interactive" "deadline" :null
             "opened_at" (1+ after-id) "profile" (%clqa-profile)))
       (event 2 "model-request"
              (obj "work_id" work-id "pulse_id" "latency:pulse:1"))
       (event
        3 "pulse-committed"
        (obj "work_id" work-id "pulse_id" "latency:pulse:1"
             "pulse_sequence" 1 "proposals"
             (vector (obj "proposal_id" "latency:proposal:tool"
                          "kind" "tool-call-proposal"))))
       (event
        4 "conscious-tool-operation-result"
        (obj "work_id" work-id "proposal_id" "latency:proposal:tool"
             "operation_id" "latency:operation:tool"
             "result" (obj "schema_version" 1 "status" "ok"
                           "matches" (vector) "database_write_count" 0)))
       (event 5 "model-request"
              (obj "work_id" work-id "pulse_id" "latency:pulse:2"))
       (event
        6 "pulse-committed"
        (obj "work_id" work-id "pulse_id" "latency:pulse:2"
             "pulse_sequence" 2 "proposals"
             (vector (obj "proposal_id" "latency:proposal:reply"
                          "kind" "publication-candidate"))))
       (event 7 "conscious-work-completed"
              (obj "work_id" work-id "reason_code" "reply-committed"))))))

(defun %clqa-delete-sqlite (path)
  (dolist (candidate
            (list path
                  (pathname (concatenate 'string (namestring path) "-wal"))
                  (pathname (concatenate 'string (namestring path) "-shm"))))
    (when (probe-file candidate) (delete-file candidate))))

(defun %clqa-work-database (path count agent-id)
  (%clqa-delete-sqlite path)
  (let ((backend (make-sqlite-storage path))
        (events
          (append (%clqa-work-events count agent-id)
                  (%clqa-turn-events (* 2 count) agent-id))))
    (%sqlite-in-transaction
     backend :latency-work-fixture
     (lambda (handle)
       (%with-sqlite-statement
           (statement handle
                      "INSERT INTO pai_events(event_id,storage_origin,agent_id,partition_status,event_type,occurred_at,event_json,integrity_hash) VALUES(?1,'native',?2,'verified',?3,?4,?5,?6)"
                      :latency-work-fixture)
         (dolist (event events)
           (let ((json (%storage-json event)))
             (%sqlite-bind-int64 handle statement 1 (gethash "id" event)
                                 :latency-work-fixture)
             (%sqlite-bind-text handle statement 2 agent-id
                                :latency-work-fixture)
             (%sqlite-bind-text handle statement 3 (gethash "type" event)
                                :latency-work-fixture)
             (%sqlite-bind-text handle statement 4 (gethash "timestamp" event)
                                :latency-work-fixture)
             (%sqlite-bind-text handle statement 5 json :latency-work-fixture)
             (%sqlite-bind-text handle statement 6 (%storage-sha256 json)
                                :latency-work-fixture)
             (%sqlite-step handle statement :latency-work-fixture +sqlite-done+)
             (%derived-reset-statement statement))))))
    backend))

(defun %clqa-runtime-turn-mode
    (depth work-backend tail-enabled-p)
  (let* ((agent-id "latency-qualification-agent")
         (base-head (* 2 depth))
         (turn-events (%clqa-turn-events base-head agent-id))
         (current-head base-head)
         (replays 0)
         (elapsed nil)
         (original-replay (symbol-function 'replay-events))
         (held nil) (held-json nil) (final nil))
    (unwind-protect
         (progn
           (setf (symbol-function 'replay-events)
                 (lambda (&key types &allow-other-keys)
                   (incf replays)
                   (storage-query-events
                    work-backend :agent-id agent-id
                    :through-id current-head :event-types types)))
           (let ((*agent-id* agent-id))
             (conscious-work-runtime-configure-head-position
              (lambda () current-head)
              (and tail-enabled-p
                   (lambda (after-position through-position event-types)
                     (let ((tail nil))
                       (multiple-value-bind (complete ignored count)
                           (storage-map-event-receipts
                            work-backend
                            (lambda (receipt)
                              (push (shasht:read-json
                                     (gethash "event_json" receipt))
                                    tail))
                            :agent-id agent-id
                            :after-position after-position
                            :through-position through-position
                            :event-types event-types)
                         (declare (ignore ignored count))
                         (unless complete
                           (error "Turn-shaped tail read is incomplete"))
                         (values (nreverse tail) through-position))))))
             (setf *conscious-work-runtime-projection-cache-hits* 0
                   *conscious-work-runtime-projection-cache-misses* 0
                   *conscious-work-runtime-projection-cache-rebuilds* 0
                   *conscious-work-runtime-projection-cache-advances* 0
                   *conscious-work-runtime-projection-cache-fallbacks* 0
                   *conscious-work-runtime-projection-cache-tail-events* 0)
             (setf held (%conscious-work-runtime-project-shared)
                   held-json (%conscious-work-canonical-json held))
             (dolist (event turn-events)
               (setf current-head (gethash "id" event))
               (multiple-value-bind (ignored milliseconds)
                   (%clqa-ms
                    (lambda ()
                      (conscious-work-select
                       (%conscious-work-runtime-project-shared))))
                 (declare (ignore ignored))
                 (push milliseconds elapsed)))
             (setf final (conscious-work-runtime-project))
             (obj
              "mode" (if tail-enabled-p "physical-tail" "full-replay")
              "changed_head_reads" (length turn-events)
              "cumulative_ms" (reduce #'+ elapsed)
              "per_boundary" (%clqa-summary elapsed)
              "authority_replays" replays
              "cache_hits" *conscious-work-runtime-projection-cache-hits*
              "cache_misses" *conscious-work-runtime-projection-cache-misses*
              "full_rebuilds"
              *conscious-work-runtime-projection-cache-rebuilds*
              "incremental_advances"
              *conscious-work-runtime-projection-cache-advances*
              "incremental_fallbacks"
              *conscious-work-runtime-projection-cache-fallbacks*
              "tail_events"
              *conscious-work-runtime-projection-cache-tail-events*
              "held_generation_unchanged"
              (string= held-json (%conscious-work-canonical-json held))
              "final_projection_fingerprint"
              (%conscious-work-canonical-json final))))
      (conscious-work-runtime-configure-head-position nil)
      (setf (symbol-function 'replay-events) original-replay))))

(defun %clqa-turn-comparison (depth work-backend)
  (let* ((full (%clqa-runtime-turn-mode depth work-backend nil))
         (tail (%clqa-runtime-turn-mode depth work-backend t))
         (identity
           (string= (gethash "final_projection_fingerprint" full)
                    (gethash "final_projection_fingerprint" tail))))
    (remhash "final_projection_fingerprint" full)
    (remhash "final_projection_fingerprint" tail)
    (obj "full_replay" full "physical_tail" tail
         "final_projection_identity" identity)))

(defun %clqa-work-depth (depth work-backend)
  (let* ((agent-id "latency-qualification-agent")
         (events (%clqa-work-events depth agent-id))
         (projection (conscious-work-project events agent-id))
         (projection-json (shasht:write-json projection nil))
         (original-replay (and (fboundp 'replay-events)
                               (symbol-function 'replay-events)))
         (runtime-summary nil)
         (runtime-hits 0)
         (runtime-misses 0)
         (shared-summary :null)
         (shared-selection-summary :null)
         (runtime-ignored nil))
    (declare (ignore runtime-ignored))
    (unwind-protect
         (progn
           (setf (symbol-function 'replay-events)
                 (lambda (&key types &allow-other-keys)
                   (declare (ignore types)) events))
           (let ((*agent-id* agent-id))
             (conscious-work-runtime-configure-head-position
              (lambda () (* 2 depth)))
             (setf *conscious-work-runtime-projection-cache-hits* 0
                   *conscious-work-runtime-projection-cache-misses* 0)
             ;; Construct the cached generation once, then measure actual hits.
             (conscious-work-runtime-project)
             (multiple-value-setq (runtime-summary runtime-ignored)
               (%clqa-sample 21 #'conscious-work-runtime-project))
             (when (fboundp '%conscious-work-runtime-project-shared)
               (multiple-value-setq (shared-summary runtime-ignored)
                 (%clqa-sample
                  21 #'%conscious-work-runtime-project-shared))
               (multiple-value-setq
                   (shared-selection-summary runtime-ignored)
                 (%clqa-sample
                  21
                  (lambda ()
                    (conscious-work-select
                     (%conscious-work-runtime-project-shared))))))
             (setf runtime-hits *conscious-work-runtime-projection-cache-hits*
                   runtime-misses *conscious-work-runtime-projection-cache-misses*)))
      (conscious-work-runtime-configure-head-position nil)
      (if original-replay
          (setf (symbol-function 'replay-events) original-replay)
          (fmakunbound 'replay-events)))
    (let ((project-summary nil) (serialize-summary nil) (parse-summary nil)
          (selection-summary nil) (replay-summary nil)
          (replay-project-summary nil) (ignored nil))
      (declare (ignore ignored))
      (multiple-value-setq (project-summary ignored)
        (%clqa-sample 11 (lambda () (conscious-work-project events agent-id))))
      (multiple-value-setq (serialize-summary ignored)
        (%clqa-sample
         11 (lambda ()
              (shasht:write-json
               (conscious-work-project events agent-id) nil))))
      (multiple-value-setq (parse-summary ignored)
        (%clqa-sample 21 (lambda () (shasht:read-json projection-json))))
      (multiple-value-setq (selection-summary ignored)
        (%clqa-sample 21 (lambda () (conscious-work-select projection))))
      (multiple-value-setq (replay-summary ignored)
        (%clqa-sample
         11
         (lambda ()
           (storage-query-events
            work-backend :agent-id agent-id :through-id (* 2 depth)
            :event-types *conscious-work-runtime-event-types*))))
      (multiple-value-setq (replay-project-summary ignored)
        (%clqa-sample
         11
         (lambda ()
           (conscious-work-project
            (storage-query-events
             work-backend :agent-id agent-id :through-id (* 2 depth)
             :event-types *conscious-work-runtime-event-types*)
            agent-id))))
      (obj "completed_items" depth "event_count" (* 2 depth)
           "projection_json_bytes"
           (length (babel:string-to-octets projection-json :encoding :utf-8))
           "project" project-summary
           "project_and_serialize" serialize-summary
           "json_parse" parse-summary
           "selection" selection-summary
           "sqlite_replay" replay-summary
           "sqlite_replay_and_project" replay-project-summary
           "runtime_public_detached" runtime-summary
           "runtime_shared_hit" shared-summary
           "runtime_shared_selection" shared-selection-summary
           "runtime_cache_hits" runtime-hits
           "runtime_cache_misses" runtime-misses
           "turn_shaped_changed_heads"
           (%clqa-turn-comparison depth work-backend)))))

(defun %clqa-result-fingerprint (report)
  (let ((digest (ironclad:make-digest :sha256)))
    (loop for candidate across (gethash "results" report)
          do (ironclad:update-digest
              digest
              (babel:string-to-octets
               (format nil "~a|~,17g~%"
                       (gethash "id" candidate)
                       (gethash "distance" candidate))
               :encoding :utf-8)))
    (ironclad:byte-array-to-hex-string (ironclad:produce-digest digest))))

(defun %clqa-first-node (backend)
  (let ((handle (%sqlite-derived-handle backend :latency-qualification)))
    (%with-sqlite-statement
        (statement handle
                   "SELECT scalar_json,embedding,retrieval_embedding FROM pai_memory_nodes ORDER BY source_ordinal LIMIT 1"
                   :latency-qualification)
      (%sqlite-step handle statement :latency-qualification +sqlite-row+)
      (values (%sqlite-column-text statement 0)
              (%derived-octets-hex (%sqlite-column-blob statement 1))
              (%derived-octets-hex (%sqlite-column-blob statement 2))))))

(defun %clqa-noop-mutation (backend)
  (let* ((report (memory-storage-projection-report backend))
         (event-id (1+ (gethash "through_event_id" report)))
         (position (1+ (gethash "through_storage_position" report))))
    (multiple-value-bind (scalar embedding retrieval) (%clqa-first-node backend)
      (let* ((payload
               (obj "operation" "update"
                    "mutation_kind" "latency-qualification-noop"
                    "scalar_json" scalar
                    "embedding_binary_hex" embedding
                    "retrieval_embedding_binary_hex" retrieval))
             (event
               (obj "schema_version" 1 "id" event-id
                    "agent_id" (gethash "agent_id" report)
                    "timestamp" "disposable-latency-qualification"
                    "type" "memory-node-state" "payload" payload
                    "caused_by" :null "tick_id" :null
                    "affect_snapshot" :null)))
        (make-memory-storage-mutation
         :event-json (%storage-json event)
         :storage-position position
         :storage-id (gethash "storage_id" report))))))

(defun %clqa-invalidation (database query-vector)
  (let ((backend nil))
    (unwind-protect
         (progn
           (setf backend (make-sqlite-derived-storage database))
           (let* ((query
                    (make-memory-exact-query
                     :vector-values query-vector
                     :profile "safe-semantic-v1" :limit 50 :hydrate-p t))
                  (before-build nil) (before-build-ms nil)
                  (warm-summary nil) (ignored nil)
                  (receipt nil) (mutation-ms nil)
                  (after-build nil) (after-build-ms nil)
                  (after-warm-summary nil))
             (declare (ignore ignored))
             (multiple-value-setq (before-build before-build-ms)
               (%clqa-ms
                (lambda () (memory-storage-exact-search backend query))))
             (multiple-value-setq (warm-summary ignored)
               (%clqa-sample
                7 (lambda () (memory-storage-exact-search backend query))))
             (multiple-value-setq (receipt mutation-ms)
               (%clqa-ms
                (lambda ()
                  (memory-storage-apply-mutation
                   backend (%clqa-noop-mutation backend)))))
             (multiple-value-setq (after-build after-build-ms)
               (%clqa-ms
                (lambda () (memory-storage-exact-search backend query))))
             (multiple-value-setq (after-warm-summary ignored)
               (%clqa-sample
                7 (lambda () (memory-storage-exact-search backend query))))
             (obj
              "initial_build_ms" before-build-ms
              "warm_before_mutation" warm-summary
              "mutation_status" (gethash "status" receipt)
              "mutation_ms" mutation-ms
              "post_mutation_search_ms" after-build-ms
              "warm_after_mutation" after-warm-summary
              "result_identity_preserved"
              (string= (%clqa-result-fingerprint before-build)
                       (%clqa-result-fingerprint after-build))
              "exact_cache_builds" (%sqlite-derived-exact-cache-builds backend)
              "exact_cache_hits" (%sqlite-derived-exact-cache-hits backend)
              "incremental_advances"
              (%sqlite-derived-exact-cache-incremental-advances backend)
              "incremental_fallbacks"
              (%sqlite-derived-exact-cache-incremental-fallbacks backend))))
      (when backend (ignore-errors (storage-close backend))))))

(let* ((database
         (pathname (or (uiop:getenv "PAI_LATENCY_BENCHMARK_DATABASE")
                       (error "PAI_LATENCY_BENCHMARK_DATABASE is required"))))
       (output
         (pathname (or (uiop:getenv "PAI_LATENCY_BENCHMARK_OUTPUT")
                       (error "PAI_LATENCY_BENCHMARK_OUTPUT is required"))))
       (work-database
         (pathname (or (uiop:getenv "PAI_LATENCY_WORK_DATABASE")
                       (error "PAI_LATENCY_WORK_DATABASE is required"))))
       (query "What relevant context should be remembered for this conversation?")
       (embedding-samples nil)
       (query-vector nil)
       (work-backend nil))
  (setf *embedding-fallback-policy* :error)
  (dotimes (index 20)
    (declare (ignore index))
    (multiple-value-bind (vector elapsed)
        (%clqa-ms (lambda () (embed-retrieval-query query)))
      (setf query-vector vector)
      (push elapsed embedding-samples)))
  (unwind-protect
       (progn
         (let ((result
                 (obj
                  "schema_version" 3
                  "benchmark" "conscious-latency-qualification-addendum-v3"
                  "source_revision"
                  (or (uiop:getenv "PAI_LATENCY_SOURCE_REVISION") "working-tree")
                  "embedding_20" (%clqa-summary embedding-samples)
                  "work_projection_depths"
                  (coerce
                   (mapcar (lambda (depth)
                             (when work-backend
                               (storage-close work-backend))
                             (setf work-backend
                                   (%clqa-work-database
                                    work-database depth
                                    "latency-qualification-agent"))
                             (%clqa-work-depth depth work-backend))
                           '(200 2000 10000))
                   'vector)
                  "exact_cache_invalidation"
                  (%clqa-invalidation database query-vector)
                  "scheduler_handoff"
                  (obj "status" "requires-live-turn"
                       "reason" "isolated execution cannot reproduce admission-to-worker and reply-to-selection scheduling"))))
           (ensure-directories-exist output)
           (with-open-file
               (stream output :direction :output :if-exists :supersede
                              :if-does-not-exist :create
                              :external-format :utf-8)
             (write-string (shasht:write-json result nil) stream)
             (terpri stream))
           (format t "Latency qualification addendum written to ~a.~%" output)))
    (when work-backend (ignore-errors (storage-close work-backend)))))
