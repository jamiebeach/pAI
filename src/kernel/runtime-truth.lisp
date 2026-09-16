;;;; runtime-truth.lisp -- sanitized report of effective runtime controls.

(in-package :agent)

(export '(runtime-truth-manifest runtime-transport-inventory
          runtime-truth-assert runtime-truth-deployed-modes-table
          runtime-nonreply-transport-source-audit
          runtime-nonreply-transport-source-assert
          runtime-truth-declare-authorities))

(defvar *stabilization-config-file* nil)
(defvar *conversation-context-config-file* nil)
(defvar *wrap-chains* nil)
(defvar *tool-dispatch-runtime-wrapper*)
(declaim (ftype function runtime-truth-manifest))

(defparameter *runtime-truth-mode-specs*
  '(("epistemic_memory" *epistemic-memory-mode* "legacy")
    ("cognitive_generation" *cognitive-generation-mode* "legacy")
    ("context_projection" *context-projection-mode* "legacy")
    ("temporal_response_policy" *temporal-response-policy-mode* "legacy")
    ("initiative_policy" *initiative-policy-mode* "legacy")
    ("initiative_delivery" *initiative-delivery-mode* "shadow")
    ("latent_thoughts" *latent-thoughts-mode* "legacy")
    ("epistemic_critic" *epistemic-critic-mode* "off")
    ("autonomous_write" *autonomous-write-mode* "normal")
    ("grounded_agency" *grounded-agency-mode* "legacy")
    ("near_term_intentions" *near-term-intentions-mode* "off")
    ("conversation_context_budget" *conversation-context-budget-mode* "enforced")
    ("reciprocity_canary" *reciprocity-canary-mode* "shadow")))

(defparameter *runtime-truth-expected-final-owners*
  '(("transport" "telegram-send" "public-outbound-gateway.lisp"
     pai-base-telegram-send-public-outbound)
    ("turn" "auto-turn" "observability-tracing.lisp" pai-base-auto-turn-timing)
    ("tool" "execute" "dynamic"
     %near-term-intention-tool-execute)
    ("memory" "memory-recall" "observability-tracing.lisp"
     pai-base-memory-recall-timing)
    ("initiative" "%drives-event-initiate" "observability-tracing.lisp"
     pai-base-drives-event-initiate-timing)
    ("persistence" "%conv-persist-write" "observability-tracing.lisp"
     pai-base-conv-persist-write-timing)))

(defparameter *runtime-truth-telegram-call-files*
  '("telegram.lisp" "candidate-policy.lisp" "drives.lisp" "scheduler.lisp"
    "turn-watchdog.lisp" "workout_nudge.lisp"))

(defparameter *runtime-truth-parameter-specs*
  '(("tick.base_interval_seconds" *tick-base-interval-seconds* 120)
    ("tick.minimum_interval_seconds" *tick-min-interval-seconds* 60)
    ("tick.maximum_interval_seconds" *tick-max-interval-seconds* 2700)
    ("tick.maximum_per_hour" *tick-max-per-hour* 30)
    ("tick.soft_daily_cost_usd" *tick-budget-soft-daily* 1.0)
    ("tick.hard_daily_cost_usd" *tick-budget-hard-daily* 3.0)
    ("context.target_records" *conversation-context-target-records* 60)
    ("context.minimum_recent_records" *conversation-context-min-recent-records* 24)
    ("context.hard_records" *conversation-context-hard-records* 100)
    ("context.target_estimated_tokens" *conversation-context-target-tokens* 100000)
    ("context.hard_estimated_tokens" *conversation-context-hard-tokens* 160000)
    ("context.estimated_characters_per_token"
     *conversation-context-estimated-chars-per-token* 4)
    ("context.target_characters" *conversation-context-target-chars* 400000)
    ("context.hard_characters" *conversation-context-hard-chars* 640000)
    ("context.continuity_brief_characters" *conversation-context-brief-chars* 4000)
    ("context.tool_result_characters" *conversation-context-tool-result-chars* 6000)
    ("memory.tool_result_characters" *turn-capture-tool-memory-max-chars* 4000)
    ("tools.state_file_timeout_seconds" *state-file-search-timeout-seconds* 5)
    ("tools.web_fetch_timeout_seconds" *web-fetch-timeout-seconds* 15)
    ("tools.web_fetch_characters" *web-fetch-max-chars* 30000)
    ("tools.deliverable_read_characters" *deliverable-read-max-chars* 20000)
    ("tools.deliverable_write_characters" *deliverable-write-max-chars* 100000)
    ("outbound.record_cap" *public-outbound-record-cap* 500)
    ("reciprocity.record_cap" *reciprocity-canary-max-records* 200)
    ("reciprocity.maximum_per_24_hours" *reciprocity-canary-max-per-24-hours* 2)
    ("reciprocity.minimum_spacing_seconds" *reciprocity-canary-min-spacing-seconds* 21600)
    ("reciprocity.topic_block_seconds" *reciprocity-canary-topic-block-seconds* 86400)
    ("replay.record_cap" *replay-capsule-record-cap* 500)
    ("replay.retention_seconds" *replay-capsule-retention-seconds* 1209600)
    ("replay.scheduled_interval_seconds" *replay-capsule-scheduled-interval-seconds* 1800)
    ("replay.scheduled_max_per_day" *replay-capsule-scheduled-max-per-day* 48)
    ("replay.event_max_per_day" *replay-capsule-event-max-per-day* 96)
    ("replay.manual_max_per_day" *replay-capsule-manual-max-per-day* 12)
    ("replay.record_max_bytes" *replay-capsule-record-max-bytes* 32768)
    ("replay.shard_max_bytes" *replay-capsule-shard-max-bytes* 8388608)
    ("replay.disk_max_bytes" *replay-capsule-disk-max-bytes* 8388608)
    ("replay.event_queue_cap" *replay-capsule-event-queue-cap* 256)
    ("replay.worker_poll_seconds" *replay-capsule-worker-poll-seconds* 1)
    ("scheduler.context_record_cap" *pai-scheduler-max-context-records* 100)
    ("pull.maximum_raw_records" *pull-reciprocity-max-raw* 20)
    ("pull.pairs_per_batch" *pull-reciprocity-pairs-per-batch* 10)
    ("pull.batch_cap" *pull-reciprocity-max-batches* 100)
    ("pull.description_max_per_24_hours"
     *pull-reciprocity-description-max-per-24-hours* 2)
    ("heap.sample_interval_seconds" *heap-health-interval-seconds* 300)
    ("heap.sample_cap" *heap-health-sample-cap* 288)))

(defparameter *runtime-truth-context-parameter-keys*
  '(("context.target_records" . "target_records")
    ("context.minimum_recent_records" . "minimum_recent_records")
    ("context.hard_records" . "hard_records")
    ("context.target_estimated_tokens" . "target_estimated_tokens")
    ("context.hard_estimated_tokens" . "hard_estimated_tokens")
    ("context.target_characters" . "target_chars")
    ("context.hard_characters" . "hard_chars")
    ("context.continuity_brief_characters" . "brief_chars")
    ("context.tool_result_characters" . "tool_result_chars")))

(defun %runtime-truth-value (symbol &optional default)
  (if (boundp symbol) (symbol-value symbol) default))

(defun %runtime-truth-report (symbol default &rest args)
  "Read an optional report from a higher layer, or DEFAULT when it is absent.

The function twin of %RUNTIME-TRUTH-VALUE. The truth report summarises
subsystems that may or may not be loaded, so every such read was already
guarded by FBOUNDP -- but written in head position, which links this file
against the callee at compile time even though the guard says it may not
exist. Two of those callees are in the publication layer, and were the last
compile-time edges from the authority layer up into cognition.

DEFAULT matters: :NULL is a truthy value in Lisp, so a caller that tests the
result for presence must pass NIL, not :NULL."
  (if (fboundp symbol) (apply symbol args) default))

(defun %runtime-truth-mode-string (value)
  (cond ((keywordp value) (string-downcase (symbol-name value)))
        ((symbolp value) (string-downcase (symbol-name value)))
        ((stringp value) (string-downcase value))
        (t :null)))

(defun %runtime-truth-persisted-object ()
  (if (and (boundp '*stabilization-config-file*)
           *stabilization-config-file* (probe-file *stabilization-config-file*))
      (handler-case
          (values (shasht:read-json
                   (uiop:read-file-string *stabilization-config-file*))
                  "available")
        (error () (values nil "unreadable")))
      (values nil "unavailable")))

(defun %runtime-truth-mode-rows ()
  (multiple-value-bind (persisted persisted-status)
      (%runtime-truth-persisted-object)
    (coerce
     (mapcar
      (lambda (spec)
        (destructuring-bind (name symbol default) spec
          (let* ((live (%runtime-truth-mode-string
                        (%runtime-truth-value symbol :unavailable)))
                 (stored (and persisted (gethash name persisted)))
                 (persisted-value (or stored :null))
                 (drift (cond ((not (string= persisted-status "available"))
                               "unverified")
                              ((null stored) "persisted-missing")
                              ((and (stringp live)
                                    (string= live
                                             (%runtime-truth-mode-string stored)))
                               "match")
                              (t "mismatch"))))
            (obj "name" name "source_default" default
                 "persisted" persisted-value "live" live
                 "persisted_status" persisted-status "drift" drift))))
      *runtime-truth-mode-specs*)
     'vector)))

(defun %runtime-truth-context-persisted-object ()
  (if (and (boundp '*conversation-context-config-file*)
           *conversation-context-config-file*
           (probe-file *conversation-context-config-file*))
      (handler-case
          (values (shasht:read-json
                   (uiop:read-file-string *conversation-context-config-file*))
                  "available")
        (error () (values nil "unreadable")))
      (values nil "unavailable")))

(defun %runtime-truth-parameter-rows ()
  (multiple-value-bind (context-persisted context-status)
      (%runtime-truth-context-persisted-object)
    (coerce
     (mapcar
      (lambda (spec)
        (destructuring-bind (name symbol default) spec
          (let* ((live (%runtime-truth-value symbol :unavailable))
                 (context-key (cdr (assoc name
                                          *runtime-truth-context-parameter-keys*
                                          :test #'string=)))
                 (stored (and context-key context-persisted
                              (gethash context-key context-persisted)))
                 (persisted-status (if context-key context-status
                                       "not-applicable"))
                 (drift
                   (cond ((eq live :unavailable) "unavailable")
                         ((not context-key)
                          (if (equalp live default) "match" "runtime-override"))
                         ((not (string= context-status "available")) "unverified")
                         ((null stored) "persisted-missing")
                         ((equalp live stored) "match")
                         (t "mismatch"))))
            (obj "name" name "source_default" default
                 "persisted" (or stored :null)
                 "persisted_status" persisted-status "live" live
                 "drift" drift))))
      *runtime-truth-parameter-specs*)
     'vector)))

(defun runtime-transport-inventory ()
  "Declared public origins. Gateway visibility is explicit, including dormant paths."
  (vector
   (obj "owner" "telegram-reactive" "file" "telegram.lisp" "kind" "reply"
        "proof" "telegram-update-id" "visibility" "origin-bound/final-wrapper")
   (obj "owner" "telegram-reactive-error" "file" "telegram.lisp"
        "kind" "system-alert" "proof" "telegram-update-id")
   (obj "owner" "web-reactive" "file" "web-terminal.lisp" "kind" "reply"
        "proof" "web-request-id" "visibility" "presentation-wrapper")
   (obj "owner" "terminal-reactive" "file" "event-log.lisp" "kind" "reply"
        "proof" "user-event-id" "visibility" "completion-observer")
   (obj "owner" "candidate-policy" "file" "candidate-policy.lisp"
        "kind" "initiative" "proof" "candidate-and-v2-decision-id")
   (obj "owner" "candidate-policy-commitment" "file" "candidate-policy.lisp"
        "kind" "commitment" "proof" "commitment-receipt-id")
   (obj "owner" "legacy-drives" "file" "drives.lisp" "kind" "initiative"
        "proof" "legacy-candidate-or-event-id")
   (obj "owner" "scheduler" "file" "scheduler.lisp" "kind" "scheduled"
        "proof" "schedule-job-id")
   (obj "owner" "workout-nudge" "file" "workout_nudge.lisp" "kind" "scheduled"
        "proof" "stable-job-and-fire-id")
   (obj "owner" "turn-watchdog" "file" "turn-watchdog.lisp"
        "kind" "system-alert" "proof" "active-turn-alert-id")
   (obj "owner" "telegram-poll-legacy" "file" "telegram.lisp"
        "kind" "initiative" "proof" "pai-maybe-initiate"
        "status" "dormant-return-path")
   (obj "owner" "public-tool-presentation"
        "file" "agent_loop.lisp,event-log.lisp,web-terminal.lisp"
        "kind" "tool-result"
        "proof" "tool-call-id+tool-result-id+active-inbound-request")))

(defun %runtime-truth-thread (name symbol &key autostart owner)
  (let ((value (%runtime-truth-value symbol nil)))
    (obj "name" name "bound" (boundp symbol)
         "alive" (and value (ignore-errors (bt:thread-alive-p value)))
         "autostart" (if autostart t nil) "autostart_owner" owner)))

(defun %runtime-truth-conversation-usage ()
  (let* ((history (%runtime-truth-value '*last-self-mod-history* nil))
         (records (if (listp history) (length history) 0))
         (characters
           (if (listp history)
               (loop for row in history
                     for content = (and (hash-table-p row) (gethash "content" row))
                     when (stringp content) sum (length content))
               0)))
    (obj "current_records" records "current_characters" characters
         "target_records" (%runtime-truth-value '*conversation-context-target-records* :null)
         "hard_records" (%runtime-truth-value '*conversation-context-hard-records* :null)
         "target_characters" (%runtime-truth-value '*conversation-context-target-chars* :null)
         "hard_characters" (%runtime-truth-value '*conversation-context-hard-chars* :null))))

(defun %runtime-truth-function-eq-p (function wrapper)
  (and (fboundp function) (fboundp wrapper)
       (eq (fdefinition function) (fdefinition wrapper))))

(defun %runtime-truth-effective-expected-owner (seam configured)
  (if (string= seam "tool")
      (cond ((and (fboundp 'tool-dispatch-kernel-boot-p)
                  (funcall 'tool-dispatch-kernel-boot-p))
             "kernel-tool-dispatch-runtime.lisp")
            ((%runtime-truth-value '*near-term-intention-tool-installed* nil)
             "near-term-intention-tool.lisp")
            (t "observability-tracing.lisp"))
      configured))

(defun %runtime-truth-observed-owner (seam function)
  (cond
    ((string= seam "transport")
     (cond ((and (boundp '*public-outbound-installed-telegram-wrapper*)
                 *public-outbound-installed-telegram-wrapper*
                 (fboundp function)
                 (eq (fdefinition function)
                     *public-outbound-installed-telegram-wrapper*))
            "public-outbound-gateway.lisp")
           ((fboundp function) "unexpected") (t "unavailable")))
    ((string= seam "turn")
     (cond ((%runtime-truth-function-eq-p function '%timing-auto-turn)
            "observability-tracing.lisp")
           ((fboundp function) "unexpected") (t "unavailable")))
    ((string= seam "tool")
     (cond ((and (boundp '*tool-dispatch-runtime-wrapper*)
                 *tool-dispatch-runtime-wrapper*
                 (fboundp function)
                 (eq (fdefinition function) *tool-dispatch-runtime-wrapper*))
            "kernel-tool-dispatch-runtime.lisp")
           ((%runtime-truth-function-eq-p function '%near-term-intention-tool-execute)
            "near-term-intention-tool.lisp")
           ((%runtime-truth-function-eq-p function '%timing-execute)
            "observability-tracing.lisp")
           ((fboundp function) "unexpected") (t "unavailable")))
    ((string= seam "memory")
     (cond ((%runtime-truth-function-eq-p function '%timing-memory-recall)
            "observability-tracing.lisp")
           ((fboundp function) "unexpected") (t "unavailable")))
    ((string= seam "initiative")
     (cond ((%runtime-truth-function-eq-p function '%timing-initiative)
            "observability-tracing.lisp")
           ((fboundp function) "unexpected") (t "unavailable")))
    ((string= seam "persistence")
     (cond ((%runtime-truth-function-eq-p function '%timing-conversation-persist)
            "observability-tracing.lisp")
           ((fboundp function) "unexpected") (t "unavailable")))
    (t "unavailable")))

(defun %runtime-truth-owner-rows ()
  (coerce
   (mapcar
    (lambda (spec)
      (destructuring-bind (seam function expected-file marker) spec
        (let* ((expected (%runtime-truth-effective-expected-owner seam expected-file))
               (observed (%runtime-truth-observed-owner seam
                                                        (intern (string-upcase function) :agent)))
               (declared (and (boundp '*wrap-chains*)
                              (hash-table-p *wrap-chains*)
                              (gethash function *wrap-chains*)))
               (declared-final (and declared (car (last (coerce declared 'list)))))
               (marker-loaded (fboundp marker)))
          (obj "seam" seam "function" function "expected_final_owner" expected
               "observed_final_owner" observed
               "declared_final_owner" (or declared-final :null)
               "owner_marker_loaded" (if marker-loaded t nil)
               "status" (cond ((string= observed "unavailable") "unavailable")
                              ((string= observed expected) "match")
                              (t "unexpected-final-owner"))))))
    *runtime-truth-expected-final-owners*)
   'vector))

(defun runtime-truth-deployed-modes-table (&optional manifest)
  (let ((source (or manifest (runtime-truth-manifest))))
    (gethash "mode_rows" source)))

(defun runtime-truth-assert (&optional manifest)
  "Fail on persisted/live mismatch or an unexpected loaded final owner."
  (let ((truth (or manifest (runtime-truth-manifest))))
    (when (fboundp 'cognition-runtime-assert)
      (funcall 'cognition-runtime-assert))
    (when (fboundp 'runtime-authority-assert)
      (runtime-authority-assert))
    (loop for row across (gethash "mode_rows" truth)
          when (string= (gethash "drift" row) "mismatch")
            do (error "Persisted/live runtime mismatch for ~a" (gethash "name" row)))
    (loop for row across (gethash "final_owners" truth)
          when (string= (gethash "status" row) "unexpected-final-owner")
            do (error "Unexpected final owner for ~a" (gethash "seam" row)))
    (when (fboundp 'runtime-authority-report)
      (let ((classes (gethash "decision_classes" (runtime-authority-report))))
        (loop for owner-row across (gethash "final_owners" truth)
              for seam = (gethash "seam" owner-row)
              for authority-row = (find seam classes :test #'string=
                                        :key (lambda (row)
                                               (gethash "decision_class" row)))
              for owners = (and authority-row
                                (gethash "effective_authorities" authority-row))
              unless (and authority-row (= 1 (length owners))
                          (string= (aref owners 0)
                                   (gethash "expected_final_owner" owner-row)))
                do (error "Declared authority does not match final owner for ~a"
                          seam))))
    t))

(defun %runtime-truth-telegram-call-source-p (source)
  (let ((lower (string-downcase source)))
    (or (search "(telegram-send " lower)
        (search "(funcall 'telegram-send " lower)
        (search "(funcall (quote telegram-send)" lower))))

(defun %runtime-truth-source-root ()
  "Directory tree holding the agent sources to audit.

   PAI_SOURCE_ROOT overrides. Otherwise walk up from this file until a src/
   directory appears -- sources live in layered subdirectories now, not
   beside this file as they did under the original flat layout."
  (let ((configured (uiop:getenv "PAI_SOURCE_ROOT")))
    (if configured
        (pathname (concatenate 'string configured "/"))
        (loop with dir = (make-pathname :name nil :type nil
                                        :defaults (or *load-truename*
                                                      *default-pathname-defaults*))
              repeat 8
              for candidate = (merge-pathnames #P"src/" dir)
              when (probe-file candidate) return candidate
              do (let ((parent (uiop:pathname-parent-directory-pathname dir)))
                   (when (equal parent dir) (return nil))
                   (setf dir parent))))))

(defun runtime-nonreply-transport-source-audit
    (&optional (root (%runtime-truth-source-root)))
  "Scan source names only in the report; never return source or message text.

   The scan is recursive. It used to list one flat directory, which was the
   entire source tree in the originating deployment; against a layered tree
   that call returns nothing and the audit reports success having examined no
   files at all. A transport escape anywhere below the root has to be found,
   so a scan that covered zero files is a failure, not a pass."
  (let ((unexpected-call-files nil) (raw-api-files nil) (base-bypass-files nil)
        (scanned 0))
    (dolist (path (ignore-errors
                    (and root (directory (merge-pathnames "**/*.lisp" root)))))
      (incf scanned)
      (let* ((name (string-downcase (file-namestring path)))
             (source (uiop:read-file-string path))
             (lower (string-downcase source)))
        ;; The scanner necessarily contains its own detection literals.
        (unless (string= name "runtime-truth.lisp")
          (when (and (%runtime-truth-telegram-call-source-p source)
                     (not (member name *runtime-truth-telegram-call-files*
                                  :test #'string=)))
            (push name unexpected-call-files))
          (when (and (search "api.telegram.org" lower)
                     (not (string= name "telegram.lisp")))
            (push name raw-api-files))
          (when (and (search "pai-base-telegram-send-public-outbound" lower)
                     (not (string= name "public-outbound-gateway.lisp")))
            (push name base-bypass-files)))))
    (obj "schema_version" 1 "scanned_files" scanned
         "expected_call_files" (coerce *runtime-truth-telegram-call-files* 'vector)
         "unexpected_call_files" (coerce (sort unexpected-call-files #'string<) 'vector)
         "raw_api_files" (coerce (sort raw-api-files #'string<) 'vector)
         "base_bypass_files" (coerce (sort base-bypass-files #'string<) 'vector)
         "passed" (if (and (plusp scanned)
                           (null unexpected-call-files) (null raw-api-files)
                           (null base-bypass-files)) t nil))))

(defun runtime-nonreply-transport-source-assert
    (&optional (root (%runtime-truth-source-root)))
  (let ((report (runtime-nonreply-transport-source-audit root)))
    (unless (gethash "passed" report)
      (error "Non-reply transport source audit failed: ~a"
             (shasht:write-json report nil)))
    t))

(defun runtime-truth-manifest ()
  (let ((mode-rows (%runtime-truth-mode-rows))
        (parameter-rows (%runtime-truth-parameter-rows)))
    (obj
     "schema_version" 2 "captured_at" (get-universal-time)
     "source_of_truth" "live-bound-values"
     "identity"
     (obj "container_id" (or (uiop:getenv "HOSTNAME") :null)
          "image_id" (or (uiop:getenv "PAI_IMAGE_ID") :null)
          "source_revision" (or (uiop:getenv "PAI_SOURCE_REVISION")
                                (uiop:getenv "OCI_REVISION") :null)
          "image_id_status" (if (uiop:getenv "PAI_IMAGE_ID") "declared" "unavailable")
          "source_revision_status"
          (if (or (uiop:getenv "PAI_SOURCE_REVISION") (uiop:getenv "OCI_REVISION"))
              "declared" "unavailable"))
     "mode_rows" mode-rows
     "parameter_rows" parameter-rows
     "deployed_modes_table" mode-rows
     "stabilization_modes"
     (%runtime-truth-report 'stabilization-mode-report :null)
     "cognition_runtime"
     (%runtime-truth-report 'cognition-runtime-report :null)
     "private_admin"
     (let ((prompt
             (%runtime-truth-report 'public-system-prompt-report nil
                                    :include-preview nil)))
       (obj "configured"
          (if (%runtime-truth-report 'admin-console-configured-p nil) t nil)
          "context_capture"
          (let ((capture (%runtime-truth-report
                          'llm-debug-current-public-context nil)))
            (if capture (gethash "status" capture "unavailable") "unavailable"))
          "prompt_fragments" (if prompt "available" "unavailable")
          "prompt_revision" (if prompt (gethash "revision" prompt) :null)
          "prompt_mutates_conversation_records" nil
          "delivery_authority" nil))
     "tick"
     (obj "base_interval_seconds" (%runtime-truth-value '*tick-base-interval-seconds* :null)
          "minimum_interval_seconds" (%runtime-truth-value '*tick-min-interval-seconds* :null)
          "maximum_interval_seconds" (%runtime-truth-value '*tick-max-interval-seconds* :null)
          "maximum_per_hour" (%runtime-truth-value '*tick-max-per-hour* :null)
          "soft_daily_cost_usd" (%runtime-truth-value '*tick-budget-soft-daily* :null)
          "hard_daily_cost_usd" (%runtime-truth-value '*tick-budget-hard-daily* :null))
     "threads"
     (vector
      (%runtime-truth-thread "telegram" '*telegram-thread* :autostart t
                             :owner "Docker entrypoint/start-telegram")
      (%runtime-truth-thread "scheduler" '*pai-scheduler-thread* :autostart nil
                             :owner "explicit pai-scheduler-start")
      (%runtime-truth-thread "tick" '*tick-thread* :autostart t
                             :owner "tick-loop")
      (%runtime-truth-thread "heap-health" '*heap-health-thread* :autostart t
                             :owner "post-recovery boot")
      (%runtime-truth-thread "turn-watchdog" '*turn-watchdog-thread* :autostart t
                             :owner "Docker entrypoint")
      (%runtime-truth-thread "replay-capsule" '*replay-capsule-thread* :autostart t
                             :owner "post-recovery boot")
      (%runtime-truth-thread "grounded-worker" '*grounded-agency-worker-thread*
                             :autostart nil :owner "explicit worker start"))
     "context" (%runtime-truth-conversation-usage)
     "limits"
     (obj "outbound_record_cap" (%runtime-truth-value '*public-outbound-record-cap* :null)
          "reciprocity_record_cap" (%runtime-truth-value '*reciprocity-canary-max-records* :null)
          "reciprocity_max_per_24_hours" (%runtime-truth-value '*reciprocity-canary-max-per-24-hours* :null)
          "reciprocity_minimum_spacing_seconds" (%runtime-truth-value '*reciprocity-canary-min-spacing-seconds* :null)
          "replay_record_cap" (%runtime-truth-value '*replay-capsule-record-cap* :null)
          "replay_retention_seconds" (%runtime-truth-value '*replay-capsule-retention-seconds* :null)
          "replay_record_limit_bytes" (%runtime-truth-value '*replay-capsule-record-max-bytes* :null)
          "replay_shard_limit_bytes" (%runtime-truth-value '*replay-capsule-shard-max-bytes* :null)
          "replay_disk_limit_bytes" (%runtime-truth-value '*replay-capsule-disk-max-bytes* :null)
          "replay_scheduled_max_per_day" (%runtime-truth-value '*replay-capsule-scheduled-max-per-day* :null)
          "replay_event_max_per_day" (%runtime-truth-value '*replay-capsule-event-max-per-day* :null)
          "replay_event_queue_cap" (%runtime-truth-value '*replay-capsule-event-queue-cap* :null)
          "label_batch_cap" (%runtime-truth-value '*pull-reciprocity-max-batches* :null)
          "pull_description_max_per_24_hours"
          (%runtime-truth-value
           '*pull-reciprocity-description-max-per-24-hours* :null)
          "candidate_representation_schema_version"
          (%runtime-truth-value '*candidate-representation-schema-version* :null))
     "providers"
     (obj "public_conversation_model" (%runtime-truth-value '*model* :unavailable)
          "private_cognition_model" (%runtime-truth-value '*model* :unavailable)
          "degraded_tick_model" (%runtime-truth-value '*tick-degraded-model* :unavailable)
          "epistemic_critic_model" (%runtime-truth-value '*epistemic-critic-model* :unavailable)
           "embedding_model" (%runtime-truth-value '*ollama-embed-model* :unavailable)
           "image_generation_model" (%runtime-truth-value '*runware-default-model* :unavailable)
           "credentials" "redacted"
           "credential_status"
           (obj "brave_search"
                (%runtime-truth-report 'brave-credential-status "unavailable")))
     "final_owners" (%runtime-truth-owner-rows)
     "public_outbound"
     (%runtime-truth-report 'public-outbound-gateway-report :null)
     "observers"
     (%runtime-truth-report 'runtime-observer-report :null)
     "authorities"
     (%runtime-truth-report 'runtime-authority-report :null)
     "observer_audit"
     (%runtime-truth-report 'runtime-observer-audit-report :null)
     "transport_inventory" (runtime-transport-inventory)
     "unverified_architecture_figures" "unmeasured")))

(defun runtime-truth-declare-authorities ()
  "Declare the effective authority for each wrapped seam.

   Must be called after the installs it describes. The effective owner of the
   tool seam is not a constant: %RUNTIME-TRUTH-EFFECTIVE-EXPECTED-OWNER
   resolves it by asking whether the near-term-intention tool and the kernel
   dispatch runtime are installed. Declaring before they install records the
   fallback owner and never revises it, so the declaration and the manifest
   then disagree for the rest of the process.

   This used to run as a top-level form at load. That was correct only because
   the tool installed itself as a load-time side effect, earlier in the load
   order. Once installation became an :INSTALL action the ordering silently
   inverted, and RUNTIME-TRUTH-ASSERT started failing on the tool seam."
  (when (fboundp 'runtime-authority-declare)
    (dolist (spec *runtime-truth-expected-final-owners*)
      (destructuring-bind (seam ignored expected ignored-marker) spec
        (declare (ignore ignored ignored-marker))
        (runtime-authority-declare
         seam (%runtime-truth-effective-expected-owner seam expected)
         :required t))))
  t)

(define-init :verify runtime-authority-declarations
    "Declare seam authorities, after :INSTALL and before the assertions that
     compare them against the loaded reality."
  (runtime-truth-declare-authorities))

;;; Cold-boot assertions.
;;;
;;; These two ran on every cold boot in the originating deployment, invoked
;;; from the container ENTRYPOINT rather than from Lisp. The load/init
;;; separation converted load-time side effects found *in source*, so
;;; assertions that only ever existed in the ENTRYPOINT were carried over by
;;; nobody -- pAI booted without them, silently, and the suite that would have
;;; caught it could not run.
;;;
;;; Registered here so the boot contract lives with the code that defines it
;;; instead of in a deployment artifact.
(define-init :verify runtime-truth-assert-boot
    "Fail closed on a persisted/live runtime mismatch or an unexpected loaded
     final seam owner."
  (runtime-truth-assert))

(define-init :verify runtime-nonreply-transport-source-assert-boot
    "Fail closed when the non-reply transport source audit does not pass."
  (runtime-nonreply-transport-source-assert))
