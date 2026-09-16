;;;; dev-workbench.lisp -- Rdev0 dev-only staged inspection UI.
;;;; Not loaded by Dockerfile. The isolated dev harness loads this file
;;;; only after executable recovery through the disposable REPL-drop seam.

(in-package :agent)

(export '(dev-workbench-capability-report dev-workbench-assemble
          dev-workbench-memory-search dev-workbench-context-projection
          dev-workbench-affect-snapshot dev-workbench-appraisal
          dev-workbench-prompt-preview dev-workbench-tool-dispatch
          dev-workbench-tool-dispatch-runtime dev-workbench-tool-catalog
          dev-workbench-tool-execute dev-workbench-turn-trace-index
          dev-workbench-turn-trace
          dev-workbench-conscious-submit-fixture
          dev-workbench-conscious-pulse dev-workbench-conscious-recover
          dev-workbench-conscious-open-captured
          dev-workbench-conscious-submit-captured-fixture
          dev-workbench-conscious-state))

(defparameter *dev-workbench-schema-version* 1)
(defparameter *dev-workbench-build-id* "rdev1-trace-1")
(defparameter *dev-workbench-max-message-chars* 8000)
(defparameter *dev-workbench-max-overlay-chars* 12000)
(defparameter *dev-workbench-max-records* 40)
(defparameter *dev-workbench-max-record-chars* 12000)
(defparameter *dev-workbench-max-tool-name-chars* 128)
(defparameter *dev-workbench-max-tool-arguments-chars* 16384)
(defparameter *dev-workbench-max-condition-chars* 2000)
(defvar *dev-workbench-tool-execution-lock*
  (bt:make-lock "dev-workbench-tool-execution"))
(defvar *dev-workbench-tool-call-sequence* 0)
(defvar *dev-workbench-tool-execution-sequence* 0)

(defun %dev-workbench-enabled-p ()
  (string= "enabled" (or (uiop:getenv "PAI_DEV_WORKBENCH") "")))

(defun %dev-workbench-run-id ()
  (or (uiop:getenv "PAI_DEV_RUN_ID") "unidentified"))

(defun %dev-workbench-manifest-valid-p ()
  (let ((path (or (uiop:getenv "PAI_DEV_MANIFEST_FILE")
                  "/agent/state/.pai-dev-runtime-manifest.json")))
    (handler-case
        (let ((manifest (shasht:read-json (uiop:read-file-string path))))
          (and (= (gethash "schema_version" manifest 0) 1)
               (string= (gethash "run_id" manifest "")
                        (%dev-workbench-run-id))
               (eq (gethash "disposable" manifest) t)
               (eq (gethash "database_label_verified" manifest) t)
               (eq (gethash "network_internal" manifest) t)
               (string= (gethash "ingress_scope" manifest "")
                        "loopback-only")
               (string= (gethash "provider_egress" manifest "") "disabled")
               (string= (gethash "delivery_authority" manifest "") "absent")))
      (error () nil))))

(defun %dev-workbench-runtime-safe-p ()
  (and (%dev-workbench-enabled-p)
       (%dev-workbench-manifest-valid-p)
       (boundp '*autonomous-write-mode*)
       (eq *autonomous-write-mode* :paused)
       (boundp '*memory-atom-decomposition-mode*)
       (eq *memory-atom-decomposition-mode* :off)))

(defun %dev-workbench-require-runtime ()
  (unless (%dev-workbench-runtime-safe-p)
    (error "Dev workbench requires the labelled paused dev runtime with N1 off."))
  t)

(defun %dev-workbench-sha256 (text)
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-sequence
    :sha256 (babel:string-to-octets (or text "") :encoding :utf-8))))

(defun %dev-workbench-list (value)
  (cond ((null value) nil)
        ((vectorp value) (coerce value 'list))
        ((listp value) value)
        (t (error "Expected a list or vector."))))

(defun %dev-workbench-exact-keys (table allowed label)
  (unless (hash-table-p table) (error "~a must be an object." label))
  (loop for key being the hash-keys of table
        unless (member key allowed :test #'string=)
          do (error "Unknown ~a key ~a." label key))
  table)

(defun %dev-workbench-bounded-text (value label maximum &key empty-ok)
  (unless (stringp value) (error "~a must be text." label))
  (unless (<= (length value) maximum)
    (error "~a exceeds ~d characters." label maximum))
  (unless (or empty-ok
              (plusp (length (string-trim '(#\Space #\Tab #\Newline #\Return)
                                           value))))
    (error "~a must not be empty." label))
  value)

(defun %dev-workbench-normalize-record (record index)
  (%dev-workbench-exact-keys record '("role" "content") "context record")
  (let ((role (gethash "role" record))
        (content (gethash "content" record)))
    (unless (member role '("system" "user" "assistant" "tool") :test #'string=)
      (error "Context record ~d has an invalid role." index))
    (%dev-workbench-bounded-text content "Context record content"
                                *dev-workbench-max-record-chars* :empty-ok t)
    (obj "role" role "content" content)))

(defun dev-workbench-assemble (request)
  (%dev-workbench-require-runtime)
  (%dev-workbench-exact-keys request '("message" "records" "overlay") "request")
  (let* ((message (%dev-workbench-bounded-text
                   (gethash "message" request "") "Message"
                   *dev-workbench-max-message-chars*))
         (overlay (%dev-workbench-bounded-text
                   (gethash "overlay" request "") "Overlay"
                   *dev-workbench-max-overlay-chars* :empty-ok t))
         (records (%dev-workbench-list (gethash "records" request #())))
         (normalized
           (progn
             (unless (<= (length records) *dev-workbench-max-records*)
               (error "Too many context records."))
             (loop for record in records for index from 0
                   collect (%dev-workbench-normalize-record record index))))
         (material
           (with-output-to-string (stream)
             (write-string message stream)
             (write-char #\Null stream)
             (write-string overlay stream)
             (dolist (record normalized)
               (write-char #\Null stream)
               (write-string (gethash "role" record) stream)
               (write-char #\Null stream)
               (write-string (gethash "content" record) stream)))))
    (obj "schema_version" *dev-workbench-schema-version*
         "status" "assembled" "run_id" (%dev-workbench-run-id)
         "message" message "records" (coerce normalized 'vector)
         "overlay" overlay
         "message_characters" (length message)
         "record_count" (length normalized)
         "overlay_characters" (length overlay)
         "input_sha256" (%dev-workbench-sha256 material)
         "provider_calls" 0 "database_writes" 0 "delivery_attempts" 0)))

(defun dev-workbench-memory-search (request)
  (%dev-workbench-require-runtime)
  (%dev-workbench-exact-keys request '("query" "limit") "memory request")
  (let ((query (%dev-workbench-bounded-text
                (gethash "query" request "") "Query" 1000))
        (limit (gethash "limit" request 3)))
    (unless (and (integerp limit) (<= 1 limit 5))
      (error "Memory limit must be from 1 through 5."))
    (unless (fboundp 'search-memory)
      (error "Declared public memory-search operator is unavailable."))
    (let ((*turn-capture-context*
            (obj "origin" "conversation" "as_of" (get-universal-time)))
          (*publication-contract-current*
            (obj "interaction_mode" "ordinary-reply")))
      (shasht:read-json (search-memory query :limit limit)))))

(defun dev-workbench-context-projection (request)
  (%dev-workbench-require-runtime)
  (%dev-workbench-exact-keys request '("message") "projection request")
  (let ((message (%dev-workbench-bounded-text
                  (gethash "message" request "") "Message"
                  *dev-workbench-max-message-chars*)))
    (unless (and (fboundp 'build-context-projection)
                 (fboundp 'render-context-projection))
      (error "Declared context-projection operators are unavailable."))
    ;; Disable the paid curator and durable event adapter while exercising the
    ;; real deterministic projection/retrieval implementation.
    (let ((*context-curator-mode* :off)
          (*context-projection-event-fn* (lambda (&rest ignored)
                                           (declare (ignore ignored)) nil)))
      (let* ((projection (build-context-projection message :mode :shadow))
             (rendered (render-context-projection projection)))
        (obj "schema_version" *dev-workbench-schema-version*
             "status" "projected" "projection" projection
             "rendered" rendered "rendered_characters" (length rendered)
             "rendered_sha256" (%dev-workbench-sha256 rendered)
             "provider_calls" 0 "database_writes" 0
             "delivery_attempts" 0)))))

(defun dev-workbench-affect-snapshot ()
  (%dev-workbench-require-runtime)
  (unless (fboundp 'modulator-state)
    (error "Declared affect-state operator is unavailable."))
  (obj "schema_version" *dev-workbench-schema-version*
       "status" "available" "source" "real-dev-modulator-state"
       "affect" (modulator-state) "simulated" nil
       "provider_calls" 0 "database_writes" 0 "delivery_attempts" 0))

(defun dev-workbench-appraisal (request)
  (%dev-workbench-require-runtime)
  (%dev-workbench-exact-keys request '("process_id") "appraisal request")
  (let ((process-id (gethash "process_id" request "")))
    (if (and (stringp process-id) (plusp (length process-id))
             (fboundp 'agent-appraisal-current))
        (obj "schema_version" *dev-workbench-schema-version*
             "status" "available" "source" "real-current-appraisal"
             "appraisal" (agent-appraisal-current process-id)
             "provider_calls" 0 "database_writes" 0 "delivery_attempts" 0)
        (obj "schema_version" *dev-workbench-schema-version*
             "status" "unavailable"
             "reason" "No declared pure message-appraisal port exists; Rdev0 will not call private scoring helpers. Supply a process_id to inspect an existing dev appraisal."
             "provider_calls" 0 "database_writes" 0 "delivery_attempts" 0))))

(defun dev-workbench-prompt-preview (request)
  (%dev-workbench-require-runtime)
  (%dev-workbench-exact-keys request '("overlay") "prompt request")
  (let ((overlay (%dev-workbench-bounded-text
                  (gethash "overlay" request "") "Overlay"
                  *dev-workbench-max-overlay-chars* :empty-ok t)))
    (unless (fboundp 'public-system-prompt-render-stable)
      (error "Declared stable prompt renderer is unavailable."))
    (let* ((stable (public-system-prompt-render-stable))
           (rendered
             (if (plusp (length overlay))
                 (format nil "~a~%~%<!-- DEV-OVERLAY:BEGIN -->~%~a~%<!-- DEV-OVERLAY:END -->"
                         stable overlay)
                 stable)))
      (obj "schema_version" *dev-workbench-schema-version*
           "status" "preview" "stable_prompt" stable "overlay" overlay
           "rendered_prompt" rendered "rendered_characters" (length rendered)
           "rendered_sha256" (%dev-workbench-sha256 rendered)
           "persisted" nil "provider_calls" 0 "database_writes" 0
           "delivery_attempts" 0))))

(defun dev-workbench-tool-dispatch (request)
  (%dev-workbench-require-runtime)
  (%dev-workbench-exact-keys request '("name") "tool-dispatch request")
  (unless (fboundp 'tool-dispatch-shadow-inspect)
    (error "Declared tool-dispatch shadow facade is unavailable."))
  ;; NAME is deliberately passed through as data so empty/non-text malformed
  ;; controls remain directly testable. No arguments or executable callback
  ;; are accepted by this endpoint.
  (tool-dispatch-shadow-inspect (gethash "name" request)))

(defun dev-workbench-tool-dispatch-runtime ()
  (%dev-workbench-require-runtime)
  (unless (and (fboundp 'tool-dispatch-runtime-report)
               (fboundp 'kernel-tool-dispatch-bootstrap-report))
    (error "Declared clean kernel tool dispatch is unavailable."))
  (obj "schema_version" *dev-workbench-schema-version*
       "status" "inspected"
       "runtime" (obj "schema_version" 1 "status" "retired"
                      "installed" nil
                      "sequence" *dev-workbench-tool-execution-sequence*)
       "kernel_dispatch" (tool-dispatch-runtime-report)
       "cold_boot" (kernel-tool-dispatch-bootstrap-report)
       "recent" #()
       "execution_attempted" nil "provider_calls" 0
       "database_writes" 0 "event_appends" 0
       "delivery_attempts" 0))

(defun %dev-workbench-tool-name (tool)
  (and (hash-table-p tool)
       (let ((function (gethash "function" tool)))
         (and (hash-table-p function) (gethash "name" function)))))

(defun %dev-workbench-json-copy (value)
  (shasht:read-json (shasht:write-json value nil)))

(defun dev-workbench-tool-catalog ()
  (%dev-workbench-require-runtime)
  (let ((tools (if (boundp '*tools*) *tools* #())))
    (obj "schema_version" *dev-workbench-schema-version*
         "status" "available"
         "tools"
         (let ((copy (%dev-workbench-json-copy
                      (if (vectorp tools) tools (coerce tools 'vector)))))
           (let* ((runtime (tool-dispatch-runtime-report))
                  (enabled (coerce (gethash "enabled_handler_ids" runtime)
                                   'list)))
             (loop for tool across copy
                   for name = (%dev-workbench-tool-name tool)
                   for plan-report = (tool-dispatch-shadow-inspect name)
                   for plan = (gethash "registry_plan" plan-report)
                   for handler-id = (gethash "handler_id" plan)
                   for binding = (and (stringp handler-id)
                                      (tool-handler-binding-lookup handler-id))
                   do (setf (gethash "rdev0_handler_id" tool)
                            (or handler-id :null)
                            (gethash "rdev0_qualification_group" tool)
                            (if binding (getf binding :group) :null)
                            (gethash "rdev0_dispatch_route" tool)
                            (if (and binding
                                     (member handler-id enabled :test #'string=))
                                "kernel explicit composition"
                                "legacy fallthrough"))))
           copy)
         "tool_count" (length tools)
         "execution_attempted" nil
         "dev_state_mutation_possible" nil)))

(defun %dev-workbench-runtime-ready-for-tool-execution ()
  (unless (and (fboundp 'tool-dispatch-runtime-report)
               (fboundp 'kernel-tool-dispatch-bootstrap-report)
               (fboundp 'execute))
    (error "Installed tool execution path is unavailable."))
  (let ((kernel (tool-dispatch-runtime-report))
        (cold-boot (kernel-tool-dispatch-bootstrap-report)))
    (unless (and (gethash "installed" cold-boot)
                 (gethash "installed" kernel)
                 (not (gethash "ownership_conflict" kernel)))
      (error "Installed tool execution path has an ownership conflict."))
    (obj "sequence" *dev-workbench-tool-execution-sequence*)))

(defun %dev-workbench-bounded-condition (condition)
  (let ((message (princ-to-string condition)))
    (obj "type" (string-downcase (symbol-name (type-of condition)))
         "message" (subseq message 0 (min (length message)
                                           *dev-workbench-max-condition-chars*)))))

(defun dev-workbench-tool-execute (request)
  (%dev-workbench-require-runtime)
  (%dev-workbench-exact-keys request '("name" "arguments")
                             "tool-execution request")
  (bt:with-lock-held (*dev-workbench-tool-execution-lock*)
    (let* ((runtime-before (%dev-workbench-runtime-ready-for-tool-execution))
           (name (%dev-workbench-bounded-text
                  (gethash "name" request "") "Tool name"
                  *dev-workbench-max-tool-name-chars*))
           (arguments (gethash "arguments" request))
           (tools (if (boundp '*tools*) *tools* #())))
      (unless (hash-table-p arguments)
        (error "Tool arguments must be a JSON object."))
      (let ((advertisement-count
              (count name (if (vectorp tools) (coerce tools 'list) tools)
                     :test #'string= :key #'%dev-workbench-tool-name)))
        (unless (= advertisement-count 1)
          (error "Tool must be advertised exactly once; observed ~d definitions."
                 advertisement-count)))
      (let ((arguments-json (shasht:write-json arguments nil)))
        (unless (<= (length arguments-json)
                    *dev-workbench-max-tool-arguments-chars*)
          (error "Tool arguments exceed ~d serialized characters."
                 *dev-workbench-max-tool-arguments-chars*))
        (let* ((sequence (incf *dev-workbench-tool-call-sequence*))
               (call-id (format nil "rdev0-~a-~d-~d"
                                (%dev-workbench-run-id)
                                (get-universal-time) sequence))
               (tool-call
                 (obj "id" call-id "type" "function" "function"
                      (obj "name" name "arguments" arguments-json)))
               (shadow-before (gethash "sequence" runtime-before 0))
               (values nil)
               (condition-report nil))
          (handler-case
                (let ((*turn-capture-context*
                      (obj "origin" "conversation"
                           "turn_id" call-id "user_event_id" :null
                           "as_of" (get-universal-time)
                           "dev_workbench_simulation" t))
                    (*publication-contract-current*
                      (obj "interaction_mode" "ordinary-reply"
                           "dev_workbench_simulation" t
                           "delivery_authority" nil)))
                (setf values (multiple-value-list (execute tool-call)))
                ;; Fail explicitly if the handler returned data the HTTP JSON
                ;; surface cannot represent; do not silently stringify it.
                (shasht:write-json (coerce values 'vector) nil))
            (error (condition)
              (setf condition-report
                    (%dev-workbench-bounded-condition condition))))
          (let* ((shadow-after (incf *dev-workbench-tool-execution-sequence*)))
            (obj "schema_version" *dev-workbench-schema-version*
                 "status" (if condition-report "condition" "executed")
                 "name" name "tool_call_id" call-id
                 "result" (if values (first values) :null)
                 "result_values" (coerce values 'vector)
                 "result_value_count" (length values)
                 "condition" (or condition-report :null)
                 "shadow_sequence_before" shadow-before
                 "shadow_sequence_after" shadow-after
                 "shadow_sequence_advanced"
                 (if (= shadow-after (1+ shadow-before)) t nil)
                 "shadow_observation" :null
                 "execution_attempted" t
                 "dev_state_mutation_possible" t
                 "rollback_performed" nil
                 "production_state_available" nil
                 "external_egress_available" nil)))))))

(defun dev-workbench-capability-report ()
  (obj "schema_version" *dev-workbench-schema-version*
       "build_id" *dev-workbench-build-id*
       "status" (if (%dev-workbench-runtime-safe-p) "ready" "blocked")
       "run_id" (%dev-workbench-run-id)
       "memory_search" (if (fboundp 'search-memory) t nil)
       "context_projection" (if (fboundp 'build-context-projection) t nil)
       "turn_trace_fixtures"
       (if (and (fboundp 'turn-trace-fixture-index)
                (fboundp 'turn-trace-fixture)) t nil)
       "conscious_pulse"
       (if (and (fboundp 'conscious-cognition-runtime-pulse)
                (fboundp 'conscious-cognition-runtime-recover)
                (fboundp 'conscious-cognition-runtime-report)) t nil)
       "affect_snapshot" (if (fboundp 'modulator-state) t nil)
       "appraisal_current" (if (fboundp 'agent-appraisal-current) t nil)
       "prompt_preview" (if (fboundp 'public-system-prompt-render-stable) t nil)
       "tool_dispatch_shadow"
       (if (and (fboundp 'tool-dispatch-shadow-capability-report)
                (gethash "initialized"
                         (tool-dispatch-shadow-capability-report))) t nil)
       "tool_dispatch_runtime"
       nil
       "kernel_dispatch_runtime"
       (if (and (fboundp 'tool-dispatch-runtime-report)
                (gethash "installed" (tool-dispatch-runtime-report))) t nil)
       "tool_execution_available"
       (if (and (fboundp 'execute)
                (fboundp 'tool-dispatch-runtime-report)
                (fboundp 'kernel-tool-dispatch-bootstrap-report)
                (gethash "installed"
                         (kernel-tool-dispatch-bootstrap-report))
                (gethash "installed" (tool-dispatch-runtime-report))) t nil)
       "tool_dispatch_boot_mode"
       (if (fboundp 'tool-dispatch-boot-mode)
           (string-downcase (symbol-name (tool-dispatch-boot-mode))) "unknown")
       "provider_calls_available" nil "web_search_available" nil
       "full_turn_available" nil "database_mutation_available" t
       "production_state_available" nil "delivery_authority" nil
       "access_scope" "loopback-only" "authentication_required" nil))

(defparameter *dev-workbench-conscious-fixtures*
  '(("q3-two-item"
     ("runtime-observer-error" . "q3-runtime-health")
     ("episode-boundary-detected" . "q3-project-change"))
    ("q4-user-message"
     ("user-message" . "q4-user-message"))))

(defvar *dev-workbench-conscious-captured-manifest* nil)

(defun %dev-workbench-require-conscious-runtime ()
  (%dev-workbench-require-runtime)
  (unless (and (fboundp 'cognition-runtime-selected-p)
               (cognition-runtime-selected-p :conscious-state))
    (error "Q3 workbench action requires the selected :conscious-state runtime"))
  t)

(defun %dev-workbench-append-readable (type fixture-code)
  (unless (and (fboundp 'log-event) (fboundp 'replay-events))
    (error "Q3 fixture submission requires the event-log ports"))
  (let ((id (log-event
             type
             (if (string= type "user-message")
                 (obj "fixture_code" fixture-code
                      "text" "How should this captured fixture be answered?"
                      "channel" "dev-workbench" "metadata" (obj))
                 (obj "fixture_code" fixture-code)))))
    (unless (and id
                 (find-if (lambda (event)
                            (and (hash-table-p event)
                                 (eql id (gethash "id" event))
                                 (string= type (gethash "type" event ""))))
                          (replay-events)))
      (error "Q3 fixture event ~s/~a was not durably readable" id type))
    id))

(defun dev-workbench-conscious-submit-fixture (request)
  "Append the closed two-item Q3 fixture and rebuild the selected projection."
  (%dev-workbench-require-conscious-runtime)
  (%dev-workbench-exact-keys request '("fixture_id") "conscious fixture request")
  (let* ((fixture-id (%dev-workbench-bounded-text
                      (gethash "fixture_id" request "") "Fixture id" 80))
         (fixture (assoc fixture-id *dev-workbench-conscious-fixtures*
                         :test #'string=)))
    (unless fixture (error "Unknown conscious fixture ~s" fixture-id))
    (let ((ids (loop for (type . code) in (rest fixture)
                     collect (%dev-workbench-append-readable type code))))
      (unless (fboundp 'cognition-runtime-restore)
        (error "Q3 fixture cannot refresh the selected projection"))
      (cognition-runtime-restore)
      (obj "schema_version" *dev-workbench-schema-version*
           "status" "submitted" "fixture_id" fixture-id
           "event_count" (length ids) "event_ids" (coerce ids 'vector)
           "provider_calls" 0 "effect_calls" 0 "delivery_attempts" 0))))

(defun dev-workbench-conscious-pulse (request)
  "Run one bounded, synchronous, providerless Q3 pulse."
  (%dev-workbench-require-conscious-runtime)
  (%dev-workbench-exact-keys request '("now" "cancelled")
                             "conscious pulse request")
  (let ((now (gethash "now" request))
        (cancelled (gethash "cancelled" request nil)))
    (unless (and (integerp now) (plusp now))
      (error "Conscious pulse now must be a positive integer"))
    (unless (member cancelled '(nil t))
      (error "Conscious pulse cancelled must be boolean"))
    (multiple-value-bind (plan state)
        (conscious-cognition-runtime-pulse
         :purpose :orient :now now :finished-at now
         :clock-identity "dev-workbench-explicit-clock"
         :budget (make-deterministic-pulse-budget
                  :wall-milliseconds 1000 :context-characters 4096
                  :proposals 1 :cancellation-checks 8)
         :cancelled-p cancelled)
      (let ((safe (conscious-pulse-plan-report plan)))
        (obj "schema_version" *dev-workbench-schema-version*
             "status" (gethash "status" safe)
             "pulse" safe
             "state_revision" (gethash "state_revision" state)
             "observation_revision" (gethash "observation_revision" state)
             "provider_calls" 0 "effect_calls" 0 "delivery_attempts" 0)))))

(defun %dev-workbench-q4-latest-fixture-event ()
  (find-if
   (lambda (event)
     (let ((payload (and (hash-table-p event) (gethash "payload" event))))
       (and (string= "user-message" (gethash "type" event ""))
            (hash-table-p payload)
            (string= "q4-user-message" (gethash "fixture_code" payload "")))))
   (reverse (replay-events))))

(defun %dev-workbench-q4-tool-names ()
  (let ((names
          (and (boundp '*tools*) (vectorp *tools*)
               (loop for tool across *tools*
                     for name = (%dev-workbench-tool-name tool)
                     when (and (stringp name) (plusp (length name)))
                       collect name))))
    (coerce (subseq (or names '()) 0 (min 8 (length (or names '())))) 'vector)))

(defun %dev-workbench-q4-assembly-spec (event)
  (let* ((event-id (gethash "id" event))
         (payload (gethash "payload" event))
         (text (%dev-workbench-bounded-text
                (gethash "text" payload "") "Q4 fixture text" 512))
         (tools (%dev-workbench-q4-tool-names)))
    (obj
     "audience" "operator" "total_character_budget" 2048
     "section_character_budgets"
     (obj "identity-instructions" 256 "sensorium" 128
          "focus-lifecycles" 128 "triggering-stimuli" 512
          "conversation-evidence" 128 "memory-bundles" 128
          "untrusted-tool-results" 128
          "tools-proposal-schema" 384 "publication-constraints" 256)
     "sections"
     (obj
      "identity-instructions"
      (vector (obj "source_id" "q4:policy"
                   "content" "Return only one structured proposal object; free prose is invalid."))
      "sensorium"
      (vector (obj "source_id" "q4:sensorium"
                   "content" "Disposable dev runtime; provider and delivery authority are absent."))
      "focus-lifecycles"
      (vector (obj "source_id" "q4:focus" "content" "Respond to the current fixture."))
      "triggering-stimuli"
      (vector (obj "source_id" event-id "content" text))
      "conversation-evidence" (vector) "memory-bundles" (vector)
      "untrusted-tool-results" (vector)
      "tools-proposal-schema"
      (vector (obj "source_id" "q4:schema"
                   "content" "Allowed proposal kinds are declared in the manifest; proposals remain inert."))
      "publication-constraints"
      (vector (obj "source_id" "q4:publication"
                   "content" "A publication candidate is private data, not delivered speech.")))
     "eligible_evidence_ids"
     (vector "q4:policy" "q4:sensorium" "q4:focus" event-id
             "q4:schema" "q4:publication")
     "available_tools" tools
     "permitted_proposal_kinds"
     (vector "tool-call-proposal" "publication-candidate" "yield" "abstain")
     "publication_constraints" (obj "audiences" (vector "operator"))
     "remaining_budget" (obj "tool_proposals" (if (plusp (length tools)) 1 0)
                              "continuations" 0
                              "publication_candidates" 1))))

(defun dev-workbench-conscious-open-captured (request)
  "Open the closed Q4 fixture and expose its private request under dev control."
  (%dev-workbench-require-conscious-runtime)
  (%dev-workbench-exact-keys request '("now") "captured open request")
  (let ((now (gethash "now" request))
        (event (%dev-workbench-q4-latest-fixture-event)))
    (unless (and (integerp now) (plusp now))
      (error "Captured pulse now must be a positive integer"))
    (unless event
      (error "Submit q4-user-message before opening captured deliberation"))
    (let ((assembled
            (conscious-cognition-runtime-open-captured
             :purpose :respond :now now
             :clock-identity "dev-workbench-explicit-clock"
             :assembly-spec (%dev-workbench-q4-assembly-spec event))))
      (setf *dev-workbench-conscious-captured-manifest*
            (gethash "manifest" assembled))
      assembled)))

(defun %dev-workbench-q4-proposal (manifest index kind payload evidence)
  (let ((pulse-id (gethash "pulse_id" manifest)))
    (obj "proposal_id" (format nil "~a:proposal:~d" pulse-id index)
         "pulse_id" pulse-id
         "runtime_revision" (gethash "runtime_revision" manifest)
         "conscious_state_revision"
         (gethash "conscious_state_revision" manifest)
         "kind" kind "created_at_stage" "model-deliberation"
         "confidence" 0.8d0 "evidence_event_ids" evidence
         "payload" payload)))

(defun %dev-workbench-q4-captured-fixture (fixture-id manifest)
  (let* ((evidence (gethash "evidence_event_ids" manifest))
         (event-id (find-if #'integerp (coerce evidence 'list)))
         (event-evidence (if event-id (vector event-id) (vector)))
         (tools (gethash "available_tools" manifest)))
    (cond
      ((string= fixture-id "q4-publication-candidate")
       (obj "schema_version" 1
            "proposals"
            (vector
             (%dev-workbench-q4-proposal
              manifest 1 "publication-candidate"
              (obj "audience" "operator" "channel_class" "diagnostic"
                   "speech_act" "answer"
                   "content" "This is a captured fixture candidate; it was not delivered."
                   "evidence_event_ids" event-evidence
                   "reason_to_speak_now" "direct-response")
              event-evidence))))
      ((string= fixture-id "q4-tool-proposal")
       (unless (plusp (length tools))
         (error "No tool is advertised in this captured manifest"))
       (obj "schema_version" 1
            "proposals"
            (vector
             (%dev-workbench-q4-proposal
              manifest 1 "tool-call-proposal"
              (obj "tool_name" (aref tools 0) "arguments" (obj))
              event-evidence))))
      ((string= fixture-id "q4-yield")
       (obj "schema_version" 1
            "proposals"
            (vector (%dev-workbench-q4-proposal
                     manifest 1 "yield" (obj) (vector)))))
      ((string= fixture-id "q4-invalid-mixed")
       (obj "schema_version" 1
            "proposals"
            (vector (%dev-workbench-q4-proposal
                     manifest 1 "yield" (obj) (vector))
                    (%dev-workbench-q4-proposal
                     manifest 2 "unknown-effect" (obj) (vector)))))
      (t (error "Unknown captured fixture ~s" fixture-id)))))

(defun dev-workbench-conscious-submit-captured-fixture (request)
  "Submit one named captured response. Arbitrary model text is not accepted."
  (%dev-workbench-require-conscious-runtime)
  (%dev-workbench-exact-keys request '("fixture_id") "captured fixture request")
  (let* ((fixture-id (%dev-workbench-bounded-text
                      (gethash "fixture_id" request "")
                      "Captured fixture id" 80))
         (manifest *dev-workbench-conscious-captured-manifest*))
    (unless (hash-table-p manifest)
      (error "No workbench captured deliberation is pending"))
    ;; Construct before entering UNWIND-PROTECT: an unknown name must not
    ;; discard the still-valid pending deliberation.
    (let ((captured (%dev-workbench-q4-captured-fixture fixture-id manifest)))
      (unwind-protect
           (multiple-value-bind (plan state)
               (conscious-cognition-runtime-submit-captured captured)
             (let ((safe (conscious-pulse-plan-report plan)))
               (obj "schema_version" *dev-workbench-schema-version*
                    "status" (gethash "status" safe)
                    "pulse" safe
                    "proposal_kinds"
                    (coerce (map 'list (lambda (proposal)
                                         (gethash "kind" proposal))
                                 (gethash "proposals" plan))
                            'vector)
                    "state_revision" (gethash "state_revision" state)
                    "observation_revision" (gethash "observation_revision" state)
                    "provider_calls" 0 "effect_calls" 0
                    "delivery_attempts" 0)))
        (setf *dev-workbench-conscious-captured-manifest* nil)))))

(defun dev-workbench-conscious-recover (request)
  (%dev-workbench-require-conscious-runtime)
  (%dev-workbench-exact-keys request '() "conscious recovery request")
  (prog1
      (obj "schema_version" *dev-workbench-schema-version*
           "status" "recovered"
           "recovered_count" (conscious-cognition-runtime-recover)
           "provider_calls" 0 "effect_calls" 0 "delivery_attempts" 0)
    (setf *dev-workbench-conscious-captured-manifest* nil)))

(defun dev-workbench-conscious-state ()
  (%dev-workbench-require-conscious-runtime)
  (conscious-cognition-runtime-report))

(defun dev-workbench-turn-trace-index ()
  (%dev-workbench-require-runtime)
  (unless (fboundp 'turn-trace-fixture-index)
    (error "Turn-trace projection is unavailable."))
  (obj "schema_version" *dev-workbench-schema-version*
       "fixtures" (turn-trace-fixture-index)
       "private_content_included" nil))

(defun dev-workbench-turn-trace (request)
  (%dev-workbench-require-runtime)
  (%dev-workbench-exact-keys request '("fixture_id") "request")
  (let ((fixture-id (%dev-workbench-bounded-text
                     (gethash "fixture_id" request "") "Fixture id" 80)))
    (unless (fboundp 'turn-trace-fixture)
      (error "Turn-trace projection is unavailable."))
    (turn-trace-fixture fixture-id)))

(defun %dev-workbench-json (value &optional (status 200))
  (setf (hunchentoot:return-code*) status
        (hunchentoot:content-type*) "application/json; charset=utf-8"
        (hunchentoot:header-out "Cache-Control") "no-store"
        (hunchentoot:header-out "X-Content-Type-Options") "nosniff")
  (shasht:write-json value nil))

(defun %dev-workbench-error (status message)
  (%dev-workbench-json
   (obj "schema_version" *dev-workbench-schema-version* "status" "error"
        "error" message) status))

(defun %dev-workbench-dispatch (thunk)
  (cond
    ((not (eq (hunchentoot:request-method*) :post))
     (%dev-workbench-error 405 "Method not allowed."))
    (t
     (handler-case
         (%dev-workbench-json
          (funcall thunk
                   (shasht:read-json
                    (or (hunchentoot:raw-post-data :force-text t) "{}"))))
       (error (condition)
         (%dev-workbench-error 400 (princ-to-string condition)))))))

(defparameter *dev-workbench-html*
  "<!doctype html><html><head><meta charset='utf-8'><meta name='viewport' content='width=device-width,initial-scale=1'><title>the agent dev workbench</title><style>body{margin:0;background:#10131b;color:#e9edf6;font:14px system-ui;padding:22px}main{max-width:1180px;margin:auto}.panel{background:#181d29;border:1px solid #30394e;border-radius:9px;padding:14px;margin:12px 0}.grid{display:grid;grid-template-columns:1fr 1fr;gap:12px}textarea,input,button{box-sizing:border-box;background:#22293a;color:#e9edf6;border:1px solid #3c4963;border-radius:6px;padding:9px}textarea{width:100%;min-height:120px;font:13px ui-monospace,monospace}input{width:100%;margin:4px 0 9px}button{cursor:pointer;margin:3px}pre{white-space:pre-wrap;word-break:break-word;max-height:520px;overflow:auto;background:#0b0e14;padding:12px}.warn{color:#ffc77d}.ok{color:#8fe0ad}@media(max-width:800px){.grid{grid-template-columns:1fr}}</style></head><body><main><h1>the agent isolated dev workbench</h1><p class='warn'>Disposable loopback-only dev state. Provider, web search, full turn and delivery are structurally disabled.</p><div class='panel'><span id='status'>Checking isolated runtime...</span><button onclick='statusRun()'>Refresh status</button></div><div class='grid'><div><div class='panel'><label>Test message<textarea id='message'></textarea></label><label>Temporary system overlay<textarea id='overlay'></textarea></label><label>Memory query<input id='query'></label><div><button onclick='run(&quot;assemble&quot;)'>Assemble</button><button onclick='run(&quot;memory&quot;)'>Memory search</button><button onclick='run(&quot;context&quot;)'>Context projection</button><button onclick='run(&quot;affect&quot;)'>Affect snapshot</button><button onclick='run(&quot;appraisal&quot;)'>Appraisal</button><button onclick='run(&quot;prompt&quot;)'>Build prompt</button><button onclick='disabled(&quot;model&quot;)'>Call OpenRouter</button><button onclick='disabled(&quot;full-turn&quot;)'>Run isolated full turn</button></div></div><div class='panel'><h2>Tool Dispatch shadow</h2><p>Inspect exact-name resolution and current tool advertisement. This never executes the tool.</p><label>Tool name<input id='toolName' value='lisp-eval'></label><button onclick='run(&quot;tool-dispatch&quot;)'>Inspect tool dispatch</button><button onclick='runtimeRun()'>Refresh installed shadow</button><p>Installed status observes only calls made by an already-authorized incumbent caller; this button never executes a tool.</p></div></div><div class='panel'><pre id='out'>No result.</pre></div></div><script>const $=id=>document.getElementById(id);async function api(path,body,method='POST'){let response=await fetch('/api/test/'+path,{method,cache:'no-store',headers:{'Content-Type':'application/json'},body:method==='POST'?JSON.stringify(body||{}):undefined}),text=await response.text(),data;try{data=JSON.parse(text)}catch(_){throw new Error('Invalid JSON '+response.status)}if(!response.ok)throw new Error(data.error||('HTTP '+response.status));return data}function bodyFor(name){if(name==='assemble')return{message:$('message').value,overlay:$('overlay').value,records:[]};if(name==='memory')return{query:$('query').value||$('message').value,limit:3};if(name==='context')return{message:$('message').value};if(name==='prompt')return{overlay:$('overlay').value};if(name==='tool-dispatch')return{name:$('toolName').value};return{}}async function statusRun(){try{let d=await api('status',null,'GET');$('status').className='ok';$('status').textContent='Loopback dev runtime: '+d.run_id;$('out').textContent=JSON.stringify(d,null,2)}catch(e){$('status').textContent=e.message}}async function runtimeRun(){try{$('out').textContent=JSON.stringify(await api('tool-dispatch-runtime',null,'GET'),null,2)}catch(e){$('out').textContent=e.message}}async function run(name){try{$('out').textContent='Running '+name+'...';$('out').textContent=JSON.stringify(await api(name,bodyFor(name)),null,2)}catch(e){$('out').textContent=e.message}}async function disabled(name){try{$('out').textContent=JSON.stringify(await api(name,{}),null,2)}catch(e){$('out').textContent=e.message}}statusRun()</script></main></body></html>")

(defun %dev-workbench-html-replace-one (text old new)
  (let ((position (search old text)))
    (unless position (error "Dev workbench HTML insertion point is absent."))
    (concatenate 'string (subseq text 0 position) new
                 (subseq text (+ position (length old))))))

(setf *dev-workbench-html*
      (%dev-workbench-html-replace-one
       *dev-workbench-html*
       "</div></div><div class='panel'><pre id='out'>"
       "</div><div class='panel'><h2>Real Tool Execution</h2><p class='warn'>Executes the actual global dispatcher. Changes to the disposable dev database, event log, appraisal/runtime state and scratch files persist. A powerful call may require Restart or Reset.</p><label>Advertised tool<select id='executeToolName'></select></label><label>Arguments JSON object<textarea id='toolArguments'>{}</textarea></label><button onclick='catalogRun()'>Refresh tool catalogue</button><button onclick='executeTool()'>Execute in disposable dev clone</button></div></div><div class='panel'><pre id='out'>"))

(setf *dev-workbench-html*
      (%dev-workbench-html-replace-one
       *dev-workbench-html*
       "statusRun()</script>"
       "async function catalogRun(){try{let d=await api('tool-catalog',null,'GET'),s=$('executeToolName'),prior=s.value;s.innerHTML='';for(let t of d.tools){let o=document.createElement('option');o.value=t.function.name;o.textContent=t.function.name;s.appendChild(o)}if([...s.options].some(o=>o.value===prior))s.value=prior;$('out').textContent=JSON.stringify(d,null,2)}catch(e){$('out').textContent=e.message}}async function executeTool(){try{let args=JSON.parse($('toolArguments').value);$('out').textContent='Executing real dev tool...';$('out').textContent=JSON.stringify(await api('tool-execute',{name:$('executeToolName').value,arguments:args}),null,2)}catch(e){$('out').textContent=e.message}}statusRun();catalogRun()</script>"))

(setf *dev-workbench-html*
      (%dev-workbench-html-replace-one
       *dev-workbench-html*
       "<div class='panel'><pre id='out'>"
       "<div class='panel'><h2>Turn Trace Explorer</h2><p>Sanitized deterministic fixtures use the same projection as the operator dashboard. No message, prompt, tool argument, response, or error body is included.</p><select id='traceFixture'></select><button onclick='traceRun()'>Load trace</button><div id='traceSummary' class='muted'></div><div id='traceViz'></div><pre id='traceOut'>No trace loaded.</pre></div><div class='panel'><pre id='out'>"))

(setf *dev-workbench-html*
      (%dev-workbench-html-replace-one
       *dev-workbench-html*
       "statusRun();catalogRun()</script>"
       "const traceEsc=s=>String(s??'').replace(/[&<>]/g,c=>({'&':'&amp;','<':'&lt;','>':'&gt;'}[c]));function renderTrace(d){let duration=Math.max(1,+d.duration_ms),spans=d.spans||[],attempts=d.provider_attempts||[],repeated=d.repeated_tools||[];$('traceViz').innerHTML=`<h3>Waterfall</h3>${spans.map(s=>`<div style='margin:5px 0'><div class=muted>${traceEsc(s.name)} - ${(+s.duration_ms).toFixed(1)} ms</div><div style='height:8px;margin-left:${Math.min(90,100*(+s.start_offset_ms)/duration)}%;width:${Math.max(1,Math.min(100,100*(+s.duration_ms)/duration))}%;background:#6d8dff;border-radius:4px'></div></div>`).join('')}<h3>Physical provider attempts</h3>${attempts.length?`<table><tr><th>Provider / model</th><th>Status</th><th>Duration</th><th>Tokens</th><th>Cost</th></tr>${attempts.map(a=>`<tr><td>${traceEsc(a.provider)} / ${traceEsc(a.model)}</td><td>${traceEsc(a.status)}</td><td>${traceEsc(a.duration_ms)} ms</td><td>${traceEsc(a.total_tokens)}</td><td>$${(+a.cost_usd||0).toFixed(6)}</td></tr>`).join('')}</table>`:'<div class=muted>Historical physical attempts are not exactly linked.</div>'}${repeated.length?'<h3>Repeated tools</h3>'+repeated.map(x=>`<div class=warn>${traceEsc(x.tool)} x ${x.count}</div>`).join(''):''}`;}async function traceIndex(){try{let d=await api('turn-trace-fixtures',null,'GET'),s=$('traceFixture');s.innerHTML=(d.fixtures||[]).map(x=>`<option value='${x.id}'>${x.label}</option>`).join('')}catch(e){$('traceOut').textContent=e.message}}async function traceRun(){try{let d=await api('turn-trace',{fixture_id:$('traceFixture').value}),t=d.totals||{},g=d.growth||{};$('traceSummary').textContent=`${d.duration_ms.toFixed(1)} ms - first output ${d.first_public_output_ms} ms - ${g.public_model_call_count} model calls - ${t.provider_attempt_total_tokens} tokens - $${(+t.provider_attempt_cost_usd).toFixed(6)}`;renderTrace(d);$('traceOut').textContent=JSON.stringify(d,null,2)}catch(e){$('traceOut').textContent=e.message}}statusRun();catalogRun();traceIndex()</script>"))

(hunchentoot:define-easy-handler (dev-workbench-page :uri "/test") ()
  (if (%dev-workbench-enabled-p)
      (progn
        (setf (hunchentoot:content-type*) "text/html; charset=utf-8"
              (hunchentoot:header-out "Cache-Control") "no-store"
              (hunchentoot:header-out "X-Content-Type-Options") "nosniff"
              (hunchentoot:header-out "Referrer-Policy") "no-referrer"
              (hunchentoot:header-out "Content-Security-Policy")
              "default-src 'self'; script-src 'unsafe-inline'; style-src 'unsafe-inline'; connect-src 'self'; object-src 'none'; base-uri 'none'; frame-ancestors 'none'")
        *dev-workbench-html*)
      (%dev-workbench-error 404 "Dev workbench is not enabled.")))

(hunchentoot:define-easy-handler (dev-workbench-status :uri "/api/test/status") ()
  (if (not (eq (hunchentoot:request-method*) :get))
      (%dev-workbench-error 405 "Method not allowed.")
      (%dev-workbench-json (dev-workbench-capability-report))))

(hunchentoot:define-easy-handler
    (dev-workbench-tool-dispatch-runtime-api
     :uri "/api/test/tool-dispatch-runtime") ()
  (if (not (eq (hunchentoot:request-method*) :get))
      (%dev-workbench-error 405 "Method not allowed.")
      (handler-case
          (%dev-workbench-json (dev-workbench-tool-dispatch-runtime))
        (error (condition)
          (%dev-workbench-error 400 (princ-to-string condition))))))

(hunchentoot:define-easy-handler
    (dev-workbench-tool-catalog-api :uri "/api/test/tool-catalog") ()
  (if (not (eq (hunchentoot:request-method*) :get))
      (%dev-workbench-error 405 "Method not allowed.")
      (handler-case
          (%dev-workbench-json (dev-workbench-tool-catalog))
        (error (condition)
          (%dev-workbench-error 400 (princ-to-string condition))))))

(hunchentoot:define-easy-handler
    (dev-workbench-turn-trace-fixtures-api
     :uri "/api/test/turn-trace-fixtures") ()
  (if (not (eq (hunchentoot:request-method*) :get))
      (%dev-workbench-error 405 "Method not allowed.")
      (handler-case
          (%dev-workbench-json (dev-workbench-turn-trace-index))
        (error (condition)
          (%dev-workbench-error 400 (princ-to-string condition))))))

(macrolet ((define-post (name uri function)
             `(hunchentoot:define-easy-handler (,name :uri ,uri) ()
                (%dev-workbench-dispatch #',function))))
  (define-post dev-workbench-assemble-api "/api/test/assemble" dev-workbench-assemble)
  (define-post dev-workbench-memory-api "/api/test/memory" dev-workbench-memory-search)
  (define-post dev-workbench-context-api "/api/test/context" dev-workbench-context-projection)
  (define-post dev-workbench-appraisal-api "/api/test/appraisal" dev-workbench-appraisal)
  (define-post dev-workbench-prompt-api "/api/test/prompt" dev-workbench-prompt-preview)
  (define-post dev-workbench-tool-dispatch-api "/api/test/tool-dispatch"
               dev-workbench-tool-dispatch)
  (define-post dev-workbench-tool-execute-api "/api/test/tool-execute"
               dev-workbench-tool-execute)
  (define-post dev-workbench-turn-trace-api "/api/test/turn-trace"
               dev-workbench-turn-trace)
  (define-post dev-workbench-conscious-fixture-api "/api/test/conscious-fixture"
               dev-workbench-conscious-submit-fixture)
  (define-post dev-workbench-conscious-pulse-api "/api/test/conscious-pulse"
               dev-workbench-conscious-pulse)
  (define-post dev-workbench-conscious-captured-open-api
               "/api/test/conscious-captured-open"
               dev-workbench-conscious-open-captured)
  (define-post dev-workbench-conscious-captured-submit-api
               "/api/test/conscious-captured-submit"
               dev-workbench-conscious-submit-captured-fixture)
  (define-post dev-workbench-conscious-recover-api "/api/test/conscious-recover"
               dev-workbench-conscious-recover))

(hunchentoot:define-easy-handler
    (dev-workbench-conscious-state-api :uri "/api/test/conscious-state") ()
  (if (not (eq (hunchentoot:request-method*) :get))
      (%dev-workbench-error 405 "Method not allowed.")
      (handler-case (%dev-workbench-json (dev-workbench-conscious-state))
        (error (condition)
          (%dev-workbench-error 400 (princ-to-string condition))))))

(hunchentoot:define-easy-handler (dev-workbench-affect-api :uri "/api/test/affect") ()
  (if (not (eq (hunchentoot:request-method*) :post))
      (%dev-workbench-error 405 "Method not allowed.")
      (handler-case (%dev-workbench-json (dev-workbench-affect-snapshot))
        (error (condition) (%dev-workbench-error 400 (princ-to-string condition))))))

(macrolet ((define-disabled (name uri capability)
             `(hunchentoot:define-easy-handler (,name :uri ,uri) ()
                (%dev-workbench-json
                 (obj "schema_version" *dev-workbench-schema-version*
                      "status" "disabled" "capability" ,capability
                      "reason" "Offline-first Rdev0 has no provider, external network, full-turn or delivery adapter."
                      "provider_calls" 0 "database_writes" 0
                      "delivery_attempts" 0) 409))))
  (define-disabled dev-workbench-model-api "/api/test/model" "openrouter")
  (define-disabled dev-workbench-full-turn-api "/api/test/full-turn" "full-turn")
  (define-disabled dev-workbench-web-search-api "/api/test/web-search" "web-search"))

;;; Runtime safety is enforced per call, not at load.
;;;
;;; Every one of the twelve acting entry points opens with
;;; %DEV-WORKBENCH-REQUIRE-RUNTIME, so nothing here can execute outside a
;;; labelled, paused dev runtime with N1 off. DEV-WORKBENCH-CAPABILITY-REPORT
;;; is deliberately exempt: it takes no action and exists to *report*
;;; "blocked", which a guard would make impossible to ask for.
;;;
;;; This used to be a bare top-level call, which made merely loading the file
;;; an error unless the whole dev runtime was already configured -- which is
;;; why this file sat outside the load chain. It protected nothing the
;;; per-call guards do not.
;;;
;;; The check is only enforced when the workbench is actually switched on. A
;;; production image loads this file and never enables it; failing
;;; verification there would refuse to boot over a dev tool nobody asked for.
(define-init :verify dev-workbench-runtime-safety
    "Refuse to start with the dev workbench enabled outside a labelled,
     paused dev runtime with N1 decomposition off. No-op when the workbench
     is switched off, which is the normal production case."
  (when (%dev-workbench-enabled-p)
    (%dev-workbench-require-runtime)))
