;;;; conscious-conversation.lisp -- native interactive CLI process.

(in-package :cl-user)

(require :asdf)

(defparameter *conversation-repo-root*
  (uiop:pathname-parent-directory-pathname
   (uiop:pathname-directory-pathname *load-truename*)))

(defun %conversation-quicklisp-setup ()
  (let ((configured (uiop:getenv "PAI_QUICKLISP_SETUP")))
    (or (and configured (probe-file configured))
        (probe-file (merge-pathnames #P".tools/quicklisp/setup.lisp"
                                     *conversation-repo-root*))
        (error "Quicklisp setup not found; run the local Lisp setup"))))

(load (%conversation-quicklisp-setup))
(push *conversation-repo-root* asdf:*central-registry*)

(defun %conversation-symbol (name) (intern (string-upcase name) :agent))
(defun %conversation-call (name &rest arguments)
  (apply (symbol-function (%conversation-symbol name)) arguments))
(defun %conversation-object (&rest pairs)
  (apply #'%conversation-call "obj" pairs))

(defun %conversation-required-env (name)
  (let ((value (uiop:getenv name)))
    (unless (and (stringp value) (plusp (length value)))
      (error "Conversation requires ~a" name))
    value))

(defparameter *conversation-startup-started* (get-internal-real-time))
(defvar *conversation-startup-phase-started* *conversation-startup-started*)
(defvar *conversation-event-authority-receipt* nil)
(defvar *conversation-event-checkpoint-receipt* nil)
(defvar *conversation-memory-import-receipt* nil)
(defvar *conversation-memory-authority-receipt* nil)
(defparameter *conversation-loop-mode*
  (or (uiop:getenv "PAI_CONVERSATION_LOOP") "work-state"))
(defparameter *conversation-curiosity-wake-seconds*
  (let ((text (or (uiop:getenv "PAI_CURIOSITY_WAKE_SECONDS") "0")))
    (handler-case
        (let ((seconds (parse-integer text :junk-allowed nil)))
          (unless (<= 0 seconds 86400)
            (error "out of range"))
          seconds)
      (error ()
        (error "PAI_CURIOSITY_WAKE_SECONDS must be an integer from 0 to 86400")))))
(defparameter *conversation-deliberate-curiosity-p*
  (string= "1" (or (uiop:getenv "PAI_DELIBERATE_CURIOSITY") "0")))
(defparameter *conversation-curiosity-reach-out-p*
  (string= "1" (or (uiop:getenv "PAI_CURIOSITY_REACH_OUT") "0")))
(defparameter *conversation-curiosity-briefing-p*
  (string= "1" (or (uiop:getenv "PAI_CURIOSITY_BRIEFING") "0")))
(defparameter *conversation-curiosity-consolidation-p*
  (not (string= "0" (or (uiop:getenv "PAI_CURIOSITY_CONSOLIDATION") "1"))))
(defparameter *conversation-affect-baseline-event-id*
  (let ((text (uiop:getenv "PAI_AFFECT_BASELINE_EVENT_ID")))
    (when text
      (handler-case
          (let ((event-id (parse-integer text :junk-allowed nil)))
            (unless (not (minusp event-id)) (error "out of range"))
            event-id)
        (error ()
          (error "PAI_AFFECT_BASELINE_EVENT_ID must be a non-negative integer"))))))
(defparameter *conversation-episodic-memory-p*
  (string= "1" (or (uiop:getenv "PAI_EPISODIC_MEMORY") "0")))
(defparameter *conversation-knowledge-graph-formation-p*
  (string= "1" (or (uiop:getenv "PAI_KNOWLEDGE_GRAPH_FORMATION") "0")))
(defparameter *conversation-knowledge-graph-rebuild-only-p*
  (string= "1" (or (uiop:getenv "PAI_KNOWLEDGE_GRAPH_REBUILD_ONLY") "0")))
(defparameter *conversation-private-budget-percent*
  (let ((text (or (uiop:getenv "PAI_PRIVATE_BUDGET_PERCENT") "30")))
    (handler-case
        (let ((percent (parse-integer text :junk-allowed nil)))
          (unless (<= 0 percent 100)
            (error "out of range"))
          percent)
        (error ()
          (error "PAI_PRIVATE_BUDGET_PERCENT must be an integer from 0 to 100")))))
(defparameter *conversation-private-reasoning-effort*
  (let ((effort (or (uiop:getenv "PAI_PRIVATE_REASONING_EFFORT") "minimal")))
    (unless (member effort '("minimal" "low" "medium" "high" "max")
                    :test #'string=)
      (error "PAI_PRIVATE_REASONING_EFFORT is invalid"))
    effort))

(defun %conversation-positive-timeout-env (name default &key optional)
  (let* ((raw (uiop:getenv name))
         (text (and raw (string-downcase
                         (string-trim '(#\Space #\Tab) raw)))))
    (when (and optional text
               (member text '("" "0" "off" "none" "nil") :test #'string=))
      (return-from %conversation-positive-timeout-env nil))
    (when (or (null text) (zerop (length text)))
      (return-from %conversation-positive-timeout-env default))
    (handler-case
        (let ((seconds (parse-integer text :junk-allowed nil)))
          (unless (<= 1 seconds 86400) (error "out of range"))
          seconds)
      (error ()
        (error "~a must be a positive integer from 1 to 86400~@[ or off~]"
               name optional)))))

(defparameter *conversation-provider-call-timeout-seconds*
  (%conversation-positive-timeout-env
   "PAI_PROVIDER_CALL_TIMEOUT_SECONDS" 600 :optional t))
(defparameter *conversation-provider-connect-timeout-seconds*
  (%conversation-positive-timeout-env
   "PAI_PROVIDER_CONNECT_TIMEOUT_SECONDS" 10))
(defparameter *conversation-provider-inactivity-timeout-seconds*
  (%conversation-positive-timeout-env
   "PAI_PROVIDER_INACTIVITY_TIMEOUT_SECONDS" 180))
(defparameter *conversation-provider-streaming-p*
  (not (string= "0" (or (uiop:getenv "PAI_PROVIDER_STREAMING") "1"))))

(defun %conversation-startup-elapsed-seconds (started)
  (/ (- (get-internal-real-time) started)
     (coerce internal-time-units-per-second 'double-float)))

(defun %conversation-startup-phase (index message)
  (setf *conversation-startup-phase-started* (get-internal-real-time))
  (format t "~&[startup ~d/5] ~a...~%" index message)
  (finish-output))

(defun %conversation-startup-phase-done (index label)
  (format t "~&[startup ~d/5] ~a (~,1fs)~%"
          index label
          (%conversation-startup-elapsed-seconds
           *conversation-startup-phase-started*))
  (finish-output))

(defun %conversation-load-pai-system ()
  "Load ASDF quietly for operators, replaying diagnostics if the load fails."
  (if (string= (or (uiop:getenv "PAI_STARTUP_VERBOSE") "") "1")
      (let ((*standard-output* (make-broadcast-stream)))
        (asdf:load-system :pai))
      (let ((diagnostics (make-string-output-stream)))
        (handler-case
            (let ((*standard-output* (make-broadcast-stream))
                  (*error-output* diagnostics)
                  (*trace-output* diagnostics))
              (asdf:load-system :pai))
          (error (condition)
            ;; A clean successful boot should not dump historical compile-time
            ;; forward-reference noise. A failed boot must retain the complete
            ;; diagnostic record needed to repair it.
            (write-string (get-output-stream-string diagnostics)
                          *error-output*)
            (finish-output *error-output*)
            (error condition))))))

(defun %conversation-path-contained-p (child parent)
  ;; UIOP:ENSURE-PATHNAME parses a bare string with Unix namestring rules
  ;; regardless of host OS, so a native Windows drive path like "Z:/..."
  ;; from an env var never satisfies :WANT-ABSOLUTE. Parse with native
  ;; rules first; this is a no-op on POSIX hosts where the two agree.
  (let ((child (uiop:ensure-pathname (uiop:parse-native-namestring child)
                                      :want-absolute t))
        (parent (uiop:ensure-directory-pathname
                 (uiop:ensure-pathname (uiop:parse-native-namestring parent)
                                        :want-absolute t))))
    (uiop:subpathp child parent)))

(defun %conversation-guard-storage-boundary ()
  "Refuse startup unless both SQLite files are contained by the state root."
  (let ((root (%conversation-required-env "PAI_STATE_ROOT"))
        (events (%conversation-required-env "PAI_EVENT_STORAGE_DATABASE"))
        (derived (%conversation-required-env "PAI_DERIVED_STORAGE_DATABASE"))
        (agent (%conversation-required-env "PAI_AGENT_ID")))
    (declare (ignore agent))
    (unless (and (%conversation-path-contained-p events root)
                 (%conversation-path-contained-p derived root))
      (error "Event and derived databases must remain inside PAI_STATE_ROOT"))))

(defun %conversation-write-manifest ()
  (let ((pathname
          (merge-pathnames
           #P".pai-conversation-manifest.json"
           (uiop:ensure-directory-pathname
            (pathname (or (uiop:getenv "PAI_STATE_ROOT")
                          (uiop:temporary-directory))))))
        (provider-egress
          (if (uiop:getenv "PAI_CONVERSATION_PROVIDER_PROFILE")
              "declared-openrouter-profile-with-session-budget"
              "loopback-only")))
    (with-open-file (stream pathname :direction :output :if-exists :supersede
                                     :if-does-not-exist :create)
      (write-string
       (format nil
               "{\"schema_version\":1,\"run_id\":\"solicited-conversation\",\"state_boundary_verified\":true,\"ingress_scope\":\"local-cli\",\"provider_egress\":\"~a\",\"delivery_authority\":\"solicited-cli-only\",\"tools_authorized\":true,\"tool_scope\":\"read-only-search-files-through-durable-work-loop\",\"effects_authorized\":false}"
               provider-egress)
       stream))
    (setf (uiop:getenv "PAI_DEV_WORKBENCH") "enabled"
          (uiop:getenv "PAI_DEV_RUN_ID") "solicited-conversation"
          (uiop:getenv "PAI_DEV_MANIFEST_FILE") (namestring pathname))))

(%conversation-guard-storage-boundary)
(%conversation-write-manifest)

(%conversation-startup-phase 1 "Preparing the contained runtime")
(let ((*standard-output* (make-broadcast-stream)))
  ;; HEAP-HEALTH has a historical load-time worker. Establish its gate before
  ;; the full ASDF load so no event can be appended before the durable ID
  ;; watermark is restored below.
  (load (merge-pathnames #P"src/kernel/agent.lisp" *conversation-repo-root*)))
(setf (symbol-value (intern "*HEAP-HEALTH-AUTOSTART-P*" :agent)) nil)
(%conversation-startup-phase-done 1 "Contained runtime prepared")

(%conversation-startup-phase 2 "Loading pAI modules")
(%conversation-load-pai-system)
(setf (symbol-value
       (%conversation-symbol
        "*CONSCIOUS-CONVERSATION-PROVIDER-CALL-TIMEOUT-SECONDS*"))
      *conversation-provider-call-timeout-seconds*
      (symbol-value
       (%conversation-symbol
        "*CONSCIOUS-CONVERSATION-PROVIDER-CONNECT-TIMEOUT-SECONDS*"))
      *conversation-provider-connect-timeout-seconds*
      (symbol-value
       (%conversation-symbol
        "*CONSCIOUS-CONVERSATION-PROVIDER-INACTIVITY-TIMEOUT-SECONDS*"))
      *conversation-provider-inactivity-timeout-seconds*
      (symbol-value
       (%conversation-symbol
        "*CONSCIOUS-CONVERSATION-PROVIDER-STREAMING-P*"))
      *conversation-provider-streaming-p*)
(defvar *conversation-curiosity-thread* nil)
(defvar *conversation-curiosity-running-p* nil)
(defvar *conversation-curiosity-lock* (bt:make-lock "conversation-curiosity"))
(defvar *conversation-curiosity-condition* (bt:make-condition-variable))
(defvar *conversation-curiosity-last-activity* (get-internal-real-time))
(%conversation-startup-phase-done 2 "pAI modules loaded")

(defparameter *conversation-unified-cli-p*
  (string= (or (uiop:getenv "PAI_UNIFIED_CLI") "") "1"))

(%conversation-startup-phase 3 "Loading CLI and persona policy")
(when *conversation-unified-cli-p*
  (load (merge-pathnames #P"scripts/conscious-lifecycle-scenario-core.lisp"
                         *conversation-repo-root*))
  (load (merge-pathnames #P"scripts/pai-cli-core.lisp"
                         *conversation-repo-root*)))

(%conversation-call
 "conscious-conversation-load-persona-profile"
 (%conversation-required-env "PAI_CONVERSATION_PERSONA")
 (or (uiop:getenv "PAI_CONVERSATION_PERSONA_FILE") ""))
(%conversation-startup-phase-done 3 "CLI and persona policy loaded")

(defun %conversation-load-context-profile ()
  (let* ((pathname (or (uiop:getenv "PAI_CONSCIOUS_CONTEXT_PROFILES")
                       (namestring
                        (merge-pathnames
                         #P"config/conscious-context-profiles.json"
                         *conversation-repo-root*))))
         (name (or (uiop:getenv "PAI_CONVERSATION_CONTEXT_PROFILE")
                   "solicited-conversation-dev"))
         (document (with-open-file (stream pathname :direction :input)
                     (shasht:read-json stream)))
         (profiles (gethash "profiles" document))
         (profile (and (hash-table-p profiles) (gethash name profiles))))
    (unless (hash-table-p profile)
      (error "Unknown conscious context profile ~s" name))
    profile))

(defun %conversation-load-work-profile ()
  (let* ((pathname
           (or (uiop:getenv "PAI_CONSCIOUS_WORK_PROFILES")
               (namestring
                (merge-pathnames #P"config/conscious-work-profiles.json"
                                 *conversation-repo-root*))))
         (name (or (uiop:getenv "PAI_CONVERSATION_WORK_PROFILE")
                   "interactive-dev"))
         (document (with-open-file (stream pathname :direction :input)
                     (shasht:read-json stream)))
         (profiles (gethash "profiles" document))
         (profile (and (hash-table-p profiles) (gethash name profiles))))
    (unless (hash-table-p profile)
      (error "Unknown conscious work profile ~s" name))
    profile))

(defun %conversation-validate-work-context-compatibility
    (context-profile work-profile)
  "Fail startup before inference when tool evidence cannot fit its section."
  (let* ((sections (and (hash-table-p context-profile)
                        (gethash "section_character_budgets" context-profile)))
         (section-budget
           (and (hash-table-p sections)
                (gethash "untrusted-tool-results" sections)))
         (result-budget
           (and (hash-table-p work-profile)
                (gethash "max_tool_result_characters" work-profile)))
         (wrapper-per-record
           (and (hash-table-p context-profile)
                (gethash "tool_result_wrapper_characters_per_record"
                         context-profile)))
         (operation-count
           (and (hash-table-p work-profile)
                (gethash "max_tool_operations" work-profile)))
         (headroom
           (and (integerp wrapper-per-record) (integerp operation-count)
                (* wrapper-per-record operation-count))))
    (unless (and (integerp section-budget) (integerp result-budget)
                 (integerp headroom) (not (minusp headroom))
                 (>= section-budget (+ result-budget headroom)))
      (error "Conversation context profile cannot carry bounded tool results"))
    t))

(defun %conversation-runtime-capabilities (work-profile)
  (let ((tools (make-hash-table :test #'equal))
        (proposals (make-hash-table :test #'equal)))
    (loop for tool across (gethash "permitted_tools" work-profile)
          do (setf (gethash tool tools)
                   (%conversation-object
                    "consumer" "conscious-tool-operation-runtime"
                    "authority_class" "bounded-read-only"
                    "max_result_characters"
                    (gethash "max_tool_result_characters" work-profile))))
    (loop for kind across (gethash "permitted_proposal_kinds" work-profile)
          do (setf (gethash kind proposals)
                   (vector
                    (if (string= kind "tool-call-proposal")
                        "cognitive-operation-executor"
                        "conversation-work-loop"))))
    (%conversation-object "tool_consumers" tools
                          "proposal_consumers" proposals)))

(defun %conversation-compile-runtime-plan (context-profile work-profile)
  (let* ((provider-name
           (or (uiop:getenv "PAI_CONVERSATION_PROVIDER_PROFILE")
               "contained-cli-provider"))
         (provider
           (%conversation-object
            "profile_id" provider-name "revision" 1
            "max_requests" (gethash "max_model_calls" work-profile)
            "max_input_characters"
            (gethash "total_character_budget" context-profile)))
         (publication
           (%conversation-object
            "profile_id" "solicited-publication" "revision" 1
            "channels" (vector "terminal")))
         (transport
           (%conversation-object
            "profile_id" "terminal-transport" "revision" 1
            "channel" "terminal"))
         (plan
           (%conversation-call
            "conscious-runtime-plan-compile"
            context-profile work-profile
            (%conversation-runtime-capabilities work-profile)
            provider publication transport)))
    (%conversation-call "conscious-runtime-plan-retain" plan)
    plan))

(defun %conversation-load-provider-profile ()
  (let ((name (uiop:getenv "PAI_CONVERSATION_PROVIDER_PROFILE")))
    (when (and name (plusp (length name)))
      (when (member name '("contained-cli-provider" "local-providerless-v1")
                    :test #'string=)
        (return-from %conversation-load-provider-profile nil))
      (let* ((pathname
               (or (uiop:getenv "PAI_CONSCIOUS_PROVIDER_PROFILES")
                   (namestring
                    (merge-pathnames
                     #P"config/conscious-provider-profiles.json"
                     *conversation-repo-root*))))
             (document (with-open-file (stream pathname :direction :input)
                         (shasht:read-json stream)))
             (profiles (gethash "profiles" document))
             (profile (and (hash-table-p profiles) (gethash name profiles))))
        (unless (and (hash-table-p profile)
                     (string= "openrouter" (gethash "provider" profile ""))
                     (string= "proposal-only"
                              (gethash "publication_role" profile ""))
                     (eq t (gethash "requires_native_tool_calls" profile))
                     (nth-value
                      1 (gethash "supports_parallel_tool_calls_parameter"
                                 profile))
                     (member
                      (gethash "supports_parallel_tool_calls_parameter"
                               profile)
                      '(t nil)))
          (error "Unknown or unauthorized conversation provider profile ~s"
                 name))
        (let ((model-override
                (uiop:getenv "PAI_CONVERSATION_MODEL_OVERRIDE"))
              (zdr-override
                (uiop:getenv "PAI_CONVERSATION_ZDR_OVERRIDE"))
              (collection-override
                (uiop:getenv "PAI_CONVERSATION_DATA_COLLECTION_OVERRIDE"))
              (reasoning-override
                (uiop:getenv "PAI_CONVERSATION_REASONING_OVERRIDE"))
              (reasoning-effort
                (uiop:getenv "PAI_CONVERSATION_REASONING_EFFORT")))
          (if (some (lambda (value)
                      (and value (plusp (length value))))
                    (list model-override zdr-override collection-override
                          reasoning-override reasoning-effort))
              (let ((selected (make-hash-table :test #'equal))
                    (routing (make-hash-table :test #'equal)))
                (loop for key being the hash-keys of profile
                        using (hash-value value)
                      do (setf (gethash key selected) value))
                (loop for key being the hash-keys of
                        (gethash "provider_routing" profile)
                        using (hash-value value)
                      do (setf (gethash key routing) value))
                (setf (gethash "provider_routing" selected) routing)
                (when (and model-override (plusp (length model-override)))
                  (unless (and (<= (length model-override) 200)
                               (= 1 (count #\/ model-override))
                               (every (lambda (character)
                                        (<= 33 (char-code character) 126))
                                      model-override)
                               (not (char= #\/ (char model-override 0)))
                               (not (char= #\/ (char model-override
                                                     (1- (length
                                                          model-override))))))
                    (error "Invalid OpenRouter model override"))
                  (setf (gethash "profile_model" selected)
                        (gethash "model" profile)
                        (gethash "model" selected) model-override
                        (gethash "operator_model_override" selected) t))
                (when (and zdr-override (plusp (length zdr-override)))
                  (unless (member zdr-override
                                  '("require" "allow-non-zdr")
                                  :test #'string=)
                    (error "Invalid OpenRouter ZDR override"))
                  (setf (gethash "zdr" routing)
                        (string= zdr-override "require")
                        (gethash "operator_zdr_override" selected)
                        zdr-override))
                (when (and collection-override
                           (plusp (length collection-override)))
                  (unless (member collection-override '("deny" "allow")
                                  :test #'string=)
                    (error "Invalid OpenRouter data-collection override"))
                  (setf (gethash "data_collection" routing)
                        collection-override
                        (gethash "operator_data_collection_override" selected)
                        collection-override))
                (when (and reasoning-override
                           (plusp (length reasoning-override)))
                  (unless (member reasoning-override
                                  '("profile" "model-default" "enabled"
                                    "disabled")
                                  :test #'string=)
                    (error "Invalid OpenRouter reasoning override"))
                  (cond
                    ((string= reasoning-override "model-default")
                     (remhash "reasoning" selected))
                    ((string= reasoning-override "enabled")
                     (setf (gethash "reasoning" selected)
                           (if (and reasoning-effort
                                    (plusp (length reasoning-effort)))
                               (%conversation-object
                                "effort" reasoning-effort)
                               (%conversation-object "enabled" t))))
                    ((string= reasoning-override "disabled")
                     (setf (gethash "reasoning" selected)
                           (%conversation-object "enabled" nil))))
                  (setf (gethash "operator_reasoning_override" selected)
                        reasoning-override))
                (when (and reasoning-effort
                           (plusp (length reasoning-effort)))
                  ;; The saved effort is valid even when reasoning is disabled;
                  ;; only the enabled branch above applies it to the request.
                  (unless (member reasoning-effort
                                  '("minimal" "low" "medium" "high" "max")
                                  :test #'string=)
                    (error "Invalid OpenRouter reasoning effort")))
                selected)
              profile))))))

(defun %conversation-content-free-receipts-equal-p (left right)
  (and (hash-table-p left) (hash-table-p right)
       (every (lambda (key)
                (equal (gethash key left) (gethash key right)))
              '("node_count" "edge_count" "node_sha256" "vector_sha256"
                "edge_sha256" "vector_binary_encoding"))))

(defun %conversation-prepare-memory-import (destination migrate-p)
  "Create the sealed derived-memory import only under the explicit cutover.

The source is a labelled loopback PostgreSQL clone selected by the operator.
Every source operation is read-only; the destination import is atomic and is
independently audited before the ledger baseline may consume it."
  (when migrate-p
    (let ((destination-state
            (%conversation-call "memory-storage-characterize" destination)))
      (if (eq t (gethash "migration_ready" destination-state))
          (setf *conversation-memory-import-receipt*
                (let ((receipt (make-hash-table :test #'equal)))
                  (setf (gethash "status" receipt) "sealed-import-present"
                        (gethash "node_count" receipt)
                        (gethash "node_count" destination-state)
                        (gethash "edge_count" receipt)
                        (gethash "edge_count" destination-state))
                  receipt))
          (let ((source-kind
                  (or (uiop:getenv "PAI_MEMORY_MIGRATION_SOURCE") ""))
                (host (or (uiop:getenv "PAI_PG_HOST") ""))
                (label (or (uiop:getenv "PAI_DEV_DATABASE_LABEL") "")))
            (unless (and (string= source-kind
                                  "labelled-local-postgres-clone")
                         (member host '("127.0.0.1" "localhost" "::1"
                                        "host.docker.internal")
                                 :test #'string-equal)
                         (string-equal label "clone"))
              (error "Memory cutover requires an explicitly labelled local PostgreSQL clone"))
            (let* ((source (%conversation-call "make-postgres-memory-storage"))
                   (before
                     (%conversation-call "memory-storage-characterize" source))
                   (provenance
                     (%conversation-call
                      "make-memory-embedding-provenance"
                      :embedding-model "nomic-embed-text"
                      :embedding-revision "legacy-unrecorded"
                      :retrieval-embedding-model "nomic-embed-text"
                      :retrieval-embedding-revision "legacy-unrecorded"
                      :vector-dimension 768
                      :revision-evidence
                      "source-schema-absent; clone-config-names-model-only"
                      :approval-scope
                      "operator-approved-development-cutover-2026-08-20")))
              (unwind-protect
                   (progn
                     (unless (eq t (gethash "structurally_ready" before))
                       (error "PostgreSQL clone memory is not structurally ready for import"))
                     (let* ((report
                              (%conversation-call
                               "memory-storage-import-snapshot"
                               destination source provenance))
                            (audit
                              (%conversation-call
                               "memory-storage-audit-snapshot" destination))
                            (after
                              (%conversation-call
                               "memory-storage-characterize" source)))
                       (unless (and
                                (%conversation-content-free-receipts-equal-p
                                 report audit)
                                (= (gethash "node_count" before)
                                   (gethash "node_count" report))
                                (= (gethash "edge_count" before)
                                   (gethash "edge_count" report))
                                (= (gethash "node_count" before)
                                   (gethash "node_count" after))
                                (= (gethash "edge_count" before)
                                   (gethash "edge_count" after)))
                         (error "Memory cutover parity or source-stability proof failed"))
                       (setf *conversation-memory-import-receipt* report)))
                (ignore-errors (%conversation-call "storage-close" source)))))))))

;; Do not run the global init registry here. Its legacy :verify phase includes
;; unrelated integration fixtures that write disposable project rows. This
;; CLI needs exactly the event sequence and selected cognition lifecycle.
(%conversation-startup-phase 4 "Restoring durable conscious state")
(let* ((*standard-output* (make-broadcast-stream))
       (initialize-p
         (string= "1" (or (uiop:getenv "PAI_EVENT_STORAGE_INITIALIZE") "0"))))
  (multiple-value-bind (backend receipt)
      (%conversation-call
       "sqlite-event-authority-prepare"
       (%conversation-required-env "PAI_EVENT_STORAGE_DATABASE")
       (symbol-value (%conversation-symbol "*event-log-file*"))
       :derived-database
       (%conversation-required-env "PAI_DERIVED_STORAGE_DATABASE")
       :agent-id (%conversation-required-env "PAI_AGENT_ID")
       :migrate-p
       (string= "1" (or (uiop:getenv "PAI_EVENT_STORAGE_MIGRATE") "0"))
       :initialize-p initialize-p
       ;; Live startup may restore a verified checkpoint and its tail. A stale
       ;; composition needs explicit offline maintenance. Only the public
       ;; instance launcher admits bounded recovery of an absent database;
       ;; the normal live CLI retains its no-implicit-rebuild policy.
       :rebuild-stale-checkpoint-p nil
       :missing-derived-replay-max-head
       (if (string= "1" (or (uiop:getenv "PAI_SMALL_INSTANCE_REBUILD") "0"))
           10000
           0))
    (declare (ignore backend))
    (setf *conversation-event-authority-receipt* receipt))
  (when initialize-p
    (multiple-value-bind (event-id durable-p)
        (%conversation-call
         "log-event" "instance-genesis"
         (%conversation-object
          "schema_version" 1
          "agent_id" (%conversation-required-env "PAI_AGENT_ID")
          "persona_id" (%conversation-required-env "PAI_CONVERSATION_PERSONA")
          "origin" "new-instance-config"))
      (declare (ignore event-id))
      (unless durable-p (error "Instance genesis event was not durable"))))
  (%conversation-prepare-memory-import
   (symbol-value
    (%conversation-symbol "*SQLITE-EVENT-AUTHORITY-CHECKPOINT-BACKEND*"))
   (string= "1" (or (uiop:getenv "PAI_MEMORY_STORAGE_MIGRATE") "0")))
  ;; Memory selection is startup-only. The one-time migration flag writes a
  ;; hash-closed baseline first; every later run requires that ledger proof.
  (setf *conversation-memory-authority-receipt*
        (%conversation-call
         "memory-ledger-install-authority"
         (symbol-value (%conversation-symbol "*SQLITE-EVENT-AUTHORITY-BACKEND*"))
         (symbol-value
          (%conversation-symbol "*SQLITE-EVENT-AUTHORITY-CHECKPOINT-BACKEND*"))
         :agent-id (%conversation-required-env "PAI_AGENT_ID")
         :migrate-p
         (string= "1" (or (uiop:getenv "PAI_MEMORY_STORAGE_MIGRATE") "0"))
         :initialize-p initialize-p
         :initial-provenance
         (when initialize-p
           (%conversation-call
            "make-memory-embedding-provenance"
            :embedding-model
            (or (uiop:getenv "PAI_MEMORY_EMBEDDING_MODEL") "nomic-embed-text")
            :embedding-revision
            (or (uiop:getenv "PAI_MEMORY_EMBEDDING_REVISION") "operator-configured-v1")
            :retrieval-embedding-model
            (or (uiop:getenv "PAI_MEMORY_RETRIEVAL_EMBEDDING_MODEL")
                "nomic-embed-text")
            :retrieval-embedding-revision
            (or (uiop:getenv "PAI_MEMORY_RETRIEVAL_EMBEDDING_REVISION")
                "operator-configured-v1")
            :vector-dimension
            (parse-integer
             (or (uiop:getenv "PAI_MEMORY_VECTOR_DIMENSION") "768")
             :junk-allowed nil)
            :revision-evidence "new-instance-config-v1"
            :approval-scope "new-instance-genesis"))
         :source-origin
         (let ((source-agent-id
                 (uiop:getenv "PAI_MEMORY_MIGRATION_SOURCE_AGENT_ID"))
               (manifest-sha256
                 (uiop:getenv "PAI_MEMORY_MIGRATION_MANIFEST_SHA256")))
           (when (or source-agent-id manifest-sha256)
             (unless (and source-agent-id manifest-sha256)
               (error "Memory migration source provenance is incomplete"))
             (%conversation-object
              "schema_version" 1 "source_agent_id" source-agent-id
              "source_kind" "migrated-semantic-memory"
              "migration_manifest_sha256" manifest-sha256)))
         ;; This contained CLI does not run the global init registry or any legacy
         ;; autonomous memory worker. All reachable seams are event-first below.
         :postgres-writer-active-p nil))
  ;; Full imported history remains available to retrieval and graph formation.
  ;; Only live cognitive recovery begins after the sealed migration baseline.
  (%conversation-call
   "conscious-work-runtime-configure-head-position"
   (lambda ()
     (%conversation-call
      "storage-head-position"
      (symbol-value (%conversation-symbol "*SQLITE-EVENT-AUTHORITY-BACKEND*"))
      :agent-id (%conversation-required-env "PAI_AGENT_ID")))
   (lambda (after-position through-position event-types)
     (let ((events nil))
       (multiple-value-bind (complete ignored count)
           (%conversation-call
            "storage-map-event-receipts"
            (symbol-value
             (%conversation-symbol "*SQLITE-EVENT-AUTHORITY-BACKEND*"))
            (lambda (receipt)
              (push (shasht:read-json (gethash "event_json" receipt)) events))
            :agent-id (%conversation-required-env "PAI_AGENT_ID")
            :after-position after-position
            :through-position through-position
            :event-types event-types)
         (declare (ignore ignored count))
         (unless complete
           (error "Cognitive work physical tail read is incomplete"))
         (values (nreverse events) through-position))))
   (gethash "baseline_storage_position"
            *conversation-memory-authority-receipt*))
  (unless (string= (or (uiop:getenv "PAI_MIGRATION_ONLY") "") "1")
    ;; Conversation retrieval is selected only after the sealed event-first
    ;; authority has installed. Query embeddings are local; the mature context
    ;; projection performs safety, sensitivity, echo and salience selection
    ;; before the conversation consumer independently checks provider egress.
    (setf (symbol-value (%conversation-symbol "*RETRIEVAL-EMBEDDING-MODE*"))
          :enforced
          (symbol-value
           (%conversation-symbol
            "*CONSCIOUS-CONVERSATION-MEMORY-PROJECTION-FN*"))
          (lambda (prompt)
            (%conversation-call "build-context-projection"
                                prompt :mode :enforced)))
    (when (boundp (%conversation-symbol "*CONTEXT-CURATOR-MODE*"))
      (setf (symbol-value (%conversation-symbol "*CONTEXT-CURATOR-MODE*"))
            :off))
    (%conversation-call "%event-restore-next-id")
    (when *conversation-unified-cli-p*
      (%conversation-call "near-term-intention-load"))
    (%conversation-call "cognition-runtime-configure")
    (%conversation-call "cognition-runtime-install")
    (%conversation-call "cognition-runtime-restore")
    (%conversation-call "cognition-runtime-verify"))
  (setf *conversation-event-checkpoint-receipt*
        (%conversation-call "sqlite-event-authority-checkpoint")))
(%conversation-startup-phase-done 4 "Durable conscious state restored")

(defun %conversation-runtime-settings-seed ()
  "Translate the launcher's non-secret behavioral policy into a one-time seed."
  (labels ((env (name default) (or (uiop:getenv name) default))
           (flag (name &optional (default "0"))
             (string= "1" (env name default)))
           (integer-env (name default)
             (parse-integer (env name (write-to-string default))
                            :junk-allowed nil)))
    (%conversation-object
     "provider_profile" (env "PAI_CONVERSATION_PROVIDER_PROFILE" "contained-cli-provider")
     "model" (env "PAI_CONVERSATION_MODEL" "local-model")
     "openrouter_zdr" (env "PAI_CONVERSATION_ZDR_OVERRIDE" "require")
     "openrouter_data_collection" (env "PAI_CONVERSATION_DATA_COLLECTION_OVERRIDE" "deny")
     "openrouter_reasoning" (env "PAI_CONVERSATION_REASONING_OVERRIDE" "profile")
     "openrouter_reasoning_effort" (env "PAI_CONVERSATION_REASONING_EFFORT" "medium")
     "cost_ceiling_usd" (/ (integer-env "PAI_CONVERSATION_COST_CEILING_MICROUSD" 1) 1000000d0)
     "provider_call_timeout_seconds" *conversation-provider-call-timeout-seconds*
     "provider_connect_timeout_seconds" *conversation-provider-connect-timeout-seconds*
     "provider_inactivity_timeout_seconds" *conversation-provider-inactivity-timeout-seconds*
     "provider_streaming" *conversation-provider-streaming-p*
     "mind_loop" *conversation-loop-mode*
     "recursive_tools" (not (null (member (env "PAI_RECURSIVE_TOOLS" "")
                                            '("host-native-development-v1" "container-development-v1")
                                            :test #'string=)))
     "curiosity_wake_seconds" *conversation-curiosity-wake-seconds*
     "deliberate_curiosity" *conversation-deliberate-curiosity-p*
     "curiosity_reach_out" *conversation-curiosity-reach-out-p*
     "curiosity_briefing" *conversation-curiosity-briefing-p*
     "curiosity_consolidation" *conversation-curiosity-consolidation-p*
     "episodic_memory" *conversation-episodic-memory-p*
     "knowledge_graph_formation" *conversation-knowledge-graph-formation-p*
     "knowledge_graph_budget_usd"
     (/ (integer-env "PAI_CONTEXT_GRAPH_GENERATION_BUDGET_MICROUSD" 2500000) 1000000d0)
     "private_budget_percent" *conversation-private-budget-percent*
     "private_reasoning_effort" *conversation-private-reasoning-effort*
     "loop_trace" (env "PAI_RECURSIVE_LOOP_TRACE" "compact")
     "context_trace" (env "PAI_CONTEXT_TRACE" "off")
     "context_profile" (env "PAI_CONVERSATION_CONTEXT_PROFILE" "solicited-conversation-dev")
     "persona" (env "PAI_CONVERSATION_PERSONA" "dev")
     "embedding_endpoint" (env "PAI_OLLAMA_ENDPOINT" "http://127.0.0.1:11435/api/embeddings")
     "web_enabled" (flag "PAI_WEB_ENABLED")
     "web_address" (env "PAI_WEB_ADDRESS" "127.0.0.1")
     "web_port" (integer-env "PAI_WEB_PORT" 8080)
     "web_file_mutation" (string= "authenticated" (env "PAI_WEB_FILE_MUTATION" ""))
     "show_rejected" (flag "PAI_CONVERSATION_SHOW_REJECTED")
     "show_memory_context" (flag "PAI_CONVERSATION_SHOW_MEMORY_CONTEXT")
     "affect_baseline_event_id" *conversation-affect-baseline-event-id*)))

(defun %conversation-adopt-durable-startup-settings ()
  "Make durable desired values authoritative before provider policy is built.

Durable values winning is deliberate: a runtime setting an operator changed
should survive a restart rather than being silently reverted by whatever
flags the last launch happened to carry. The hazard is the reverse
direction -- a launch flag the operator believes is authoritative being
replaced without a word. So every divergence between what this launch asked
for and what the ledger already holds is reported, naming both values and
how to change the durable one."
  (labels ((setting (key) (%conversation-call "runtime-settings-value" key))
           (put (name value)
             (let* ((requested (uiop:getenv name))
                    (adopted (cond ((eq value t) "1") ((null value) "0")
                                   (t (princ-to-string value)))))
               (when (and (stringp requested) (plusp (length requested))
                          (not (string= requested adopted)))
                 (format t "~&[settings] ~a: this launch asked for ~a; the durable setting is ~a and wins. Change it with the runtime setting, not the launch flag.~%"
                         name requested adopted))
               (setf (uiop:getenv name) adopted))))
    (setf *conversation-loop-mode* (setting "mind_loop")
          *conversation-curiosity-wake-seconds* (setting "curiosity_wake_seconds")
          *conversation-deliberate-curiosity-p* (setting "deliberate_curiosity")
          *conversation-curiosity-reach-out-p* (setting "curiosity_reach_out")
          *conversation-curiosity-briefing-p* (setting "curiosity_briefing")
          *conversation-curiosity-consolidation-p* (setting "curiosity_consolidation")
          *conversation-episodic-memory-p* (setting "episodic_memory")
          *conversation-knowledge-graph-formation-p* (setting "knowledge_graph_formation")
          *conversation-private-budget-percent* (setting "private_budget_percent")
          *conversation-private-reasoning-effort* (setting "private_reasoning_effort")
          *conversation-provider-call-timeout-seconds* (setting "provider_call_timeout_seconds")
          *conversation-provider-connect-timeout-seconds* (setting "provider_connect_timeout_seconds")
          *conversation-provider-inactivity-timeout-seconds* (setting "provider_inactivity_timeout_seconds")
          *conversation-provider-streaming-p* (setting "provider_streaming"))
    (put "PAI_CONVERSATION_PROVIDER_PROFILE" (setting "provider_profile"))
    (put "PAI_CONVERSATION_MODEL" (setting "model"))
    (put "PAI_CONVERSATION_MODEL_OVERRIDE" (setting "model"))
    (put "PAI_CONVERSATION_ZDR_OVERRIDE" (setting "openrouter_zdr"))
    (put "PAI_CONVERSATION_DATA_COLLECTION_OVERRIDE" (setting "openrouter_data_collection"))
    (put "PAI_CONVERSATION_REASONING_OVERRIDE" (setting "openrouter_reasoning"))
    (put "PAI_CONVERSATION_REASONING_EFFORT" (setting "openrouter_reasoning_effort"))
    (put "PAI_CONVERSATION_COST_CEILING_MICROUSD"
         (round (* 1000000 (setting "cost_ceiling_usd"))))
    (put "PAI_CONTEXT_GRAPH_GENERATION_BUDGET_MICROUSD"
         (round (* 1000000 (setting "knowledge_graph_budget_usd"))))
    (put "PAI_RECURSIVE_LOOP_TRACE" (setting "loop_trace"))
    (let ((enabled (setting "web_file_mutation")))
      (put "PAI_WEB_FILE_MUTATION" (if enabled "authenticated" "disabled"))
      (set (%conversation-symbol "*web-file-mutation-authority*")
           (if enabled :authenticated-web :disabled)))
    (put "PAI_CONTEXT_TRACE" (setting "context_trace"))))

(when (string= (or (uiop:getenv "PAI_MIGRATION_ONLY") "") "1")
  (let ((storage (%conversation-call "event-authority-report")))
    (format t "~&Migration event storage: ~a (~a)~%"
            (gethash "database" storage) (gethash "authority" storage)))
  (when *conversation-memory-import-receipt*
    (format t "Migration memory import: ~a; nodes ~d; edges ~d~%"
            (or (gethash "status" *conversation-memory-import-receipt*)
                "imported-and-audited")
            (gethash "node_count" *conversation-memory-import-receipt*)
            (gethash "edge_count" *conversation-memory-import-receipt*)))
  (format t "Migration-only startup completed; no cognition, provider policy, web, or conversation worker was started.~%")
  (finish-output)
  (sb-ext:exit :code 0))

(%conversation-call "runtime-settings-initialize"
                    (%conversation-runtime-settings-seed))
(%conversation-adopt-durable-startup-settings)

(%conversation-startup-phase 5 "Applying contained provider policy")
(setf (symbol-value (%conversation-symbol "*autonomous-write-mode*")) :paused)
(setf (symbol-value
       (%conversation-symbol "*conscious-conversation-budget-profile*"))
      (%conversation-load-context-profile))

(let ((provider-profile (%conversation-load-provider-profile)))
  (when provider-profile
    (let ((ceiling-microusd
            (parse-integer
             (%conversation-required-env
              "PAI_CONVERSATION_COST_CEILING_MICROUSD")
             :junk-allowed nil)))
      (unless (plusp ceiling-microusd)
        (error "Conversation cost ceiling must be positive"))
      (setf (symbol-value
            (%conversation-symbol "*conscious-conversation-provider-profile*"))
            provider-profile
            (symbol-value
             (%conversation-symbol "*conscious-conversation-cost-ceiling-usd*"))
            (/ ceiling-microusd 1000000d0)))))

(let* ((mode-symbol (%conversation-symbol "*llm-debug-capture-mode*"))
       (directory-symbol
         (%conversation-symbol "*llm-debug-capture-directory*"))
       (trace-symbol
         (%conversation-symbol
          "*conscious-conversation-context-trace-fn*"))
       (mode (symbol-value mode-symbol)))
  ;; Compose the generic private diagnostic adapter at the final solicited
  ;; provider boundary. The runtime owns no file path or observability module.
  (setf (symbol-value trace-symbol)
        (lambda (messages metadata provider-thunk)
          (%conversation-call "llm-debug-capture-call"
                              messages "public" provider-thunk
                              :metadata metadata)))
  (unless (eq mode :off)
    (format t
            "~&[private context trace: ~a; directory ~a; retention 24h]~%"
            (string-downcase (symbol-name mode))
            (namestring (symbol-value directory-symbol)))
    (when (member mode '(:full :on))
      (format t
              "[warning: full trace contains credential-redacted private model context and responses]~%"))))

(%conversation-startup-phase-done 5 "Contained provider policy applied")

;; The first Gate-B tool has one explicit, read-only filesystem capability.
;; Its root is launcher policy, never model-selected and never the host root.
(%conversation-call
 "conscious-file-search-configure"
 (%conversation-required-env "PAI_FILE_SEARCH_ROOT"))

;; One channel-neutral coordinator owns durable admission and serialized
;; inference for this mind. CLI waits synchronously; web returns its receipt
;; immediately and observes status through the same worker.
(defparameter *conversation-progress-lock*
  (bt:make-lock "conversation cli progress"))
(defvar *conversation-progress-active-p* nil)
(defvar *conversation-progress-interaction-id* nil)
(defvar *conversation-progress-stage* nil)
(defvar *conversation-progress-turn-started* 0)
(defvar *conversation-progress-stage-started* 0)
(defvar *conversation-progress-last-heartbeat* 0)
(defvar *conversation-progress-running-p* t)
(defvar *conversation-progress-thread* nil)
(defvar *conversation-provider-progress-generation-id* nil)
(defvar *conversation-provider-progress-last-emitted* 0)

(defun %conversation-progress-seconds-since (started)
  (/ (- (get-internal-real-time) started)
     (coerce internal-time-units-per-second 'double-float)))

(defun %conversation-progress-label (phase)
  (cond
    ((string= phase "admission") "verifying durable admission")
    ((string= phase "interaction_claimed") "interaction claimed by the cognitive worker")
    ((string= phase "cognitive_quantum") "selecting the next cognitive step")
    ((string= phase "context_open") "assembling context and retrieving memory")
    ((string= phase "request_journal") "recording the bounded model request")
    ((string= phase "provider") "waiting for the model")
    ((string= phase "response_journal") "recording the model response receipt")
    ((string= phase "captured_parse") "parsing the structured proposal")
    ((string= phase "captured_commit") "validating and committing the proposal")
    ((string= phase "publication_validation") "validating publication")
    ((string= phase "reply_commit") "committing the authorized reply")
    ((string= phase "tool_execution") "running the bounded read-only tool")
    ((string= phase "continuation") "scheduling another cognitive quantum")
    ((string= phase "cognitive_work") "finishing cognitive work")
    (t (format nil "working (~a)" phase))))

(defun %conversation-progress-write (message)
  (let* ((ansi-p (string= "1" (or (uiop:getenv "PAI_CLI_ANSI") "")))
         (escape (code-char 27))
         (prefix (if ansi-p (format nil "~c[2;3;90m" escape) ""))
         (suffix (if ansi-p (format nil "~c[0m" escape) ""))
         (elapsed (if *conversation-progress-active-p*
                      (%conversation-progress-seconds-since
                       *conversation-progress-turn-started*)
                      0d0)))
    (format t "~&~a[turn +~,1fs] ~a~a~%" prefix elapsed message suffix)
    (finish-output)))

(defun %conversation-provider-progress-private-p ()
  (let ((symbol (%conversation-symbol
                 "*CONSCIOUS-CONVERSATION-PRIVATE-PROVIDER-CALL-P*")))
    (and (boundp symbol) (symbol-value symbol))))

(defun %conversation-provider-progress-usage-token
    (usage key &optional nested-key)
  (let* ((container (if nested-key
                        (and (hash-table-p usage)
                             (gethash nested-key usage))
                        usage))
         (value (and (hash-table-p container) (gethash key container))))
    (and (integerp value) value)))

(defun %conversation-provider-progress-display (detail private-p)
  (let* ((usage (gethash "usage" detail))
         (exact (%conversation-provider-progress-usage-token
                 usage "completion_tokens"))
         (reasoning-exact
           (%conversation-provider-progress-usage-token
            usage "reasoning_tokens" "completion_tokens_details"))
         (estimate (gethash "estimated_output_tokens" detail 0))
         (reasoning-p
           (plusp (gethash "reasoning_characters" detail 0)))
         (activity (if reasoning-p "reasoning" "responding"))
         (prefix (if private-p "private cognition" "pAI")))
    (cond
      (exact
       (format nil "~a model response complete · ~:d completion tokens~@[ · ~:d reasoning tokens~]"
               prefix exact reasoning-exact))
      ((plusp estimate)
       (format nil "~a is ~a · ~~ ~:d tokens received"
               prefix activity estimate))
      ((eq t (gethash "heartbeat" detail))
       (format nil "~a provider is still processing" prefix))
      (t (format nil "~a is ~a" prefix activity)))))

(defun %conversation-provider-progress-observer (detail)
  "Throttle content-free streamed token progress for console and web views."
  (when (hash-table-p detail)
    (let* ((now (get-internal-real-time))
           (generation (gethash "generation_id" detail :null))
           (usage (gethash "usage" detail))
           (estimate (gethash "estimated_output_tokens" detail 0))
           (exact-p (hash-table-p usage))
           (emit-p nil)
           (private-p (%conversation-provider-progress-private-p))
           (turn-id nil)
           (presentation nil))
      (bt:with-lock-held (*conversation-progress-lock*)
        (unless (equal generation *conversation-provider-progress-generation-id*)
          (setf *conversation-provider-progress-generation-id* generation
                *conversation-provider-progress-last-emitted* 0))
        (when (or exact-p
                  (zerop *conversation-provider-progress-last-emitted*)
                  (>= (%conversation-progress-seconds-since
                       *conversation-provider-progress-last-emitted*)
                      2d0))
          (setf emit-p t
                *conversation-provider-progress-last-emitted* now
                turn-id *conversation-progress-interaction-id*
                presentation
                (%conversation-provider-progress-display detail private-p))))
      (when emit-p
        (bt:with-lock-held (*conversation-progress-lock*)
          (if private-p
              (progn
                (format t "~&[private stream] ~a~%" presentation)
                (finish-output))
              (%conversation-progress-write presentation)))
        (when (fboundp (%conversation-symbol
                        "WEB-TERMINAL-PRESENT-STREAM-PROGRESS"))
          (let ((web-detail
                  (%conversation-object
                       "generation_id" generation
                       "display" presentation
                       "private" (if private-p t nil)
                       "estimated_output_tokens" estimate
                       "output_characters"
                       (gethash "output_characters" detail 0)
                       "reasoning_characters"
                       (gethash "reasoning_characters" detail 0)
                       "tool_argument_characters"
                       (gethash "tool_argument_characters" detail 0)
                       "heartbeat" (if (eq t (gethash "heartbeat" detail))
                                       t nil)
                       "usage" (if exact-p usage :null))))
            (%conversation-call
             "web-terminal-present-stream-progress" web-detail
             :turn-id turn-id)))))))

(setf (symbol-value
       (%conversation-symbol
        "*CONSCIOUS-CONVERSATION-PROVIDER-PROGRESS-OBSERVER*"))
      #'%conversation-provider-progress-observer)

(defparameter *conversation-loop-trace-mode*
  (let ((mode (string-downcase
               (or (uiop:getenv "PAI_RECURSIVE_LOOP_TRACE") "compact"))))
    (unless (member mode '("off" "compact" "full") :test #'string=)
      (error "PAI_RECURSIVE_LOOP_TRACE must be off, compact, or full"))
    mode))

(defun %conversation-loop-trace-preview (value maximum)
  (let* ((text (if (stringp value) value (format nil "~a" value)))
         (flat (substitute #\Space #\Return
                           (substitute #\Space #\Newline text))))
    (if (<= (length flat) maximum)
        flat
        (concatenate 'string (subseq flat 0 (max 0 (- maximum 3))) "..."))))

(defun %conversation-loop-trace-tool-input (detail)
  (let ((encoded (gethash "tool_arguments" detail "")))
    (handler-case
        (let* ((arguments (shasht:read-json encoded))
               (value (or (gethash "command" arguments)
                          (gethash "form" arguments)
                          encoded)))
          (%conversation-loop-trace-preview
           value (if (string= *conversation-loop-trace-mode* "full") 500 180)))
      (error () (%conversation-loop-trace-preview encoded 180)))))

(defun %conversation-progress-activity (detail)
  (unless (or (string= *conversation-loop-trace-mode* "off")
              (not (hash-table-p detail)))
    (let ((kind (gethash "kind" detail "")))
      (cond
        ((string= kind "model-request")
         (%conversation-progress-write
          (format nil "  model step · ~d messages · ~d characters~:[ · tools available~; · final synthesis~]"
                  (gethash "message_count" detail 0)
                  (gethash "message_characters" detail 0)
                  (eq t (gethash "final_synthesis" detail)))))
        ((string= kind "tool-start")
         (%conversation-progress-write
          (format nil "    ~a  ~a"
                  (gethash "tool_name" detail "tool")
                  (%conversation-loop-trace-tool-input detail))))
        ((string= kind "tool-result")
         (%conversation-progress-write
          (if (string= *conversation-loop-trace-mode* "full")
              (format nil "      -> ~d characters · ~a"
                      (gethash "result_characters" detail 0)
                      (%conversation-loop-trace-preview
                       (gethash "content" detail "") 500))
              (format nil "      -> completed · ~d characters"
                      (gethash "result_characters" detail 0)))))
        ((string= kind "tool-suppressed")
         (%conversation-progress-write
          (format nil "    warning: repeated ~a call suppressed; tools closed for final synthesis"
                  (gethash "tool_name" detail "tool"))))
        ((string= kind "reply-ready")
         (%conversation-progress-write
          (format nil "  final reply ready~@[ · input ~d tokens~]~@[ · session $~,6f~]"
                  (let ((value (gethash "input_tokens" detail)))
                    (and (integerp value) value))
                  (let ((value (gethash "session_cost_usd" detail)))
                    (and (numberp value) value)))))
        ((string= kind "budget-paused")
         (%conversation-progress-write
          "  budget paused before another provider request; durable tool evidence retained"))
        ((string= kind "model-failed")
         (%conversation-progress-write
          (format nil "  model boundary failed: ~a"
                       (gethash "reason" detail "unknown provider failure"))))))))

(defun %conversation-web-activity-text (detail)
  "Render recursive activity for a human observer without exposing a second
metadata parser or asking the browser to understand runtime structures."
  (let ((kind (and (hash-table-p detail) (gethash "kind" detail "activity"))))
    (cond
      ((string= kind "model-request")
       (format nil "model step · ~d messages · ~d characters"
               (gethash "message_count" detail 0)
               (gethash "message_characters" detail 0)))
      ((string= kind "tool-start")
       (format nil "~a · ~a"
               (gethash "tool_name" detail "tool")
               (%conversation-loop-trace-tool-input detail)))
      ((string= kind "tool-result")
       (format nil "~a completed · ~d characters"
               (gethash "tool_name" detail "tool")
               (gethash "result_characters" detail 0)))
      ((string= kind "tool-suppressed")
       (format nil "repeated ~a call suppressed; final synthesis requested"
               (gethash "tool_name" detail "tool")))
      ((string= kind "reply-ready") "final reply ready")
      ((string= kind "budget-paused") "budget paused; durable evidence retained")
      ((string= kind "model-failed")
       (format nil "model boundary failed · ~a"
               (gethash "reason" detail "unknown provider failure")))
      (t kind))))

(defun %conversation-progress-begin (status item)
  (bt:with-lock-held (*conversation-progress-lock*)
    (let ((interaction-id (gethash "interaction_id" item)))
      (cond
        ((and *conversation-progress-active-p*
              (equal *conversation-progress-interaction-id* interaction-id))
         nil)
        (*conversation-progress-active-p*
         (%conversation-progress-write
          "another interaction was durably queued behind this turn"))
        (t
         (let ((now (get-internal-real-time)))
           (setf *conversation-progress-active-p* t
                 *conversation-progress-interaction-id* interaction-id
                 *conversation-progress-stage* status
                 *conversation-progress-turn-started* now
                 *conversation-progress-stage-started* now
                 *conversation-progress-last-heartbeat* now)
           (%conversation-progress-write
            (if (string= status "queued")
                "durably queued behind earlier work"
                "durably admitted"))))))))

(defun %conversation-progress-update (status phase elapsed-ms)
  (declare (ignore elapsed-ms))
  (bt:with-lock-held (*conversation-progress-lock*)
    (when *conversation-progress-active-p*
      (let ((now (get-internal-real-time))
            (label (%conversation-progress-label phase)))
        (cond
          ((string= status "started")
           (setf *conversation-progress-stage* phase
                 *conversation-progress-stage-started* now
                 *conversation-progress-last-heartbeat* now)
           (%conversation-progress-write label))
          ((string= status "failed")
           (%conversation-progress-write (format nil "failed while ~a" label)))
          ((member phase '("provider" "tool_execution") :test #'string=)
           (%conversation-progress-write
            (format nil "completed: ~a" label))))))))

(defun %conversation-progress-interaction (status item)
  ;; Private roots have their own bounded diagnostics. They must never acquire
  ;; or retain the operator-turn presentation slot: that UI flag is neither
  ;; cognitive authority nor a quiet-time predicate.
  (unless (string= "private" (gethash "channel" item ""))
   (when (and (plusp *conversation-curiosity-wake-seconds*)
             (or (member status '("accepted" "queued") :test #'string=)
                 (member status '("replied" "no-reply" "withheld" "failed"
                                  "outcome-unknown" "provider-call-failed"
                                  "paused-budget" "provider-response-invalid"
                                  "provider-response-truncated")
                         :test #'string=)))
    ;; Only operator-root lifecycle boundaries reset quiet time. Internal
    ;; journal appends and private review activity deliberately do not.
    (bt:with-lock-held (*conversation-curiosity-lock*)
      (setf *conversation-curiosity-last-activity* (get-internal-real-time))
      (bt:condition-notify *conversation-curiosity-condition*)))
   (cond
    ((string= status "activity")
     ;; Activity is presentation only. The recursive runtime emits it after
     ;; the corresponding model/tool fact is durable.
     nil)
    ((member status '("accepted" "queued") :test #'string=)
     (%conversation-progress-begin status item))
    ((string= status "thinking")
     ;; A web interaction may have queued while another interaction owned the
     ;; terminal trace. Start a fresh trace when that queued item is selected.
     (unless (bt:with-lock-held (*conversation-progress-lock*)
               (and *conversation-progress-active-p*
                    (equal *conversation-progress-interaction-id*
                           (gethash "interaction_id" item))))
       (%conversation-progress-begin "accepted" item))
     (%conversation-progress-update "started" "interaction_claimed" 0))
    ((member status '("replied" "no-reply" "withheld" "failed"
                      "outcome-unknown" "provider-call-failed"
                      "paused-budget"
                      "provider-response-invalid" "provider-response-truncated")
             :test #'string=)
     (bt:with-lock-held (*conversation-progress-lock*)
       (when (and *conversation-progress-active-p*
                  (equal *conversation-progress-interaction-id*
                         (gethash "interaction_id" item)))
         (%conversation-progress-write
          (if (member status '("replied" "no-reply") :test #'string=)
              "turn complete"
              (format nil "turn stopped: ~a" status)))
         (setf *conversation-progress-active-p* nil
               *conversation-progress-interaction-id* nil
               *conversation-progress-stage* nil)))))))

(defun %conversation-progress-loop ()
  (loop while *conversation-progress-running-p*
        do (sleep 1)
           (bt:with-lock-held (*conversation-progress-lock*)
             (when (and *conversation-progress-active-p*
                        *conversation-progress-stage*
                        (>= (%conversation-progress-seconds-since
                             *conversation-progress-last-heartbeat*)
                            10d0))
               (setf *conversation-progress-last-heartbeat*
                     (get-internal-real-time))
               (%conversation-progress-write
                (format nil "still ~a (~,0fs in this stage)"
                        (%conversation-progress-label
                         *conversation-progress-stage*)
                        (%conversation-progress-seconds-since
                         *conversation-progress-stage-started*)))))))

(setf *conversation-progress-thread*
      (bt:make-thread #'%conversation-progress-loop
                      :name "conversation-cli-progress"))
(%conversation-call "conscious-conversation-progress-configure"
                    #'%conversation-progress-update)

(defun %conversation-episode-graph-partition ()
  (unless (and (boundp (%conversation-symbol
                        "*sqlite-event-authority-backend*"))
               (boundp (%conversation-symbol
                        "*sqlite-event-authority-checkpoint-backend*")))
    (error "Episode graph storage composition is unavailable"))
  (let ((event-backend
          (symbol-value
           (%conversation-symbol "*sqlite-event-authority-backend*")))
        (derived-backend
          (symbol-value
           (%conversation-symbol
            "*sqlite-event-authority-checkpoint-backend*")))
        (agent-id (%conversation-required-env "PAI_AGENT_ID"))
        (persona-id
          (gethash "persona_id" (%conversation-call
                                  "%conversation-persona-profile"))))
    (unless (and event-backend derived-backend)
      (error "Episode graph storage composition is not active"))
    (values event-backend derived-backend agent-id persona-id)))

(defun %conversation-episode-graph-maintain ()
  (multiple-value-bind (event-backend derived-backend agent-id persona-id)
      (%conversation-episode-graph-partition)
    (%conversation-call
     "conversation-episode-graph-synchronize"
     event-backend derived-backend agent-id persona-id)))

(defun %conversation-episode-graph-inspect ()
  (multiple-value-bind (event-backend derived-backend agent-id persona-id)
      (%conversation-episode-graph-partition)
    (let ((boundary
            (%conversation-call "storage-authority-boundary"
                                event-backend :agent-id agent-id)))
      (%conversation-call
       "conversation-episode-graph-inspect"
       derived-backend agent-id persona-id
       :event-storage-id (gethash "storage_id" boundary)))))

(defun %conversation-knowledge-graph-form-one ()
  (multiple-value-bind (event-backend derived-backend agent-id persona-id)
      (%conversation-episode-graph-partition)
    (%conversation-call
     "conscious-context-graph-formation-step"
     event-backend derived-backend agent-id persona-id)))

(defun %conversation-knowledge-graph-search-storage
    (request linked-source-ids linked-evidence-event-ids)
  (declare (ignore linked-source-ids linked-evidence-event-ids))
  (multiple-value-bind (event-backend derived-backend agent-id persona-id)
      (%conversation-episode-graph-partition)
    (%conversation-call "conscious-context-graph-search"
                        event-backend agent-id persona-id request derived-backend)))

(defun %conversation-knowledge-graph-search (request)
  "One current authority projection for native search and the graph explorer."
  (%conversation-knowledge-graph-search-storage request #() #()))

(defun %conversation-knowledge-graph-confirmation-candidate (fact-id)
  "Resolve one exact inference; the recursive tool records, but cannot apply, it."
  (multiple-value-bind (event-backend derived-backend agent-id persona-id)
      (%conversation-episode-graph-partition)
    (%conversation-call
     "conscious-context-graph-confirmation-candidate"
     event-backend agent-id persona-id fact-id derived-backend)))

(defun %conversation-knowledge-graph-proposal-result (proposal-event-id)
  "Synchronize one append-only conversational proposal and return its outcome."
  (multiple-value-bind (event-backend derived-backend agent-id persona-id)
      (%conversation-episode-graph-partition)
    (%conversation-call
     "conscious-context-graph-proposal-result"
     event-backend agent-id persona-id proposal-event-id derived-backend)))

(defun %conversation-knowledge-graph-attention-context
    (frame semantic-candidates episode-candidates character-budget)
  "Bounded current entities, admitted facts and role-labeled source windows."
  (declare (ignore semantic-candidates episode-candidates))
  (multiple-value-bind (event-backend derived-backend agent-id persona-id)
      (%conversation-episode-graph-partition)
    (%conversation-call "conscious-context-graph-attention-context"
                        event-backend agent-id persona-id frame character-budget
                        derived-backend)))

(let ((endpoint (or (uiop:getenv "PAI_CONVERSATION_ENDPOINT") ""))
      (model (or (uiop:getenv "PAI_CONVERSATION_MODEL") ""))
      (context-profile (%conversation-load-context-profile))
      (recursive-tools-p
        (member (or (uiop:getenv "PAI_RECURSIVE_TOOLS") "")
                '("host-native-development-v1" "container-development-v1")
                :test #'string=)))
  (if (string= *conversation-loop-mode* "recursive")
      (progn
        (when (and *conversation-knowledge-graph-rebuild-only-p*
                   (not *conversation-knowledge-graph-formation-p*))
          (error "Knowledge graph rebuild-only mode requires formation"))
        (setf (symbol-value
               (%conversation-symbol
                "*CONSCIOUS-CONVERSATION-GRAPH-CONTEXT-FN*"))
              (and *conversation-episodic-memory-p*
                   #'%conversation-knowledge-graph-attention-context))
        (when recursive-tools-p
          (%conversation-call
           "recursive-primitive-tools-configure"
           :workspace-root
           (%conversation-required-env "PAI_RECURSIVE_WORKSPACE_ROOT")
           :bash-executable
           (%conversation-required-env "PAI_RECURSIVE_BASH")
           :lisp-eval-review-log
           (%conversation-required-env "PAI_LISP_EVAL_REVIEW_LOG")))
        (%conversation-call
         "conscious-recursive-mind-configure"
         :agent-id (%conversation-required-env "PAI_AGENT_ID")
         :endpoint endpoint :model model :context-profile context-profile
         :tools-enabled-p recursive-tools-p
         :curiosity-enabled-p
         (and (not *conversation-knowledge-graph-rebuild-only-p*)
              (plusp *conversation-curiosity-wake-seconds*))
         :deliberate-curiosity-enabled-p
         *conversation-deliberate-curiosity-p*
         :curiosity-reach-out-enabled-p
         *conversation-curiosity-reach-out-p*
         :curiosity-briefing-enabled-p
         *conversation-curiosity-briefing-p*
         :curiosity-consolidation-enabled-p
         *conversation-curiosity-consolidation-p*
         :episodic-memory-enabled-p
         (and (not *conversation-knowledge-graph-rebuild-only-p*)
              *conversation-episodic-memory-p*)
         :episode-provider-profile-fn
         (and (not *conversation-knowledge-graph-rebuild-only-p*)
              (symbol-function
               (%conversation-symbol
                "%CONVERSATION-EPISODE-PROVIDER-PROFILE")))
         :episode-graph-maintenance-fn
         ;; The grounded graph supersedes the old episode-to-concept graph as
         ;; a knowledge surface. Episodic recall remains independently active;
         ;; do not keep rewriting or retrying the legacy derived projection
         ;; when grounded formation owns graph retrieval.
         (and (not *conversation-knowledge-graph-formation-p*)
              #'%conversation-episode-graph-maintain)
         :episode-graph-inspect-fn
         #'%conversation-episode-graph-inspect
         :knowledge-graph-formation-fn
         (and *conversation-knowledge-graph-formation-p*
              #'%conversation-knowledge-graph-form-one)
         :graph-search-fn
         #'%conversation-knowledge-graph-search
         :graph-confirmation-fn
         #'%conversation-knowledge-graph-confirmation-candidate
         :graph-proposal-fn
         #'%conversation-knowledge-graph-proposal-result
         :fleet-peers-fn (symbol-function (%conversation-symbol "fleet-peer-list"))
         :fleet-message-fn (symbol-function (%conversation-symbol "fleet-board-post"))
         :fleet-board-read-fn
         (symbol-function (%conversation-symbol "fleet-board-read-or-list"))
         :fleet-board-reply-fn
         (symbol-function (%conversation-symbol "fleet-board-reply"))
         :fleet-notification-flush-fn
         (symbol-function (%conversation-symbol "fleet-board-notification-flush-one"))
         :finding-memory-fn
         (and (fboundp (%conversation-symbol "memory-write-node"))
              (symbol-function (%conversation-symbol "memory-write-node")))
         :private-budget-percent *conversation-private-budget-percent*
         :private-reasoning-effort *conversation-private-reasoning-effort*
         :working-summary-backend
         (symbol-value
          (%conversation-symbol "*sqlite-event-authority-checkpoint-backend*"))
         :review-ready-fn
         (and (not *conversation-knowledge-graph-rebuild-only-p*)
              (plusp *conversation-curiosity-wake-seconds*)
              (lambda ()
                (>= (%conversation-startup-elapsed-seconds
                    *conversation-curiosity-last-activity*)
                    *conversation-curiosity-wake-seconds*)))
         :recovery-start-storage-position
         (gethash "baseline_storage_position"
                  *conversation-memory-authority-receipt*)
         :tool-executor
         (and recursive-tools-p
              (symbol-function
               (%conversation-symbol "recursive-primitive-tool-execute")))
         :observer-fn
         (lambda (status item result)
           (when (string= status "operational-anomaly")
             (bt:with-lock-held (*conversation-progress-lock*)
               (format t "~&[operational anomaly observed; cognition continued] ~a~%"
                       (gethash "reason" result "unknown"))
               (finish-output))
             (when (fboundp (%conversation-symbol
                              "web-terminal-present-operational-notice"))
               (%conversation-call
                "web-terminal-present-operational-notice"
                (cond
                  ((string= "generation-reconciled"
                            (gethash "accounting_status" result ""))
                   (format nil
                           "An interrupted provider call was reconciled against its generation record ($~,6f actual cost); asynchronous thinking continued."
                           (gethash "charged_cost_usd" result 0d0)))
                  ((string= "generation-reconciliation-pending"
                            (gethash "accounting_status" result ""))
                   "An interrupted provider call is awaiting its exact OpenRouter cost record. No estimated spend was booked; provider work will resume automatically after settlement.")
                  (t
                   (format nil
                           "Provider accounting anomaly (~a). The conservative admitted maximum ($~,6f) was charged; asynchronous thinking continued. The agent recorded this as an observation."
                           (gethash "reason" result "unknown")
                           (gethash "charged_cost_usd" result 0d0))))
                :turn-id (gethash "interaction_id" item))))
           (when (string= status "projection-anomaly")
             (let* ((kind (gethash "kind" item "projection"))
                    (grounded-p
                      (string= kind "knowledge-graph-formation"))
                    (label (if grounded-p
                               "grounded graph formation"
                               "episodic graph maintenance")))
               (bt:with-lock-held (*conversation-progress-lock*)
                 (format t "~&[~a] failed (~a); cognition continued and retry remains scheduled~%"
                         label (gethash "detail" result
                                        (gethash "reason" result "unknown")))
                 (finish-output))
               (when (fboundp (%conversation-symbol
                                "web-terminal-present-operational-notice"))
                 (%conversation-call
                  "web-terminal-present-operational-notice"
                  (format nil
                          "~:(~a~) failed (~a). Conversation and private thinking continued; a later quiet quantum will retry."
                          label (gethash "detail" result
                                         (gethash "reason" result "unknown")))))))
           (when (string= status "autonomous-reach-out")
             (bt:with-lock-held (*conversation-progress-lock*)
               (format t "~&pAI> ~a~%" (gethash "content" result ""))
               (finish-output))
             (when (fboundp (%conversation-symbol "%v2-broadcast"))
               (%conversation-call "%v2-broadcast" "final"
                                   (gethash "content" result "")
                                   :turn-id (gethash "interaction_id" item))))
           (when (string= status "activity")
             (%conversation-progress-activity result)
             (when (fboundp (%conversation-symbol
                              "web-terminal-present-activity"))
               (%conversation-call
                "web-terminal-present-activity"
                (%conversation-web-activity-text result)
                :private-p (string= "private" (gethash "channel" item ""))
                :turn-id (gethash "interaction_id" item))))
           (%conversation-progress-interaction status item)
           (when (and (fboundp (%conversation-symbol "%v2-broadcast"))
                      (hash-table-p item)
                      (string= "web" (gethash "channel" item "")))
             (cond
               ((string= status "accepted")
                (%conversation-call
                 "%v2-broadcast" "user"
                 (%conversation-call "%v2-user-display"
                                     (gethash "content" item ""))
                 :turn-id (gethash "interaction_id" item)))
               ((member status '("queued" "thinking")
                        :test #'string=)
                (%conversation-call "%v2-broadcast" status item
                                    :turn-id (gethash "interaction_id" item)))
               ((string= status "replied")
                (%conversation-call "%v2-broadcast" "final"
                                    (gethash "content" result "")
                                    :turn-id (gethash "interaction_id" item)))
               ((member status '("failed" "outcome-unknown" "paused-budget"
                                  "provider-response-invalid"
                                  "provider-response-truncated")
                        :test #'string=)
                (%conversation-call "%v2-broadcast" "error" result
                                    :turn-id (gethash "interaction_id" item))))))))
      (let ((work-profile (%conversation-load-work-profile)))
        (%conversation-validate-work-context-compatibility
         context-profile work-profile)
        (%conversation-call
         "conscious-conversation-work-configure"
         :agent-id (%conversation-required-env "PAI_AGENT_ID")
         :profile work-profile
         :runtime-plan
         (%conversation-compile-runtime-plan context-profile work-profile)
         :progress-fn #'%conversation-progress-update
         :turn-fn
         (lambda (prompt &key admitted-event-id channel interaction-id work-id)
           (%conversation-call
            "conscious-conversation-turn" prompt
            :endpoint endpoint :model model :channel channel
            :admitted-event-id admitted-event-id :interaction-id interaction-id
            :work-id work-id)))
        (%conversation-call "conscious-conversation-work-start")
        (%conversation-call
         "conscious-interaction-configure"
         :agent-id (%conversation-required-env "PAI_AGENT_ID")
         :queue-capacity 32
         :metadata-fn
         (lambda () (%conversation-call "conscious-conversation-admission-metadata"))
         :executor-fn
         (lambda (prompt &key admitted-event-id channel interaction-id)
           (%conversation-call
            "conscious-conversation-work-run" prompt :channel channel
            :admitted-event-id admitted-event-id :interaction-id interaction-id))
         :observer-fn
         (lambda (status item result)
           (%conversation-progress-interaction status item)
           (when (fboundp (%conversation-symbol "%v2-broadcast"))
             (cond
               ((string= status "replied")
                (%conversation-call "%v2-broadcast" "final"
                                    (gethash "content" result)))
               ((member status '("accepted" "queued" "thinking") :test #'string=)
                (%conversation-call
                 "%v2-broadcast" status
                 (%conversation-call
                  "obj" "interaction_id" (gethash "interaction_id" item)
                  "user_event_id" (gethash "user_event_id" item))))
               ((member status '("failed" "outcome-unknown") :test #'string=)
                (%conversation-call
                 "%v2-broadcast" "error"
                 (%conversation-call
                  "obj" "interaction_id" (gethash "interaction_id" item)
                  "status" status)))))))
        (%conversation-call "conscious-interaction-start"))))

(defun %conversation-runtime-setting-apply (key value)
  "Adopt one already-validated durable value at a safe runtime boundary."
  (labels ((agent-set (name new-value)
             (let ((symbol (%conversation-symbol name)))
               (when (boundp symbol) (setf (symbol-value symbol) new-value))))
           (profile-copy ()
             (let* ((symbol (%conversation-symbol "*CONSCIOUS-CONVERSATION-PROVIDER-PROFILE*"))
                    (source (and (boundp symbol) (symbol-value symbol)))
                    (copy (make-hash-table :test #'equal)))
               (unless (hash-table-p source) (error "Provider profile is unavailable"))
               (maphash (lambda (k v) (setf (gethash k copy) v)) source)
               (let ((routing (gethash "provider_routing" source)))
                 (when (hash-table-p routing)
                   (let ((routing-copy (make-hash-table :test #'equal)))
                     (maphash (lambda (k v) (setf (gethash k routing-copy) v)) routing)
                     (setf (gethash "provider_routing" copy) routing-copy))))
               copy))
           (desired (name)
             (gethash name
                      (symbol-value
                       (%conversation-symbol "*RUNTIME-SETTINGS-VALUES*"))))
           (update-profile (function)
             (let ((copy (profile-copy)))
               (funcall function copy)
               (agent-set "*CONSCIOUS-CONVERSATION-PROVIDER-PROFILE*" copy))))
    (cond
      ((string= key "model")
       (agent-set "*CONSCIOUS-RECURSIVE-MIND-MODEL*" value))
      ((string= key "openrouter_zdr")
       (update-profile
        (lambda (profile)
          (let ((routing (gethash "provider_routing" profile)))
            (unless (hash-table-p routing) (error "Provider routing is unavailable"))
            (setf (gethash "zdr" routing) (string= value "require"))))))
      ((string= key "openrouter_data_collection")
       (update-profile
        (lambda (profile)
          (let ((routing (gethash "provider_routing" profile)))
            (unless (hash-table-p routing) (error "Provider routing is unavailable"))
            (setf (gethash "data_collection" routing) value)))))
      ((string= key "openrouter_reasoning")
       (update-profile
        (lambda (profile)
          (cond ((string= value "model-default") (remhash "reasoning" profile))
                ((string= value "disabled")
                 (setf (gethash "reasoning" profile)
                       (%conversation-object "enabled" nil)))
                ((string= value "enabled")
                 (setf (gethash "reasoning" profile)
                       (%conversation-object
                        "effort" (desired "openrouter_reasoning_effort"))))))))
      ((string= key "openrouter_reasoning_effort")
       (when (string= (desired "openrouter_reasoning") "enabled")
         (update-profile
          (lambda (profile)
            (setf (gethash "reasoning" profile)
                  (%conversation-object "effort" value))))))
      ((string= key "cost_ceiling_usd")
       (agent-set "*CONSCIOUS-CONVERSATION-COST-CEILING-USD*"
                  (coerce value 'double-float)))
      ((string= key "provider_call_timeout_seconds")
       (agent-set "*CONSCIOUS-CONVERSATION-PROVIDER-CALL-TIMEOUT-SECONDS*" value))
      ((string= key "provider_connect_timeout_seconds")
       (agent-set "*CONSCIOUS-CONVERSATION-PROVIDER-CONNECT-TIMEOUT-SECONDS*" value))
      ((string= key "provider_inactivity_timeout_seconds")
       (agent-set "*CONSCIOUS-CONVERSATION-PROVIDER-INACTIVITY-TIMEOUT-SECONDS*" value))
      ((string= key "provider_streaming")
       (agent-set "*CONSCIOUS-CONVERSATION-PROVIDER-STREAMING-P*" value))
      ((string= key "curiosity_wake_seconds")
       (setf *conversation-curiosity-wake-seconds* value)
       (bt:with-lock-held (*conversation-curiosity-lock*)
         (bt:condition-notify *conversation-curiosity-condition*)))
      ((string= key "deliberate_curiosity")
       (setf *conversation-deliberate-curiosity-p* value)
       (agent-set "*CONSCIOUS-RECURSIVE-MIND-DELIBERATE-CURIOSITY-ENABLED-P*" value))
      ((string= key "curiosity_reach_out")
       (setf *conversation-curiosity-reach-out-p* value)
       (agent-set "*CONSCIOUS-RECURSIVE-MIND-CURIOSITY-REACH-OUT-ENABLED-P*" value))
      ((string= key "curiosity_briefing")
       (setf *conversation-curiosity-briefing-p* value)
       (agent-set "*CONSCIOUS-RECURSIVE-MIND-CURIOSITY-BRIEFING-ENABLED-P*" value))
      ((string= key "curiosity_consolidation")
       (setf *conversation-curiosity-consolidation-p* value)
       (agent-set "*CONSCIOUS-RECURSIVE-MIND-CURIOSITY-CONSOLIDATION-ENABLED-P*" value))
      ((string= key "episodic_memory")
       (setf *conversation-episodic-memory-p* value)
       (agent-set "*CONSCIOUS-RECURSIVE-MIND-EPISODIC-MEMORY-ENABLED-P*" value))
      ((string= key "knowledge_graph_formation")
       (setf *conversation-knowledge-graph-formation-p* value)
       (agent-set "*CONSCIOUS-RECURSIVE-MIND-KNOWLEDGE-GRAPH-FORMATION-FN*"
                  (and value #'%conversation-knowledge-graph-form-one)))
      ((string= key "knowledge_graph_budget_usd")
       (agent-set "*CONSCIOUS-CONTEXT-GRAPH-GENERATION-BUDGET-MICROUSD*"
                  (round (* 1000000 value))))
      ((string= key "private_budget_percent")
       (setf *conversation-private-budget-percent* value)
       (agent-set "*CONSCIOUS-RECURSIVE-MIND-PRIVATE-BUDGET-PERCENT*" value))
      ((string= key "private_reasoning_effort")
       (setf *conversation-private-reasoning-effort* value)
       (agent-set "*CONSCIOUS-RECURSIVE-MIND-PRIVATE-REASONING-EFFORT*" value))
      ((string= key "loop_trace") (setf *conversation-loop-trace-mode* value))
      ((string= key "context_trace")
       (agent-set "*LLM-DEBUG-CAPTURE-MODE*"
                  (intern (string-upcase value) :keyword)))
      ((string= key "show_rejected")
       (setf (uiop:getenv "PAI_CONVERSATION_SHOW_REJECTED") (if value "1" "0")))
      ((string= key "show_memory_context")
       (setf (uiop:getenv "PAI_CONVERSATION_SHOW_MEMORY_CONTEXT") (if value "1" "0")))
      ((string= key "affect_baseline_event_id")
       (setf *conversation-affect-baseline-event-id* value)))
    t))

(%conversation-call "runtime-settings-configure-applier"
                    #'%conversation-runtime-setting-apply)
(%conversation-call "runtime-settings-apply-all")

(when (fboundp (%conversation-symbol "web-terminal-configure-submit"))
  (%conversation-call
   "web-terminal-configure-submit"
   (if (string= *conversation-loop-mode* "recursive")
       (lambda (prompt channel)
         (multiple-value-bind (backend ignored agent persona)
             (%conversation-episode-graph-partition)
           (declare (ignore ignored agent persona))
           (%conversation-call "sustained-activity-operator-submit"
                               backend prompt channel "operator:web")))
       (lambda (prompt channel)
         (let ((receipt
                 (%conversation-call "conscious-interaction-admit"
                                     prompt :channel channel)))
           (%conversation-call "%v2-broadcast" "user"
                               (%conversation-call "%v2-user-display" prompt)
                               :turn-id (gethash "interaction_id" receipt))
           receipt)))))

(defun %conversation-web-command-words (line)
  (remove "" (uiop:split-string line :separator '(#\Space #\Tab))
          :test #'string=))

(defun %conversation-runtime-setting-value-from-text (text)
  "Accept JSON scalars for exact types, with bare words as ergonomic strings."
  (handler-case (shasht:read-json text)
    (error () text)))

(defun %conversation-runtime-settings-text (&optional report)
  (let ((current (or report (%conversation-call "runtime-settings-report"))))
    (with-output-to-string (stream)
      (format stream "Runtime settings revision ~a~%"
              (gethash "revision" current))
      (loop for row across (gethash "settings" current)
            do (format stream "~a = ~s (effective ~s; ~a)~@[ [restart pending]~]~@[ [error: ~a]~]~%"
                       (gethash "key" row) (gethash "desired" row)
                       (gethash "effective" row) (gethash "apply_mode" row)
                       (and (gethash "pending_restart" row) t)
                       (let ((error (gethash "error" row)))
                         (and (stringp error) error)))))))

(defun %conversation-runtime-setting-update (key value-text actor)
  (%conversation-runtime-settings-text
   (%conversation-call "runtime-settings-update" key
                       (%conversation-runtime-setting-value-from-text value-text)
                       :actor actor)))

(defun %conversation-web-positive-decimal (text)
  "Parse one ordinary positive decimal to six fractional places."
  (let* ((dot (position #\. text))
         (whole (if dot (subseq text 0 dot) text))
         (fraction (and dot (subseq text (1+ dot)))))
    (unless (and (plusp (length whole))
                 (every #'digit-char-p whole)
                 (or (null fraction)
                     (and (plusp (length fraction))
                          (<= (length fraction) 6)
                          (every #'digit-char-p fraction))))
      (error "USD must be a positive decimal with at most six fractional digits"))
    (let* ((scale (if fraction (expt 10 (length fraction)) 1))
           (value (+ (parse-integer whole)
                     (/ (if fraction (parse-integer fraction) 0) scale))))
      (unless (plusp value)
        (error "USD addition must be positive"))
      value)))

(defun %conversation-web-budget-text (report &optional prefix)
  (let* ((pending
           (gethash "pending_generation_settlement_count" report 0))
         (pending-text
           (and (plusp pending)
                (format nil
                        "Pending provider settlement: ~d generation~:p; $~,6f conservative fallback. Admission remains paused until exact accounting or fallback settlement."
                        pending
                        (gethash "pending_generation_fallback_usd"
                                 report 0d0)))))
    (format nil
            "~@[~a~%~]Budget: $~,6f/$~,6f used ($~,6f remaining); ~d requests recorded for telemetry only.~%Private cost share (~d%): $~,6f/$~,6f used ($~,6f remaining); ~d private requests recorded for telemetry only.~@[ Accounting is uncertain; further provider calls are paused.~]~@[ Conservative accounting fallbacks: ~d.~]~@[~%~a~]"
            prefix
            (gethash "spent_usd" report)
            (gethash "cost_ceiling_usd" report)
            (gethash "remaining_usd" report)
            (gethash "request_attempts" report)
            (gethash "private_budget_percent" report)
            (gethash "private_spent_usd" report)
            (gethash "private_cost_ceiling_usd" report)
            (gethash "private_remaining_usd" report)
            (gethash "private_request_attempts" report)
            (gethash "accounting_uncertain" report)
            (let ((count (gethash "accounting_anomaly_count" report 0)))
              (and (plusp count) count))
            pending-text)))

(defun %conversation-run-web-command (line)
  (unless (string= *conversation-loop-mode* "recursive")
    (error "Web operator commands require the recursive mind loop"))
  (let ((words (%conversation-web-command-words line)))
    (cond
      ((and words (string-equal (first words) "/activity"))
       (multiple-value-bind (backend ignored agent persona)
           (%conversation-episode-graph-partition)
         (declare (ignore ignored agent persona))
         (%conversation-call "sustained-activity-operator-command"
                             backend (rest words) "web" "operator:web")))
      ((and (= (length words) 1)
            (string-equal (first words) "/budget"))
       (%conversation-web-budget-text
        (%conversation-call "conscious-recursive-session-budget-report")))
      ((and (= (length words) 2)
            (string-equal (first words) "/budget-add"))
       (let ((usd (%conversation-web-positive-decimal (second words))))
         (%conversation-web-budget-text
          (%conversation-call "conscious-recursive-session-budget-add"
                              usd)
          (format nil "Added $~,6f." usd))))
      ((and (= (length words) 1)
            (string-equal (first words) "/config"))
       (%conversation-runtime-settings-text))
      ((and (>= (length words) 3)
            (string-equal (first words) "/config-set"))
       (%conversation-runtime-setting-update
        (second words) (format nil "~{~a~^ ~}" (cddr words))
        "authenticated-web-command"))
      ((and (<= 1 (length words) 2)
            (string-equal (first words) "/curiosity-inspect"))
       (let ((maximum (if (second words)
                          (parse-integer (second words) :junk-allowed nil)
                          20)))
         (unless (<= 1 maximum 100)
           (error "Curiosity inspection maximum must be between 1 and 100"))
         (let ((*print-pretty* t))
           (shasht:write-json
            (%conversation-call "conscious-recursive-curiosity-inspect"
                                maximum)
            nil))))
      ((and (= (length words) 1)
            (string-equal (first words) "/affect-inspect"))
       (unless *conversation-affect-baseline-event-id*
         (error "/affect-inspect requires --affect-baseline-event-id at process launch"))
       (let ((*print-pretty* t))
         (shasht:write-json
          (%conversation-call
           "conscious-affect-inspect-window"
           *conversation-affect-baseline-event-id*
           (%conversation-required-env "PAI_AGENT_ID")
           (%conversation-required-env "PAI_AGENT_ID")
           :now (get-universal-time))
          nil)))
      ((and (= (length words) 1)
            (string-equal (first words) "/graph-inspect"))
       (let ((*print-pretty* t))
         (shasht:write-json
          (%conversation-call
           "conscious-recursive-conversation-episode-graph-inspect")
          nil)))
      ((and (>= (length words) 2)
            (string-equal (first words) "/graph-search"))
       (let* ((query (format nil "~{~a~^ ~}" (rest words)))
              (request
                (%conversation-call
                 "knowledge-graph-search-request" :query query)))
         (let ((*print-pretty* t))
           (shasht:write-json
            (%conversation-call "knowledge-graph-search-compact-result"
              (%conversation-knowledge-graph-search-storage request #() #()))
            nil))))
      ((and (>= (length words) 3)
            (string-equal (first words) "/curiosity-supersede"))
       (let ((result-id (parse-integer (second words) :junk-allowed nil))
             (reason (format nil "~{~a~^ ~}" (cddr words))))
         (unless (and (plusp result-id) (<= 1 (length reason) 2000))
           (error "Curiosity supersession requires a positive result ID and a bounded reason"))
         (let ((*print-pretty* t))
           (shasht:write-json
            (%conversation-call
             "conscious-recursive-curiosity-supersede-finding"
             result-id reason)
            nil))))
      ((and (= (length words) 2)
            (string-equal (first words) "/fleet-request"))
       (%conversation-call "fleet-request-peer" (second words)))
      ((and (= (length words) 2)
            (string-equal (first words) "/fleet")
            (string-equal (second words) "pending"))
       (%conversation-call "fleet-pending-requests"))
      ((and (= (length words) 2)
            (string-equal (first words) "/fleet")
            (string-equal (second words) "peers"))
       (%conversation-call "fleet-peer-list"))
      ((and (= (length words) 3)
            (string-equal (first words) "/fleet-approve"))
       (%conversation-call "fleet-approve-request" (second words) (third words)))
      ((and (= (length words) 2)
            (string-equal (first words) "/board")
            (string-equal (second words) "list"))
       (%conversation-call "fleet-board-list"))
      ((and (= (length words) 3)
            (string-equal (first words) "/board")
            (string-equal (second words) "read"))
       (%conversation-call "fleet-board-read" (third words)))
      ((and (>= (length words) 3)
            (string-equal (first words) "/board")
            (string-equal (second words) "post"))
       (%conversation-call "fleet-board-post" (third words)
                           (format nil "~{~a~^ ~}" (cdddr words))))
      ((and (>= (length words) 3)
            (string-equal (first words) "/board")
            (string-equal (second words) "post-new"))
       (%conversation-call "fleet-board-post" (third words)
                           (format nil "~{~a~^ ~}" (cdddr words)) t))
      (t
       (error "Unknown web command. Use /config, /config-set KEY VALUE, /affect-inspect, /curiosity-inspect [maximum], /curiosity-supersede RESULT-ID REASON, /graph-inspect, /graph-search QUERY, /budget, /budget-add USD, /fleet-request HOST:PORT, /fleet pending, /fleet peers, /fleet-approve REQUESTER-ID CODE, /board list, /board read THREAD-ID, /board post PEER-ID TEXT, or /board post-new PEER-ID TEXT")))))

(when (fboundp (%conversation-symbol "web-terminal-configure-command"))
  (%conversation-call "web-terminal-configure-command"
                      #'%conversation-run-web-command))

(when (fboundp (%conversation-symbol "web-graph-explorer-configure-search"))
  (%conversation-call "web-graph-explorer-configure-search"
                      (lambda (request)
                        (%conversation-knowledge-graph-search-storage
                         request #() #()))))

(unless (string= (or (uiop:getenv "PAI_MIGRATION_ONLY") "") "1")
  ;; Fleet identity persists independent of whether the web acceptor is
  ;; enabled this run (docs/FLEET_DESIGN.md S2.3) -- only excluded from a
  ;; migration-only run, which is not a normal boot.
  (%conversation-call "web-fleet-init"))
(when (and (not (string= (or (uiop:getenv "PAI_MIGRATION_ONLY") "") "1"))
           (string= (or (uiop:getenv "PAI_WEB_ENABLED") "") "1"))
  (let ((port (parse-integer (%conversation-required-env "PAI_WEB_PORT")))
        (address (%conversation-required-env "PAI_WEB_ADDRESS")))
    (%conversation-call "start-web" port address)))
(format t "~&Startup complete in ~,1fs.~%"
        (%conversation-startup-elapsed-seconds *conversation-startup-started*))

(let ((storage (%conversation-call "event-authority-report")))
  (format t "~&Event storage: ~a (~a)~%"
          (gethash "database" storage) (gethash "authority" storage)))
(format t "Derived storage: ~a (rebuildable projections)~%"
        (%conversation-required-env "PAI_DERIVED_STORAGE_DATABASE"))
(when *conversation-memory-import-receipt*
  (format t "Memory import: ~a; nodes ~d; edges ~d~%"
          (or (gethash "status" *conversation-memory-import-receipt*)
              "imported-and-audited")
          (gethash "node_count" *conversation-memory-import-receipt*)
          (gethash "edge_count" *conversation-memory-import-receipt*)))
(let ((receipt *conversation-event-authority-receipt*))
  (format t "~&Event authority receipt: ~a; events ~d; maximum id ~a"
          (gethash "status" receipt)
          (gethash "event_count" receipt)
          (gethash "maximum_event_id" receipt))
  (when *conversation-event-checkpoint-receipt*
    (format t "; checkpoint position ~d"
            (gethash "through_storage_position"
                     *conversation-event-checkpoint-receipt*)))
  (when (member (gethash "status" receipt)
                '("migrated" "migration-resumed") :test #'string=)
    (format t "; sources ~d; audit ~a; duplicate ids ~d; rewinds ~d"
            (gethash "source_file_count" receipt)
            (gethash "audit_status" receipt)
            (gethash "duplicate_id_count" receipt)
            (gethash "rewind_count" receipt)))
  (terpri))

(defun %conversation-curiosity-loop ()
  "After sustained quiet, review one batch and wake one private curiosity."
  ;; Process loss can leave a truthful MODEL-REQUEST without a corresponding
  ;; outcome. Reconcile that one authority boundary immediately on restart;
  ;; this step is providerless and does not run the broader quiet pipeline.
  (handler-case
      (let ((recovery
              (%conversation-call
               "conscious-recursive-curiosity-recover-abandoned-one")))
        (when (string= "recovered-abandoned-request"
                       (gethash "status" recovery ""))
          (bt:with-lock-held (*conversation-progress-lock*)
            (format t "~&[curiosity] restart recovery settled abandoned provider request at event ~a~%"
                    (gethash "recovery_event_id" recovery))
            (finish-output))))
    (error (condition)
      (bt:with-lock-held (*conversation-progress-lock*)
        (format t "~&[curiosity restart-recovery error: ~a]~%" condition)
        (finish-output))))
  (loop
    (bt:with-lock-held (*conversation-curiosity-lock*)
      (unless *conversation-curiosity-running-p* (return))
      (bt:condition-wait *conversation-curiosity-condition*
                         *conversation-curiosity-lock*
                         :timeout *conversation-curiosity-wake-seconds*)
      (unless *conversation-curiosity-running-p* (return)))
    (handler-case
        (let* ((quiet (%conversation-call
                       "conscious-recursive-mind-quiet-step"))
               (episode (let ((value (gethash "episode" quiet)))
                          (and (hash-table-p value) value)))
               (episode-graph (let ((value (gethash "episode_graph" quiet)))
                                (and (hash-table-p value) value)))
               (knowledge-graph-formation
                 (let ((value (gethash "knowledge_graph_formation" quiet)))
                   (and (hash-table-p value) value)))
               (review (let ((value (gethash "review" quiet)))
                         (and (hash-table-p value) value)))
               (consolidation (let ((value (gethash "consolidation" quiet)))
                                (and (hash-table-p value) value)))
               (attention (let ((value (gethash "attention" quiet)))
                            (and (hash-table-p value) value)))
               (result (let ((value (gethash "investigation" quiet)))
                         (and (hash-table-p value) value)))
               (result-review (let ((value (gethash "result_review" quiet)))
                                (and (hash-table-p value) value)))
               (incorporation (let ((value (gethash "incorporation" quiet)))
                                (and (hash-table-p value) value)))
               (briefing (let ((value (gethash "briefing" quiet)))
                           (and (hash-table-p value) value))))
          (when (and (hash-table-p episode)
                     (string= "episode-sealed" (gethash "status" episode "")))
            (bt:with-lock-held (*conversation-progress-lock*)
              (format t "~&[episodic memory] sealed ~d episode~:p; latest ~a~%"
                      (gethash "sealed_count" episode 1)
                      (gethash "episode_id" episode))
              (finish-output)))
          (when (and episode
                     (string= "failed" (gethash "status" episode "")))
            (bt:with-lock-held (*conversation-progress-lock*)
              (format t "~&[episodic memory] sealing failed; curiosity and later quiet cognition remain available~%")
              (finish-output)))
          (when (and episode-graph
                     (string= "synchronized"
                              (gethash "status" episode-graph "")))
            (bt:with-lock-held (*conversation-progress-lock*)
              (format t "~&[episodic graph] ~a through position ~d; ~d episodes, ~d nodes, ~d edges~%"
                      (gethash "mode" episode-graph "synchronized")
                      (gethash "through_storage_position" episode-graph 0)
                      (gethash "episode_count" episode-graph 0)
                      (gethash "node_count" episode-graph 0)
                      (gethash "edge_count" episode-graph 0))
              (finish-output)))
          ;; Rebuild-only runs have no operator turn in which to surface their
          ;; bounded formation report.  Keep the diagnostic content-free, but
          ;; do not hide a local validation failure behind an apparently idle
          ;; prompt: status, durable task ids, and budget counters are the
          ;; evidence needed to resume or stop the controlled rebuild safely.
          (when knowledge-graph-formation
            (bt:with-lock-held (*conversation-progress-lock*)
              (format t
                      "~&[knowledge graph] status=~a opened=~a applied=~d retryable=~d unresolved=~d source-blocked=~d exposure-microusd=~d remaining-microusd=~d~@[ reason=~a~]~@[ detail=~a~]~%"
                      (gethash "status" knowledge-graph-formation "unknown")
                      (gethash "opened_event_id" knowledge-graph-formation :null)
                      (gethash "applied_tasks" knowledge-graph-formation 0)
                      (gethash "retryable_tasks" knowledge-graph-formation 0)
                      (gethash "unresolved_tasks" knowledge-graph-formation 0)
                      (gethash "source_blocked_count" knowledge-graph-formation 0)
                      (gethash "exposure_microusd" knowledge-graph-formation 0)
                      (gethash "remaining_microusd" knowledge-graph-formation 0)
                      (gethash "reason" knowledge-graph-formation nil)
                      (gethash "detail" knowledge-graph-formation nil))
              (finish-output)))
          (when (and review
                     (string= "review-completed"
                              (gethash "status" review "")))
            (bt:with-lock-held (*conversation-progress-lock*)
              (format t "~&[curiosity] quiet review completed; observations ~d~%"
                      (gethash "observation_count" review 0))
              (finish-output)))
          (when (and attention
                     (member (gethash "status" attention "")
                             '("focus-chosen" "attention-declined")
                             :test #'string=))
            (bt:with-lock-held (*conversation-progress-lock*)
              (format t "~&[curiosity] attention ~a for register ~a~%"
                      (gethash "status" attention)
                      (gethash "register_revision" attention))
              (finish-output)))
          (when (and consolidation
                     (string= "consolidation-updated"
                              (gethash "status" consolidation "")))
            (bt:with-lock-held (*conversation-progress-lock*)
              (format t "~&[curiosity] open interests consolidated into ~d semantic threads~%"
                      (gethash "thread_count" consolidation 0))
              (finish-output)))
          (when (and consolidation
                     (string= "failed" (gethash "status" consolidation "")))
            (bt:with-lock-held (*conversation-progress-lock*)
              (format t "~&[curiosity] consolidation failed; raw attention and later private cognition remain available~%")
              (finish-output)))
          (when (and result
                     (string= "curiosity-completed"
                              (gethash "status" result "")))
             (bt:with-lock-held (*conversation-progress-lock*)
               (format t "~&[curiosity] private investigation completed; use /curiosity-inspect~%")
               (finish-output)))
          (when (and result
                     (string= "failed" (gethash "status" result "")))
            (bt:with-lock-held (*conversation-progress-lock*)
              (format t "~&[curiosity] private investigation failed: ~a~%"
                      (gethash "reason" result "unknown failure"))
              (finish-output)))
          (when (and result-review
                     (string= "result-reviewed"
                              (gethash "status" result-review "")))
            (bt:with-lock-held (*conversation-progress-lock*)
              (format t "~&[curiosity] result reviewed; disposition ~a~%"
                      (gethash "disposition" result-review))
              (finish-output)))
           (when (and incorporation
                     (string= "finding-incorporated"
                              (gethash "status" incorporation "")))
            (bt:with-lock-held (*conversation-progress-lock*)
              (format t "~&[curiosity] finding ~a~@[; reach-out event ~a~]~%"
                      (gethash "disposition" incorporation)
                      (let ((id (gethash "reach_out_event_id" incorporation)))
                        (and (integerp id) id)))
               (finish-output)))
           (when (and briefing
                      (string= "briefing-updated"
                               (gethash "status" briefing "")))
             (bt:with-lock-held (*conversation-progress-lock*)
               (format t "~&[curiosity] private-state briefing updated~%")
               (finish-output)))
           (when (and briefing
                      (string= "failed" (gethash "status" briefing "")))
             (bt:with-lock-held (*conversation-progress-lock*)
               (format t "~&[curiosity] private-state briefing failed; later private cognition remains available~%")
               (finish-output)))
           (dolist (stage-result
                    (list (cons "review" review)
                          (cons "consolidation" consolidation)
                          (cons "attention" attention)
                          (cons "investigation" result)
                          (cons "result review" result-review)
                          (cons "incorporation" incorporation)
                          (cons "briefing" briefing)))
            (let* ((stage (car stage-result))
                   (value (cdr stage-result))
                   (status (and value (gethash "status" value ""))))
              (when (member status
                            '("revision-settled" "focus-pending"
                              "paused-budget" "preempted")
                            :test #'string=)
                (bt:with-lock-held (*conversation-progress-lock*)
                  (format t "~&[curiosity] ~a: ~a~@[ (~a)~]~%"
                          stage status
                          (or (gethash "register_revision" value)
                              (gethash "focus_event_id" value)))
                  (finish-output))))))
      (error (condition)
        (bt:with-lock-held (*conversation-progress-lock*)
          (format t "~&[curiosity error: ~a]~%" condition)
          (finish-output))))))

(when (and (string= *conversation-loop-mode* "recursive")
           (plusp *conversation-curiosity-wake-seconds*))
  (bt:with-lock-held (*conversation-curiosity-lock*)
    (setf *conversation-curiosity-running-p* t
          *conversation-curiosity-thread*
          (bt:make-thread #'%conversation-curiosity-loop
                          :name "conversation-private-curiosity"))))

(defun %conversation-present-result-unlocked (result)
  (let ((status (gethash "status" result))
        (usage (gethash "usage" result)))
    (cond
      ((string= status "replied")
       (format t "~&pAI> ~a~%" (gethash "content" result)))
      ((string= status "no-reply")
       (format t "~&[pAI explicitly chose not to reply: ~a]~%"
               (gethash "reason" result)))
      ((string= status "withheld")
       (let ((codes (gethash "violation_codes" result)))
         (format t "~&[candidate withheld by publication validation: ~a~@[; codes: ~{~a~^, ~}~]]~%"
                 (gethash "reason" result)
                 (and (vectorp codes) (plusp (length codes))
                      (coerce codes 'list)))))
      ((string= status "paused-budget")
       (format t "~&[turn paused before another provider request: ~a]~%"
               (gethash "reason" result)))
      (t (format t
                 "~&[turn failed closed: ~a~@[; code: ~a~]~@[; reason: ~a~]]~%"
                 status
                 (let ((code (gethash "error_code" result)))
                   (and (stringp code) code))
                 (gethash "reason" result))))
    (let ((memory (gethash "memory_context" result))
          (history (gethash "history_context" result)))
      (when (hash-table-p memory)
        (format t
                "~&[context: history selected ~d/~d; omitted ~d; history characters ~d; estimated tokens ~d; provider message characters ~d; provider input tokens ~d across ~d boundaries; memory ~a; candidates ~d; eligible ~d; selected ~d; episodes ~a/~d; memory characters ~d; writes ~d]~%"
                 (if (hash-table-p history)
                     (gethash "record_count" history 0) 0)
                 (if (hash-table-p history)
                     (gethash "candidate_count" history 0) 0)
                 (if (hash-table-p history)
                     (gethash "omitted_record_count" history 0) 0)
                 (if (hash-table-p history)
                     (gethash "rendered_characters" history 0) 0)
                 (if (hash-table-p history)
                     (gethash "estimated_tokens" history 0) 0)
                 (if (hash-table-p history)
                     (gethash "provider_message_characters" history 0) 0)
                 (if (hash-table-p history)
                     (gethash "provider_input_tokens" history 0) 0)
                 (if (hash-table-p history)
                     (gethash "provider_boundary_count" history 0) 0)
                 (gethash "status" memory "unknown")
                (gethash "candidate_count" memory 0)
                (gethash "eligible_count" memory 0)
                (gethash "selected_count" memory 0)
                (gethash "episodic_status" memory "disabled")
                (gethash "episodic_selected_count" memory 0)
                (gethash "rendered_characters" memory 0)
                (gethash "database_write_count" memory 0))))
    (when (hash-table-p usage)
      (format t "~&[tokens: input ~a, output ~a, reasoning ~a, total ~a; output cap ~a]~%"
              (gethash "input_tokens" usage)
              (gethash "output_tokens" usage)
              (gethash "reasoning_tokens" usage)
              (gethash "total_tokens" usage)
              (gethash "max_output_tokens" usage))
      (when (numberp (gethash "cost_usd" usage))
        (format t "~&[cost: turn $~,8f; session $~,8f; requests ~d]~%"
                (gethash "cost_usd" usage)
                (gethash "session_cost_usd" usage)
                (gethash "session_request_attempts" usage))))
    (let ((timing (gethash "timing_ms" result)))
      (when (hash-table-p timing)
        (format t
                "~&[timing ms: total ~d | quanta ~d | cognition ~d | tools ~d | scheduler ~d | settlement ~d | admission ~d | context/open ~d (embed ~d, semantic ~d, neighborhood ~d) | request-log ~d | provider ~d | response-log ~d | parse ~d | capture ~d | publication ~d | reply-commit ~d | other ~d]~%"
                (gethash "total" timing 0)
                (gethash "quantum_count" timing 0)
                (gethash "cognitive_quantum" timing 0)
                (gethash "tool_execution" timing 0)
                (gethash "scheduler_handoff" timing 0)
                (gethash "boundary_settlement" timing 0)
                (gethash "admission" timing 0)
                (gethash "context_open" timing 0)
                (gethash "memory_query_embedding" timing 0)
                (gethash "memory_semantic_scan" timing 0)
                (gethash "memory_neighborhood_scan" timing 0)
                (gethash "request_journal" timing 0)
                (gethash "provider" timing 0)
                (gethash "response_journal" timing 0)
                (gethash "captured_parse" timing 0)
                (gethash "captured_commit" timing 0)
                (gethash "publication_validation" timing 0)
                (gethash "reply_commit" timing 0)
                (gethash "unattributed" timing 0))))))

(defun %conversation-present-result (result)
  ;; The interaction observer can finish on its worker immediately after the
  ;; waiter wakes. Share the progress output lock so ANSI diagnostics and the
  ;; public result can never splice bytes into one another.
  (bt:with-lock-held (*conversation-progress-lock*)
    (%conversation-present-result-unlocked result)))

(defun %conversation-present-lifecycle-result (result)
  (format t "~&[lifecycle] ")
  (shasht:write-json result *standard-output*)
  (terpri)
  (finish-output))

(defun %conversation-run-memory-inspect (payload)
  "Show evidence that the final read-only conversation memory consumer ran.

Content remains private unless the operator explicitly enabled its display at
process startup."
  (multiple-value-bind (records ids report)
      (%conversation-call "%conversation-memory-context-records"
                          (gethash "query" payload)
                          (%conversation-load-context-profile)
                          "local")
    (declare (ignore ids))
    (unless (hash-table-p report)
      (error "Conversation memory consumer returned no report"))
    (format t "~&[memory] ")
    (shasht:write-json
     (let ((copy (make-hash-table :test #'equal)))
       (setf (gethash "schema_version" copy) 1
             (gethash "status" copy) "ok")
       (loop for key being the hash-keys of report using (hash-value value)
             do (setf (gethash key copy) value))
       (when (string= "1" (or (uiop:getenv
                                "PAI_CONVERSATION_SHOW_MEMORY_CONTEXT") ""))
         (setf (gethash "selected_memory_context" copy) records))
       copy)
     *standard-output*)
    (terpri)
    (finish-output)))

(defun %conversation-run-curiosity-inspect ()
  (unless (string= *conversation-loop-mode* "recursive")
    (error "/curiosity-inspect requires the recursive mind loop"))
  (format t "~&[curiosity] ")
  (shasht:write-json
   (%conversation-call "conscious-recursive-curiosity-inspect")
   *standard-output*)
  (terpri)
  (finish-output))

(defun %conversation-run-affect-inspect ()
  (unless (string= *conversation-loop-mode* "recursive")
    (error "/affect-inspect requires the recursive mind loop"))
  (unless *conversation-affect-baseline-event-id*
    (error "/affect-inspect requires --affect-baseline-event-id at process launch"))
  (format t "~&[affect] ")
  (shasht:write-json
   (%conversation-call
    "conscious-affect-inspect-window"
    *conversation-affect-baseline-event-id*
    (%conversation-required-env "PAI_AGENT_ID")
    (%conversation-required-env "PAI_AGENT_ID")
    :now (get-universal-time))
   *standard-output*)
  (terpri)
  (finish-output))

(defun %conversation-run-graph-inspect ()
  (unless (string= *conversation-loop-mode* "recursive")
    (error "/graph-inspect requires the recursive mind loop"))
  (format t "~&[episodic graph] ")
  (shasht:write-json
   (%conversation-call
    "conscious-recursive-conversation-episode-graph-inspect")
   *standard-output*)
  (terpri)
  (finish-output))

(defun %conversation-run-graph-search (payload)
  (unless (string= *conversation-loop-mode* "recursive")
    (error "/graph-search requires the recursive mind loop"))
  (format t "~&[knowledge graph] ")
  (shasht:write-json
   (%conversation-call
    "conscious-recursive-knowledge-graph-search"
    (%conversation-call "knowledge-graph-search-request"
                        :query (gethash "query" payload)))
   *standard-output*)
  (terpri)
  (finish-output))

(defun %conversation-run-lifecycle-command (kind payload)
  (let ((write-mode (%conversation-symbol "*autonomous-write-mode*"))
        (intention-mode (%conversation-symbol "*near-term-intentions-mode*"))
        (delivery-fn
          (%conversation-symbol "*near-term-intention-delivery-fn*")))
    (progv (list write-mode intention-mode delivery-fn)
           (list :normal :enforced nil)
      (let ((result
              (%conversation-call
               "pai-cli-run-lifecycle" kind payload
               (lambda (&rest arguments)
                 (apply #'%conversation-call
                        "conscious-lifecycle-scenario-run" arguments)))))
        ;; The lifecycle command installs its own lifecycle projection. The
        ;; next chat's captured-open consumer refreshes the complete conscious
        ;; projection after reconciliation; another full restore here only
        ;; duplicates a potentially large ledger replay.
        (%conversation-present-lifecycle-result result)))))

(defun %conversation-web-enabled-p ()
  (string= (or (uiop:getenv "PAI_WEB_ENABLED") "") "1"))

(defun %conversation-await-web-shutdown-after-input-eof ()
  "Keep the process owner alive when a detached web runtime has no terminal.

The authenticated web acceptor and private cognition threads are intentional
process work. Standard-input EOF therefore ends only terminal input in web
mode; container/process termination remains the shutdown boundary."
  (format t "~&[terminal input closed; web runtime remains active]~%")
  (finish-output)
  (loop (sleep 3600)))

(let ((endpoint (or (uiop:getenv "PAI_CONVERSATION_ENDPOINT") ""))
      (model (or (uiop:getenv "PAI_CONVERSATION_MODEL") ""))
      (one-shot (uiop:getenv "PAI_CONVERSATION_MESSAGE")))
  (labels ((run-turn (line)
             (unwind-protect
                  (handler-case
                      (%conversation-present-result
                      (if (string= *conversation-loop-mode* "recursive")
                          (%conversation-call
                           "conscious-recursive-mind-submit" line
                           :channel "terminal")
                          (%conversation-call
                           "conscious-interaction-submit-and-wait" line
                           :channel "terminal" :timeout 240)))
                    (error (condition)
                      (format t "~&[turn error: ~a]~%" condition)))
               ;; The expanded ledger snapshot is now out of scope.  Run the
               ;; synchronous contained-runtime heap guard before accepting
               ;; another command; no worker or routine health append is used.
               (handler-case
                   (%conversation-call "conscious-conversation-memory-boundary")
                 (error (condition)
                   (format t "~&[memory guard error: ~a]~%" condition))))))
    (if (and one-shot (plusp (length one-shot)))
        (run-turn one-shot)
        (progn
          (if *conversation-unified-cli-p*
              (format t "~&pAI is ready (persona: ~a; mind loop: ~a). Type /help for commands or /quit to exit.~%"
                      (%conversation-required-env "PAI_CONVERSATION_PERSONA")
                      *conversation-loop-mode*)
              (format t "~&Q4.5 conversation is ready. Type :quit to exit.~%"))
          (loop
            (format t "~&you> ")
            (finish-output)
            (let ((line (read-line *standard-input* nil nil)))
              (when (null line)
                (if (%conversation-web-enabled-p)
                    (%conversation-await-web-shutdown-after-input-eof)
                    (return)))
              (if *conversation-unified-cli-p*
                  (when (plusp (length line))
                    (handler-case
                        (multiple-value-bind (kind payload)
                            (%conversation-call "pai-cli-parse-input" line)
                          (case kind
                            (:quit (return))
                            (:help
                             (format t "~&~a~%"
                                     (%conversation-call "pai-cli-help-text")))
                            (:chat (run-turn payload))
                            (:memory-inspect
                             (%conversation-run-memory-inspect payload))
                            (:curiosity-inspect
                             (%conversation-run-curiosity-inspect))
                            (:affect-inspect
                             (%conversation-run-affect-inspect))
                            (:graph-inspect
                             (%conversation-run-graph-inspect))
                            (:graph-search
                             (%conversation-run-graph-search payload))
                            (:config-inspect
                             (format t "~&~a" (%conversation-runtime-settings-text)))
                            (:config-set
                             (format t "~&~a"
                                     (%conversation-runtime-setting-update
                                      (gethash "key" payload)
                                      (gethash "value_text" payload)
                                      "authenticated-cli-command")))
                            (otherwise
                             (%conversation-run-lifecycle-command kind payload))))
                      (error (condition)
                        (format t "~&[command error: ~a]~%" condition))))
                  (progn
                    (when (string-equal line ":quit") (return))
                    (when (plusp (length line)) (run-turn line))))))))))

(format t "~&Conversation process ended. Durable history remains in the event ledger.~%")
(bt:with-lock-held (*conversation-curiosity-lock*)
  (setf *conversation-curiosity-running-p* nil)
  (bt:condition-notify *conversation-curiosity-condition*))
(when (and *conversation-curiosity-thread*
           (not (eq *conversation-curiosity-thread* (bt:current-thread))))
  (ignore-errors (bt:join-thread *conversation-curiosity-thread*)))
(setf *conversation-progress-running-p* nil)
(when (and *conversation-progress-thread*
           (not (eq *conversation-progress-thread* (bt:current-thread))))
  (ignore-errors (bt:join-thread *conversation-progress-thread*)))
(ignore-errors (%conversation-call "conscious-interaction-stop"))
(ignore-errors (%conversation-call "conscious-conversation-work-stop"))
(finish-output)
