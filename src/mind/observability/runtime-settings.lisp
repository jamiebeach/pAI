;;;; runtime-settings.lisp -- durable operator-owned runtime configuration.
;;;;
;;;; Behavioral settings are append-only events.  The in-memory table is only
;;;; a rebuildable projection, and consumers adopt an immutable value at their
;;;; next safe boundary.  Credentials and one-shot migration/bootstrap actions
;;;; are deliberately not settings.

(in-package :agent)

(export '(runtime-settings-initialize runtime-settings-report
          runtime-settings-update runtime-settings-value
          runtime-settings-configure-applier runtime-settings-apply-all))

(defvar *runtime-settings-lock* (bt:make-lock "runtime settings"))
(defvar *runtime-settings-values* (make-hash-table :test #'equal))
(defvar *runtime-settings-effective* (make-hash-table :test #'equal))
(defvar *runtime-settings-last-errors* (make-hash-table :test #'equal))
(defvar *runtime-settings-applier* nil)
(defvar *runtime-settings-initialized-p* nil)
(defvar *runtime-settings-revision* 0)

(defparameter *runtime-settings-catalog*
  ;; KEY TYPE APPLY-MODE DESCRIPTION [choices/min/max].  Restart settings are
  ;; still editable: DESIRED changes durably while EFFECTIVE remains explicit.
  '(("provider_profile" :string :restart "Provider policy profile")
    ("model" :string :next-boundary "Provider model slug")
    ("openrouter_zdr" :enum :next-boundary "Zero-data-retention routing" ("require" "allow-non-zdr"))
    ("openrouter_data_collection" :enum :next-boundary "Provider data collection policy" ("deny" "allow"))
    ("openrouter_reasoning" :enum :next-boundary "Public reasoning mode" ("profile" "model-default" "enabled" "disabled"))
    ("openrouter_reasoning_effort" :enum :next-boundary "Public reasoning effort" ("minimal" "low" "medium" "high" "max"))
    ("cost_ceiling_usd" :number :immediate "Cumulative session provider ceiling" 0.000001d0 10000d0)
    ("provider_call_timeout_seconds" :nullable-integer :next-boundary "Provider wall-clock deadline; null disables it" 1 86400)
    ("provider_connect_timeout_seconds" :integer :next-boundary "Provider connection deadline" 1 86400)
    ("provider_inactivity_timeout_seconds" :integer :next-boundary "Streaming inactivity deadline" 1 86400)
    ("provider_streaming" :boolean :next-boundary "Stream provider responses")
    ("mind_loop" :enum :restart "Conversation owner" ("work-state" "recursive"))
    ("recursive_tools" :boolean :restart "Contained native tool envelope")
    ("curiosity_wake_seconds" :integer :immediate "Quiet cognition interval; zero disables" 0 86400)
    ("deliberate_curiosity" :boolean :immediate "Preserve explicit curiosities from conversation")
    ("curiosity_reach_out" :boolean :immediate "Permit reviewed autonomous outreach")
    ("curiosity_briefing" :boolean :immediate "Permit private-state briefing calls")
    ("curiosity_consolidation" :boolean :immediate "Permit semantic curiosity consolidation")
    ("episodic_memory" :boolean :immediate "Seal quiet conversation episodes")
    ("knowledge_graph_formation" :boolean :immediate "Form graph knowledge from sealed episodes")
    ("knowledge_graph_budget_usd" :number :immediate "Cumulative graph-formation ceiling" 0.000001d0 1000d0)
    ("private_budget_percent" :integer :immediate "Share of session budget available to private cognition" 0 100)
    ("private_reasoning_effort" :enum :next-boundary "Private cognition reasoning effort" ("minimal" "low" "medium" "high" "max"))
    ("loop_trace" :enum :immediate "Terminal cognitive activity detail" ("off" "compact" "full"))
    ("context_trace" :enum :immediate "Credential-redacted model context capture" ("off" "metadata" "full"))
    ("context_profile" :string :restart "Context assembly profile")
    ("persona" :string :restart "Persona profile name")
    ("embedding_endpoint" :string :restart "Local embedding endpoint")
    ("web_enabled" :boolean :restart "Authenticated web surface")
    ("web_address" :string :restart "Web listener address")
    ("web_port" :integer :restart "Web listener port" 1 65535)
    ("web_file_mutation" :boolean :restart "Authenticated workspace file mutation")
    ("show_rejected" :boolean :immediate "Show rejected private output in operator CLI")
    ("show_memory_context" :boolean :immediate "Permit explicit memory inspection content")
    ("affect_baseline_event_id" :nullable-integer :immediate "Exclusive event ID for affect inspection" 0 999999999999)))

(defun %runtime-setting-spec (key)
  (find key *runtime-settings-catalog* :key #'first :test #'string=))

(defun %runtime-setting-value-valid-p (spec value)
  (destructuring-bind (key type mode description &rest constraints) spec
    (declare (ignore key mode description))
    (case type
      (:boolean (or (eq value t) (eq value nil)))
      (:string (and (stringp value) (plusp (length value)) (<= (length value) 1000)))
      (:enum (and (stringp value) (member value (first constraints) :test #'string=)))
      (:integer (and (integerp value) (<= (first constraints) value (second constraints))))
      (:nullable-integer (or (null value)
                             (and (integerp value)
                                  (<= (first constraints) value (second constraints)))))
      (:number (and (realp value)
                    (<= (first constraints) value (second constraints))))
      (otherwise nil))))

(defun %runtime-setting-copy-table (table)
  (let ((copy (make-hash-table :test #'equal)))
    (maphash (lambda (key value) (setf (gethash key copy) value)) table)
    copy))

(defun %runtime-settings-project-events (seed)
  (let ((values (%runtime-setting-copy-table seed)) (revision 0))
    (dolist (event (replay-events :types '("runtime-settings-initialized"
                                           "runtime-setting-changed")))
      (let ((type (gethash "type" event)) (payload (gethash "payload" event)))
        (when (hash-table-p payload)
          (cond
            ((string= type "runtime-settings-initialized")
             (let ((initial (gethash "values" payload)))
               (when (hash-table-p initial)
                 (setf values (%runtime-setting-copy-table initial)))))
            ((string= type "runtime-setting-changed")
             (let ((key (gethash "key" payload)))
               (when (%runtime-setting-spec key)
                 (setf (gethash key values) (gethash "value" payload)))))))
        (setf revision (max revision (or (gethash "id" event) 0)))))
    (values values revision)))

(defun runtime-settings-initialize (seed &key (actor "startup-bootstrap"))
  "Restore settings from the ledger, seeding it once from launcher defaults."
  (unless (hash-table-p seed) (error "Runtime settings seed must be an object"))
  (dolist (spec *runtime-settings-catalog*)
    (multiple-value-bind (value present-p) (gethash (first spec) seed)
      (unless (and present-p (%runtime-setting-value-valid-p spec value))
        (error "Invalid or absent runtime setting seed: ~a" (first spec)))))
  (bt:with-lock-held (*runtime-settings-lock*)
    (let ((events (replay-events :types '("runtime-settings-initialized"
                                          "runtime-setting-changed"))))
      (unless events
        (multiple-value-bind (id durable-p)
            (log-event "runtime-settings-initialized"
                       (obj "schema_version" 1 "actor" actor
                            "values" (%runtime-setting-copy-table seed)))
          (unless durable-p (error "Runtime settings seed was not durable"))
          (setf *runtime-settings-revision* id)))
      (multiple-value-bind (values revision) (%runtime-settings-project-events seed)
        (setf *runtime-settings-values* values
              *runtime-settings-effective* (%runtime-setting-copy-table values)
              *runtime-settings-last-errors* (make-hash-table :test #'equal)
              *runtime-settings-revision* revision
              *runtime-settings-initialized-p* t))))
  (runtime-settings-report))

(defun runtime-settings-configure-applier (function)
  (unless (or (null function) (functionp function))
    (error "Runtime settings applier must be a function or NIL"))
  (bt:with-lock-held (*runtime-settings-lock*)
    (setf *runtime-settings-applier* function))
  (not (null function)))

(defun runtime-settings-value (key &optional default)
  (bt:with-lock-held (*runtime-settings-lock*)
    (gethash key *runtime-settings-values* default)))

(defun %runtime-setting-apply (key value mode)
  (if (eq mode :restart)
      nil
      (let ((applier *runtime-settings-applier*))
        (when (functionp applier) (funcall applier key value))
        t)))

(defun runtime-settings-apply-all ()
  (bt:with-lock-held (*runtime-settings-lock*)
    (dolist (spec *runtime-settings-catalog*)
      (let* ((key (first spec)) (mode (third spec))
             (value (gethash key *runtime-settings-values*)))
        (unless (eq mode :restart)
          (handler-case
              (when (%runtime-setting-apply key value mode)
                (setf (gethash key *runtime-settings-effective*) value)
                (remhash key *runtime-settings-last-errors*))
            (error (condition)
              (setf (gethash key *runtime-settings-last-errors*)
                    (princ-to-string condition))))))))
  (runtime-settings-report))

(defun runtime-settings-update (key value &key (actor "authenticated-operator"))
  "Validate and append one desired setting, then apply it at its safe boundary."
  (let ((spec (%runtime-setting-spec key)))
    (unless spec (error "Unknown runtime setting ~s" key))
    (unless (%runtime-setting-value-valid-p spec value)
      (error "Invalid value for runtime setting ~a" key))
    (bt:with-lock-held (*runtime-settings-lock*)
      (unless *runtime-settings-initialized-p* (error "Runtime settings are not initialized"))
      (let ((previous (gethash key *runtime-settings-values*)))
        (multiple-value-bind (id durable-p)
            (log-event "runtime-setting-changed"
                       (obj "schema_version" 1 "key" key "value" value
                            "previous" previous "actor" actor
                            "apply_mode" (string-downcase (symbol-name (third spec)))))
          (unless durable-p (error "Runtime setting change was not durable"))
          (setf (gethash key *runtime-settings-values*) value
                *runtime-settings-revision* id)
          (unless (eq (third spec) :restart)
            (handler-case
                (progn
                  (%runtime-setting-apply key value (third spec))
                  (setf (gethash key *runtime-settings-effective*) value)
                  (remhash key *runtime-settings-last-errors*))
              (error (condition)
                (setf (gethash key *runtime-settings-last-errors*)
                      (princ-to-string condition)))))))))
  (runtime-settings-report))

(defun runtime-settings-report ()
  (bt:with-lock-held (*runtime-settings-lock*)
    (let ((rows (make-array (length *runtime-settings-catalog*))))
      (loop for spec in *runtime-settings-catalog* for index from 0
            for key = (first spec)
            do (setf (aref rows index)
                     (obj "key" key
                          "type" (string-downcase (symbol-name (second spec)))
                          "apply_mode" (string-downcase (symbol-name (third spec)))
                          "description" (fourth spec)
                          "choices" (if (eq (second spec) :enum)
                                        (coerce (fifth spec) 'vector) :null)
                          "minimum" (if (member (second spec) '(:integer :nullable-integer :number))
                                        (fifth spec) :null)
                          "maximum" (if (member (second spec) '(:integer :nullable-integer :number))
                                        (sixth spec) :null)
                          "desired" (gethash key *runtime-settings-values*)
                          "effective" (gethash key *runtime-settings-effective*)
                          "pending_restart" (if (and (eq (third spec) :restart)
                                                     (not (equal (gethash key *runtime-settings-values*)
                                                                 (gethash key *runtime-settings-effective*))))
                                                t nil)
                          "error" (or (gethash key *runtime-settings-last-errors*) :null))))
      (obj "schema_version" 1 "initialized" (if *runtime-settings-initialized-p* t nil)
           "revision" *runtime-settings-revision* "settings" rows
           "excluded" (vector "credentials" "migration/import/export actions"
                              "state and database paths" "startup diagnostics")))))

;;; The page uses the web terminal's authenticated session.  Unlike the older
;;; prompt-admin API it needs no second bearer secret; the acceptor also
;;; enforces same-origin marking on mutations.
(defparameter *runtime-settings-html*
  (asset "../../adapters/web/assets/settings.html"))
(defparameter *runtime-settings-js*
  (asset "../../adapters/web/assets/settings.js"))

(defun %runtime-settings-json (value)
  (setf (hunchentoot:content-type*) "application/json; charset=utf-8"
        (hunchentoot:header-out "Cache-Control") "no-store")
  (shasht:write-json value nil))

(hunchentoot:define-easy-handler (runtime-settings-page :uri "/settings") ()
  (setf (hunchentoot:content-type*) "text/html; charset=utf-8")
  *runtime-settings-html*)

(hunchentoot:define-easy-handler (runtime-settings-script :uri "/settings.js") ()
  (setf (hunchentoot:content-type*) "text/javascript; charset=utf-8")
  *runtime-settings-js*)

(hunchentoot:define-easy-handler
    (runtime-settings-api :uri "/api/settings/runtime") ()
  (handler-case
      (case (hunchentoot:request-method*)
        (:get (%runtime-settings-json (runtime-settings-report)))
        (:post
         (let* ((body (hunchentoot:raw-post-data :force-text t))
                (data (and (stringp body) (<= (length body) 65536)
                           (shasht:read-json body))))
           (unless (hash-table-p data) (error "Expected one JSON object"))
           (multiple-value-bind (value present-p) (gethash "value" data)
             (unless present-p (error "Setting update requires value"))
             (%runtime-settings-json
              (runtime-settings-update (gethash "key" data) value
                                       :actor "authenticated-web-operator")))))
        (otherwise
         (setf (hunchentoot:return-code*) 405)
         (%runtime-settings-json (obj "error" "Method not allowed"))))
    (error (condition)
      (setf (hunchentoot:return-code*) 400)
      (%runtime-settings-json (obj "error" (princ-to-string condition))))))
