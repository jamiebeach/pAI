;;;; stabilization-smoke-tests.lisp -- deterministic recovery probes.

(in-package :agent)

(export '(stabilization-smoke-run stabilization-smoke-report
          stabilization-state-fingerprint
          stabilization-fingerprint-equivalent-p))

(declaim (special *latent-v2-thoughts* *latent-v2-file*
                  *last-self-mod-history* *initiative-v2-decisions*
                  *event-next-id* *self-model* *soul* *wrap-chains*))

(defvar *stabilization-smoke-last-report* nil)
(defvar *stabilization-smoke-probe-overrides* (make-hash-table :test #'equal)
  "Test/scratch adapters keyed by probe name. Production defaults are no-network.")

(defun %smoke-hash (value)
  (let ((text (if (stringp value) value
                  (with-output-to-string (out) (shasht:write-json value out)))))
    (if (fboundp '%latent-v2-hash) (funcall '%latent-v2-hash text)
        (format nil "~16,'0x" (ldb (byte 64 0) (sxhash text))))))

(defun %smoke-projection ()
  (unless (fboundp 'render-context-projection) (error "projection renderer missing"))
  (let ((rendered
          (render-context-projection
           (obj "current_exchange"
                (vector (obj "speaker" "the operator" "content" "recovery probe"))
                "publication_guidance" "Recovery guidance remains available."
                "relevant_shared_memory" (vector)
                "audited_background_activity" (vector)
                "open_loops" (vector) "affect" (obj) "drives" (obj)
                "audit_status" "complete" "audit_window_seconds" 0
                "audit_user_boundary_found" nil "temporal_query" nil))))
    (and (stringp rendered)
         (search "Recovery guidance remains available." rendered)
         (null (search "recovery probe" rendered))
         (search "<!-- PAI-STATE:BEGIN -->" rendered)
         (search "<!-- PAI-STATE:END -->" rendered))))

(defun %smoke-admission-validator ()
  (unless (fboundp '%epistemic-validation-reasons-current-connection)
    (error "admission validator missing"))
  ;; Invalid synthetic admission is intentionally rejected before any write.
  (multiple-value-bind (reasons roots)
      (with-pg
        (%epistemic-validation-reasons-current-connection
         :id "recovery-invalid" :kind "thought" :origin-class "synthetic"
         :epistemic-status "hypothesis" :producer "recovery-probe"
         :model-purpose "recovery" :confidence 0.5d0
         :grounding-status "grounded" :lineage-parent-ids nil))
    (declare (ignore roots))
    (plusp (length reasons))))

(defun %smoke-handler-registry ()
  (let ((checks '((%extract-curiosity-finding . ("and found: result"))
                  (%latent-v2-overlap . ("alpha beta gamma" "alpha beta delta"))
                  (%initiative-v2-preview . ("preview"))
                  (%context-projection-temporal-p . ("what happened overnight?")))))
    (dolist (entry checks t)
      (unless (fboundp (car entry)) (error "handler missing: ~a" (car entry)))
      ;; Invocation, not mere binding, catches the bound-but-broken class.
      (apply (symbol-function (car entry)) (cdr entry)))))

(defun %smoke-multipart-capture ()
  (unless (and (fboundp '%turn-capture-new-context)
               (fboundp '%turn-capture-add-entry)
               (fboundp '%turn-capture-ordered-entries))
    (error "turn capture helpers missing"))
  (let ((context (%turn-capture-new-context "user")))
    (%turn-capture-add-entry context "user" "user" 1)
    (%turn-capture-add-entry context "assistant" "part one" 2 :final-p nil)
    (%turn-capture-add-entry context "tool" "tool result" 3
                             :tool-call-id "tool-1" :tool-name "fixture")
    (%turn-capture-add-entry context "assistant" "part two" 4 :final-p t)
    (equal '("user" "assistant" "tool" "assistant")
           (mapcar (lambda (entry) (gethash "role" entry))
                   (%turn-capture-ordered-entries context)))))

(defun %smoke-initiative-latent ()
  (unless (and (fboundp '%initiative-v2-gates) (fboundp 'latent-v2-seed)
               (fboundp 'latent-v2-transition))
    (error "initiative/latent v2 missing"))
  (let* ((outward (obj "proposed_content" "Concrete scratch draft."
                       "audience" "the operator" "topic" "scratch"
                       "earliest_at" :null "expires_at" :null))
         (gates (%initiative-v2-gates outward nil (get-universal-time) t nil nil))
         (*latent-v2-thoughts* nil)
         (*latent-v2-file* #P"/tmp/pai-recovery-latent-v2.json"))
    (unwind-protect
         (multiple-value-bind (thought status)
             (latent-v2-seed "Recovery scratch private thought."
                             :topic "recovery-scratch" :evidence-ids '("scratch-evidence"))
           (and (find "ungrounded-evidence" gates :test #'string=)
                (string= status "seeded")
                (multiple-value-bind (ready reason)
                    (latent-v2-transition (gethash "id" thought) "draft"
                                          :content "Recovery scratch completed draft.")
                  (and ready (null reason) (string= (gethash "state" ready) "ready")))))
      (ignore-errors (delete-file *latent-v2-file*)))))

(defun %smoke-wrap-chains ()
  (unless (boundp '*wrap-chains*) (error "wrap registry missing"))
  (let ((seen 0))
    (maphash
     (lambda (target files)
       (declare (ignore target))
       (let ((list (coerce files 'list)))
         (unless (= (length list) (length (remove-duplicates list :test #'string=)))
           (error "duplicate wrapper layer"))
         (incf seen)))
     *wrap-chains*)
    (plusp seen)))

(defun %smoke-heartbeat-event ()
  (and (fboundp 'conversation-persistence-ready-p)
       (not (null (conversation-persistence-ready-p)))
       (fboundp 'replay-events)
       (listp (replay-events :from (get-universal-time) :to (get-universal-time)))))

(defun %smoke-scheduler ()
  "Read-only clock/scheduler wiring probe; creates no job or timer."
  (and (fboundp 'pai-timezone-name)
       (fboundp 'pai-format-local-time)
       (fboundp 'pai-scheduler-report)
       (fboundp 'pai-cron-next-fire)
       (stringp (pai-timezone-name))
       (stringp (pai-format-local-time))
       (hash-table-p (pai-scheduler-report))))

(define-condition grounded-recovery-rollback (condition) ())

(defun %smoke-grounded-table-counts ()
  (with-pg
    (mapcar
     (lambda (table)
       (pomo:query (format nil "SELECT count(*) FROM ~a" table) :single))
     '("grounded_project_proposals" "agent_processes" "creative_projects"
       "agent_process_operations" "agent_artifacts"
       "agent_artifact_versions" "agent_attestations"
       "publication_candidates"))))

(defun %smoke-grounded-agency-transaction ()
  "Execute the complete synthetic story authority chain under one forced
rollback. The probe binds all event, model, and delivery adapters locally."
  (unless (and (boundp '*pai-pg-reuse-current-transaction-p*)
               (fboundp 'grounded-project-proposal-create)
               (fboundp 'initiative-v2-observe-shadow-candidate))
    (error "grounded recovery authority is incomplete"))
  (let* ((before (%smoke-grounded-table-counts))
         (result nil)
         (temp-file #P"/tmp/pai-grounded-recovery-initiative.json"))
    (unwind-protect
         (handler-case
             (with-pg
               (pomo:with-transaction ()
                 (let* ((*pai-pg-reuse-current-transaction-p* t)
                        (*grounded-agency-mode* :shadow)
                        (*autonomous-write-mode* :normal)
                        (*first-person-evidence-event-fn*
                          (lambda (&rest values) (declare (ignore values)) nil))
                        (*initiative-v2-event-fn*
                          (lambda (&rest values) (declare (ignore values)) nil))
                        (*initiative-v2-decisions* nil)
                        (*initiative-v2-file* temp-file)
                        (*initiative-v2-delivery-fn*
                          (lambda (&rest values)
                            (declare (ignore values))
                            (error "recovery reached delivery")))
                        (*initiative-v2-scorer-fn*
                          (lambda (options)
                            (obj "results"
                                 (coerce
                                  (mapcar
                                   (lambda (option)
                                     (obj "candidate_id" (gethash "id" option)
                                          "user_value" 9 "agent_outcome" 8
                                          "timing_quality" 9
                                          "interruption_cost" 1
                                          "confidence" 0.95d0))
                                   options)
                                  'vector))))
                        (*grounded-appraisal-modulator-fn*
                          (lambda (name)
                            (if (string= name "arousal") 0.5d0 0.0d0)))
                        (*first-person-evidence-node-fn*
                          (lambda (id)
                            (when (string= id "recovery-shared-root")
                              (obj "id" id "origin_class" "lived-user"
                                   "epistemic_status" "user-report"
                                   "grounding_status" "grounded"
                                   "quarantined" nil
                                   "root_observation_ids" (vector)
                                   "shared_conversation" t
                                   "participants" (vector "the operator" "the agent")
                                   "content" "Synthetic recovery story seed."))))
                        (budget (%fpe-copy-object
                                 *first-person-evidence-default-budget*)))
                   (setf (gethash "minimum_seconds_between_operations" budget) 0)
                   (let* ((proposal
                            (grounded-project-proposal-create
                             '("recovery-shared-root")
                             "Executable recovery scratch project."))
                          (proposal-id (gethash "id" proposal)))
                     (grounded-project-proposal-review
                      proposal-id :approved :actor "scratch-operator"
                      :reason "executable-recovery")
                     (let* ((process
                              (agent-process-start-from-approved-proposal
                               proposal-id :budget budget))
                            (process-id (gethash "id" process))
                            (owner "recovery-scratch-owner")
                            (usage (obj "model_name" "synthetic-recovery"
                                        "prompt_tokens" 0
                                        "completion_tokens" 0 "cost" 0.0d0))
                            (contents
                              '(("recovery-cycle-1" "outline"
                                 "A letter appears, a promise is tested, and a choice resolves it.")
                                ("recovery-cycle-2" "draft"
                                 "Mira found a letter before dawn and carried it home through rain before choosing to open it.")
                                ("recovery-cycle-3" "revise"
                                 "Before dawn, Mira found the letter beneath a cedar step. It carried it through the storm, opened it beside the hearth, and chose to keep its difficult promise.")))
                            (validation-operation-id nil))
                       (dolist (entry contents)
                         (destructuring-bind (cycle type content) entry
                           (declare (ignore type))
                           (let* ((operation
                                    (grounded-agency-claim-next-operation
                                     process-id cycle))
                                  (leased
                                    (agent-operation-acquire-lease
                                     (gethash "id" operation) owner 60)))
                             (agent-artifact-commit-operation
                              (gethash "id" leased) owner content :usage usage))))
                       (let* ((operation
                                (grounded-agency-claim-next-operation
                                 process-id "recovery-cycle-4"))
                              (leased
                                (agent-operation-acquire-lease
                                 (gethash "id" operation) owner 60))
                              (artifact
                                (agent-artifact-get
                                 (gethash "input_artifact_id" leased))))
                         (setf validation-operation-id (gethash "id" leased))
                         (agent-validation-commit-operation
                          validation-operation-id owner
                          (obj "passed" t "artifact_sha256"
                               (gethash "sha256" artifact)
                               "violation_codes" (vector))))
                       (let* ((operation
                                (grounded-agency-claim-next-operation
                                 process-id "recovery-cycle-5"))
                              (leased
                                (agent-operation-acquire-lease
                                 (gethash "id" operation) owner 60)))
                         (agent-process-complete-from-validation
                          (gethash "id" leased) owner
                          :validation-operation-id validation-operation-id))
                       (agent-appraisal-compute process-id)
                       (let* ((grants
                                (compile-claim-grants
                                 process-id "private-project-sharing-shadow"))
                              (candidate
                                (semantic-publication-compile-candidate
                                 process-id grants))
                              (decision
                                (initiative-v2-observe-shadow-candidate
                                 (gethash "rendered_text" candidate)
                                 (list (funcall *first-person-evidence-node-fn*
                                                "recovery-shared-root"))
                                 :trigger-event-ids '("recovery-cycle-5")
                                 :topic process-id
                                 :claim-grants (gethash "claim_grants" candidate)
                                 :candidate-id (gethash "id" candidate))))
                         (setf result
                               (and (string= (gethash "status" candidate) "valid")
                                    (not (gethash "delivery_reachable" decision))
                                    (= 5 (pomo:query
                                          "SELECT count(*) FROM agent_process_operations WHERE process_id=$1"
                                          process-id :single)))))))
                   (error 'grounded-recovery-rollback))))
           (grounded-recovery-rollback () nil)
           (error (condition)
             (return-from %smoke-grounded-agency-transaction
               (obj "ok" nil "error_class"
                    (string-downcase (symbol-name (type-of condition)))
                    "error" (format nil "~a" condition)))))
      (ignore-errors (delete-file temp-file)))
    (obj "ok" (and result (equal before (%smoke-grounded-table-counts)))
         "rolled_back" t "delivery_calls" 0)))

(defparameter *stabilization-smoke-probes*
  `(("projection" . ,#'%smoke-projection)
    ("admission-rollback" . ,#'%smoke-admission-validator)
    ("handlers" . ,#'%smoke-handler-registry)
    ("multipart-turn" . ,#'%smoke-multipart-capture)
    ("initiative-latent-scratch" . ,#'%smoke-initiative-latent)
    ("wrap-chains" . ,#'%smoke-wrap-chains)
    ("heartbeat-event" . ,#'%smoke-heartbeat-event)
    ("scheduler" . ,#'%smoke-scheduler)
    ("grounded-agency-transaction" . ,(lambda ()
                                          (if (fboundp 'grounded-agency-report)
                                              (%smoke-grounded-agency-transaction)
                                              (obj "ok" t "skipped" t
                                                   "reason" "module-not-loaded"))))))

(defun stabilization-smoke-run ()
  (let ((results (obj)) (ok t))
    (dolist (entry *stabilization-smoke-probes*)
      (let* ((name (car entry))
             (probe (or (gethash name *stabilization-smoke-probe-overrides*)
                        (cdr entry))))
        (handler-case
            (let* ((raw (funcall probe))
                   (structured (hash-table-p raw))
                   (passed (if structured (not (null (gethash "ok" raw)))
                               (not (null raw)))))
              (setf (gethash name results)
                    (if structured raw
                        (obj "ok" passed "error_class" :null)))
              (unless passed (setf ok nil)))
          (error (condition)
            (setf ok nil (gethash name results)
                  (obj "ok" nil "error_class"
                       (string-downcase (symbol-name (type-of condition)))))))))
    (setf *stabilization-smoke-last-report*
          (obj "level" "executable" "ok" ok "network_calls" 0
               "probes" results))
    *stabilization-smoke-last-report*))

(defun stabilization-smoke-report ()
  (or *stabilization-smoke-last-report*
      (obj "level" "executable" "ok" :null "probes" (obj))))

(defun %smoke-db-fingerprint ()
  (handler-case
      (with-pg
        (let ((counts (obj)))
          (dolist (row (pomo:query
                        "SELECT origin_class||'/'||epistemic_status||'/'||grounding_status,count(*) FROM memory_nodes GROUP BY 1 ORDER BY 1"))
            (setf (gethash (first row) counts) (second row)))
          counts))
    (error () :null)))

(defun stabilization-state-fingerprint ()
  "Fingerprint durable state only; process-local threads/connections excluded."
  (let* ((history (and (boundp '*last-self-mod-history*) *last-self-mod-history*))
         (last-message (and history (car (last history))))
         (candidate (and (boundp '*initiative-v2-decisions*)
                         (first *initiative-v2-decisions*)))
         (latent (and (boundp '*latent-v2-thoughts*) (first *latent-v2-thoughts*)))
         (durable
           (obj "conversation_count" (length history)
                "conversation_last_hash" (%smoke-hash (or last-message ""))
                "event_next_id" (if (boundp '*event-next-id*) *event-next-id* :null)
                "memory_counts" (%smoke-db-fingerprint)
                "latest_candidate_id" (or (and candidate (gethash "id" candidate)) :null)
                "latest_latent_id" (or (and latent (gethash "id" latent)) :null)
                "self_model_hash" (%smoke-hash (if (boundp '*self-model*) *self-model* ""))
                "soul_hash" (%smoke-hash (if (boundp '*soul*) *soul* ""))
                "user_timezone" (if (fboundp 'pai-timezone-name)
                                     (pai-timezone-name) :null)
                "schedule_hash"
                (%smoke-hash (if (fboundp 'pai-schedule-list)
                                 (pai-schedule-list) (make-array 0)))
                "scheduled_context_hash"
                (%smoke-hash
                 (if (fboundp 'pai-scheduler-context-snapshot)
                     (pai-scheduler-context-snapshot) (make-array 0)))
                "grounded_agency_hash"
                (%smoke-hash
                 (if (fboundp 'grounded-agency-state-fingerprint)
                     (grounded-agency-state-fingerprint)
                     (obj "schema_version" 1 "unavailable" t)))
                "config_hash" (%smoke-hash (if (fboundp 'stabilization-mode-report)
                                                (stabilization-mode-report) (obj))))))
    (setf (gethash "fingerprint" durable) (%smoke-hash durable))
    durable))

(defun stabilization-fingerprint-equivalent-p (before after)
  "Durable semantic equivalence across recovery. Audit/event restoration is
monotonic rather than byte-identical because recovery emits its own events."
  (and (hash-table-p before) (hash-table-p after)
       (every
        (lambda (key)
          (string= (with-output-to-string (out)
                     (shasht:write-json (gethash key before) out))
                   (with-output-to-string (out)
                     (shasht:write-json (gethash key after) out))))
        '("conversation_count" "conversation_last_hash" "memory_counts"
          "latest_candidate_id" "latest_latent_id" "self_model_hash"
          "soul_hash" "user_timezone" "schedule_hash"
          "scheduled_context_hash" "grounded_agency_hash" "config_hash"))
       (numberp (gethash "event_next_id" before))
       (numberp (gethash "event_next_id" after))
       (>= (gethash "event_next_id" after) (gethash "event_next_id" before))))
