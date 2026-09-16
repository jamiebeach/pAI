;;;; pulse-runtime.lisp -- Q3 durable adapter for deterministic pulses.
;;;;
;;;; The pure engine plans. This adapter owns the event boundaries and proves
;;;; every returned append ID is durably replayable before exposing success.
;;;; It has no provider, tool/effect or publication route.

(in-package :agent)

(export '(conscious-pulse-runtime-run
          conscious-pulse-runtime-open-captured
          conscious-pulse-runtime-submit-captured
          conscious-pulse-runtime-fail-captured
          conscious-pulse-runtime-recover
          conscious-pulse-runtime-report))

(defvar *conscious-pulse-runtime-lock*
  (bt:make-lock "conscious-pulse-runtime"))
(defvar *conscious-pulse-runtime-in-flight* nil)
(defvar *conscious-pulse-runtime-pending-deliberation* nil)
(defvar *conscious-pulse-runtime-last-plan* nil)
(defvar *conscious-pulse-runtime-last-terminal-id* nil)
(defvar *conscious-pulse-runtime-last-error* nil)

(defparameter *conscious-pulse-terminal-types*
  '("pulse-committed" "pulse-cancelled" "pulse-failed" "pulse-recovered"))

(defun %pulse-runtime-events ()
  (cond ((fboundp 'event-projection-events)
         (funcall 'event-projection-events))
        ((fboundp 'replay-events)
         (funcall 'replay-events))
        (t (error "Pulse runtime requires the event replay port"))))

(defun %pulse-runtime-event (id type events)
  (find-if (lambda (event)
             (and (hash-table-p event)
                  (eql id (gethash "id" event))
                  (equal type (gethash "type" event))))
           events :from-end t))

(defun %pulse-runtime-append-readable
    (type payload &key caused-by (event-snapshot nil event-snapshot-p))
  (unless (fboundp 'log-event)
    (error "Pulse runtime requires the event append port"))
  (let* ((result (multiple-value-list
                  (funcall 'log-event type payload :caused-by caused-by)))
         (id (first result)))
    (unless id (error "Pulse runtime append of ~a returned no event id" type))
    (let* ((receipt-p (>= (length result) 3))
           (receipt (and receipt-p (third result)))
           (events
             (cond
               ((and receipt-p event-snapshot-p)
                (unless (second result)
                  (error "Pulse runtime ~a event ~s was not durably appended"
                         type id))
                (append event-snapshot (list receipt)))
               (t (%pulse-runtime-events))))
           (stored (%pulse-runtime-event id type events)))
      (unless stored
        (error "Pulse runtime ~a event ~s was not durably readable" type id))
      (values id stored events))))

(defun %pulse-runtime-next-sequence (events)
  (let ((latest 0))
    (dolist (event events (1+ latest))
      (when (and (hash-table-p event)
                 (equal "pulse-committed" (gethash "type" event)))
        (let* ((payload (gethash "payload" event))
               (sequence (and (hash-table-p payload)
                              (gethash "pulse_sequence" payload))))
          (when (and (integerp sequence) (plusp sequence)
                     (> sequence latest))
            (setf latest sequence)))))))

(defun %pulse-runtime-copy-plan (plan)
  (shasht:read-json (shasht:write-json plan nil)))

(defun %pulse-runtime-terminal-type (plan)
  (let ((status (gethash "status" plan)))
    (cond ((string= status "completed") "pulse-committed")
          ((string= status "cancelled") "pulse-cancelled")
          (t "pulse-failed"))))

(defun %pulse-runtime-add-work-lineage (payload work-id parent-pulse-id)
  (when work-id
    (setf (gethash "work_id" payload) work-id
          (gethash "parent_pulse_id" payload) (or parent-pulse-id :null)))
  payload)

(defun %pulse-runtime-valid-work-lineage-p (work-id parent-pulse-id)
  (and (or (null work-id)
           (and (stringp work-id) (plusp (length work-id))
                (<= (length work-id) 256)))
       (or (null parent-pulse-id)
           (and work-id (stringp parent-pulse-id)
                (plusp (length parent-pulse-id))
                (<= (length parent-pulse-id) 256)))))

(defun %pulse-runtime-terminal-payload
    (plan sequence agent-id runtime-revision &key work-id parent-pulse-id)
  (let ((payload (%pulse-runtime-copy-plan plan)))
    (setf (gethash "pulse_sequence" payload) sequence
          (gethash "agent_id" payload) agent-id
          (gethash "runtime_revision" payload) runtime-revision
          (gethash "consumer" payload) "conscious-state"
          (gethash "stimulus_ids" payload)
          (gethash "consumed_stimulus_ids" plan)
          (gethash "disposition" payload)
          (let ((value (gethash "disposition" plan)))
            (if (stringp value) value :null)))
    (%pulse-runtime-add-work-lineage payload work-id parent-pulse-id)))

(defun %pulse-runtime-planning-failure-payload
    (pulse-id sequence agent-id runtime-revision purpose)
  ;; Never persist the condition text or the rejected input. Both may contain
  ;; private or non-data objects; the durable terminal records only the closed
  ;; failure class needed for recovery and operator truth.
  (obj "schema_version" *conscious-pulse-schema-version*
       "pulse_id" pulse-id "pulse_sequence" sequence
       "agent_id" agent-id "runtime_revision" runtime-revision
       "purpose" (string-downcase (symbol-name purpose))
       "status" "failed" "terminal_reason" "planning-rejected"
       "proposals" (vector) "consumed_stimulus_ids" (vector)
       "stimulus_ids" (vector) "consumer" "conscious-state"
       "disposition" :null "model_calls" 0 "tool_proposals" 0
       "continuations" 0 "tokens" 0 "cost_microunits" 0))

(defun %pulse-runtime-captured-failure-payload
    (pulse-id sequence agent-id runtime-revision purpose reason
     &key work-id parent-pulse-id)
  (let ((payload (%pulse-runtime-planning-failure-payload
                  pulse-id sequence agent-id runtime-revision purpose)))
    (setf (gethash "terminal_reason" payload) reason)
    (%pulse-runtime-add-work-lineage payload work-id parent-pulse-id)))

(defun %pulse-runtime-assembly-spec-value (spec key)
  (unless (hash-table-p spec) (error "Captured assembly spec must be an object"))
  (unless (nth-value 1 (gethash key spec))
    (error "Captured assembly spec is missing ~s" key))
  (gethash key spec))

(defun %pulse-runtime-build-assembly-context
    (spec pulse-id purpose runtime-revision state-revision clock-identity)
  (let ((allowed
          '("audience" "total_character_budget" "section_character_budgets"
            "sections" "eligible_evidence_ids" "available_tools"
            "permitted_proposal_kinds" "publication_constraints"
            "remaining_budget" "pre_render_refusals")))
    (loop for key being the hash-keys of spec
          unless (member key allowed :test #'string=)
            do (error "Unknown captured assembly spec key ~s" key))
    (make-conscious-assembly-context
     :pulse-id pulse-id :purpose (string-downcase (symbol-name purpose))
     :audience (%pulse-runtime-assembly-spec-value spec "audience")
     :runtime-revision runtime-revision
     :conscious-state-revision state-revision :clock-identity clock-identity
     :total-character-budget
     (%pulse-runtime-assembly-spec-value spec "total_character_budget")
     :section-character-budgets
     (%pulse-runtime-assembly-spec-value spec "section_character_budgets")
     :sections (%pulse-runtime-assembly-spec-value spec "sections")
     :eligible-evidence-ids
     (%pulse-runtime-assembly-spec-value spec "eligible_evidence_ids")
     :available-tools (%pulse-runtime-assembly-spec-value spec "available_tools")
     :permitted-proposal-kinds
     (%pulse-runtime-assembly-spec-value spec "permitted_proposal_kinds")
     :publication-constraints
     (%pulse-runtime-assembly-spec-value spec "publication_constraints")
     :remaining-budget
     (%pulse-runtime-assembly-spec-value spec "remaining_budget")
     :pre-render-refusals
     (gethash "pre_render_refusals" spec (vector)))))

(defun %pulse-runtime-captured-budget (spec model-call-budget)
  (let ((remaining (%pulse-runtime-assembly-spec-value spec "remaining_budget")))
    (unless (hash-table-p remaining)
      (error "Captured remaining budget must be an object"))
    (make-captured-pulse-budget
     :wall-milliseconds 1000
     :context-characters
     (%pulse-runtime-assembly-spec-value spec "total_character_budget")
     :proposals 8 :cancellation-checks 8 :model-calls model-call-budget
     :tool-proposals (gethash "tool_proposals" remaining 0)
     :continuations (gethash "continuations" remaining 0)
     :publication-candidates (gethash "publication_candidates" remaining 0))))

(defun %pulse-runtime-open-readable-or-release (payload events-before)
  (handler-case
      (%pulse-runtime-append-readable
       "pulse-opened" payload :event-snapshot events-before)
    (error (condition)
      ;; No readable durable open exists for this process to continue. A later
      ;; restore still scans the log and recovers it if visibility returns.
      (setf *conscious-pulse-runtime-in-flight* nil
            *conscious-pulse-runtime-pending-deliberation* nil
            *conscious-pulse-runtime-last-error* "pulse-open-unreadable")
      (error condition))))

(defun conscious-pulse-runtime-open-captured
    (state &key projection-context agent-id runtime-revision purpose now
                clock-identity assembly-spec (model-call-budget 0)
                work-id parent-pulse-id
                (event-snapshot nil event-snapshot-p))
  "Durably open one Q4 pulse and return its private request plus safe manifest."
  (unless (projection-context-p projection-context)
    (error "Captured pulse requires the exact pinned projection context"))
  (unless (%pulse-runtime-valid-work-lineage-p work-id parent-pulse-id)
    (error "Captured pulse cognitive work lineage is invalid"))
  (bt:with-lock-held (*conscious-pulse-runtime-lock*)
    (when *conscious-pulse-runtime-in-flight*
      (error "A conscious pulse is already in flight"))
    (let* ((events-before (if event-snapshot-p event-snapshot
                              (%pulse-runtime-events)))
           (sequence (%pulse-runtime-next-sequence events-before))
           (budget (%pulse-runtime-captured-budget assembly-spec model-call-budget))
           (open-payload
             (%pulse-runtime-add-work-lineage
              (obj "agent_id" agent-id "runtime_revision" runtime-revision
                   "source_state_revision" (gethash "state_revision" state)
                   "source_observation_revision"
                   (gethash "observation_revision" state)
                   "purpose" (string-downcase (symbol-name purpose))
                   "opened_at" now "clock_identity" clock-identity
                   "budget_snapshot" budget "deliberation_mode" "captured")
              work-id parent-pulse-id)))
      ;; Validate all pre-open inputs before claiming the single in-flight
      ;; slot. A malformed spec must not wedge later pulses.
       (setf *conscious-pulse-runtime-in-flight* t
             *conscious-pulse-runtime-last-error* nil)
       (multiple-value-bind (open-id open-event events-through-open)
           (%pulse-runtime-open-readable-or-release open-payload events-before)
        (declare (ignore open-event))
        (let ((pulse-id (format nil "pulse:~a" open-id)))
          (handler-case
              (let* ((snapshot (%pulse-runtime-copy-plan state))
                     (assembly-context
                       (%pulse-runtime-build-assembly-context
                        assembly-spec pulse-id purpose runtime-revision
                        (gethash "state_revision" snapshot) clock-identity))
                     (assembled
                       (conscious-context-assemble snapshot assembly-context)))
                (setf *conscious-pulse-runtime-pending-deliberation*
                      (obj "open_id" open-id "pulse_id" pulse-id
                           "pulse_sequence" sequence "state" snapshot
                           "projection_context" projection-context
                           "agent_id" agent-id
                           "runtime_revision" runtime-revision
                           "purpose" purpose "opened_at" now
                           "clock_identity" clock-identity "budget" budget
                           "manifest" (gethash "manifest" assembled)
                           "work_id" (or work-id :null)
                           "parent_pulse_id" (or parent-pulse-id :null)))
                assembled)
            (error (condition)
              (let ((failure (%pulse-runtime-captured-failure-payload
                              pulse-id sequence agent-id runtime-revision
                              purpose "context-assembly-rejected"
                              :work-id work-id
                              :parent-pulse-id parent-pulse-id)))
                (%pulse-runtime-append-readable
                 "pulse-failed" failure :caused-by open-id
                 :event-snapshot events-through-open)
                (setf *conscious-pulse-runtime-last-plan* failure
                      *conscious-pulse-runtime-in-flight* nil
                      *conscious-pulse-runtime-pending-deliberation* nil
                      *conscious-pulse-runtime-last-error* "context-assembly-rejected"))
              (error condition))))))))

(defun %pulse-runtime-captured-plan-or-fail (pending captured model-calls)
  ;; Validation/planning failure is safely terminalized. The later terminal
  ;; append remains outside this handler so append uncertainty cannot produce
  ;; a contradictory failure after a possible commit.
  (handler-case
      (let* ((manifest (gethash "manifest" pending))
             (validated (conscious-proposals-validate captured manifest)))
        (conscious-pulse-plan
         (gethash "state" pending)
         :pulse-id (gethash "pulse_id" pending)
         :agent-id (gethash "agent_id" pending)
         :runtime-revision (gethash "runtime_revision" pending)
         :purpose (gethash "purpose" pending)
         :opened-at (gethash "opened_at" pending)
         :now (gethash "opened_at" pending)
         :clock-identity (gethash "clock_identity" pending)
         :budget (gethash "budget" pending)
         :validated-proposals validated :context-manifest manifest
         :model-calls model-calls))
    (error (condition)
      (let* ((open-id (gethash "open_id" pending))
             (failure
               (%pulse-runtime-captured-failure-payload
                (gethash "pulse_id" pending)
                (gethash "pulse_sequence" pending)
                (gethash "agent_id" pending)
                (gethash "runtime_revision" pending)
                (gethash "purpose" pending)
                "captured-deliberation-rejected"
                :work-id
                (let ((value (gethash "work_id" pending)))
                  (and (stringp value) value))
                :parent-pulse-id
                (let ((value (gethash "parent_pulse_id" pending)))
                  (and (stringp value) value)))))
        (multiple-value-bind (terminal-id terminal ignored-events)
          (%pulse-runtime-append-readable
             "pulse-failed" failure :caused-by open-id)
          (declare (ignore terminal ignored-events))
          (setf *conscious-pulse-runtime-last-plan* failure
                *conscious-pulse-runtime-last-terminal-id* terminal-id
                *conscious-pulse-runtime-last-error*
                "captured-deliberation-rejected"
                *conscious-pulse-runtime-pending-deliberation* nil
                *conscious-pulse-runtime-in-flight* nil)))
      (error condition))))

(defun conscious-pulse-runtime-submit-captured (captured &key (model-calls 0))
  "Validate and durably commit a response for the currently open Q4 pulse."
  (bt:with-lock-held (*conscious-pulse-runtime-lock*)
    (let ((pending *conscious-pulse-runtime-pending-deliberation*))
      (unless (hash-table-p pending)
        (error "No captured conscious deliberation is pending"))
      (let* ((plan (%pulse-runtime-captured-plan-or-fail pending captured model-calls))
             (sequence (gethash "pulse_sequence" pending))
             (terminal-payload
               (%pulse-runtime-terminal-payload
                plan sequence (gethash "agent_id" pending)
                (gethash "runtime_revision" pending)
                :work-id
                (let ((value (gethash "work_id" pending)))
                  (and (stringp value) value))
                :parent-pulse-id
                (let ((value (gethash "parent_pulse_id" pending)))
                  (and (stringp value) value)))))
        (multiple-value-bind (terminal-id terminal stored-events)
          (%pulse-runtime-append-readable
             "pulse-committed" terminal-payload
             :caused-by (gethash "open_id" pending))
          (declare (ignore terminal))
          (setf (gethash "pulse_sequence" plan) sequence
                *conscious-pulse-runtime-last-plan* plan
                *conscious-pulse-runtime-last-terminal-id* terminal-id
                *conscious-pulse-runtime-pending-deliberation* nil
                *conscious-pulse-runtime-in-flight* nil)
          (values plan
                  (conscious-state-project
                   stored-events
                   :context (gethash "projection_context" pending))))))))

(defun conscious-pulse-runtime-fail-captured (reason)
  "Durably fail and release the currently opened captured/provider pulse."
  (unless (member reason '("provider-call-failed" "provider-response-invalid")
                  :test #'string=)
    (error "Unsupported captured pulse failure reason ~s" reason))
  (bt:with-lock-held (*conscious-pulse-runtime-lock*)
    (let ((pending *conscious-pulse-runtime-pending-deliberation*))
      (unless (hash-table-p pending)
        (error "No captured conscious deliberation is pending"))
      (let ((failure
              (%pulse-runtime-captured-failure-payload
               (gethash "pulse_id" pending)
               (gethash "pulse_sequence" pending)
               (gethash "agent_id" pending)
               (gethash "runtime_revision" pending)
               (gethash "purpose" pending) reason
               :work-id
               (let ((value (gethash "work_id" pending)))
                 (and (stringp value) value))
               :parent-pulse-id
               (let ((value (gethash "parent_pulse_id" pending)))
                 (and (stringp value) value)))))
        (multiple-value-bind (terminal-id terminal ignored-events)
          (%pulse-runtime-append-readable
             "pulse-failed" failure :caused-by (gethash "open_id" pending))
          (declare (ignore terminal ignored-events))
          (setf *conscious-pulse-runtime-last-plan* failure
                *conscious-pulse-runtime-last-terminal-id* terminal-id
                *conscious-pulse-runtime-last-error* reason
                *conscious-pulse-runtime-pending-deliberation* nil
                *conscious-pulse-runtime-in-flight* nil)
          failure)))))

(defun %pulse-runtime-plan-or-fail
    (state pulse-id open-id sequence agent-id runtime-revision purpose now
     finished-at clock-identity budget cancelled-p)
  ;; Delimit this handler to the pure planning call. An unreadable terminal
  ;; append is an uncertain commit, not a planning rejection, and must never
  ;; be followed by a second contradictory terminal record.
  (handler-case
      (conscious-pulse-plan
       state :pulse-id pulse-id :agent-id agent-id
       :runtime-revision runtime-revision :purpose purpose :opened-at now
       :now finished-at :clock-identity clock-identity :budget budget
       :cancelled-p cancelled-p)
    (error (planning-condition)
      (let ((failure (%pulse-runtime-planning-failure-payload
                      pulse-id sequence agent-id runtime-revision purpose)))
        (multiple-value-bind (terminal-id terminal ignored-events)
            (%pulse-runtime-append-readable
             "pulse-failed" failure :caused-by open-id)
          (declare (ignore terminal ignored-events))
          (setf *conscious-pulse-runtime-last-plan* failure
                *conscious-pulse-runtime-last-terminal-id* terminal-id)))
      (error planning-condition))))

(defun conscious-pulse-runtime-run
    (state &key projection-context agent-id runtime-revision purpose now
                clock-identity budget cancelled-p (finished-at now))
  "Open, plan and durably terminate one deterministic pulse.

Returns the terminal plan and the committed projection. Cancellation/failure
return the unchanged input projection because no commit occurred."
  (unless (projection-context-p projection-context)
    (error "Pulse runtime requires the exact pinned projection context"))
  (bt:with-lock-held (*conscious-pulse-runtime-lock*)
    (when *conscious-pulse-runtime-in-flight*
      (error "A conscious pulse is already in flight"))
    (setf *conscious-pulse-runtime-in-flight* t
          *conscious-pulse-runtime-last-error* nil)
    (unwind-protect
         (handler-case
             (let* ((events-before (%pulse-runtime-events))
                    (sequence (%pulse-runtime-next-sequence events-before))
                    (open-payload
                      (obj "agent_id" agent-id
                           "runtime_revision" runtime-revision
                           "source_state_revision"
                           (gethash "state_revision" state)
                           "source_observation_revision"
                           (gethash "observation_revision" state)
                           "purpose" (string-downcase (symbol-name purpose))
                           "opened_at" now "clock_identity" clock-identity
                           "budget_snapshot" budget)))
               (multiple-value-bind (open-id open-event ignored-events)
                   (%pulse-runtime-append-readable "pulse-opened" open-payload)
                 (declare (ignore open-event ignored-events))
                 (let ((pulse-id (format nil "pulse:~a" open-id)))
                   (let* ((plan (%pulse-runtime-plan-or-fail
                                 state pulse-id open-id sequence agent-id
                                 runtime-revision purpose now finished-at
                                 clock-identity budget cancelled-p))
                          (terminal-type (%pulse-runtime-terminal-type plan))
                          (terminal-payload
                            (%pulse-runtime-terminal-payload
                             plan sequence agent-id runtime-revision)))
                     (multiple-value-bind
                           (terminal-id terminal stored-events)
                         (%pulse-runtime-append-readable
                          terminal-type terminal-payload :caused-by open-id)
                       (declare (ignore terminal))
                       (setf (gethash "pulse_sequence" plan) sequence
                             *conscious-pulse-runtime-last-plan* plan
                             *conscious-pulse-runtime-last-terminal-id*
                             terminal-id)
                       (values
                        plan
                        (if (string= terminal-type "pulse-committed")
                            (conscious-state-project
                             stored-events :context projection-context)
                            state)))))))
           (error (condition)
             (setf *conscious-pulse-runtime-last-error*
                   (format nil "~a" condition))
             (error condition)))
      (setf *conscious-pulse-runtime-in-flight* nil))))

(defun %pulse-runtime-terminal-pulse-ids (events)
  (let ((ids (make-hash-table :test #'equal)))
    (dolist (event events ids)
      (when (and (hash-table-p event)
                 (member (gethash "type" event)
                         *conscious-pulse-terminal-types* :test #'equal))
        (let* ((payload (gethash "payload" event))
               (pulse-id (and (hash-table-p payload)
                              (gethash "pulse_id" payload))))
          (when (stringp pulse-id) (setf (gethash pulse-id ids) t)))))))

(defun conscious-pulse-runtime-recover (&key agent-id runtime-revision)
  "Durably terminalize every matching open pulse with no terminal record."
  (bt:with-lock-held (*conscious-pulse-runtime-lock*)
    (let* ((events (%pulse-runtime-events))
           (terminal-ids (%pulse-runtime-terminal-pulse-ids events))
           (recovered 0))
      (dolist (event events)
        (when (and (hash-table-p event)
                   (equal "pulse-opened" (gethash "type" event))
                   (or (null agent-id)
                       (equal agent-id (gethash "agent_id" event))))
          (let* ((open-id (gethash "id" event))
                 (pulse-id (format nil "pulse:~a" open-id))
                 (payload (gethash "payload" event))
                 (open-revision (and (hash-table-p payload)
                                     (gethash "runtime_revision" payload))))
            (when (and (not (gethash pulse-id terminal-ids))
                       (or (null runtime-revision)
                           (equal runtime-revision open-revision)))
              (%pulse-runtime-append-readable
               "pulse-recovered"
               (%pulse-runtime-add-work-lineage
                (obj "pulse_id" pulse-id "agent_id" agent-id
                     "runtime_revision" runtime-revision
                     "status" "recovered"
                     "terminal_reason" "orphaned-open"
                     "stimulus_ids" (vector)
                     "consumer" "conscious-state"
                     "disposition" :null)
                (let ((value (gethash "work_id" payload)))
                  (and (stringp value) value))
                (let ((value (gethash "parent_pulse_id" payload)))
                  (and (stringp value) value)))
               :caused-by open-id)
              (setf (gethash pulse-id terminal-ids) t)
              (incf recovered)))))
      (when (and (hash-table-p *conscious-pulse-runtime-pending-deliberation*)
                 (gethash (gethash "pulse_id"
                                   *conscious-pulse-runtime-pending-deliberation*)
                          terminal-ids))
        (setf *conscious-pulse-runtime-pending-deliberation* nil
              *conscious-pulse-runtime-in-flight* nil))
      recovered)))

(defun conscious-pulse-runtime-report ()
  "Content-free live adapter truth."
  (let ((plan *conscious-pulse-runtime-last-plan*)
        (pending *conscious-pulse-runtime-pending-deliberation*))
    (obj "in_flight" (if *conscious-pulse-runtime-in-flight* t nil)
         "last_terminal_event_id"
         (or *conscious-pulse-runtime-last-terminal-id* :null)
         "last_pulse"
         (if (hash-table-p plan)
             (conscious-pulse-plan-report plan) :null)
         "pending_deliberation"
         (if (hash-table-p pending)
             (conscious-context-manifest-report (gethash "manifest" pending))
             :null)
         "last_error" (or *conscious-pulse-runtime-last-error* :null)
         "provider_route" nil "effect_route" nil "publication_route" nil)))
