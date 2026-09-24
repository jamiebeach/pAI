;;;; Scoped foreground contexts with explicit arbitration and gated defaults.
(in-package :agent)

(defvar *sustained-activity-operator-lock* (bt:make-lock "operator-activity"))
(defvar *sustained-activity-default-continuity-p* nil
  "Qualification gate until full-request budgeting and source paging are ready.
When enabled, an unselected/completed scoped conversation enrolls its first turn.
An explicit pause remains an opt-out, never an inferred task completion.")

(defun %sao-create (backend root channel resource-id reason)
  "Create membership from a trusted admitted root; no model or tool side effects."
  (multiple-value-bind (agent persona channel resource) (%sao-scope backend channel resource-id)
    (let* ((event (storage-read-event backend root :agent-id agent :event-type "user-message"))
           (body (and event (gethash "payload" event)))
           (meta (and body (gethash "metadata" body))))
      (unless (and (hash-table-p meta) (equal channel (gethash "channel" body))
                   (equal persona (gethash "persona_id" meta)))
        (error "Default continuity requires a scoped admitted operator root"))
      (let ((payload
              (obj "schema_version" 1
                   "activity_id" (format nil "operator:~a:~d:~d" resource root (storage-head-position backend))
                   "persona_id" persona "channel" channel "resource_id" resource
                   "root_event_ids" (vector root) "evidence_event_ids" (vector root)
                   "previous_reference_event_id" :null "actor" "runtime" "reason" reason
                   "retention_policy" "high-bandwidth" "attention_state" "active"
                   "completion_state" "open" "completion_confidence" :null)))
        (validate-activity-reference payload)
        (log-event "sustained-activity-revised" payload :caused-by root
                   :expected-head (storage-head-position backend))))))

(defun %sao-default-start-p (event)
  (and *sustained-activity-default-continuity-p*
       (or (null event)
           (equal "complete" (gethash "completion_state" (gethash "payload" event))))))

(defun %sao-select (backend reference-id current channel resource-id)
  "Explicitly foreground a saved task; scope validation precedes any append."
  (multiple-value-bind (agent persona channel resource) (%sao-scope backend channel resource-id)
    (let* ((candidate (storage-read-event backend reference-id :agent-id agent
                                          :event-type "sustained-activity-revised"))
           (payload (and candidate (gethash "payload" candidate))))
      (unless (and (hash-table-p payload) (equal persona (gethash "persona_id" payload))
                   (equal channel (gethash "channel" payload))
                   (equal resource (gethash "resource_id" payload)))
        (error "Selected task is outside the authenticated conversation scope"))
      (let ((latest (storage-latest-activity-reference backend agent persona channel resource
                                                      :activity-id (gethash "activity_id" payload))))
        (when (and (%sao-active-p current)
                   (not (equal (gethash "activity_id" (gethash "payload" current))
                               (gethash "activity_id" payload))))
          (let* ((roots (gethash "root_event_ids" (gethash "payload" current)))
                 (root (aref roots (1- (length roots)))))
            (%sao-revise backend current channel resource-id :evidence-event-id root
                         :attention-state "interrupted" :reason "Operator switched foreground task")))
        (let* ((roots (gethash "root_event_ids" (gethash "payload" latest)))
               (root (aref roots (1- (length roots)))))
          (%sao-revise backend latest channel resource-id :evidence-event-id root
                       :attention-state "active" :completion-state "open"
                       :completion-confidence :null :reason "Operator selected saved task"))
        (format nil "Selected task ~a. Original task history is retained."
                (gethash "activity_id" payload))))))

(defun %sao-scope (backend channel resource-id)
  (unless (and (event-authority-owns-storage-p backend)
               (stringp resource-id) (plusp (length resource-id)))
    (error "Activity controls require the installed authority and trusted resource scope"))
  (values *conscious-recursive-mind-agent-id*
          (gethash "persona_id" (%conversation-persona-profile)) channel resource-id))

(defun %sao-latest (backend channel resource-id)
  (multiple-value-bind (agent persona channel resource) (%sao-scope backend channel resource-id)
    (storage-latest-activity-reference backend agent persona channel resource)))

(defun %sao-active-p (event)
  (let ((p (and event (gethash "payload" event))))
    (and p (equal "active" (gethash "attention_state" p))
         (not (equal "complete" (gethash "completion_state" p))))))

(defun %sao-packet (backend event)
  (multiple-value-bind (ref rows coverage)
      (storage-read-activity-context backend (gethash "id" event)
                                    :agent-id *conscious-recursive-mind-agent-id*
                                    :through-event-id (storage-max-event-id backend :agent-id *conscious-recursive-mind-agent-id*))
    (unless (equal "complete" (gethash "status" coverage))
      (error 'activity-context-error :code "activity-source-limit"
             :public-message "Continuation was not admitted: this activity exceeds its bounded source-read allowance. History is preserved. Source paging is not yet available; /activity pause permits ordinary conversation without deleting the activity."))
    (let ((packet (project-sustained-activity-context ref rows :defer-budget-p t)))
      (unless (equal "ready" (gethash "status" packet))
        (if (equal "incomplete" (gethash "status" packet))
            (error 'activity-context-error :code "activity-incomplete"
                   :public-message "Continuation was not admitted: an earlier activity turn is unfinished. Inspect its status before using /activity recover, which may resume tools. /activity pause preserves history and permits ordinary conversation.")
            (error 'activity-context-error :code "activity-protected-context-limit"
                   :public-message "Continuation was not admitted: native working history still exceeds its local allowance after compacting older execution evidence. The latest exchange, older dialogue and retrieval index are retained. This is a local character limit, not a measured provider token limit. Nothing was discarded; /activity recover cannot resolve this size limit.")))
      (setf (gethash "coverage" packet) coverage)
      packet)))

(defun %sao-revise (backend event channel resource-id &rest options)
  (multiple-value-bind (agent persona channel resource) (%sao-scope backend channel resource-id)
    (apply #'sustained-activity-revise backend (gethash "id" event) (storage-head-position backend)
           :agent-id agent :persona-id persona :channel channel :resource-id resource
           :actor "operator" options)))

(defun sustained-activity-operator-command (backend words channel resource-id)
  "Called only by an authenticated deterministic operator-command transport."
  (bt:with-lock-held (*sustained-activity-operator-lock*)
    (bt:with-lock-held (*conscious-recursive-mind-lock*)
      (let* ((event (%sao-latest backend channel resource-id))
             (action (or (first words) "status"))
             (p (and event (gethash "payload" event))))
        (unless (and (<= (length words) 2)
                     (or (<= (length words) 1) (member action '("start" "recover" "select") :test #'equal)))
          (error "Use /activity start [ROOT-ID], select REFERENCE-ID, status, window, pause, resume, complete, or recover"))
        (cond
          ((equal action "select")
           (unless (second words) (error "Use /activity select REFERENCE-ID"))
           (%sao-select backend (parse-integer (second words)) event channel resource-id))
          ((equal action "start")
           (when (%sao-active-p event) (error "An activity is already active; pause it before starting another"))
           (let* ((root (if (second words)
                            (storage-read-event backend (parse-integer (second words))
                                                :agent-id *conscious-recursive-mind-agent-id* :event-type "user-message")
                            (find-if (lambda (row)
                                       (let* ((body (gethash "payload" row)) (meta (gethash "metadata" body)))
                                         (and (equal channel (gethash "channel" body)) (hash-table-p meta)
                                              (equal (gethash "persona_id" (%conversation-persona-profile))
                                                     (gethash "persona_id" meta)))))
                                     (reverse (storage-recent-events backend '("user-message") 64
                                                                     :agent-id *conscious-recursive-mind-agent-id*)))))
                  (body (and root (gethash "payload" root)))
                  (meta (and body (gethash "metadata" body)))
                  (id (and root (gethash "id" root))))
             (unless (and id (hash-table-p meta) (equal channel (gethash "channel" body))
                          (equal (gethash "persona_id" (%conversation-persona-profile)) (gethash "persona_id" meta)))
               (error "No matching operator turn; send the initial task first, then /activity start"))
             (let ((payload (obj "schema_version" 1 "activity_id" (format nil "operator:~a:~d:~d" resource-id id (storage-head-position backend))
                                 "persona_id" (gethash "persona_id" meta) "channel" channel "resource_id" resource-id
                                 "root_event_ids" (vector id) "evidence_event_ids" (vector id)
                                 "previous_reference_event_id" :null "actor" "operator"
                                 "reason" "Operator explicitly selected sustained context for this conversation"
                                 "retention_policy" "high-bandwidth" "attention_state" "active"
                                 "completion_state" "open" "completion_confidence" :null)))
               (validate-activity-reference payload)
               (log-event "sustained-activity-revised" payload :caused-by id
                          :expected-head (storage-head-position backend))
               (format nil "High-bandwidth activity started from root ~d. Subsequent turns in this operator conversation will join it. /activity window previews history; /activity pause returns to ordinary context." id))))
          ((equal action "status")
           (if event (format nil "Activity ~a · reference ~d · ~d roots · ~a · completion ~a"
                             (gethash "activity_id" p) (gethash "id" event) (length (gethash "root_event_ids" p))
                             (gethash "attention_state" p) (gethash "completion_state" p))
               (if *sustained-activity-default-continuity-p*
                   "No saved task yet. The next admitted turn will start scoped continuity."
                   "No sustained activity selected. Send an initial task, then /activity start.")))
          ((null event) (error "No sustained activity exists for this operator conversation"))
          ((equal action "window")
           (let ((packet (%sao-packet backend event)))
             (format nil "Ledger working-history preview only (not the complete provider envelope):~%~a"
                     (shasht:write-json (obj "coverage" (gethash "coverage" packet)
                                             "messages" (sustained-activity-native-messages packet)) nil))))
          ((member action '("pause" "resume" "complete") :test #'equal)
           (let ((root (aref (gethash "root_event_ids" p) (1- (length (gethash "root_event_ids" p))))))
             (%sao-revise backend event channel resource-id :evidence-event-id root
                          :reason (format nil "Authenticated operator requested ~a" action)
                          :attention-state (if (equal action "resume") "active" "parked")
                          :completion-state (if (equal action "complete") "complete" (if (equal action "resume") "open" (gethash "completion_state" p)))
                          :completion-confidence :null)
             (format nil "Activity ~a recorded. Original ledger evidence is retained." action)))
          ((equal action "recover")
           (let* ((id (if (second words) (parse-integer (second words))
                          (aref (gethash "root_event_ids" p) (1- (length (gethash "root_event_ids" p))))))
                  (root (event-read-event id)) (body (gethash "payload" root))
                  (meta (gethash "metadata" body))
                  (context-failure (recursive-root-failure-receipt id))
                  (provider-request (%recursive-root-model-request
                                     (%recursive-thread-events) id)))
             (unless (find id (gethash "root_event_ids" p))
               (setf event
                     (%sao-revise backend event channel resource-id :add-root-event-id id
                                  :evidence-event-id id :reason "Operator recovered an admitted activity turn after interruption")))
             (if (and context-failure (null provider-request))
                 ;; A root-failure receipt is immutable terminal evidence.  An
                 ;; explicit operator recovery may safely create a linked retry
                 ;; only when the failed attempt never crossed the provider
                 ;; boundary; replaying the terminal root itself cannot progress.
                 (multiple-value-bind (retry-id interaction)
                     (%recursive-operator-admit
                      (gethash "text" body) channel
                      :activity-reference-event-id (gethash "id" event)
                      :recovery-of-event-id id)
                   (%sao-revise backend (%sao-latest backend channel resource-id)
                                channel resource-id :add-root-event-id retry-id
                                :evidence-event-id retry-id
                                :reason "Operator retried a terminal pre-provider activity turn")
                   (%recursive-maybe-resolve-graph-confirmation
                    retry-id (gethash "text" body))
                   (shasht:write-json
                    (%recursive-run-root-locked
                     retry-id interaction :channel channel
                     :content (gethash "text" body))
                    nil))
                 (shasht:write-json
                  (%recursive-run-root-locked
                   id (gethash "interaction_id" meta)
                   :channel channel :content (gethash "text" body))
                  nil))))
          (t (error "Unknown activity action; use start, select, status, window, pause, resume, complete, or recover")))))))

(defun sustained-activity-operator-submit (backend prompt channel resource-id)
  "Serialize this operator surface so queued turns select the latest membership.
The qualification gate enables default creation; explicit pause remains honored."
  (bt:with-lock-held (*sustained-activity-operator-lock*)
    (let ((event (%sao-latest backend channel resource-id)))
      (if (and (not (%sao-active-p event)) (not (%sao-default-start-p event)))
          (conscious-recursive-mind-submit prompt :channel channel)
          (progn
            (%recursive-operator-waiter-change 1)
            (let ((waiting-p t))
             (unwind-protect
                 (bt:with-lock-held (*conscious-recursive-mind-lock*)
                   (%recursive-operator-waiter-change -1)
                   (setf waiting-p nil)
                   ;; Refuse pressure/incomplete prior work before admitting new input.
                   (when (%sao-active-p event) (%sao-packet backend event))
                   (multiple-value-bind (root interaction)
                       (%recursive-operator-admit prompt channel
                                                 :activity-reference-event-id
                                                 (and (%sao-active-p event) (gethash "id" event)))
                     ;; Enroll before execution: interruptions retain the new root.
                     (if (%sao-active-p event)
                         (%sao-revise backend event channel resource-id :add-root-event-id root
                                      :evidence-event-id root :reason "Operator continued the foreground task")
                         (%sao-create backend root channel resource-id
                                      "Default continuity for newly admitted scoped conversation"))
                     (%recursive-maybe-resolve-graph-confirmation root prompt)
                     (%recursive-run-root-locked root interaction :channel channel :content prompt)))
              (when waiting-p (%recursive-operator-waiter-change -1)))))))))
