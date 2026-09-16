;;;; conversation-work-loop.lisp -- durable multi-quantum conversation owner.

(in-package :agent)

(declaim (special *embedding-turn-cache*))

(export '(conscious-conversation-work-configure
          conscious-conversation-work-run
          conscious-conversation-work-inspect
          conscious-conversation-work-rejoin
          conscious-conversation-work-start
          conscious-conversation-work-stop
          conscious-conversation-work-report))

(declaim (ftype (function () t) conscious-conversation-work-report))

(defparameter *conscious-conversation-work-revision*
  "conscious-conversation-work-v1")
(defparameter *conscious-conversation-work-lock*
  (bt:make-lock "conscious conversation work results"))
(defparameter *conscious-conversation-work-condition*
  (bt:make-condition-variable))
(defvar *conscious-conversation-work-agent-id* nil)
(defvar *conscious-conversation-work-profile* nil)
(defvar *conscious-conversation-work-runtime-plan* nil)
(defvar *conscious-conversation-work-turn-fn* nil)
(defvar *conscious-conversation-work-progress-fn* nil)
(defvar *conscious-conversation-work-results* (make-hash-table :test #'equal))
(defvar *conscious-conversation-work-timings* (make-hash-table :test #'equal))
(defvar *conscious-conversation-work-embedding-caches*
  (make-hash-table :test #'equal))

(defparameter *conscious-conversation-quantum-timing-keys*
  '("admission" "context_open" "request_journal" "provider"
    "response_journal" "captured_parse" "captured_commit"
    "publication_validation" "reply_commit" "memory_query_embedding"
    "memory_semantic_scan" "memory_neighborhood_scan" "unattributed"))

(defun %conscious-conversation-work-now ()
  (get-internal-real-time))

(defun %conscious-conversation-work-elapsed-ms (started &optional ended)
  (round (* 1000
            (/ (- (or ended (%conscious-conversation-work-now)) started)
               (coerce internal-time-units-per-second 'double-float)))))

(defun %conscious-conversation-work-timing-start (work-id &optional started-at)
  (bt:with-lock-held (*conscious-conversation-work-lock*)
    (let ((started (or started-at (%conscious-conversation-work-now))))
      (setf (gethash work-id *conscious-conversation-work-timings*)
            (obj "started_at" started
               "ready_at" started
               "quantum_count" 0 "cognitive_quantum" 0
               "tool_execution" 0 "scheduler_handoff" 0
               "boundary_settlement" 0)))))

(defun %conscious-conversation-work-timing-quantum-start (work-id)
  (let ((now (%conscious-conversation-work-now)))
    (bt:with-lock-held (*conscious-conversation-work-lock*)
      (let ((timing (gethash work-id *conscious-conversation-work-timings*)))
        (when (hash-table-p timing)
          (let ((ready (gethash "ready_at" timing)))
            (when (integerp ready)
              (incf (gethash "scheduler_handoff" timing 0)
                    (%conscious-conversation-work-elapsed-ms ready now))))
          (remhash "ready_at" timing))))
    now))

(defun %conscious-conversation-work-timing-add-quantum
    (work-id started result)
  (let ((ended (%conscious-conversation-work-now))
        (quantum-timing (and (hash-table-p result)
                             (gethash "timing_ms" result))))
    (bt:with-lock-held (*conscious-conversation-work-lock*)
      (let ((timing (gethash work-id *conscious-conversation-work-timings*)))
        (when (hash-table-p timing)
          (incf (gethash "quantum_count" timing 0))
          (incf (gethash "cognitive_quantum" timing 0)
                (%conscious-conversation-work-elapsed-ms started ended))
          (when (hash-table-p quantum-timing)
            (dolist (key *conscious-conversation-quantum-timing-keys*)
              (incf (gethash key timing 0)
                    (gethash key quantum-timing 0)))))))
    ended))

(defun %conscious-conversation-work-timing-ready (work-id)
  (bt:with-lock-held (*conscious-conversation-work-lock*)
    (let ((timing (gethash work-id *conscious-conversation-work-timings*)))
      (when (hash-table-p timing)
        (setf (gethash "ready_at" timing)
              (%conscious-conversation-work-now))))))

(defun %conscious-conversation-work-timing-tool-start (work-id)
  (bt:with-lock-held (*conscious-conversation-work-lock*)
    (let ((timing (gethash work-id *conscious-conversation-work-timings*)))
      (when (hash-table-p timing)
        (setf (gethash "tool_started_at" timing)
              (%conscious-conversation-work-now))))))

(defun %conscious-conversation-work-timing-tool-stop (work-id)
  (let ((now (%conscious-conversation-work-now)))
    (bt:with-lock-held (*conscious-conversation-work-lock*)
      (let ((timing (gethash work-id *conscious-conversation-work-timings*)))
        (when (hash-table-p timing)
          (let ((started (gethash "tool_started_at" timing)))
            (when (integerp started)
              (incf (gethash "tool_execution" timing 0)
                    (%conscious-conversation-work-elapsed-ms started now))))
          (remhash "tool_started_at" timing)
          (setf (gethash "ready_at" timing) now))))))

(defun %conscious-conversation-work-timing-finalize (work-id result)
  (bt:with-lock-held (*conscious-conversation-work-lock*)
    (let ((timing (gethash work-id *conscious-conversation-work-timings*)))
      (when (and (hash-table-p timing) (hash-table-p result))
        (let* ((total (%conscious-conversation-work-elapsed-ms
                       (gethash "started_at" timing)))
               (closed (obj "schema_version" 1 "total" total)))
          (dolist (key (append '("quantum_count" "cognitive_quantum"
                                 "tool_execution" "scheduler_handoff"
                                 "boundary_settlement")
                               *conscious-conversation-quantum-timing-keys*))
            (setf (gethash key closed) (gethash key timing 0)))
          (setf (gethash "timing_ms" result) closed
                (gethash "%work_started_at" result)
                (gethash "started_at" timing))))
      (remhash work-id *conscious-conversation-work-timings*)))
  (bt:with-lock-held (*conscious-conversation-work-lock*)
    (remhash work-id *conscious-conversation-work-embedding-caches*))
  result)

(defun %conscious-conversation-work-embedding-cache (work-id)
  (bt:with-lock-held (*conscious-conversation-work-lock*)
    (or (gethash work-id *conscious-conversation-work-embedding-caches*)
        (setf (gethash work-id *conscious-conversation-work-embedding-caches*)
              (make-hash-table :test #'equal)))))

(defun %conscious-conversation-work-transition (work-id transition reason-code)
  "Request a transition against one exact observed durable work revision."
  (let* ((projection (%conscious-work-runtime-project-shared))
         (work (gethash work-id (gethash "items" projection))))
    (unless (hash-table-p work)
      (error "Conversation transition target is absent"))
    (conscious-work-runtime-transition
     work-id transition :reason-code reason-code
     :expected-state (gethash "state" work)
     :expected-revision (gethash "projection_revision" work))))
(defvar *conscious-conversation-work-quanta* 0)
(defvar *conscious-conversation-work-completions* 0)
(defvar *conscious-conversation-work-failures* 0)

(defun %conscious-conversation-work-notify (status phase)
  (let ((observer *conscious-conversation-work-progress-fn*))
    (when (functionp observer)
      (handler-case (funcall observer status phase 0)
        (error () nil)))))

(defun %conscious-conversation-work-cognition-observer (status report)
  (declare (ignore report))
  (when (string= status "selected")
    (%conscious-conversation-work-notify "started" "cognitive_quantum")))

(defun %conscious-conversation-work-condition-summary (condition)
  "Return one bounded private diagnostic line for a synchronous waiter."
  (let* ((raw (format nil "~a" condition))
         (text
           (map 'string
                (lambda (character)
                  (if (member character '(#\Newline #\Return #\Tab))
                      #\Space character))
                raw)))
    (if (> (length text) 240) (subseq text 0 240) text)))

(defun %conscious-conversation-work-events ()
  (let* ((events
           (%conscious-work-runtime-events
            (append *conscious-work-runtime-event-types*
                    '("conscious-tool-operation-claimed"
                      "conscious-tool-operation-failed"))))
         (root-ids nil))
    (dolist (event events)
      (when (string= "conscious-work-opened" (gethash "type" event ""))
        (dolist (stimulus-id
                 (%conscious-work-items
                  (gethash "stimulus_ids" (gethash "payload" event))))
          (when (and (stringp stimulus-id) (> (length stimulus-id) 9)
                     (string= "stimulus:" stimulus-id :end2 9))
            (pushnew (parse-integer stimulus-id :start 9 :junk-allowed nil)
                     root-ids)))))
    (dolist (event-id root-ids)
      (let ((root
              (%conscious-work-runtime-root-event
               (format nil "stimulus:~d" event-id)
               *conscious-conversation-work-agent-id*)))
        (when root (push root events))))
    (sort (remove-duplicates events :key (lambda (event) (gethash "id" event))
                             :test #'equal)
          #'< :key (lambda (event) (gethash "id" event)))))

(defun %conscious-conversation-work-store (work-id result &optional terminal-p)
  (when terminal-p
    (%conscious-conversation-work-timing-finalize work-id result))
  (bt:with-lock-held (*conscious-conversation-work-lock*)
    (setf (gethash work-id *conscious-conversation-work-results*)
          (cons (not (null terminal-p)) result))
    (bt:condition-notify *conscious-conversation-work-condition*))
  result)

(defun %conscious-conversation-work-await-boundary-settlement (work-id result)
  "Close timing only after the executor has verified and selected past WORK-ID."
  (let ((deadline (+ (%conscious-conversation-work-now)
                     (* 5 internal-time-units-per-second))))
    (loop
      for report = (conscious-work-executor-report)
      while (and (gethash "in_progress" report)
                 (equal work-id (gethash "last_work_id" report))
                 (< (%conscious-conversation-work-now) deadline))
      do (sleep 0.001d0)))
  (let* ((started (gethash "%work_started_at" result))
         (timing (gethash "timing_ms" result)))
    (when (and (integerp started) (hash-table-p timing))
      (let* ((old-total (gethash "total" timing 0))
             (new-total (%conscious-conversation-work-elapsed-ms started)))
        (setf (gethash "boundary_settlement" timing)
              (max 0 (- new-total old-total))
              (gethash "total" timing) (max old-total new-total))))
    (remhash "%work_started_at" result))
  result)

(defun %conscious-conversation-work-root (events work)
  (let ((matches nil))
    (dolist (stimulus-id
             (%conscious-work-items (gethash "stimulus_ids" work)))
      (dolist (event events)
        (when (and (hash-table-p event)
                   (equal *conscious-conversation-work-agent-id*
                          (gethash "agent_id" event))
                   (string= "user-message" (gethash "type" event ""))
                   (string= stimulus-id
                            (format nil "stimulus:~a" (gethash "id" event))))
          (push event matches))))
    (unless (= 1 (length matches))
      (error "Conversation work requires exactly one durable user root"))
    (first matches)))

(defun %conscious-conversation-work-quantum (work selection)
  (declare (ignore selection))
  (let* ((work-id (gethash "work_id" work))
         (quantum-started
           (%conscious-conversation-work-timing-quantum-start work-id))
         (root (%conscious-conversation-work-root
                (%conscious-conversation-work-events) work))
         (root-id (gethash "id" root))
         (payload (gethash "payload" root))
         (metadata (and (hash-table-p payload) (gethash "metadata" payload)))
         (prompt (and (hash-table-p payload) (gethash "text" payload)))
         (channel (or (and (hash-table-p payload)
                           (gethash "channel" payload))
                      "terminal"))
         (interaction-id
           (and (hash-table-p metadata)
                (let ((value (gethash "interaction_id" metadata)))
                  (and (stringp value) value))))
         (result nil))
    (handler-case
        (let ((*embedding-turn-cache*
                (%conscious-conversation-work-embedding-cache work-id)))
          (setf result
                (funcall *conscious-conversation-work-turn-fn*
                         prompt :admitted-event-id root-id :channel channel
                         :interaction-id interaction-id :work-id work-id)))
      (error (condition)
        (let* ((lineage (conscious-work-runtime-events-for-work work-id))
               (request-appended-p
                 (not (null
                       (find "model-request" lineage
                             :key (lambda (event)
                                    (gethash "type" event ""))
                             :test #'string=))))
               (fallback-transition
                 (if request-appended-p "outcome-unknown" "failed"))
               (fallback-reason
                 (if request-appended-p
                     "conversation-quantum-outcome-unknown"
                     "conversation-quantum-local-failure"))
               (projection (%conscious-work-runtime-project-shared))
               (projected
                 (gethash work-id (gethash "items" projection)))
               (state (and projected (gethash "state" projected ""))))
          ;; OPEN-CAPTURED may have durably failed its pulse before signalling
          ;; the context error. That pulse is already the terminal work
          ;; boundary; attempting a second transition would itself fail and
          ;; strand the synchronous waiter until timeout.
          (unless (member state '("failed" "outcome-unknown") :test #'string=)
            (%conscious-conversation-work-transition
             work-id fallback-transition fallback-reason)
            (setf state fallback-transition))
          (incf *conscious-conversation-work-failures*)
          (setf result
                (obj "schema_version" 1 "status" state
                     "error_code"
                     (if (string= state fallback-transition)
                         fallback-reason
                         "conversation-quantum-failed")
                     "reason"
                     (%conscious-conversation-work-condition-summary condition)
                     "content" :null "work_id" work-id)))))
    (%conscious-conversation-work-timing-add-quantum
     work-id quantum-started result)
    (incf *conscious-conversation-work-quanta*)
    (let ((status (gethash "status" result "")))
      (cond
        ((string= status "tool-proposed")
         (%conscious-conversation-work-store work-id result)
         (%conscious-conversation-work-notify "started" "tool_execution")
         (%conscious-conversation-work-timing-tool-start work-id)
         (conscious-operation-executor-wake)
         (obj "schema_version" 1 "status" "committed-boundary"
              "work_id" work-id "boundary_kind" "pulse-committed"))
        ((string= status "continuation-requested")
         (%conscious-conversation-work-store work-id result)
         (%conscious-conversation-work-notify "started" "continuation")
         (%conscious-conversation-work-timing-ready work-id)
         (obj "schema_version" 1 "status" "committed-boundary"
              "work_id" work-id "boundary_kind" "pulse-committed"))
        ((member status '("replied" "no-reply") :test #'string=)
         (let* ((projected (%conscious-work-runtime-project-shared))
                (current
                  (gethash work-id (gethash "items" projected)))
                (state (and current (gethash "state" current))))
           (when (string= state "completing")
              (%conscious-conversation-work-transition
               work-id "completed"
               (if (string= status "replied")
                   "publication-committed" "abstention-committed"))))
         (incf *conscious-conversation-work-completions*)
         (%conscious-conversation-work-notify "completed" "cognitive_work")
         (%conscious-conversation-work-store work-id result t)
         (obj "schema_version" 1 "status" "committed-boundary"
              "work_id" work-id "boundary_kind" "pulse-committed"))
        ((string= status "withheld")
          (%conscious-conversation-work-transition
           work-id "failed" "publication-withheld")
         (incf *conscious-conversation-work-failures*)
         (%conscious-conversation-work-store work-id result t)
         (obj "schema_version" 1 "status" "committed-boundary"
              "work_id" work-id "boundary_kind" "pulse-committed"))
        ((member status '("provider-call-failed" "provider-response-invalid"
                          "provider-response-truncated") :test #'string=)
         (incf *conscious-conversation-work-failures*)
         (%conscious-conversation-work-store work-id result t)
         (obj "schema_version" 1 "status" "committed-boundary"
              "work_id" work-id "boundary_kind" "failed"))
        ((member status '("failed" "outcome-unknown") :test #'string=)
         (%conscious-conversation-work-store work-id result t)
         (obj "schema_version" 1 "status" "committed-boundary"
              "work_id" work-id "boundary_kind" status))
        (t
          (%conscious-conversation-work-transition
           work-id "failed" "conversation-result-invalid")
         (incf *conscious-conversation-work-failures*)
         (%conscious-conversation-work-store
          work-id (obj "schema_version" 1 "status" "failed"
                       "content" :null) t)
         (obj "schema_version" 1 "status" "committed-boundary"
              "work_id" work-id "boundary_kind" "failed"))))))

(defun %conscious-conversation-work-operation-observer (status receipt)
  (when (string= status "advanced")
    (%conscious-conversation-work-timing-tool-stop
     (gethash "work_id" receipt))
    (%conscious-conversation-work-notify "completed" "tool_execution"))
  (when (member status '("failed" "outcome-unknown") :test #'string=)
    (%conscious-conversation-work-notify "failed" "tool_execution")
    (incf *conscious-conversation-work-failures*)
    (%conscious-conversation-work-store
     (gethash "work_id" receipt)
     (obj "schema_version" 1 "status" status "content" :null
          "proposal_id" (gethash "proposal_id" receipt))
     t)))

(defun conscious-conversation-work-configure
    (&key agent-id profile runtime-plan turn-fn progress-fn)
  (unless (and (stringp agent-id) (plusp (length agent-id))
               (functionp turn-fn))
    (error "Conversation work configuration is incomplete"))
  (unless (or (null progress-fn) (functionp progress-fn))
    (error "Conversation work progress observer must be a function or NIL"))
  (unless (and (hash-table-p runtime-plan)
               (fboundp 'conscious-runtime-plan-hash))
    (error "Conversation work requires a sealed runtime plan"))
  (let* ((validated-profile (conscious-work-profile-validate profile))
         (plan-profile
           (gethash "work_profile" (gethash "canonical_plan" runtime-plan))))
    (conscious-runtime-plan-hash runtime-plan)
    (unless (string= (%conscious-work-canonical-json validated-profile)
                     (%conscious-work-canonical-json plan-profile))
      (error "Conversation work profile does not match its sealed plan")))
  (setf *conscious-conversation-work-agent-id* agent-id
         *conscious-conversation-work-profile*
         (conscious-work-profile-validate profile)
        *conscious-conversation-work-runtime-plan* runtime-plan
        *conscious-conversation-work-turn-fn* turn-fn
        *conscious-conversation-work-progress-fn* progress-fn)
  (conscious-work-executor-configure
   :agent-id agent-id
   :prepare-fn (lambda () (obj "schema_version" 1 "status" "idle"))
   :projection-fn #'%conscious-work-runtime-project-shared
   :quantum-fn #'%conscious-conversation-work-quantum
   :observer-fn #'%conscious-conversation-work-cognition-observer)
  (conscious-operation-executor-configure
   :agent-id agent-id
   :events-fn #'%conscious-conversation-work-events
   :projection-fn #'%conscious-work-runtime-project-shared
   :operation-fn #'conscious-tool-operation-run
   :transition-fn #'conscious-work-runtime-transition
   :cognition-wake-fn #'conscious-work-executor-wake
   :observer-fn #'%conscious-conversation-work-operation-observer)
  (conscious-conversation-work-report))

(defun conscious-conversation-work-start ()
  (unless (and *conscious-conversation-work-agent-id*
               *conscious-conversation-work-profile*
               (functionp *conscious-conversation-work-turn-fn*))
    (error "Conversation work loop is not configured"))
  (conscious-operation-executor-start)
  (conscious-work-executor-start)
  ;; Recovery wakes are content-free. Durable projections decide whether
  ;; either worker actually has something eligible to do.
  (conscious-operation-executor-wake)
  (conscious-work-executor-wake :reason "startup-recovery")
  (conscious-conversation-work-report))

(defun conscious-conversation-work-stop ()
  (ignore-errors (conscious-work-executor-stop))
  (ignore-errors (conscious-operation-executor-stop))
  (conscious-conversation-work-report))

(defun %conscious-conversation-work-recover-terminal
    (work-id admitted-event-id)
  (let* ((projection (%conscious-work-runtime-project-shared))
         (work (gethash work-id (gethash "items" projection)))
         (state (and work (gethash "state" work ""))))
    (cond
      ((string= state "completed")
       (let* ((recent-events
                (if (fboundp 'event-recent-conversation-events)
                    (funcall 'event-recent-conversation-events nil 128)
                    (funcall 'replay-events
                             :limit 128
                             :types '("user-message" "agent-message"))))
              (messages
                (remove-if-not
                 (lambda (event)
                   (and (hash-table-p event)
                        (equal *conscious-conversation-work-agent-id*
                               (gethash "agent_id" event))
                        (string= "agent-message" (gethash "type" event ""))
                        (equal admitted-event-id
                               (gethash "caused_by" event))))
                 recent-events)))
         (cond
           ((= 1 (length messages))
            (let* ((event (first messages))
                   (payload (gethash "payload" event)))
              (obj "schema_version" 1 "status" "replied"
                   "content" (gethash "text" payload)
                   "user_event_id" admitted-event-id
                   "agent_event_id" (gethash "id" event)
                   "recovered" t)))
           ((and (null messages)
                 (string= "abstain"
                          (gethash "last_transition_kind" work "")))
            (obj "schema_version" 1 "status" "no-reply"
                 "reason" "abstain" "content" :null "recovered" t))
           (t (error "Completed conversation work has ambiguous publication")))))
      ((string= state "suspended")
       (obj "schema_version" 1 "status" "no-reply"
            "reason" (gethash "waiting_reason" work "suspended")
            "content" :null "recovered" t))
      ((member state '("failed" "outcome-unknown") :test #'string=)
       (obj "schema_version" 1 "status" state "content" :null
            "recovered" t))
      ((string= state "completing")
       ;; A committed disposition without its durable public/terminal receipt
       ;; must never cause the model to author a second candidate on restart.
        (%conscious-conversation-work-transition
         work-id "outcome-unknown"
         "committed-disposition-outcome-unknown")
       (obj "schema_version" 1 "status" "outcome-unknown"
            "content" :null "recovered" t))
      (t nil))))

(defun conscious-conversation-work-run
    (prompt &key admitted-event-id channel interaction-id (timeout 240))
  "Open exact direct work, wake cognition, and await its public disposition."
  (declare (ignore prompt channel interaction-id))
  (unless (and (integerp admitted-event-id) (plusp admitted-event-id))
    (error "Conversation work requires an admitted event"))
  (let* ((run-started (%conscious-conversation-work-now))
         (opened
           (conscious-work-runtime-open-direct-event
            admitted-event-id *conscious-conversation-work-profile*
            :purpose "respond" :opened-at (get-universal-time)
            :runtime-plan-hash
            (conscious-runtime-plan-hash
             *conscious-conversation-work-runtime-plan*)))
         (work-id (gethash "work_id" opened))
         (deadline (+ (get-internal-real-time)
                      (* timeout internal-time-units-per-second))))
    (let ((recovered
            (%conscious-conversation-work-recover-terminal
             work-id admitted-event-id)))
      (when recovered
        (return-from conscious-conversation-work-run recovered)))
    (%conscious-conversation-work-timing-start work-id run-started)
    (conscious-work-executor-wake :reason "durable-stimulus")
    (let ((result
            (bt:with-lock-held (*conscious-conversation-work-lock*)
              (loop
                (let ((entry
                        (gethash work-id
                                 *conscious-conversation-work-results*)))
                  (when (and entry (car entry))
                    (remhash work-id *conscious-conversation-work-results*)
                    (return (cdr entry))))
                (let ((remaining
                        (/ (- deadline (get-internal-real-time))
                           (coerce internal-time-units-per-second
                                   'double-float))))
                  (when (<= remaining 0)
                    (let ((inspection
                            (conscious-conversation-work-inspect work-id)))
                      (return
                        (obj "schema_version" 1 "status" "detached"
                             "content" :null "work_id" work-id
                             "observed_state" (gethash "state" inspection)
                             "projection_revision"
                             (gethash "projection_revision" inspection)
                             "can_rejoin" t))))
                  (bt:condition-wait *conscious-conversation-work-condition*
                                     *conscious-conversation-work-lock*
                                     :timeout remaining))))))
      (if (hash-table-p (gethash "timing_ms" result))
          (%conscious-conversation-work-await-boundary-settlement
           work-id result)
          result))))

(defun conscious-conversation-work-inspect (work-id)
  "Return a content-free durable snapshot suitable for a presentation adapter."
  (unless (and (stringp work-id) (plusp (length work-id)))
    (error "Conversation work inspection identity is invalid"))
  (let* ((projection (%conscious-work-runtime-project-shared))
         (work (gethash work-id (gethash "items" projection))))
    (unless (hash-table-p work)
      (error "Conversation work inspection target is absent"))
    (obj "schema_version" 1 "work_id" work-id
         "state" (gethash "state" work)
         "waiting_reason" (gethash "waiting_reason" work :null)
         "projection_revision" (gethash "projection_revision" work)
         "runtime_plan_hash" (gethash "runtime_plan_hash" work :null)
         "terminal"
         (not (null (member (gethash "state" work)
                            *conscious-work-terminal-states* :test #'string=)))
         "can_rejoin" t)))

(defun conscious-conversation-work-rejoin
    (admitted-event-id &key (timeout 240))
  "Rejoin the exact durable direct work rooted at ADMITTED-EVENT-ID."
  (conscious-conversation-work-run
   "" :admitted-event-id admitted-event-id
   :channel "rejoin" :interaction-id "presentation-rejoin"
   :timeout timeout))

(defun conscious-conversation-work-report ()
  (obj "schema_version" 1
       "runtime_revision" *conscious-conversation-work-revision*
       "agent_id" (or *conscious-conversation-work-agent-id* :null)
       "configured" (not (null *conscious-conversation-work-profile*))
       "quanta" *conscious-conversation-work-quanta*
       "completions" *conscious-conversation-work-completions*
       "failures" *conscious-conversation-work-failures*
       "cognitive_executor" (conscious-work-executor-report)
       "operation_executor" (conscious-operation-executor-report)))
