;;;; runtime.lisp -- adapters for the exclusive cognition registry.
;;;;
;;;; AUTO is intentionally opaque: resolve and invoke the final AUTO-TURN
;;;; once, without wrapping or reordering its characterized fourteen layers.
;;;; CONSCIOUS-STATE rebuilds and admits events and, in Q3, exposes one
;;;; synchronous deterministic pulse. Q4.5 adds an explicit manual, solicited,
;;;; loopback-provider route after this file loads. Q5 adds a durable manual
;;;; lifecycle boundary, typed producer reconciliation and stale-result handling; tools/effects,
;;;; autonomous work and unsolicited delivery remain absent.

(in-package :agent)

(export '(auto-cognition-runtime-report conscious-cognition-runtime-report
          conscious-cognition-runtime-pulse
          conscious-cognition-runtime-open-captured
          conscious-cognition-runtime-submit-captured
          conscious-cognition-runtime-recover
          conscious-cognition-runtime-lifecycle-transition
          conscious-cognition-runtime-reconcile-result))

(defparameter *auto-cognition-runtime-revision* "auto-opaque-q2-v1")
(defparameter *conscious-cognition-runtime-revision* "conscious-q5m-v1")
(defvar *conscious-runtime-last-events* '())
(defvar *conscious-runtime-last-submit-status* :never-submitted)
(defvar *conscious-runtime-projection-context* nil)

(defun %auto-tick-worker-live-p ()
  (and (boundp '*tick-thread*)
       (symbol-value '*tick-thread*)
       (bt:thread-alive-p (symbol-value '*tick-thread*))))

(defun %auto-drives-worker-live-p ()
  (and (boundp '*drives-thread*)
       (symbol-value '*drives-thread*)
       (bt:thread-alive-p (symbol-value '*drives-thread*))))

(defun %auto-cognition-recovery-probe ()
  ;; Resolve the final function, not a saved-original marker. The registry is
  ;; above the whole chain and the chain remains opaque by recorded decision.
  (fboundp 'auto-turn))

(defun %auto-cognition-submit
    (stimulus &key kind metadata wait-for-public-result)
  (declare (ignore kind metadata wait-for-public-result))
  ;; FUNCALL by symbol resolves the final owner at call time and preserves all
  ;; values. Capturing #'AUTO-TURN here would recreate Q0's stale-function bug.
  (funcall 'auto-turn stimulus))

(defun %auto-cognition-start () :running)

(defun auto-cognition-runtime-report ()
  (obj "state" (if (eq *cognition-runtime-state* :running)
                   "running" "ready")
       "adapter" "opaque-final-auto-turn"
       "final_owner_available" (if (fboundp 'auto-turn) t nil)
       "provider_route" "legacy-selected-only"
       "effect_route" "legacy-selected-only"))

(defun %conscious-runtime-events ()
  "Read authoritative events when the event-log adapter is available.

An absent event-log in a module-isolation test means an empty captured stream,
not permission to synthesize history. Errors are retained as degraded status
and the prior projection is left intact."
  (cond ((fboundp 'event-projection-events)
         (funcall 'event-projection-events))
        ((fboundp 'replay-events)
         (funcall 'replay-events))
        (t '())))

(defun %conscious-runtime-agent-id ()
  (and (boundp '*agent-id*) (symbol-value '*agent-id*)))

(defun %conscious-runtime-project (events)
  ;; Retain the exact immutable policy snapshot used for this projection.
  ;; Q1 intentionally keeps bounds in the context rather than duplicating
  ;; them into every state slot; runtime truth must therefore report this
  ;; pinned context, not whichever mutable defaults happen to be live later.
  (let* ((agent-id (%conscious-runtime-agent-id))
         (lifecycle-projection
           (conscious-lifecycle-project events :agent-id agent-id))
         (semantic-projection
           (conscious-lifecycle-semantic-project events :agent-id agent-id))
         (awaiting (conscious-lifecycle-awaiting lifecycle-projection))
         (context
           (make-projection-context
            :now (get-universal-time) :agent-id agent-id
            :runtime-revision *conscious-cognition-runtime-revision*
            :lifecycle awaiting)))
    (values (conscious-state-project events :context context)
            context lifecycle-projection semantic-projection)))

(defun %conscious-runtime-prefix-through-event (events through-event-id)
  "Return the authoritative prefix ending at THROUGH-EVENT-ID.

This is a durable-position boundary expressed by the event authority's
logical ID.  Native SQLite IDs are unique and strictly increasing; imported
legacy duplicates can only precede newly admitted coordinator events."
  (if (null through-event-id)
      events
      (let ((prefix nil)
            (found nil))
        (dolist (event events)
          (push event prefix)
          (when (and (hash-table-p event)
                     (equal through-event-id (gethash "id" event)))
            (setf found t)
            (return)))
        (unless found
          (error "Conscious context boundary event ~s is not durably readable"
                 through-event-id))
        (nreverse prefix))))

(defun %conscious-runtime-install-projections
    (events &key reconcile-sources-p through-event-id)
  "Rebuild and install lifecycle before the conscious-state consumer.
Producer reconciliation is explicit: admission and ordinary projection
refreshes never rescan the complete source stream."
  (when reconcile-sources-p
    (let* ((result
             (multiple-value-list
              (conscious-lifecycle-runtime-reconcile-producer-events
               (%conscious-runtime-agent-id)
               :actor-runtime-revision *conscious-cognition-runtime-revision*)))
           (source-report (first result)))
      (declare (ignore source-report))
      ;; Current reconciliation returns the exact snapshot it read while
      ;; holding its lock (or one post-append reread). One-value injected
      ;; ports retain the legacy reread fallback.
      (setf events (if (>= (length result) 2)
                       (second result)
                       (%conscious-runtime-events))))
    ;; Motivational candidates are reconciled only at the same explicit
    ;; consumer boundaries. They depend on the post-lifecycle snapshot (a
    ;; legal completion may satisfy a motive) and must become durable before
    ;; the inbox projection can acknowledge them.
    (let* ((result
             (multiple-value-list
              (conscious-motivation-runtime-reconcile
               (%conscious-runtime-agent-id)
               :actor-runtime-revision *conscious-cognition-runtime-revision*
               :origin-runtime-revision *conscious-cognition-runtime-revision*
               :now (get-universal-time)))))
      (setf events (if (>= (length result) 2)
                       (second result)
                       (%conscious-runtime-events)))))
  (let* ((authoritative-events events)
         (projection-events
           (%conscious-runtime-prefix-through-event
            authoritative-events through-event-id)))
    (multiple-value-bind (projection context lifecycle-projection semantic-projection)
        (%conscious-runtime-project projection-events)
    ;; EVENTS can expand a large JSONL ledger by an order of magnitude in the
    ;; Lisp heap. Projections are detached and rebuildable; retaining the full
    ;; replay adds no runtime truth and makes the next replay overlap it.
    (setf *conscious-runtime-last-events* '()
          *cognition-runtime-projection* projection
          *conscious-runtime-projection-context* context
          *conscious-lifecycle-runtime-projection* lifecycle-projection
          *conscious-lifecycle-semantic-runtime-projection* semantic-projection
          *conscious-lifecycle-runtime-agent-id* (%conscious-runtime-agent-id))
      ;; The bounded prefix drives cognition.  The full snapshot remains
      ;; available only to the append boundary, which needs the true ledger
      ;; tail for sequence allocation and durable-read verification.
      (values projection projection-events authoritative-events))))

(defun %conscious-cognition-restore ()
  (handler-case
      (progn
        ;; Recovery itself is an event-log transition. Run it before the
        ;; authoritative reread so the restored projection observes the
        ;; terminalized history, never the pre-recovery snapshot.
        (conscious-pulse-runtime-recover
         :agent-id (%conscious-runtime-agent-id)
         :runtime-revision *conscious-cognition-runtime-revision*)
        (let ((events (%conscious-runtime-events)))
          (prog1 (%conscious-runtime-install-projections
                  events :reconcile-sources-p t)
            (setf *conscious-runtime-last-submit-status* :restored))))
    (error (condition)
      (setf *conscious-runtime-last-submit-status*
            (list :restore-failed (format nil "~a" condition)))
      (error condition))))

(defun %conscious-runtime-event-type (kind)
  (case kind
    (:user-message "user-message")
    (:scheduled-check-in "schedule-fired")
    (:drive-threshold "drive-near-threshold")
    (otherwise nil)))

(defun %conscious-cognition-submit
    (stimulus &key kind metadata wait-for-public-result)
  "Admit one input event and refresh the Q1 projection, without a pulse.

WAIT-FOR-PUBLIC-RESULT is deliberately unsatisfied at this admission boundary.
NIL/:ACCEPTED/event-id tells callers that the input is durable but no
publication outcome exists; callers must not manufacture a placeholder reply."
  (declare (ignore wait-for-public-result))
  (let ((event-type (%conscious-runtime-event-type kind)))
    (unless event-type
      (error "Conscious runtime Q2 does not admit stimulus kind ~s" kind))
    (unless (fboundp 'log-event)
      (error "Conscious runtime cannot admit input without the event log"))
    (let ((event-id
            (funcall 'log-event event-type
                     (obj "text" (if (stringp stimulus)
                                     stimulus (format nil "~a" stimulus))
                          "channel"
                          (if (and (boundp '*public-inbound-channel*)
                                   (stringp (symbol-value
                                             '*public-inbound-channel*)))
                              (symbol-value '*public-inbound-channel*)
                              "internal")
                          "metadata" (or metadata :null)
                          "origin_runtime_revision"
                          *conscious-cognition-runtime-revision*))))
      (unless event-id
        (error "Conscious runtime event admission failed"))
      ;; Re-read rather than constructing a lookalike event. The append
      ;; boundary stamps event id, time, partition and causation; a projection
      ;; that validates values it manufactured itself is not validation.
      (let* ((events (%conscious-runtime-events))
             (stored
               (find-if
                (lambda (event)
                  (and (hash-table-p event)
                       (equal event-id (gethash "id" event))
                       (string= event-type (gethash "type" event ""))
                       (equal (%conscious-runtime-agent-id)
                              (gethash "agent_id" event))))
                events :from-end t)))
        (unless stored
          (error "Conscious runtime input event ~s was not durably readable"
                 event-id))
        (%conscious-runtime-install-projections events)
        (setf *conscious-runtime-last-submit-status* :accepted)
        (values nil :accepted event-id)))))

(defun %conscious-cognition-verify ()
  (and (hash-table-p *cognition-runtime-projection*)
       (eql *conscious-state-schema-version*
            (gethash "schema_version" *cognition-runtime-projection*))))

(defun %conscious-cognition-recovery-probe ()
  (and (fboundp 'conscious-state-project)
       (hash-table-p *cognition-runtime-projection*)))

(defun %conscious-cognition-start () :idle)

(defun conscious-cognition-runtime-pulse
    (&key purpose now clock-identity budget cancelled-p (finished-at now))
  "Run one explicit Q3 deterministic pulse for the selected conscious runtime."
  (unless (cognition-runtime-selected-p :conscious-state)
    (error "Manual conscious pulse requires the selected :conscious-state runtime"))
  ;; A pulse is an execution boundary. Reconcile durable producer history
  ;; immediately before assembling the state that the pulse will consume.
  (%conscious-runtime-install-projections
   (%conscious-runtime-events) :reconcile-sources-p t)
  (unless (and (hash-table-p *cognition-runtime-projection*)
               (projection-context-p *conscious-runtime-projection-context*))
    (error "Manual conscious pulse requires a restored projection and context"))
  (multiple-value-bind (plan projected)
      (conscious-pulse-runtime-run
       *cognition-runtime-projection*
       :projection-context *conscious-runtime-projection-context*
       :agent-id (%conscious-runtime-agent-id)
       :runtime-revision *conscious-cognition-runtime-revision*
       :purpose purpose :now now :clock-identity clock-identity :budget budget
       :cancelled-p cancelled-p :finished-at finished-at)
    (declare (ignore projected))
    ;; Only a committed terminal can advance the installed live projection.
    (when (string= "completed" (gethash "status" plan ""))
      (%conscious-runtime-install-projections (%conscious-runtime-events)))
    (values plan *cognition-runtime-projection*)))

(defun conscious-cognition-runtime-open-captured
    (&key purpose now clock-identity assembly-spec assembly-spec-fn
          assembly-spec-events-fn through-event-id
          (model-call-budget 0) work-id parent-pulse-id)
  "Open a selected Q4 captured deliberation and return its private request."
  (unless (cognition-runtime-selected-p :conscious-state)
    (error "Captured deliberation requires the selected :conscious-state runtime"))
  ;; Captured deliberation is another consumer boundary: source work becomes
  ;; visible here, rather than making every unrelated stimulus append rescan it.
  (multiple-value-bind (installed events authoritative-events)
      (%conscious-runtime-install-projections
       nil :reconcile-sources-p t :through-event-id through-event-id)
    (declare (ignore installed))
    (unless (and (hash-table-p *cognition-runtime-projection*)
                 (projection-context-p *conscious-runtime-projection-context*))
      (error "Captured deliberation requires a restored projection and context"))
    (unless (= 1 (count-if #'identity
                           (list assembly-spec assembly-spec-fn
                                 assembly-spec-events-fn)))
      (error "Captured deliberation accepts exactly one assembly-spec source"))
    (let ((resolved-spec
            (cond (assembly-spec-events-fn
                   (funcall assembly-spec-events-fn events))
                  (assembly-spec-fn (funcall assembly-spec-fn))
                  (t assembly-spec))))
      (conscious-pulse-runtime-open-captured
       *cognition-runtime-projection*
       :projection-context *conscious-runtime-projection-context*
       :agent-id (%conscious-runtime-agent-id)
       :runtime-revision *conscious-cognition-runtime-revision*
       :purpose purpose :now now :clock-identity clock-identity
       :assembly-spec resolved-spec :model-call-budget model-call-budget
       :work-id work-id :parent-pulse-id parent-pulse-id
       :event-snapshot authoritative-events))))

(defun conscious-cognition-runtime-submit-captured (captured &key (model-calls 0))
  "Validate and commit one captured response without executing its proposals."
  (unless (cognition-runtime-selected-p :conscious-state)
    (error "Captured deliberation requires the selected :conscious-state runtime"))
  (multiple-value-bind (plan projected)
      (conscious-pulse-runtime-submit-captured captured :model-calls model-calls)
    ;; The pulse adapter's one post-terminal reread already produced the exact
    ;; committed projection. Installing it directly avoids a duplicate fourth
    ;; ledger scan while retaining no expanded event list between phases.
    (setf *cognition-runtime-projection* projected
          *conscious-runtime-last-events* '())
    (values plan *cognition-runtime-projection*)))

(defun conscious-cognition-runtime-recover ()
  "Recover Q3 orphaned opens, then refresh the selected live projection."
  (unless (cognition-runtime-selected-p :conscious-state)
    (error "Conscious recovery requires the selected :conscious-state runtime"))
  (let ((recovered
          (conscious-pulse-runtime-recover
           :agent-id (%conscious-runtime-agent-id)
           :runtime-revision *conscious-cognition-runtime-revision*)))
    (when (plusp recovered)
      (%conscious-cognition-restore))
    recovered))

(defun conscious-cognition-runtime-lifecycle-transition
    (lifecycle-id transition &key request-id lifecycle-kind
                                  (source-event-id :null) (checkpoint-ref :null)
                                  (reason-code "explicit-transition") now)
  "Append one explicit Q5 lifecycle transition for the selected runtime."
  (unless (cognition-runtime-selected-p :conscious-state)
    (error "Lifecycle transition requires the selected :conscious-state runtime"))
  (let* ((existing
           (conscious-lifecycle-current
            *conscious-lifecycle-runtime-projection* lifecycle-id))
         (effective-kind
           (or (and existing (gethash "lifecycle_kind" existing)) lifecycle-kind))
         (origin
           (or (and existing (gethash "origin_runtime_revision" existing))
               *conscious-cognition-runtime-revision*))
         (event-id
           (conscious-lifecycle-runtime-transition
            lifecycle-id transition :request-id request-id
            :lifecycle-kind effective-kind :origin-runtime-revision origin
            :actor-runtime-revision *conscious-cognition-runtime-revision*
            :source-event-id source-event-id :checkpoint-ref checkpoint-ref
            :reason-code reason-code :now now
            :agent-id (%conscious-runtime-agent-id))))
    (%conscious-runtime-install-projections (%conscious-runtime-events))
    (values event-id *cognition-runtime-projection*)))

(defun conscious-cognition-runtime-reconcile-result
    (lifecycle-id source-event-id &key request-id now)
  "Reconcile one stored result through Q5 without executing or publishing it."
  (unless (cognition-runtime-selected-p :conscious-state)
    (error "Result reconciliation requires the selected :conscious-state runtime"))
  (let ((event-id
          (conscious-lifecycle-runtime-reconcile-result
           lifecycle-id source-event-id :request-id request-id :now now
           :agent-id (%conscious-runtime-agent-id)
           :current-runtime-revision *conscious-cognition-runtime-revision*)))
    (%conscious-runtime-install-projections (%conscious-runtime-events))
    (values event-id *cognition-runtime-projection*)))

(defun %conscious-cognition-stop ()
  ;; No worker exists in Q2. Clear only the rebuildable live projection; the
  ;; event log remains untouched and can reconstruct it on the next restore.
  (setf *cognition-runtime-projection* nil
        *conscious-runtime-projection-context* nil
        *conscious-lifecycle-runtime-projection* nil)
  :stopped)

(defun conscious-cognition-runtime-report ()
  (let* ((state *cognition-runtime-projection*)
         (focus (and (hash-table-p state) (gethash "focus" state)))
         (flags-slot (and (hash-table-p state) (gethash "flags" state)))
         (flags (and (hash-table-p flags-slot)
                     (gethash "value" flags-slot)))
         (degraded (and (hash-table-p flags) (gethash "degraded" flags)))
         (degraded-reason (and (hash-table-p flags)
                               (gethash "degraded_reason" flags)))
         (bounds (and (hash-table-p *conscious-runtime-projection-context*)
                      (gethash "bounds"
                               *conscious-runtime-projection-context*)))
         (pulse-report (conscious-pulse-runtime-report))
         (lifecycle-report (conscious-lifecycle-runtime-report))
         (source-report (conscious-lifecycle-source-report)))
    (obj
     "state" (cond ((not (hash-table-p state)) "unavailable")
                   ((and (hash-table-p focus)
                         (string= "idle" (gethash "lifecycle" focus "")))
                    "idle")
                   (t "projected"))
     "projection_schema_version"
     (if (hash-table-p state) (gethash "schema_version" state) :null)
     "state_revision"
     (if (hash-table-p state) (gethash "state_revision" state) :null)
     "observation_revision"
     (if (hash-table-p state) (gethash "observation_revision" state) :null)
     "queue_bounds" (if (hash-table-p bounds) bounds :null)
     "queue_bound_status"
     (cond (degraded (if (stringp degraded-reason)
                         degraded-reason "degraded"))
           ((hash-table-p state) "within-bounds")
           (t :null))
     "degraded" (if degraded t nil)
     "degraded_reason" (or degraded-reason :null)
     "last_submit_status"
     (if (keywordp *conscious-runtime-last-submit-status*)
         (string-downcase
          (symbol-name *conscious-runtime-last-submit-status*))
         "failed")
     "pulse_worker" "manual-q5-lifecycle"
     "last_committed_pulse"
     (if (hash-table-p pulse-report)
         (gethash "last_pulse" pulse-report :null) :null)
     "pulse" pulse-report
     "lifecycle" lifecycle-report
     "lifecycle_sources" source-report
     "motivation_candidates" (conscious-motivation-runtime-report)
     "provider_route"
     (if (fboundp 'conscious-conversation-turn)
         "manual-loopback-solicited" nil)
     "effect_route" nil
     "publication_route"
     (if (fboundp 'conscious-conversation-turn)
         "validated-durable-solicited-reply" nil))))

(define-cognition-runtime :auto
  :revision *auto-cognition-runtime-revision*
  :owner "final-auto-turn"
  :entry %auto-cognition-submit
  :restore nil
  :verify nil
  :start %auto-cognition-start
  :stop nil
  :report auto-cognition-runtime-report
  :recovery-probe %auto-cognition-recovery-probe
  :required-capabilities (auto-turn)
  :owned-workers (("tick-loop" %auto-tick-worker-live-p)
                  ("drives" %auto-drives-worker-live-p)))

(define-cognition-runtime :conscious-state
  :revision *conscious-cognition-runtime-revision*
  :owner "conscious-pulse"
  :entry %conscious-cognition-submit
  :restore %conscious-cognition-restore
  :verify %conscious-cognition-verify
  :start %conscious-cognition-start
  :stop %conscious-cognition-stop
  :report conscious-cognition-runtime-report
  :recovery-probe %conscious-cognition-recovery-probe
  :required-capabilities (log-event replay-events make-projection-context
                          conscious-state-project projection-context-p
                          conscious-pulse-runtime-run
                          conscious-pulse-runtime-open-captured
                          conscious-pulse-runtime-submit-captured
                          conscious-pulse-runtime-recover
                          conscious-pulse-runtime-report
                          conscious-lifecycle-project
                          conscious-lifecycle-current
                          conscious-lifecycle-awaiting
                          conscious-lifecycle-runtime-transition
                          conscious-lifecycle-runtime-reconcile-result
                          conscious-lifecycle-runtime-report
                          conscious-lifecycle-runtime-reconcile-producer-events
                          conscious-lifecycle-source-report
                          conscious-motivation-runtime-reconcile
                          conscious-motivation-runtime-report)
  :owned-workers ())
