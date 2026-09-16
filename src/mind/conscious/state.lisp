;;;; state.lisp -- the bounded conscious-state projection.
;;;;
;;;; Workstream Q, slice Q1, final piece. Pure fold over logged events:
;;;; no model, no tools, no publication, no I/O, no clock read that is not
;;;; passed in, registers nothing.
;;;;
;;;; WHAT THIS IS AND IS NOT
;;;;
;;;; This is "what is active now" -- a bounded synthesis, rebuildable from the
;;;; event log at any time. It is NOT the durable mind (the event log is), and
;;;; it is NOT the LLM context (that is a purpose-specific rendering of this,
;;;; produced later under audience and budget rules).
;;;;
;;;; Conflating those three is the failure this whole architecture exists to
;;;; avoid. The predecessor conflated the second and third: ten separate
;;;; writers spliced prose into one system-prompt string, so "what the agent
;;;; is thinking about" and "what we happened to put in the prompt" were the
;;;; same object and neither could be inspected. Here the state is structured
;;;; data with provenance, and rendering is a separate step that reads it.
;;;;
;;;; REFERENCES, NOT CONTENT
;;;;
;;;; Slots carry identifiers and never payloads. A raw message, tool body, or
;;;; memory row is retrieved when a pulse is rendered, under that pulse's
;;;; purpose and audience budget -- not hoisted into state where it would
;;;; escape those budgets and quietly become unbounded.
;;;;
;;;; EVERY SLOT EXPLAINS ITSELF
;;;;
;;;; Each slot carries provenance, freshness, lifecycle and a reason for
;;;; inclusion. Nothing is in the conscious state without a readable answer to
;;;; "why is this here, where did it come from, and how stale is it". That is
;;;; the property that makes the state reviewable by the operator and by the
;;;; agent itself, and it is enforced by construction: %SLOT requires all four.

(in-package :agent)

(export '(conscious-state-project conscious-state-report
          *conscious-state-schema-version*))

(defparameter *conscious-state-schema-version* 1)

;;; --- slot construction ---------------------------------------------------

(defun %slot (value &key provenance freshness lifecycle reason)
  "Wrap VALUE with the four facts every slot must carry. All four are
required keywords rather than optional: a slot that cannot say why it is
present should not be constructible."
  (unless (and provenance freshness lifecycle reason)
    (error "conscious-state slot requires provenance, freshness, lifecycle and reason"))
  (obj "value" value
       "provenance" provenance
       "freshness" freshness
       "lifecycle" lifecycle
       "reason" reason))

(defun %freshness (observed-at now)
  "Age in the caller's time unit, plus a coarse band. The band is what a
codelet or renderer should branch on; the raw age is kept for audit."
  (let ((age (if (and (numberp observed-at) (numberp now) (>= now observed-at))
                 (- now observed-at)
                 :null)))
    (obj "observed_at" (or observed-at :null)
         "age" age
         "band" (cond ((not (numberp age)) "unknown")
                      ((<= age 60) "current")
                      ((<= age 3600) "recent")
                      (t "stale")))))

;;; --- reference extraction ------------------------------------------------

(defun %conscious-refs (coalition)
  "Identifiers only. A coalition's members name stimuli and events; this
carries those names forward and nothing else."
  (obj "coalition_key" (gethash "coalition_key" coalition)
       "priority_class" (gethash "priority_class" coalition)
       "explanation_code" (gethash "explanation_code" coalition)
       "member_count" (gethash "member_count" coalition)
       "evidence_ids" (gethash "evidence_ids" coalition)))

(defun %conscious-evidence-roots (coalitions)
  "Union of every referenced id across active coalitions, de-duplicated and
ordered so the projection is stable."
  (let ((seen '()))
    (map nil
         (lambda (c)
           (map nil (lambda (id) (pushnew id seen :test #'equal))
                (gethash "evidence_ids" c)))
         coalitions)
    (coerce (sort seen (lambda (a b) (string< (princ-to-string a) (princ-to-string b))))
            'vector)))

;;; --- projection ----------------------------------------------------------

(defun conscious-state-project (events &key now agent-id current-revision
                                            context pulse-in-flight
                                            (secondary-bound *conscious-secondary-bound*)
                                            (soft-bound *inbox-soft-bound*)
                                            (hard-bound *inbox-hard-bound*))
  "Fold EVENTS into a bounded conscious state.

Pure and total: the same events with the same NOW always produce the same
state, and no input is mutated. This is what makes exact rebuild possible --
there is no hidden accumulator, so replaying the log reconstructs the state
rather than approximating it.

STATE_REVISION is monotonic in the highest observed event id, so a later
projection over a superset of events never reports an earlier revision. It is
distinct from CONSUMPTION_WATERMARK, which advances only on acknowledgement."
  (let* ((ctx (if (projection-context-p context)
                  context
                  (make-projection-context
                   :now now :agent-id agent-id :runtime-revision current-revision
                   :soft-bound (or soft-bound *inbox-soft-bound*)
                   :hard-bound (or hard-bound *inbox-hard-bound*)
                   :secondary-bound (or secondary-bound *conscious-secondary-bound*))))
         (now (let ((v (gethash "now" ctx))) (if (eq v :null) nil v)))
         (current-revision (let ((v (gethash "runtime_revision" ctx)))
                             (if (eq v :null) nil v)))
         (secondary-bound (gethash "secondary" (gethash "bounds" ctx)))
         (inbox (inbox-project events :context ctx))
         (decision (attention-decide inbox :context ctx :now now
                                           :pulse-in-flight pulse-in-flight))
         (admitted (coerce (gethash "admitted" inbox) 'list))
         (by-id (let ((h (make-hash-table :test #'equal)))
                  (dolist (s admitted h)
                    (setf (gethash (gethash "stimulus_id" s) h) s))))
         (winner (let ((w (gethash "winner" decision))) (if (hash-table-p w) w nil)))
         (ordered-keys (coerce (gethash "ordered_keys" decision) 'list))
         ;; Reuse the coalitions the decision was actually made from. Running
         ;; the codelets again here would double their cost and, worse, let a
         ;; nondeterministic codelet produce a state whose coalitions
         ;; contradict its own selection record.
         (coalitions (coerce (gethash "coalitions" decision) 'list))
         (by-key (let ((h (make-hash-table :test #'equal)))
                   (map nil (lambda (c) (setf (gethash (gethash "coalition_key" c) h) c))
                        coalitions)
                   h))
         ;; Secondary items follow the selection ordering, minus the focus,
         ;; truncated to the bound. Ordering comes from attention rather than
         ;; being recomputed, so focus and secondary can never disagree about
         ;; relative priority.
         (secondary (let ((rest (remove (and winner (gethash "coalition_key" winner))
                                        (rest ordered-keys) :test #'equal)))
                      (loop for k in rest
                            for c = (gethash k by-key)
                            repeat secondary-bound
                            when c collect (%conscious-refs c))))
         (cancellation (find-if (lambda (s) (string= (gethash "kind" s) "cancellation"))
                                admitted))
         (watermark (gethash "watermark" inbox))
         (degraded (gethash "degraded" inbox)))
    (obj
     "schema_version" *conscious-state-schema-version*
     ;; Q1 originally called the highest observed event id STATE_REVISION.
     ;; Q3 makes that name incorrect: failed/cancelled pulse records are
     ;; observed facts, but only a successful commit may advance conscious
     ;; state. Preserve both event-derived, exactly rebuildable revisions.
     "observation_revision" (let ((h (gethash "highest_event_id" inbox)))
                              (if (numberp h) h 0))
     "state_revision"
     (let ((latest 0))
       (map nil
            (lambda (event)
              (when (and (hash-table-p event)
                         (equal "pulse-committed" (gethash "type" event)))
                (let* ((payload (gethash "payload" event))
                       (sequence (and (hash-table-p payload)
                                      (gethash "pulse_sequence" payload))))
                  (when (and (integerp sequence) (plusp sequence)
                             (> sequence latest))
                    (setf latest sequence)))))
            events)
       latest)
     "consumption_watermark" watermark
     "runtime_revision" (or current-revision :null)
     "composition_hash" (projection-context-hash ctx)
     "evaluated_at" (or now :null)

     "focus"
     (if winner
         (%slot (%conscious-refs winner)
                :provenance (gethash "evidence_ids" winner)
                :freshness (%freshness
                            (let ((m (aref (gethash "members" winner) 0)))
                              (let ((s (gethash (gethash "stimulus_id" m) by-id)))
                                (and s (gethash "observed_at" s))))
                            now)
                :lifecycle "active"
                :reason (gethash "explanation_code" winner))
         (%slot :null
                :provenance (vector)
                :freshness (%freshness nil now)
                :lifecycle "idle"
                :reason "no-eligible-coalition"))

     "selection"
     (%slot (obj "decision" (gethash "decision" decision)
                 "decided_by" (gethash "decided_by" decision)
                 "coalition_count" (gethash "coalition_count" decision)
                 "assessment_count" (gethash "assessment_count" decision))
            :provenance (gethash "ordered_keys" decision)
            :freshness (%freshness now now)
            :lifecycle "current"
            :reason "attention-stage-d")

     "secondary"
     (%slot (coerce secondary 'vector)
            :provenance (coerce (mapcar (lambda (x) (gethash "coalition_key" x)) secondary) 'vector)
            :freshness (%freshness now now)
            :lifecycle "active"
            :reason "ranked-below-focus")

     ;; Supplied as DATA on the context, never called from here. An earlier
     ;; version invoked a global function from inside the projection, which
     ;; meant a "pure" fold could do anything its caller had installed and
     ;; could return different results on identical events. A projection reads
     ;; its inputs; it does not summon them.
     "awaited"
     (let ((lifecycle (gethash "lifecycle" ctx)))
       (if (and lifecycle (plusp (length lifecycle)))
           (%slot lifecycle
                  :provenance
                  (coerce
                   (loop for row across lifecycle
                         for event-id = (and (hash-table-p row)
                                             (gethash "last_event_id" row))
                         when event-id collect event-id)
                   'vector)
                  :freshness (%freshness now now)
                  :lifecycle "open" :reason "lifecycle-projection")
           (%slot (vector)
                  :provenance (vector) :freshness (%freshness nil now)
                  :lifecycle "unavailable"
                  :reason "no-active-lifecycle")))

     "evidence_roots"
     (%slot (%conscious-evidence-roots coalitions)
            :provenance (vector "inbox")
            :freshness (%freshness now now)
            :lifecycle "referenced"
            :reason "active-coalition-members")

     ;; Measured, never inferred. Everything here is a count or a flag the
     ;; projection can substantiate.
     "sensorium"
     (%slot (obj "now" (or now :null)
                 "admitted" (gethash "admitted_count" inbox)
                 "barriers" (gethash "barrier_count" inbox)
                 "coalesced" (gethash "coalesced_count" inbox)
                 "rejected" (length (gethash "rejected" inbox))
                 "deferred" (length (gethash "deferred" inbox))
                 "watermark" watermark)
            :provenance (vector "inbox")
            :freshness (%freshness now now)
            :lifecycle "measured"
            :reason "inbox-projection")

     "interruption"
     (%slot (obj "cancellation_pending" (if cancellation t nil)
                 "stimulus_id" (if cancellation (gethash "stimulus_id" cancellation) :null)
                 "pulse_in_flight" (if pulse-in-flight t nil))
            :provenance (if cancellation (gethash "source_event_ids" cancellation) (vector))
            :freshness (%freshness (and cancellation (gethash "observed_at" cancellation)) now)
            :lifecycle (if cancellation "pending" "clear")
            :reason (if cancellation "cancellation-admitted" "no-cancellation"))

     ;; What would justify waking again. Derived from the decision so it can
     ;; never contradict it.
     "next_wake"
     (%slot (let ((d (gethash "decision" decision)))
              (cond ((string= d "pulse-now") (obj "condition" "immediate"))
                    ((string= d "interrupt-at-boundary")
                     (obj "condition" "pulse-boundary"))
                    ((string= d "materialize-only")
                     (obj "condition" "degradation-cleared"))
                    (t (obj "condition" "new-eligible-stimulus"))))
            :provenance (vector "attention")
            :freshness (%freshness now now)
            :lifecycle "conditional"
            :reason "derived-from-decision")

     "flags"
     (%slot (obj "degraded" (if degraded t nil)
                 "degraded_reason" (gethash "degraded_reason" inbox)
                 "codelet_errors" (length (gethash "codelet_errors" decision))
                 "unverified_revision_items"
                 (count-if (lambda (s) (string= (gethash "revision_status" s "") "unverified"))
                           admitted)
                 ;; Counted, never assumed safe. LOG-EVENT now stamps the
                 ;; partition, so a legacy reading means the event predates
                 ;; that -- a statement about history, not a standing excuse.
                 ;; Q2 can refuse these when live and accept them when
                 ;; explicitly replaying.
                 "legacy_partition_items"
                 (count-if (lambda (s)
                             (string= (gethash "partition_status" s "")
                                      "legacy-partition-assumed"))
                           admitted))
            :provenance (vector "inbox" "attention")
            :freshness (%freshness now now)
            :lifecycle "measured"
            :reason "degradation-and-uncertainty"))))

(defun conscious-state-report (state)
  "Operator-facing summary. Structure and counts only -- no payloads, no
evidence content, no stimulus text. Safe for a dashboard."
  (obj "schema_version" (gethash "schema_version" state)
       "state_revision" (gethash "state_revision" state)
       "observation_revision" (gethash "observation_revision" state)
       "runtime_revision" (gethash "runtime_revision" state)
       "decision" (gethash "decision" (gethash "value" (gethash "selection" state)))
       "decided_by" (gethash "decided_by" (gethash "value" (gethash "selection" state)))
       "focus_reason" (gethash "reason" (gethash "focus" state))
       "focus_lifecycle" (gethash "lifecycle" (gethash "focus" state))
       "secondary_count" (length (gethash "value" (gethash "secondary" state)))
       "awaited_lifecycle" (gethash "lifecycle" (gethash "awaited" state))
       "awaited_count" (length (gethash "value" (gethash "awaited" state)))
       "evidence_root_count" (length (gethash "value" (gethash "evidence_roots" state)))
       "flags" (gethash "value" (gethash "flags" state))
       "next_wake" (gethash "value" (gethash "next_wake" state))))
