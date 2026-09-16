;;;; concern.lisp -- stable concern identity and event-derived history.
;;;;
;;;; Workstream Q, slice Q1b. Added to close the codelet-context gap: the
;;;; spec names ten codelets, one of which is "persistence/insistence with
;;;; decay and fatigue controls" -- and insistence is by definition a fact
;;;; ABOUT REPEATED ENCOUNTERS, which a function of a single stimulus cannot
;;;; know.
;;;;
;;;; The tempting shortcut is a counter inside the codelet. That is exactly
;;;; wrong here, for the same reason the inbox's consumption position could
;;;; not be a variable: a counter is state a rebuild cannot reconstruct, so
;;;; the moment insistence lives in a codelet, replay stops reproducing
;;;; history and starts inventing it. Cross-pulse state must be event-derived
;;;; or it is not state, it is drift.
;;;;
;;;; So: concern history is a PROJECTION, computed the same way everything
;;;; else here is, and handed to codelets as immutable context data. A codelet
;;;; reads it and may PROPOSE a transition; it never writes one. Proposals are
;;;; inert until some later stage materializes them as events, at which point
;;;; the next projection sees them. That is the same shape as every other
;;;; effect in this architecture, applied to the agent's own attentional
;;;; habits.
;;;;
;;;; WHAT IS DERIVABLE TODAY, AND WHAT IS NOT
;;;;
;;;; First-observed, last-observed and observation count come from the event
;;;; log now. Selection, presentation, deferral and outcome need pulse
;;;; records, which do not exist until Q3. Those fields are present and read
;;;; zero, with `history_completeness` saying so -- rather than being omitted,
;;;; which would let a consumer mistake "never selected" for "not tracked".

(in-package :agent)

(export '(concern-identity concern-history-project concern-history-for
          make-concern-transition *concern-schema-version*
          *concern-selection-window*))

(defparameter *concern-schema-version* 1)

(defparameter *concern-selection-window* 20
  "How many recent PULSES the selection count is computed over.

Pulses, not selections. The first implementation kept the last N selection
timestamps and truncated only when a selection occurred -- so a concern
selected once and then passed over for a thousand pulses still counted that
selection, and fatigue never decayed. Last-20-selections and selections-in-
the-last-20-pulses diverge exactly when a concern goes quiet, which is the
case the window exists to handle.

Measured against pulse SEQUENCE, which advances on every pulse whether or not
this concern was involved. That needs pulse records to carry a sequence; until
Q3 produces them the window cannot decay, and HISTORY_COMPLETENESS says so
rather than implying a decay that is not happening.")

(defparameter *concern-pulse-event-types*
  '("pulse-committed" "concern-deferred" "concern-presented")
  "Event types that carry concern outcomes. None is produced yet -- pulses
arrive in Q3. Declared here so the projection is written against the real
vocabulary rather than retrofitted, and so a census can see what this
subsystem expects to consume.")

;;; --- identity ------------------------------------------------------------

(defun concern-identity (stimulus)
  "A key that is stable across pulses for the same ongoing concern.

Deliberately NOT the coalition key: that includes the fence and is scoped to
one pulse. Concern identity has to survive across pulses or insistence cannot
be measured.

Where a stimulus has a real correlation -- an operation, a schedule, a task --
the concern is that thing, and every result about it is the same concern
recurring. Where it has none, the concern is the stimulus itself and by
construction never recurs, which is correct: a one-off event should never
accumulate insistence. That also avoids the failure this codebase has now hit
three times, where a key built from insufficient identity silently merges
unrelated things (see %STIMULUS-COALESCING-KEY)."
  (let ((correlation (gethash "correlation_id" stimulus)))
    (if (stringp correlation)
        ;; Namespaced by SOURCE, not by kind. Keying on kind split a
        ;; succeeded operation from a failed one -- concern:tool-result:op
        ;; versus concern:tool-failure:op -- so the same operation failing
        ;; then succeeding looked like two unrelated concerns and neither
        ;; accumulated insistence. That directly contradicted this function's
        ;; own contract, which is that every result about an operation is the
        ;; same concern recurring.
        (format nil "concern:~a:~a" (gethash "source" stimulus "unknown") correlation)
        (format nil "concern:~a" (gethash "stimulus_id" stimulus)))))

;;; --- history projection --------------------------------------------------

(defun %concern-blank (identity)
  (obj "concern_identity" identity
       "first_observed" :null
       "last_observed" :null
       "observation_count" 0
       ;; Zero because unproduced, not because untrue. See HISTORY_COMPLETENESS.
       "selection_count" 0
       ;; Pulse sequence numbers, newest first. Evicted by DISTANCE from the
       ;; latest pulse, not by list length.
       "recent_selections" '()
       "last_selected" :null
       "last_presented" :null
       "consecutive_deferrals" 0
       "last_outcome" :null
       "cooldown_until" :null))

(defun %concern-observe (history identity observed-at)
  (let ((entry (or (gethash identity history) (%concern-blank identity))))
    (when (numberp observed-at)
      (let ((first (gethash "first_observed" entry)))
        (unless (numberp first) (setf (gethash "first_observed" entry) observed-at)))
      (setf (gethash "last_observed" entry) observed-at))
    (incf (gethash "observation_count" entry))
    (setf (gethash identity history) entry)
    entry))

(defun %concern-apply-outcome (history payload)
  "Fold a pulse outcome into concern history. Tolerant of absent fields: the
producing events do not exist yet, so this must not assume their final shape
beyond the names declared in *CONCERN-PULSE-EVENT-TYPES*."
  (let* ((identity (gethash "concern_identity" payload))
         (entry (and (stringp identity)
                     (or (gethash identity history)
                         (setf (gethash identity history) (%concern-blank identity))))))
    (when entry
      (let ((outcome (gethash "outcome" payload))
            (at (gethash "at" payload)))
        (setf (gethash "last_outcome" entry) (or outcome :null))
        (cond
          ((equal outcome "selected")
           ;; Record which PULSE this happened in. Eviction is deferred to the
           ;; end of the fold, when the latest pulse sequence is known --
           ;; truncating here would implement last-N-selections again.
           (push (or (gethash "pulse_sequence" payload) 0)
                 (gethash "recent_selections" entry))
           (setf (gethash "last_selected" entry) (or at :null)
                 (gethash "consecutive_deferrals" entry) 0))
          ((equal outcome "presented")
           (setf (gethash "last_presented" entry) (or at :null)))
          ((equal outcome "deferred")
           (incf (gethash "consecutive_deferrals" entry))))
        (let ((cooldown (gethash "cooldown_until" payload)))
          (when (numberp cooldown)
            (setf (gethash "cooldown_until" entry) cooldown)))))
    entry))

(defun concern-history-project (events &key agent-id context
                                            (kind-map *stimulus-kind-map*)
                                            (discriminators *stimulus-discriminators*))
  "Fold EVENTS into per-concern history. Pure; no clock, no mutation of
EVENTS.

Returns a hash table of concern identity -> history entry, plus a
completeness marker recording which fields are actually being produced. The
marker exists because a consumer must be able to distinguish `never selected`
from `selection is not tracked yet`, and those look identical in the data."
  (let* ((km (or (projection-context-policy context "kind_map" nil) kind-map))
         (disc (or (projection-context-policy context "discriminators" nil) discriminators))
         (history (make-hash-table :test #'equal))
         (seen-kinds (make-hash-table :test #'equal))
         (latest-pulse 0)
         (outcome-events 0))
    (map nil
         (lambda (event)
           (when (hash-table-p event)
             (let ((type (gethash "type" event))
                   (payload (let ((p (gethash "payload" event)))
                              (if (hash-table-p p) p (obj)))))
               (cond
                 ((member type *concern-pulse-event-types* :test #'equal)
                  (incf outcome-events)
                  (let ((outcome (gethash "outcome" payload))
                        (seq (gethash "pulse_sequence" payload)))
                    (when (and (numberp seq) (> seq latest-pulse))
                      (setf latest-pulse seq))
                    (when (stringp outcome)
                      (incf (gethash outcome seen-kinds 0))))
                  (%concern-apply-outcome history payload))
                 ;; Projected under the SAME admission policy candidacy uses,
                 ;; from the pinned context when one is supplied. Reading the
                 ;; globals meant insistence history could be built from a
                 ;; different classification than the inbox saw, so a
                 ;; payload-journalized event still contaminated it.
                 ((nth-value 1 (gethash type km))
                  (let ((s (stimulus-from-event event :agent-id agent-id
                                                      :kind-map km
                                                      :discriminators disc)))
                    (when s
                      (%concern-observe history (concern-identity s)
                                        (gethash "observed_at" s)))))))))
         events)
    ;; Evict selections older than the window, measured in PULSES from the
    ;; latest pulse observed -- so a concern that goes quiet decays even
    ;; though it records no new selections.
    (maphash
     (lambda (identity entry)
       (declare (ignore identity))
       (let ((kept (remove-if (lambda (seq)
                                (and (numberp seq)
                                     (> (- latest-pulse seq) *concern-selection-window*)))
                              (gethash "recent_selections" entry))))
         (setf (gethash "recent_selections" entry) kept
               (gethash "selection_count" entry) (length kept))))
     history)
    (obj "schema_version" *concern-schema-version*
         "concerns" history
         "latest_pulse_sequence" latest-pulse
         "concern_count" (hash-table-count history)
         ;; Honest about which half of the contract is live.
         "history_completeness"
         ;; Reported PER FIELD. Previously one outcome event of any sort made
        ;; selection, presentation and deferral all claim "derived", so a log
        ;; containing only deferrals asserted that selection was tracked.
        (obj "observation" "derived"
              "selection" (if (plusp (gethash "selected" seen-kinds 0))
                              "derived" "unavailable-until-q3")
              "presentation" (if (plusp (gethash "presented" seen-kinds 0))
                                 "derived" "unavailable-until-q3")
              "deferral" (if (plusp (gethash "deferred" seen-kinds 0))
                             "derived" "unavailable-until-q3")
              "selection_window" *concern-selection-window*
              ;; Whether the window can actually decay. Without pulse
              ;; sequences every selection sits at 0 and nothing ages out, so
              ;; claiming "windowed" would imply a decay that is not happening.
              "window_basis" (if (plusp latest-pulse) "pulse-sequence"
                                 "unavailable-until-q3")
              "outcome_events_seen" outcome-events))))

(defun concern-history-for (projection stimulus)
  "History for STIMULUS's concern, or a blank entry. Never NIL: a codelet
branching on absence would otherwise have to distinguish `no history` from
`no history projection`, and both should read as `nothing known yet`."
  (let* ((identity (concern-identity stimulus))
         (concerns (and (hash-table-p projection) (gethash "concerns" projection))))
    (or (and (hash-table-p concerns) (gethash identity concerns))
        (%concern-blank identity))))

;;; --- transition proposals ------------------------------------------------

(defun make-concern-transition (&key concern-identity kind reason
                                     (cooldown-until :null))
  "An INERT proposal to change concern state. Returned by a codelet, applied
by nobody here.

A codelet that mutated its own history would put cross-pulse state outside
the event log, which is precisely the drift this file exists to prevent. So a
codelet says what it thinks should change and a later stage decides whether
to materialize it as an event. Until then it is a suggestion in a data
structure -- the same relationship every other proposal in this architecture
has to its effect."
  (obj "schema_version" *concern-schema-version*
       "concern_identity" concern-identity
       "transition" kind
       "reason" reason
       "cooldown_until" cooldown-until
       "materialized" nil))
