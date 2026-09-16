;;;; policy.lisp -- every input that changes how events are interpreted.
;;;;
;;;; Workstream Q, slice Q1d. Created after a second review found that the
;;;; "pinned" projection context pinned almost nothing: it carried a copy of
;;;; the admission table that no code path read, omitted the payload
;;;; discriminators entirely, and hashed codelets by name and order only. The
;;;; demonstration was unambiguous -- mutate the live discriminator, and the
;;;; SAME pinned context turned the SAME event from non-barrier to barrier
;;;; while reporting an unchanged composition hash.
;;;;
;;;; The root cause was structural, not an oversight in one place: policy was
;;;; scattered across four files as defparameters, each consulted directly by
;;;; the code next to it. Nothing could snapshot what it could not enumerate.
;;;;
;;;; So: one file owns every policy table. The projection context snapshots
;;;; THIS, hashes it, and hands it down. Projection code reads the snapshot it
;;;; was given and never these globals -- the globals are defaults for
;;;; building a context, not a back channel around one.
;;;;
;;;; THE RULE
;;;;
;;;;   If changing it changes how an event is interpreted, it lives here and
;;;;   it is in the hash.
;;;;
;;;; That is the test to apply when adding anything. A tie-break, an urgency
;;;; rank, a bound, an admission row, a discriminator -- all change
;;;; interpretation, so all are here. A logging format or a report field does
;;;; not, so it is not.

(in-package :agent)

(export '(*stimulus-kind-map* *stimulus-discriminators*
          *attention-priority-classes* *attention-urgency-rank*
          *attention-tie-breaks* *attention-explanation-max*
          *inbox-soft-bound* *inbox-hard-bound* *conscious-secondary-bound*
          *curiosity-motivation-policy*
          *policy-normalization-version* *policy-schema-version*
          *pai-runtime-revision*))

(defparameter *policy-schema-version* 1)

(defvar *pai-runtime-revision* nil
  "Identity of the running cognition-runtime composition, or NIL when unset.

Stamped onto asynchronous internal results so a result returning after a
revision change can be told from a current one -- the stale-result rule in
the runtime spec depends on it, and without it the rule cannot be honoured.

Defined here rather than in the conscious subsystem because a PRODUCER writes
it (first-person-evidence.lisp stamps operation terminals), and that producer
must not have to depend on the conscious runtime being loaded. NIL reads as
UNKNOWN downstream, never as current.")

(defparameter *policy-normalization-version* 2
  "Version of the assessment-normalization rules. Bumped when what
normalization accepts, rejects or rewrites changes -- because that changes
interpretation as surely as an admission row does, and two runs under
different normalization are not comparable.")

;;; --- bounds --------------------------------------------------------------

(defparameter *inbox-soft-bound* 64
  "Active candidacy count above which the projector starts coalescing,
expiring, and deferring low-urgency material. A threshold, not a limit.")

(defparameter *inbox-hard-bound* 256
  "Active candidacy count above which non-barrier stimuli are refused
admission with an explicit reason. Barriers are never refused here.")

(defparameter *conscious-secondary-bound* 7
  "How many non-focus active items the conscious state retains. Bounded
because conscious state is a working set, not an archive.")

;;; --- urgency and priority ------------------------------------------------

(defparameter *inbox-urgency-rank*
  (obj "interactive" 0 "timely" 1 "background" 2)
  "Lower sorts first. Deterministic, derived from the admission table, never
from content.")

(defparameter *attention-urgency-rank* *inbox-urgency-rank*
  "Selection uses the same ranking as candidacy ordering. Deliberately the
same object: two rankings that could disagree would let a stimulus be shed by
one rule and preferred by another.")

(defparameter *attention-priority-classes*
  '(("critical"  . 0)   ; cancellation, operator control, runtime anomaly
    ("direct"    . 1)   ; a person addressed the agent and is waiting
    ("committed" . 2)   ; a result the agent is awaiting, or a promise it made
    ("relevant"  . 3)   ; bears on an active goal, or contradicts something held
    ("ambient"   . 4))  ; novelty, social information, background change
  "Alist of class name -> rank. Lower ranks are selected first, absolutely:
no quantity of ambient candidates outranks one direct address.")

(defparameter *attention-tie-breaks*
  '("sole-candidate" "priority-class" "urgency-class" "deadline-soonest"
    "oldest-waiting" "coalition-key-lexical")
  "The complete, ordered list of comparisons selection may use. A comparison
not on this list cannot influence the outcome, which is what keeps the
baseline auditable.")

(defparameter *attention-explanation-max* 120
  "Explanation codes reach the operator-facing report, so their length is
bounded and their shape constrained; see %ATTENTION-CODE-SHAPED-P.")

;;; --- motivational dynamics ---------------------------------------------

(defparameter *curiosity-motivation-policy*
  '((:schema-version . 1)
    (:reinforcement-weights
     ("novel-observation" . 220)
     ("unresolved-recurrence" . 180)
     ("operator-interest" . 260)
     ("contradictory-evidence" . 80))
    (:decay-milliunits-per-hour . 10)
    (:rising-threshold . 150)
    (:salient-threshold . 550)
    (:partial-satisfaction-drop . 250)
    (:full-satisfaction-level . 50)
    (:refractory-seconds . 3600)
    (:motive-bound . 64)
    (:active-retirement-policy . "settled-then-lowest-nonsalient-v1")
    (:subject-ref-bound . 16)
    (:evidence-root-bound . 64)
    (:invalid-diagnostic-bound . 32)
    (:expression-policy . "private-consideration-only"))
  "Pinned Q5M curiosity policy. Activation is integer milliunits; it is
within-motive lifecycle state, never a universal priority or authority score.
MOTIVE-BOUND limits the active projection, not lifetime authority: at pressure
the projector retires fully satisfied rows first, then the oldest
lowest-activation non-salient row. Salient rows are never displaced.")

;;; --- stimulus admission --------------------------------------------------
;;;
;;; event type -> (kind source urgency barrier-p)
;;;
;;; The full classification of all 83 event types, including the reasoning for
;;; every EXCLUSION, is generated from docs/event-type-census.sexp. This table
;;; is built from that manifest so the two cannot drift; see census.lisp.

(defparameter *stimulus-kind-map* (make-hash-table :test #'equal)
  "Event type -> stimulus classification, populated from the census manifest
at load time by census.lisp. An event type absent from this table is
journal-only and is never admitted.")

(defparameter *stimulus-discriminators* (make-hash-table :test #'equal)
  "Event type -> (PAYLOAD) -> (kind sub-kind urgency barrier-p), populated
from the census manifest.

A discriminator may refine kind, urgency and barrier status from payload
content -- but only from fields the PRODUCER set, never from anything a model
wrote. The admission table remains the authority on what is admissible at
all; a discriminator only says which sort it is.")
