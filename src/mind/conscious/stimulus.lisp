;;;; stimulus.lisp -- typed stimulus admission over the event log.
;;;;
;;;; Workstream Q, slice Q1. Pure projection: no workers, no model, no tools,
;;;; no publication, no I/O, no clock read that is not passed in. Nothing in
;;;; this file is reachable from the :auto runtime; it is loaded and inert
;;;; until a conscious-state runtime selects it.
;;;;
;;;; WHY AN ALLOWLIST
;;;;
;;;; The event log carries ~70 distinct types. Most are journal entries --
;;;; pg-backup, timing-trace, self-mod-provenance-recorded -- written so the
;;;; past is reconstructible, not because anything should ever wake for them.
;;;; A stimulus is the much smaller set of facts that can legitimately compete
;;;; for attention.
;;;;
;;;; So admission is an explicit allowlist, not a filter over everything. An
;;;; unmapped event type is journal-only and is NOT admitted. That direction
;;;; matters: a new event type added anywhere in the system must be
;;;; deliberately declared a stimulus before it can reach cognition, rather
;;;; than silently appearing in the inbox because it happened to be logged.
;;;;
;;;; The failure mode this avoids is the one gotcha 25 describes from the
;;;; other side: there, a scan that matched nothing reported success. Here, an
;;;; admission rule that matched everything would flood attention with backup
;;;; notifications. Both are cases of a rule whose default is wrong.

(in-package :agent)

(export '(stimulus-from-event stimulus-admissible-p stimulus-kinds
          stimulus-kind-map-report *stimulus-schema-version*))

(defparameter *stimulus-schema-version* 1)

(defparameter *stimulus-unknown-revision* :null
  "Value for ORIGIN_RUNTIME_REVISION when an event predates revision
stamping. Deliberately NOT the current revision: a historical row means
*unknown*, and reading it as *current* is exactly the mistake that would let a
stale asynchronous result be treated as fresh. See the Q0 schema freeze.")

;;; --- the admission table -------------------------------------------------
;;;
;;; event type -> (kind source urgency barrier-p)
;;;
;;;   kind      the typed vocabulary from the runtime spec, section 9.1
;;;   source    channel / scheduler / tool / model / memory / system / internal
;;;   urgency   deterministic class. Derived from the source and deadline
;;;             semantics ONLY. A model cannot raise its own urgency, which is
;;;             why this is a static table and not a computed score.
;;;   barrier   ordering-sensitive: cannot be skipped or coalesced away.
;;;
;;; Kept deliberately small. Adding a row is a decision with a fixture, not a
;;; convenience.

(defun %stimulus-discriminate (type payload spec discriminators)
  "Refine SPEC using a payload discriminator when one is declared.

Returns (values kind source urgency barrier-p sub-kind), or NIL when the
discriminator reclassifies this payload as journal-only. A type can therefore
be admissible in general and journal for a particular payload -- a delivered
scheduler notification being the case that forced it.

DISCRIMINATORS is passed in rather than read from the global, so a pinned
projection context interprets events by the policy it captured. Reading the
global here is exactly the defect that made the previous context pin
nothing."
  (destructuring-bind (kind source urgency barrier-p) spec
    (let ((fn (and discriminators (gethash type discriminators))))
      (if (null fn)
          (values kind source urgency barrier-p :null)
          (let ((result (funcall fn payload)))
            (if (eq result :journal)
                nil
                (destructuring-bind (d-kind sub d-urgency d-barrier) result
                  (values d-kind source d-urgency d-barrier sub))))))))

(defparameter *stimulus-discriminator-kinds* '("tool-failure")
  "Kinds no base admission row declares, reachable only via a discriminator.

Declared rather than derived because a discriminator is a function of a
payload: its outputs cannot be enumerated without inventing payloads. Omitting
them made STIMULUS-KINDS under-report -- it never listed `tool-failure`, so
anything auditing \"what kinds can this agent produce\" got an answer that was
quietly incomplete. Under-reporting a capability is worse than over-reporting
one, because nobody goes looking for what the inventory says is absent.")

(defun stimulus-kinds ()
  "Every stimulus kind this admission policy can produce, INCLUDING kinds
reachable only through a payload discriminator."
  (let ((seen (copy-list *stimulus-discriminator-kinds*)))
    (maphash (lambda (type spec)
               (declare (ignore type))
               (pushnew (first spec) seen :test #'string=))
             *stimulus-kind-map*)
    (sort seen #'string<)))

(defun stimulus-admissible-p (event-type)
  "True when EVENT-TYPE is declared a stimulus. Journal-only types return NIL."
  (and (stringp event-type)
       (nth-value 1 (gethash event-type *stimulus-kind-map*))))

;;; --- field derivation ----------------------------------------------------

(defun %stimulus-payload (event)
  (let ((payload (gethash "payload" event)))
    (if (hash-table-p payload) payload (obj))))

(defun %stimulus-correlation-id (event payload)
  "Correlation is not uniform in the legacy envelope: ticks carry TICK_ID on
the event, turns carry TURN_ID inside the payload, schedules carry
SCHEDULE_ID. Prefer the most specific available rather than inventing one."
  (or (%stimulus-present (gethash "schedule_id" payload))
      (%stimulus-present (gethash "operation_id" payload))
      (%stimulus-present (gethash "turn_id" payload))
      (%stimulus-present (gethash "tick_id" event))
      :null))

(defun %stimulus-present (value)
  "NIL and :NULL both mean absent. Returns the value, or NIL when absent.
Serialisation sentinels are truthy in Lisp (gotcha 31), so presence is tested
here once rather than at each call site."
  (cond ((null value) nil)
        ((eq value :null) nil)
        ((and (stringp value) (zerop (length value))) nil)
        (t value)))

(defun %stimulus-occurred-at (event payload)
  "When the fact happened. For a schedule this is when it was DUE, which can
be well before it fired; that gap is information and is preserved rather than
collapsed. Everything else occurred when it was observed."
  (or (%stimulus-present (gethash "scheduled_for_utc" payload))
      (%stimulus-present (gethash "timestamp" event))
      :null))

(defun %stimulus-observed-at (event payload)
  "When the system learned of it."
  (or (%stimulus-present (gethash "fired_at_utc" payload))
      (%stimulus-present (gethash "timestamp" event))
      :null))

(defparameter *stimulus-inline-payload-max* 512
  "Byte budget above which an inline payload is referenced by size and digest
rather than described as merely `inline`.")

(defun %stimulus-canonical-json (value)
  "Serialise VALUE with hash-table keys in sorted order.

SHASHT:WRITE-JSON emits hash-table keys in iteration order, which is stable
within neither a process nor across images. A digest over that is a digest
over an accident: the same payload could hash differently in two processes,
so a cross-image replay comparing digests would report a difference that does
not exist -- and the whole point of the digest is letting a later retrieval
confirm it got what this projection saw."
  (labels ((emit (v out)
             (cond
               ((hash-table-p v)
                (let ((keys '()))
                  (maphash (lambda (k x) (declare (ignore x)) (push k keys)) v)
                  (write-char #\{ out)
                  (loop for k in (sort keys #'string< :key #'princ-to-string)
                        for first = t then nil
                        do (unless first (write-char #\, out))
                           (format out "~s:" (princ-to-string k))
                           (emit (gethash k v) out))
                  (write-char #\} out)))
               ((and (vectorp v) (not (stringp v)))
                (write-char #\[ out)
                (loop for x across v for first = t then nil
                      do (unless first (write-char #\, out))
                         (emit x out))
                (write-char #\] out))
               ((stringp v) (format out "~s" v))
               (t (format out "~a" v)))))
    (with-output-to-string (out) (emit value out))))

(defun %stimulus-payload-digest (payload)
  "FNV-1a over the payload's CANONICAL serialised form. Matches %PC-DIGEST;
detects change, does not resist forgery. Returns (values digest byte-length)."
  (let ((text (handler-case (%stimulus-canonical-json payload)
                (error () (princ-to-string payload))))
        (hash 14695981039346656037))
    (declare (type (unsigned-byte 64) hash))
    (loop for ch across text
          do (setf hash (ldb (byte 64 0)
                             (* (logxor hash (char-code ch)) 1099511628211))))
    ;; BYTE length. The field is documented as a byte budget, and a character
    ;; count would understate any payload containing multi-byte characters --
    ;; so a bound meant to cap transferred size would not have capped it.
    (values (format nil "~(~16,'0x~)" hash)
            (length (sb-ext:string-to-octets text :external-format :utf-8)))))

(defun %stimulus-payload-ref (payload event-id)
  "A reference to the payload, never the payload itself.

Previously this emitted a bare {\"kind\":\"inline\"} when no artifact digest
existed -- which contains neither a bounded payload nor a reference, so a
consumer holding it could not retrieve the content OR verify it. It named a
category and supplied nothing.

Every branch now yields something a consumer can act on: a content-addressed
digest where the producer supplied one, else an immutable event reference plus
a digest and size of the payload as projected. The digest lets a later
retrieval confirm it got what this projection saw, which is the property that
makes references-not-content safe across a replay."
  (let ((sha (%stimulus-present (gethash "artifact_sha256" payload))))
    (if sha
        (obj "kind" "content-addressed" "sha256" sha
             "artifact_id" (or (%stimulus-present (gethash "artifact_id" payload)) :null)
             "artifact_version" (or (%stimulus-present (gethash "artifact_version" payload)) :null))
        (multiple-value-bind (digest size) (%stimulus-payload-digest payload)
          (obj "kind" "event-reference"
               "source_event_id" event-id
               "payload_digest" digest
               "payload_size" size
               ;; Bounded: past the budget a consumer must fetch and verify
               ;; rather than assume the payload is small enough to inline.
               "bounded" (if (<= size *stimulus-inline-payload-max*) t nil))))))

(defun %stimulus-coalescing-key (kind correlation-id)
  "Replaceable signals share a key so the inbox can keep only the newest.
Only advisory kinds coalesce; barriers never do, because collapsing them is
precisely the loss the barrier flag exists to prevent.

A key REQUIRES a real correlation. An earlier version formatted the key from
whatever CORRELATION-ID held, which for an uncorrelated stimulus was :NULL --
so every uncorrelated advisory event of the same kind shared the key
\"prediction-due:NULL\" and collapsed into one, discarding unrelated
resolutions that merely had nothing to correlate against. Coalescing means
\"this supersedes that\"; without a correlation there is no basis for the
claim, so such stimuli are independent instead."
  (if (and (member kind '("project-change" "prediction-due") :test #'string=)
           (stringp correlation-id))
      (format nil "~a:~a" kind correlation-id)
      :null))

;;; --- construction --------------------------------------------------------

(defun stimulus-from-event (event &key agent-id
                                       (kind-map *stimulus-kind-map*)
                                       (discriminators *stimulus-discriminators*))
  "Project EVENT into a stimulus envelope, or NIL when the type is
journal-only.

EVENT is the stored event hash table: id, type, payload, caused_by,
timestamp, tick_id. Pure -- no clock, no I/O, no mutation of EVENT.

KIND-MAP and DISCRIMINATORS are the admission policy. They default to the
globals for direct use, but a projection running under a pinned context
passes that context's captured policy instead. The globals are defaults for
BUILDING a context, never a back channel around one -- reading them here
regardless is what made the previous context pin nothing.

Fields the legacy envelope cannot supply are set explicitly rather than
omitted, so a consumer never has to distinguish absent-because-unsupported
from absent-because-unset."
  (unless (hash-table-p event) (return-from stimulus-from-event nil))
  (let* ((type (gethash "type" event))
         (spec (gethash type kind-map)))
    (unless spec (return-from stimulus-from-event nil))
    (let ((payload (%stimulus-payload event)))
      (multiple-value-bind (kind source urgency barrier-p sub-kind)
          (%stimulus-discriminate type payload spec discriminators)
        ;; A discriminator may reclassify a particular payload as journal.
        (unless kind (return-from stimulus-from-event nil))
      (let* ((event-id (gethash "id" event))
             (correlation (%stimulus-correlation-id event payload)))
        ;; A stimulus without a usable event id has no stable identity and no
        ;; causal root. Admitting it would produce "stimulus:NIL" with a root
        ;; vector of #(NIL) -- non-empty, so the malformed check passed, while
        ;; violating the stable-id and non-empty-root semantics the envelope
        ;; promises. Refused here rather than downstream.
        (unless (or (numberp event-id)
                    (and (stringp event-id) (plusp (length event-id))))
          (return-from stimulus-from-event nil))
        (obj
         "schema_version"    *stimulus-schema-version*
         "stimulus_id"       (format nil "stimulus:~a" event-id)
         "sub_kind"          sub-kind
         "source_event_ids"  (vector event-id)
         "kind"              kind
         "source"            source
         "occurred_at"       (%stimulus-occurred-at event payload)
         "observed_at"       (%stimulus-observed-at event payload)
         ;; The partition the EVENT claims, never the one the projection was
         ;; asked for. An earlier version stamped the target agent id onto
         ;; every stimulus, so the Stage A partition check validated a value
         ;; the projection had just manufactured -- an event explicitly
         ;; carrying another agent's partition was admitted and rewritten as
         ;; belonging to the target. A check on a value you just wrote is not
         ;; a check.
         ;;
         ;; :NULL when the event does not say. Absent is NOT the same as
         ;; matching, and the inbox refuses it when a partition is expected;
         ;; see %INBOX-WRONG-PARTITION-P. Historical events predate partition
         ;; stamping, so this is common -- which is exactly why it must read
         ;; as unknown rather than as belonging to whoever is asking.
         "agent_id"          (or (%stimulus-present (gethash "agent_id" event))
                                 (%stimulus-present (gethash "agent_id" payload))
                                 :null)
         "requested_partition" (or agent-id :null)
         ;; v1-compatible default per the runtime spec. Not inferred from
         ;; content: audience is an authority-adjacent fact and a stimulus
         ;; must not be able to widen its own.
         "audience"          "operator"
         "correlation_id"    correlation
         "causation_id"      (or (%stimulus-present (gethash "caused_by" event)) :null)
         "payload_ref"       (%stimulus-payload-ref payload event-id)
         ;; Trust is the SOURCE's status, never a judgement about content.
         "trust"             (if (string= source "channel") "inbound-unverified" "internal")
         "grounding"         "event-log"
         "urgency_class"     urgency
         "expires_at"        :null
         "coalescing_key"    (%stimulus-coalescing-key kind correlation)
         "barrier"           (if barrier-p t nil)
         "origin_runtime_revision"
         (or (%stimulus-present (gethash "origin_runtime_revision" payload))
             *stimulus-unknown-revision*)))))))

(defun stimulus-kind-map-report ()
  "Inspectable admission table. Answers 'what can wake this agent, and why'
by lookup rather than by reading source."
  (let ((rows '()))
    (maphash
     (lambda (type spec)
       (destructuring-bind (kind source urgency barrier-p) spec
         (push (obj "event_type" type "kind" kind "source" source
                    "urgency_class" urgency "barrier" (if barrier-p t nil))
               rows)))
     *stimulus-kind-map*)
    (obj "schema_version" *stimulus-schema-version*
         "admitted_event_types" (hash-table-count *stimulus-kind-map*)
         "kinds" (coerce (stimulus-kinds) 'vector)
         "rows" (coerce (sort rows #'string<
                              :key (lambda (r) (gethash "event_type" r)))
                        'vector))))
