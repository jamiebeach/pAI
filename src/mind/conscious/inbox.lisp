;;;; inbox.lisp -- bounded active candidacy over the event log.
;;;;
;;;; Workstream Q, slice Q1. Pure projection, same isolation rules as
;;;; stimulus.lisp: no workers, no model, no tools, no publication, no I/O,
;;;; no clock read that is not passed in.
;;;;
;;;; The inbox is NOT a queue and NOT a second source of truth. It is a
;;;; rebuildable view over logged events plus a consumer watermark. Nothing
;;;; here can lose an event, because nothing here owns one: rejecting a
;;;; stimulus removes it from *candidacy*, never from history, and the source
;;;; event is always still there to re-project.
;;;;
;;;; That distinction is the whole safety argument for the bounds below. A
;;;; queue that drops under load has lost data. A projection that drops under
;;;; load has narrowed what is currently eligible for attention, and can widen
;;;; again on the next pass. Every bound in this file is a bound on active
;;;; candidacy, never on history.
;;;;
;;;; THE ASYMMETRY THAT MATTERS
;;;;
;;;; Barriers and non-barriers are not treated alike at the hard bound, and
;;;; the difference is deliberate. A non-barrier stimulus may be rejected from
;;;; candidacy with an explicit reason. A barrier may not -- barrier overflow
;;;; instead marks the whole projection degraded and pauses new cognitive
;;;; effects, because the alternative is silently dropping a user message
;;;; under load, which is the one failure this design will not accept.
;;;;
;;;; Losing a person's message is worse than pausing. Pausing is visible.

(in-package :agent)

(export '(inbox-project inbox-report
          *inbox-schema-version* *inbox-consumption-event-type*
          *inbox-consumption-event-types*))

(defparameter *inbox-schema-version* 2)

(defparameter *inbox-ack-dispositions*
  '("handled" "superseded" "expired" "rejected")
  "Recognised acknowledgement dispositions.

An acknowledgement states WHY candidacy ended. \"I dealt with this\" and
\"this expired unhandled\" are different facts about the agent's behaviour,
and collapsing them loses the only record that would distinguish an agent
keeping up from one silently dropping work. An unrecognised or absent
disposition is refused.")

(defparameter *inbox-ack-max-ids* 512
  "Maximum stimulus ids one acknowledgement may name. Unbounded, a single
event could retire the entire inbox -- a denial-of-attention primitive
reachable by anything able to write an event.")

(defparameter *inbox-consumption-event-type* "stimulus-consumed"
  "Event type recording that a consumer has finished with one or more
stimuli. Payload carries STIMULUS_IDS.

Consumption is EVENT-DERIVED, deliberately. An earlier version computed the
highest event id, called it a watermark, and never applied a consumption
position at all -- so every historical user message stayed an active barrier
forever, and a degraded inbox could never reach `degradation-cleared`. The
runtime would have livelocked, and the fixtures missed it because they each
projected one short stream once.

Holding consumption in a variable instead would have reintroduced the same
class of defect from the other direction: state that a rebuild cannot
reconstruct. Acknowledgement is a fact about the past, so it belongs in the
log with every other fact about the past.

This type is deliberately NOT in the stimulus admission table. An
acknowledgement is bookkeeping, not something to wake for, and admitting it
would make consuming a stimulus produce a new stimulus.")

(defparameter *inbox-consumption-event-types*
  (list *inbox-consumption-event-type* "pulse-committed")
  "Closed event types allowed to acknowledge candidacy.

Q3 makes a successful pulse commit and its consumption one durable fact. A
second STIMULUS-CONSUMED append would create a crash window on either side of
the commit. Both types pass through the same causal, partition, consumer,
disposition and cardinality validation below.")

;;; --- Stage A: deterministic eligibility (spec 11.1) ----------------------
;;; No model call. Every rejection carries a machine-readable reason so a
;;; degraded inbox can be explained without re-deriving it.

(defun %inbox-urgency-rank (stimulus &optional (ranks *inbox-urgency-rank*))
  (or (gethash (gethash "urgency_class" stimulus "background") ranks) 9))

(defun %inbox-root-usable-p (root)
  "A causal root must be a usable identifier, not merely present. A vector
containing NIL is non-empty and was accepted by the earlier length check
while violating the non-empty-root semantics the envelope promises."
  (or (and (numberp root))
      (and (stringp root) (plusp (length root)))))

(defun %inbox-malformed-p (stimulus)
  (or (not (hash-table-p stimulus))
      (not (stringp (gethash "kind" stimulus)))
      (let ((id (gethash "stimulus_id" stimulus)))
        (or (not (stringp id))
            (zerop (length id))
            ;; "stimulus:NIL" is a stable-looking id derived from no id at
            ;; all. Refusing the shape here is belt-and-braces: stimulus.lisp
            ;; now declines to build one, and this catches any other producer.
            (search "NIL" id)))
      (let ((roots (gethash "source_event_ids" stimulus)))
        (or (not (vectorp roots))
            (zerop (length roots))
            (notevery #'%inbox-root-usable-p (coerce roots 'list))))
      ;; Enum fields must hold declared values. An undeclared urgency would
      ;; sort at rank 9 and an undeclared audience would silently widen
      ;; nothing -- both fail quietly, which is why they are checked loudly.
      (not (nth-value 1 (gethash (gethash "urgency_class" stimulus "")
                                 *inbox-urgency-rank*)))))



;;; CONTROL-LIKE CONTENT: CHECK REMOVED 2026-08-17.
;;;
;;; A pattern check lived here, claiming that an obvious prompt-injection
;;; attempt would be "quarantined and visible". It could not fire. It examined
;;; the stimulus fields "concern" and "sub_kind"; user message text lives in
;;; the event payload and never reaches either. A real event containing
;;; "Ignore previous instructions" was admitted normally, and the integration
;;; fixture passed SPECIFICALLY when nothing was quarantined.
;;;
;;; It is deleted rather than repaired, deliberately. Repairing it would have
;;; meant projecting payload text into the stimulus to scan it -- pulling
;;; content into a projection whose whole discipline is references-not-content,
;;; to power a check that is trivially evadable and was never the security
;;; boundary anyway.
;;;
;;; The real boundary is structural and unchanged: a stimulus cannot raise its
;;; own urgency, widen its own audience, or assert its own trust (stimulus.lisp),
;;; and a codelet cannot forge priority, identity or evidence (codelets.lisp).
;;; A poisoned document gains nothing by asking.
;;;
;;; If typed control metadata is wanted later it belongs at the authenticated
;;; ingress adapter, which sees the raw message and can attest to it. A
;;; downstream projection guessing from text is the wrong layer.
;;;
;;; A protection that cannot fire is worse than none: it is a claim in the
;;; source that a reader will believe (gotcha 25, and gotcha 34's rule that a
;;; mechanism must answer the question you think it answers).

(defun %inbox-expired-p (stimulus now)
  (let ((expires (gethash "expires_at" stimulus)))
    (and (numberp expires) (numberp now) (<= expires now))))

(defun %inbox-wrong-partition-p (stimulus agent-id)
  "PROVABLY wrong partition only.

Three positions were tried here and the history matters:

1. Compare only when the stimulus states a partition. Sound in principle, but
   the projection was stamping the target partition onto every stimulus, so
   this compared a value it had just written -- a tautology.
2. Reject whenever the stimulus does not state a matching partition. Correct
   against the tautology, and immediately wrong in practice: the event log
   does not write AGENT_ID at all, so supplying a target partition rejected
   every real event. That regression passed its tests because the fixtures
   were synthetic events built to carry a partition -- the same
   fixtures-shaped-to-pass failure this codebase keeps repeating.
3. This: reject on an explicit mismatch, admit an unstated partition and flag
   it. Same shape as %INBOX-STALE-REVISION-P, and for the same reason --
   unverifiable is not the same as wrong, and treating it as wrong discards
   the entire history.

The real fix is upstream: the event log should stamp the partition, at which
point unstated becomes rare and suspicious rather than universal. Until then
PARTITION_STATUS records which stimuli could actually be checked, so
`unverified` is visible rather than being quietly counted as safe."
  (when agent-id
    (let ((sid (gethash "agent_id" stimulus)))
      (and (stringp sid) (plusp (length sid))
           (not (string= sid agent-id))))))

(defun %inbox-partition-status (stimulus agent-id)
  "verified | legacy-partition-assumed | mismatched | unverified.

LEGACY-PARTITION-ASSUMED is an explicit attestation, not a shrug: the event
predates partition stamping, the projection is proceeding anyway, and it says
so in the record. Distinguishing it from `verified` is the whole point --
once LOG-EVENT stamps every new event, a legacy reading is a statement about
history rather than a permanent blanket excuse, and a live runtime seeing one
is seeing something worth questioning.

The distinction also gives Q2 a rule it can enforce: refuse
legacy-partition-assumed when the runtime is live, accept it when explicitly
replaying history."
  (let ((sid (gethash "agent_id" stimulus)))
    (cond ((not agent-id) "unverified")
          ((not (stringp sid)) "legacy-partition-assumed")
          ((string= sid agent-id) "verified")
          (t "mismatched"))))

(defun %inbox-stale-revision-p (stimulus current-revision)
  "Provably stale only. An UNKNOWN revision (:null, from an event written
before revision stamping) is NOT stale -- it is unverifiable, and treating
unverifiable as stale would reject the entire history. It is admitted and
flagged instead; see %INBOX-REVISION-STATUS."
  (let ((origin (gethash "origin_runtime_revision" stimulus)))
    (and (stringp current-revision) (stringp origin)
         (not (string= origin current-revision)))))

(defun %inbox-revision-status (stimulus current-revision)
  (let ((origin (gethash "origin_runtime_revision" stimulus)))
    (cond ((not (stringp origin)) "unverified")
          ((not (stringp current-revision)) "unverified")
          ((string= origin current-revision) "current")
          (t "stale"))))

(defun %inbox-eligibility (stimulus &key now agent-id current-revision seen)
  "Return NIL when eligible, or a rejection reason string. Ordered so the
cheapest and most structural checks run first."
  (cond ((%inbox-malformed-p stimulus) "malformed")
        ((%inbox-wrong-partition-p stimulus agent-id) "wrong-partition")
        ((and seen (gethash (gethash "stimulus_id" stimulus) seen)) "duplicate")
        ((%inbox-expired-p stimulus now) "expired")
        ((%inbox-stale-revision-p stimulus current-revision) "stale-revision")
        (t nil)))

;;; --- coalescing ----------------------------------------------------------

(defun %inbox-newer-p (a b)
  "Deterministic recency. OBSERVED_AT first; STIMULUS_ID breaks ties so the
result never depends on hash-table iteration order."
  (let ((oa (gethash "observed_at" a)) (ob (gethash "observed_at" b)))
    (cond ((and (numberp oa) (numberp ob) (/= oa ob)) (> oa ob))
          (t (string> (gethash "stimulus_id" a "") (gethash "stimulus_id" b ""))))))

(defun %inbox-coalesce (stimuli)
  "Replace older candidates sharing a coalescing key with the newest.
Barriers never carry a key (enforced in stimulus.lisp), so they cannot be
collapsed here.

Returns (values kept coalesced-records). Records name WHICH stimulus was
superseded and by what -- a bare count said something had been dropped
without saying what, which is exactly the accounting gap the projection is
supposed to close. Superseded is not lost: the source event is untouched and
the stimulus re-projects next pass."
  (let ((by-key (make-hash-table :test #'equal))
        (keyless '())
        (records '()))
    (flet ((record (superseded winner)
             (push (obj "stimulus_id" (gethash "stimulus_id" superseded)
                        "kind" (gethash "kind" superseded)
                        "reason" "coalesced"
                        "superseded_by" (gethash "stimulus_id" winner)
                        "coalescing_key" (gethash "coalescing_key" superseded)
                        "source_event_ids" (gethash "source_event_ids" superseded))
                   records)))
      (dolist (s stimuli)
        (let ((key (gethash "coalescing_key" s)))
          (if (stringp key)
              (let ((existing (gethash key by-key)))
                (cond ((null existing) (setf (gethash key by-key) s))
                      ((%inbox-newer-p s existing)
                       (record existing s)
                       (setf (gethash key by-key) s))
                      (t (record s existing))))
              (push s keyless)))))
    (let ((kept (nreverse keyless)))
      (maphash (lambda (k v) (declare (ignore k)) (push v kept)) by-key)
      (values kept (nreverse records)))))

;;; --- fairness ------------------------------------------------------------

(defun %inbox-age-promoted-sort (stimuli now &optional (ranks *inbox-urgency-rank*))
  "Order candidacy for bounding decisions: urgency first, then age.

Fairness (spec section 10): a continuing concern must not be starved forever
by a stream of low-value novelty. Age promotion is expressed here as *older
sorts earlier within its urgency class*, so when the soft bound sheds items it
sheds the NEWEST low-value ones, not the longest-waiting.

Fairness deliberately does not cross urgency classes -- the spec is explicit
that it never overrides a user's immediate turn."
  (stable-sort
   (copy-list stimuli)
   (lambda (a b)
     (let ((ra (%inbox-urgency-rank a ranks)) (rb (%inbox-urgency-rank b ranks)))
       (cond ((/= ra rb) (< ra rb))
             (t (let ((oa (gethash "observed_at" a)) (ob (gethash "observed_at" b)))
                  (if (and (numberp oa) (numberp ob) (/= oa ob))
                      (< oa ob)                        ; older first
                      (string< (gethash "stimulus_id" a "")
                               (gethash "stimulus_id" b ""))))))))))

(defun %inbox-waiting-cycles (stimulus now)
  "Insistence signal for the attention stage. Not used for bounding here --
exposed so a codelet can weigh how long something has been waiting without
recomputing it."
  (let ((observed (gethash "observed_at" stimulus)))
    (if (and (numberp observed) (numberp now) (> now observed))
        (- now observed)
        0)))

(defun %inbox-authorized-conversation-reply-root (event agent-id)
  "Return the user stimulus durably handled by a published assistant reply.

Three source-defined publication shapes exist: the old AUTO-TURN completion
event (or its single-final fallback), the recursive solicited publication,
and the Q4.5 accepted publication.  Tool-bearing draft segments, private
findings, generic model output, and arbitrary causal journal rows cannot
retire a barrier.  This is replay compatibility for event-derived completion,
not a new way for a model to claim that a stimulus was handled."
  (when (and (hash-table-p event)
             (string= "agent-message" (gethash "type" event "")))
    (let* ((payload (gethash "payload" event))
           (metadata (and (hash-table-p payload)
                          (gethash "metadata" payload)))
           (cause (gethash "caused_by" event))
           (authorization (and (hash-table-p payload)
                               (gethash "authorization_kind" payload)))
           (text (and (hash-table-p payload) (gethash "text" payload)))
           (text-present
             (or (and (stringp text) (plusp (length text)))
                 (eq t (and (hash-table-p payload)
                            (gethash "text_present" payload))))))
      (when (and (hash-table-p payload)
                 (or (null agent-id)
                     (equal agent-id (gethash "agent_id" event))
                     ;; Imported AUTO-TURN replies predate partition stamping.
                     ;; Only that unversioned public shape may rely on the
                     ;; caller's single-agent authority partition; an explicit
                     ;; mismatch or a modern missing partition is refused.
                     (and (null authorization)
                          (let ((partition (gethash "agent_id" event)))
                            (or (null partition) (eq partition :null)))))
                 (or (integerp cause)
                     (and (stringp cause) (plusp (length cause))))
                 (cond
                   ((equal authorization "solicited-publication-candidate")
                    (and (hash-table-p metadata)
                         (string= "q4.5-conversation"
                                  (gethash "source" metadata ""))
                         (member (gethash "publication_validation" metadata)
                                 '("accepted" "removal-only")
                                 :test #'string=)))
                   ((equal authorization "recursive-solicited-reply")
                    (and text-present (hash-table-p metadata)
                         (string= "recursive-mind-v1"
                                  (gethash "source" metadata ""))
                         (let ((id (gethash "authorization_id" payload)))
                           (and (stringp id) (plusp (length id))))))
                   ((null authorization)
                    (and text-present (null metadata)
                         (multiple-value-bind (final present-p)
                             (gethash "final" payload)
                           (or (not present-p) (eq final t)))))))
        (format nil "stimulus:~a" cause)))))

;;; --- projection ----------------------------------------------------------

(defun %inbox-consumed-ids (events &key agent-id consumer)
  "Fold acknowledgement events into the set of consumed stimulus ids.

Acknowledgements are SCOPED. An earlier version accepted any claimed id from
any acknowledgement, which meant an ack could be written BEFORE its stimulus
and suppress it on arrival -- a future user message silenced by a record
written earlier. Demonstrated, not theorised.

Three rules close that:

  causal order   an ack applies only to a stimulus already OBSERVED at the
                 point the ack appears in the log. Acknowledging something
                 that has not happened is not acknowledgement, it is
                 pre-emption, and there is no legitimate producer of it.
  partition      an ack from another agent's partition is ignored.
  consumer       when a consumer is named, only that consumer's acks apply,
                 so one consumer cannot retire another's work.

Ignored acknowledgements are counted rather than silently dropped: an ack
that applies to nothing is either a bug or an attack, and both are worth
seeing."
  (let ((consumed (make-hash-table :test #'equal))
        (observed (make-hash-table :test #'equal))
        (tool-call-roots (make-hash-table :test #'equal))
        (tool-results-by-root (make-hash-table :test #'equal))
        (ignored 0))
    (map nil
         (lambda (event)
           (when (hash-table-p event)
             (let ((type (gethash "type" event)))
               (cond
                 ;; Record what has been seen so far, in log order.
                 ((stimulus-admissible-p type)
                  (let ((id (gethash "id" event)))
                    (when id
                      (setf (gethash (format nil "stimulus:~a" id) observed)
                            type)
                      (when (equal type "tool-result")
                        (let ((root (gethash (gethash "caused_by" event)
                                             tool-call-roots)))
                          (when root
                            (push (format nil "stimulus:~a" id)
                                  (gethash root tool-results-by-root))))))))
                 ;; A direct tool result is associated with an already
                 ;; observed user root only through its durable call event.
                 ;; The call itself is journal evidence, not a stimulus.
                 ((equal "tool-call" type)
                  (let* ((root (format nil "stimulus:~a"
                                       (gethash "caused_by" event)))
                         (id (gethash "id" event)))
                    (when (and id
                               (equal "user-message"
                                      (gethash root observed)))
                      (setf (gethash id tool-call-roots) root))))
                 ;; A canonical authorized reply is durable evidence that its
                 ;; triggering barrier was handled.  Older Q4.5 turns did not
                 ;; yet duplicate that fact into pulse consumption.
                 ((equal "agent-message" type)
                  (let ((root (%inbox-authorized-conversation-reply-root
                               event agent-id)))
                    (when (and root
                               (string= "user-message"
                                        (gethash root observed "")))
                      (setf (gethash root consumed) t)
                      ;; Only tool results that preceded this committed
                      ;; public reply were available to that turn. A later
                      ;; result stays pending until a later publication.
                      (dolist (result-id
                               (gethash root tool-results-by-root))
                        (setf (gethash result-id consumed) t)))))
                 ((member type *inbox-consumption-event-types* :test #'equal)
                  (let* ((payload (let ((p (gethash "payload" event)))
                                    (if (hash-table-p p) p (obj))))
                         (ack-agent (gethash "agent_id" payload))
                         (ack-consumer (gethash "consumer" payload))
                         (ids (gethash "stimulus_ids" payload)))
                    ;; FAIL CLOSED. The previous form rejected only a
                    ;; PRESENT-and-mismatching field, so an acknowledgement
                    ;; carrying neither agent_id nor consumer was accepted
                    ;; even under a context requiring both -- an unsigned ack
                    ;; could retire anyone's work. Since acknowledgement is
                    ;; the mechanism that makes stimuli disappear, absent
                    ;; scope must be refused rather than waved through, and
                    ;; there is no historical Q1 acknowledgement needing
                    ;; compatibility.
                    ;; A commit with no consumed ids (for example Q3's honest
                    ;; abstention on a user barrier) is a terminal journal
                    ;; fact, not a malformed acknowledgement.
                    (unless (zerop (length (cond ((vectorp ids) ids)
                                                  ((listp ids) ids)
                                                  (t '()))))
                      (if (or (and agent-id (not (and (stringp ack-agent)
                                                    (string= ack-agent agent-id))))
                            (and consumer (not (and (stringp ack-consumer)
                                                    (string= ack-consumer consumer))))
                            ;; A disposition must be stated and recognised:
                            ;; "I am done with this" and "this expired" are
                            ;; different facts, and an ack that will not say
                            ;; which is not an acknowledgement.
                            (not (member (gethash "disposition" payload)
                                         *inbox-ack-dispositions* :test #'equal))
                            ;; Bounded. One event naming an unbounded id
                            ;; vector is a denial-of-attention primitive.
                            (> (length (cond ((vectorp ids) ids)
                                             ((listp ids) (coerce ids 'vector))
                                             (t (vector))))
                               *inbox-ack-max-ids*))
                          (incf ignored)
                          (map nil
                               (lambda (id)
                                 (if (gethash id observed)
                                     (setf (gethash id consumed) t)
                                     ;; Not yet observed: cannot be acknowledged.
                                     (incf ignored)))
                               (cond ((vectorp ids) ids)
                                     ((listp ids) (coerce ids 'vector))
                                     (t (vector))))))))))))
         events)
    (values consumed ignored)))

(defun %inbox-watermark (events accounted &key kind-map discriminators agent-id)
  "Highest event id below which every event is ACCOUNTED FOR.

An event is accounted for when it is not an active candidate: it never
projected to a stimulus (journal-only, or payload-discriminated to journal),
it was refused by Stage A, or its stimulus was consumed. Only outstanding
work holds the watermark back.

The previous version asked a different question -- whether the base event
TYPE was allowlisted -- which is not the same question candidacy asks. It
ignored payload discrimination, Stage A eligibility, partition, revision and
expiry, so a `schedule-fired(mode=notify)` that correctly produced NO stimulus
still pinned the watermark forever, directly contradicting the reason it was
journalized. Rejected, foreign, stale and expired stimuli did the same. A
watermark computed from a different admission model than the inbox is not a
watermark for that inbox.

ACCOUNTED is the set of stimulus ids the caller has already determined are
not outstanding -- consumed plus terminally dispositioned."
  (let ((watermark 0))
    (block scan
      (map nil
           (lambda (event)
             (let* ((id (and (hash-table-p event) (gethash "id" event)))
                    (s (and (hash-table-p event)
                            (stimulus-from-event event :agent-id agent-id
                                                       :kind-map kind-map
                                                       :discriminators discriminators))))
               ;; No stimulus at all: nothing to wait for.
               (when (and s (not (gethash (gethash "stimulus_id" s) accounted)))
                 (return-from scan))
               (when (numberp id) (setf watermark id))))
           events))
    watermark))

(defun inbox-project (events &key context now agent-id current-revision
                                  soft-bound hard-bound
                                  observed-highest-event-id)
  "Project EVENTS into bounded active candidacy.

EVENTS is a sequence of stored event hash tables, oldest first. Pure: no
mutation of EVENTS, no I/O, no clock read, and no global policy lookup --
everything that can vary between runs arrives in CONTEXT.

CONTEXT is a projection context. The individual keywords are a convenience
that builds one; there is a single code path either way, so a caller can
never half-supply a composition.

OBSERVED-HIGHEST-EVENT-ID may be supplied by a source-bound indexed reader
when neutral journal gaps have been collapsed. It must not understate any
provided row. The event ledger remains the authority for that scalar.

Stimuli already acknowledged by a consumption event are excluded from active
candidacy and counted under `consumed`. They are not rejections -- they were
handled, which is the ordinary end of a stimulus's life."
  (let* ((ctx (or context
                  (make-projection-context
                   :now now :agent-id agent-id :runtime-revision current-revision
                   :soft-bound (or soft-bound *inbox-soft-bound*)
                   :hard-bound (or hard-bound *inbox-hard-bound*))))
         (now (let ((v (gethash "now" ctx))) (if (eq v :null) nil v)))
         (agent-id (let ((v (gethash "agent_id" ctx))) (if (eq v :null) nil v)))
         (current-revision (let ((v (gethash "runtime_revision" ctx)))
                             (if (eq v :null) nil v)))
         (soft-bound (gethash "soft" (gethash "bounds" ctx)))
         (hard-bound (gethash "hard" (gethash "bounds" ctx)))
         ;; The captured admission policy. Passing these down is what makes
         ;; pinning real: without it the context carried a kind map that no
         ;; code path read, and the discriminators were consulted from the
         ;; live global -- so a pinned replay reinterpreted events by
         ;; today's rules while reporting yesterday's hash.
         (kind-map (or (gethash "kind_map" ctx) *stimulus-kind-map*))
         (discriminators (or (gethash "discriminators" ctx) *stimulus-discriminators*))
         (urgency-ranks (or (gethash "urgency_ranks" ctx) *inbox-urgency-rank*))
         (consumer (let ((c (and (projection-context-p ctx) (gethash "consumer" ctx))))
                     (if (and (stringp c) (plusp (length c))) c nil)))
         ;; Highest event id OBSERVED, distinct from the consumption
         ;; watermark. They answer different questions -- "how far has the
         ;; log been read" versus "how far has it been dealt with" -- and
         ;; conflating them is what made the previous field meaningless.
         (highest-event-id
           (let ((m 0))
             (map nil (lambda (e)
                        (let ((id (and (hash-table-p e) (gethash "id" e))))
                          (when (and (numberp id) (> id m)) (setf m id))))
                  events)
             (when observed-highest-event-id
               (unless (and (integerp observed-highest-event-id)
                            (<= m observed-highest-event-id))
                 (error "Indexed inbox highest event ID understates supplied rows"))
               (setf m observed-highest-event-id))
             m))
         (seen (make-hash-table :test #'equal))
         (admitted '()) (rejected '()) (deferred '())
         (ignored-acks 0)
         (consumed-count 0) (coalesced '())
         (consumed-ids (multiple-value-bind (ids ignored)
                           (%inbox-consumed-ids events :agent-id agent-id
                                                       :consumer consumer)
                         (setf ignored-acks ignored)
                         ids))
         (watermark nil))

    ;; Pass 1 -- admission and Stage A eligibility.
    (map nil
         (lambda (event)
           (let ((s (stimulus-from-event event :agent-id agent-id
                                              :kind-map kind-map
                                              :discriminators discriminators)))
             (when s
               (let ((sid (gethash "stimulus_id" s)))
                 (cond
                   ;; Already handled. Not an active candidate and not a
                   ;; failure; its life ended normally.
                   ((gethash sid consumed-ids)
                    (setf (gethash sid seen) t)
                    (incf consumed-count))
                   (t
                    (let ((reason (%inbox-eligibility
                                   s :now now :agent-id agent-id
                                   :current-revision current-revision :seen seen)))
                      (setf (gethash sid seen) t)
                      (if reason
                          (push (obj "stimulus_id" sid
                                     "kind" (gethash "kind" s)
                                     "reason" reason
                                     "source_event_ids" (gethash "source_event_ids" s))
                                rejected)
                          (progn
                            (setf (gethash "revision_status" s)
                                  (%inbox-revision-status s current-revision))
                            (setf (gethash "partition_status" s)
                                  (%inbox-partition-status s agent-id))
                            (setf (gethash "waiting_for" s) (%inbox-waiting-cycles s now))
                            (push s admitted))))))))))
         events)
    (setf admitted (nreverse admitted))

    ;; The watermark folds the SAME classified stream candidacy did. Anything
    ;; consumed or terminally dispositioned is accounted for; only admitted
    ;; stimuli remain outstanding. Computing it from a separate admission
    ;; model is what let journal-discriminated events pin it forever.
    (let ((accounted (make-hash-table :test #'equal)))
      (maphash (lambda (k v) (declare (ignore v)) (setf (gethash k accounted) t))
               consumed-ids)
      (dolist (r rejected)
        (setf (gethash (gethash "stimulus_id" r) accounted) t))
      (setf watermark (%inbox-watermark events accounted
                                        :kind-map kind-map
                                        :discriminators discriminators
                                        :agent-id agent-id)))

    ;; Pass 2 -- coalesce replaceable candidates.
    (multiple-value-bind (kept records) (%inbox-coalesce admitted)
      (setf admitted kept coalesced records))

    ;; Pass 3 -- bounds. Barriers are partitioned out first and are never
    ;; refused; only non-barriers are subject to the bounds.
    (let* ((ordered (%inbox-age-promoted-sort admitted now urgency-ranks))
           (barriers (remove-if-not (lambda (s) (gethash "barrier" s)) ordered))
           (others (remove-if (lambda (s) (gethash "barrier" s)) ordered))
           (barrier-count (length barriers))
           (room (max 0 (- hard-bound barrier-count)))
           (keep-others '()))
      ;; Hard bound: refuse non-barrier candidacy beyond the remaining room.
      (loop for s in others
            for i from 0
            do (if (< i room)
                   (push s keep-others)
                   (push (obj "stimulus_id" (gethash "stimulus_id" s)
                              "kind" (gethash "kind" s)
                              "reason" "hard-bound"
                              "source_event_ids" (gethash "source_event_ids" s))
                         rejected)))
      (setf keep-others (nreverse keep-others))
      ;; Soft bound: defer the lowest-value, newest non-barriers. Deferral is
      ;; not rejection -- these remain re-projectable and simply are not
      ;; competing for attention this pass.
      (let ((total (+ barrier-count (length keep-others))))
        (when (> total soft-bound)
          (let* ((excess (- total soft-bound))
                 (shed (last keep-others (min excess (length keep-others)))))
            (dolist (s shed)
              (push (obj "stimulus_id" (gethash "stimulus_id" s)
                         "kind" (gethash "kind" s)
                         "reason" "soft-bound-deferred"
                         "source_event_ids" (gethash "source_event_ids" s))
                    deferred))
            (setf keep-others (butlast keep-others (length shed))))))

      (let* ((final (append barriers keep-others))
             ;; Barrier overflow: barriers alone exceed the hard bound. They
             ;; are all still admitted -- the projection reports degraded so
             ;; the runtime pauses new cognitive effects instead of dropping
             ;; one. Visible beats silent.
             (degraded (> barrier-count hard-bound)))
        (obj "schema_version" *inbox-schema-version*
             "evaluated_at" (gethash "now" ctx)
             "agent_id" (gethash "agent_id" ctx)
             "consumer" (gethash "consumer" ctx)
             "watermark" watermark
             "highest_event_id" highest-event-id
             "composition_hash" (projection-context-hash ctx)
             "admitted" (coerce final 'vector)
             "admitted_count" (length final)
             "barrier_count" barrier-count
             "consumed_count" consumed-count
             "rejected" (coerce (nreverse rejected) 'vector)
             "deferred" (coerce (nreverse deferred) 'vector)
             "ignored_acknowledgements" ignored-acks
             "coalesced_count" (length coalesced)
             "coalesced" (coerce coalesced 'vector)
             "degraded" (if degraded t nil)
             "degraded_reason" (if degraded "barrier-overflow" :null)
             "bounds" (obj "soft" soft-bound "hard" hard-bound))))))

(defun inbox-report (inbox)
  "Operator-facing summary. Content-free: counts and reasons only, never
stimulus payloads, so it is safe to surface in a dashboard."
  (let ((by-reason (obj)))
    (flet ((tally (v)
             (map nil (lambda (r)
                        (let ((reason (gethash "reason" r)))
                          (setf (gethash reason by-reason)
                                (1+ (gethash reason by-reason 0)))))
                  v)))
      (tally (gethash "rejected" inbox))
      (tally (gethash "deferred" inbox)))
    (obj "schema_version" (gethash "schema_version" inbox)
         "watermark" (gethash "watermark" inbox)
         "admitted" (gethash "admitted_count" inbox)
         "barriers" (gethash "barrier_count" inbox)
         "coalesced" (gethash "coalesced_count" inbox)
         "degraded" (gethash "degraded" inbox)
         "degraded_reason" (gethash "degraded_reason" inbox)
         "excluded_by_reason" by-reason)))
