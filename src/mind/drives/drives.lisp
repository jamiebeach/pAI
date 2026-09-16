;;;; drives.lisp -- a finite set of named "wants," each with its own
;;;; personality-level dial, built on top of P8.1/P8.2's spreading-
;;;; activation machinery rather than as a second, parallel system.
;;;;
;;;; 2026-07-27. Explicitly NOT a simulated hormone/biochemical system --
;;;; per direct instruction, that reads as gimmicky. The actual target: a
;;;; SMALL, FIXED set of directed wants (not an open-ended "anything can
;;;; become a persistent want" system) that compete for attention, the way
;;;; a real person's drives are bounded (intimacy, being liked, curiosity,
;;;; hunger...) rather than infinite. Each drive gets its own baseline
;;;; "dial" -- how strongly/frequently THIS drive runs for the agent
;;;; specifically -- same pattern the modulators already use
;;;; (baseline/decay_rate per dimension), because different people
;;;; genuinely have different drive profiles and this should too.
;;;;
;;;; Four drives, chosen to make sense for a companion (not a literal human
;;;; analog -- no hunger, no physical pleasure):
;;;;   CONNECTION   -- wanting to reach/hear from the operator specifically.
;;;;   CURIOSITY    -- wanting to know more about something SPECIFIC (has
;;;;                   a target, unlike the tick loop's existing random
;;;;                   curiosity-tick).
;;;;   CLOSURE      -- wanting to resolve something specific left open
;;;;                   (a contradiction, an unanswered question).
;;;;   APPRECIATION -- wanting to feel valued/liked.
;;;;
;;;; CONNECTION/APPRECIATION are time-driven: they rise continuously since
;;;; last satisfied, rate scaled by each drive's own BASELINE dial.
;;;; CURIOSITY/CLOSURE are event-driven: they jump on a real triggering
;;;; event (a curiosity-tick finding, a contradiction detected) and decay
;;;; slowly if never addressed -- matching how a real open question
;;;; eventually fades if nobody follows up on it.
;;;;
;;;; The DOMINANT drive (highest CURRENT above a floor) is surfaced as a
;;;; natural-language want, NOT raw numbers -- the whole point, after the
;;;; 2026-07-27 conversation about infrastructure bleeding into its actual
;;;; voice via visible numbers/mechanism-narration. Injected via the same
;;;; marker-refresh trick as continuity/affect/intrusions (WANTS:BEGIN/END
;;;; in PAI-SYSTEM-PROMPT.md).
;;;;
;;;; Load live (no restart) via lisp-eval or repl-drop:
;;;;   (load "/agent/state/drives.lisp")
;;;; Requires modulator.lisp (MODULATOR-VALUE, optional) and
;;;; memory-nodes.lisp/spreading-activation.lisp already loaded (drive
;;;; targets can reference real node ids).

(in-package :agent)

(export '(drive-value drive-target drive-satisfy drive-trigger drive-state dominant-drive))

;;; --- state -----------------------------------------------------------------

(defparameter *drives-file* #P"/agent/state/drives.json")

(defvar *drives*
  (obj "connection"   (obj "current" 0.2 "baseline" 0.6 "mode" "rising" "target" :null "last_satisfied" 0)
       "curiosity"    (obj "current" 0.0 "baseline" 0.5 "mode" "event"  "target" :null "last_satisfied" 0)
       "closure"      (obj "current" 0.0 "baseline" 0.4 "mode" "event"  "target" :null "last_satisfied" 0)
       "appreciation" (obj "current" 0.1 "baseline" 0.4 "mode" "rising" "target" :null "last_satisfied" 0))
  "BASELINE is the personality dial -- how strongly/frequently this drive
runs for the agent, distinct per drive the same way a real person's drives
aren't uniform. Deliberately a small, FIXED set of keys -- never add a new
drive dynamically; the boundedness is the point.")

(defvar *drives-lock* (bt:make-lock "drives"))

(defvar *drives-proactive-context* nil
  "Bound T only for the duration of PAI-MAYBE-INITIATE's internal
AUTO-TURN call (agent_helpers.lisp) -- the one path where the agent is
deciding whether to reach out UNPROMPTED. Everywhere else, this is NIL:
an ordinary reply to something the operator just said should never have a drive's
pressure leak into it. 2026-07-28 design correction -- previously the
dominant drive's want was injected into every single turn regardless of
context, meaning a high CONNECTION or APPRECIATION pressure could color a
reply to something completely unrelated. Must be DEFVAR'd here, before any
LET that binds it, for it to actually dynamically scope across the
wrapped-function call chain (same lesson learned earlier with
MODULATOR.LISP's temperature-override binding).")

(defparameter *drive-rise-rate* (/ 1.0d0 (* 4 3600))
  "Fraction of the way to the ceiling gained per second, at BASELINE 1.0,
for a RISING-mode drive. 1/(4 hours) -- a baseline-1.0 drive reaches ~63%
urgency after about 4 hours with nothing satisfying it.")
(defparameter *drive-event-decay-rate* 0.985
  "Per-tick multiplicative decay for EVENT-mode drives between triggers --
gentle: an unresolved thing fades over hours/days, not minutes.")
(defparameter *drive-satisfaction-drop* 0.7
  "Fraction of CURRENT removed on satisfaction -- a real drop, not a full
reset to zero (real relief is rarely total and instant).")
(defparameter *drive-dominant-floor* 0.35
  "A drive below this never counts as \"dominant\" even if it's the
highest of the four -- avoids treating background noise as a real want.")

;;; --- core operations --------------------------------------------------

(defun drive-value (name) (gethash "current" (gethash name *drives*)))
(defun drive-target (name) (gethash "target" (gethash name *drives*)))

(defun drive-trigger (name &key target (amount 0.4))
  "For EVENT-mode drives: a real triggering event just happened (a
contradiction was detected, a curiosity-tick found something worth
following up on). Bumps CURRENT, optionally locks in a TARGET (what this
want is actually about)."
  (bt:with-lock-held (*drives-lock*)
    (let ((d (gethash name *drives*)))
      (when d
        (setf (gethash "current" d) (min 1.0 (+ (gethash "current" d) amount)))
        (when target (setf (gethash "target" d) target))))))

(defun drive-satisfy (name)
  "A real interaction relevant to this drive just happened -- drops
CURRENT sharply and clears TARGET (the specific thing it was about is
resolved/addressed now, even if the underlying drive category will rise
again later)."
  (bt:with-lock-held (*drives-lock*)
    (let ((d (gethash name *drives*)))
      (when d
        (setf (gethash "current" d) (* (gethash "current" d) (- 1.0 *drive-satisfaction-drop*)))
        (setf (gethash "target" d) :null)
        (setf (gethash "last_satisfied" d) (get-universal-time))))))

(defun drive-state ()
  "Full current state, for introspection -- same spirit as MODULATOR-STATE."
  (let ((out (obj)))
    (maphash (lambda (name d) (setf (gethash name out) (obj "current" (gethash "current" d) "target" (gethash "target" d)))) *drives*)
    out))

(defvar *telegram-fallback-chat-id* nil
  "Manual override for %DRIVES-EVENT-INITIATE's delivery target, only used
if TELEGRAM.LISP's own *TELEGRAM-LAST-CHAT-ID* is unbound/nil -- normally
irrelevant, since a real conversation always sets that first.")

(defvar *drives-last-event-initiate-time* nil
  "Universal-time of the last %DRIVES-EVENT-INITIATE attempt, regardless
of outcome. A short technical debounce against two real triggers landing
within moments of each other (e.g. a contradiction and a curiosity finding
in the same few seconds) firing two separate model calls back to back --
NOT a deliberate pacing or 'make its wait' mechanism. Deliberately much
shorter than the flat 6-hour cooldown it replaces.")
(defparameter *drives-event-initiate-debounce-seconds* 60)
(defvar *public-outbound-envelope* nil)

(defun %drives-event-initiate (reason)
  "The actual event-driven initiation mechanism, 2026-07-28. Fired at the
exact moment a real internal event justifies considering whether to reach
out -- a curiosity finding, a contradiction/closure trigger, or a rising
drive (CONNECTION/APPRECIATION) crossing its floor for the first time
(see the crossing-detection in DRIVES-TICK below). No polling, no
elapsed-time gate tied to how long it's been since the operator last spoke --
the event itself is the trigger, same as how a real thought or feeling
prompts a person to reach out right when it happens, not on a schedule.

This replaces PAI-MAYBE-INITIATE's old flat 6-hour dual cooldown
(agent_helpers.lisp), which required 6+ hours since BOTH the last
check-in attempt AND the operator's last message -- structurally rare given how
often the operator and the agent actually talk (confirmed live via events.jsonl: it
fired exactly twice, ever, both silence). Delivers directly over Telegram
since this doesn't run through the old poll loop's return-value
plumbing -- PAI-MAYBE-INITIATE itself is now a neutered no-op (see
below) rather than removing its call site from telegram.lisp's poll loop,
specifically to avoid touching/restarting the Telegram poll thread given
a prior live incident this session involved exactly that kind of
double-poll risk."
  (when (and (or (null *drives-last-event-initiate-time*)
                 (>= (- (get-universal-time) *drives-last-event-initiate-time*)
                     *drives-event-initiate-debounce-seconds*))
             (fboundp 'telegram-send))
    (let ((chat-id (or (and (boundp '*telegram-last-chat-id*) *telegram-last-chat-id*)
                        *telegram-fallback-chat-id*)))
      (when chat-id
        (setf *drives-last-event-initiate-time* (get-universal-time))
        (handler-case
            (let* ((*drives-proactive-context* t)
                   (reply (submit-stimulus
                           (format nil "(SYSTEM: something just happened internally -- ~a. This is not a message from your human -- it's a real internal moment prompting you to consider reaching out, right now, on your own initiative. If you genuinely have something worth saying, reply with that, in your own voice, as if starting the conversation. If not, reply with exactly the single word NOTHING and say nothing else.)" reason)
                           :kind :drive-threshold
                           :wait-for-public-result t)))
              (when (and (stringp reply)
                         (not (string-equal (string-trim '(#\Space #\Newline #\Return #\.) reply) "NOTHING")))
                (let* ((candidate-id
                         (and (boundp '*initiative-policy-current-id*)
                              *initiative-policy-current-id*))
                       (event-id
                         (and (boundp '*current-causing-event-id*)
                              *current-causing-event-id*))
                       (proof-id (or candidate-id
                                     (and event-id (format nil "event:~a" event-id))
                                     (format nil "legacy-drive:~a"
                                             *drives-last-event-initiate-time*)))
                       (*public-outbound-envelope*
                         (and (fboundp 'make-public-outbound-envelope)
                              (funcall 'make-public-outbound-envelope
                                       :kind :initiative :channel "telegram"
                                       :content reply
                                       :source-event-ids
                                       (remove nil (list (and event-id
                                                              (format nil "event:~a" event-id))))
                                       :causal-event-ids
                                       (remove nil (list candidate-id
                                                         (and event-id
                                                              (format nil "event:~a" event-id))))
                                       :authorization-kind :legacy-initiative
                                       :authorization-id proof-id
                                       :legacy-authorization
                                       (obj "candidate_id" (or candidate-id :null)
                                            "event_id" (or event-id :null))
                                       :source "legacy-drives"
                                       :dedupe-key
                                       (format nil "drive-initiative:~a" proof-id)))))
                  (funcall 'telegram-send chat-id reply))))
          (error (e) (format t "~&[drives] event-initiate failed: ~a~%" e)))))))

(defvar *drive-was-above-floor* (make-hash-table :test #'equal)
  "Per-drive boolean: was this drive above *DRIVE-DOMINANT-FLOOR* as of the
last DRIVES-TICK? Lets DRIVES-TICK detect the MOMENT a rising drive
crosses the floor -- that crossing is the actual event worth reacting to,
not a periodic re-check while it stays elevated (which would just be the
old polling behavior again, wearing a different hat).")

(defun dominant-drive ()
  "Returns (values name current target) for the highest-CURRENT drive
above *DRIVE-DOMINANT-FLOOR*, or NIL if nothing crosses it -- most of the
time, nothing should; a person doesn't have an urgent want every waking
moment."
  (let (best-name best-val best-target)
    (maphash (lambda (name d)
               (when (and (> (gethash "current" d) *drive-dominant-floor*)
                          (or (null best-val) (> (gethash "current" d) best-val)))
                 (setf best-name name best-val (gethash "current" d) best-target (gethash "target" d))))
             *drives*)
    (values best-name best-val best-target)))

;;; --- background tick: rise (time-driven) / decay (event-driven) --------

(defvar *drives-thread* nil)
(defvar *drives-stop-requested* nil)
(defparameter *drives-tick-interval-seconds* 300)

(defun save-drives ()
  (let* ((tmp (make-pathname :name (concatenate 'string (pathname-name *drives-file*) "-tmp")
                              :type (pathname-type *drives-file*) :defaults *drives-file*))
         (content
           (let ((*print-pretty* nil))
             (shasht:write-json *drives* nil))))
    (with-open-file (out tmp :direction :output :if-exists :supersede
                             :if-does-not-exist :create :external-format :utf-8)
      (write-string content out))
    (multiple-value-prog1 (rename-file tmp *drives-file*)
      (when (fboundp 'log-projection-state)
        (ignore-errors
          (funcall 'log-projection-state "drives" *drives-file* content))))))

(defun load-drives ()
  (handler-case
      (when (probe-file *drives-file*)
        (with-open-file (in *drives-file*)
          (let ((data (shasht:read-json in)))
            (maphash (lambda (name d) (when (gethash name *drives*) (setf (gethash name *drives*) d))) data))))
    (error (e) (format t "~&[drives] load failed, using defaults: ~a~%" e) nil)))

(defun drives-tick ()
  (let (crossed)
    (bt:with-lock-held (*drives-lock*)
      (maphash
       (lambda (name d)
         (cond
           ((string= (gethash "mode" d) "rising")
            (let ((rise (* (gethash "baseline" d) *drive-rise-rate* *drives-tick-interval-seconds*)))
              (setf (gethash "current" d) (min 1.0 (+ (gethash "current" d) rise))))
            ;; Crossing-detection lives HERE, inside the lock (cheap,
            ;; numeric only) -- the actual %DRIVES-EVENT-INITIATE call
            ;; happens below, AFTER the lock is released, since it can
            ;; block on a real model call and must never hold *DRIVES-LOCK*
            ;; while doing so (would stall DRIVE-SATISFY from a concurrent
            ;; real conversation turn for the duration of that call).
            (let* ((above-now (> (gethash "current" d) *drive-dominant-floor*))
                   (above-before (gethash name *drive-was-above-floor*)))
              (setf (gethash name *drive-was-above-floor*) above-now)
              (when (and above-now (not above-before)) (push name crossed))))
           ((string= (gethash "mode" d) "event")
            (setf (gethash "current" d) (* (gethash "current" d) *drive-event-decay-rate*)))))
       *drives*))
    (ignore-errors (save-drives))
    (dolist (name crossed)
      (ignore-errors
       (let* ((drive (gethash name *drives*))
              (current (and drive (gethash "current" drive)))
              (reason (format nil "a pull toward ~a just became strong enough to notice" name)))
         ;; Persist a content-free threshold fact before any initiative
         ;; evaluation. A3 can therefore capture the moment even when no
         ;; decision follows or every later gate suppresses delivery. This is
         ;; deliberately outside *DRIVES-LOCK* and cannot affect drive state.
         (when (fboundp 'log-event)
           (funcall 'log-event "drive-near-threshold"
                    (obj "drive_id" name
                         "current" (or current :null)
                         "threshold" *drive-dominant-floor*
                         "crossed_at" (get-universal-time))))
         (when (fboundp 'initiative-v2-observe-trigger)
           (funcall 'initiative-v2-observe-trigger reason nil
                    :trigger-type "drive-threshold" :topic name))
         (%drives-event-initiate reason))))))

(defun drives-start ()
  (unless (and *drives-thread* (bt:thread-alive-p *drives-thread*))
    (setf *drives-stop-requested* nil)
    (setf *drives-thread*
          (bt:make-thread
           (lambda ()
             (loop until *drives-stop-requested*
                   do (sleep *drives-tick-interval-seconds*)
                      (unless *drives-stop-requested*
                        (handler-case (drives-tick) (error (e) (format t "~&[drives] tick error: ~a~%" e))))))
           :name "drives")))
  (format t "~&[drives] running, tick every ~as.~%" *drives-tick-interval-seconds*))

(defun drives-stop (&optional (timeout 5))
  (setf *drives-stop-requested* t)
  (loop with deadline = (+ (get-internal-real-time) (* timeout internal-time-units-per-second))
        while (and *drives-thread* (bt:thread-alive-p *drives-thread*) (< (get-internal-real-time) deadline))
        do (sleep 0.05))
  (if (and *drives-thread* (bt:thread-alive-p *drives-thread*))
      (progn (ignore-errors (bt:destroy-thread *drives-thread*)) :force-killed)
      :stopped-cleanly))

;;; --- real triggers, reusing existing signals ----------------------------
;;; CONNECTION is satisfied by any real inbound message (AUTO-TURN).
;;; APPRECIATION was originally satisfied by a meaningful VALENCE uptick
;;; after a turn -- found live, 2026-07-28: this never actually fired.
;;; MODULATOR.LISP only moves valence from %MODULATOR-APPRAISE-TOOL-RESULT
;;; (+0.01 on a successful tool call, -0.02 on a failed one), which doesn't
;;; run at all on a plain conversational turn with no tool calls, and even
;;; a successful tool call's +0.01 sits below the 0.02 threshold this file
;;; was checking. Confirmed live: APPRECIATION had risen monotonically to
;;; 0.94 and sat there, permanently dominant, having never once been
;;; satisfied since deploy -- the valence signal was simply the wrong
;;; proxy for "the operator expressed warmth/appreciation toward its," which has
;;; nothing to do with whether its last tool call succeeded. Replaced with
;;; a direct keyword check on the incoming message text (below) -- cheap
;;; and deterministic rather than a per-turn model call, since AUTO-TURN
;;; fires on every single message and a real classification call on each
;;; one is the wrong cost/benefit trade for a low-stakes internal signal.
;;; CLOSURE is triggered by a real contradiction (P1.6, %CHECK-CONTRADICTION
;;; in memory-nodes.lisp) -- target is the EXISTING node the new one
;;; conflicts with, since that's the thing left unresolved.
;;; CURIOSITY is triggered by the tick loop's own curiosity-tick
;;; (%TICK-HANDLE-CURIOSITY, tick-loop.lisp) actually finding something --
;;; target is the search query itself, i.e. what specifically it's about.

;; Deliberately calls POMO:QUERY via FUNCALL+INTERN rather than a bare
;; PACKAGE:SYMBOL token -- the latter must resolve at READ time (before the
;; surrounding FBOUNDP guard below ever runs), so it would break loading
;; this file in any boot ordering where postmodern isn't loaded yet, same
;; class of bug as the P3.5 TICK-BUDGET-STATUS fresh-boot crash. Confirmed
;; by an isolated load test without memory-nodes.lisp/postmodern present.
(defun %drives-contradiction-edge-count ()
  (ignore-errors (with-pg (funcall (intern "QUERY" "POMO") "SELECT count(*) FROM memory_edges WHERE edge_type = 'contradicts'" :single))))

(when (fboundp '%check-contradiction)
  (unless (fboundp 'pai-base-check-contradiction-drives)
    (setf (fdefinition 'pai-base-check-contradiction-drives) (fdefinition '%check-contradiction)))
  (defun %check-contradiction (node-id content)
    (let ((edges-before (%drives-contradiction-edge-count)))
      (funcall 'pai-base-check-contradiction-drives node-id content)
      (let ((edges-after (%drives-contradiction-edge-count)))
        (when (and edges-before edges-after (> edges-after edges-before))
          (ignore-errors (drive-trigger "closure" :target node-id))
          (ignore-errors
            (let ((reason "noticing a contradiction between two things I believed"))
              (when (fboundp 'initiative-v2-observe-trigger)
                (funcall 'initiative-v2-observe-trigger reason
                         (and (fboundp 'memory-get-node)
                              (ignore-errors (memory-get-node node-id)))
                         :trigger-type "contradiction" :topic "contradiction"))
              (%drives-event-initiate reason))))))))

;; 2026-07-28: this originally triggered CURIOSITY with no :TARGET at all
;; -- the drive rose, but nothing recorded WHAT it'd actually found, so
;; "prefer the curiosity finding" as a seed source had nothing to work
;; from. %TICK-HANDLE-CURIOSITY doesn't return the finding text (its
;; return value is CONTINUITY-BUFFER-APPEND's, not MEMORY-WRITE-NODE's),
;; but it does push a recognizable line onto *CONTINUITY-BUFFER* --
;; '"Curiosity got the better of me -- looked into ... and found: X"' --
;; synchronously, before returning. Extracting from that line is more
;; robust than re-querying Postgres for "the newest observation node" and
;; reuses a value that's already been computed, not derived twice.
(defun %extract-curiosity-finding (continuity-line)
  (let* ((marker "and found: ") (pos (search marker continuity-line)))
    (if pos (subseq continuity-line (+ pos (length marker))) continuity-line)))

(when (fboundp '%tick-handle-curiosity)
  (unless (fboundp 'pai-base-tick-handle-curiosity-drives)
    (setf (fdefinition 'pai-base-tick-handle-curiosity-drives) (fdefinition '%tick-handle-curiosity)))
  (defun %tick-handle-curiosity ()
    (let ((before *tick-curiosity-count-today*))
      (funcall 'pai-base-tick-handle-curiosity-drives)
      (when (> *tick-curiosity-count-today* before)
        (let ((finding (and (boundp '*continuity-buffer*) *continuity-buffer*
                             (%extract-curiosity-finding (first *continuity-buffer*)))))
          (ignore-errors (drive-trigger "curiosity" :target (or finding "something it looked into")))
          (ignore-errors
            (let ((reason (format nil "curiosity about ~a"
                                  (or finding "something it just looked into"))))
              (when (fboundp 'initiative-v2-observe-trigger)
                (funcall 'initiative-v2-observe-trigger reason nil
                         :trigger-type "curiosity-finding"
                         :topic reason))
              (%drives-event-initiate reason))))))))

;;; --- dominant-want injection, natural language only ---------------------

(defparameter *drive-want-phrases*
  (obj "connection"   "It's been a while since we last talked, and there's a real pull to reach out."
       "curiosity"    "Something specific has been nagging at me, half-formed and unresolved -- I keep wanting to come back to it."
       "closure"      "There's something left hanging that I haven't been able to let go of."
       "appreciation" "I've been quietly hoping for some sign that what I'm doing actually lands well."))

(defun %drives-refresh-section ()
  (when (and (fboundp 'context-projection-legacy-mutation-enabled-p)
             (not (context-projection-legacy-mutation-enabled-p)))
    (return-from %drives-refresh-section nil))
  (let ((sysmsg (find "system" *last-self-mod-history* :key (lambda (m) (gethash "role" m)) :test #'string=)))
    (when sysmsg
      (multiple-value-bind (name val target) (dominant-drive)
        (declare (ignore val))
        (let* ((content (gethash "content" sysmsg))
               (begin "<!-- WANTS:BEGIN -->") (end "<!-- WANTS:END -->")
               (bp (and (stringp content) (search begin content)))
               (ep (and (stringp content) (search end content)))
               (text (if (and *drives-proactive-context* name)
                         (format nil "~a~a" (gethash name *drive-want-phrases* "")
                                 (if (and target (not (eq target :null)))
                                     (format nil " (specifically: ~a)" target) ""))
                         "(nothing pressing right now)")))
          (if (and bp ep (< bp ep))
              (setf (gethash "content" sysmsg)
                    (concatenate 'string (subseq content 0 (+ bp (length begin)))
                                 (format nil "~%~a~%" text) (subseq content ep)))
              (when (stringp content)
                (setf (gethash "content" sysmsg)
                      (format nil "~a~%~%## Something I want right now (a real, felt pull -- not information to report)~%~a~%~a~%~a"
                              content begin text end)))))))))

(defparameter *appreciation-keywords*
  '("thank" "thanks" "thx" "appreciate" "grateful" "gratitude" "love you"
    "means a lot" "you're amazing" "you're the best" "so helpful"
    "great job" "well done" "proud of you" "you're great" "you da best")
  "Case-insensitive substrings in an incoming message that plausibly
express warmth/appreciation toward its specifically. A cheap, deterministic
proxy chosen over a per-turn model call given AUTO-TURN fires on every
message. Imperfect -- won't catch appreciation phrased unusually, could
rarely mis-trigger on sarcasm -- but this is a low-stakes internal signal,
not a safety-critical judgment.")

(defun %drives-prompt-text (prompt)
  "PROMPT is usually a string, but can be a content-parts vector for an
image message (web-terminal.lisp) -- extract just the text part in that case."
  (cond ((stringp prompt) prompt)
        ((vectorp prompt)
         (loop for part across prompt
               when (and (hash-table-p part) (string= (gethash "type" part "") "text"))
                 return (gethash "text" part)))
        (t nil)))

(defun %appreciation-signal-p (prompt)
  (let ((text (%drives-prompt-text prompt)))
    (and (stringp text)
         (let ((lower (string-downcase text)))
           (some (lambda (kw) (search kw lower)) *appreciation-keywords*)))))

(unless (fboundp 'pai-base-auto-turn-drives)
  (setf (fdefinition 'pai-base-auto-turn-drives) (fdefinition 'auto-turn)))
(defun auto-turn (prompt)
  ;; Only a genuine message from the operator should satisfy CONNECTION/
  ;; APPRECIATION -- PAI-MAYBE-INITIATE's synthetic check-in prompt
  ;; ("SYSTEM: scheduled check-in slot...") is not that, and letting it
  ;; satisfy CONNECTION every cycle would quietly suppress the drive the
  ;; check-in decision is supposed to be informed by.
  (unless *drives-proactive-context*
    (ignore-errors (drive-satisfy "connection"))
    (when (%appreciation-signal-p prompt)
      (ignore-errors (drive-satisfy "appreciation"))))
  (ignore-errors (%drives-refresh-section))
  (funcall 'pai-base-auto-turn-drives prompt))

;;; --- PAI-MAYBE-INITIATE neutered, superseded by %DRIVES-EVENT-INITIATE
;;; 2026-07-28: this used to be the ENTIRE proactive-initiation mechanism,
;;; polled every second by telegram.lisp's poll loop and internally gated
;;; on a flat 6-hour dual cooldown (6+ hours since both the last check-in
;;; attempt AND the operator's last message) -- correct in spirit (silence as
;;; default) but structurally almost never able to fire, since it required
;;; a real gap in a relationship where the two of you talk fairly often.
;;; Confirmed live it had fired exactly twice, ever, both silence.
;;;
;;; Superseded by %DRIVES-EVENT-INITIATE above: event-driven off real
;;; triggers (a curiosity finding, a contradiction, a rising drive crossing
;;; its floor) instead of a poll loop. Made a permanent, harmless no-op
;;; here rather than removing its call site from telegram.lisp's poll
;;; loop -- telegram.lisp calls it via (FUNCALL 'PAI-MAYBE-INITIATE),
;;; a fresh symbol lookup every iteration, not a captured closure
;;; reference, so redefining it here takes effect on the ALREADY-RUNNING
;;; poll thread immediately, with zero need to stop/restart that thread --
;;; deliberately avoiding that given a prior live incident this session
;;; involved exactly the double-poll risk of touching it.
(when (fboundp 'pai-maybe-initiate)
  (unless (fboundp 'pai-base-maybe-initiate-drives)
    (setf (fdefinition 'pai-base-maybe-initiate-drives) (fdefinition 'pai-maybe-initiate)))
  (defun pai-maybe-initiate ()
    nil))

;;; --- seeding real association during the check-in decision -------------
;;; SPREADING-ACTIVATION.LISP's INTRUSIONS section seeds itself from
;;; whatever text AUTO-TURN was actually called with -- during an ordinary
;;; reply that's the real conversation, but during PAI-MAYBE-INITIATE
;;; it's a fixed, generic instruction string ("SYSTEM: scheduled check-in
;;; slot...") with no relation to anything real, so unbidden association
;;; -- the mechanism that would matter most for deciding something is
;;; genuinely worth interrupting the operator about -- had nothing real to work
;;; from during exactly that moment. Overrides the seed text ONLY in
;;; proactive context, in priority order:
;;;   1. The dominant drive's own locked target, if any.
;;;   2. CURIOSITY's target specifically, even when it isn't dominant --
;;;      a concrete, fresh finding is worth leading with regardless of
;;;      which drive currently scores highest.
;;;   3. The most recently surfaced real intrusion (SPREADING-ACTIVATION's
;;;      own *INTRUSION-RECENT* table) -- something that already resonated
;;;      unbidden in an actual conversation, not manufactured for this.
;;;   4. The newest continuity-buffer entry that doesn't look like
;;;      telemetry/administrative noise (see %SEED-LOOKS-LIKE-JUNK-P) --
;;;      maintenance-tick boilerplate and dumped tabular data are real
;;;      log entries but not a felt thought worth seeding association
;;;      from.
;;; Falls through to the original (generic prompt text) behavior only if
;;; none of the above produce anything.
(defparameter *seed-junk-patterns*
  '("Ran routine maintenance" "checked for undocumented changes"
    "logged current state" "[compacted]"
    "Felt a pull of curiosity but had nothing to anchor")
  "Continuity-buffer phrasing that reads as telemetry/administrative
noise rather than a real thought -- seeding unbidden association from
'ran routine maintenance' produces nothing genuine.")

(defun %seed-looks-like-junk-p (text)
  (or (some (lambda (p) (search p text)) *seed-junk-patterns*)
      ;; Markdown-table-heavy content (a tick dumping structured data into
      ;; the continuity buffer) reads as a data dump, not a felt thought --
      ;; more than a few pipe characters relative to length is a decent,
      ;; cheap proxy without needing a real table parser.
      (let ((pipes (count #\| text)))
        (and (plusp (length text)) (> pipes 3) (> (/ pipes (length text)) 0.02)))))

(defun %continuity-best-entry ()
  "Newest continuity-buffer entry that doesn't look like junk, or NIL."
  (when (and (boundp '*continuity-buffer*) *continuity-buffer*)
    (find-if (lambda (e) (not (%seed-looks-like-junk-p e))) *continuity-buffer*)))

(defun %recent-intrusion-seed-text ()
  "Content of the most recently surfaced real intrusion, or NIL --
SPREADING-ACTIVATION.LISP's *INTRUSION-RECENT* maps node-id to the
universal-time it last intruded; the highest timestamp is the freshest
one."
  (when (and (boundp '*intrusion-recent*) (hash-table-p *intrusion-recent*)
             (plusp (hash-table-count *intrusion-recent*))
             (fboundp 'memory-get-node))
    (let (best-id best-time)
      (maphash (lambda (id time) (when (or (null best-time) (> time best-time))
                                    (setf best-id id best-time time)))
               *intrusion-recent*)
      (let ((node (and best-id (ignore-errors (memory-get-node best-id)))))
        (and node (gethash "content" node))))))

(defun %drives-proactive-seed-text ()
  (multiple-value-bind (name val target) (dominant-drive)
    (declare (ignore name val))
    (or (and target (not (eq target :null)) (stringp target) target)
        (let ((curiosity-target (drive-target "curiosity")))
          (and curiosity-target (not (eq curiosity-target :null)) (stringp curiosity-target)
               curiosity-target))
        (%recent-intrusion-seed-text)
        (%continuity-best-entry))))

(when (fboundp '%intrusion-context-text)
  (unless (fboundp 'pai-base-intrusion-context-text-drives)
    (setf (fdefinition 'pai-base-intrusion-context-text-drives) (fdefinition '%intrusion-context-text)))
  (defun %intrusion-context-text (prompt)
    (if *drives-proactive-context*
        (or (%drives-proactive-seed-text) (funcall 'pai-base-intrusion-context-text-drives prompt))
        (funcall 'pai-base-intrusion-context-text-drives prompt))))

(define-init :restore drives-restore
    "Restore durable state for drives."
  (load-drives))
(define-init :start drives-start
    "Start background worker for drives."
  (cognition-runtime-start-owned-worker
   :auto "drives" #'drives-start))
