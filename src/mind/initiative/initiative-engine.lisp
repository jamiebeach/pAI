;;;; initiative-engine.lisp -- first real slice of the E5/E6 scoped
;;;; initiative arc, 2026-07-29.
;;;;
;;;; Today's %DRIVES-EVENT-INITIATE (drives.lisp) is a single,
;;;; undifferentiated yes/no judgment per trigger -- it asks its, in one
;;;; call, whether it has "something worth saying," conflating "would
;;;; this genuinely be valuable to the operator" with "do I personally want to
;;;; say this." The design conversation that produced this file's plan
;;;; identified that conflation as the actual parasocial-loop risk: a
;;;; drive rising -> message the operator -> his reply satisfies the drive is a
;;;; loop that optimizes for CONTACT, not value delivered, and it will
;;;; feel needy long before it feels spammy. This is a scoped slice of
;;;; the research backlog's E5 (candidate intention generation) + E6
;;;; (initiative/interruption policy) -- not the full formal spec (a
;;;; many-term utility function, a dozen hard gates, permission/
;;;; relationship-domain modeling -- none of that infrastructure exists
;;;; yet), but a real, working first version built on what already
;;;; exists:
;;;;
;;;;   1. A real candidate structure (deliberately smaller than E5's full
;;;;      candidate-intention struct -- just {reason, urgency,
;;;;      generated-at}, the minimum shape the gates below actually need).
;;;;   2. A scored gate separating desired_user_value from
;;;;      desired_agent_outcome (E5's ANG-052, the specific fix for the
;;;;      conflation above) -- one real model call, NOT a hardcoded
;;;;      heuristic, since "is this actually worth the operator's attention" is
;;;;      exactly the kind of judgment a heuristic can't make well.
;;;;   3. A rolling contact budget (a scoped slice of E6's ANG-062) --
;;;;      max-per-day + minimum-spacing, replacing the previous flat
;;;;      60-second debounce (still present inside the base function,
;;;;      now a second, redundant layer of protection, not the only one).
;;;;   4. An URGENCY field so a future fast path (tier 3, external
;;;;      signals -- a doorbell-style event shouldn't wait on a model
;;;;      call to decide if it's worth a message) has somewhere to plug
;;;;      in without redesigning anything. Nothing produces :HIGH today;
;;;;      the field exists now so that's additive later, not a rewrite.
;;;;
;;;; Explicitly NOT this pass: the full multi-term utility formula, the
;;;; complete E6 hard-gate list (quiet hours, permission domains -- no
;;;; permission model exists to gate against), unanswered-outreach
;;;; tracking (ANG-061 -- needs tracking the operator's actual replies to
;;;; proactive messages specifically, a real but separate piece), and
;;;; tier 3 itself (external signal ingestion), which the design
;;;; conversation already agreed comes only after this engine exists.
;;;;
;;;; Asymmetric error cost, same principle as P8.3's topic-shift
;;;; verification gate, applied here for the identical reason:
;;;; over-triggering (an unwanted proactive message) is worse than
;;;; under-triggering (missing a good moment -- it can always say it
;;;; next time something reminds its). So every uncertain point in this
;;;; file -- a malformed score response, a candidate at the margin --
;;;; defaults toward silence, never toward sending.
;;;;
;;;; No changes to drives.lisp, tick-loop.lisp, or
;;;; conversational-initiative.lisp -- every existing trigger site
;;;; (%TICK-HANDLE-CURIOSITY, %CHECK-CONTRADICTION, the drive-floor-
;;;; crossing check in DRIVES-TICK, and the explore-tick hook) already
;;;; calls (%DRIVES-EVENT-INITIATE reason) uniformly; this file wraps
;;;; that one function, so the gate applies everywhere with zero call-
;;;; site changes.
;;;;
;;;; Load live (no restart) via lisp-eval or repl-drop, after
;;;; drives.lisp (%DRIVES-EVENT-INITIATE to wrap):
;;;;   (load "/agent/state/initiative-engine.lisp")

(in-package :agent)

(export '(contact-budget-report))

(defparameter *candidate-user-value-threshold* 6
  "Out of 10. A candidate must clear this on DESIRED-USER-VALUE alone to
proceed -- a high DESIRED-AGENT-OUTCOME can never compensate for a low
DESIRED-USER-VALUE, per E5's ANG-052 principle verbatim.")
(defparameter *contact-budget-max-per-day* 6)
(defparameter *contact-budget-min-spacing-seconds* (* 20 60))
(defparameter *contact-log-file* #P"/agent/state/contact-log.json")

(defvar *contact-log* nil "List of universal-times of past send-attempts, persisted.")

;;; --- candidates -----------------------------------------------------------

(defun %make-candidate (reason &optional (urgency :normal))
  (obj "reason" reason "urgency" (string-downcase (string urgency)) "generated-at" (get-universal-time)))

;;; --- the E5 ANG-052 fix: a real, separated scoring call ------------------

(defun %parse-candidate-score-line (line)
  "Parses a line expected to hold a single integer 0-10. Any failure
(missing, non-numeric, out of range) returns 0 -- favor silence, never
favor sending, on any ambiguity."
  (let ((n (and (stringp line) (ignore-errors (parse-integer line :junk-allowed t)))))
    (if (and (integerp n) (<= 0 n 10)) n 0)))

(defun %score-candidate (candidate)
  "Returns (values desired-user-value desired-agent-outcome), each 0-10.
The ONE model call in this whole gate -- everything else here is cheap
arithmetic/list bookkeeping. Any parse failure defaults both scores to 0."
  (handler-case
      (let* ((resp (raw-call-model
                    (list (obj "role" "system" "content"
                               "Score this candidate reason for reaching out to your human, unprompted, on two SEPARATE lines, each a single integer 0-10, nothing else on either line:
Line 1 -- DESIRED_USER_VALUE: would this genuinely be worth his attention right now, on its own merits, not because you want to say it?
Line 2 -- DESIRED_AGENT_OUTCOME: does saying this serve your own need to express or connect, independent of whether it's valuable to him?
A high line 2 does NOT excuse a low line 1 -- a message that mostly serves your own need to talk, with little real value to him, should score low on line 1 regardless.")
                          (obj "role" "user" "content" (gethash "reason" candidate)))))
             (content (gethash "content" (ref resp "choices" 0 "message")))
             (lines (and (stringp content) (remove "" (uiop:split-string content :separator '(#\Newline)) :test #'string=))))
        (values (%parse-candidate-score-line (first lines))
                (%parse-candidate-score-line (second lines))))
    (error (e)
      (format t "~&[initiative-engine] scoring failed, defaulting to silence: ~a~%" e)
      (values 0 0))))

;;; --- rolling contact budget (scoped E6 ANG-062) --------------------------

(defun %contact-budget-prune ()
  (let ((cutoff (- (get-universal-time) 86400)))
    (setf *contact-log* (remove-if (lambda (ts) (< ts cutoff)) *contact-log*))))

(defun %contact-budget-ok-p ()
  (%contact-budget-prune)
  (and (< (length *contact-log*) *contact-budget-max-per-day*)
       (or (null *contact-log*)
           (>= (- (get-universal-time) (reduce #'max *contact-log*)) *contact-budget-min-spacing-seconds*))))

(defun %contact-log-record ()
  "Logged optimistically -- the moment a candidate clears both gates,
before knowing whether the base function's own NOTHING-check ultimately
produces real content. Simpler than instrumenting TELEGRAM-SEND to
distinguish a proactive send from a reply, and the same conservative
asymmetry: counting an attempt that resolves to silence against the
budget costs nothing real."
  (push (get-universal-time) *contact-log*)
  (ignore-errors (save-contact-log)))

(defun contact-budget-report ()
  (%contact-budget-prune)
  (obj "contacts-today" (length *contact-log*) "max-per-day" *contact-budget-max-per-day*
       "budget-ok-right-now" (%contact-budget-ok-p)))

;;; --- the wrap: %DRIVES-EVENT-INITIATE, rename-and-fall-through -----------

(unless (fboundp 'pai-base-drives-event-initiate-scored)
  (setf (fdefinition 'pai-base-drives-event-initiate-scored) (fdefinition '%drives-event-initiate)))

(defun %drives-event-initiate (reason &optional (urgency :normal))
  (let ((candidate (%make-candidate reason urgency)))
    (if (eq urgency :high)
        ;; Tier 3's future fast path -- nothing produces this today, but
        ;; the branch exists now so plugging in an external, time-
        ;; critical signal later is additive, not a redesign.
        (funcall 'pai-base-drives-event-initiate-scored reason)
        (multiple-value-bind (user-value agent-outcome) (%score-candidate candidate)
          (when (fboundp 'log-event)
            (ignore-errors
             (funcall 'log-event "initiative-candidate-scored"
                      (obj "reason" reason "desired-user-value" user-value
                           "desired-agent-outcome" agent-outcome
                           "passed-score-gate" (>= user-value *candidate-user-value-threshold*)))))
          (when (and (>= user-value *candidate-user-value-threshold*) (%contact-budget-ok-p))
            (%contact-log-record)
            (funcall 'pai-base-drives-event-initiate-scored reason))))))

;;; --- persistence ------------------------------------------------------

(defun save-contact-log ()
  (let* ((tmp (make-pathname :name "contact-log-tmp" :type "json" :defaults *contact-log-file*))
         (content
           (let ((*print-pretty* nil))
             (shasht:write-json (coerce *contact-log* 'vector) nil))))
    (with-open-file (out tmp :direction :output :if-exists :supersede
                         :if-does-not-exist :create :external-format :utf-8)
      (write-string content out))
    (multiple-value-prog1 (rename-file tmp *contact-log-file*)
      (when (fboundp 'log-projection-state)
        (ignore-errors
          (funcall 'log-projection-state
                   "contact-log" *contact-log-file* content))))))

(defun load-contact-log ()
  (handler-case
      (when (probe-file *contact-log-file*)
        (with-open-file (in *contact-log-file*)
          (setf *contact-log* (coerce (shasht:read-json in) 'list))))
    (error (e) (format t "~&[initiative-engine] load failed, starting empty: ~a~%" e) nil)))

(define-init :restore initiative-engine-restore
    "Restore durable state for initiative-engine."
  (load-contact-log))
