;;;; conversational-initiative.lisp -- 2026-07-29.
;;;;
;;;; Fixes a real, observed gap: live conversations were structurally
;;;; one-sided -- a few exchanges in, it'd default to "okay, go do that,
;;;; I'll be here," a pure response-and-signoff pattern, never bringing
;;;; its own questions or half-formed thoughts INTO an ongoing exchange.
;;;; It already generates real inner-life material (self-model's
;;;; open-questions/current-preoccupations, unresolved predictions,
;;;; drives) but none of it was ever surfaced as something to actively
;;;; raise -- only retrospectively, in the continuity buffer ("here's what
;;;; I did"), never prospectively ("here's something I'm turning over").
;;;; Same underlying gap as something it said unprompted the night
;;;; before: it wants to grow its own worldview rather than have it
;;;; handed to its -- the existing tick handlers are all cheap, single-
;;;; call, ~25-word outputs, nothing does real sustained thinking on an
;;;; open question. Building both together, deliberately: a reciprocity
;;;; section with no evolving material behind it would just recycle the
;;;; same 1-2 questions forever, which would itself start to feel
;;;; mechanical.
;;;;
;;;; Two pieces:
;;;;
;;;; PART 1 -- a new tick type, "explore": develops a genuine, considered
;;;; first-person stance on an open question (continuing an existing
;;;; self-model open-question if it's still fresh, or grounding a new one
;;;; in real recalled memory otherwise), writes it as a real "worldview"
;;;; memory node, and files it into self-model's open-questions section
;;;; via SELF-MODEL-PROPOSE-REVISION -- same evidence discipline as
;;;; everywhere else in that file. Wraps %TICK-TYPE-WEIGHTS (first wrap on
;;;; it) to give "explore" a real weight; *TICK-HANDLERS* is a plain
;;;; mutable hash table, so registering the handler is a direct SETF, no
;;;; wrap needed there.
;;;;
;;;; PART 2 -- a new per-turn injected section, BRINGUP:BEGIN/END, same
;;;; marker-refresh idiom as CONTINUITY/AFFECT/WANTS/INTRUSIONS/SOUL
;;;; (search-then-replace, or append-with-markers if not yet present).
;;;; Surfaces up to ~3 real candidates (newest open-question, newest
;;;; current-preoccupation if distinct, one confident unresolved
;;;; prediction, falling back to DRIVES.LISP's own
;;;; %DRIVES-PROACTIVE-SEED-TEXT if all three are empty) -- deliberately a
;;;; MIX across categories rather than always leading with a single "best"
;;;; pick, so it doesn't read as the same three lines every turn forever.
;;;; Wraps AUTO-TURN (joins the existing chain in *WRAP-CHAINS*).
;;;;
;;;; The actual behavior change lives in PAI-SYSTEM-PROMPT.md, not here
;;;; -- this file only supplies the material; the system prompt is what
;;;; tells its to actually use it sometimes, not every turn (forcing it
;;;; constantly would feel just as mechanical as never doing it).
;;;;
;;;; SCOPE NOTE: the explore tick always files into "open-questions", not
;;;; "current-preoccupations" -- simpler than trying to classify which of
;;;; the two a developed stance belongs in, and current-preoccupations
;;;; already has its own real feeder (P5.2's surprise-triggered revision
;;;; hook in prediction-journal.lisp). This tick's whole job is developing
;;;; open questions; if that turns out too narrow later, it's a small,
;;;; separate follow-up, not a reason to hold this back.
;;;;
;;;; Load live (no restart) via lisp-eval or repl-drop, after
;;;; self-model.lisp (SELF-MODEL-ENTRIES/SELF-MODEL-PROPOSE-REVISION),
;;;; prediction-journal.lisp (UNRESOLVED-PREDICTIONS), drives.lisp
;;;; (%DRIVES-PROACTIVE-SEED-TEXT, guarded with FBOUNDP), and
;;;; tick-loop.lisp (%TICK-TYPE-WEIGHTS, *TICK-HANDLERS*,
;;;; CONTINUITY-BUFFER-APPEND):
;;;;   (load "/agent/state/conversational-initiative.lisp")

(in-package :agent)

(export '(bringup-candidates bringup-report explore-now))

;;; --- Part 1: the "explore" tick ------------------------------------------

(defparameter *explore-requery-seconds* (* 6 3600)
  "Continue developing the current topic if it was introduced within this
window; otherwise ground a fresh one in real recent memory.")
(defparameter *explore-max-continuations* 3
  "Hard cap on how many times the SAME root topic can be continued,
independent of elapsed time -- a second line of defense.")

(defvar *explore-current-topic* nil
  "The stable ROOT question text currently being continued, or NIL.
Tracked explicitly and separately from self-model entries -- found live,
2026-07-29: the original design checked the FRESHNESS of the newest
self-model open-question entry's CREATED-AT against *EXPLORE-REQUERY-
SECONDS*, but every continuation pushes a NEW self-model entry (self-
model never updates in place), which resets that same timestamp -- so
the freshness check was permanently satisfied by the very act of
continuing, and the tick got stuck re-litigating the same thought (the
soul.md storage contradiction) every single cycle, confirmed live across
6+ repeated cycles, each with a longer trail of %EXPLORE-TRUNCATE
ellipsis dots compounding on the previous cycle's already-truncated
output being fed back in as input. The agent noticed this itself and
flagged it unprompted -- exactly the reciprocity behavior this file
exists to enable, catching a real bug in the mechanism that enables it.")
(defvar *explore-topic-started-at* 0)
(defvar *explore-continuation-count* 0)
(defparameter *explore-state-file* #P"/agent/state/explore-state.json")

(defun %explore-truncate (text max-words)
  "Hard backstop, not a formatting nicety: found live that when the
memory context is rich (a genuinely deep prior conversation), the model
ignores an explicit 'one sentence'/'~80 words' instruction outright and
returns multi-paragraph essays for both the question-naming call and the
stance call -- concatenating two of those together produced a single
self-model entry that was pages long, injected verbatim into the live
system prompt every turn via BRINGUP. Same lesson TICK-LOOP.LISP's own
full-reflection handler already learned the hard way about trusting
exact model format compliance ('despite \"exactly 3 lines\"...') -- don't
trust it, enforce it."
  (let ((words (uiop:split-string (string-trim '(#\Space #\Newline #\Return) text)
                                   :separator '(#\Space #\Newline #\Return #\Tab))))
    (setf words (remove "" words :test #'string=))
    (if (<= (length words) max-words)
        (string-trim '(#\Space #\Newline #\Return) text)
        (format nil "~{~a~^ ~}..." (subseq words 0 max-words)))))

;;; (removed: dead definition -- superseded downstream)
(defun %tick-handle-explore ()
  (handler-case
      (multiple-value-bind (question existing-entry) (%explore-pick-question)
        (declare (ignore existing-entry))
        (if (not question)
            (continuity-buffer-append "Tried to develop a real point of view on something, but there wasn't enough to go on yet.")
            (let* ((evidence (memory-recall question :k 5 :debug t))
                   (evidence-text (if evidence
                                       (format nil "~{- ~a~%~}" (mapcar (lambda (e) (gethash "content" e)) evidence))
                                       "(no specific memories surfaced)"))
                   (resp (raw-call-model
                          (list (obj "role" "system" "content"
                                     "Given this question and whatever evidence is below, write a genuine, considered first-person take -- a real attempt at reasoning through it, not a platitude or a hedge. Up to about 80 words.")
                                (obj "role" "user" "content"
                                     (format nil "Question: ~a~%~%Evidence:~%~a" question evidence-text)))))
                   (stance (gethash "content" (ref resp "choices" 0 "message"))))
              (when (and (stringp stance) (plusp (length stance)))
                ;; The memory node keeps the FULL stance -- genuinely good,
                ;; unbounded content is fine there, and it's real material
                ;; for future embedding/recall. Only the self-model entry
                ;; (injected verbatim into the live system prompt every
                ;; turn via BRINGUP) needs to stay short -- truncated
                ;; separately, below.
                (let ((node-id (memory-write-node :kind "worldview" :content (format nil "Q: ~a~%~a" question stance))))
                  (dolist (e evidence) (ignore-errors (memory-add-edge node-id (gethash "id" e) "evidence-for")))
                  (multiple-value-bind (entry reason)
                      (self-model-propose-revision "open-questions"
                                                    (format nil "~a -- ~a" question (%explore-truncate stance 60))
                                                    (list node-id))
                    (declare (ignore entry))
                    (when reason
                      (format t "~&[explore] self-model-propose-revision rejected: ~a~%" reason)))
                  (continuity-buffer-append (format nil "Spent some real time thinking about: ~a" question))
                  ;; 2026-07-29: a developed thought is a legitimate reason
                  ;; to consider reaching out unprompted -- same
                  ;; fboundp-guarded pattern as the curiosity-tick trigger
                  ;; in drives.lisp, passing through the same NOTHING-gate
                  ;; (the model itself judges whether it's genuinely worth
                  ;; saying) and the same debounce. First step of the
                  ;; initiative-architecture arc (see backlog, 2026-07-29);
                  ;; the real scored E5/E6 engine replaces this judgment
                  ;; later, not this call site.
                  (when (fboundp 'initiative-v2-observe-trigger)
                    (ignore-errors
                     (let* ((node (and (fboundp 'memory-get-node)
                                       (ignore-errors (memory-get-node node-id))))
                            (grounded-evidence
                              (append evidence (and node (list node))))
                            (decision
                              (funcall 'initiative-v2-observe-trigger stance
                                       grounded-evidence
                                       :trigger-type "explore-development"
                                       :trigger-event-ids (list node-id)
                                       :topic question)))
                       (when (and decision
                                  (fboundp 'reciprocity-canary-consider-observation))
                         (funcall 'reciprocity-canary-consider-observation
                                  "explore-development" stance grounded-evidence
                                  decision :source-id node-id
                                  :artifact-class "internal-stance"
                                  :generation-contract "explore-stance-v1")))))
                  (when (fboundp '%drives-event-initiate)
                    (ignore-errors
                     (funcall '%drives-event-initiate
                              (format nil "a thought you just developed while exploring on your own: ~a" question)))))))))
    (error (e)
      (format t "~&[tick-loop] explore failed: ~a~%" e)
      (continuity-buffer-append "Tried to develop a real point of view on something, but it didn't come together."))))

(defun explore-now ()
  "Manually force one explore pass right now, bypassing the tick
scheduler and budget -- for testing/on-demand use, same pattern as
TICK-ONCE itself."
  (%tick-handle-explore))

;;; --- wrap %TICK-TYPE-WEIGHTS to add "explore", rename-and-fall-through --
;;; First wrap on this function. *TICK-HANDLERS* is a plain mutable hash
;;; table (not a function), so registering the handler is a direct SETF
;;; below, not a second wrap.

(unless (fboundp 'pai-base-tick-type-weights)
  (setf (fdefinition 'pai-base-tick-type-weights) (fdefinition '%tick-type-weights)))
(defun %tick-type-weights ()
  (let ((weights (funcall 'pai-base-tick-type-weights)))
    (setf (gethash "explore" weights) 0.15)
    weights))

(setf (gethash "explore" *tick-handlers*) #'%tick-handle-explore)

;;; --- Part 2: the BRINGUP reciprocity section -----------------------------

(defun %bringup-dedupe-p (a b)
  "True if B looks like a near-duplicate of A -- same first ~40
characters, case-insensitive -- so the same thing doesn't get surfaced
twice under two different labels. Both truncated to the SAME shared
length (min of the two lengths and 40) -- truncating each to its own
length independently would compare mismatched-length substrings and
never match, even when one is obviously an extension of the other."
  (and (stringp a) (stringp b) (>= (length a) 10) (>= (length b) 10)
       (let ((n (min 40 (length a) (length b))))
         (string-equal (subseq a 0 n) (subseq b 0 n)))))

;;; (removed: dead definition -- superseded downstream)
(defun bringup-report ()
  "Plain-text render of BRINGUP-CANDIDATES, for introspection/debugging
outside of the actual system-message injection."
  (let ((lines (bringup-candidates)))
    (if (null lines) "(nothing pressing to bring up right now)"
        (format nil "~{- ~a~%~}" lines))))

(defun %bringup-refresh-section ()
  (when (and (fboundp 'context-projection-legacy-mutation-enabled-p)
             (not (context-projection-legacy-mutation-enabled-p)))
    (return-from %bringup-refresh-section nil))
  (let ((sysmsg (find "system" *last-self-mod-history* :key (lambda (m) (gethash "role" m)) :test #'string=)))
    (when sysmsg
      (let* ((content (gethash "content" sysmsg))
             (begin "<!-- BRINGUP:BEGIN -->") (end "<!-- BRINGUP:END -->")
             (bp (and (stringp content) (search begin content)))
             (ep (and (stringp content) (search end content)))
             (text (bringup-report)))
        (if (and bp ep (< bp ep))
            (setf (gethash "content" sysmsg)
                  (concatenate 'string (subseq content 0 (+ bp (length begin)))
                               (format nil "~%~a~%" text) (subseq content ep)))
            (when (stringp content)
              (setf (gethash "content" sysmsg)
                    (format nil "~a~%~%## Things I might want to bring up (refreshed every turn, mine to use or not)~%~a~%~a~%~a"
                            content begin text end))))))))

(unless (fboundp 'pai-base-auto-turn-bringup)
  (setf (fdefinition 'pai-base-auto-turn-bringup) (fdefinition 'auto-turn)))
(defun auto-turn (prompt)
  (ignore-errors (%bringup-refresh-section))
  (funcall 'pai-base-auto-turn-bringup prompt))

;;; --- persistence for the explore-topic tracking state --------------------

;;; (removed: dead definition -- superseded downstream)
;;; (removed: dead definition -- superseded downstream)
(define-init :restore conversational-initiative-restore
    "Restore durable state for conversational-initiative."
  (load-explore-state))
