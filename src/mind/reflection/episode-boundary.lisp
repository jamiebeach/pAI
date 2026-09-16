;;;; episode-boundary.lisp -- P8.3 (event boundary detection) + P8.4
;;;; (boundary-triggered flush) + scoped P8.5 (episode replay + cross-
;;;; episode abstraction), 2026-07-29.
;;;;
;;;; Today's compaction (agent_loop.lisp's MANAGE-CONTEXT) is purely a
;;;; message-count threshold (keep=100/limit=120) -- no awareness of
;;;; topic shift, elapsed time, or anything conversational. This file
;;;; supplies a better PRIMARY trigger (a real conversational boundary);
;;;; the count threshold remains as the backstop, unchanged, exactly as
;;;; P8.4's own text specifies.
;;;;
;;;; CRITICAL CONSTRAINT: MANAGE-CONTEXT runs synchronously on every real
;;;; turn (the hot path). Detection (DETECT-BOUNDARY) must stay cheap and
;;;; local -- no RAW-CALL-MODEL call except in the one specific,
;;;; expected-to-be-rare case documented below. Only the actual flush
;;;; (FLUSH-EPISODE, a rare event) and the replay pass (on the
;;;; independent tick schedule, not the hot path) get to make real model
;;;; calls, same as SUMMARIZE-MESSAGES already does today.
;;;;
;;;; FOUR SIGNALS, three strong/objective (fire a flush directly), one
;;;; weak/inferred (gated behind a verification call):
;;;;   - Elapsed-time gap: reuses AGENT_LOOP.LISP's own [YYYY-MM-DD
;;;;     HH:MM UTC] stamp on every message (NOW-STAMP/STAMP-MESSAGE) --
;;;;     no new metadata, just parsing what's already there.
;;;;   - Closing-phrase: a cheap deterministic keyword check, same idiom
;;;;     as DRIVES.LISP's %APPRECIATION-SIGNAL-P -- no model call.
;;;;   - Prediction-error spike: PREDICTION-JOURNAL.LISP's *PREDICTIONS*
;;;;     resolving "inaccurate" recently -- a hard, already-adjudicated
;;;;     event, not an inference.
;;;;   - Topic shift: the newest substantive user message compared
;;;;     against the centroid of a recent window (not just the single
;;;;     prior message -- too noisy; a short "yeah" would swing a
;;;;     pairwise distance on nothing). Built on an INFERRED soft
;;;;     measure (embedding similarity), so it's the one signal most
;;;;     likely to produce false positives -- and, per an explicit
;;;;     design discussion with the operator, over-triggering (compressing
;;;;     context that was still relevant) is worse than under-triggering
;;;;     (which just leans on the count-based backstop a little longer).
;;;;     So: when topic-shift is the SOLE signal (no strong signal also
;;;;     fired), it doesn't flush directly -- one narrow model call
;;;;     verifies it first, and any ambiguous/malformed response defaults
;;;;     to NOT flushing, matching that same asymmetry.
;;;;   - "Task completion" / explicit user signal: NOT built as a
;;;;     separate detector (would need either a fragile heuristic or a
;;;;     per-turn model call, neither acceptable on the hot path) --
;;;;     folded into closing-phrase where it naturally overlaps. A real,
;;;;     deliberate scope narrowing, not a silent gap.
;;;;
;;;; PART 2, same file (tightly coupled, same pattern as combining
;;;; P5.4+P5.2 and P4.1+P4.4 earlier this session): a scoped slice of
;;;; P8.5 (replay-based consolidation) -- prompted directly by the operator
;;;; asking what happens to a closed segment's summary besides sitting
;;;; at the top of the context window, and whether (as in human memory
;;;; consolidation -- hippocampal replay transferring gist into
;;;; neocortical, generalized knowledge, the episodic trace fading
;;;; faster than the abstraction it produced) something should replay
;;;; episodes later and extract what recurs across them. Without this,
;;;; FLUSH-EPISODE's episode nodes would just be a pile of equally-
;;;; weighted memory nodes with nothing turning repeated cross-episode
;;;; patterns into anything more durable. Runs from the CONSOLIDATE tick
;;;; (matching P8.5's own text), event-driven (enough unreplayed episodes
;;;; accumulate) rather than calendar-based (substituting for P8.5's
;;;; literal "the day's episodes" -- same event-driven-over-polling
;;;; preference as %DRIVES-EVENT-INITIATE replacing the old flat-timer
;;;; poll). Writes abstractions as "reflection"-kind nodes (P1.5's
;;;; existing kind, per P8.5's own instruction to reuse it, not invent a
;;;; new one) -- deliberately does NOT auto-file into self-model,
;;;; consistent with how P1.5's own reflection nodes already work; a
;;;; genuinely resonant pattern reaches self-model organically through
;;;; the explore tick's own memory-recall. "Prediction-error weighting"
;;;; from P8.5's text is honestly approximated via the existing
;;;; importance/arousal-at-encoding columns (a precise link would need
;;;; edges that don't exist yet). P8.5's DOWNSCALING half (multiplicative
;;;; systemwide activation reduction) is explicitly NOT built here --
;;;; more mechanical and separable, left for the next follow-up.
;;;;
;;;; Load live (no restart) via lisp-eval or repl-drop, after
;;;; memory-nodes.lisp (MEMORY-WRITE-NODE/MEMORY-ADD-EDGE/EMBED-TEXT/
;;;; %PARSE-PG-TIMESTAMP), prediction-journal.lisp (*PREDICTIONS*),
;;;; event-log.lisp (LOG-EVENT), and tick-loop.lisp
;;;; (%TICK-HANDLE-CONSOLIDATE, CONTINUITY-BUFFER-APPEND):
;;;;   (load "/agent/state/episode-boundary.lisp")
;;;;
;;;; DETECT-BOUNDARY/FLUSH-EPISODE are called via FBOUNDP-guarded direct
;;;; FUNCALL from agent_loop.lisp (not the rename-and-fall-through idiom
;;;; -- there's no separate top-level symbol to wrap, since
;;;; MANAGE-CONTEXT is a LABELS-local function inside AGENT-LOOP's own
;;;; body). Only %TICK-HANDLE-CONSOLIDATE gets a real wrap here.

(in-package :agent)

(export '(detect-boundary flush-episode episode-history episode-replay-now))

;;; --- thresholds, all individually tunable -------------------------------

(defparameter *episode-min-body-for-boundary* 6
  "Don't even consider a boundary until the segment has at least this
many messages -- avoids flushing a barely-started exchange.")
(defparameter *episode-elapsed-gap-seconds* (* 2 3600))
(defparameter *episode-closing-phrases*
  '("talk later" "gotta go" "got to go" "catch you later" "talk soon"
    "heading out" "bye for now" "ttyl" "signing off" "catch up later"))
(defparameter *episode-prediction-error-window-seconds* (* 30 60))
(defparameter *episode-topic-shift-window-size* 6)
(defparameter *episode-topic-shift-min-message-length* 40
  "Messages shorter than this (after stripping the timestamp prefix) are
excluded from both the comparison window and as the triggering message
itself -- acknowledgments and brief continuations carry no reliable topic
signal.  This gate is deliberately conservative: missing a short topic shift
falls back to count-based compaction, while falsely declaring one can disrupt
an active conversation.")
(defparameter *episode-topic-shift-similarity-threshold* 0.55d0)
(defparameter *episode-replay-min-unreplayed* 3
  "Event-driven, not calendar-based: skip the replay pass entirely until
at least this many un-replayed episodes exist.")
(defparameter *episode-replay-sample-size* 6)

(defvar *current-episode-node-id* nil)
(defvar *last-episode-replay-at* 0)
(defparameter *episode-state-file* #P"/agent/state/episode-state.json")

;;; --- timestamp handling (reuses agent_loop.lisp's own stamp format) -----

(defun %episode-strip-timestamp (content)
  "Strips agent_loop.lisp's own [YYYY-MM-DD HH:MM UTC] prefix if
present. Finds the closing bracket rather than assuming fixed width, so
it can't silently desync from NOW-STAMP's own format."
  (if (and (stringp content) (plusp (length content)) (char= (char content 0) #\[))
      (let ((close (position #\] content)))
        (if close (string-trim '(#\Space) (subseq content (1+ close))) content))
      (or content "")))

(defun %episode-parse-timestamp (content)
  "Parses agent_loop.lisp's own [YYYY-MM-DD HH:MM UTC] prefix back into
universal-time, matching the exact field positions its own
TIMESTAMPED-P/NOW-STAMP already use, or NIL if not stamped that way."
  (and (stringp content) (>= (length content) 18) (char= (char content 0) #\[)
       (ignore-errors
        (let ((year (parse-integer content :start 1 :end 5))
              (month (parse-integer content :start 6 :end 8))
              (day (parse-integer content :start 9 :end 11))
              (hour (parse-integer content :start 12 :end 14))
              (min (parse-integer content :start 15 :end 17)))
          (encode-universal-time 0 min hour day month year 0)))))

;;; --- small local vector helpers (no Postgres round-trip needed for two
;;; ad hoc vectors not yet in the DB) --------------------------------------

(defun %episode-cosine-similarity (a b)
  (let ((dot 0.0d0) (na 0.0d0) (nb 0.0d0))
    (loop for x in a for y in b
          do (incf dot (* x y)) (incf na (* x x)) (incf nb (* y y)))
    (if (or (zerop na) (zerop nb)) 0.0d0 (/ dot (* (sqrt na) (sqrt nb))))))

(defun %episode-average-vectors (vecs)
  (let* ((dim (length (first vecs)))
         (sum (make-list dim :initial-element 0.0d0)))
    (dolist (v vecs) (setf sum (mapcar #'+ sum v)))
    (mapcar (lambda (x) (/ x (length vecs))) sum)))

(defun %episode-substantive-p (m)
  (let ((c (gethash "content" m)))
    (and (stringp c) (>= (length (%episode-strip-timestamp c)) *episode-topic-shift-min-message-length*))))

;;; --- the four signals ---------------------------------------------------

(defun %episode-elapsed-gap-p (body)
  (let ((n (length body)))
    (when (>= n 2)
      (let ((t1 (%episode-parse-timestamp (gethash "content" (nth (- n 2) body))))
            (t2 (%episode-parse-timestamp (gethash "content" (nth (- n 1) body)))))
        (and t1 t2 (>= (- t2 t1) *episode-elapsed-gap-seconds*))))))

(defun %episode-closing-phrase-p (body)
  (let* ((last (car (last body))) (c (and last (gethash "content" last))))
    (and (stringp c)
         (let ((lower (string-downcase (%episode-strip-timestamp c))))
           (some (lambda (p) (search p lower)) *episode-closing-phrases*)))))

(defun %episode-prediction-error-spike-p ()
  (and (boundp '*predictions*)
       (let ((cutoff (- (get-universal-time) *episode-prediction-error-window-seconds*)))
         (some (lambda (p) (and (string= (gethash "status" p) "inaccurate")
                                 (numberp (gethash "resolved-at" p))
                                 (>= (gethash "resolved-at" p) cutoff)))
               *predictions*))))

(defun %episode-topic-shift-candidate-p (body)
  "Returns the computed similarity if a comparison was possible and it's
below threshold (a real candidate), NIL otherwise -- NIL means either
'not evaluated' (not enough substantive history, or the last message
isn't a fresh user message) or 'evaluated, no shift', both handled the
same way by the caller (no flush from this signal)."
  (let ((last (car (last body))))
    (when (and last (string= (gethash "role" last) "user") (%episode-substantive-p last))
      (let ((window (last (remove-if-not #'%episode-substantive-p (butlast body)) *episode-topic-shift-window-size*)))
        (when (>= (length window) 2)
          (let* ((new-text (%episode-strip-timestamp (gethash "content" last)))
                 (window-vecs (mapcar (lambda (m) (embed-text (%episode-strip-timestamp (gethash "content" m)))) window))
                 (new-vec (embed-text new-text))
                 (centroid (%episode-average-vectors window-vecs))
                 (sim (%episode-cosine-similarity new-vec centroid)))
            (when (< sim *episode-topic-shift-similarity-threshold*) sim)))))))

(defun %episode-verify-topic-shift (body)
  "The one model call in DETECT-BOUNDARY -- only reached when topic-shift
is the SOLE signal pointing to a boundary. Any ambiguous or malformed
response defaults to NIL (don't flush): an uncertain verification should
fail toward the cheaper error (miss it, fall back to the count backstop),
never toward the more expensive one (wrongly flush real context)."
  (handler-case
      (let* ((last (car (last body)))
             (window (last (remove-if-not #'%episode-substantive-p (butlast body)) *episode-topic-shift-window-size*))
             (window-text (format nil "~{- ~a~%~}"
                                   (mapcar (lambda (m) (%episode-strip-timestamp (gethash "content" m))) window)))
             (new-text (%episode-strip-timestamp (gethash "content" last)))
             (resp (raw-call-model
                    (list (obj "role" "system" "content"
                               "Given this recent conversation window and the newest message, has the topic genuinely shifted to something substantively different -- not just a new detail on the same subject? Respond with exactly one word: YES or NO.")
                          (obj "role" "user" "content"
                               (format nil "Recent window:~%~a~%~%Newest message: ~a" window-text new-text)))))
             (verdict (gethash "content" (ref resp "choices" 0 "message"))))
        ;; The prompt asks for one word.  Treat anything else as ambiguous,
        ;; rather than accepting prose which merely happens to mention YES.
        (and (stringp verdict)
             (string= "YES"
                      (string-upcase
                       (string-trim '(#\Space #\Tab #\Newline #\Return) verdict)))
             t))
    (error (e)
      (format t "~&[episode-boundary] topic-shift verification failed, defaulting to NO: ~a~%" e)
      nil)))

;;; --- detect-boundary -----------------------------------------------

(defun detect-boundary (sys body)
  (declare (ignore sys))
  (when (>= (length body) *episode-min-body-for-boundary*)
    (let* ((elapsed (%episode-elapsed-gap-p body))
           (closing (%episode-closing-phrase-p body))
           (pred-error (%episode-prediction-error-spike-p)))
      (if (or elapsed closing pred-error)
          (progn
            (when (fboundp 'log-event)
              (ignore-errors
               (funcall 'log-event "episode-boundary-detected"
                        (obj "elapsed" (and elapsed t) "closing-phrase" (and closing t)
                             "prediction-error" (and pred-error t) "verified" :null))))
            t)
          (let ((sim (%episode-topic-shift-candidate-p body)))
            (when sim
              (let ((verified (%episode-verify-topic-shift body)))
                (when (fboundp 'log-event)
                  (ignore-errors
                   (funcall 'log-event "episode-boundary-detected"
                            (obj "elapsed" :false "closing-phrase" :false "prediction-error" :false
                                 "topic-shift-similarity" sim "verified" (and verified t)))))
                verified)))))))

;;; --- flush-episode --------------------------------------------------

(defun %episode-render-segment (body)
  (format nil "~{[~a] ~a~%~}"
          (loop for m in body
                collect (gethash "role" m)
                collect (%episode-strip-timestamp (or (gethash "content" m) "")))))

(defun flush-episode (sys body)
  (declare (ignore sys))
  (handler-case
      (let* ((segment-text (%episode-render-segment body))
             (resp (raw-call-model
                    (list (obj "role" "system" "content"
                               "You are summarizing a closed conversational episode for long-term memory. Write a concise third-person summary covering: what was discussed, what was decided or accomplished, and anything left open. Under 150 words.")
                          (obj "role" "user" "content" segment-text))))
             (summary (gethash "content" (ref resp "choices" 0 "message"))))
        (when (and (stringp summary) (plusp (length summary)))
          (let ((episode-id (memory-write-node :kind "episode" :content summary)))
            (when *current-episode-node-id*
              (ignore-errors (memory-add-edge episode-id *current-episode-node-id* "follows")))
            (setf *current-episode-node-id* episode-id)
            (ignore-errors (save-episode-state))
            (when (fboundp 'log-event)
              (ignore-errors
               (funcall 'log-event "episode-flushed"
                        (obj "episode-node-id" episode-id "messages-flushed" (length body)))))
            (list (obj "role" "system" "content"
                       (format nil "[Picking up after a closed episode: ~a]" summary))))))
    (error (e)
      (format t "~&[episode-boundary] flush failed, falling back to count-based compaction: ~a~%" e)
      nil)))

;;; --- Part 2 (scoped P8.5): episode replay + cross-episode abstraction ---

(defun %episode-unreplayed-episodes ()
  "Episode nodes created since *LAST-EPISODE-REPLAY-AT*, oldest first.
Fetches a bounded recent set and filters/sorts in Lisp -- same pattern
MEMORY-RECALL/%RECENCY-SCORE already use (SQL narrows, Lisp scores)."
  (let ((rows (with-pg
                (pomo:query
                 "SELECT id, content, importance, arousal_at_encoding, created_at::text FROM memory_nodes WHERE kind = 'episode' ORDER BY created_at DESC LIMIT 50"))))
    (sort (remove-if-not (lambda (row) (> (%parse-pg-timestamp (fifth row)) *last-episode-replay-at*)) rows)
          #'< :key (lambda (row) (%parse-pg-timestamp (fifth row))))))

(defun %episode-replay-sample (episodes)
  "Salience-weighted sample -- importance + arousal-at-encoding, both
already-existing columns. A genuine link to specific P5.4 prediction
errors would need edges that don't exist yet, so 'prediction-error
weighting' from the backlog's own text is honestly approximated via this
existing signal rather than fabricating a precise link."
  (let ((sorted (sort (copy-list episodes) #'>
                       :key (lambda (row) (+ (or (third row) 0.0d0) (or (fourth row) 0.0d0))))))
    (subseq sorted 0 (min *episode-replay-sample-size* (length sorted)))))

(defun %maybe-replay-episodes ()
  (handler-case
      (let ((unreplayed (%episode-unreplayed-episodes)))
        (if (< (length unreplayed) *episode-replay-min-unreplayed*)
            nil
            (let* ((sample (%episode-replay-sample unreplayed))
                   (episode-text (format nil "~{Episode: ~a~%~%~}" (mapcar #'second sample)))
                   (resp (raw-call-model
                          (list (obj "role" "system" "content"
                                     "Given these separate episode summaries from different past conversations, identify ONE genuine pattern or theme that recurs across them -- something true of several, not a re-summary of any single one. If nothing genuinely recurs, respond with exactly NOTHING.")
                                (obj "role" "user" "content" episode-text))))
                   (abstraction (gethash "content" (ref resp "choices" 0 "message"))))
              (when (and (stringp abstraction) (plusp (length abstraction))
                         (not (string-equal (string-trim '(#\Space #\.) abstraction) "NOTHING")))
                (let ((reflection-id (memory-write-node :kind "reflection" :content abstraction)))
                  (dolist (row sample) (ignore-errors (memory-add-edge reflection-id (first row) "evidence-for")))
                  (ignore-errors (continuity-buffer-append (format nil "Noticed a pattern across some recent episodes: ~a" abstraction)))
                  (when (fboundp 'log-event)
                    (ignore-errors
                     (funcall 'log-event "episode-replay"
                              (obj "episodes-considered" (length unreplayed) "sampled" (length sample)
                                   "reflection-node-id" reflection-id))))))
              (setf *last-episode-replay-at* (get-universal-time))
              (ignore-errors (save-episode-state))
              :replayed)))
    (error (e) (format t "~&[episode-boundary] replay failed: ~a~%" e) nil)))

(defun episode-replay-now ()
  "Force one replay pass right now, bypassing the min-unreplayed gate --
for testing/on-demand use, same pattern as EXPLORE-NOW/TICK-ONCE."
  (let ((*episode-replay-min-unreplayed* 0)) (%maybe-replay-episodes)))

(defun episode-history (&optional (n 10))
  "Recent episode nodes, newest first -- for introspection/debugging."
  (with-pg
    (pomo:query
     (format nil "SELECT id, content, created_at::text FROM memory_nodes WHERE kind = 'episode' ORDER BY created_at DESC LIMIT ~a" n))))

;;; --- wrap %TICK-HANDLE-CONSOLIDATE, rename-and-fall-through -------------
;;; Runs alongside (not instead of) the existing light/full reflection
;;; logic -- first wrap on this function.

(unless (fboundp 'pai-base-tick-handle-consolidate-episodes)
  (setf (fdefinition 'pai-base-tick-handle-consolidate-episodes) (fdefinition '%tick-handle-consolidate)))
(defun %tick-handle-consolidate ()
  (funcall 'pai-base-tick-handle-consolidate-episodes)
  (ignore-errors (%maybe-replay-episodes)))

;;; --- persistence ----------------------------------------------------------

(defun save-episode-state ()
  (let ((tmp (make-pathname :name "episode-state-tmp" :type "json" :defaults *episode-state-file*)))
    (with-open-file (out tmp :direction :output :if-exists :supersede
                         :if-does-not-exist :create :external-format :utf-8)
      (let ((*print-pretty* nil))
        (shasht:write-json (obj "current-episode-node-id" (or *current-episode-node-id* :null)
                                 "last-episode-replay-at" *last-episode-replay-at*)
                            out)))
    (rename-file tmp *episode-state-file*)))

(defun load-episode-state ()
  (handler-case
      (when (probe-file *episode-state-file*)
        (with-open-file (in *episode-state-file*)
          (let ((data (shasht:read-json in)))
            (setf *current-episode-node-id* (let ((v (gethash "current-episode-node-id" data)))
                                               (if (eq v :null) nil v)))
            (setf *last-episode-replay-at* (or (gethash "last-episode-replay-at" data) 0)))))
    (error (e) (format t "~&[episode-boundary] load failed, starting fresh: ~a~%" e) nil)))

(define-init :restore episode-boundary-restore
    "Restore durable state for episode-boundary."
  (load-episode-state))
