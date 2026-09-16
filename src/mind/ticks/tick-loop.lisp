;;;; tick-loop.lisp -- (tick loop) + P3.2 (tick-type
;;;; selection) + P3.3 (tick handlers) + P3.4 (continuity buffer), built
;;;; pragmatically against the current architecture (same reasoning as
;;;; every other file added this pass -- see event-log.lisp's header).
;;;;
;;;; P0.3's virtual clock doesn't exist -- ticks run on the real clock,
;;;; real SLEEP, same as every other background thread here (drift-
;;;; monitor, modulator decay). P3.5's budget governor doesn't exist
;;;; either -- *TICK-MAX-PER-HOUR* is a simple, conservative stand-in.
;;;;
;;;; "User message preempts" (P3.1's acceptance criterion) means: a tick
;;;; simply does not START while *V2-TURN-IN-FLIGHT* is true -- not true
;;;; mid-tick cancellation (a tick's one model call would need to be
;;;; interrupted mid-HTTP-request, which this codebase has no
;;;; infrastructure for, and isn't what the criterion is actually
;;;; protecting against). Each tick's writes go through the SAME already
;;;; thread-safe primitives real turns and other background threads
;;;; share (MEMORY-WRITE-NODE, MODULATOR-ADJUST, LOG-EVENT), so
;;;; concurrency-safety is inherited, not reinvented.
;;;;
;;;; Tick handlers reuse existing infrastructure rather than
;;;; waiting on subsystems that don't exist yet (P1.4 decay, P1.5
;;;; reflection, P1.6 contradiction detection, P5.4 prediction) --
;;;; CONSOLIDATE/ANTICIPATE/RUMINATE/CURIOSITY are lighter than their full
;;;; future versions but genuinely functional today: they write real
;;;; memory nodes via the real APIs those future phases will build on
;;;; too, not stubs that just log intent.
;;;;
;;;; Continuity buffer is injected into every user-facing turn via
;;;; the same CONTINUITY:BEGIN/END marker-refresh trick already used for
;;;; the live tool list (enhancements.lisp) -- refreshed on EVERY
;;;; AUTO-TURN call, not just at conversation start, since its content
;;;; genuinely changes tick to tick (unlike the tool list, which only
;;;; needs refreshing once per fresh conversation).
;;;;
;;;; Load live (no restart) via lisp-eval or repl-drop:
;;;;   (load "/agent/state/tick-loop.lisp")
;;;; Requires memory-nodes.lisp and modulator.lisp already loaded.

(in-package :agent)

(export '(tick-loop-start tick-loop-stop tick-once continuity-buffer-text tick-cost-summary tick-budget-status))

;;; --- continuity buffer ---------------------------------------------

(defparameter *continuity-buffer-file* #P"/agent/state/continuity-buffer.json")
(defparameter *continuity-buffer-max-entries* 30
  "Above this, the oldest half gets compacted into one summary line --
keeps the buffer bounded without ever silently dropping history: nothing
is lost, it's just increasingly compressed the further back it goes, same
philosophy as MANAGE-CONTEXT's own summarization.")
(defvar *continuity-buffer* nil "List of strings, newest first.")
(defvar *continuity-lock* (bt:make-lock "continuity-buffer"))

(defun continuity-buffer-append (entry)
  ;; Logged here, not just in MEMORY-WRITE-NODE, so every tick's actual
  ;; output is visible in events.jsonl -- including ticks that early-exit
  ;; without writing a memory node (e.g. "not enough in memory yet"),
  ;; which previously left a silent gap between TICK-START and TICK-END
  ;; with no record of what was actually generated. Found live, 2026-07-27
  ;; (the operator noticed memory-write's payload had node_id/kind but no text).
  (when (fboundp 'log-event)
    (ignore-errors (funcall 'log-event "tick-note" (obj "text" entry))))
  (bt:with-lock-held (*continuity-lock*)
    (push (format nil "[~a] ~a" (%tick-now-iso8601) entry) *continuity-buffer*)
    (when (> (length *continuity-buffer*) *continuity-buffer-max-entries*)
      (let* ((keep (subseq *continuity-buffer* 0 (floor *continuity-buffer-max-entries* 2)))
             (to-compact (subseq *continuity-buffer* (floor *continuity-buffer-max-entries* 2))))
        (handler-case
            (let* ((text (format nil "~{~a~%~}" (reverse to-compact)))
                   (resp (raw-call-model
                          (list (obj "role" "system" "content"
                                     "Summarize this rolling internal-activity log into ONE third-person sentence capturing the gist. Under 30 words.")
                                (obj "role" "user" "content" text))))
                   (summary (gethash "content" (ref resp "choices" 0 "message"))))
              (setf *continuity-buffer*
                    (append keep (list (format nil "[compacted] ~a" (if (stringp summary) summary "(older activity)"))))))
          (error (e)
            (format t "~&[tick-loop] continuity compaction failed: ~a~%" e)
            (setf *continuity-buffer* keep))))))
  (ignore-errors (save-continuity-buffer)))

(defun continuity-buffer-text ()
  (bt:with-lock-held (*continuity-lock*)
    (format nil "~{~a~%~}" (reverse *continuity-buffer*))))

(defun save-continuity-buffer ()
  (let ((tmp (make-pathname :name (concatenate 'string (pathname-name *continuity-buffer-file*) "-tmp")
                            :type (pathname-type *continuity-buffer-file*) :defaults *continuity-buffer-file*)))
    (with-open-file (out tmp :direction :output :if-exists :supersede
                             :if-does-not-exist :create :external-format :utf-8)
      (let ((*print-pretty* nil))
        (shasht:write-json (coerce *continuity-buffer* 'vector) out)))
    (rename-file tmp *continuity-buffer-file*)))

(defun load-continuity-buffer ()
  (handler-case
      (when (probe-file *continuity-buffer-file*)
        (with-open-file (in *continuity-buffer-file*)
          (setf *continuity-buffer* (coerce (shasht:read-json in) 'list))))
    (error (e) (format t "~&[tick-loop] continuity load failed: ~a~%" e) nil)))

;;; Re-injection into every user-facing turn: refresh the CONTINUITY
;;; markers in *LAST-SELF-MOD-HISTORY*'s system message on every call, not
;;; just at conversation start.
(defun %tick-refresh-continuity-section ()
  (when (and (fboundp 'context-projection-legacy-mutation-enabled-p)
             (not (context-projection-legacy-mutation-enabled-p)))
    (return-from %tick-refresh-continuity-section nil))
  (let ((sysmsg (find "system" *last-self-mod-history* :key (lambda (m) (gethash "role" m)) :test #'string=)))
    (when sysmsg
      (let* ((content (gethash "content" sysmsg))
             (begin "<!-- CONTINUITY:BEGIN -->") (end "<!-- CONTINUITY:END -->")
             (bp (and (stringp content) (search begin content)))
             (ep (and (stringp content) (search end content))))
        (if (and bp ep (< bp ep))
            (setf (gethash "content" sysmsg)
                  (concatenate 'string (subseq content 0 (+ bp (length begin)))
                               (format nil "~%~a~%" (continuity-buffer-text))
                               (subseq content ep)))
            ;; No markers yet (system prompt predates this file) -- append
            ;; a section rather than silently doing nothing.
            (when (stringp content)
              (setf (gethash "content" sysmsg)
                    (format nil "~a~%~%## Recent internal activity~%~a~a~%~a"
                            content begin (continuity-buffer-text) end))))))))

;;; --- shared tick machinery -------------------------------------------------

(defparameter *tick-max-per-hour* 30
  "Conservative stand-in for P3.5's real budget governor -- bounds real
model-call cost from autonomous activity. Raised again (4 -> 8 -> 15 ->
30) alongside the 2026-07-27 interval shortenings, matching a true ~2min
cadence without the cap immediately throttling it back down. Revisit
downward once past active testing.")
(defvar *tick-timestamps-this-hour* nil)
(defvar *tick-curiosity-count-today* 0)
(defvar *tick-curiosity-day* nil)
(defparameter *tick-curiosity-max-per-day* 3)

(defun %tick-now-iso8601 ()
  (multiple-value-bind (sec min hour day month year)
      (decode-universal-time (get-universal-time) 0)
    (format nil "~a-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0dZ" year month day hour min sec)))

(defun %tick-budget-ok-p ()
  (let ((now (get-universal-time)))
    (setf *tick-timestamps-this-hour* (remove-if (lambda (ts) (> (- now ts) 3600)) *tick-timestamps-this-hour*))
    (< (length *tick-timestamps-this-hour*) *tick-max-per-hour*)))

(defun %tick-record-run ()
  (push (get-universal-time) *tick-timestamps-this-hour*))

(defun %tick-today-key ()
  (multiple-value-bind (s m h day month year) (decode-universal-time (get-universal-time) 0)
    (declare (ignore s m h)) (list year month day)))

(defun %tick-curiosity-budget-ok-p ()
  (let ((today (%tick-today-key)))
    (unless (equal today *tick-curiosity-day*)
      (setf *tick-curiosity-day* today *tick-curiosity-count-today* 0))
    (< *tick-curiosity-count-today* *tick-curiosity-max-per-day*)))

;;; --- real budget governor, added 2026-07-27 ---------------------
;;; *TICK-MAX-PER-HOUR* above is a tick-COUNT cap -- cheap, but blind to
;;; actual dollar cost (a handful of expensive CURIOSITY ticks cost far
;;; more than a dozen cheap MAINTENANCE ones). This adds a real cost-based
;;; governor on top, using TICK-COST-SUMMARY's real spend data (P0.2's
;;; event log) rather than a proxy. Soft limit degrades (cheaper handling,
;;; longer intervals, no curiosity); hard limit suspends everything except
;;; MAINTENANCE. Queryable by anything before it spends via
;;; TICK-BUDGET-STATUS, per the backlog's own phrasing.

(defparameter *tick-budget-soft-daily* 1.00
  "Dollars of real tick spend in the last 24h before degrading. Current
actual spend across this whole session has been roughly a quarter of a
dollar, so this is comfortably above normal usage -- a real safety net for
a cost spike, not a limit meant to bind day to day.")
(defparameter *tick-budget-hard-daily* 3.00
  "Above this, only MAINTENANCE ticks run at all, logged clearly.")
(defparameter *tick-degraded-model* "xiaomi/mimo-v2.5"
  "Dynamically bound over *MODEL* for tick calls while in the soft-limit
state. Same model as the default today -- there's no genuinely cheaper
option configured yet, so the real degrade under soft limit is dropping
CURIOSITY and lengthening the interval; this is here so a cheaper model
can be dropped in later with a one-line change, not a functional no-op
pretending to be a real lever.")

(defvar *tick-budget-cost-records* (make-hash-table :test #'equal)
  "Rebuildable 24-hour tick-cost projection keyed by terminal event ID.")
(defvar *tick-budget-cost-records-ready-p* nil)
(defvar *tick-budget-cost-lock* (bt:make-lock "tick-budget-cost"))

(defun tick-budget-status ()
  "Returns :OK, :SOFT, or :HARD based on real cost over the last 24h.
Fails open (:OK) if cost data isn't available -- a governor that can't see
cost data shouldn't silently block everything, same policy as every other
optional-dependency guard here. Found live on a genuine fresh boot (not
caught by today's incremental live-patching, where everything was already
loaded): tick-loop.lisp loads BEFORE event-log.lisp in the Dockerfile's
boot chain, and the FBOUNDP check alone isn't enough -- TICK-COST-SUMMARY
is always fboundp (defined right here in this file) but hard-errors
internally when ITS OWN dependency (REPLAY-EVENTS) isn't loaded yet. That
error came from OUTSIDE this loop's HANDLER-CASE (interval computation
runs before the sleep, before the protected block), crashing the whole
tick-loop thread into the debugger on every fresh boot. Wrapping the call
itself in HANDLER-CASE, not just checking FBOUNDP, actually fixes it."
  (handler-case
      (if (fboundp '%tick-budget-cost-summary)
          (let ((total (gethash "total_cost"
                                (funcall '%tick-budget-cost-summary))))
            (cond ((>= total *tick-budget-hard-daily*) :hard)
                  ((>= total *tick-budget-soft-daily*) :soft)
                  (t :ok)))
          :ok)
    (error () :ok)))

(defvar *tick-forced-type* nil
  "When bound (by the hard-limit branch below), TICK-ONCE uses this type
directly instead of calling %TICK-SELECT-TYPE.")
(defvar *tick-soft-degrade* nil
  "When true (by the soft-limit branch below), %TICK-TYPE-WEIGHTS zeroes
CURIOSITY's weight entirely -- the most expensive tick type (a model call
to pick a search topic, a real web search, a summarization call) is the
first thing to go under budget pressure.")

;;; --- tick-type selection --------------------------------------------

(defun %tick-type-weights ()
  ;; NODE-COUNT used to read (HASH-TABLE-COUNT *MEMORY-NODES*) -- that
  ;; global hasn't existed since the Postgres migration, but the symbol
  ;; was never explicitly unbound, so this was silently returning a
  ;; frozen pre-migration snapshot instead of erroring or updating. Found
  ;; alongside the same class of bug in the MAINTENANCE handler.
  (let ((arousal (modulator-value "arousal")) (boredom (modulator-value "boredom"))
        (certainty (modulator-value "certainty")) (social-need (modulator-value "social_need"))
        (node-count (if (fboundp '%memory-node-count) (funcall '%memory-node-count) 0)))
    (obj "idle-drift"   (+ 0.3 (* 0.3 (- 1.0 arousal)))
         "consolidate"  (+ 0.1 (* 0.4 (min 1.0 (/ node-count 40.0))))
         "anticipate"   (+ 0.1 (* 0.5 (- 1.0 social-need)))
         "ruminate"     (+ 0.1 (* 0.6 (- 1.0 certainty)))
         "curiosity"    (if (and (%tick-curiosity-budget-ok-p) (not *tick-soft-degrade*)) (+ 0.1 (* 0.5 boredom)) 0.0)
         "maintenance"  0.15)))

(defun %tick-select-type ()
  (let* ((weights (%tick-type-weights))
         (total (loop for v being the hash-values of weights sum v))
         (r (random (max total 0.0001)))
         (acc 0.0))
    (or (loop for k being the hash-keys of weights using (hash-value v)
              do (incf acc v)
              when (<= r acc) return k)
        "maintenance")))

;;; --- tick handlers -----------------------------------------------
;;; Each returns nothing meaningful -- side effects (memory writes,
;;; modulator deltas, continuity entries) ARE the commit, applied directly
;;; through already-thread-safe APIs rather than a separate commit-set
;;; structure. Simpler than the backlog's literal "handlers return a
;;; commit-set" design, and safe for the same reason: nothing here holds a
;;; lock across a network call, and every write is independently atomic.

(defun %tick-random-node ()
  "Return one live Postgres memory node.  The former JSON-memory implementation
used *MEMORY-NODES*, which is no longer bound after the Postgres migration."
  (handler-case
      (let ((row (first (with-pg
                          (pomo:query "SELECT id, content FROM memory_nodes WHERE is_cold = false ORDER BY random() LIMIT 1")))))
        (when row (obj "id" (first row) "content" (second row))))
    (error (e)
      (format t "~&[tick-loop] random-node lookup failed: ~a~%" e)
      nil)))

(defun %tick-handle-idle-drift ()
  (let ((node (%tick-random-node)))
    (if (not node)
        (continuity-buffer-append "Drifted for a moment, but there's nothing in memory yet to drift toward.")
        (handler-case
            (let* ((resp (raw-call-model
                          (list (obj "role" "system" "content"
                                     "You are free-associating quietly, alone with your thoughts. Given this memory, write ONE short first-person sentence of genuine, specific reflection -- not a summary, an actual thought. Under 25 words.")
                                (obj "role" "user" "content" (gethash "content" node)))))
                   (thought (gethash "content" (ref resp "choices" 0 "message"))))
              (when (stringp thought)
                (let ((thought-id (memory-write-node :kind "thought" :content thought :arousal 0.2)))
                  (memory-add-edge thought-id (gethash "id" node) "elaborates"))
                (continuity-buffer-append (format nil "Drifted back to something in memory and thought: ~a" thought))))
          (error (e) (format t "~&[tick-loop] idle-drift failed: ~a~%" e))))))

;;; --- real reflection engine, added 2026-07-27 ----------------------
;;; The original CONSOLIDATE handler (kept below as %TICK-HANDLE-LIGHT-
;;; CONSOLIDATE, still used between full passes) only ever produced one
;;; generic synthesis sentence with no evidence trail -- not what the
;;; backlog actually specifies. This adds the real pipeline: ask for the
;;; most salient QUESTIONS recent memory raises, retrieve evidence per
;;; question, synthesize a reflection grounded in that evidence, link it to
;;; every piece of evidence via an evidence-for edge. Gated on cumulative
;;; importance since the last full pass (*IMPORTANCE-SINCE-LAST-
;;; REFLECTION*, memory-nodes.lisp) rather than running every time
;;; CONSOLIDATE is selected -- a full pass costs up to ~7 model calls (1
;;; for questions + up to 3 questions x (1 recall + 1 synthesis)), so it
;;; should be occasional, not constant. Reflections are written through the
;;; same MEMORY-WRITE-NODE as anything else, so they're automatically
;;; eligible as evidence for a LATER reflection pass -- the tree can deepen
;;; on its own, nothing special needed for that.

(defparameter *reflection-importance-threshold* 4.0
  "Cumulative importance since the last full pass before CONSOLIDATE runs
the real pipeline instead of the lighter single-sentence version.")

(defun %tick-handle-light-consolidate ()
  (let ((top (memory-recall "what matters most right now" :k 5)))
    (if (< (length top) 2)
        (continuity-buffer-append "Tried to consolidate, but there isn't enough in memory yet to draw a thread through.")
        (handler-case
            (let* ((resp (raw-call-model
                          (list (obj "role" "system" "content"
                                     "Given these memories, write ONE third-person sentence synthesizing a pattern or connection across them -- a genuine small insight, not a list. Under 30 words.")
                                (obj "role" "user" "content" (format nil "~{- ~a~%~}" top)))))
                   (reflection (gethash "content" (ref resp "choices" 0 "message"))))
              (when (stringp reflection)
                (memory-write-node :kind "reflection" :content reflection)
                (continuity-buffer-append (format nil "Took a moment to consolidate recent memory: ~a" reflection))))
          (error (e) (format t "~&[tick-loop] light consolidate failed: ~a~%" e))))))

(defun %tick-handle-full-reflection ()
  (handler-case
      (let* ((recent (memory-recall "what has been happening recently" :k 8)))
        (if (< (length recent) 2)
            (continuity-buffer-append "Tried a deeper reflection pass, but there isn't enough in memory yet.")
            (let* ((questions-resp
                     (raw-call-model
                      (list (obj "role" "system" "content"
                                 "Given these recent memories, name the 3 most salient QUESTIONS worth reflecting on -- genuine open questions the memories raise, not facts already answered. Respond with exactly 3 lines, one question per line, nothing else.")
                            (obj "role" "user" "content" (format nil "~{- ~a~%~}" recent)))))
                   (questions-text (gethash "content" (ref questions-resp "choices" 0 "message")))
                   ;; Found live: despite "exactly 3 lines, one question per
                   ;; line", the model sometimes runs all 3 together on one
                   ;; line (still 3 genuine, well-formed questions -- just
                   ;; not newline-separated). Splitting on "?" instead is
                   ;; far more robust: every one of these IS phrased as a
                   ;; real question, so "?" is a reliable delimiter
                   ;; regardless of whether newlines show up. Re-appends the
                   ;; "?" to each trimmed, non-empty fragment.
                   (questions (and (stringp questions-text)
                                   (let ((trimmed (mapcar (lambda (f) (string-trim '(#\Space #\Newline #\- #\Tab) f))
                                                           (uiop:split-string questions-text :separator '(#\?)))))
                                     (mapcar (lambda (f) (concatenate 'string f "?"))
                                             (remove-if (lambda (f) (zerop (length f))) trimmed)))))
                   (reflections-made 0))
              (if (< (length questions) 1)
                  (continuity-buffer-append "Tried a deeper reflection pass, but nothing salient enough came up.")
                  (dolist (q questions)
                    (let ((evidence (memory-recall q :k 5 :debug t)))
                      (when (>= (length evidence) 1)
                        (let* ((evidence-text (format nil "~{- ~a~%~}" (mapcar (lambda (e) (gethash "content" e)) evidence)))
                               (resp (raw-call-model
                                      (list (obj "role" "system" "content"
                                                 "Given this question and the evidence below, write ONE third-person sentence synthesizing an answer or insight -- grounded specifically in the evidence, not a generic statement. Under 30 words.")
                                            (obj "role" "user" "content" (format nil "Question: ~a~%~%Evidence:~%~a" q evidence-text)))))
                               (reflection (gethash "content" (ref resp "choices" 0 "message"))))
                          (when (stringp reflection)
                            (let ((reflection-id (memory-write-node :kind "reflection" :content reflection)))
                              (dolist (e evidence)
                                (ignore-errors (memory-add-edge reflection-id (gethash "id" e) "evidence-for")))
                              (incf reflections-made)
                              (continuity-buffer-append (format nil "Reflected on \"~a\" and concluded: ~a" q reflection)))))))))
              (setf *importance-since-last-reflection* 0.0)
              (when (fboundp 'log-event)
                (ignore-errors (funcall 'log-event "reflection-pass" (obj "questions" (length questions) "reflections_made" reflections-made)))))))
    (error (e)
      (format t "~&[tick-loop] full reflection failed: ~a~%" e)
      (continuity-buffer-append "Tried a deeper reflection pass but it didn't come together."))))

(defun %tick-handle-consolidate ()
  (if (>= *importance-since-last-reflection* *reflection-importance-threshold*)
      (%tick-handle-full-reflection)
      (%tick-handle-light-consolidate)))

(defun %tick-handle-anticipate ()
  (let ((tail (last *last-self-mod-history* 6)))
    (if (< (length tail) 2)
        (continuity-buffer-append "Tried to anticipate what's next, but there's not much conversation yet to go on.")
        (handler-case
            (let* ((text (format nil "~{[~a] ~a~%~}"
                                  (loop for m in tail collect (gethash "role" m)
                                        collect (let ((c (gethash "content" m))) (if (stringp c) c "")))))
                   (resp (raw-call-model
                          (list (obj "role" "system" "content"
                                     "Given this recent conversation excerpt, write ONE short first-person guess at what your human might want to talk about or need next. Under 25 words. If genuinely nothing suggests itself, reply with exactly NOTHING.")
                                (obj "role" "user" "content" text))))
                   (prediction (gethash "content" (ref resp "choices" 0 "message"))))
              (when (and (stringp prediction) (not (string-equal (string-trim '(#\Space #\.) prediction) "NOTHING")))
                (memory-write-node :kind "prediction" :content prediction)
                (continuity-buffer-append (format nil "Found myself anticipating: ~a" prediction))))
          (error (e) (format t "~&[tick-loop] anticipate failed: ~a~%" e))))))

(defun %tick-handle-ruminate ()
  (let ((node (%tick-random-node)))
    (if (not node)
        (continuity-buffer-append "Tried to ruminate on something unresolved, but memory's still too thin.")
        (handler-case
            (let* ((resp (raw-call-model
                          (list (obj "role" "system" "content"
                                     "You feel uncertain about something. Given this memory, write ONE short first-person sentence questioning or reconsidering it -- genuine second-guessing, not resolution. Under 25 words.")
                                (obj "role" "user" "content" (gethash "content" node)))))
                   (thought (gethash "content" (ref resp "choices" 0 "message"))))
              (when (stringp thought)
                (let ((thought-id (memory-write-node :kind "thought" :content thought :arousal 0.4)))
                  (memory-add-edge thought-id (gethash "id" node) "elaborates"))
                (continuity-buffer-append (format nil "Kept turning something over: ~a" thought))))
          (error (e) (format t "~&[tick-loop] ruminate failed: ~a~%" e))))))

(defparameter *curiosity-query-max-length* 80
  "A genuine 3-8 word search query is well under this. Found live,
2026-07-28: the query-generation model call sometimes ignores the 'short
query' instruction entirely and echoes back a large chunk of prior
conversation instead -- one real instance sent a multi-paragraph block to
BRAVE-SEARCH, which correctly rejected it with a 422. Validating the
query's SHAPE before calling out, rather than trusting the instruction was
followed, turns that into a graceful skip instead of a doomed API call.")

(defun %curiosity-query-valid-p (query)
  (and (plusp (length query))
       (<= (length query) *curiosity-query-max-length*)
       (not (find #\Newline query))))

(defun %curiosity-result-valid-p (result)
  "Reject provider failures before they can become cognitive evidence."
  (and (stringp result)
       (>= (length (string-trim '(#\Space #\Tab #\Newline #\Return) result)) 12)
       (not (some (lambda (marker)
                    (search marker result :test #'char-equal))
                  '("ERROR:" "invalid api key" "unauthorized" "forbidden"
                    "\"error\"" "authentication failed")))))

(defun %tick-handle-curiosity ()
  (let ((node (%tick-random-node)))
    (if (or (not node) (not (fboundp 'brave-search)))
        (continuity-buffer-append "Felt a pull of curiosity but had nothing to anchor a search to.")
        (handler-case
            (let* ((query-resp (raw-call-model
                                 (list (obj "role" "system" "content"
                                            "Given this memory, name ONE specific, genuinely search-worthy topic it makes you curious about. Respond with ONLY a short web search query (3-8 words), nothing else.")
                                       (obj "role" "user" "content" (gethash "content" node)))))
                   (query (string-trim '(#\Space #\Newline #\" #\.)
                                        (or (gethash "content" (ref query-resp "choices" 0 "message")) ""))))
              (if (not (%curiosity-query-valid-p query))
                  (continuity-buffer-append "Felt a pull of curiosity but what came out wasn't really a search query, so let it go this time.")
                  (let ((results (brave-search query :count 3)))
                    (if (not (%curiosity-result-valid-p results))
                        (continuity-buffer-append
                         "A curiosity search failed at the provider boundary, so I discarded it rather than treating an error as something learned.")
                        (let* ((resp (raw-call-model
                                      (list (obj "role" "system" "content"
                                                 "Summarize what's new or interesting in these search results in ONE first-person sentence, as something you just learned. Under 30 words.")
                                            (obj "role" "user" "content" results))))
                               (finding (gethash "content" (ref resp "choices" 0 "message"))))
                          (incf *tick-curiosity-count-today*)
                          (when (stringp finding)
                            (memory-write-node :kind "observation" :content finding)
                            (continuity-buffer-append (format nil "Curiosity got the better of me -- looked into \"~a\" and found: ~a" query finding))))))))
          (error (e) (format t "~&[tick-loop] curiosity failed: ~a~%" e))))))

(defun %tick-handle-maintenance ()
  (when (fboundp 'drift-monitor-scan-now) (ignore-errors (funcall 'drift-monitor-scan-now)))
  ;; decay + real forgetting, run here rather than on its own
  ;; schedule -- MAINTENANCE already exists as the "routine upkeep" tick
  ;; type, no need for a second background thread.
  (let ((newly-cold (when (fboundp 'memory-decay-tick) (ignore-errors (funcall 'memory-decay-tick)))))
    (when (fboundp 'log-event)
      (ignore-errors
        ;; Found live, 2026-07-27: this referenced the OLD JSON-file-era
        ;; *MEMORY-NODES* hash-table, which no longer exists post-Postgres-
        ;; migration -- silently erroring every time, swallowed by this
        ;; same IGNORE-ERRORS, so this event never actually logged real
        ;; data. %MEMORY-NODE-COUNT is the real, current node count.
        (funcall 'log-event "tick-maintenance"
                 (obj "node_count" (if (fboundp '%memory-node-count) (funcall '%memory-node-count) :null)
                      "newly_cold" (or newly-cold 0)
                      "modulators" (modulator-state)))))
    (continuity-buffer-append
     (if (and newly-cold (plusp newly-cold))
         (format nil "Ran routine maintenance -- checked for undocumented changes, ~a older mem~:p faded into cold storage." newly-cold)
         "Ran routine maintenance -- checked for undocumented changes, logged current state."))))

(defparameter *tick-handlers*
  (obj "idle-drift" #'%tick-handle-idle-drift "consolidate" #'%tick-handle-consolidate
       "anticipate" #'%tick-handle-anticipate "ruminate" #'%tick-handle-ruminate
       "curiosity" #'%tick-handle-curiosity "maintenance" #'%tick-handle-maintenance))

;;; --- the loop itself ------------------------------------------------

(defparameter *tick-base-interval-seconds* (* 2 60)
  "2 minutes baseline -- shortened repeatedly for active testing
(2026-07-27: 25min -> 10min -> 5min -> 2min). Real spend so far (~$0.25
total on OpenRouter) is low enough that faster, more observable activity
is worth more right now than cost discipline. Modulated by arousal/
boredom below. Raise back toward 20-25min once past active testing.")
(defparameter *tick-min-interval-seconds* (* 1 60))
(defparameter *tick-max-interval-seconds* (* 45 60))

(defvar *tick-thread* nil)
(defvar *tick-stop-requested* nil)
(defvar *tick-lock* (bt:make-lock "tick-loop"))

(defun %tick-compute-interval ()
  (let* ((arousal (modulator-value "arousal")) (boredom (modulator-value "boredom"))
         (raw (/ *tick-base-interval-seconds* (+ 1.0 arousal boredom)))
         (jitter (+ 0.8 (random 0.4))))
    (max *tick-min-interval-seconds* (min *tick-max-interval-seconds* (round (* raw jitter))))))

;;; --- cost tracking (2026-07-27, added once tick frequency ramped up) ---
;;; Pure "observe after" wrap around RAW-CALL-MODEL: reads OpenRouter's own
;;; "usage" field (cost, prompt_tokens, completion_tokens -- the same field
;;; already seen when debugging the reasoning-fallback bug) off every
;;; response made DURING a tick, and accumulates it. Deliberately NOT
;;; wrapping the request-construction primitive the way the temperature
;;; coupling had to (modulator.lisp) -- this only ever READS the response,
;;; never changes control flow or content, so wrap ORDER relative to other
;;; wraps doesn't matter here the way it did there.

(defvar *tick-cost-accumulator* nil
  "NIL outside a tick. Bound to a fresh (cost prompt-tokens completion-
tokens) list for the duration of one tick's handler call.")

(unless (fboundp 'pai-base-raw-call-model-tickcost)
  (setf (fdefinition 'pai-base-raw-call-model-tickcost) (fdefinition 'raw-call-model)))
(defun raw-call-model (messages)
  (let ((resp (funcall 'pai-base-raw-call-model-tickcost messages)))
    (when *tick-cost-accumulator*
      (ignore-errors
        (let* ((usage (gethash "usage" resp)))
          (when usage
            (incf (first *tick-cost-accumulator*) (or (gethash "cost" usage) 0))
            (incf (second *tick-cost-accumulator*) (or (gethash "prompt_tokens" usage) 0))
            (incf (third *tick-cost-accumulator*) (or (gethash "completion_tokens" usage) 0))))))
    resp))

(defun tick-cost-summary (&key hours)
  "Aggregate real spend from TICK-END events in the event log.
HOURS restricts to the last N hours (e.g. :HOURS 24); omitted means all
time. Returns total cost/tokens plus a per-tick-type breakdown. Requires
event-log.lisp to be loaded (REPLAY-EVENTS) -- errors clearly if not,
rather than silently reporting zero."
  (unless (fboundp 'replay-events)
    (error "tick-cost-summary requires event-log.lisp (REPLAY-EVENTS) to be loaded"))
  (let* ((from (and hours (- (get-universal-time) (* hours 3600))))
         ;; Apply the type predicate at the event-storage seam.  REPLAY-EVENTS
         ;; extracts the type from the JSONL line before decoding the payload,
         ;; so large model request/response rows are never materialized merely
         ;; to compute the autonomous-tick budget.  Filtering after replay made
         ;; this two-minute governor reparse the full 24-hour ledger twice per
         ;; loop cycle and caused the 2026-08-09 production heap exhaustion.
         (events (funcall 'replay-events
                          :from from
                          :types '("tick-end" "tick-terminal")))
         (by-type (make-hash-table :test #'equal))
         (total-cost 0.0d0) (total-prompt 0) (total-completion 0))
    (dolist (e events)
      (let* ((payload (gethash "payload" e))
             (tick-type (or (gethash "type" payload) "unknown"))
             (cost (or (gethash "cost" payload) 0))
             (ptok (or (gethash "prompt_tokens" payload) 0))
             (ctok (or (gethash "completion_tokens" payload) 0))
             (entry (or (gethash tick-type by-type) (obj "count" 0 "cost" 0.0d0 "prompt_tokens" 0 "completion_tokens" 0))))
        (incf total-cost cost) (incf total-prompt ptok) (incf total-completion ctok)
        (incf (gethash "count" entry)) (incf (gethash "cost" entry) cost)
        (incf (gethash "prompt_tokens" entry) ptok) (incf (gethash "completion_tokens" entry) ctok)
        (setf (gethash tick-type by-type) entry)))
    (obj "tick_count" (length events) "total_cost" total-cost "total_prompt_tokens" total-prompt
         "total_completion_tokens" total-completion "by_type" by-type
         "window_hours" (or hours :null))))

(defun %tick-budget-event-time (event now)
  (or (gethash "timestamp_universal" event)
      (and (fboundp '%event-parse-ts-string)
           (ignore-errors
             (funcall '%event-parse-ts-string (gethash "timestamp" event))))
      ;; A missing timestamp must not make the safety governor drop a cost.
      ;; Retaining it for the next 24 hours is conservative and bounded.
      now))

(defun %tick-budget-merge-events (events &optional (now (get-universal-time)))
  (bt:with-lock-held (*tick-budget-cost-lock*)
    (dolist (event events)
      (let ((event-type (gethash "type" event))
            (event-id (gethash "id" event)))
        (when (and event-id
                   (member event-type '("tick-end" "tick-terminal")
                           :test #'string=))
          (let ((payload (gethash "payload" event)))
            (setf (gethash event-id *tick-budget-cost-records*)
                  (obj "timestamp_universal"
                       (%tick-budget-event-time event now)
                       "type" (or (gethash "type" payload) "unknown")
                       "cost" (or (gethash "cost" payload) 0)
                       "prompt_tokens" (or (gethash "prompt_tokens" payload) 0)
                       "completion_tokens"
                       (or (gethash "completion_tokens" payload) 0)))))))
    (let ((cutoff (- now (* 24 3600)))
          (expired nil))
      (maphash
       (lambda (id record)
         (when (< (gethash "timestamp_universal" record now) cutoff)
           (push id expired)))
       *tick-budget-cost-records*)
      (dolist (id expired) (remhash id *tick-budget-cost-records*))))
  t)

(defun %tick-budget-ensure-cost-records (&optional (now (get-universal-time)))
  "Hydrate once from durable truth; thereafter merge only the bounded ring."
  (unless *tick-budget-cost-records-ready-p*
    (when (fboundp 'replay-events)
      (handler-case
          (let ((events
                  (funcall 'replay-events
                           :from (- now (* 24 3600))
                           :types '("tick-end" "tick-terminal"))))
            (%tick-budget-merge-events events now)
            (setf *tick-budget-cost-records-ready-p* t))
        (error () nil))))
  ;; LOG-EVENT's ring is newest-first and capped.  IDs make this merge
  ;; idempotent, so polling it twice per loop cycle is cheap and exact after
  ;; the one-time hydration, including V2 TICK-TERMINAL events.
  (when (and (boundp '*event-ring*) (listp *event-ring*))
    (%tick-budget-merge-events *event-ring* now))
  *tick-budget-cost-records-ready-p*)

(defun %tick-budget-cost-summary (&optional (now (get-universal-time)))
  (%tick-budget-ensure-cost-records now)
  (let ((total-cost 0.0d0) (total-prompt 0) (total-completion 0)
        (count 0))
    (bt:with-lock-held (*tick-budget-cost-lock*)
      (maphash
       (lambda (id record)
         (declare (ignore id))
         (incf count)
         (incf total-cost (gethash "cost" record 0))
         (incf total-prompt (gethash "prompt_tokens" record 0))
         (incf total-completion (gethash "completion_tokens" record 0)))
       *tick-budget-cost-records*))
    (obj "tick_count" count "total_cost" total-cost
         "total_prompt_tokens" total-prompt
         "total_completion_tokens" total-completion
         "window_hours" 24
         "source" (if *tick-budget-cost-records-ready-p*
                      "hydrated-ring-projection" "ring-fail-open"))))

(defun tick-once ()
  "Runs exactly one tick right now, ignoring the tick-count budget cap and
the turn-in-flight check -- for manual testing/triggering, not called by
the loop itself (which applies both). Respects *TICK-FORCED-TYPE* if
bound (the hard-limit branch below forces \"maintenance\")."
  (let ((run
          (lambda ()
            (bt:with-lock-held (*tick-lock*)
              (let* ((tick-type (or *tick-forced-type* (%tick-select-type)))
                     (handler (gethash tick-type *tick-handlers*))
                     (tick-event-id
                       (when (fboundp 'log-event)
                         (ignore-errors
                           (funcall 'log-event "tick-start"
                                    (obj "type" tick-type)))))
                     (*tick-cost-accumulator* (list 0.0d0 0 0)))
                (%tick-record-run)
                (funcall handler)
                (when (fboundp 'log-event)
                  (ignore-errors
                    (funcall 'log-event "tick-end"
                             (obj "type" tick-type
                                  "cost" (first *tick-cost-accumulator*)
                                  "prompt_tokens" (second *tick-cost-accumulator*)
                                  "completion_tokens" (third *tick-cost-accumulator*))
                             :caused-by tick-event-id)))
                tick-type)))))
    (if (fboundp 'call-with-event-tick-context)
        (funcall 'call-with-event-tick-context
                 (if (fboundp 'make-event-tick-id)
                     (funcall 'make-event-tick-id "tick-legacy")
                     (format nil "tick-legacy-~d-~d" (get-universal-time)
                             (random 1000000)))
                 run)
        (funcall run))))

(defun tick-loop-start ()
  (unless (and *tick-thread* (bt:thread-alive-p *tick-thread*))
    (setf *tick-stop-requested* nil)
    (setf *tick-thread*
          (bt:make-thread
           (lambda ()
             (loop until *tick-stop-requested*
                   do (let* ((budget (tick-budget-status))
                             (base-interval (%tick-compute-interval))
                             (interval (case budget (:soft (round (* base-interval 2))) (:hard (round (* base-interval 3))) (t base-interval))))
                        (sleep interval)
                        (unless (or *tick-stop-requested*
                                    (and (boundp '*autonomous-write-mode*)
                                         (eq *autonomous-write-mode* :paused)))
                          (handler-case
                              ;; Re-checked after sleeping -- real spend may
                              ;; have changed state during the wait.
                              (let ((budget (tick-budget-status)))
                                (cond
                                  ((eq budget :hard)
                                   (when (%tick-budget-ok-p)
                                     (let ((*tick-forced-type* "maintenance")) (tick-once))
                                     (when (fboundp 'log-event)
                                       (ignore-errors (funcall 'log-event "tick-budget-limit" (obj "level" "hard"))))))
                                  ((eq budget :soft)
                                   (when (%tick-budget-ok-p)
                                     (let ((*model* *tick-degraded-model*) (*tick-soft-degrade* t)) (tick-once))
                                     (when (fboundp 'log-event)
                                       (ignore-errors (funcall 'log-event "tick-budget-limit" (obj "level" "soft"))))))
                                  (t (when (and (%tick-budget-ok-p)
                                                (not (and (boundp '*v2-turn-in-flight*) *v2-turn-in-flight*)))
                                       (tick-once)))))
                            (error (e) (format t "~&[tick-loop] tick error: ~a~%" e))))))
             (format t "~&[tick-loop] stopped.~%"))
           :name "tick-loop")))
  (format t "~&[tick-loop] running, base interval ~as, max ~a/hour, budget soft $~,2f / hard $~,2f per 24h.~%"
          *tick-base-interval-seconds* *tick-max-per-hour* *tick-budget-soft-daily* *tick-budget-hard-daily*))

(defun tick-loop-stop (&optional (timeout 5))
  (setf *tick-stop-requested* t)
  (loop with deadline = (+ (get-internal-real-time) (* timeout internal-time-units-per-second))
        while (and *tick-thread* (bt:thread-alive-p *tick-thread*) (< (get-internal-real-time) deadline))
        do (sleep 0.05))
  (if (and *tick-thread* (bt:thread-alive-p *tick-thread*))
      (progn (ignore-errors (bt:destroy-thread *tick-thread*)) :force-killed)
      :stopped-cleanly))

;;; --- wire continuity injection into auto-turn -----------------------------

(unless (fboundp 'pai-base-auto-turn-tickloop)
  (setf (fdefinition 'pai-base-auto-turn-tickloop) (fdefinition 'auto-turn)))
(defun auto-turn (prompt)
  (ignore-errors (%tick-refresh-continuity-section))
  (funcall 'pai-base-auto-turn-tickloop prompt))

(define-init :restore tick-loop-restore
    "Restore durable state for tick-loop."
  (load-continuity-buffer))
(define-init :start tick-loop-start
    "Start background worker for tick-loop."
  (cognition-runtime-start-owned-worker
   :auto "tick-loop" #'tick-loop-start))
