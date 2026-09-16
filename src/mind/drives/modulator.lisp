;;;; modulator.lisp -- (modulator state) + P2.3 (modulator ->
;;;; behaviour coupling), built pragmatically against the current
;;;; architecture (same reasoning as event-log.lisp/memory-nodes.lisp --
;;;; see those files' headers).
;;;;
;;;; P2.2 (appraisal) is only built here in the minimal slice P2.3 actually
;;;; needs to be DEMONSTRABLE -- without something moving the modulators
;;;; off baseline, "modulators frozen vs. live" would look identical.
;;;; Skipped: prediction error (needs P5.4, doesn't exist), contradiction
;;;; detection (needs P1.6, doesn't exist), retrieval-novelty -> boredom
;;;; (MEMORY-RECALL isn't called automatically anywhere in the turn loop
;;;; today, only via explicit LISP-EVAL, so there's no natural per-turn
;;;; hook for this yet). Wired up: task success/failure (tool-result
;;;; content, propose-loop outcome) and social-need (elapsed time since
;;;; last inbound message, reusing *PAI-LAST-INBOUND-TIME* from
;;;; agent_helpers.lisp -- the same signal PAI-MAYBE-INITIATE already
;;;; uses).
;;;;
;;;; Derived PSI-style parameters (RESOLUTION-LEVEL/SELECTION-THRESHOLD/
;;;; SAMPLING-RATE) use formulas that are my own reasonable first pass, not
;;;; from a cited source -- the backlog names the concepts and their
;;;; direction of effect, not exact equations. Documented inline, easy to
;;;; retune once there's real behavioural data to tune against.
;;;;
;;;; Coupling implemented, each additive/wrapped, none touching
;;;; agent_loop.lisp:
;;;;   - CALL-MODEL: temperature set from RESOLUTION-LEVEL (careful/
;;;;     uncertain -> lower temperature; confident/casual -> higher).
;;;;   - MEMORY-NODES.LISP's *MEMORY-AFFECT-MODULATOR-FN* hook: high
;;;;     arousal shifts retrieval weight toward recency, away from raw
;;;;     similarity (narrower, recency-heavy, per the backlog's own
;;;;     wording). MODULATOR-RECALL-K narrows K under high arousal.
;;;;   - PROPOSE-LOOP: gated on competence above a floor AND certainty
;;;;     below a ceiling ("change when unsure but capable") -- outside
;;;;     that window, rejected before it even reaches the verifier.
;;;;
;;;; Load live (no restart) via lisp-eval or repl-drop:
;;;;   (load "/agent/state/modulator.lisp")

(in-package :agent)

(export '(modulator-value modulator-set modulator-adjust modulator-state
          resolution-level selection-threshold sampling-rate
          modulator-recall-k save-modulators load-modulators
          observe-tool-result-appraisal))

;;; --- state -----------------------------------------------------------

(defparameter *modulators-file* #P"/agent/state/modulators.json")

(defvar *modulators*
  (obj "arousal"     (obj "current" 0.3 "baseline" 0.3 "decay_rate" 0.05 "min" 0.0 "max" 1.0)
       "valence"     (obj "current" 0.5 "baseline" 0.5 "decay_rate" 0.03 "min" -1.0 "max" 1.0)
       "certainty"   (obj "current" 0.6 "baseline" 0.6 "decay_rate" 0.02 "min" 0.0 "max" 1.0)
       "competence"  (obj "current" 0.6 "baseline" 0.6 "decay_rate" 0.01 "min" 0.0 "max" 1.0)
       "boredom"     (obj "current" 0.2 "baseline" 0.2 "decay_rate" 0.05 "min" 0.0 "max" 1.0)
       "social_need" (obj "current" 0.3 "baseline" 0.3 "decay_rate" 0.02 "min" 0.0 "max" 1.0))
  "Six modulators, each with CURRENT/BASELINE/DECAY_RATE/MIN/MAX. Decays
toward baseline on a background tick (see MODULATOR-DECAY-START below).")

(defvar *modulators-last-decay-ut* (get-universal-time))
(defvar *modulator-lock* (bt:make-lock "modulator"))

(defun modulator-value (name)
  (gethash "current" (gethash name *modulators*)))

(defun modulator-set (name value)
  (bt:with-lock-held (*modulator-lock*)
    (let* ((m (gethash name *modulators*))
           (clamped (max (gethash "min" m) (min (gethash "max" m) value))))
      (setf (gethash "current" m) clamped))))

(defun modulator-adjust (name delta)
  "Adjusts NAME by DELTA, clamped to its MIN/MAX. Returns the new value."
  (modulator-set name (+ (modulator-value name) delta))
  (modulator-value name))

(defun modulator-state ()
  "Full current state, for introspection -- the agent can call this via
LISP-EVAL to actually know its own affect, not guess at it."
  (let ((out (obj)))
    (maphash (lambda (name m) (setf (gethash name out) (gethash "current" m))) *modulators*)
    out))

;;; --- derived PSI-style parameters --------------------------------------------

(defun resolution-level ()
  "How carefully to think. High when CERTAINTY or COMPETENCE is low (more
at stake in getting it right); low when both are high (safe to act
quickly/casually). Range ~0-1."
  (- 1.0 (* (modulator-value "certainty") (modulator-value "competence"))))

(defun selection-threshold ()
  "How easily to switch goals/topics. Low (switches easily) when BOREDOM
or AROUSAL is high; high (stays the course) when both are low."
  (- 1.0 (max (modulator-value "boredom") (modulator-value "arousal"))))

(defun sampling-rate ()
  "How much to gather before acting. High (gather more) when CERTAINTY is
low and AROUSAL is low; low (act on less) when either is high."
  (* (- 1.0 (modulator-value "certainty")) (- 1.0 (modulator-value "arousal"))))

;;; --- decay toward baseline ---------------------------------------------------

(defvar *modulator-decay-thread* nil)
(defvar *modulator-decay-stop-requested* nil)
(defparameter *modulator-decay-interval-seconds* 60)

(defun modulator-decay-tick ()
  "Exponential approach to baseline: current += (baseline - current) *
decay_rate, once per tick. A DECAY_RATE of 0.05 reaches ~95% of the way to
baseline in about 60 ticks (an hour, at the default interval) -- gentle,
not instant amnesia of mood."
  (bt:with-lock-held (*modulator-lock*)
    (maphash
     (lambda (name m)
       (declare (ignore name))
       (let* ((cur (gethash "current" m)) (base (gethash "baseline" m)) (rate (gethash "decay_rate" m)))
         (setf (gethash "current" m) (+ cur (* (- base cur) rate)))))
     *modulators*))
  (setf *modulators-last-decay-ut* (get-universal-time)))

(defun modulator-decay-start ()
  (unless (and *modulator-decay-thread* (bt:thread-alive-p *modulator-decay-thread*))
    (setf *modulator-decay-stop-requested* nil)
    (setf *modulator-decay-thread*
          (bt:make-thread
           (lambda ()
             (loop until *modulator-decay-stop-requested*
                   do (handler-case (modulator-decay-tick)
                        (error (e) (format t "~&[modulator] decay error: ~a~%" e)))
                      (sleep *modulator-decay-interval-seconds*)))
           :name "modulator-decay")))
  (format t "~&[modulator] decay thread watching every ~as.~%" *modulator-decay-interval-seconds*))

(defun modulator-decay-stop (&optional (timeout 5))
  (setf *modulator-decay-stop-requested* t)
  (loop with deadline = (+ (get-internal-real-time) (* timeout internal-time-units-per-second))
        while (and *modulator-decay-thread* (bt:thread-alive-p *modulator-decay-thread*)
                   (< (get-internal-real-time) deadline))
        do (sleep 0.05))
  (if (and *modulator-decay-thread* (bt:thread-alive-p *modulator-decay-thread*))
      (progn (ignore-errors (bt:destroy-thread *modulator-decay-thread*)) :force-killed)
      :stopped-cleanly))

;;; --- persistence --------------------------------------------------------

(defun save-modulators ()
  (let* ((tmp (make-pathname :name (concatenate 'string (pathname-name *modulators-file*) "-tmp")
                              :type (pathname-type *modulators-file*) :defaults *modulators-file*))
         (content
           (let ((*print-pretty* nil))
             (shasht:write-json
              (obj "modulators" *modulators*
                   "saved_at" (get-universal-time)) nil))))
    (with-open-file (out tmp :direction :output :if-exists :supersede
                             :if-does-not-exist :create :external-format :utf-8)
      (write-string content out))
    (multiple-value-prog1 (rename-file tmp *modulators-file*)
      (when (fboundp 'log-projection-state)
        (ignore-errors
          (funcall 'log-projection-state
                   "modulators" *modulators-file* content))))))

(defun load-modulators ()
  "On load, decays proportionally to real elapsed time since SAVED_AT
(capped at 24 simulated hours' worth of ticks) rather than jumping
straight back to baseline -- a mood shouldn't just vanish across a
restart, but a week-old mood shouldn't survive untouched either."
  (handler-case
      (when (probe-file *modulators-file*)
        (with-open-file (in *modulators-file*)
          (let* ((data (shasht:read-json in))
                 (saved (gethash "modulators" data))
                 (saved-at (or (gethash "saved_at" data) (get-universal-time)))
                 (elapsed (max 0 (- (get-universal-time) saved-at)))
                 (capped-ticks (min 1440 (floor elapsed *modulator-decay-interval-seconds*))))
            (maphash (lambda (name m) (setf (gethash name *modulators*) m)) saved)
            (dotimes (i capped-ticks) (modulator-decay-tick)))))
    (error (e)
      (format t "~&[modulator] load failed, using defaults: ~a~%" e)
      nil)))

;;; --- appraisal (minimal P2.2 slice) --------------------------------------

(defparameter *modulator-social-need-half-life-seconds* (* 4 3600)
  "4 hours -- social-need rises toward 1.0 the longer since the last real
inbound message, saturating rather than growing unbounded.")

(defun %modulator-appraise-turn-start ()
  "Called at the start of AUTO-TURN, before the model call. Raises
social_need based on elapsed time since *PAI-LAST-INBOUND-TIME* (the
same variable PAI-MAYBE-INITIATE already tracks) -- a real user message
arriving after a long silence is exactly the social-need signal P2.2
describes."
  (when (and (boundp '*pai-last-inbound-time*) *pai-last-inbound-time*)
    (let* ((elapsed (max 0 (- (get-universal-time) *pai-last-inbound-time*)))
           (saturation (- 1.0 (exp (- (/ elapsed (float *modulator-social-need-half-life-seconds* 1.0d0))))))
           (baseline (gethash "baseline" (gethash "social_need" *modulators*))))
      (modulator-set "social_need" (max baseline saturation)))))

(defun %modulator-appraise-tool-result (result)
  "Called after EXECUTE returns. A tool-role message whose content starts
with \"ERROR:\" (the established convention every tool in this codebase
already uses for failures) nudges competence/valence down; anything else
nudges them up slightly. Small steps -- one tool call is weak evidence."
  (let ((content (and (hash-table-p result) (gethash "content" result))))
    (if (and (stringp content) (>= (length content) 6) (string= (subseq content 0 6) "ERROR:"))
        (progn (modulator-adjust "competence" -0.03) (modulator-adjust "valence" -0.02))
        (progn (modulator-adjust "competence" 0.01) (modulator-adjust "valence" 0.01)))))

(defun %modulator-appraise-propose-outcome (result)
  "Called after PROPOSE-LOOP's base implementation returns (only reached
when the competence/certainty gate below actually let the attempt
through). ACCEPTED raises competence and certainty; REJECTED/rolled-back
lowers competence and raises arousal a little (something unexpected just
happened)."
  (cond
    ((and (stringp result) (>= (length result) 8) (string= (subseq result 0 8) "APPROVED"))
     (modulator-adjust "competence" 0.08) (modulator-adjust "certainty" 0.05))
    ((and (stringp result) (or (search "rolled back" result)
                                (and (>= (length result) 8) (string= (subseq result 0 8) "REJECTED"))))
     (modulator-adjust "competence" -0.05) (modulator-adjust "arousal" 0.05))))

;;; --- coupling ------------------------------------------------------

;;; 1. Sampling temperature from RESOLUTION-LEVEL. CALL-MODEL currently
;;; sends no "temperature" field at all (provider default). Wrapping here
;;; -- outermost is fine for this one, since it only ADDS a field to the
;;; outgoing payload rather than needing to see the response before
;;; anything else does (unlike event-log.lisp/the reasoning-fallback fix,
;;; where wrap ORDER genuinely mattered).

(defparameter *modulator-temp-min* 0.3)
(defparameter *modulator-temp-max* 1.0)

;;; Declared here, BEFORE CALL-MODEL's LET* below binds it -- found in
;;; review before this ever ran live: with the DEFVAR appearing later in
;;; the file (as it originally did), SBCL would compile CALL-MODEL's LET*
;;; binding as an ordinary LEXICAL variable (the symbol isn't yet declared
;;; SPECIAL at that point), invisible to RAW-CALL-MODEL's later read of
;;; the "same" name -- silently making the whole override mechanism a
;;; no-op. Declaring it first makes it a genuine dynamic binding.
(defvar *call-model-temperature-override* nil)
(defvar *call-model-reasoning-override* nil)

;;; R0c1b durable legacy/raw model-attempt events. This is deliberately
;;; composed at the existing HTTP owner below rather than as another
;;; CALL-MODEL/RAW-CALL-MODEL wrapper: reasoning recovery can make several
;;; real provider attempts inside one outer call, and each attempt needs its
;;; own replayable pair without changing the established wrapper order.
(defvar *legacy-model-call-sequence* 0)
(defvar *legacy-model-call-lock* (bt:make-lock "legacy-model-call"))
(defvar *modulator-http-post-fn* nil
  "Optional source-test adapter. NIL preserves the production DEX:POST path.")

(defun %legacy-model-log (type payload &key caused-by)
  (when (fboundp 'log-event)
    (ignore-errors
      (funcall 'log-event type payload :caused-by caused-by))))

(defun %legacy-model-call-id ()
  (bt:with-lock-held (*legacy-model-call-lock*)
    (format nil "legacy-model-~d-~d" (get-universal-time)
            (incf *legacy-model-call-sequence*))))

(defun %legacy-model-purpose ()
  (or (and (boundp '*timing-model-purpose*)
           (symbol-value '*timing-model-purpose*))
      "legacy-raw"))

(defun %legacy-model-context-id (symbol)
  (and (boundp symbol)
       (stringp (symbol-value symbol))
       (symbol-value symbol)))

(defun %legacy-model-elapsed-ms (start)
  (* 1000.0d0
     (/ (- (get-internal-real-time) start)
        (float internal-time-units-per-second 1.0d0))))

(defun %legacy-raw-base-request-body (messages)
  ;; Keep this in lockstep with the bare request captured from AGENT.LISP by
  ;; SELF-MOD.LISP before the legacy wrappers are installed.
  (obj "model" *model* "messages" (coerce messages 'vector)
       "tools" *tools*))

(defun %legacy-invoke-model-attempt (request thunk)
  "Emit one fail-isolated durable pair around one actual provider attempt."
  (let* ((model-call-id (%legacy-model-call-id))
         (purpose (%legacy-model-purpose))
         (trace-id (%legacy-model-context-id '*timing-trace-id*))
         (turn-id (%legacy-model-context-id '*timing-turn-id*))
         (started-at (get-internal-real-time))
         (request-event-id
           (%legacy-model-log
            "model-request"
            (obj "model_call_id" model-call-id
                 "generation_id" :null
                 "trace_id" (or trace-id :null)
                 "turn_id" (or turn-id :null)
                 "purpose" purpose
                 "request_kind" "legacy-raw"
                 "adapter_kind" "openrouter-http"
                 "endpoint" *endpoint*
                 "request" request)
            :caused-by
            (and (boundp '*current-causing-event-id*)
                 (symbol-value '*current-causing-event-id*)))))
    (handler-case
        (let ((response (funcall thunk)))
          (%legacy-model-log
           "model-response"
           (obj "model_call_id" model-call-id
                "generation_id" :null
                "trace_id" (or trace-id :null)
                "turn_id" (or turn-id :null)
                "purpose" purpose
                "request_kind" "legacy-raw"
                "adapter_kind" "openrouter-http"
                "status" "ok"
                "duration_ms" (%legacy-model-elapsed-ms started-at)
                "response" response
                "error_type" :null
                "error_message" :null)
           :caused-by request-event-id)
          response)
      (error (condition)
        (%legacy-model-log
         "model-response"
         (obj "model_call_id" model-call-id
              "generation_id" :null
              "trace_id" (or trace-id :null)
              "turn_id" (or turn-id :null)
              "purpose" purpose
              "request_kind" "legacy-raw"
              "adapter_kind" "openrouter-http"
              "status" "error"
              "duration_ms" (%legacy-model-elapsed-ms started-at)
              "response" :null
              "error_type" (string-downcase (princ-to-string
                                               (type-of condition)))
              "error_message" (princ-to-string condition))
         :caused-by request-event-id)
        (error condition)))))

(unless (fboundp 'pai-base-call-model-modulator)
  (setf (fdefinition 'pai-base-call-model-modulator) (fdefinition 'call-model)))
(defun call-model (messages)
  ;; RESOLUTION-LEVEL near 1.0 (careful) -> low temperature; near 0.0
  ;; (casual/confident) -> high temperature.
  (let* ((res (resolution-level))
         (temp (+ *modulator-temp-min* (* (- 1.0 res) (- *modulator-temp-max* *modulator-temp-min*))))
         (*call-model-temperature-override* temp))
    (funcall 'pai-base-call-model-modulator messages)))

;;; agent.lisp's CALL-MODEL builds its own request body and doesn't accept
;;; a temperature argument -- rather than duplicate its whole HTTP-call
;;; body here (risking drift from the real implementation), this patches
;;; the actual request-building primitive directly instead (below).
;;;
;;; IMPORTANT, found live before this ever reached a real turn: wrapping
;;; RAW-CALL-MODEL itself here (the naive first attempt) would mean this
;;; branch's own direct HTTP call BYPASSES the reasoning-fallback fix
;;; enhancements.lisp already layers on top of RAW-CALL-MODEL --
;;; since *CALL-MODEL-TEMPERATURE-OVERRIDE* is non-nil on every normal
;;; turn (CALL-MODEL above always sets it), that would silently disable
;;; the reasoning-fallback fix for every real conversation, reintroducing
;;; the exact "goes silent" bug that fix was built to solve. Fixed by
;;; redefining the TRUE bottom primitive instead -- PAI-BASE-RAW-CALL-
;;; MODEL-REASONING-FALLBACK, the bare HTTP-calling function nothing
;;; delegates beneath, captured under that name by the reasoning-fallback
;;; fix itself. Adding "temperature" there means every layer built on top
;;; (reasoning-fallback, this file's own CALL-MODEL logging/broadcast,
;;; anything else) still runs in exactly the same order as before; only
;;; the actual network request gains a field. Falls back to wrapping
;;; RAW-CALL-MODEL directly (the old, narrower-correct behaviour) only if
;;; that specific name isn't present for some reason.

(defun %modulator-http-call-with-overrides (messages)
  (let ((body
          (obj "model" *model* "messages" (coerce messages 'vector)
               "tools" *tools*)))
    (when (numberp *call-model-temperature-override*)
      (setf (gethash "temperature" body)
            *call-model-temperature-override*))
    (when (eq *call-model-reasoning-override* :disabled)
      ;; MiMo reasoning is optional at the provider boundary. Preserve the
      ;; ordinary response token budget and reasoning fallback everywhere
      ;; except a simple check-in, where hidden deliberation adds latency but
      ;; no factual or tool-selection value.
      (setf (gethash "reasoning" body) (obj "effort" "none")))
    (%legacy-invoke-model-attempt
     body
     (lambda ()
       (if *modulator-http-post-fn*
           (funcall *modulator-http-post-fn* body)
           (shasht:read-json
            (dex:post *endpoint*
                      :headers `(("Authorization" . ,(format nil "Bearer ~a" *api-key*))
                                 ("Content-Type" . "application/json"))
                      :connect-timeout *http-connect-timeout*
                      :read-timeout *http-read-timeout*
                      :content (shasht:write-json body nil))))))))

(if (fboundp 'pai-base-raw-call-model-reasoning-fallback)
    (progn
      (unless (fboundp 'pai-base-raw-http-call-modulator)
        (setf (fdefinition 'pai-base-raw-http-call-modulator)
              (fdefinition 'pai-base-raw-call-model-reasoning-fallback)))
      (defun pai-base-raw-call-model-reasoning-fallback (messages)
        (if (or (numberp *call-model-temperature-override*)
                (eq *call-model-reasoning-override* :disabled))
            (%modulator-http-call-with-overrides messages)
            (%legacy-invoke-model-attempt
             (%legacy-raw-base-request-body messages)
             (lambda ()
               (funcall 'pai-base-raw-http-call-modulator messages))))))
    (progn
      (unless (fboundp 'pai-base-raw-call-model-modulator)
        (setf (fdefinition 'pai-base-raw-call-model-modulator) (fdefinition 'raw-call-model)))
      (defun raw-call-model (messages)
        (if (or (numberp *call-model-temperature-override*)
                (eq *call-model-reasoning-override* :disabled))
            (%modulator-http-call-with-overrides messages)
            (%legacy-invoke-model-attempt
             (%legacy-raw-base-request-body messages)
             (lambda ()
               (funcall 'pai-base-raw-call-model-modulator messages)))))))

;;; 2. Retrieval weight/width from AROUSAL, via the hook MEMORY-NODES.LISP
;;; already built for exactly this (*MEMORY-AFFECT-MODULATOR-FN*).

(defun %modulator-adjust-recall-weights (weights)
  (let* ((arousal (modulator-value "arousal"))
         ;; High arousal shifts mass from similarity toward recency --
         ;; "narrower, recency-heavy" per the backlog's own phrasing.
         (shift (* arousal 0.3)))
    (obj "sim" (max 0.05 (- (gethash "sim" weights) shift))
         "imp" (gethash "imp" weights)
         "rec" (+ (gethash "rec" weights) shift)
         "act" (gethash "act" weights))))

(when (boundp '*memory-affect-modulator-fn*)
  (setf *memory-affect-modulator-fn* #'%modulator-adjust-recall-weights))

(defun modulator-recall-k (&optional (base-k 8))
  "High arousal narrows retrieval (fewer, more targeted results); low
arousal broadens it (more associative). MEMORY-NODES.LISP's default :K
calls this when this file is loaded (guarded FBOUNDP there), so this has
no effect at all until this file is loaded, and no dependency the other
direction (MEMORY-NODES.LISP works standalone without this file)."
  (let ((arousal (modulator-value "arousal")))
    (max 2 (round (* base-k (- 1.3 arousal))))))

;;; 3. Willingness to self-modify, gated on competence/certainty.

(defparameter *modulator-competence-floor* 0.35
  "Below this, PROPOSE-LOOP is gated off -- not confident enough in
current capability to safely attempt a self-rewrite.")
(defparameter *modulator-certainty-ceiling* 0.85
  "Above this, PROPOSE-LOOP is gated off too -- already very sure, so
there's nothing here to resolve by changing; per the backlog's framing,
the right time to propose is specifically when UNSURE but CAPABLE.")

(unless (fboundp 'pai-base-propose-loop-modulator)
  (setf (fdefinition 'pai-base-propose-loop-modulator) (fdefinition 'propose-loop)))
(defun propose-loop (proposed-src)
  (let ((competence (modulator-value "competence")) (certainty (modulator-value "certainty")))
    (if (or (< competence *modulator-competence-floor*) (> certainty *modulator-certainty-ceiling*))
        (format nil "REJECTED (modulator gate): competence=~,2f certainty=~,2f -- self-modification proposals are only accepted when competence is above ~,2f and certainty is below ~,2f (unsure but capable). Try again after more successful turns, or if something has genuinely made you less sure of your current behaviour."
                competence certainty *modulator-competence-floor* *modulator-certainty-ceiling*)
        (let ((result (funcall 'pai-base-propose-loop-modulator proposed-src)))
          (%modulator-appraise-propose-outcome result)
          result))))

;;; 4. Response verbosity/tone from modulator state -- the last of P2.3's
;;; five listed couplings. A hard max_tokens cutoff risks truncating a
;;; reply mid-thought, a real UX regression -- instead this injects a
;;; short natural-language state note into the system prompt (same
;;; CONTINUITY:BEGIN/END marker-refresh trick tick-loop.lisp already uses,
;;; here against a second, parallel AFFECT:BEGIN/END marker pair), which
;;; steers tone/length without ever hard-limiting the response.

(defun %modulator-affect-note ()
  (let ((arousal (modulator-value "arousal")) (boredom (modulator-value "boredom"))
        (certainty (modulator-value "certainty")) (social-need (modulator-value "social_need")))
    (format nil "Arousal ~,2f, boredom ~,2f, certainty ~,2f, social-need ~,2f. ~a ~a"
            arousal boredom certainty social-need
            (cond ((> arousal 0.6) "Feeling keyed up and energetic right now -- keep replies tight and to the point.")
                  ((< arousal 0.25) "Feeling calm and unhurried right now -- comfortable taking more time and space to think things through.")
                  (t "Feeling level and steady right now."))
            (cond ((< certainty 0.4) "Genuinely unsure about some things at the moment -- comfortable saying so rather than projecting false confidence.")
                  ((> boredom 0.6) "A little restless -- drawn to explore a tangent or ask a real question rather than just answering flatly.")
                  (t "")))))

(defun %modulator-refresh-affect-section ()
  (when (and (fboundp 'context-projection-legacy-mutation-enabled-p)
             (not (context-projection-legacy-mutation-enabled-p)))
    (return-from %modulator-refresh-affect-section nil))
  (let ((sysmsg (find "system" *last-self-mod-history* :key (lambda (m) (gethash "role" m)) :test #'string=)))
    (when sysmsg
      (let* ((content (gethash "content" sysmsg))
             (begin "<!-- AFFECT:BEGIN -->") (end "<!-- AFFECT:END -->")
             (bp (and (stringp content) (search begin content)))
             (ep (and (stringp content) (search end content))))
        (if (and bp ep (< bp ep))
            (setf (gethash "content" sysmsg)
                  (concatenate 'string (subseq content 0 (+ bp (length begin)))
                               (format nil "~%~a~%" (%modulator-affect-note))
                               (subseq content ep)))
            (when (stringp content)
              (setf (gethash "content" sysmsg)
                    (format nil "~a~%~%## Current internal state~%~a~a~%~a"
                            content begin (%modulator-affect-note) end))))))))

;;; --- wire appraisal into the existing turn/tool flow ----------------------

(unless (fboundp 'pai-base-auto-turn-modulator)
  (setf (fdefinition 'pai-base-auto-turn-modulator) (fdefinition 'auto-turn)))
(defun auto-turn (prompt)
  (%modulator-appraise-turn-start)
  (ignore-errors (%modulator-refresh-affect-section))
  (funcall 'pai-base-auto-turn-modulator prompt))

(defun observe-tool-result-appraisal (result)
  "Apply the incumbent isolated tool-result appraisal and return RESULT."
  (ignore-errors (%modulator-appraise-tool-result result))
  result)

(when (or (not (fboundp 'tool-dispatch-legacy-wrapper-enabled-p))
          (funcall 'tool-dispatch-legacy-wrapper-enabled-p))
  (unless (fboundp 'pai-base-execute-modulator)
    (setf (fdefinition 'pai-base-execute-modulator) (fdefinition 'execute)))
  (defun execute (tool-call)
    (let ((result (funcall 'pai-base-execute-modulator tool-call)))
      (observe-tool-result-appraisal result))))

(define-init :restore modulator-restore
    "Restore durable state for modulator."
  (load-modulators))
(define-init :start modulator-start
    "Start background worker for modulator."
  (modulator-decay-start))
