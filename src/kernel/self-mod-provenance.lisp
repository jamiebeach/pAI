;;;; self-mod-provenance.lisp -- P4.1 (change proposal record) + P4.4
;;;; (outcome attribution), 2026-07-28.
;;;;
;;;; every propose-loop call -- accepted OR rejected -- gets a real
;;;; structured record: target-symbol (always "agent-loop", the only thing
;;;; propose-loop ever proposes), old-source, new-source, rationale,
;;;; expected-effect, proposed-at, status. Written to disk
;;;; (self-mod-proposals.json, same JSONL-safe discipline as every other
;;;; journal this session) AND as real memory nodes (kind
;;;; "self-mod-proposal" / "self-mod-proposal-outcome"), satisfying the
;;;; acceptance criterion literally: retrievable via normal memory recall,
;;;; not just a side file only code can read.
;;;;
;;;; RATIONALE and EXPECTED-EFFECT are captured STRUCTURALLY, via new
;;;; fields added to the propose-loop TOOL SCHEMA itself -- not parsed out
;;;; of its free-form turn text with a regex/heuristic. That would be
;;;; exactly the anti-pattern E1 (2026-07-28, earlier the same day) moved
;;;; away from ("never extract structure from free text with textual
;;;; pattern-matching") -- if it was wrong there, it's wrong here too.
;;;;
;;;; EXPECTED-EFFECT is not a second, parallel prediction system: it's
;;;; recorded as a real P5.4 prediction (WRITE-PREDICTION), so P4.4's
;;;; later verdict can just RESOLVE-PREDICTION it -- which, on a
;;;; disconfirmed verdict, automatically fires P5.2's surprise-triggered
;;;; self-model revision for free. Three phases' machinery, one real
;;;; event, no duplicated concept.
;;;;
;;;; a maintenance-tick-triggered EVALUATE-SELF-MOD-OUTCOMES checks
;;;; every accepted, still-pending proposal old enough to judge, comparing
;;;; real tool-call error rate (the codebase's own established "ERROR:"-
;;;; prefix convention on tool-result content, confirmed consistent across
;;;; agent.lisp/self-mod.lisp/runware.lisp/memory-nodes.lisp/web-terminal.lisp/
;;;; repl-drop.lisp/eval-journal.lisp) in the window after acceptance
;;;; against an equal-length window before it. A regression above
;;;; *OUTCOME-EVAL-REGRESSION-THRESHOLD* -> disconfirmed; otherwise
;;;; confirmed; genuinely insufficient data (no tool calls yet in the
;;;; window) -> left pending for a later cycle, up to
;;;; *OUTCOME-EVAL-MAX-AGE-SECONDS*, after which it's finalized
;;;; "inconclusive" rather than staying pending forever.
;;;;
;;;; SCOPE NOTE, stated plainly: the literal deliverable asks for "task
;;;; success rate, error rate, user corrections, token cost" -- a full
;;;; battery that needs P7.2 (metrics tooling), not built. Tool-call error
;;;; rate is the one signal that's real, already logged, and unambiguous
;;;; today; the others are deliberately not fabricated. This is a coarse
;;;; regression proxy, not a causal analysis -- documented as such, not
;;;; oversold. *SELF-MOD-OUTCOME-AUTO-ROLLBACK* defaults to NIL per the
;;;; deliverable's own instruction to gate that behind a flag initially.
;;;;
;;;; Load live (no restart) via lisp-eval or repl-drop, after
;;;; self-mod-sandbox.lisp (propose-loop's existing wrap chain),
;;;; memory-nodes.lisp, prediction-journal.lisp, tick-loop.lisp, and
;;;; event-log.lisp (propose-loop/execute's most recent wrap, and
;;;; REPLAY-EVENTS -- forward-referenced safely below if not loaded yet
;;;; yet, same established idiom as everywhere else in this codebase):
;;;;   (load "/agent/state/self-mod-provenance.lisp")

(in-package :agent)

(export '(self-mod-proposal-history self-mod-outcomes-report evaluate-self-mod-outcomes
          *self-mod-outcome-auto-rollback* call-with-proposal-provenance))

(defparameter *self-mod-proposals-file* #P"/agent/state/self-mod-proposals.json")
(defvar *self-mod-proposals* nil
  "List of proposal records (newest-first): {id, target-symbol,
old-source, new-source, rationale, expected-effect, prediction-id,
proposal-node-id, proposed-at, status (\"accepted\"/\"rejected\"),
result, outcome-status (\"pending\"/\"resolved\"), verdict, verdict-node-id,
resolved-at}.")
(defvar *self-mod-proposal-counter* 0)
(defvar *self-mod-proposals-lock* (bt:make-lock "self-mod-proposals"))

(defparameter *outcome-eval-min-seconds* 3600
  "Don't judge a change before at least this long has passed -- too
little real usage to mean anything.")
(defparameter *outcome-eval-window-seconds* (* 6 3600)
  "Compare equal-length before/after windows, capped at this length.")
(defparameter *outcome-eval-max-age-seconds* (* 3 86400)
  "If still genuinely undecidable after this long (no tool-call activity
in the post-window at all), finalize as \"inconclusive\" rather than
checking forever.")
(defparameter *outcome-eval-regression-threshold* 0.15
  "Absolute error-rate increase (post minus pre-baseline) treated as a
real regression -> disconfirmed. Below this, confirmed.")
(defvar *self-mod-outcome-auto-rollback* nil
  "P4.4's own explicit instruction: gate automatic rollback behind a
config flag, default OFF. When T, a DISCONFIRMED verdict against the most
recently accepted loop calls ROLLBACK-LOOP.")

;;; --- extend the propose-loop tool schema in place -----------------
;;; *TOOLS* is a plain vector produced by self-mod.lisp -- mutated here,
;;; not re-derived, so this stays additive on top of whatever self-mod.lisp
;;; itself defines (which is never edited directly).

(let ((tool (find "propose-loop" *tools* :key (lambda (tl) (ref tl "function" "name")) :test #'string=)))
  (when tool
    (setf (gethash "parameters" (gethash "function" tool))
          (obj "type" "object"
               "properties"
               (obj "source" (obj "type" "string"
                                   "description" "Complete (defun agent-loop (messages) ...) form.")
                    "rationale" (obj "type" "string"
                                     "description" "Why you're proposing this change, in your own words -- recorded as a real memory node, retrievable later via normal recall.")
                    "expected_effect" (obj "type" "string"
                                            "description" "A falsifiable prediction: specifically what should be different afterward. Recorded as a real prediction and checked later against real tool-call error rates.")
                    "confidence" (obj "type" "number"
                                       "description" "0.0-1.0: how confident you are the expected effect will hold. Defaults to 0.6 if omitted."))
               "required" (vector "source" "rationale" "expected_effect")))))

;;; --- carry rationale/expected-effect from the tool call down to
;;; propose-loop itself, via a dynamic variable set in an EXECUTE wrap --
;;; dynamic scope means load order relative to other EXECUTE wraps doesn't
;;; matter (it stays bound through every nested funcall in the same call),
;;; only load order relative to PROPOSE-LOOP's own wrap chain does.

(defvar *pending-proposal-provenance* nil)

(defun call-with-proposal-provenance (tool-call thunk)
  "Bind incumbent structured proposal provenance around one THUNK call."
  (unless (functionp thunk)
    (error "Proposal provenance requires an explicit function."))
  (let* ((args (ignore-errors
                 (shasht:read-json (ref tool-call "function" "arguments"))))
         (*pending-proposal-provenance*
           (obj "rationale"
                (or (and args (gethash "rationale" args)) "(no rationale given)")
                "expected-effect"
                (or (and args (gethash "expected_effect" args))
                    "(no expected effect given)")
                "confidence"
                (or (and args
                         (ignore-errors
                           (float (gethash "confidence" args) 0.0d0)))
                    0.6d0))))
    (multiple-value-call #'values (funcall thunk tool-call))))

(when (or (not (fboundp 'tool-dispatch-legacy-wrapper-enabled-p))
          (funcall 'tool-dispatch-legacy-wrapper-enabled-p))
  (unless (fboundp 'pai-base-execute-provenance)
    (setf (fdefinition 'pai-base-execute-provenance) (fdefinition 'execute)))
  (defun execute (tool-call)
    (let ((name (ignore-errors (ref tool-call "function" "name"))))
      (if (equal name "propose-loop")
          (call-with-proposal-provenance
           tool-call (lambda (call) (funcall 'pai-base-execute-provenance call)))
          (funcall 'pai-base-execute-provenance tool-call)))))

;;; --- the actual record, on every propose-loop call ----------------

(unless (fboundp 'pai-base-propose-loop-provenance)
  (setf (fdefinition 'pai-base-propose-loop-provenance) (fdefinition 'propose-loop)))

(defun propose-loop (proposed-src)
  (let* ((prov (or *pending-proposal-provenance*
                    (obj "rationale" "(no rationale given)" "expected-effect" "(no expected effect given)" "confidence" 0.6d0)))
         (old-source *current-loop-source*)
         (proposal-node-id (memory-write-node
                             :kind "self-mod-proposal"
                             :content (format nil "Proposed a change to agent-loop.~%Rationale: ~a~%Expected effect: ~a"
                                               (gethash "rationale" prov) (gethash "expected-effect" prov))))
         (prediction-id (when (fboundp 'write-prediction)
                           (ignore-errors (funcall 'write-prediction (gethash "expected-effect" prov) (gethash "confidence" prov)))))
         (result (funcall 'pai-base-propose-loop-provenance proposed-src))
         (accepted (and (stringp result) (>= (length result) 8) (string= (subseq result 0 8) "APPROVED"))))
    (bt:with-lock-held (*self-mod-proposals-lock*)
      (incf *self-mod-proposal-counter*)
      (let ((record (obj "id" *self-mod-proposal-counter* "target-symbol" "agent-loop"
                          "old-source" old-source "new-source" proposed-src
                          "rationale" (gethash "rationale" prov) "expected-effect" (gethash "expected-effect" prov)
                          "prediction-id" (or prediction-id :null)
                          "motivating-event-id" (or (and (boundp '*current-causing-event-id*) *current-causing-event-id*) :null)
                          "proposal-node-id" proposal-node-id
                          "proposed-at" (get-universal-time)
                          "status" (if accepted "accepted" "rejected")
                          "result" result
                          "outcome-status" "pending" "verdict" :null "verdict-node-id" :null "resolved-at" :null)))
        (push record *self-mod-proposals*)
        (let ((outcome-node-id (memory-write-node
                                 :kind "self-mod-proposal-outcome"
                                 :content (format nil "~a: ~a" (if accepted "Accepted" "Rejected") result))))
          (ignore-errors (memory-add-edge proposal-node-id outcome-node-id "proposal-outcome")))
        (ignore-errors (save-self-mod-proposals))
        (when (fboundp 'log-event)
          (ignore-errors (funcall 'log-event "self-mod-provenance-recorded"
                                   (obj "id" (gethash "id" record) "status" (gethash "status" record)))))))
    result))

;;; --- outcome attribution -------------------------------------------

(defun %self-mod-tool-error-rate (from to)
  "FROM/TO are universal-time bounds. Returns (values rate total) over
real tool-result events in that window, using the codebase's own
established \"ERROR\"-prefix convention on tool-result content. (values
NIL 0) means genuinely no data in the window -- distinct from a real 0%
error rate, never conflated with it."
  (let* ((events (replay-events :from from :to (max from (1- to))))
         (results (remove-if-not (lambda (e) (equal (gethash "type" e) "tool-result")) events))
         (total (length results))
         (errors (count-if (lambda (e)
                              (let ((c (gethash "content" (gethash "payload" e))))
                                (and (stringp c) (>= (length c) 5) (string= (subseq c 0 5) "ERROR"))))
                            results)))
    (if (zerop total) (values nil 0) (values (/ (float errors 1.0d0) total) total))))

(defun %self-mod-finalize-verdict (record verdict post-rate baseline post-n)
  (let* ((proposal-node-id (gethash "proposal-node-id" record))
         (evidence-text (if post-rate
                             (format nil "Tool-call error rate after acceptance: ~,2f (n=~a) vs. baseline ~,2f before."
                                     post-rate post-n (or baseline 0.0d0))
                             "Not enough real tool-call activity accumulated to judge either way."))
         (verdict-node-id (memory-write-node
                            :kind "self-mod-verdict"
                            :content (format nil "Verdict on proposal ~a (expected: ~a): ~a. ~a"
                                              (gethash "id" record) (gethash "expected-effect" record) verdict evidence-text))))
    (ignore-errors (memory-add-edge proposal-node-id verdict-node-id "verdict-for"))
    (setf (gethash "outcome-status" record) "resolved"
          (gethash "verdict" record) verdict
          (gethash "verdict-node-id" record) verdict-node-id
          (gethash "resolved-at" record) (get-universal-time))
    (ignore-errors (save-self-mod-proposals))
    (when (fboundp 'log-event)
      (ignore-errors (funcall 'log-event "self-mod-outcome-verdict" (obj "id" (gethash "id" record) "verdict" verdict))))
    (when (and (not (equal (gethash "prediction-id" record) :null)) (not (string= verdict "inconclusive"))
               (fboundp 'resolve-prediction))
      (ignore-errors (funcall 'resolve-prediction (gethash "prediction-id" record) (string= verdict "confirmed") evidence-text)))
    (when (and *self-mod-outcome-auto-rollback* (string= verdict "disconfirmed") (fboundp 'rollback-loop))
      (ignore-errors
       (funcall 'rollback-loop)
       (when (fboundp 'log-event)
         (ignore-errors (funcall 'log-event "self-mod-auto-rollback" (obj "id" (gethash "id" record)))))))
    verdict))

(defun evaluate-self-mod-outcomes (&key ignore-age)
  "Called from the maintenance tick. Judges every accepted, still-pending
proposal old enough (or all of them, with IGNORE-AGE T, for manual/
testing use) against real tool-call error rate before vs. after
acceptance. Never signals -- one bad record must not block the rest."
  (let ((now (get-universal-time)))
    (dolist (record *self-mod-proposals*)
      (ignore-errors
       (when (and (string= (gethash "status" record) "accepted")
                  (string= (gethash "outcome-status" record) "pending"))
         (let* ((proposed-at (gethash "proposed-at" record))
                (elapsed (- now proposed-at)))
           (when (or ignore-age (>= elapsed *outcome-eval-min-seconds*))
             (let* ((window (max 1 (min elapsed *outcome-eval-window-seconds*)))
                    (post-from proposed-at) (post-to (+ proposed-at window))
                    (pre-from (- proposed-at window)) (pre-to proposed-at))
               (multiple-value-bind (post-rate post-n) (%self-mod-tool-error-rate post-from post-to)
                 (multiple-value-bind (pre-rate pre-n) (%self-mod-tool-error-rate pre-from pre-to)
                   (declare (ignore pre-n))
                   (cond
                     ((null post-rate)
                      (when (or ignore-age (>= elapsed *outcome-eval-max-age-seconds*))
                        (%self-mod-finalize-verdict record "inconclusive" nil nil post-n)))
                     (t
                      (let* ((baseline (or pre-rate 0.0d0))
                             (delta (- post-rate baseline))
                             (verdict (if (> delta *outcome-eval-regression-threshold*) "disconfirmed" "confirmed")))
                        (%self-mod-finalize-verdict record verdict post-rate baseline post-n)))))))))))))
  :done)

;;; --- introspection --------------------------------------------------------

(defun self-mod-proposal-history (&optional n)
  "Newest-first proposal records, optionally limited to the first N."
  (let ((sorted (sort (copy-list *self-mod-proposals*) #'> :key (lambda (r) (gethash "id" r)))))
    (if n (subseq sorted 0 (min n (length sorted))) sorted)))

(defun self-mod-outcomes-report ()
  "Which of my own self-modifications worked, so far -- confirmed/
disconfirmed/inconclusive counts plus per-proposal detail, real evidence
behind each verdict rather than a guess."
  (let* ((resolved (remove-if-not (lambda (r) (string= (gethash "outcome-status" r) "resolved")) *self-mod-proposals*))
         (pending (remove-if (lambda (r) (or (not (string= (gethash "status" r) "accepted"))
                                              (not (string= (gethash "outcome-status" r) "pending"))))
                              *self-mod-proposals*)))
    (obj "confirmed" (count-if (lambda (r) (equal (gethash "verdict" r) "confirmed")) resolved)
         "disconfirmed" (count-if (lambda (r) (equal (gethash "verdict" r) "disconfirmed")) resolved)
         "inconclusive" (count-if (lambda (r) (equal (gethash "verdict" r) "inconclusive")) resolved)
         "pending" (length pending)
         "details" (coerce (mapcar (lambda (r) (obj "id" (gethash "id" r) "rationale" (gethash "rationale" r)
                                                      "expected-effect" (gethash "expected-effect" r)
                                                      "verdict" (gethash "verdict" r)))
                                    resolved)
                            'vector))))

;;; --- run on every maintenance tick, rename-and-fall-through --------

(unless (fboundp 'pai-base-tick-handle-maintenance-outcomes)
  (setf (fdefinition 'pai-base-tick-handle-maintenance-outcomes) (fdefinition '%tick-handle-maintenance)))
(defun %tick-handle-maintenance ()
  (funcall 'pai-base-tick-handle-maintenance-outcomes)
  (ignore-errors (evaluate-self-mod-outcomes)))

;;; --- persistence ------------------------------------------------------

(defun save-self-mod-proposals ()
  (let ((tmp (make-pathname :name (concatenate 'string (pathname-name *self-mod-proposals-file*) "-tmp")
                            :type (pathname-type *self-mod-proposals-file*) :defaults *self-mod-proposals-file*)))
    (with-open-file (out tmp :direction :output :if-exists :supersede
                         :if-does-not-exist :create :external-format :utf-8)
      ;; *PRINT-PRETTY* NIL -- the standing SHASHT:WRITE-JSON/JSONL gotcha,
      ;; recurred multiple times already this session.
      (let ((*print-pretty* nil)) (shasht:write-json (coerce *self-mod-proposals* 'vector) out)))
    (rename-file tmp *self-mod-proposals-file*)))

(defun load-self-mod-proposals ()
  (handler-case
      (when (probe-file *self-mod-proposals-file*)
        (with-open-file (in *self-mod-proposals-file*)
          (let ((data (shasht:read-json in)))
            (setf *self-mod-proposals* (coerce data 'list))
            (setf *self-mod-proposal-counter*
                  (reduce #'max *self-mod-proposals* :key (lambda (r) (gethash "id" r)) :initial-value 0)))))
    (error (e) (format t "~&[self-mod-provenance] load failed, starting empty: ~a~%" e) nil)))

(define-init :restore self-mod-provenance-restore
    "Restore durable state for self-mod-provenance."
  (load-self-mod-proposals))
