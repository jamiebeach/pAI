;;;; attention-schema.lisp -- P5.3, 2026-07-28.
;;;;
;;;; A principled basis for "what am I paying attention to" rather than
;;;; vibes -- Graziano's attention schema idea: maintain a MODEL of your
;;;; own attention (not the raw process itself), and answer introspective
;;;; questions FROM that model. The tick loop's own weighted tick-type
;;;; selection (%TICK-TYPE-WEIGHTS / %TICK-SELECT-TYPE, tick-loop.lisp)
;;;; is the natural substrate: each tick IS a "cycle," and the weights
;;;; that go into picking one already encode something like salience.
;;;;
;;;; Structure, per the backlog: current-focus, why-focused, what-was-
;;;; displaced, confidence-in-focus -- recomputed on every tick-type
;;;; selection by wrapping %TICK-SELECT-TYPE (rename-and-fall-through,
;;;; same idiom as everywhere else; this is its first and only wrap, now
;;;; registered in *WRAP-CHAINS*).
;;;;
;;;; CRITICAL, per the backlog's own words: the schema is SOMETIMES WRONG
;;;; about the underlying process, and that's not a bug to paper over --
;;;; it's the interesting part. %TICK-SELECT-TYPE draws from a WEIGHTED
;;;; RANDOM distribution, not a greedy argmax, so the schema's natural
;;;; story ("I focused on X because it had the highest weight") is
;;;; sometimes simply false -- a lower-weighted option won the draw by
;;;; chance. SCHEMA-MATCHED-PROCESS-P records exactly this, every single
;;;; update, and ATTENTION-DIVERGENCE-RATE surfaces how often it happens
;;;; -- a real, logged, queryable measure of self-model inaccuracy, not a
;;;; hidden footnote.
;;;;
;;;; Load live (no restart) via lisp-eval or repl-drop, after
;;;; tick-loop.lisp (wraps %TICK-SELECT-TYPE) and, ideally,
;;;; conversation-persistence.lisp's continuity buffer (CONTINUITY-
;;;; BUFFER-APPEND -- guarded with FBOUNDP, so not a hard requirement):
;;;;   (load "/agent/state/attention-schema.lisp")

(in-package :agent)

(export '(attention-schema attention-report attention-divergence-rate))

(defvar *attention-schema* nil
  "Current schema snapshot: {current-focus, why-focused, what-was-
displaced, confidence-in-focus, schema-matched-process, updated-at}. NIL
until the first tick-type selection after this file loads.")

(defvar *attention-schema-log* nil
  "Bounded history (newest-first, capped at *ATTENTION-SCHEMA-LOG-CAP*)
of {type, matched, confidence, at} -- enough to compute a real divergence
rate without re-deriving it from the full event log every call.")
(defparameter *attention-schema-log-cap* 200)

(defun attention-schema () *attention-schema*)

(defun %attention-schema-update (selected weights)
  "SELECTED is the tick type %TICK-SELECT-TYPE actually picked; WEIGHTS
is a fresh %TICK-TYPE-WEIGHTS call (a pure read of current modulator
values, safe to call a second time same as the real selection call did)
-- used here purely for introspection, never to influence SELECTED
itself. Building and logging the schema is this function's whole job."
  (let* ((total (loop for v being the hash-values of weights sum v))
         (highest (let (best-k (best-v -1.0d0))
                    (loop for k being the hash-keys of weights using (hash-value v)
                          when (> v best-v) do (setf best-k k best-v v))
                    best-k))
         (selected-weight (gethash selected weights 0.0))
         (highest-weight (gethash highest weights 0.0))
         (matched (string= selected highest))
         (displaced (remove selected (loop for k being the hash-keys of weights collect k) :test #'string=))
         (confidence (if (plusp total) (/ selected-weight (float total 0.0d0)) 0.0d0)))
    (setf *attention-schema*
          (obj "current-focus" selected
               "why-focused"
               (if matched
                   (format nil "highest-weighted option (~,2f of ~,2f total weight)" selected-weight total)
                   (format nil "picked by the weighted draw even though ~s carried more weight (~,2f vs ~,2f) -- not what the usual \"I focused on it because it mattered most\" story would predict"
                           highest highest-weight selected-weight))
               "what-was-displaced" (coerce displaced 'vector)
               "confidence-in-focus" confidence
               "schema-matched-process" matched
               "updated-at" (get-universal-time)))
    (push (obj "type" selected "matched" matched "confidence" confidence "at" (get-universal-time))
          *attention-schema-log*)
    (when (> (length *attention-schema-log*) *attention-schema-log-cap*)
      (setf *attention-schema-log* (subseq *attention-schema-log* 0 *attention-schema-log-cap*)))
    (when (fboundp 'continuity-buffer-append)
      (ignore-errors
       (continuity-buffer-append
        (if matched
            (format nil "Attention settled on ~a." selected)
            (format nil "Attention landed on ~a -- not what I'd have expected given the weights." selected)))))
    (when (fboundp 'log-event)
      (ignore-errors (funcall 'log-event "attention-schema-update" *attention-schema*))
      (unless matched
        (ignore-errors
         (funcall 'log-event "attention-schema-divergence"
                  (obj "selected" selected "selected-weight" selected-weight
                       "expected-highest" highest "highest-weight" highest-weight)))))
    *attention-schema*))

(defun attention-report ()
  "Natural-language answer to \"what are you paying attention to, and
what are you ignoring?\" -- drawn from the SCHEMA (the model of the
process), not re-derived from tick internals fresh each call. That's the
point of an attention schema: the report can be wrong in exactly the way
a real self-report can be wrong, and ATTENTION-DIVERGENCE-RATE says how
often."
  (if (not *attention-schema*)
      "No attention cycle has run yet."
      (format nil "Right now: ~a (~a). Not: ~{~a~^, ~}."
              (gethash "current-focus" *attention-schema*)
              (gethash "why-focused" *attention-schema*)
              (coerce (gethash "what-was-displaced" *attention-schema*) 'list))))

(defun attention-divergence-rate (&optional (n 50))
  "Fraction of the last N attention-schema updates (default 50) where
the schema's own story didn't match what actually got selected -- the
honest, logged rate at which this self-model is simply wrong. :NULL if
no updates have happened yet, not a fabricated 0."
  (let ((recent (subseq *attention-schema-log* 0 (min n (length *attention-schema-log*)))))
    (if (null recent)
        :null
        (/ (float (count-if (lambda (e) (not (gethash "matched" e))) recent) 0.0d0) (length recent)))))

;;; --- wrap %TICK-SELECT-TYPE, rename-and-fall-through --------------------
;;; First and only wrap on this function -- registered below in
;;; *WRAP-CHAINS* so a future live edit to tick-loop.lisp knows to
;;; reload this file afterward too.

(unless (fboundp 'pai-base-tick-select-type)
  (setf (fdefinition 'pai-base-tick-select-type) (fdefinition '%tick-select-type)))

(defun %tick-select-type ()
  (let* ((weights (%tick-type-weights))
         (selected (funcall 'pai-base-tick-select-type)))
    (ignore-errors (%attention-schema-update selected weights))
    selected))
