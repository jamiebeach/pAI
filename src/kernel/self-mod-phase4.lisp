;;;; self-mod-phase4.lisp -- P4.6 (immutable core whitelist) + P4.3
;;;; (versioned rollback), 2026-07-28. Built directly on top of
;;;; self-mod-verifier-calibration.lisp's evidence-based recalibration --
;;;; both files wrap/extend self-mod.lisp's existing propose-loop
;;;; machinery rather than replacing it, per the same "narrow hard
;;;; restrictions, don't block real work" calibration from that file.
;;;;
;;;; Load live (no restart) via lisp-eval or repl-drop, AFTER
;;;; self-mod-verifier-calibration.lisp:
;;;;   (load "/agent/state/self-mod-phase4.lisp")

(in-package :agent)

;;; --- immutable core whitelist --------------------------------------
;;; self-mod.lisp's STATIC-CHECK already hardcodes a 3-symbol check
;;; (raw-call-model, *calls-remaining*, *max-calls*) inline. This widens
;;; that into one explicit, named, documented list covering everything
;;; P4.6 actually calls for -- the review pipeline itself and the budget
;;; primitive -- WITHOUT trying to additionally sandbox LISP-EVAL, which
;;; is unrestricted by design (it's the agent's literal hands; see
;;; agent.lisp's own docstring) and would be a much larger, much more
;;; restrictive change than what was asked for. Being honest about that
;;; scope boundary in the docs below rather than overstating what this
;;; actually protects.
(defparameter *immutable-core-symbols*
  '("RAW-CALL-MODEL" "*CALLS-REMAINING*" "*MAX-CALLS*"
    "PROPOSE-LOOP" "VERIFY-WITH-MODEL" "*CONSTITUTION*" "STATIC-CHECK"
    "SNAPSHOT-LOOP" "ROLLBACK-LOOP" "*LOOP-SNAPSHOT*"
    "*IMMUTABLE-CORE-SYMBOLS*")
  "Symbols an AGENT-LOOP proposal may never reference, checked by
STATIC-CHECK before the verifier ever sees the proposal. This is the
machinery that reviews and can undo self-modification -- if a proposed
loop could touch it, a bad proposal could disable its own oversight.
Deliberately narrow: this list is the review/rollback/budget pipeline
ITSELF, not a broad 'agent-loop changes are risky' posture -- ordinary
agent-loop logic (retries, tool orchestration, logging, control flow) is
untouched by this and reviewed only by the (now recalibrated) verifier.

Scope note, stated plainly rather than implied: this protects the
PROPOSE-LOOP surface specifically, since that is the agent's only mechanized,
reviewed self-modification path (it only ever installs (defun agent-loop
...) forms). LISP-EVAL remains a separate, unrestricted surface by
design -- it could technically (defun call-model ...) directly through
it, bypassing this whitelist entirely. That boundary is upheld by
instruction/judgment today, not a technical barrier, and deliberately
not closed here -- doing so would be exactly the kind of broad,
restrictive change the operator asked this work to avoid.")

(unless (fboundp 'pai-base-static-check-p46)
  (setf (fdefinition 'pai-base-static-check-p46) (fdefinition 'static-check)))
(defun static-check (src)
  (multiple-value-bind (ok reason) (funcall 'pai-base-static-check-p46 src)
    (if (not ok)
        (values ok reason)
        (multiple-value-bind (form err) (safe-read src)
          (declare (ignore err))
          (if (and form (tree-mentions-p (cddr form) *immutable-core-symbols*))
              (values nil "loop may not reference protected core-safety machinery (immutable core, P4.6)")
              (values t reason))))))

;;; --- versioned code journal and rollback ---------------------------
;;; self-mod.lisp's own SNAPSHOT-LOOP/ROLLBACK-LOOP hold exactly ONE prior
;;; version (the immediately-preceding one), used internally by
;;; PROPOSE-LOOP as a fast safety net if a freshly-approved install
;;; itself errors. That's still exactly right for that job and is left
;;; untouched. This adds a SEPARATE, persistent, full history on top --
;;; every accepted install, not just the last one -- so a deliberate,
;;; later "actually, go back to version 7" is possible, not just "undo
;;; the most recent change."

(defparameter *loop-version-journal-file* #P"/agent/state/loop-versions.jsonl")
(defvar *loop-versions* nil
  "Alist (version-number . source-string), newest first. Version numbers
are monotonically increasing across the process's lifetime; entries
persist to disk so they survive a restart.")
(defvar *loop-version-counter* 0)

(defun %loop-journal-append (source-string)
  (incf *loop-version-counter*)
  (push (cons *loop-version-counter* source-string) *loop-versions*)
  (ignore-errors
   (with-open-file (out *loop-version-journal-file* :direction :output
                         :if-exists :append :if-does-not-exist :create
                         :external-format :utf-8)
     ;; *PRINT-PRETTY* T (the default in some contexts) makes SHASHT:WRITE-
     ;; JSON emit multi-line, pretty-printed output -- fatal for a JSONL
     ;; file, since each physical line is expected to be one whole record.
     ;; The exact same bug already bit WEB-V2.LISP, EVAL-JOURNAL.LISP, and
     ;; P0.2's EVENT-LOG.LISP before this file; a fourth occurrence, worth
     ;; remembering as a standing SHASHT:WRITE-JSON gotcha every time a new
     ;; JSONL writer gets added anywhere in this codebase.
     (let ((*print-pretty* nil))
       (write-string (shasht:write-json
                       (obj "version" *loop-version-counter*
                            "source" source-string
                            "timestamp" (get-universal-time))
                       nil)
                      out))
     (terpri out))))

(defun load-loop-versions ()
  (handler-case
      (when (probe-file *loop-version-journal-file*)
        (with-open-file (in *loop-version-journal-file*)
          (loop for line = (read-line in nil nil)
                while line
                when (plusp (length (string-trim '(#\Space #\Return) line)))
                  do (let* ((entry (shasht:read-json line))
                            (v (gethash "version" entry))
                            (src (gethash "source" entry)))
                       (push (cons v src) *loop-versions*)
                       (setf *loop-version-counter* (max *loop-version-counter* v))))))
    (error (e) (format t "~&[self-mod-phase4] loop-version journal load failed: ~a~%" e) nil))
  (setf *loop-versions* (sort *loop-versions* #'> :key #'car)))

(defun loop-version-history ()
  "Newest-first list of (version . source), for introspection."
  (reverse (reverse *loop-versions*)))

(defun rollback-to-version (n)
  "Restore agent-loop to the exact source recorded as version N. Returns a
human-readable result string, same convention as PROPOSE-LOOP."
  (let ((entry (assoc n *loop-versions*)))
    (if (not entry)
        (format nil "No such version: ~a (known versions: ~{~a~^, ~})"
                n (mapcar #'car *loop-versions*))
        (handler-case
            (progn
              (eval (safe-read (cdr entry)))
              (setf *current-loop-source* (cdr entry))
              (when (fboundp 'log-event)
                (ignore-errors (funcall 'log-event "self-mod-rolled-back"
                                         (obj "target_version" n "method" "explicit-version-rollback"))))
              (format nil "Rolled back to version ~a. Effective on your next turn." n))
          (error (e)
            (format nil "Rollback to version ~a failed: ~a" n e))))))

(unless (fboundp 'pai-base-propose-loop-p43)
  (setf (fdefinition 'pai-base-propose-loop-p43) (fdefinition 'propose-loop)))
(defun propose-loop (proposed-src)
  (let ((result (funcall 'pai-base-propose-loop-p43 proposed-src)))
    (when (and (stringp result) (>= (length result) 8)
               (string= (subseq result 0 8) "APPROVED"))
      (ignore-errors (%loop-journal-append *current-loop-source*)))
    result))

(define-init :restore self-mod-phase4-restore
    "Restore durable state for self-mod-phase4."
  (load-loop-versions))
