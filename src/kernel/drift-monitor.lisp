;;;; drift-monitor.lisp -- continuous heap-vs-disk drift
;;;; detection and auto-capture, replacing A.5 (core snapshot) and A.7
;;;; (restore drill) as originally scoped.
;;;;
;;;; Rationale (2026-07-27): A.5/A.7 depended on a MANUAL, human-triggered
;;;; save (a5-save-core) -- but the actual threat Phase Alpha exists to
;;;; protect against is an UNINTENDED outage, one nobody gets a chance to
;;;; trigger a save before. A.5's own live trigger also has a confirmed,
;;;; unfixed bug (SB-THREAD:INTERRUPT-THREAD doesn't reliably reach the
;;;; main thread while it's blocked on a stdin read) -- so even the manual
;;;; path isn't currently reliable. Manual snapshotting is the wrong shape
;;;; for this problem regardless of whether that bug gets fixed.
;;;;
;;;; The actual risk: the agent has self-modification tools (lisp-eval,
;;;; propose-loop) that can create or change a function purely in the live
;;;; heap, with nothing on disk reflecting it -- exactly what happened to
;;;; AGENT-LOOP itself before A.3 recovered it. Nothing currently stops
;;;; that from happening again to some OTHER function, silently, with no
;;;; alert, and a fresh boot (or a restore from any backup) would just
;;;; silently lose it.
;;;;
;;;; This runs a background thread that periodically diffs every FBOUNDP
;;;; function in the :AGENT package against a baseline (by name + a hash of
;;;; its FUNCTION-LAMBDA-EXPRESSION) and, for anything new or changed,
;;;; writes its current source to disk automatically -- so "only in
;;;; memory" stops being a state any function can be in for more than one
;;;; scan interval.
;;;;
;;;; Scope: functions only, this first pass. Special variables (DEFVAR/
;;;; DEFPARAMETER) get a lighter, secondary pass below -- existence alerts
;;;; only, not full value capture, since arbitrary Lisp values (hash-
;;;; tables, circular structures) don't have one safe universal on-disk
;;;; representation the way function source does. The load-bearing
;;;; variables (conversation, memory graph) already have dedicated
;;;; persistence (conversation-persistence.lisp, enhancements.lisp);
;;;; this is a safety net for whatever isn't covered yet, not a
;;;; replacement for those.
;;;;
;;;; Load live (no restart) via the agent's own lisp-eval tool, or repl-drop:
;;;;   (load "/agent/state/drift-monitor.lisp")

(in-package :agent)

(export '(drift-monitor-start drift-monitor-stop drift-monitor-scan-now))

(defparameter *drift-monitor-interval-seconds* 300
  "5 minutes. Cheap to run (a few hundred symbol lookups + hashing), no
need for anything tighter -- this is a safety net, not a real-time guard.")

(defparameter *drift-capture-dir*
  (let ((root (or (sb-ext:posix-getenv "PAI_STAGED_PROPOSAL_ROOT")
                  (sb-ext:posix-getenv "PAI_R3A_STAGED_PROPOSAL_ROOT"))))
    (if (and root (> (length root) 0))
        (merge-pathnames "captured-functions/" (pathname root))
        #P"/agent/state/captured-functions/")))
(defparameter *drift-log-file*
  (merge-pathnames "capture-log.jsonl" *drift-capture-dir*))

(defvar *drift-fn-baseline* (make-hash-table :test #'equal)
  "Maps function-name-string -> fingerprint-string, for every :AGENT
function seen on the most recent scan.")

(defvar *drift-var-baseline* (make-hash-table :test #'equal)
  "Maps variable-name-string -> T, for every bound :AGENT special variable
seen on the most recent scan. Existence tracking only -- see file header.")

(defvar *drift-monitor-thread* nil)
(defvar *drift-monitor-stop-requested* nil)
(defvar *drift-first-scan-done* nil
  "The first scan seeds both baselines from whatever's already loaded --
that's the known-good starting point, not drift, so nothing is captured
until the SECOND scan onward.")

(defun %drift-now-iso8601 ()
  (multiple-value-bind (sec min hour day month year)
      (decode-universal-time (get-universal-time) 0)
    (format nil "~a-~2,'0d-~2,'0dT~2,'0d~2,'0d~2,'0dZ" year month day hour min sec)))

(defun %drift-own-function-symbols ()
  "Every symbol actually interned in :AGENT (not inherited from :CL etc.)
that names a function -- excludes macros and special operators, which
don't have a meaningful FUNCTION-LAMBDA-EXPRESSION to capture."
  (let ((pkg (find-package :agent))
        (out nil))
    (do-symbols (sym pkg)
      (when (and (eq (symbol-package sym) pkg)
                 (fboundp sym)
                 (not (macro-function sym))
                 (not (special-operator-p sym)))
        (push sym out)))
    out))

(defun %drift-own-variable-symbols ()
  "Every symbol actually interned in :AGENT that's currently BOUND as a
special variable."
  (let ((pkg (find-package :agent))
        (out nil))
    (do-symbols (sym pkg)
      (when (and (eq (symbol-package sym) pkg) (boundp sym) (not (keywordp sym)))
        (push sym out)))
    out))

(defun %drift-fingerprint (sym)
  "A cheap fingerprint for SYM's current function definition: the printed
form of its FUNCTION-LAMBDA-EXPRESSION when available (the same mechanism
that recovered AGENT-LOOP for A.3), else a fallback based on the function
object's identity -- still detects a redefinition (always a new function
object), just can't be diffed further without the expression."
  (let* ((fn (symbol-function sym))
         (expr (ignore-errors (function-lambda-expression fn))))
    (if expr
        (let ((*package* (find-package :agent)))
          (prin1-to-string expr))
        (format nil "~a" fn))))

(defun %drift-render-defun (sym expr)
  "EXPR is (LAMBDA lambda-list . body), from FUNCTION-LAMBDA-EXPRESSION.
Render as a loadable DEFUN form. Best-effort, not a byte-perfect
reconstruction of the original source (comments and original formatting
are gone by the time it's a lambda-expression) -- the goal is a genuinely
loadable capture, not a cosmetic match."
  (let ((*package* (find-package :agent)))
    (destructuring-bind (lambda-kw lambda-list &rest body) expr
      (declare (ignore lambda-kw))
      (with-output-to-string (s)
        (format s "(defun ~(~a~) ~s~%" (symbol-name sym) lambda-list)
        (dolist (form body)
          (format s "  ~s~%" form))
        (format s ")~%")))))

(defun %drift-write-function-capture (sym reason)
  "Write SYM's current source to a dated file under *DRIFT-CAPTURE-DIR*,
and append one line to *DRIFT-LOG-FILE*. If FUNCTION-LAMBDA-EXPRESSION
isn't available, the file still records that the function exists and that
source couldn't be introspected -- an existence alert is still useful, not
nothing."
  (ensure-directories-exist *drift-capture-dir*)
  (let* ((stamp (%drift-now-iso8601))
         (expr (ignore-errors (function-lambda-expression (symbol-function sym))))
         (path (merge-pathnames (format nil "~(~a~)-~a.lisp" (symbol-name sym) stamp)
                                 *drift-capture-dir*)))
    (handler-case
        (with-open-file (out path :direction :output :if-exists :supersede
                                  :if-does-not-exist :create :external-format :utf-8)
          (format out ";;;; auto-captured by drift-monitor.lisp~%")
          (format out ";;;; reason: ~a~%;;;; captured: ~a~%" reason stamp)
          (format out ";;;; recovered live via FUNCTION-LAMBDA-EXPRESSION, same~%")
          (format out ";;;; mechanism A.3 used to recover AGENT-LOOP. Review before~%")
          (format out ";;;; trusting -- this is a safety net, not a substitute for~%")
          (format out ";;;; deliberately saving something to a permanent file.~%~%")
          (format out "(in-package :agent)~%~%")
          (if expr
              (write-string (%drift-render-defun sym expr) out)
              (format out ";; FUNCTION-LAMBDA-EXPRESSION unavailable for ~(~a~) --~%;; existence noted, source not recovered.~%"
                      (symbol-name sym))))
      (error (e)
        (with-open-file (out path :direction :output :if-exists :supersede
                                  :if-does-not-exist :create :external-format :utf-8)
          (format out ";;;; auto-capture FAILED for ~(~a~): ~a~%" (symbol-name sym) e))))
    (handler-case
        (with-open-file (log *drift-log-file* :direction :output :if-exists :append
                                  :if-does-not-exist :create :external-format :utf-8)
          (write-string
           (shasht:write-json
            (obj "ts" stamp "kind" "function"
                 "name" (string-downcase (symbol-name sym))
                 "reason" reason
                 "captured_to" (namestring path)
                 "source_available" (if expr t nil))
            nil)
           log)
          (terpri log))
      (error (e) (format t "~&[drift-monitor] log-write failed: ~a~%" e)))
    path))

(defun %drift-log-variable-alert (sym reason)
  (ensure-directories-exist *drift-capture-dir*)
  (let ((stamp (%drift-now-iso8601))
        (value-summary (handler-case
                            (let ((v (symbol-value sym)))
                              (format nil "~a: ~a" (type-of v)
                                      (let ((s (prin1-to-string v)))
                                        (subseq s 0 (min 200 (length s))))))
                          (error (e) (format nil "(unprintable: ~a)" e)))))
    (handler-case
        (with-open-file (log *drift-log-file* :direction :output :if-exists :append
                                  :if-does-not-exist :create :external-format :utf-8)
          (write-string
           (shasht:write-json
            (obj "ts" stamp "kind" "variable"
                 "name" (string-downcase (symbol-name sym))
                 "reason" reason "value_summary" value-summary)
            nil)
           log)
          (terpri log))
      (error (e) (format t "~&[drift-monitor] variable log-write failed: ~a~%" e)))))

(defun drift-monitor-scan-now ()
  "Run one scan pass immediately (also called periodically by the
background thread). Returns a list of (name . path-or-nil) for whatever
was captured this pass -- empty on the very first call, since that call
only seeds the baseline."
  (let ((captured nil))
    (dolist (sym (%drift-own-function-symbols))
      (let* ((name (symbol-name sym))
             (fp (handler-case (%drift-fingerprint sym) (error () nil)))
             (prev (gethash name *drift-fn-baseline*)))
        (when fp
          (cond
            ((null prev)
             (setf (gethash name *drift-fn-baseline*) fp)
             (when *drift-first-scan-done*
               (handler-case
                   (push (cons name (%drift-write-function-capture sym "new function detected"))
                         captured)
                 (error (e) (format t "~&[drift-monitor] capture failed for ~a: ~a~%" sym e)))))
            ((not (string= prev fp))
             (setf (gethash name *drift-fn-baseline*) fp)
             (handler-case
                 (push (cons name (%drift-write-function-capture sym "definition changed"))
                       captured)
               (error (e) (format t "~&[drift-monitor] capture failed for ~a: ~a~%" sym e))))))))
    (dolist (sym (%drift-own-variable-symbols))
      (let* ((name (symbol-name sym))
             (seen (gethash name *drift-var-baseline*)))
        (unless seen
          (setf (gethash name *drift-var-baseline*) t)
          (when *drift-first-scan-done*
            (handler-case (%drift-log-variable-alert sym "new variable detected")
              (error (e) (format t "~&[drift-monitor] variable alert failed for ~a: ~a~%" sym e)))))))
    (setf *drift-first-scan-done* t)
    (nreverse captured)))

(defun drift-monitor-start ()
  "Idempotent: safe to call again after reloading this file -- won't spawn
a second monitor thread if one is already alive."
  (unless (and *drift-monitor-thread* (bt:thread-alive-p *drift-monitor-thread*))
    (setf *drift-monitor-stop-requested* nil)
    (setf *drift-monitor-thread*
          (bt:make-thread
           (lambda ()
             (loop until *drift-monitor-stop-requested*
                   do (handler-case
                          (let ((captured (drift-monitor-scan-now)))
                            (when captured
                              (format t "~&[drift-monitor] captured ~a new/changed function(s): ~{~a~^, ~}~%"
                                      (length captured) (mapcar #'car captured))))
                        (error (e) (format t "~&[drift-monitor] scan error: ~a~%" e)))
                      (sleep *drift-monitor-interval-seconds*)))
           :name "drift-monitor")))
  (format t "~&[drift-monitor] watching :AGENT functions/variables every ~as, capturing to ~a~%"
          *drift-monitor-interval-seconds* *drift-capture-dir*))

(defun drift-monitor-stop (&optional (timeout 5))
  "Gracefully stop the monitor thread: request stop, then wait (up to
TIMEOUT seconds) for it to exit on its own between scans, same pattern as
REPL-DROP-STOP."
  (setf *drift-monitor-stop-requested* t)
  (loop with deadline = (+ (get-internal-real-time) (* timeout internal-time-units-per-second))
        while (and *drift-monitor-thread* (bt:thread-alive-p *drift-monitor-thread*)
                   (< (get-internal-real-time) deadline))
        do (sleep 0.05))
  (if (and *drift-monitor-thread* (bt:thread-alive-p *drift-monitor-thread*))
      (progn (ignore-errors (bt:destroy-thread *drift-monitor-thread*)) :force-killed)
      :stopped-cleanly))

(define-init :start drift-monitor-start
    "Start background worker for drift-monitor."
  (drift-monitor-start))
