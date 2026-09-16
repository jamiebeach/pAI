;;;; eval-journal.lisp -- journal the eval seam.
;;;;
;;;; Wraps LISP-EVAL and PROPOSE-LOOP (self-mod.lisp) so every form that
;;;; could bring a new definition into existence is written, timestamped,
;;;; to an append-only JSONL file BEFORE it's evaluated -- not after, and
;;;; not only when someone remembers to snapshot. This is the fix for
;;;; exactly what A.1/A.3 found: agent-loop, save-graph, serialize-graph,
;;;; load-graph, and a dozen other functions existed ONLY as live state
;;;; with nothing on disk, recoverable only because FUNCTION-LAMBDA-
;;;; EXPRESSION happened to still hold their source.
;;;;
;;;; Fails closed: if the journal write itself errors, the eval is never
;;;; reached -- caught and reported the same way this codebase already
;;;; reports every other eval/propose-loop failure (a plain "ERROR: "/
;;;; "REJECTED (...)" string), never an uncaught condition that could
;;;; blow out the whole turn.
;;;;
;;;; Acceptance (per the backlog): a new eval'd function appears in the
;;;; journal file on disk before it is callable. Killing the process
;;;; immediately after an eval loses nothing.
;;;;
;;;; Deployed via the A.0 repl-drop channel, not through the agent's own
;;;; lisp-eval -- per the backlog's own channel rule, HUMAN-AUTHORED work
;;;; routes through A.0 once it exists.

(in-package :agent)

(defparameter *eval-journal-file* (pai-state-path "eval-journal.jsonl"))

(defun %eval-journal-target (form-string)
  "Best-effort: the symbol being defined, if FORM-STRING looks like a
definition. NIL for plain computation -- still journaled, just with no
target, since the whole point is a complete record, not a filtered one."
  (ignore-errors
    (let* ((*package* (find-package :agent))
           (*read-eval* nil)
           (form (read-from-string form-string nil nil)))
      (and (consp form) (symbolp (car form))
           (member (symbol-name (car form))
                   '("DEFUN" "DEFPARAMETER" "DEFVAR" "DEFSTRUCT" "DEFMETHOD"
                     "DEFCLASS" "DEFMACRO")
                   :test #'string=)
           (consp (cdr form))
           (format nil "~a" (second form))))))

(defun %eval-journal-write (kind form-string)
  "Append one JSONL record. Signals an error on failure rather than
swallowing it -- callers must let that propagate to stay fail-closed."
  (with-open-file (out *eval-journal-file* :direction :output
                       :if-exists :append :if-does-not-exist :create
                       :external-format :utf-8)
    (write-string
     (let ((*print-pretty* nil))
       (shasht:write-json
        (obj "ts" (get-universal-time) "kind" kind
             "target" (or (%eval-journal-target form-string) :null)
             "form" form-string)
        nil))
     out)
    (terpri out)
    (finish-output out)))


(register-layer lisp-eval eval-journal :order 200
  ;; Inner layer: journal the call, then evaluate. Fails closed -- if the
  ;; journal write fails the evaluation does NOT happen.
  :function (lambda (next form-string)
      (handler-case
          (progn
            (%eval-journal-write "lisp-eval" form-string)
            (funcall next form-string))
        (error (e)
          (format nil "ERROR: eval journal write failed, eval NOT performed (fail-closed): ~a" e))))
    )

(unless (fboundp 'pai-base-propose-loop)
  (setf (fdefinition 'pai-base-propose-loop) (fdefinition 'propose-loop)))

(defun propose-loop (proposed-src)
  (handler-case
      (progn
        (%eval-journal-write "propose-loop" proposed-src)
        (funcall 'pai-base-propose-loop proposed-src))
    (error (e)
      (format nil "REJECTED (eval journal write failed, fail-closed): ~a" e))))
