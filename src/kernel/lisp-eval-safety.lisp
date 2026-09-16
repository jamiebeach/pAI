;;;; lisp-eval-safety.lisp -- bounded, non-interactive public Lisp evaluation.
;;;;
;;;; LISP-EVAL runs inside the single public-turn lock. An interactive operator
;;;; such as INSPECT can wait on process stdin forever, holding that lock and
;;;; queueing every later web/Telegram turn. This final wrapper rejects known
;;;; interactive readers before evaluation, disables reader evaluation, binds
;;;; query/debug input to EOF, and places the entire underlying wrapper chain
;;;; behind an SBCL timeout. It preserves eval-journal and all existing output
;;;; capture because those wrappers remain underneath this boundary.

(in-package :agent)

(export '(lisp-eval-safety-report))

(defparameter *lisp-eval-timeout-seconds* 20)

(defparameter *lisp-eval-interactive-operators*
  '("INSPECT" "BREAK" "INVOKE-DEBUGGER"
    "READ" "READ-LINE" "READ-CHAR" "READ-CHAR-NO-HANG" "PEEK-CHAR"
    "Y-OR-N-P" "YES-OR-NO-P"))

(defvar *lisp-eval-safety-installed-wrapper* nil)
(defvar *lisp-eval-safety-rejected* 0)
(defvar *lisp-eval-safety-timeouts* 0)


(defun %lisp-eval-read-single-form (source)
  (let ((*read-eval* nil))
    (with-input-from-string (stream source)
      (let ((form (read stream nil :eof))
            (trailing nil))
        (when (eq form :eof)
          (error "lisp-eval requires one form"))
        (setf trailing (read stream nil :eof))
        (unless (eq trailing :eof)
          (error "lisp-eval accepts exactly one form"))
        form))))

(defun %lisp-eval-interactive-operator (form)
  (labels ((walk (value)
             (cond
               ((symbolp value)
                (find (symbol-name value) *lisp-eval-interactive-operators*
                      :test #'string=))
               ((consp value)
                (or (walk (car value)) (walk (cdr value))))
               ((vectorp value)
                (loop for item across value thereis (walk item)))
               (t nil))))
    (walk form)))

(register-layer lisp-eval safety-timeout :order 100
  ;; Outermost by design: this places the ENTIRE chain beneath a wall-time
  ;; bound and rejects interactive readers before anything evaluates. Any
  ;; layer registered outside this one would escape the timeout.
  ;; Runs one non-interactive form through the rest of the chain, bounded in
  ;; wall time. Returns an ERROR string rather than entering an inspector or
  ;; holding the public-turn lock indefinitely.
  :function (lambda (next form-string)
    ;; DEFUN gave this body an implicit block named LISP-EVAL, which its
    ;; RETURN-FROM below relies on. A LAMBDA has no such block, so it is
    ;; made explicit here.
    (block lisp-eval
      (handler-case
          (sb-ext:with-timeout *lisp-eval-timeout-seconds*
            (let* ((source (if (stringp form-string) form-string ""))
                   (package (find-package :agent))
                   (before (%lisp-eval-package-symbol-snapshot package))
                   (form (%lisp-eval-read-single-form source))
                   (reader-created
                     (%lisp-eval-reader-created-symbols package before))
                   (interactive (%lisp-eval-interactive-operator form))
                   (result nil)
                   (removed nil))
              (unwind-protect
                   (setf result
                         (if interactive
                             (progn
                               (incf *lisp-eval-safety-rejected*)
                               (format nil "ERROR: interactive Lisp operator ~a is unavailable in lisp-eval; use non-interactive introspection such as DESCRIBE, APROPOS, or FUNCTION-LAMBDA-EXPRESSION."
                                       interactive))
                             (let* ((*read-eval* nil)
                                    (*standard-input*
                                      (make-string-input-stream ""))
                                    (query-output (make-broadcast-stream))
                                    (*query-io*
                                      (make-two-way-stream
                                       (make-string-input-stream "")
                                       query-output))
                                    (*debug-io*
                                      (make-two-way-stream
                                       (make-string-input-stream "")
                                       query-output)))
                               (funcall next source))))
                (setf removed
                      (%lisp-eval-clean-reader-artifacts
                       package reader-created)))
              (if removed
                  (format nil "~a~%[reader artifacts removed: ~{~a~^, ~}]"
                          result removed)
                  result)))
        (sb-ext:timeout ()
          (incf *lisp-eval-safety-timeouts*)
          (format nil "ERROR: lisp-eval exceeded the ~a-second safety timeout."
                  *lisp-eval-timeout-seconds*))
        (error (condition)
          (format nil "ERROR: ~a" condition)))))
    )

(defun lisp-eval-safety-report ()
  (obj "schema_version" 1
       "timeout_seconds" *lisp-eval-timeout-seconds*
       "interactive_operator_count" (length *lisp-eval-interactive-operators*)
       "rejected" *lisp-eval-safety-rejected*
       "timeouts" *lisp-eval-safety-timeouts*))

;; The installed-wrapper bookkeeping that lived here guarded against this
;; file re-capturing its own wrapper as the base on reload. A named layer
;; is replaced in place, so the hazard it defended against no longer exists.
