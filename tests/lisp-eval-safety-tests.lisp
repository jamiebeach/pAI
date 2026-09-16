(in-package :agent)

(defvar *lisp-safety-passed* 0)
(defvar *lisp-safety-failed* 0)
(defvar *lisp-safety-base-calls* 0)

(defun lisp-safety-check (name condition)
  (if condition
      (progn (incf *lisp-safety-passed*) (format t "PASS ~a~%" name))
      (progn (incf *lisp-safety-failed*) (format t "FAIL ~a~%" name))))

;; Install a counting stub as the seam's BASE.
;;
;; This was a plain DEFUN, which worked under the rename-and-fall-through
;; idiom: loading lisp-eval-safety.lisp captured whatever LISP-EVAL was and
;; wrapped it. Now that LISP-EVAL is a seam, a DEFUN would overwrite the
;; seam's dispatcher instead of supplying its base -- calls would bypass the
;; chain entirely and the safety layer would never run, which is exactly how
;; this suite failed after the conversion.
;;
;; DEFINE-SEAM against an existing seam replaces only the base and leaves
;; registered layers intact, which is the behaviour the test wants.
(define-seam lisp-eval (source)
  (incf *lisp-safety-base-calls*)
  (let ((*package* (find-package :agent)))
    (format nil "~s" (eval (read-from-string source)))))

(load (test-source "lisp-eval-safety.lisp"))

(setf *lisp-safety-base-calls* 0)
(let ((result (lisp-eval "(+ 2 3)")))
  (lisp-safety-check "ordinary form reaches existing evaluator once"
                     (and (string= "5" result)
                          (= 1 *lisp-safety-base-calls*))))

(dolist (source '("(inspect #'car)"
                  "(funcall #'break)"
                  "(read-line)"
                  "(yes-or-no-p \"continue?\")"))
  (let ((before *lisp-safety-base-calls*)
        (result (lisp-eval source)))
    (lisp-safety-check
     (format nil "interactive form rejected: ~a" source)
     (and (search "interactive Lisp operator" result)
          (= before *lisp-safety-base-calls*)))))

(let ((before *lisp-safety-base-calls*)
      (result (lisp-eval "(+ 1 1) (+ 2 2)")))
  (lisp-safety-check "multiple forms rejected before evaluation"
                     (and (search "exactly one form" result)
                          (= before *lisp-safety-base-calls*))))

(let ((before *lisp-safety-base-calls*)
      (result (lisp-eval "#.(error \"reader side effect\")")))
  (lisp-safety-check "reader evaluation is disabled"
                     (and (search "can't read" result :test #'char-equal)
                          (= before *lisp-safety-base-calls*))))

(let ((*lisp-eval-timeout-seconds* 0.05d0)
      (before *lisp-safety-base-calls*))
  (let ((result (lisp-eval "(loop)")))
    (lisp-safety-check "non-interactive infinite evaluation times out"
                       (and (search "safety timeout" result)
                            (= (1+ before) *lisp-safety-base-calls*)))))

;; Reload safety. This previously inspected PAI-BASE-LISP-EVAL-SAFETY -- the
;; saved-original the wrap idiom needed, plus a guard to stop the file
;; capturing its own wrapper as the base on reload. The seam makes that
;; structural: re-registering a named layer replaces that one entry. Same
;; property, asserted through the mechanism that now provides it.
(let ((layers-before (seam-layers-in-order 'lisp-eval)))
  (load (test-source "lisp-eval-safety.lisp"))
  (lisp-safety-check "reload does not duplicate or drop layers"
                     (equal layers-before (seam-layers-in-order 'lisp-eval)))
  (lisp-safety-check "reload leaves the chain callable end to end"
                     (string= "9" (lisp-eval "(+ 4 5)")))
  (lisp-safety-check "reload retains the safety layer"
                     (member 'safety-timeout (seam-layers-in-order 'lisp-eval))))

(let ((report (lisp-eval-safety-report)))
  (lisp-safety-check "report exposes rejection and timeout counters"
                     (and (>= (gethash "rejected" report) 4)
                          (>= (gethash "timeouts" report) 1)
                          (= 20 (gethash "timeout_seconds" report)))))

(format t "~%LISP-EVAL SAFETY TESTS: ~d passed, ~d failed.~%"
        *lisp-safety-passed* *lisp-safety-failed*)
(when (plusp *lisp-safety-failed*) (uiop:quit 1))
