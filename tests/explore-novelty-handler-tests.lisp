(in-package :agent)

(defvar *explore-test-pass* 0)
(defvar *explore-test-fail* 0)

(defun explore-test-check (name condition)
  (if condition
      (progn (incf *explore-test-pass*) (format t "  ok   ~a~%" name))
      (progn (incf *explore-test-fail*) (format t "  FAIL ~a~%" name))))

;; Minimal deterministic seams. The protected body fails before model, DB,
;; memory, or persistence can run; this directly tests the malformed
;; HANDLER-CASE that previously compiled (ERROR (E) ...) as ordinary calls.
(defparameter *tick-handlers* (make-hash-table :test #'equal))
(defparameter *explore-state-file* #P"/tmp/nonexistent-explore-state.json")
(defparameter *explore-current-topic* nil)
(defparameter *explore-topic-started-at* 0)
(defparameter *explore-continuation-count* 0)
(defparameter *explore-test-continuity* nil)

(defun %explore-pick-question () (error "forced explore failure"))
(defun continuity-buffer-append (text) (push text *explore-test-continuity*))

(load (test-source "explore-novelty.lisp"))

(format t "~%== explore terminal error handling ==~%")
(let ((escaped nil))
  (handler-case (%tick-handle-explore)
    (error () (setf escaped t)))
  (explore-test-check "handler error does not escape" (not escaped))
  (explore-test-check "fallback continuity is recorded"
                      (and *explore-test-continuity*
                           (search "didn't come together"
                                   (first *explore-test-continuity*))))
  (explore-test-check "corrected handler is registered"
                      (eq (gethash "explore" *tick-handlers*)
                          (fdefinition '%tick-handle-explore))))

(format t "~%~a passed, ~a failed~%" *explore-test-pass* *explore-test-fail*)
(when (plusp *explore-test-fail*) (sb-ext:exit :code 1))
