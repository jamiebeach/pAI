(in-package :agent)

(ql:quickload :bordeaux-threads :silent t)

(defvar *turn-cancel-passed* 0)
(defvar *turn-cancel-failed* 0)
(defvar *turn-cancel-base-entered* nil)
(defvar *turn-cancel-observed-error* nil)

(defun turn-cancel-check (name condition)
  (if condition
      (progn (incf *turn-cancel-passed*) (format t "PASS ~a~%" name))
      (progn (incf *turn-cancel-failed*) (format t "FAIL ~a~%" name))))

(defun %run-self-mod-messages (messages)
  (declare (ignore messages))
  (setf *turn-cancel-base-entered* t)
  (loop (sleep 0.05)))

(load (test-source "turn-cancellation.lisp"))

(setf *turn-cancel-base-entered* nil
      *turn-cancel-observed-error* nil)
(let ((worker
        (bt:make-thread
         (lambda ()
           (handler-case
               (%run-self-mod-messages nil)
             (pai-turn-cancelled (condition)
               (setf *turn-cancel-observed-error*
                     (pai-turn-cancelled-reason condition)))))
         :name "turn-cancellation-fixture")))
  (loop repeat 100
        until *turn-cancel-base-entered*
        do (sleep 0.01))
  (turn-cancel-check "wrapper records active thread"
                     (gethash "active" (turn-cancellation-report)))
  (turn-cancel-check "cancel sends interrupt to different active thread"
                     (eq :interrupt-sent
                         (cancel-active-turn "deterministic-test")))
  (bt:join-thread worker)
  (turn-cancel-check "worker receives typed cancellation reason"
                     (string= "deterministic-test"
                              *turn-cancel-observed-error*))
  (turn-cancel-check "unwind clears active thread"
                     (not (gethash "active" (turn-cancellation-report))))
  (turn-cancel-check "cancel with no active turn is explicit"
                     (eq :no-active-turn (cancel-active-turn))))

(let ((base (fdefinition 'pai-base-run-self-mod-messages-cancellation)))
  (load (test-source "turn-cancellation.lisp"))
  (turn-cancel-check "reload retains true underlying turn pipeline"
                     (eq base
                         (fdefinition
                          'pai-base-run-self-mod-messages-cancellation))))

(format t "~%TURN CANCELLATION TESTS: ~d passed, ~d failed.~%"
        *turn-cancel-passed* *turn-cancel-failed*)
(when (plusp *turn-cancel-failed*) (uiop:quit 1))
