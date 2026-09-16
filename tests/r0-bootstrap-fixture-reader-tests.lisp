(defpackage :agent (:use :cl))
(in-package :agent)

(ql:quickload '(:postmodern :shasht) :silent t)

(let ((count 0))
  (dolist (name '("r0-bootstrap-synthetic-tail.lisp"
                  "r0-bootstrap-tail-materializer.lisp"
                  "r0-bootstrap-checkpoint-contract.lisp"
                  "r0-bootstrap-checkpoint-contract-tests.lisp"
                  "r0-bootstrap-checkpoint-worker.lisp"))
    (let ((path (merge-pathnames (format nil "tests/~a" name) *pai-root*)))
    (with-open-file (in path :direction :input)
      (loop for form = (read in nil nil)
            while form do (incf count)))
    (format t "R0_BOOTSTRAP_FIXTURE_READ_PASS path=~a~%" path)))
  (unless (plusp count) (error "No candidate fixture forms were read")))
