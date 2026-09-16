(defpackage :agent (:use :cl))
(in-package :agent)

(defvar *tools* #())
(defun obj (&rest pairs)
  (let ((result (make-hash-table :test #'equal)))
    (loop for (key value) on pairs by #'cddr
          do (setf (gethash key result) value))
    result))
(defun tool-dispatch-kernel-boot-p () nil)

(load (test-source "kernel-tool-dispatch-bootstrap.lisp"))

(defvar *r2f-catalogue-pass* 0)
(defvar *r2f-catalogue-fail* 0)
(defvar *r2f-test-advertisement-count* 1)

(defun r2f-catalogue-check (name condition)
  (if condition
      (progn (incf *r2f-catalogue-pass*) (format t "PASS ~a~%" name))
      (progn (incf *r2f-catalogue-fail*) (format t "FAIL ~a~%" name))))

(defun r2f-catalogue-errors-p (thunk)
  (handler-case (progn (funcall thunk) nil) (error () t)))

(defun r2f-test-tool (name)
  (obj "function" (obj "name" name)))

(defun tool-dispatch-shadow-inspect (name)
  (let ((resolved (member name '("alpha" "beta") :test #'string=)))
    (obj "registry_plan"
         (obj "status" (if resolved "resolved" "unknown-tool")
              "handler_id" (if resolved name :null))
         "incumbent_advertisement"
         (obj "count" *r2f-test-advertisement-count*))))

(defun tool-handler-binding-lookup (handler-id)
  (when (member handler-id '("alpha" "beta") :test #'string=)
    (list :handler-id handler-id :external-name handler-id)))

(setf *tools* (vector (r2f-test-tool "alpha") (r2f-test-tool "beta")))
(r2f-catalogue-check
 "exact advertised subset resolves in catalogue order"
 (equal (%kernel-tool-dispatch-enabled-handler-ids) '("alpha" "beta")))

(setf *tools* (vector (r2f-test-tool "alpha") (r2f-test-tool "alpha")))
(r2f-catalogue-check
 "duplicate advertised name fails closed"
 (r2f-catalogue-errors-p #'%kernel-tool-dispatch-enabled-handler-ids))

(setf *tools* (vector (r2f-test-tool "unknown")))
(r2f-catalogue-check
 "unresolved advertised name fails closed"
 (r2f-catalogue-errors-p #'%kernel-tool-dispatch-enabled-handler-ids))

(setf *tools* (vector (r2f-test-tool "alpha"))
      *r2f-test-advertisement-count* 2)
(r2f-catalogue-check
 "advertisement count mismatch fails closed"
 (r2f-catalogue-errors-p #'%kernel-tool-dispatch-enabled-handler-ids))

(setf *tools* (vector (obj "not-function" "alpha"))
      *r2f-test-advertisement-count* 1)
(r2f-catalogue-check
 "malformed advertised tool fails closed"
 (r2f-catalogue-errors-p #'%kernel-tool-dispatch-enabled-handler-ids))

(setf *tools* #())
(r2f-catalogue-check
 "empty catalogue derives an empty set for bootstrap to reject"
 (null (%kernel-tool-dispatch-enabled-handler-ids)))

(format t "RESULT catalogue correction: ~d passed, ~d failed~%"
        *r2f-catalogue-pass* *r2f-catalogue-fail*)
(when (plusp *r2f-catalogue-fail*) (uiop:quit 1))
