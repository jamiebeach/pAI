(unless (find-package :agent)
  (defpackage :agent (:use :cl)))

(in-package :agent)

(ql:quickload '(:bordeaux-threads :shasht) :silent t)

(unless (fboundp 'obj)
  (defun obj (&rest kvs)
    (loop with table = (make-hash-table :test #'equal)
          for (key value) on kvs by #'cddr
          do (setf (gethash key table) value)
          finally (return table))))

;; Offline operator harness: provide only the legacy call points EVENT-LOG wraps.
;; No runtime adapter, provider, database, transport, or background worker loads.
(unless (fboundp 'auto-turn)
  (setf (fdefinition 'auto-turn) (lambda (prompt) prompt)))
(unless (fboundp 'execute)
  (setf (fdefinition 'execute)
        (lambda (&rest arguments) (declare (ignore arguments)) nil)))
(unless (fboundp 'propose-loop)
  (setf (fdefinition 'propose-loop)
        (lambda (&rest arguments) (declare (ignore arguments)) nil)))
(unless (boundp '*tools*) (defparameter *tools* (vector)))

(load (test-source "event-log.lisp"))

(let ((watermark (event-log-initialize-segmentation)))
  (format t "~&R0D_OFFLINE_CUTOVER_OK last_reserved_id=~d~%"
          (gethash "last_reserved_id" watermark)))
