;;;; Read a conversation JSON array from stdin and print counts only.
(defpackage :agent (:use :cl))
(in-package :agent)
(ql:quickload :shasht :silent t)
(defun obj (&rest kvs)
  (loop with table = (make-hash-table :test #'equal)
        for (key value) on kvs by #'cddr
        do (setf (gethash key table) value)
        finally (return table)))
(defvar *conversation-context-budget-mode* :enforced)
(load (test-source "conversation-context-budget.lisp"))
(let* ((history (coerce (shasht:read-json *standard-input*) 'list))
       (last-user (position "user" history :from-end t
                            :key (lambda (message) (gethash "role" message ""))
                            :test #'string=))
       (result (conversation-context-budget-manage
                history #'identity (lambda (messages)
                                     (declare (ignore messages)) nil)))
       (retained-last-user
         (and last-user
              (find (nth last-user history) result :test #'eq)))
       (retained-previous
         (and last-user (plusp last-user)
              (find (nth (1- last-user) history) result :test #'eq))))
  (format t "input_records=~d input_chars=~d output_records=~d output_chars=~d previous_record_retained=~a last_user_retained=~a valid=~a~%"
          (length history) (%ccb-message-chars history)
          (length result) (%ccb-message-chars result)
          (if retained-previous "true" "false")
          (if retained-last-user "true" "false")
          (if (%ccb-valid-candidate-p result) "true" "false")))
