(ql:quickload '(:uiop :shasht) :silent t)
(defpackage :agent (:use :cl))
(in-package :agent)
(defun obj (&rest pairs)
  (loop with h = (make-hash-table :test #'equal)
        for (k v) on pairs by #'cddr do (setf (gethash k h) v)
        finally (return h)))
(defun execute (call) (values :terminal call))
(defvar *boot-pass* 0) (defvar *boot-fail* 0)
(defun boot-check (name condition)
  (if condition (progn (incf *boot-pass*) (format t "PASS ~a~%" name))
      (progn (incf *boot-fail*) (format t "FAIL ~a~%" name))))
(load (test-source "tool-dispatch-boot-mode.lisp"))
(let* ((raw (uiop:getenv "PAI_TOOL_DISPATCH_BOOT_MODE"))
       (expected (if (and raw (string-equal raw "kernel")) :kernel :legacy)))
  (boot-check "process boot mode matches exact environment choice"
              (eq (tool-dispatch-boot-mode) expected))
  (boot-check "legacy wrapper predicate is mode exact"
              (eq (tool-dispatch-legacy-wrapper-enabled-p)
                  (eq expected :legacy))))
(boot-check "terminal identity is captured exactly"
            (eq (fdefinition 'execute) (tool-dispatch-terminal-execute-identity)))
(boot-check "kernel parser is exact"
            (eq (%tool-dispatch-parse-boot-mode "kernel") :kernel))
(boot-check "legacy parser is case insensitive"
            (eq (%tool-dispatch-parse-boot-mode "LEGACY") :legacy))
(boot-check "unknown mode fails closed"
            (handler-case (progn (%tool-dispatch-parse-boot-mode "shadow") nil)
              (error () t)))
(boot-check "report declares immutable process choice"
            (let ((r (tool-dispatch-boot-mode-report)))
              (and (string= (gethash "mode" r)
                            (string-downcase
                             (symbol-name (tool-dispatch-boot-mode))))
                   (not (gethash "mutable" r))
                   (gethash "terminal_execute_intact" r))))
(format t "RESULT tool-dispatch-boot-mode: ~d passed, ~d failed~%"
        *boot-pass* *boot-fail*)
(when (plusp *boot-fail*) (sb-ext:exit :code 1))
