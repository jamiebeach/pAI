(ql:quickload :bordeaux-threads :silent t)
(defpackage :agent (:use :cl))
(in-package :agent)
(defun obj (&rest pairs)
  (loop with table = (make-hash-table :test #'equal)
        for (key value) on pairs by #'cddr do (setf (gethash key table) value)
        finally (return table)))
(defvar *tools* #())
(dolist (name '(self-mod-tool-handle pai-enhancements-tool-handle
                web-terminal-tool-handle runware-tool-handle
                bounded-work-tool-handle memory-search-tool-handle
                near-term-intention-tool-handle))
  (setf (fdefinition name) (lambda (call) call)))
(in-package :cl-user)
(defvar *binding-pass* 0) (defvar *binding-fail* 0)
(defun binding-check (name condition)
  (if condition (progn (incf *binding-pass*) (format t "PASS ~a~%" name))
      (progn (incf *binding-fail*) (format t "FAIL ~a~%" name))))
(defun binding-error (thunk)
  (handler-case (progn (funcall thunk) nil) (error (condition) condition)))
(defun binding-tool (name)
  (agent::obj "type" "function" "function" (agent::obj "name" name)))
(load (test-source "kernel-tool-dispatch-core.lisp"))
(load (test-source "kernel-tool-dispatch-shadow.lisp"))
(agent:tool-dispatch-shadow-initialize
 (namestring (test-source "kernel-tool-dispatch-module.sexp")))
(load (test-source "kernel-tool-handler-bindings.lisp"))
(let* ((rows (agent:tool-handler-binding-rows))
       (names (mapcar (lambda (row) (getf row :external-name)) rows)))
  (setf agent::*tools* (coerce (mapcar #'binding-tool names) 'vector))
  (let ((report (agent:tool-handler-bindings-initialize)))
    (binding-check "closed table initializes all thirteen manifest rows"
                   (and (gethash "initialized" report)
                        (= (gethash "binding_count" report) 13)))
    (binding-check "binding names are exact and unique"
                   (and (= (length names) 13)
                        (= (length names)
                           (length (remove-duplicates names :test #'string=)))))
    (binding-check "all four qualification groups are represented"
                   (= (length (gethash "groups" report)) 4))
    (dolist (row rows)
      (let ((found (agent:tool-handler-binding-lookup
                    (getf row :handler-id))))
        (binding-check "literal lookup returns exact name and port"
                       (and (string= (getf found :external-name)
                                     (getf row :external-name))
                            (eq (getf found :port) (getf row :port)))))))
  (let ((agent::*tool-handler-bindings-initialized* nil)
        (agent::*tool-handler-binding-table* (rest rows)))
    (binding-check "missing row fails closed"
                   (binding-error #'agent:tool-handler-bindings-initialize)))
  (let* ((bad (mapcar #'copy-list rows))
         (agent::*tool-handler-bindings-initialized* nil)
         (agent::*tool-handler-binding-table* bad))
    (setf (getf (second bad) :handler-id) (getf (first bad) :handler-id))
    (binding-check "duplicate handler ID fails closed"
                   (binding-error #'agent:tool-handler-bindings-initialize)))
  (let* ((bad (mapcar #'copy-list rows))
         (agent::*tool-handler-bindings-initialized* nil)
         (agent::*tool-handler-binding-table* bad))
    (setf (getf (first bad) :port) 'agent::missing-binding-port)
    (binding-check "unbound literal port fails closed"
                   (binding-error #'agent:tool-handler-bindings-initialize))))
(let ((source (uiop:read-file-string
               (namestring (test-source "kernel-tool-handler-bindings.lisp")))))
  (binding-check "binding source has no dynamic symbol or domain authority"
                 (and (null (search "(intern " source :test #'char-equal))
                      (null (search "find-symbol" source :test #'char-equal))
                      (null (search "openrouter" source :test #'char-equal))
                      (null (search "postgres" source :test #'char-equal))
                      (null (search "dexador" source :test #'char-equal)))))
(format t "RESULT kernel-tool-handler-bindings: ~d passed, ~d failed~%"
        *binding-pass* *binding-fail*)
(when (plusp *binding-fail*) (sb-ext:exit :code 1))
