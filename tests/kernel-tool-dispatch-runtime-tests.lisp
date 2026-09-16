(ql:quickload :bordeaux-threads :silent t)
(defpackage :agent (:use :cl))
(in-package :agent)
(defun obj (&rest pairs)
  (loop with table = (make-hash-table :test #'equal)
        for (key value) on pairs by #'cddr do (setf (gethash key table) value)
        finally (return table)))
(defvar *tools* #()) (defvar *route-order* nil) (defvar *legacy-calls* 0)
(defun execute (call) (declare (ignore call)) (incf *legacy-calls*)
  (values :legacy 2 "three"))
(defun call-with-timing-span (name thunk &key attributes)
  (declare (ignore name attributes)) (push :timing-enter *route-order*)
  (multiple-value-prog1 (funcall thunk) (push :timing-exit *route-order*)))
(defun call-with-tool-event-observation (call thunk)
  (push :event-call *route-order*)
  (multiple-value-prog1 (funcall thunk call) (push :event-result *route-order*)))
(defun observe-tool-result-appraisal (result) (push :appraisal *route-order*) result)
(defun call-with-proposal-provenance (call thunk)
  (push :provenance-enter *route-order*)
  (multiple-value-prog1 (funcall thunk call) (push :provenance-exit *route-order*)))
(defun fixture-port (call)
  (push :handler *route-order*)
  (obj "role" "tool" "tool_call_id" (gethash "id" call)
       "content" (gethash "name" (gethash "function" call))))
(dolist (name '(self-mod-tool-handle pai-enhancements-tool-handle
                web-terminal-tool-handle runware-tool-handle bounded-work-tool-handle
                memory-search-tool-handle near-term-intention-tool-handle))
  (setf (fdefinition name) #'fixture-port))
(in-package :cl-user)
(defvar *runtime-pass* 0) (defvar *runtime-fail* 0)
(defun runtime-check (name condition)
  (if condition (progn (incf *runtime-pass*) (format t "PASS ~a~%" name))
      (progn (incf *runtime-fail*) (format t "FAIL ~a~%" name))))
(defun runtime-error (thunk)
  (handler-case (progn (funcall thunk) nil) (error (condition) condition)))
(defun runtime-obj (&rest pairs) (apply #'agent::obj pairs))
(defun runtime-call (name)
  (runtime-obj "id" (format nil "call-~a" name) "function"
               (runtime-obj "name" name "arguments" "{}")))
(defun runtime-tool (name)
  (runtime-obj "type" "function" "function" (runtime-obj "name" name)))
(load (test-source "kernel-tool-dispatch-core.lisp"))
(load (test-source "kernel-tool-dispatch-shadow.lisp"))
(agent:tool-dispatch-shadow-initialize
 (namestring (test-source "kernel-tool-dispatch-module.sexp")))
(load (test-source "kernel-tool-handler-bindings.lisp"))
(load (test-source "kernel-tool-dispatch-runtime.lisp"))
(let* ((rows (agent:tool-handler-binding-rows))
       (ids (mapcar (lambda (row) (getf row :handler-id)) rows))
       (names (mapcar (lambda (row) (getf row :external-name)) rows)))
  (setf agent::*tools* (coerce (mapcar #'runtime-tool names) 'vector))
  (let ((incumbent (fdefinition 'agent::execute)))
    (let ((report (agent:tool-dispatch-runtime-install ids))
          (wrapper (fdefinition 'agent::execute)))
      (runtime-check "all thirteen routes install"
                     (and (gethash "installed" report)
                          (= (length (gethash "enabled_handler_ids" report)) 13)))
      (agent:tool-dispatch-runtime-install ids)
      (runtime-check "repeat identical install is idempotent"
                     (eq wrapper (fdefinition 'agent::execute)))
      (dolist (name names)
        (setf agent::*route-order* nil)
        (let ((result (agent::execute (runtime-call name))))
          (runtime-check "enabled route invokes its exact handler once"
                         (string= (gethash "content" result) name))
          (runtime-check "ordinary route uses frozen observer composition"
                         (if (string= name "propose-loop")
                             (equal (reverse agent::*route-order*)
                                    '(:timing-enter :event-call
                                      :provenance-enter :handler
                                      :provenance-exit :appraisal
                                      :event-result :timing-exit))
                             (equal (reverse agent::*route-order*)
                                    '(:timing-enter :event-call :handler
                                      :appraisal :event-result :timing-exit))))))
      (runtime-check "enabled routes never invoke legacy"
                     (zerop agent::*legacy-calls*))
      (let ((after (agent:tool-dispatch-runtime-report)))
        (runtime-check "report has thirteen selected counters"
                       (and (= (length (gethash "selected_counts" after)) 13)
                            (= (reduce #'+ (coerce (gethash "selected_counts" after)
                                                   'list)
                                       :key (lambda (row) (gethash "count" row)))
                               13)
                            (zerop (gethash "composition_errors" after)))))
      (agent:tool-dispatch-runtime-uninstall)
      (runtime-check "uninstall restores exact incumbent identity"
                     (eq incumbent (fdefinition 'agent::execute)))))
  (setf agent::*legacy-calls* 0)
  (agent:tool-dispatch-runtime-install '("search-memory"))
  (multiple-value-bind (a b c) (agent::execute (runtime-call "lisp-eval"))
    (runtime-check "disabled handler preserves legacy multiple values"
                   (and (eq a :legacy) (= b 2) (string= c "three"))))
  (multiple-value-bind (a b c) (agent::execute (runtime-call "unknown"))
    (runtime-check "unknown call preserves legacy multiple values"
                   (and (eq a :legacy) (= b 2) (string= c "three"))))
  (multiple-value-bind (a b c) (agent::execute (runtime-obj "id" "bad"))
    (runtime-check "malformed call preserves legacy multiple values"
                   (and (eq a :legacy) (= b 2) (string= c "three"))))
  (runtime-check "disabled unknown malformed each fall through once"
                 (= agent::*legacy-calls* 3))
  (agent:tool-dispatch-runtime-uninstall)
  (let ((saved-tools agent::*tools*))
    (setf agent::*tools* (subseq agent::*tools* 1))
    (let ((identity (fdefinition 'agent::execute)))
      (runtime-check "missing enabled advertisement fails closed"
                     (runtime-error (lambda ()
                                      (agent:tool-dispatch-runtime-install
                                       '("lisp-eval")))))
      (runtime-check "failed install preserves execute identity"
                     (eq identity (fdefinition 'agent::execute))))
    (setf agent::*tools* saved-tools)))
(let ((source (uiop:read-file-string
               (namestring (test-source "kernel-tool-dispatch-runtime.lisp")))))
  (runtime-check "generic runtime has no handler-name conditional"
                 (and (null (search "string= name \"search-memory\"" source
                                    :test #'char-equal))
                      (null (search "intern" source :test #'char-equal))
                      (null (search "openrouter" source :test #'char-equal))
                      (null (search "postgres" source :test #'char-equal)))))
(format t "RESULT kernel-tool-dispatch-runtime: ~d passed, ~d failed~%"
        *runtime-pass* *runtime-fail*)
(when (plusp *runtime-fail*) (sb-ext:exit :code 1))
