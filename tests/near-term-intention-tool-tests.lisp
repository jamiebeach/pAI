(in-package :agent)

(defvar *near-term-tool-test-pass* 0)
(defvar *near-term-tool-test-fail* 0)
(defun near-term-tool-test-check (name condition)
  (if condition
      (progn (incf *near-term-tool-test-pass*) (format t "  ok   ~a~%" name))
      (progn (incf *near-term-tool-test-fail*) (format t "  FAIL ~a~%" name))))

(defvar *tools* (vector))
(defvar *turn-capture-context* nil)
(defun near-term-tool-fixture-execute (tool-call)
  (obj "role" "tool" "tool_call_id" (gethash "id" tool-call)
       "content" "base-executor"))
(unless (boundp '*near-term-intentions-mode*)
  (defvar *near-term-intentions-mode* :off))
(unless (boundp '*autonomous-write-mode*)
  (defvar *autonomous-write-mode* :normal))

(load (test-source "near-term-intentions.lisp"))
(load (test-source "near-term-intention-tool.lisp"))

(let ((prior-execute (and (fboundp 'execute) (fdefinition 'execute)))
      (prior-legacy-gate
        (and (fboundp 'tool-dispatch-legacy-wrapper-enabled-p)
             (fdefinition 'tool-dispatch-legacy-wrapper-enabled-p)))
      (prior-delivery-readiness
        (and (fboundp 'initiative-committed-delivery-readiness)
             (fdefinition 'initiative-committed-delivery-readiness)))
      (prior-budget-status
        (and (fboundp 'tick-budget-status)
             (fdefinition 'tick-budget-status)))
      (prior-installed *near-term-intention-tool-installed*)
      (prior-base *near-term-intention-tool-base-execute*)
      (prior-tools *tools*)
      (*autonomous-write-mode* :normal)
      (*near-term-intention-file* #P"/tmp/near-term-intention-tool-test.json")
      (*near-term-intention-records* nil)
      (*near-term-intention-delivery-fn*
        (lambda (record) (declare (ignore record))
          (obj "status" "delivered"))))
  (unwind-protect
      (progn
        ;; The isolated harness already owns EXECUTE. Take explicit ownership
        ;; so this suite exercises this wrapper rather than an inherited one.
        (setf (fdefinition 'execute) #'near-term-tool-fixture-execute
              (fdefinition 'tool-dispatch-legacy-wrapper-enabled-p)
              (lambda () t)
              (fdefinition 'initiative-committed-delivery-readiness)
              (lambda (&key audience)
                (declare (ignore audience))
                (values t nil))
              (fdefinition 'tick-budget-status) (lambda () :ok)
              *near-term-intention-tool-installed* nil
              *near-term-intention-tool-base-execute* nil)
        (when (probe-file *near-term-intention-file*)
          (delete-file *near-term-intention-file*))
        (near-term-tool-test-check "off mode leaves tool absent"
                                   (not (find "hold-near-term-thought" *tools*
                                              :key (lambda (tool)
                                                     (ref tool "function" "name"))
                                              :test #'string=)))
        (setf *near-term-intentions-mode* :enforced)
        (near-term-intention-tool-install)
        (near-term-tool-test-check "enforced promotion lists tool"
                                   (gethash "listed" (near-term-intention-tool-report)))
        (let* ((*turn-capture-context*
           (obj "turn_id" "turn-tool-1" "user_event_id" 501))
         (*publication-contract-current*
           (obj "near_term_intentions_enforced" t
                "deferred_receipt_active" nil))
         (call (obj "id" "call-1" "function"
                    (obj "name" "hold-near-term-thought"
                         "arguments"
                         (shasht:write-json
                          (obj "subject" "patent" "aim" "develop one idea"
                               "return_window_seconds" 180) nil))))
         (result (execute call))
         (content (shasht:read-json (gethash "content" result))))
    (near-term-tool-test-check "tool returns accepted receipt"
                               (string= "accepted" (gethash "status" content)))
    (near-term-tool-test-check "tool injects causal turn from capture context"
                               (string= "turn-tool-1"
                                        (gethash "origin_turn_id"
                                                 (near-term-intention-active))))
    (near-term-tool-test-check "accepted receipt updates only the current turn contract"
                               (and (gethash "deferred_receipt_active"
                                             *publication-contract-current*)
                                    (string= (gethash "receipt_id" content)
                                             (gethash "deferred_receipt_id"
                                                      *publication-contract-current*))))
          (near-term-tool-test-check "tool does not claim cognition occurred"
                                     (search "no thinking"
                                             (gethash "warning" content))))
        (near-term-intention-tool-uninstall)
        (near-term-tool-test-check "rollback removes tool"
                                   (not (gethash "listed"
                                                 (near-term-intention-tool-report)))))
    (setf *tools* prior-tools
          *near-term-intention-tool-installed* prior-installed
          *near-term-intention-tool-base-execute* prior-base)
    (if prior-execute
        (setf (fdefinition 'execute) prior-execute)
        (fmakunbound 'execute))
    (if prior-legacy-gate
        (setf (fdefinition 'tool-dispatch-legacy-wrapper-enabled-p)
              prior-legacy-gate)
        (fmakunbound 'tool-dispatch-legacy-wrapper-enabled-p))
    (if prior-delivery-readiness
        (setf (fdefinition 'initiative-committed-delivery-readiness)
              prior-delivery-readiness)
        (fmakunbound 'initiative-committed-delivery-readiness))
    (if prior-budget-status
        (setf (fdefinition 'tick-budget-status) prior-budget-status)
        (fmakunbound 'tick-budget-status))
    (when (probe-file *near-term-intention-file*)
      (delete-file *near-term-intention-file*))))

(format t "~%~a passed, ~a failed~%"
        *near-term-tool-test-pass* *near-term-tool-test-fail*)
(when (plusp *near-term-tool-test-fail*) (sb-ext:exit :code 1))
