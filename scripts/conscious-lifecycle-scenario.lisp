;;;; conscious-lifecycle-scenario.lisp -- one native Q5 operator action.

(in-package :cl-user)

(require :asdf)

(defparameter *q5-scenario-repo-root*
  (uiop:pathname-parent-directory-pathname
   (uiop:pathname-directory-pathname *load-truename*)))

(defun %q5-scenario-quicklisp ()
  (let ((configured (uiop:getenv "PAI_QUICKLISP_SETUP")))
    (or (and configured (probe-file configured))
        (probe-file (merge-pathnames #P".tools/quicklisp/setup.lisp"
                                     *q5-scenario-repo-root*))
        (error "Quicklisp setup not found; run the local Lisp setup"))))

(load (%q5-scenario-quicklisp))
(push *q5-scenario-repo-root* asdf:*central-registry*)

(defun %q5-scenario-symbol (name) (intern (string-upcase name) :agent))
(defun %q5-scenario-call (name &rest arguments)
  (apply (symbol-function (%q5-scenario-symbol name)) arguments))

(defun %q5-scenario-required-env (name)
  (let ((value (uiop:getenv name)))
    (unless (and (stringp value) (plusp (length value)))
      (error "Q5 scenario requires ~a" name))
    value))

(defun %q5-scenario-guard ()
  (let ((agent (%q5-scenario-required-env "PAI_AGENT_ID"))
        (state (%q5-scenario-required-env "PAI_STATE_ROOT"))
        (action (%q5-scenario-required-env "PAI_Q5_LIFECYCLE_ACTION")))
    (unless (string= agent "q5-lifecycle-dev")
      (error "Refusing Q5 scenario outside its dedicated agent partition"))
    (unless (member action '("create" "inspect" "ready" "complete" "cancel")
                    :test #'string=)
      (error "Unknown Q5 scenario action ~s" action))
    (unless (uiop:directory-exists-p state)
      (error "Q5 scenario state directory is absent"))))

(%q5-scenario-guard)

;; Loading is noisy but the operator contract is one JSON object. Python keeps
;; warnings available on failure and presents only the marked result on success.
(let ((*standard-output* (make-broadcast-stream)))
  ;; HEAP-HEALTH historically autostarts at file load, before the selected
  ;; cognition lifecycle can restore the event ID watermark. Pre-bind its
  ;; DEFVAR gate so this dev CLI starts no worker and cannot append a duplicate
  ;; low ID during system load.
  (load (merge-pathnames #P"src/kernel/agent.lisp" *q5-scenario-repo-root*)))
(setf (symbol-value (intern "*HEAP-HEALTH-AUTOSTART-P*" :agent)) nil)
(let ((*standard-output* (make-broadcast-stream)))
  (asdf:load-system :pai))
(load (merge-pathnames #P"scripts/conscious-lifecycle-scenario-core.lisp"
                       *q5-scenario-repo-root*))

(defvar *q5-scenario-delivery-attempts* 0)
(defvar *q5-scenario-provider-calls* 0)
(defvar *q5-scenario-effect-calls* 0)

(defun %q5-scenario-deny-authority (name counter-symbol)
  "Replace one process-local authority seam with a measured fail-closed stub."
  (let ((symbol (%q5-scenario-symbol name)))
    (unless (fboundp symbol)
      (error "Q5 scenario authority seam ~a is absent" name))
    (setf (symbol-function symbol)
          (lambda (&rest ignored)
            (declare (ignore ignored))
            (incf (symbol-value counter-symbol))
            (error "Q5 lifecycle scenario has no ~a authority" name)))))

;; These process-local replacements turn the zeroes in the operator report
;; into measurements. Any attempted provider or effect call increments its
;; counter and aborts the action rather than touching an external adapter.
(%q5-scenario-deny-authority "CALL-MODEL" '*q5-scenario-provider-calls*)
(%q5-scenario-deny-authority "RAW-CALL-MODEL" '*q5-scenario-provider-calls*)
(%q5-scenario-deny-authority "EXECUTE" '*q5-scenario-effect-calls*)

(handler-case
    (progn
      (%q5-scenario-call "%event-restore-next-id")
      (%q5-scenario-call "near-term-intention-load")
      (setf (symbol-value (%q5-scenario-symbol "*near-term-intentions-mode*"))
            :enforced
            (symbol-value (%q5-scenario-symbol "*autonomous-write-mode*"))
            :normal
            (symbol-value
             (%q5-scenario-symbol "*near-term-intention-delivery-fn*"))
            nil)
      ;; Targeted cognition boot only. START is intentionally absent: the
      ;; conscious runtime owns no worker, and this scenario never processes
      ;; due intentions.
      (%q5-scenario-call "cognition-runtime-configure")
      (%q5-scenario-call "cognition-runtime-install")
      (%q5-scenario-call "cognition-runtime-restore")
      (%q5-scenario-call "cognition-runtime-verify")
      (let* ((action (%q5-scenario-required-env "PAI_Q5_LIFECYCLE_ACTION"))
             (result
               (%q5-scenario-call
                "conscious-lifecycle-scenario-run" action
                :subject (uiop:getenv "PAI_Q5_LIFECYCLE_SUBJECT")
                :aim (uiop:getenv "PAI_Q5_LIFECYCLE_AIM")
                :result-summary (uiop:getenv "PAI_Q5_LIFECYCLE_ARTIFACT")
                :observed-reply
                (uiop:getenv "PAI_Q5_LIFECYCLE_OBSERVED_REPLY"))))
        ;; Rebuild the actual selected consumer after the producer action.
        (%q5-scenario-call "cognition-runtime-restore")
        (let* ((projection
                 (symbol-value (%q5-scenario-symbol
                                "*cognition-runtime-projection*")))
               (awaited-slot (and (hash-table-p projection)
                                  (gethash "awaited" projection)))
               (awaited (and (hash-table-p awaited-slot)
                             (gethash "value" awaited-slot))))
          (setf (gethash "selected_runtime_awaiting_count" result)
                (if (vectorp awaited) (length awaited) 0)
                (gethash "delivery_attempts" result)
                *q5-scenario-delivery-attempts*
                (gethash "provider_calls" result)
                *q5-scenario-provider-calls*
                (gethash "effect_calls" result)
                *q5-scenario-effect-calls*))
        (format t "~&CONSCIOUS-Q5-SCENARIO-BEGIN~%")
        (shasht:write-json result *standard-output*)
        (terpri)
        (format t "CONSCIOUS-Q5-SCENARIO-END~%")))
  (error (condition)
    (format *error-output* "~&CONSCIOUS-Q5-SCENARIO-ERROR: ~a~%" condition)
    (uiop:quit 1)))
