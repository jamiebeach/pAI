(in-package :agent)

(defvar *stab-config-pass* 0)
(defvar *stab-config-fail* 0)
(defvar *stab-config-events* nil)

(defun stab-config-check (name condition)
  (if condition
      (progn (incf *stab-config-pass*) (format t "  ok   ~a~%" name))
      (progn (incf *stab-config-fail*) (format t "  FAIL ~a~%" name))))

(setf (fdefinition 'log-event)
      (lambda (type payload &key caused-by)
        (declare (ignore caused-by))
        (push (list type payload) *stab-config-events*)
        (length *stab-config-events*)))

(load (test-source "stabilization-config.lisp"))

(format t "~%== stabilization config defaults and transitions ==~%")
(stab-config-check "defaults are legacy/normal"
                   (and (eq *epistemic-memory-mode* :legacy)
                        (eq *cognitive-generation-mode* :legacy)
                        (eq *context-projection-mode* :legacy)
                        (eq *temporal-response-policy-mode* :legacy)
                        (eq *initiative-policy-mode* :legacy)
                        (eq *initiative-delivery-mode* :shadow)
                        (eq *latent-thoughts-mode* :legacy)
                        (eq *epistemic-critic-mode* :off)
                        (eq *autonomous-write-mode* :normal)
                        (eq *grounded-agency-mode* :legacy)
                        (eq *near-term-intentions-mode* :off)
                        (eq *conversation-context-budget-mode* :enforced)
                        (eq *reciprocity-canary-mode* :shadow)
                        (eq *retrieval-embedding-mode* :legacy)
                        (eq *context-curator-mode* :off)
                        (eq *memory-atom-decomposition-mode* :off)))

(stabilization-set-mode :epistemic-memory :shadow :reason "test")
(stab-config-check "valid transition takes effect"
                   (eq *epistemic-memory-mode* :shadow))
(stab-config-check "transition is audited"
                   (find "stabilization-mode-changed" *stab-config-events*
                         :key #'first :test #'string=))
(stab-config-check "config persisted"
                   (probe-file *stabilization-config-file*))
(stab-config-check "temporary file does not remain"
                   (not (probe-file (make-pathname :name "stabilization-config-tmp"
                                                   :type "json"
                                                   :defaults *stabilization-config-file*))))

(let ((before *epistemic-memory-mode*) (signalled nil))
  (handler-case (stabilization-set-mode :epistemic-memory :invalid)
    (error () (setf signalled t)))
  (stab-config-check "invalid mode signals" signalled)
  (stab-config-check "invalid mode leaves state unchanged"
                     (eq before *epistemic-memory-mode*)))

(setf *epistemic-memory-mode* :legacy)
(load-stabilization-config)
(stab-config-check "persisted mode reloads" (eq *epistemic-memory-mode* :shadow))

(stabilization-set-mode :temporal-response-policy :shadow :reason "test")
(stab-config-check "temporal response policy has an independent rollout mode"
                   (eq *temporal-response-policy-mode* :shadow))

(stabilization-set-mode :epistemic-critic :shadow :reason "test")
(stab-config-check "epistemic critic has a default-off independent rollout"
                   (eq *epistemic-critic-mode* :shadow))
(stabilization-set-mode :epistemic-critic :enforced :reason "test")
(stab-config-check "epistemic critic supports removal-only enforcement"
                   (eq *epistemic-critic-mode* :enforced))

(stabilization-set-mode :initiative-delivery :operator-only :reason "test")
(stab-config-check "the operator-only initiative delivery is independently gated"
                   (eq *initiative-delivery-mode* :operator-only))
(stabilization-set-mode :initiative-delivery :external-approved :reason "test")
(stab-config-check "external initiative delivery requires its explicit mode"
                   (eq *initiative-delivery-mode* :external-approved))

(stabilization-set-mode :latent-thoughts :shadow :reason "test")
(stab-config-check "latent v2 has an independent shadow rollout"
                   (eq *latent-thoughts-mode* :shadow))

(stabilization-set-mode :grounded-agency :shadow :reason "test")
(stab-config-check "grounded agency has an independent shadow-only rollout"
                   (eq *grounded-agency-mode* :shadow))
(let ((signalled nil))
  (handler-case (stabilization-set-mode :grounded-agency :enforced)
    (error () (setf signalled t)))
  (stab-config-check "grounded agency rejects misleading enforced mode"
                     signalled))

(stabilization-set-mode :near-term-intentions :shadow :reason "test")
(stab-config-check "near-term intentions have an independent off/shadow/enforced rollout"
                   (eq *near-term-intentions-mode* :shadow))
(stabilization-set-mode :near-term-intentions :enforced :reason "test")
(stab-config-check "near-term intentions support bounded enforcement"
                   (eq *near-term-intentions-mode* :enforced))

(stabilization-set-mode :conversation-context-budget :enforced :reason "test")
(stab-config-check "conversation context budget has an independent rollout mode"
                   (eq *conversation-context-budget-mode* :enforced))

(stabilization-set-mode :reciprocity-canary :operator-only :reason "test")
(stab-config-check "reciprocity canary has an independent the operator-only rollout"
                   (eq *reciprocity-canary-mode* :operator-only))
(let ((signalled nil))
  (handler-case (stabilization-set-mode :reciprocity-canary :enforced)
    (error () (setf signalled t)))
  (stab-config-check "reciprocity canary rejects broad enforced mode" signalled))

(stabilization-set-mode :retrieval-embedding :enforced :reason "test")
(stab-config-check "typed retrieval embedding has an independent rollout mode"
                   (eq *retrieval-embedding-mode* :enforced))
(stabilization-set-mode :context-curator :enforced :reason "test")
(stab-config-check "context curator supports direct bounded consumption"
                   (eq *context-curator-mode* :enforced))
(stabilization-set-mode :memory-atom-decomposition :shadow :reason "test")
(stab-config-check "memory atom decomposition has an off/shadow rollout mode"
                   (eq *memory-atom-decomposition-mode* :shadow))
(let ((signalled nil))
  (handler-case (stabilization-set-mode :memory-atom-decomposition :enforced)
    (error () (setf signalled t)))
  (stab-config-check "memory atom decomposition rejects enforced mode" signalled))

(stabilization-set-mode :autonomous-write :paused :reason "test")
(stab-config-check "autonomous mode vocabulary is separate"
                   (eq *autonomous-write-mode* :paused))
(stabilization-set-all-legacy :reason "test rollback")
(stab-config-check "one-call rollback restores every default"
                   (and (eq *epistemic-memory-mode* :legacy)
                        (eq *cognitive-generation-mode* :legacy)
                        (eq *context-projection-mode* :legacy)
                        (eq *temporal-response-policy-mode* :legacy)
                        (eq *initiative-policy-mode* :legacy)
                        (eq *initiative-delivery-mode* :shadow)
                        (eq *latent-thoughts-mode* :legacy)
                        (eq *epistemic-critic-mode* :off)
                        (eq *autonomous-write-mode* :normal)
                        (eq *grounded-agency-mode* :legacy)
                        (eq *near-term-intentions-mode* :off)
                        (eq *conversation-context-budget-mode* :legacy)
                        (eq *reciprocity-canary-mode* :off)
                        (eq *retrieval-embedding-mode* :legacy)
                        (eq *context-curator-mode* :off)
                        (eq *memory-atom-decomposition-mode* :off)))

(format t "~%~a passed, ~a failed~%" *stab-config-pass* *stab-config-fail*)
(when (plusp *stab-config-fail*) (sb-ext:exit :code 1))
