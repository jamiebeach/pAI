;;;; stabilization-config.lisp -- reversible rollout modes for STAB-01+.
;;;;
;;;; This file is deliberately loaded before memory-nodes.lisp. Modes persist
;;;; atomically, default to legacy/normal, and can be returned to the complete
;;;; legacy set through one out-of-band call without restarting the agent.

(in-package :agent)

(export '(stabilization-mode-report stabilization-set-mode
          stabilization-set-all-legacy save-stabilization-config
          load-stabilization-config))

(defparameter *stabilization-config-file*
  (pathname (or (uiop:getenv "PAI_STABILIZATION_CONFIG")
                "/agent/state/stabilization-config.json")))
(defvar *stabilization-config-lock* (bt:make-lock "stabilization-config"))

(declaim (ftype function stabilization-mode-report))

;; DEFVAR, not DEFPARAMETER: a harmless file reload must not silently reset a
;; live rollout. LOAD-STABILIZATION-CONFIG below remains authoritative at boot.
(defvar *epistemic-memory-mode* :legacy)
(defvar *cognitive-generation-mode* :legacy)
(defvar *context-projection-mode* :legacy)
(defvar *temporal-response-policy-mode* :legacy)
(defvar *initiative-policy-mode* :legacy)
(defvar *initiative-delivery-mode* :shadow)
(defvar *latent-thoughts-mode* :legacy)
(defvar *epistemic-critic-mode* :off)
(defvar *autonomous-write-mode* :normal)
(defvar *grounded-agency-mode* :legacy)
(defvar *near-term-intentions-mode* :off)
(defvar *conversation-context-budget-mode* :enforced)
(defvar *reciprocity-canary-mode* :shadow)
(defvar *retrieval-embedding-mode* :legacy)
(defvar *context-curator-mode* :off)
(defvar *memory-atom-decomposition-mode* :off)

(defparameter *stabilization-three-state-modes* '(:legacy :shadow :enforced))
(defparameter *stabilization-autonomous-modes* '(:normal :shadow-only :paused))
(defparameter *stabilization-critic-modes* '(:off :shadow :enforced))
(defparameter *stabilization-initiative-delivery-modes*
  '(:shadow :operator-only :external-approved))
(defparameter *stabilization-grounded-agency-modes* '(:legacy :shadow))
(defparameter *stabilization-reciprocity-canary-modes* '(:off :shadow :operator-only))

(defun %stabilization-normalize-mode (value)
  (etypecase value
    (keyword value)
    (symbol (intern (string-upcase (symbol-name value)) :keyword))
    (string (intern (string-upcase value) :keyword))))

(defun %stabilization-mode-slot (subsystem)
  (case (%stabilization-normalize-mode subsystem)
    (:epistemic-memory '*epistemic-memory-mode*)
    (:cognitive-generation '*cognitive-generation-mode*)
    (:context-projection '*context-projection-mode*)
    (:temporal-response-policy '*temporal-response-policy-mode*)
    (:initiative-policy '*initiative-policy-mode*)
    (:initiative-delivery '*initiative-delivery-mode*)
    (:latent-thoughts '*latent-thoughts-mode*)
    (:epistemic-critic '*epistemic-critic-mode*)
    (:autonomous-write '*autonomous-write-mode*)
    (:grounded-agency '*grounded-agency-mode*)
    (:near-term-intentions '*near-term-intentions-mode*)
    (:conversation-context-budget '*conversation-context-budget-mode*)
    (:reciprocity-canary '*reciprocity-canary-mode*)
    (:retrieval-embedding '*retrieval-embedding-mode*)
    (:context-curator '*context-curator-mode*)
    (:memory-atom-decomposition '*memory-atom-decomposition-mode*)
    (otherwise (error "Unknown stabilization subsystem: ~a" subsystem))))

(defun %stabilization-valid-mode-p (slot mode)
  (member mode (cond ((eq slot '*autonomous-write-mode*)
                      *stabilization-autonomous-modes*)
                     ((eq slot '*epistemic-critic-mode*)
                      *stabilization-critic-modes*)
                     ((eq slot '*initiative-delivery-mode*)
                      *stabilization-initiative-delivery-modes*)
                     ((eq slot '*grounded-agency-mode*)
                      *stabilization-grounded-agency-modes*)
                     ((eq slot '*near-term-intentions-mode*)
                      *stabilization-critic-modes*)
                     ((eq slot '*reciprocity-canary-mode*)
                      *stabilization-reciprocity-canary-modes*)
                     ((eq slot '*context-curator-mode*)
                      *stabilization-critic-modes*)
                     ((eq slot '*memory-atom-decomposition-mode*)
                      '(:off :shadow))
                     (t *stabilization-three-state-modes*))))

(defun %stabilization-json-mode (mode)
  (string-downcase (symbol-name mode)))

(defun %stabilization-current-object ()
  (obj "schema_version" 1
       "epistemic_memory" (%stabilization-json-mode *epistemic-memory-mode*)
       "cognitive_generation" (%stabilization-json-mode *cognitive-generation-mode*)
       "context_projection" (%stabilization-json-mode *context-projection-mode*)
       "temporal_response_policy" (%stabilization-json-mode *temporal-response-policy-mode*)
       "initiative_policy" (%stabilization-json-mode *initiative-policy-mode*)
       "initiative_delivery" (%stabilization-json-mode *initiative-delivery-mode*)
       "latent_thoughts" (%stabilization-json-mode *latent-thoughts-mode*)
       "epistemic_critic" (%stabilization-json-mode *epistemic-critic-mode*)
       "autonomous_write" (%stabilization-json-mode *autonomous-write-mode*)
       "grounded_agency" (%stabilization-json-mode *grounded-agency-mode*)
       "near_term_intentions"
       (%stabilization-json-mode *near-term-intentions-mode*)
       "conversation_context_budget"
       (%stabilization-json-mode *conversation-context-budget-mode*)
       "reciprocity_canary"
       (%stabilization-json-mode *reciprocity-canary-mode*)
       "retrieval_embedding"
       (%stabilization-json-mode *retrieval-embedding-mode*)
       "context_curator"
       (%stabilization-json-mode *context-curator-mode*)
       "memory_atom_decomposition"
       (%stabilization-json-mode *memory-atom-decomposition-mode*)))

(defun save-stabilization-config ()
  (bt:with-lock-held (*stabilization-config-lock*)
    (ensure-directories-exist *stabilization-config-file*)
    (let ((tmp (make-pathname :name "stabilization-config-tmp" :type "json"
                              :defaults *stabilization-config-file*)))
      (with-open-file (out tmp :direction :output :if-exists :supersede
                               :if-does-not-exist :create :external-format :utf-8)
        (let ((*print-pretty* nil))
          (write-string (shasht:write-json (%stabilization-current-object) nil) out))
        (terpri out)
        (finish-output out))
      (uiop:rename-file-overwriting-target tmp *stabilization-config-file*)))
  t)

(defun %stabilization-load-one (data key slot)
  (let* ((raw (gethash key data))
         (mode (and raw (%stabilization-normalize-mode raw))))
    (when (and mode (%stabilization-valid-mode-p slot mode))
      (set slot mode))))

(defun load-stabilization-config ()
  "Load valid persisted fields independently. A malformed file fails closed
to the already-bound defaults rather than partially inventing mode values."
  (handler-case
      (when (probe-file *stabilization-config-file*)
        (let ((data (shasht:read-json
                     (uiop:read-file-string *stabilization-config-file*))))
          (%stabilization-load-one data "epistemic_memory" '*epistemic-memory-mode*)
          (%stabilization-load-one data "cognitive_generation" '*cognitive-generation-mode*)
          (%stabilization-load-one data "context_projection" '*context-projection-mode*)
          (%stabilization-load-one data "temporal_response_policy" '*temporal-response-policy-mode*)
          (%stabilization-load-one data "initiative_policy" '*initiative-policy-mode*)
          (%stabilization-load-one data "initiative_delivery" '*initiative-delivery-mode*)
          (%stabilization-load-one data "latent_thoughts" '*latent-thoughts-mode*)
          (%stabilization-load-one data "epistemic_critic" '*epistemic-critic-mode*)
          (%stabilization-load-one data "autonomous_write" '*autonomous-write-mode*)
          (%stabilization-load-one data "grounded_agency" '*grounded-agency-mode*)
          (%stabilization-load-one data "near_term_intentions" '*near-term-intentions-mode*)
          (%stabilization-load-one data "conversation_context_budget"
                                   '*conversation-context-budget-mode*)
          (%stabilization-load-one data "reciprocity_canary"
                                   '*reciprocity-canary-mode*)
          (%stabilization-load-one data "retrieval_embedding"
                                   '*retrieval-embedding-mode*)
          (%stabilization-load-one data "context_curator"
                                   '*context-curator-mode*)
          (%stabilization-load-one data "memory_atom_decomposition"
                                   '*memory-atom-decomposition-mode*)))
    (error (e)
      (format t "~&[stabilization-config] load failed; retaining bound modes: ~a~%" e)
      nil))
  (stabilization-mode-report))

(defun stabilization-mode-report ()
  (%stabilization-current-object))

(defun stabilization-set-mode (subsystem value &key (reason "operator change")
                                                   (actor "out-of-band"))
  "Persist one validated transition and audit it when LOG-EVENT is present."
  (let* ((slot (%stabilization-mode-slot subsystem))
         (mode (%stabilization-normalize-mode value)))
    (unless (%stabilization-valid-mode-p slot mode)
      (error "Invalid mode ~a for ~a" value subsystem))
    (let ((before (symbol-value slot)))
      (labels ((sync-tool (target)
                 (when (eq slot '*near-term-intentions-mode*)
                    (cond ((and (not (eq target :enforced))
                                (fboundp 'near-term-intention-tool-uninstall))
                           (funcall 'near-term-intention-tool-uninstall))
                          ((and (eq target :enforced)
                                (fboundp 'near-term-intention-tool-install))
                          (funcall 'near-term-intention-tool-install)))))
               (sync-memory-atom-worker (target)
                 (when (eq slot '*memory-atom-decomposition-mode*)
                   (cond ((and (eq target :off)
                               (fboundp 'memory-atom-shadow-worker-stop))
                          (funcall 'memory-atom-shadow-worker-stop))
                         ((and (eq target :shadow)
                               (fboundp 'memory-atom-shadow-worker-start))
                          (funcall 'memory-atom-shadow-worker-start))))))
        (set slot mode)
        (handler-case
            (progn (sync-tool mode) (sync-memory-atom-worker mode)
                   (save-stabilization-config))
          (error (e)
            (set slot before)
            (ignore-errors (sync-tool before))
            (ignore-errors (sync-memory-atom-worker before))
            (error e))))
      (when (fboundp 'log-event)
        (funcall 'log-event "stabilization-mode-changed"
                 (obj "subsystem" (string-downcase (symbol-name
                                                     (%stabilization-normalize-mode subsystem)))
                      "before" (%stabilization-json-mode before)
                      "after" (%stabilization-json-mode mode)
                      "reason" reason "actor" actor)))
      (stabilization-mode-report))))

(defun stabilization-set-all-legacy (&key (reason "operator rollback")
                                          (actor "out-of-band"))
  "One-call, no-restart rollback for every stabilization subsystem."
  (let ((before (stabilization-mode-report)))
    (setf *epistemic-memory-mode* :legacy
          *cognitive-generation-mode* :legacy
          *context-projection-mode* :legacy
          *temporal-response-policy-mode* :legacy
          *initiative-policy-mode* :legacy
          *initiative-delivery-mode* :shadow
          *latent-thoughts-mode* :legacy
          *epistemic-critic-mode* :off
          *autonomous-write-mode* :normal
          *grounded-agency-mode* :legacy
          *near-term-intentions-mode* :off
          *conversation-context-budget-mode* :legacy
          *reciprocity-canary-mode* :off
          *retrieval-embedding-mode* :legacy
          *context-curator-mode* :off
          *memory-atom-decomposition-mode* :off)
    (when (fboundp 'near-term-intention-tool-uninstall)
      (funcall 'near-term-intention-tool-uninstall))
    (when (fboundp 'memory-atom-shadow-worker-stop)
      (funcall 'memory-atom-shadow-worker-stop))
    (save-stabilization-config)
    (when (fboundp 'log-event)
      (funcall 'log-event "stabilization-mode-changed"
               (obj "subsystem" "all" "before" before
                    "after" (stabilization-mode-report)
                    "reason" reason "actor" actor)))
    (stabilization-mode-report)))

(define-init :configure stabilization-config-configure
    "Read configuration for stabilization-config."
  (load-stabilization-config))
