;;;; recovery-health.lisp -- cold-recovery reconciliation, 2026-07-29.
;;;;
;;;; This is intentionally read-only.  A boot report must not "repair" an
;;;; incomplete recovery by inventing state, starting duplicate threads, or
;;;; mutating the conversation.  It makes the recovery contract inspectable
;;;; and lets the entrypoint fail visibly in its logs if a required mounted
;;;; source file or definition was omitted from the load chain.

(in-package :agent)

(export '(pai-recovery-report pai-recovery-assert))

(defparameter *pai-recovery-source-root*
  (let ((root (or (uiop:getenv "PAI_APPLICATION_ROOT")
                  (uiop:getenv "PAI_R3A_APPLICATION_ROOT"))))
    (if (and root (plusp (length root))) (pathname root) #P"")))

(defparameter *pai-recovery-required-files*
  '("agent_loop.lisp" "brave-credential.lisp" "enhancements.lisp" "memory-nodes.lisp"
    "stabilization-config.lisp" "conversation-context-budget.lisp"
    "user-time.lisp" "scheduler.lisp"
    "epistemic-memory.lisp" "typed-retrieval.lisp"
    "cognitive-call.lisp" "embedding-turn-cache.lisp"
    "tick-commit.lisp" "tick-proposals.lisp"
    "tick-execution.lisp"
    "modulator.lisp" "tick-loop.lisp" "drives.lisp" "self-model.lisp"
    "conversational-initiative.lisp" "conversation-persistence.lisp"
    "conversation-persistence-heartbeat.lisp" "turn-cancellation.lisp"
    "event-log.lisp" "runtime-observer-registry.lisp"
    "runtime-observer-audit.lisp" "runware.lisp"
    "bounded-work-tools.lisp"
    "lisp-eval-safety.lisp"
    "episode-boundary.lisp" "initiative-engine.lisp"
    "candidate-policy.lisp" "initiative-policy.lisp"
    "latent-thoughts.lisp" "latent-thoughts-v2.lisp" "explore-novelty.lisp"
    "feedback-loop-containment.lisp"
    "legacy-memory-audit.lisp" "stabilization-smoke-tests.lisp"
    "stabilization-evals.lisp"
    "conversation-episodic-memory.lisp" "conversation-turn-capture.lisp"
    "mind-memory-core.lisp" "memory-architecture.lisp" "memory-atom-candidate.lisp"
    "memory-atom-shadow.lisp"
    "public-system-prompt.lisp"
    "templates/PAI-IDENTITY.default.md"
    "templates/PAI-VOICE.default.md"
    "context-curator-candidate.lisp" "turn-bundle-retrieval.lisp"
    "context-curator-consumer.lisp"
    "context-projection.lisp" "memory-search-tool.lisp"
    "publication-contract.lisp"
    "candidate-representation.lisp"
    "reciprocity-canary.lisp" "pull-reciprocity.lisp"
    "epistemic-critic.lisp"
    "temporal-response-policy.lisp" "llm-debug-capture.lisp"
    "first-person-evidence.lisp" "grounded-agency-worker.lisp"
    "creative-projects.lisp" "grounded-appraisal.lisp" "claim-grants.lisp"
    "semantic-publication.lisp" "grounded-agency.lisp"
    "reflection-novelty.lisp" "observability-tracing.lisp" "heap-health.lisp"
    "near-term-workspace.lisp" "near-term-intentions.lisp"
    "near-term-intention-tool.lisp" "near-term-workspace-adapters.lisp"
    "public-outbound-gateway.lisp" "runtime-truth.lisp"
    "replay-capsules.lisp" "dashboard.lisp" "admin-console.lisp"
    "wrap-chain-registry.lisp"))

(defparameter *pai-recovery-required-functions*
  '(agent-loop auto-turn brave-api-key brave-credential-status
    memory-write-node memory-recall drives-start
    stabilization-mode-report stabilization-set-all-legacy
    conversation-context-budget-manage conversation-context-budget-report
    conversation-context-budget-config-report conversation-context-budget-update
    llm-debug-current-public-context admin-console-configured-p
    public-system-prompt-report public-system-prompt-render-stable
    public-system-prompt-update public-system-prompt-rollback
    pai-timezone-name pai-format-local-time pai-schedule-once
    pai-schedule-in pai-schedule-cron pai-schedule-cancel
    pai-schedule-list pai-scheduler-report pai-scheduler-start
    ensure-epistemic-memory-schema epistemic-memory-schema-report
    memory-admit-node memory-lineage-roots memory-grounded-p
    memory-quarantine memory-supersede
    memory-search memory-search-turn-neighborhood memory-record-use
    typed-retrieval-report turn-bundle-build-candidates
    typed-retrieval-embedding-readiness
    context-curator-build-manifest context-curator-build-request
    context-curator-validate-response context-curator-compile-block
    context-curator-consume context-curator-report
    context-curator-archive-anomalous-budget-ledger
    context-curator-current-private-result
    context-curator-last-selected-private-result
    context-curator-initialize-budget-ledger context-curator-rollover-budget-ledger
    cognitive-call cognitive-call-report embedding-turn-cache-report
    lisp-eval-safety-report
    cancel-active-turn turn-cancellation-report
    tick-commit-validate tick-commit-apply tick-commit-report tick-terminal-call
    tick-build-proposal tick-execute-proposal tick-proposal-report
    tick-loop-start self-model-propose-revision feedback-loop-containment-report
    detect-boundary
    %drives-event-initiate log-event replay-events contact-budget-report
    runtime-observer-register runtime-observer-emit runtime-observer-report
    runtime-observer-assert runtime-observer-audit-report
    runtime-observer-audit-assert runtime-authority-report
    runtime-authority-assert make-public-outbound-envelope
    public-outbound-gateway-report public-outbound-audit-observer-report
    runtime-truth-manifest runtime-truth-assert
    runtime-nonreply-transport-source-audit
    runtime-nonreply-transport-source-assert
    runtime-transport-inventory replay-capsule-capture replay-capsule-report
    replay-capsule-assert replay-capsule-start replay-capsule-stop
    replay-capsule-scheduled-step replay-capsule-extract-fixture
    initiative-policy-report initiative-v2-evaluate initiative-v2-report
    latent-thought-report latent-incubate latent-v2-report latent-v2-seed
    latent-v2-mark-expressed
    legacy-memory-audit-dry-run legacy-memory-audit-report
    turn-capture-report turn-capture-reconcile turn-capture-worker-start
    ensure-memory-architecture-schema memory-architecture-schema-report
    memory-atom-build-manifest memory-atom-build-request
    memory-atom-validate-response ensure-memory-atom-shadow-schema
    memory-atom-shadow-schema-report memory-atom-shadow-enqueue-turn
    memory-atom-shadow-worker-step memory-atom-shadow-worker-start
    memory-atom-shadow-worker-stop memory-atom-shadow-reconcile
    memory-atom-shadow-report
    memory-atom-shadow-current-private-review
    build-context-projection render-context-projection context-projection-report
    search-memory memory-search-tool-report
    publication-contract-violations publication-contract-factual-nucleus
    render-publication-generation-guidance realize-publication-draft
    publication-contract-report
    candidate-artifact-class candidate-generation-contract
    candidate-representation-normalize-record candidate-send-readiness-lint
    candidate-reduced-e0-envelope candidate-representation-report
    reciprocity-canary-consider-observation reciprocity-canary-observe-reply
    reciprocity-canary-report reciprocity-canary-snapshot
    pull-reciprocity-handle-inbound pull-reciprocity-report
    initiative-deliver-approved-message
    epistemic-critic-review epistemic-critic-observe epistemic-critic-realize
    epistemic-critic-report
    temporal-response-policy-violations
    near-term-workspace-materialize near-term-intention-report
    near-term-intention-due-p near-term-intention-process-due
    near-term-intention-observe-public-reply near-term-intention-tool-report
    near-term-workspace-shadow-snapshot
    initiative-committed-delivery-readiness
    initiative-deliver-committed-result
    dashboard-report find-state-files generate-image upload-reference-image
    web-fetch write-deliverable read-deliverable bounded-work-tools-report
    %reflection-cooldown-p %conversation-memory-record-turn
    reload-wrap-chain conversation-persistence-ready-p
    %conv-persist-write %conv-heartbeat-tick
    timing-trace-report timing-observability-report
    heap-health-report heap-health-sample heap-health-start
    stabilization-smoke-run stabilization-state-fingerprint
    stabilization-fingerprint-equivalent-p
    stabilization-evaluate-deterministic stabilization-promotion-readiness))

(defparameter *pai-recovery-required-mind-memory-functions*
  '("VALIDATE-STATE" "ROW-ELIGIBLE-P" "BUILD-ATOM-MANIFEST"
    "BUILD-ATOM-REQUEST" "VALIDATE-ATOM-RESPONSE" "CAPABILITY-REPORT"))

(defparameter *pai-recovery-grounded-required-functions*
  '(ensure-first-person-evidence-schema first-person-evidence-schema-report
    first-person-evidence-report grounded-agency-claim-next-operation
    grounded-agency-worker-start grounded-agency-worker-report
    creative-project-report agent-appraisal-compute agent-appraisal-current
    claim-grant-set-report semantic-publication-report
    semantic-publication-revalidate-candidate
    semantic-publication-record-initiative-observation
    semantic-publication-withhold-candidate
    initiative-v2-observe-shadow-candidate grounded-agency-observe-tick
    grounded-agency-report grounded-agency-alerts grounded-agency-state-fingerprint
    grounded-agency-reconcile-completions
    grounded-agency-install-worker-hooks
    grounded-agency-mark-recovery-ready))

(defun %recovery-effective-required-functions ()
  "Require the grounded contract only in images whose entrypoint loaded it.
This keeps the prior image bootable as a real rollback target while the new
image still proves every grounded function before authorizing recovery."
  (append *pai-recovery-required-functions*
          ;; Older rollback images do not load this additive diagnostic.
          ;; A new image that binds its mode must prove the implementation.
          (when (and (boundp '*llm-debug-capture-loaded-p*)
                     (symbol-value '*llm-debug-capture-loaded-p*))
            '(llm-debug-capture-call llm-debug-capture-index
              llm-debug-capture-read))
          (when (fboundp 'grounded-agency-report)
            *pai-recovery-grounded-required-functions*)))

(defun %recovery-thread-alive-p (symbol)
  (and (boundp symbol)
       (let ((thread (symbol-value symbol)))
         (and thread (bt:thread-alive-p thread)))))

(defun %recovery-memory-node-count ()
  "A real read-only Postgres probe. MEMORY-NODES.LISP owns the connection
details; this deliberately reuses its count helper rather than introducing
a second, potentially divergent database configuration path."
  (if (fboundp '%memory-node-count)
      (handler-case (values (funcall '%memory-node-count) t)
        (error () (values :null nil)))
      (values :null nil)))

(defun pai-recovery-report (&key (level :shallow))
  "Return a read-only report of the restart contract currently in force."
  (let ((files (obj)) (functions (obj))
        (mind-memory-functions (obj)) (threads (obj)))
    (dolist (file *pai-recovery-required-files*)
      ;; Resolve by basename through the source index rather than by joining
      ;; onto a root. The required-files list is flat, from when the tree was;
      ;; joining a bare name onto the root now misses every file, which made
      ;; this contract report INCOMPLETE on every boot and -- since an
      ;; incomplete contract pauses autonomous writes as a fail-safe -- quietly
      ;; degraded the agent each time it started.
      ;;
      ;; *PAI-RECOVERY-SOURCE-ROOT* still wins when explicitly configured, so a
      ;; deployment that really does keep a flat tree is unaffected.
      (setf (gethash file files)
            (not (null (if (and *pai-recovery-source-root*
                                (plusp (length (namestring
                                                *pai-recovery-source-root*))))
                           (probe-file (merge-pathnames
                                        file *pai-recovery-source-root*))
                           (pai-source-file file))))))
    (dolist (fn (%recovery-effective-required-functions))
      ;; FBOUNDP returns a generalized boolean. SBCL may return the function
      ;; object itself; persisting that in an otherwise JSON-shaped report can
      ;; make serializers traverse runtime code and fault. Materialize T/NIL.
      (setf (gethash (string-downcase (symbol-name fn)) functions)
            (not (null (fboundp fn)))))
    (let ((package (find-package :pai.mind.memory)))
      (dolist (name *pai-recovery-required-mind-memory-functions*)
        (let ((symbol (and package (find-symbol name package))))
          (setf (gethash (string-downcase name) mind-memory-functions)
                (not (null (and symbol (fboundp symbol))))))))
    (dolist (entry '(("tick-loop" *tick-thread*)
                     ("drives" *drives-thread*)
                     ("modulator-decay" *modulator-decay-thread*)
                     ("conversation-heartbeat" *conv-heartbeat-thread*)
                     ("turn-capture" *turn-capture-worker*)
                     ("memory-atom-shadow" *memory-atom-shadow-worker*)
                     ("scheduler" *pai-scheduler-thread*)
                     ("replay-capsule" *replay-capsule-thread*)
                     ("grounded-agency" *grounded-agency-worker-thread*)
                     ("heap-health" *heap-health-thread*)
                     ("repl-drop" *repl-drop-thread*)))
      (setf (gethash (first entry) threads) (%recovery-thread-alive-p (second entry))))
    (multiple-value-bind (node-count postgres-reachable) (%recovery-memory-node-count)
      (let ((report (obj "level" (string-downcase (symbol-name level))
           "files" files
           "functions" functions
           "mind_memory_functions" mind-memory-functions
           "threads" threads
           "conversation_messages" (if (boundp '*last-self-mod-history*)
                                       (length *last-self-mod-history*) 0)
           "conversation_input_ready"
           (and (fboundp 'conversation-persistence-ready-p)
                (funcall 'conversation-persistence-ready-p))
           "event_next_id" (if (boundp '*event-next-id*) *event-next-id* :null)
           "postgres_reachable" postgres-reachable
           "memory_node_count" node-count
           "grounded_contract_required" (if (fboundp 'grounded-agency-report)
                                              t nil))))
        (setf (gethash "grounded_agency" report)
              (if (fboundp 'grounded-agency-report)
                  (handler-case (grounded-agency-report)
                    (error (condition)
                      (obj "recovery_ready" nil
                           "error_class"
                           (string-downcase (symbol-name (type-of condition))))))
                  :null))
        (setf (gethash "grounded_schema" report)
              (if (fboundp 'first-person-evidence-schema-report)
                  (handler-case (first-person-evidence-schema-report)
                    (error (condition)
                      (obj "table_count" 0
                           "error_class"
                           (string-downcase (symbol-name (type-of condition))))))
                  :null))
        (when (member level '(:executable :state-fingerprint))
          (setf (gethash "executable" report)
                (if (fboundp 'stabilization-smoke-run)
                    (stabilization-smoke-run)
                    (obj "ok" nil "error_class" "missing-smoke-runner"))))
        (when (eq level :state-fingerprint)
          (setf (gethash "state_fingerprint" report)
                (if (fboundp 'stabilization-state-fingerprint)
                    (stabilization-state-fingerprint) :null)))
        report))))

(defun pai-recovery-assert (&key (level :shallow) expected-fingerprint)
  "Print and return T only when the mounted source and required definitions
are all present.  Thread status is reported but not made fatal: a thread may
be deliberately disabled by configuration, whereas a missing definition
means the recovery image is objectively incomplete."
  (let* ((report (pai-recovery-report :level level))
         (files (gethash "files" report))
         (functions (gethash "functions" report))
         (mind-memory-functions (gethash "mind_memory_functions" report))
         (missing-files nil) (missing-functions nil)
         (postgres-reachable (gethash "postgres_reachable" report))
         (grounded-shadow-p
           (and (boundp '*grounded-agency-mode*)
                (eq *grounded-agency-mode* :shadow)))
         (grounded-schema-ok
           (or (not grounded-shadow-p)
               (= (gethash "table_count" (gethash "grounded_schema" report) 0)
                  8)))
         (executable-ok
           (or (eq level :shallow)
               (gethash "ok" (gethash "executable" report))))
         (fingerprint-ok
           (or (not (eq level :state-fingerprint))
               (null expected-fingerprint)
               (let ((current (gethash "state_fingerprint" report)))
                 (if (hash-table-p expected-fingerprint)
                     (and (fboundp 'stabilization-fingerprint-equivalent-p)
                          (stabilization-fingerprint-equivalent-p
                           expected-fingerprint current))
                     (and (stringp expected-fingerprint)
                          (string= expected-fingerprint
                                   (gethash "fingerprint" current))))))))
    (maphash (lambda (name present) (unless present (push name missing-files))) files)
    (maphash (lambda (name present) (unless present (push name missing-functions))) functions)
    (maphash (lambda (name present)
               (unless present
                 (push (format nil "pai.mind.memory:~a" name)
                       missing-functions)))
             mind-memory-functions)
    (format t "~&[recovery] conversation=~a event-next-id=~a postgres-reachable=~a memory-nodes=~a~%"
            (gethash "conversation_messages" report)
            (gethash "event_next_id" report)
            postgres-reachable
            (gethash "memory_node_count" report))
    (if (or missing-files missing-functions (not postgres-reachable)
            (not grounded-schema-ok)
            (not executable-ok) (not fingerprint-ok))
        (progn
          (when (fboundp 'grounded-agency-mark-recovery-ready)
            (grounded-agency-mark-recovery-ready nil))
          ;; Preserve public chat fallback while preventing background writes.
          (when (boundp '*autonomous-write-mode*)
            (setf *autonomous-write-mode* :paused))
          (format t "~&[recovery] INCOMPLETE: level=~a missing files=~s functions=~s postgres-reachable=~s grounded-schema=~s executable=~s fingerprint=~s; autonomous generation paused.~%"
                  level (nreverse missing-files) (nreverse missing-functions)
                  postgres-reachable grounded-schema-ok executable-ok fingerprint-ok)
          nil)
        (progn
          (when (fboundp 'grounded-agency-mark-recovery-ready)
            (grounded-agency-mark-recovery-ready t))
          (format t "~&[recovery] OK: level ~a recovery contract passed.~%" level)
          t))))

;;; Cold-boot recovery assertion.
;;;
;;; The originating ENTRYPOINT ran this at :executable level on every cold
;;; boot, so a recovery image missing source files or definitions refused to
;;; start rather than coming up subtly incomplete. It lived in the container
;;; definition, not in source, and so was not carried over by the load/init
;;; separation.
(define-init :verify recovery-contract-assert
    "Fail closed unless the mounted source and every required definition are
     present, at :executable level."
  (pai-recovery-assert :level :executable))
