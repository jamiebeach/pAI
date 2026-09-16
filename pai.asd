;;;; pai.asd -- system definition for the pAI substrate.
;;;;
;;;; This file replaces the container entrypoint load chain as the module
;;;; system. The chain was previously ~250 lines of shell inside a
;;;; Dockerfile, where every ordering constraint lived only in accumulated
;;;; comments. It is now declared, versioned, diffable, testable, and
;;;; loadable on any platform with SBCL -- which was the point of P0.
;;;;
;;;; WHY ONE SERIAL SYSTEM, NOT PER-LAYER SUBSYSTEMS
;;;;
;;;; The physical directories (src/kernel, src/mind, src/adapters) express
;;;; the intended stratification. The dependency graph does not enforce it
;;;; yet, deliberately: the codebase grew through live sequential loading
;;;; with wrap chains, so real cross-layer cycles exist. One concrete
;;;; example: kernel/agent_print wraps call-model, which is defined in
;;;; pending-split/enhancements, which itself needs kernel/agent. Declaring
;;;; per-layer subsystems today would either not load, or would require
;;;; inventing a layering the code does not actually have.
;;;;
;;;; So: :serial t over the exact proven boot order. This is honest about
;;;; what is true, and it is already the whole win -- the load order is now
;;;; data under version control instead of shell in a comment block.
;;;; Subsystems get introduced as cycles are broken, one seam at a time,
;;;; each with its own qualification. Do not add speculative subsystem
;;;; structure ahead of that work.

;; Standalone test harnesses LOAD-ASD without installing a source registry.
;; Register the inert sibling library definition; ASDF owns actual loading.
(asdf:load-asd (merge-pathnames "pai-memory-access.asd" *load-pathname*))

(defsystem "pai"
  :description "pAI -- an event-sourced personal agent substrate."
  :license "MIT"
  ;; ASDF versions use dotted numeric components. Release maturity belongs in
  ;; project status/docs, not in a version token ASDF rejects on every boot.
  :version "0.1.0"
  :depends-on ("dexador" "shasht" "hunchentoot" "cl-base64" "postmodern" "local-time" "ironclad" "cffi" "pai-memory-access")
  :serial t
  :components
  (
   ;; Frontend assets are real files, embedded at compile time by the ASSET
   ;; macro. Declaring them here is not decoration: without it ASDF does not
   ;; know an edited .html invalidates the .lisp that embeds it, and the
   ;; change silently would not take effect on rebuild.
   (:static-file "src/adapters/web/assets/terminal.html")
   (:static-file "src/adapters/web/assets/login.html")
   (:static-file "src/adapters/web/assets/viewport.js")
   (:static-file "src/adapters/web/assets/app-shell.js")
   (:static-file "src/adapters/web/assets/graph-explorer.html")
   (:static-file "src/adapters/web/assets/graph-explorer.js")
   (:static-file "src/adapters/web/assets/observability.html")
   (:static-file "src/adapters/web/assets/observability.js")
   (:static-file "src/adapters/web/assets/settings.html")
   (:static-file "src/adapters/web/assets/settings.js")
   (:static-file "src/adapters/web/assets/manifest.webmanifest")
   (:static-file "src/adapters/web/assets/service-worker.js")
   (:static-file "src/adapters/web/assets/pwa-icon.svg")
   (:file "src/kernel/agent")
   ;; Infrastructure macros must precede every file that uses them:
   ;; DEFINE-SEAM, DEFINE-INIT and ASSET are macros, so a file compiled
   ;; before they exist parses their call as a function call and its
   ;; first argument as an unbound variable.
   (:file "src/kernel/init")
   (:file "src/kernel/cognition-runtime")
   (:file "src/kernel/seams")
   (:file "src/kernel/assets")
   (:file "src/kernel/self-mod")
   (:file "src/kernel/tool-dispatch-boot-mode")
   (:file "src/kernel/virtual-clock")
   (:file "src/kernel/self-mod-verifier-calibration")
   (:file "src/kernel/self-mod-phase4")
   (:file "src/kernel/self-mod-sandbox")
   (:file "src/kernel/agent_helpers")
   (:file "src/adapters/brave/brave-credential")
   (:file "src/tools/definitions")
   (:file "src/kernel/agent_loop")
   (:file "src/pending-split/enhancements")
   (:file "src/mind/stabilization/stabilization-config")
   (:file "src/mind/context/conversation-context-budget")
   ;; Memory persistence capabilities are outcome-shaped rather than generic
   ;; SQL. The PostgreSQL adapter is currently a read-only qualification
   ;; source; no runtime consumer or authority is selected by loading it.
   (:file "src/mind/memory/memory-storage")
   (:file "src/adapters/postgres/postgres-memory-storage")
   (:file "src/mind/memory/memory-nodes")
   (:file "src/mind/memory/embedding-turn-cache")
   (:file "src/mind/memory/epistemic-memory")
   (:file "src/mind/memory/typed-retrieval")
   (:file "src/mind/publication/cognitive-call")
   (:file "src/adapters/postgres/postgres-backup")
   (:file "src/mind/drives/modulator")
   (:file "src/mind/ticks/tick-loop")
   (:file "src/mind/drives/attention-schema")
   (:file "src/mind/reflection/spreading-activation")
   (:file "src/mind/drives/drives")
   (:file "src/mind/drives/soul")
   (:file "src/kernel/agent-config")
   (:file "src/kernel/wrap-chain-registry")
   (:file "src/kernel/soft-edge-port-registry")
   (:file "src/mind/drives/self-model")
   (:file "src/mind/drives/prediction-journal")
   (:file "src/mind/initiative/conversational-initiative")
   (:file "src/adapters/telegram/telegram")
   (:file "src/adapters/env/user-time")
   (:file "src/kernel/agent_print")
   (:file "src/adapters/web/web-security")
   (:file "src/adapters/web/web")
   (:file "src/adapters/web/web-terminal")
   (:file "src/mind/conversation/conversation-persistence")
   (:file "src/mind/conversation/conversation-persistence-heartbeat")
   (:file "src/kernel/turn-watchdog")
   (:file "src/kernel/turn-cancellation")
   (:file "src/adapters/chat/chat")
   (:file "src/adapters/runware/runware")
   (:file "src/adapters/chat/repl-drop")
   (:file "src/kernel/core-snapshot")
   (:file "src/kernel/eval-journal")
   (:file "src/kernel/lisp-eval-safety")
   (:file "src/kernel/drift-monitor")
   (:file "src/kernel/event-log")
   ;; Backend-neutral durable storage contract plus the first local adapter.
   ;; Both are inert at load: no database is opened until explicitly selected.
   (:file "src/kernel/storage-substrate")
   (:file "src/adapters/sqlite/sqlite-storage")
   ;; Rebuildable checkpoints and memory live outside the sacred event DB.
   ;; This adapter remains inert until an explicit path is opened.
   (:file "src/adapters/sqlite/sqlite-derived-storage")
   (:file "src/adapters/sqlite/sqlite-import")
   (:file "src/kernel/runtime-observer-registry")
   (:file "src/kernel/runtime-observer-audit")
   (:file "src/kernel/self-mod-provenance")
   (:file "src/mind/reflection/episode-boundary")
   (:file "src/mind/drives/modulator-watchdog")
   (:file "src/mind/drives/soul-candidate-pool")
   (:file "src/mind/initiative/initiative-engine")
   (:file "src/mind/reflection/ambient-recall-diversity")
   (:file "src/mind/reflection/workout_nudge")
   (:file "src/mind/reflection/candidate-policy")
   (:file "src/mind/initiative/initiative-policy")
   (:file "src/mind/reflection/latent-thoughts")
   (:file "src/mind/reflection/latent-thoughts-v2")
   (:file "src/mind/reflection/explore-novelty")
   (:file "src/mind/initiative/feedback-loop-containment")
   (:file "src/mind/conversation/conversation-episodic-memory")
   (:file "src/mind/conversation/conversation-turn-capture")
   (:file "src/mind/publication/public-system-prompt")
   (:file "src/mind/context/context-curator-candidate")
   (:file "src/mind/context/turn-bundle-retrieval")
   (:file "src/mind/context/context-curator-consumer")
   (:file "src/mind/context/recall-selection")
   (:file "src/mind/context/context-projection")
   (:file "src/mind/ticks/tick-commit")
   ;; Concrete event-first cognition is inert until explicitly selected.
   (:file "src/mind/memory/sqlite-memory-router")
   (:file "src/mind/memory/memory-ledger-baseline")
   (:file "src/mind/ticks/tick-proposals")
   (:file "src/mind/reflection/near-term-workspace")
   (:file "src/mind/reflection/near-term-intentions")
   (:file "src/mind/memory/mind-memory-core")
   (:file "src/mind/memory/memory-architecture")
   (:file "src/mind/memory/memory-atom-candidate")
   (:file "src/mind/memory/memory-atom-shadow")
   (:file "src/tools/memory-search-tool")
   ;; Omitted from the seed by oversight, not by decision. The recovery
   ;; contract lists WEB-FETCH, READ-DELIVERABLE, WRITE-DELIVERABLE and
   ;; BOUNDED-WORK-TOOLS-REPORT as required, so without this file every boot
   ;; reported the contract INCOMPLETE -- which pauses autonomous writes as a
   ;; fail-safe. The agent came up degraded and said so only in a line nobody
   ;; was reading.
   (:file "src/tools/bounded-work-tools")
   (:file "src/tools/conscious-file-search-tool")
   (:file "src/tools/recursive-primitive-tools")
   (:file "src/mind/ticks/scheduler")
   (:file "src/mind/reflection/reflection-novelty")
   (:file "src/mind/ticks/tick-execution")
   (:file "src/adapters/observability/llm-debug-capture")
   (:file "src/adapters/observability/observability-tracing")
   (:file "src/kernel/heap-health")
   (:file "src/mind/publication/publication-contract")
   (:file "src/mind/reflection/candidate-representation")
   (:file "src/mind/initiative/reciprocity-canary")
   (:file "src/mind/publication/epistemic-critic")
   (:file "src/mind/publication/temporal-response-policy")
   (:file "src/tools/near-term-intention-tool")
   (:file "src/kernel/tool-dispatch/kernel-tool-dispatch-bootstrap")
   (:file "src/mind/publication/first-person-evidence")
   (:file "src/mind/publication/creative-projects")
   (:file "src/mind/publication/grounded-appraisal")
   (:file "src/mind/publication/claim-grants")
   (:file "src/mind/publication/semantic-publication")
   (:file "src/mind/publication/grounded-agency")
   (:file "src/mind/publication/grounded-agency-worker")
   (:file "src/mind/reflection/near-term-workspace-adapters")
   (:file "src/mind/memory/legacy-memory-audit")
   (:file "src/mind/stabilization/stabilization-evals")
   (:file "src/mind/initiative/pull-reciprocity")
   (:file "src/mind/publication/public-outbound-gateway")
   (:file "src/kernel/runtime-truth")
   (:file "src/kernel/replay-capsules")
   (:file "src/mind/observability/turn-trace-projection")
   (:file "src/mind/observability/runtime-settings")
   (:file "src/mind/observability/dashboard")
   (:file "src/mind/observability/admin-console")
   (:file "src/mind/stabilization/stabilization-smoke-tests")
   (:file "src/kernel/recovery-health")
   ;; Workstream Q, conscious-state runtime. Loaded last and deliberately
   ;; inert: nothing above calls into it, and it registers no init action, no
   ;; worker and no seam layer. Loading it cannot affect the :auto runtime,
   ;; which is the isolation condition the runtime spec attaches to starting
   ;; this work early. It becomes reachable only when a runtime registry
   ;; selects it (Q2).
   ;; The census manifest is source: the stimulus admission table is built
   ;; from it at load time. Declared static so an edit invalidates census.lisp,
   ;; the same staleness guard the frontend assets use.
   (:static-file "src/mind/conscious/event-type-census.sexp")
   (:file "src/mind/conscious/policy")
   (:file "src/mind/conscious/stimulus")
   (:file "src/mind/conscious/census")
   (:file "src/mind/conscious/concern")
   (:file "src/mind/conscious/codelets")
   (:file "src/mind/conscious/context")
   (:file "src/mind/conscious/inbox")
   (:file "src/mind/conscious/attention")
   (:file "src/mind/conscious/lifecycle")
   (:file "src/mind/conscious/lifecycle-runtime")
   (:file "src/mind/conscious/lifecycle-sources")
   (:file "src/mind/conscious/lifecycle-semantics")
   (:file "src/mind/conscious/motivation")
   (:file "src/mind/conscious/motivation-runtime")
   (:file "src/mind/conscious/affect-observation")
   (:file "src/mind/conscious/affect-disposition")
   (:file "src/mind/conscious/continuity-capsule")
   (:file "src/mind/conscious/work-docket")
   (:file "src/mind/conscious/state")
   (:file "src/mind/conscious/proposal")
   (:file "src/mind/conscious/context-assembly")
   (:file "src/mind/conscious/pulse")
   (:file "src/mind/conscious/pulse-runtime")
   (:file "src/mind/conscious/cognitive-work")
   (:file "src/mind/conscious/cognitive-work-runtime")
   (:file "src/mind/conscious/boundary-outcome")
   (:file "src/mind/conscious/runtime-composition")
   (:file "src/mind/conscious/tool-operation-runtime")
   (:file "src/mind/conscious/cognitive-work-context")
   (:file "src/mind/conscious/cognitive-work-executor")
   (:file "src/mind/conscious/cognitive-operation-executor")
   (:file "src/mind/conscious/runtime")
   ;; Pure rebuildable conversational episode/topic projection.  It loads
   ;; before the two runtime owners that consume it and performs no IO.
   (:file "src/mind/conversation/conversation-episode-graph")
   ;; Explicit derived persistence owner for that pure graph. Loading remains
   ;; inert; callers must supply a derived backend and ledger boundary.
     (:file "src/mind/conversation/conversation-episode-graph-storage")
   (:file "src/mind/conversation/conversation-episode-graph-sync")
   ;; Pure KG2 sealed formation contract and deterministic identity projector.
   ;; Provider and persistence owners are later files; loading remains inert.
     (:file "src/mind/knowledge/knowledge-graph-ontology")
     (:file "src/mind/knowledge/knowledge-graph-formation")
     (:file "src/mind/knowledge/knowledge-graph-formation-storage")
     (:file "src/mind/knowledge/knowledge-graph-formation-sync")
     (:file "src/mind/knowledge/knowledge-graph-formation-owner")
     (:file "src/mind/knowledge/knowledge-graph-formation-source")
     ;; Shared pure normalization used by both live KG formation and its lab.
     (:file "src/mind/knowledge/context-graph/package")
     (:file "src/mind/knowledge/context-graph/core")
     (:file "src/mind/knowledge/context-graph/grounding")
     (:file "src/mind/knowledge/context-graph/authority")
     (:file "src/mind/knowledge/context-graph/search")
     (:file "src/mind/knowledge/context-graph/resolution")
     (:file "src/mind/knowledge/context-graph/lifecycle")
     (:file "src/mind/knowledge/context-graph/authority-projection")
     (:file "src/mind/knowledge/context-graph/authority-retrieval")
     (:file "src/mind/knowledge/context-graph/runtime-candidates")
     (:file "src/mind/knowledge/context-graph/model-adapter")
     (:file "src/mind/knowledge/context-graph/simple-model-adapter")
     (:file "src/mind/knowledge/context-graph/staged-model-adapter")
     (:file "src/mind/knowledge/context-graph/identity-page-adapter")
     (:file "src/mind/knowledge/context-graph/runtime-generation")
     (:file "src/mind/knowledge/context-graph/runtime-owner")
     (:file "src/mind/knowledge/context-graph/runtime-context")
     (:file "src/mind/knowledge/context-graph/identity-page-owner")
     (:file "src/mind/knowledge/context-graph/identity-formation")
     ;; KG3 verified read path. It is inert until the recursive/operator
     ;; composition injects an explicit derived backend.
     (:file "src/mind/knowledge/knowledge-graph-search")
     (:file "src/mind/knowledge/knowledge-graph-search-storage")
     (:file "src/mind/knowledge/knowledge-graph-hybrid-retrieval")
     (:file "src/mind/knowledge/knowledge-graph-attention-context")
     (:file "src/mind/conscious/knowledge-graph-search-tool")
     ;; Authenticated operator visualization consumes the same read-only KG3
     ;; port through a separately injected adapter.
     (:file "src/adapters/web/web-graph-explorer")
   (:file "src/mind/conscious/conversation-runtime")
   (:file "src/mind/conscious/recursive-mind-runtime")
   (:file "src/mind/conscious/knowledge-graph-formation-adapter")
   (:file "src/mind/conscious/context-graph-runtime-adapter")
   (:file "src/mind/conscious/context-graph-budget")
   (:file "src/mind/conscious/conversation-work-loop")
   (:file "src/mind/conscious/interaction-runtime")
   ;; Qualified projection checkpoint/tail path. Selected only through an
   ;; explicit event authority; loading it remains inert.
   (:file "src/mind/conscious/storage-projection")
   ;; Explicit single-authority bridge, installed only by an operator entry
   ;; point after migration/parity/checkpoint qualification.
   (:file "src/adapters/sqlite/sqlite-event-authority")))

(defsystem "pai/tests"
  :description "Deterministic suites. Flat until boot parity confirms the
                source layer assignments; see README."
  :depends-on ("pai")
  :pathname "tests/"
  :perform (test-op (o c) (symbol-call :pai.test :run-all)))
