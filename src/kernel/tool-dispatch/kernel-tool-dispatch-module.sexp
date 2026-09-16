(:schema-version 1
 :module-id "pai.kernel.tool-dispatch"
 :layer :kernel
 :purpose "Pure exact-name tool resolution and data-only observer composition."
 :permitted-package-dependencies (:common-lisp)
 :forbidden-dependencies
 (:agent :provider :http :sql :filesystem :threads :event-append
  :model-invocation :prompt :context :transport :delivery
  :self-modification :authority-mutation)
 :public-interface
 ("VALIDATE-REGISTRY" "RESOLVE-TOOL" "COMPOSE-DISPATCH-PLAN"
  "CAPABILITY-REPORT")
 :observer-stage-order
 (:timing :tool-call-event :proposal-provenance :handler
  :modulator-appraisal :tool-result-event)
 :observer-stages
 ((:id :timing :owner-file "observability-tracing.lisp" :position :around)
  (:id :tool-call-event :owner-file "event-log.lisp" :position :before)
  (:id :proposal-provenance :owner-file "self-mod-provenance.lisp"
   :position :around :tool-names ("propose-loop"))
  (:id :modulator-appraisal :owner-file "modulator.lisp" :position :after)
  (:id :tool-result-event :owner-file "event-log.lisp" :position :after))
 :tools
 ((:name "lisp-eval" :handler-id :lisp-eval
   :handler-owner "self-mod.lisp" :schema-owner "agent.lisp"
   :availability-owner "self-mod.lisp"
   :observer-stage-ids
   (:timing :tool-call-event :modulator-appraisal :tool-result-event))
  (:name "propose-loop" :handler-id :propose-loop
   :handler-owner "self-mod.lisp" :schema-owner "self-mod.lisp"
   :availability-owner "self-mod.lisp"
   :observer-stage-ids
   (:timing :tool-call-event :proposal-provenance :modulator-appraisal
    :tool-result-event))
  (:name "web-search" :handler-id :web-search
   :handler-owner "enhancements.lisp"
   :schema-owner "enhancements.lisp"
   :availability-owner "enhancements.lisp"
   :observer-stage-ids
   (:timing :tool-call-event :modulator-appraisal :tool-result-event))
  (:name "broadcast-image" :handler-id :broadcast-image
   :handler-owner "web.lisp" :schema-owner "web.lisp"
   :availability-owner "web.lisp"
   :observer-stage-ids
   (:timing :tool-call-event :modulator-appraisal :tool-result-event))
  (:name "upload-reference-image" :handler-id :upload-reference-image
   :handler-owner "runware.lisp" :schema-owner "runware.lisp"
   :availability-owner "runware.lisp"
   :observer-stage-ids
   (:timing :tool-call-event :modulator-appraisal :tool-result-event))
  (:name "generate-image" :handler-id :generate-image
   :handler-owner "runware.lisp" :schema-owner "runware.lisp"
   :availability-owner "runware.lisp"
   :observer-stage-ids
   (:timing :tool-call-event :modulator-appraisal :tool-result-event))
  (:name "view-image" :handler-id :view-image
   :handler-owner "runware.lisp" :schema-owner "runware.lisp"
   :availability-owner "runware.lisp"
   :observer-stage-ids
   (:timing :tool-call-event :modulator-appraisal :tool-result-event))
  (:name "find-state-files" :handler-id :find-state-files
   :handler-owner "runware.lisp" :schema-owner "runware.lisp"
   :availability-owner "runware.lisp"
   :observer-stage-ids
   (:timing :tool-call-event :modulator-appraisal :tool-result-event))
  (:name "web-fetch" :handler-id :web-fetch
   :handler-owner "bounded-work-tools.lisp"
   :schema-owner "bounded-work-tools.lisp"
   :availability-owner "bounded-work-tools.lisp"
   :observer-stage-ids
   (:timing :tool-call-event :modulator-appraisal :tool-result-event))
  (:name "write-deliverable" :handler-id :write-deliverable
   :handler-owner "bounded-work-tools.lisp"
   :schema-owner "bounded-work-tools.lisp"
   :availability-owner "bounded-work-tools.lisp"
   :observer-stage-ids
   (:timing :tool-call-event :modulator-appraisal :tool-result-event))
  (:name "read-deliverable" :handler-id :read-deliverable
   :handler-owner "bounded-work-tools.lisp"
   :schema-owner "bounded-work-tools.lisp"
   :availability-owner "bounded-work-tools.lisp"
   :observer-stage-ids
   (:timing :tool-call-event :modulator-appraisal :tool-result-event))
  (:name "search-memory" :handler-id :search-memory
   :handler-owner "memory-search-tool.lisp"
   :schema-owner "memory-search-tool.lisp"
   :availability-owner "memory-search-tool.lisp"
   :observer-stage-ids
   (:timing :tool-call-event :modulator-appraisal :tool-result-event))
  (:name "hold-near-term-thought" :handler-id :hold-near-term-thought
   :handler-owner "near-term-intention-tool.lisp"
   :schema-owner "near-term-intention-tool.lisp"
   :availability-owner "near-term-intention-tool.lisp"
   :observer-stage-ids
   (:timing :tool-call-event :modulator-appraisal :tool-result-event)))
 :execute-audit-files
 ("self-mod.lisp" "enhancements.lisp" "modulator.lisp"
  "web.lisp" "runware.lisp" "bounded-work-tools.lisp"
  "event-log.lisp" "self-mod-provenance.lisp" "memory-search-tool.lisp"
  "observability-tracing.lisp" "near-term-intention-tool.lisp")
 :effective-behavior :none
 :production-loaded nil)
