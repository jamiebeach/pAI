;;;; event-type-census.sexp -- the authoritative event-type classification.
;;;;
;;;; ONE SOURCE. src/mind/conscious/census.lisp builds the admission table
;;;; from this file, and docs/event-type-census.md is generated from it by
;;;; scripts/generate-census-doc.js. Nothing is hand-maintained twice.
;;;;
;;;; The previous arrangement had a markdown document and a Lisp table written
;;;; separately, plus a fixture that compared a Lisp constant to a Lisp hash
;;;; table while being described as comparing document to code. It was not
;;;; checking what it claimed, and the two had already drifted.
;;;;
;;;; FORMAT
;;;;
;;;;   (:type "event-type"
;;;;    :class :stimulus | :journal
;;;;    :kind "spec-9.1-kind"          ; :stimulus only
;;;;    :source "channel|scheduler|tool|memory|system|internal"
;;;;    :urgency "interactive|timely|background"
;;;;    :barrier t | nil
;;;;    :discriminator :schedule-mode | :operation-status | nil
;;;;    :group "grouping for the generated doc"   ; :journal only
;;;;    :reason "why it wakes the agent, or why it does not")
;;;;
;;;; Every entry needs a reason. The exclusions are the part worth reviewing:
;;;; each is a claim that the agent is not missing something.

(:census-version 20
 :spec-kinds ("user-message" "channel-state" "schedule-due" "timer"
              "tool-result" "tool-failure" "model-result" "model-failure"
              "memory-result" "intention-cue" "project-change"
              "prediction-due" "runtime-health" "cancellation"
              "operator-control" "self-mod-result" "environment-change")
 :entries
 (
  (:type "peer-message-received" :class :stimulus :kind "environment-change"
   :source "system" :urgency "background" :barrier t
   :reason "authenticated local peer delivery is itself a retained private stimulus without inheriting operator authority; historical linked generic roots remain sole owners where present")
  (:type "agent-stimulus-received" :class :stimulus :kind "environment-change"
   :source "system" :urgency "background" :barrier t
   :reason "retained adapter experience is eligible for private agent execution without inheriting operator authority")
  (:type "recursive-stimulus-result" :class :journal :group "cognition"
   :reason "durable private completion of an admitted stimulus root, not another stimulus")
  (:type "recursive-activity-opened" :class :journal :group "cognition"
   :reason "frozen bounded source membership for one private activity, not a new stimulus")
  (:type "recursive-stimulus-disposition" :class :journal :group "cognition"
   :reason "durable terminal interpretation of a completed retained stimulus, not a new stimulus")
  (:type "recursive-private-opportunity-selected" :class :journal :group "cognition"
   :reason "durable fairness choice between generic stimuli and ordinary private work; selection is not a new stimulus")
  (:type "peer-board-notification-queued" :class :journal :group "fleet"
   :reason "durable sender outbox entry for an authenticated board reply, not a new stimulus")
  (:type "peer-board-notification-delivered" :class :journal :group "fleet"
   :reason "transport receipt for one queued board notification, not new cognition")
  (:type "peer-board-publication-intent" :class :journal :group "fleet"
   :reason "frozen outbound board request and operation identity before transmission; recoverable effect evidence, not a stimulus")
  ;; ---------------------------------------------------------------- admitted
  (:type "user-message" :class :stimulus :kind "user-message"
   :source "channel" :urgency "interactive" :barrier t
   :reason "a person is waiting")

  (:type "turn-cancel-requested" :class :stimulus :kind "cancellation"
   :source "system" :urgency "interactive" :barrier t
   :reason "a person is retracting; acting on the retracted turn is worse than late")

  (:type "stabilization-mode-changed" :class :stimulus :kind "operator-control"
   :source "system" :urgency "timely" :barrier t
   :reason "the operating envelope changed under the agent")

  (:type "schedule-fired" :class :stimulus :kind "schedule-due"
   :source "scheduler" :urgency "timely" :barrier t
   :discriminator :schedule-mode
   :reason "a commitment came due; mode discriminates delivered from pending")

  (:type "agent-operation-terminal" :class :stimulus :kind "tool-result"
   :source "tool" :urgency "timely" :barrier t
   :discriminator :operation-status
   :reason "work the agent started finished; dropping it leaves the operation awaited forever")

  (:type "tool-result" :class :stimulus :kind "tool-result"
   :source "tool" :urgency "timely" :barrier t
   :reason "legacy direct tool completion")

  (:type "near-term-intention-created" :class :stimulus :kind "intention-cue"
   :source "internal" :urgency "timely" :barrier t
   :reason "a commitment was made and now needs tracking")

  (:type "near-term-intention-transition" :class :stimulus :kind "intention-cue"
   :source "internal" :urgency "timely" :barrier t
   :reason "a promise changed state; a dropped transition leaves it mistracked")

  (:type "conscious-curiosity-candidate-raised" :class :stimulus
   :kind "intention-cue" :source "internal" :urgency "background" :barrier nil
   :discriminator :motivation-candidate
   :reason "one coalesced motive revision may compete for private consideration without granting action authority")

  (:type "recursive-curiosity-focus-opened" :class :stimulus
   :kind "intention-cue" :source "internal" :urgency "background" :barrier nil
   :discriminator :motivation-candidate
   :reason "the private reviewer chose one evidence-backed question for bounded recursive investigation without granting publication authority")

  (:type "heap-pressure" :class :stimulus :kind "runtime-health"
   :source "system" :urgency "timely" :barrier t
   :reason "resource exhaustion is an anomaly the agent should notice about itself")

  (:type "modulator-watchdog-triggered" :class :stimulus :kind "runtime-health"
   :source "system" :urgency "timely" :barrier nil
   :reason "a regulatory loop stalled")

  (:type "runtime-observer-error" :class :stimulus :kind "runtime-health"
   :source "system" :urgency "background" :barrier nil
   :reason "an observer failed; the agent's self-monitoring is degraded")

  ;; Admitted on review challenge. The earlier exclusion argued it would wake
  ;; the agent mid-write, which was factually wrong -- %MEMORY-AFTER-WRITE is
  ;; documented as non-transactional and runs after the node commits. The
  ;; structural argument is stronger anyway: a codelet that can only read
  ;; contradiction state when some unrelated stimulus already triggered
  ;; assessment can never wake FOR a contradiction, which is the dead-codelet
  ;; problem this census fixed for runtime health.
  (:type "contradiction-detected" :class :stimulus :kind "project-change"
   :source "memory" :urgency "background" :barrier nil
   :reason "an unresolved contradiction must be able to wake its own codelet")

  (:type "prediction-resolved" :class :stimulus :kind "prediction-due"
   :source "internal" :urgency "background" :barrier nil
   :reason "a prediction came due; the discrepancy is the signal")

  (:type "episode-boundary-detected" :class :stimulus :kind "project-change"
   :source "internal" :urgency "background" :barrier nil
   :reason "a coherent episode closed")

  (:type "self-model-revised" :class :stimulus :kind "project-change"
   :source "internal" :urgency "background" :barrier nil
   :reason "the agent's model of itself changed")

  (:type "self-mod-accepted" :class :stimulus :kind "self-mod-result"
   :source "internal" :urgency "timely" :barrier t
   :reason "its own implementation changed")

  (:type "self-mod-rejected" :class :stimulus :kind "self-mod-result"
   :source "internal" :urgency "background" :barrier nil
   :reason "a proposal was refused; the reason is evidence")

  (:type "self-mod-rolled-back" :class :stimulus :kind "self-mod-result"
   :source "internal" :urgency "timely" :barrier t
   :reason "a change was reverted, possibly under it")

  (:type "self-mod-auto-rollback" :class :stimulus :kind "self-mod-result"
   :source "internal" :urgency "timely" :barrier t
   :reason "reverted automatically; the agent did not decide this")

  ;; ------------------------------------------------------------ journal-only

  ;; Reclassified on review challenge. Q0 classified scheduler notification as
  ;; an already-authorized EFFECT, not a cognitive producer. Admitting the
  ;; delivered variant as `schedule-due` kept it eligible to trigger another
  ;; pulse and blocked the consumption watermark until acknowledged. Awareness
  ;; of what was published belongs in a publication projection, not in wake
  ;; candidacy. Handled by the :schedule-mode discriminator, which classifies
  ;; the notify variant as journal.
  (:type "schedule-fired/notify" :class :journal :group "effects already taken"
   :reason "an already-delivered notification is a published effect, not a reason to think again")

  (:type "timing-trace" :class :journal :group "observability and infrastructure"
   :reason "no cognitive content")
  (:type "pg-backup" :class :journal :group "observability and infrastructure"
   :reason "no cognitive content")
  (:type "heap-health" :class :journal :group "observability and infrastructure"
   :reason "routine sample; the heap-pressure variant is admitted")

  (:type "agent-message" :class :journal :group "turn mechanics"
   :reason "the agent's own output; waking for it would be circular")
  (:type "conversation-episode-seal-opened" :class :journal
   :group "turn mechanics"
   :reason "durable provider-bound episode work is already owned by the quiet recursive step")
  (:type "conversation-episode-sealed" :class :journal
   :group "turn mechanics"
   :reason "a completed derived episode becomes recall evidence but must not recursively wake itself")
  (:type "conversation-episode-seal-failed" :class :journal
   :group "turn mechanics"
   :reason "the quiet recursive owner reports failure without a second scheduler wake")
  (:type "tool-call" :class :journal :group "turn mechanics"
   :reason "the agent issued it; only the result carries new information")
  (:type "conversation-context-budget" :class :journal :group "turn mechanics"
   :reason "turn bookkeeping")
  (:type "conversation-context-config-changed" :class :journal :group "turn mechanics"
   :reason "turn bookkeeping")
  (:type "context-curator-consumed" :class :journal :group "turn mechanics"
   :reason "turn bookkeeping")
  (:type "context-curator-fallback" :class :journal :group "turn mechanics"
   :reason "turn bookkeeping")
  (:type "conversation-episode-written" :class :journal :group "turn mechanics"
   :reason "turn bookkeeping")

  (:type "tick-start" :class :journal :group "tick internals"
   :reason "the tick loop is a legacy cognition owner being dismantled")
  (:type "tick-end" :class :journal :group "tick internals"
   :reason "the tick loop is a legacy cognition owner being dismantled")
  (:type "tick-note" :class :journal :group "tick internals"
   :reason "the tick loop is a legacy cognition owner being dismantled")
  (:type "tick-maintenance" :class :journal :group "tick internals"
   :reason "the tick loop is a legacy cognition owner being dismantled")
  (:type "tick-budget-limit" :class :journal :group "tick internals"
   :reason "the tick loop is a legacy cognition owner being dismantled")
  (:type "tick-continuity-fact" :class :journal :group "tick internals"
   :reason "the tick loop is a legacy cognition owner being dismantled")
  (:type "tick-initiative-proposed" :class :journal :group "tick internals"
   :reason "the tick loop is a legacy cognition owner being dismantled")
  (:type "reflection-pass" :class :journal :group "tick internals"
   :reason "the tick loop is a legacy cognition owner being dismantled")

  (:type "memory-write" :class :journal :group "memory bookkeeping"
   :reason "a consequence of cognition; admitting it creates a think-write-think loop with no stop")
  (:type "memory-decay" :class :journal :group "memory bookkeeping"
   :reason "maintenance, not a reason to think")
  (:type "memory-importance-scored" :class :journal :group "memory bookkeeping"
   :reason "maintenance, not a reason to think")
  (:type "memory-use-recorded" :class :journal :group "memory bookkeeping"
   :reason "maintenance, not a reason to think")
  (:type "memory-superseded" :class :journal :group "memory bookkeeping"
   :reason "maintenance, not a reason to think")
  (:type "memory-quarantined" :class :journal :group "memory bookkeeping"
   :reason "maintenance, not a reason to think")
  (:type "memory-quarantine-changed" :class :journal :group "memory bookkeeping"
   :reason "maintenance, not a reason to think")
  (:type "legacy-memory-recall-used" :class :journal :group "memory bookkeeping"
   :reason "maintenance, not a reason to think")
  (:type "memory-operation-state" :class :journal :group "memory authority"
   :reason "authoritative projection input; cognition already caused it and replay must not self-wake")
  (:type "memory-baseline-started" :class :journal :group "memory authority"
   :reason "migration framing for rebuild, not a new stimulus")
  (:type "memory-baseline-node" :class :journal :group "memory authority"
   :reason "bounded private baseline state for replay, not a new stimulus")
  (:type "memory-baseline-edge" :class :journal :group "memory authority"
   :reason "bounded private baseline state for replay, not a new stimulus")
  (:type "memory-baseline-committed" :class :journal :group "memory authority"
   :reason "hash-closed migration evidence, not a new stimulus")
  (:type "epistemic-admission-accepted" :class :journal :group "memory bookkeeping"
   :reason "admission bookkeeping")
  (:type "epistemic-admission-rejected" :class :journal :group "memory bookkeeping"
   :reason "admission bookkeeping")
  (:type "epistemic-admission-shadow" :class :journal :group "memory bookkeeping"
   :reason "admission bookkeeping")

  (:type "initiative-candidate-scored" :class :journal :group "deliberation internals"
   :reason "an intermediate step of a decision already being made")
  (:type "initiative-decision" :class :journal :group "deliberation internals"
   :reason "an intermediate step of a decision already being made")
  (:type "candidate-nominated" :class :journal :group "deliberation internals"
   :reason "an intermediate step of a decision already being made")
  (:type "candidate-coincidence" :class :journal :group "deliberation internals"
   :reason "an intermediate step of a decision already being made")
  (:type "candidate-promoted" :class :journal :group "deliberation internals"
   :reason "an intermediate step of a decision already being made")
  (:type "candidate-aged-out" :class :journal :group "deliberation internals"
   :reason "an intermediate step of a decision already being made")
  (:type "explore-novelty-deferred" :class :journal :group "deliberation internals"
   :reason "an intermediate step of a decision already being made")
  (:type "explore-root-deferred" :class :journal :group "deliberation internals"
   :reason "an intermediate step of a decision already being made")
  (:type "explore-stance-committed" :class :journal :group "deliberation internals"
   :reason "an intermediate step of a decision already being made")
  (:type "explore-stance-deferred" :class :journal :group "deliberation internals"
   :reason "an intermediate step of a decision already being made")
  (:type "attention-schema-update" :class :journal :group "deliberation internals"
   :reason "an intermediate step of a decision already being made")
  (:type "attention-schema-divergence" :class :journal :group "deliberation internals"
   :reason "an intermediate step of a decision already being made")
  (:type "self-model-question-status" :class :journal :group "deliberation internals"
   :reason "an intermediate step of a decision already being made")
  (:type "self-model-question-migration" :class :journal :group "deliberation internals"
   :reason "an intermediate step of a decision already being made")
  (:type "reflection-no-novelty" :class :journal :group "deliberation internals"
   :reason "an intermediate step of a decision already being made")
  (:type "drive-near-threshold" :class :journal :group "deliberation internals"
   :reason "an intermediate step of a decision already being made")
  (:type "prediction-written" :class :journal :group "deliberation internals"
   :reason "the resolution is the signal, not the writing")
  (:type "soul-entry-added" :class :journal :group "deliberation internals"
   :reason "an outcome already decided")
  (:type "episode-flushed" :class :journal :group "deliberation internals"
   :reason "an outcome already decided")
  (:type "episode-replay" :class :journal :group "deliberation internals"
   :reason "an outcome already decided")
  (:type "reciprocity-canary-transition" :class :journal :group "deliberation internals"
   :reason "an outcome already decided")
  (:type "public-system-prompt-changed" :class :journal :group "deliberation internals"
   :reason "an outcome already decided")
  (:type "user-timezone-changed" :class :journal :group "deliberation internals"
   :reason "configuration, applied where it is read")
  (:type "scheduled-context-consumed" :class :journal :group "deliberation internals"
   :reason "acknowledgement bookkeeping")

  (:type "agent-operation-claimed" :class :journal :group "operation sub-lifecycle"
   :reason "only the terminal is admitted; waking on each state would make one operation produce a dozen wakes")
  (:type "agent-operation-lease" :class :journal :group "operation sub-lifecycle"
   :reason "only the terminal is admitted")
  (:type "agent-process-transition" :class :journal :group "operation sub-lifecycle"
   :reason "only the terminal is admitted")
  (:type "operation-claimed" :class :journal :group "operation sub-lifecycle"
   :reason "only the terminal is admitted")
  (:type "operation-budget" :class :journal :group "operation sub-lifecycle"
   :reason "only the terminal is admitted")
  (:type "operation-class" :class :journal :group "operation sub-lifecycle"
   :reason "only the terminal is admitted")
  (:type "operation-cooldown" :class :journal :group "operation sub-lifecycle"
   :reason "only the terminal is admitted")
  (:type "operation-in-flight" :class :journal :group "operation sub-lifecycle"
   :reason "only the terminal is admitted")
  (:type "operation-not-claimable" :class :journal :group "operation sub-lifecycle"
   :reason "only the terminal is admitted")
  (:type "grounded-project-proposal-created" :class :journal :group "operation sub-lifecycle"
   :reason "only the terminal is admitted")
  (:type "grounded-project-proposal-reviewed" :class :journal :group "operation sub-lifecycle"
   :reason "only the terminal is admitted")
  (:type "publication-candidate-withheld" :class :journal :group "operation sub-lifecycle"
   :reason "only the terminal is admitted")

  (:type "self-mod-proposed" :class :journal :group "self-modification internals"
   :reason "a pipeline step; terminal outcomes are admitted")
  (:type "self-mod-provenance-recorded" :class :journal :group "self-modification internals"
   :reason "a pipeline step; terminal outcomes are admitted")
  (:type "self-mod-outcome-verdict" :class :journal :group "self-modification internals"
   :reason "a pipeline step; terminal outcomes are admitted")

  (:type "stimulus-consumed" :class :journal :group "consumption bookkeeping"
   :reason "written by this runtime; admitting it would make consuming a stimulus produce a stimulus")
  (:type "pulse-opened" :class :journal :group "conscious pulse lifecycle"
   :reason "attempt boundary; recovery input, never a new attention candidate")
  (:type "pulse-committed" :class :journal :group "conscious pulse lifecycle"
   :reason "terminal commit and consumption acknowledgement; admitting it would self-wake")
  (:type "pulse-cancelled" :class :journal :group "conscious pulse lifecycle"
   :reason "terminal cancellation record; its original trigger remains candidacy")
  (:type "pulse-failed" :class :journal :group "conscious pulse lifecycle"
   :reason "terminal failure record; runtime health reporting owns escalation")
  (:type "pulse-recovered" :class :journal :group "conscious pulse lifecycle"
   :reason "recovery disposition for an orphaned open pulse")
  (:type "concern-presented" :class :journal :group "conscious pulse lifecycle"
   :reason "event-derived concern history consumed by the conscious projection")
  (:type "concern-deferred" :class :journal :group "conscious pulse lifecycle"
   :reason "event-derived concern history consumed by the conscious projection")
  (:type "conscious-lifecycle-transition" :class :journal :group "conscious work lifecycle"
   :reason "Q5 work coordination state; admitting bookkeeping would make a lifecycle transition wake on itself")
  (:type "conscious-lifecycle-result-rejected" :class :journal :group "conscious work lifecycle"
   :reason "Q5 stale/invalid result disposition; the original result event remains the attention stimulus")
  (:type "conscious-lifecycle-source-rejected" :class :journal :group "conscious work lifecycle"
   :reason "Q5 durable disposition of an illegal producer transition; the original producer event remains the attention stimulus")
  (:type "conscious-lifecycle-command-requested" :class :journal :group "conscious work lifecycle"
   :reason "content-free operator command receipt; the producer creation event carries the attention transition")
  (:type "conscious-lifecycle-semantic-described" :class :journal :group "conscious work lifecycle"
   :reason "Q5S semantic descriptor source; lifecycle truth remains the attention stimulus")
  (:type "conscious-curiosity-observed" :class :journal :group "motivational dynamics"
   :reason "Q5M evidence; one coalesced motive candidate, not each recurrence, owns attention")
  (:type "recursive-curiosity-origin-context-recorded" :class :journal :group "motivational dynamics"
   :reason "runtime-owned exact origin evidence for one curiosity observation; later projections may use it without waking attention independently")
  (:type "recursive-curiosity-follow-up-requested" :class :journal :group "motivational dynamics"
   :reason "operator-authorized one-shot result-delivery commitment attached to an exact open motive")
  (:type "recursive-curiosity-follow-up-completed" :class :journal :group "motivational dynamics"
   :reason "idempotent delivery receipt fulfilling one requested curiosity follow-up")
  (:type "conscious-curiosity-opportunity-observed" :class :journal :group "motivational dynamics"
   :reason "Q5M opportunity evidence; a later content-free candidate event owns admission")
  (:type "conscious-curiosity-satisfaction-observed" :class :journal :group "motivational dynamics"
   :reason "Q5M satisfaction evidence; suppressing a motive must not wake it again")
  (:type "recursive-curiosity-result" :class :journal :group "motivational dynamics"
   :reason "private terminal finding; inspection owns it and admitting it would make completed curiosity wake itself")
  (:type "recursive-curiosity-focus-failed" :class :journal :group "motivational dynamics"
   :reason "terminal durable failure for one private focus attempt; it preserves provider or protocol-quarantine evidence and prevents one unrecoverable root from blocking later attention")
  (:type "recursive-root-failed" :class :stimulus :kind "runtime-health"
   :source "system" :urgency "background" :barrier nil
   :group "recursive cognition"
   :reason "terminal pre-provider context failure for one admitted recursive root; it closes presentation, preserves replay evidence and lets the continuing mind notice its own runtime anomaly")
  (:type "recursive-curiosity-review-opened" :class :journal :group "motivational dynamics"
   :reason "sealed private review range; background bookkeeping must not wake attention on itself")
  (:type "recursive-curiosity-review-completed" :class :journal :group "motivational dynamics"
   :reason "private review watermark and receipt; observations separately carry motivational evidence")
  (:type "recursive-curiosity-attention-opened" :class :journal :group "motivational dynamics"
   :reason "sealed open-curiosity generation for one private recursive attention decision")
  (:type "recursive-curiosity-attention-declined" :class :journal :group "motivational dynamics"
   :reason "revision-bound durable decline; the same unchanged register must not repeatedly consume inference")
  (:type "recursive-curiosity-attention-completed" :class :journal :group "motivational dynamics"
   :reason "content-free receipt linking one sealed register generation to its chosen private focus")
  (:type "recursive-curiosity-attention-quiescent" :class :journal :group "motivational dynamics"
   :reason "one content-free receipt that every bounded page of an unchanged open-curiosity generation has settled; quiet wakes remain silent until evidence changes")
  (:type "recursive-curiosity-consolidation-opened" :class :journal :group "motivational dynamics"
   :reason "sealed bounded open-motive generation awaiting one non-destructive semantic presentation frame")
  (:type "recursive-curiosity-consolidation-completed" :class :journal :group "motivational dynamics"
   :reason "validated presentation-only motive partition consumed by attention and briefing; it cannot mutate motive authority")
  (:type "recursive-curiosity-consolidation-failed" :class :stimulus :kind "runtime-health"
   :source "system" :urgency "background" :barrier nil
   :group "motivational dynamics"
   :reason "terminal anomaly for one exact consolidation revision; raw-register fallback preserves liveness while the continuing mind may notice the fault")
  (:type "recursive-curiosity-result-review-opened" :class :journal :group "motivational dynamics"
   :reason "sealed private result review; lifecycle disposition remains a separate deterministic commit")
  (:type "recursive-curiosity-result-review-completed" :class :journal :group "motivational dynamics"
   :reason "content-free close/refine/sustain receipt for one completed private investigation")
  (:type "recursive-curiosity-incorporation-opened" :class :journal :group "motivational dynamics"
   :reason "sealed private retention judgment for one reviewed curiosity result")
  (:type "recursive-curiosity-incorporation-completed" :class :journal :group "motivational dynamics"
   :reason "durable retained-or-declined receipt linking one result to memory and optional autonomous publication evidence")
  (:type "recursive-curiosity-briefing-opened" :class :journal :group "motivational dynamics"
   :reason "sealed bounded private-cognition rows awaiting one compact briefing; it is bookkeeping inside an already-running quiet root")
  (:type "recursive-curiosity-briefing-completed" :class :journal :group "motivational dynamics"
   :reason "private model-generated compression consumed explicitly by ordinary context; it must not wake attention on itself")
  (:type "recursive-curiosity-briefing-failed" :class :stimulus :kind "runtime-health"
   :source "system" :urgency "background" :barrier nil
   :group "motivational dynamics"
   :reason "terminal anomaly for one exact briefing revision; admitting the failure allows the continuing mind to notice its own private-cognition machinery without retrying the provider boundary")

  ;; ---------------------------------------------------------------------
  ;; Added by scripts/census-coverage.js, which compares this manifest
  ;; against the event types producers actually write. All eight were being
  ;; written and classified nowhere -- so each defaulted to journal-only
  ;; without anyone deciding it should. The default was right in seven cases
  ;; and wrong in one, which is the argument for the check.
  ;; ---------------------------------------------------------------------

  ;; The one the default got wrong. A failure in the agency loop is the agent
  ;; failing at its own autonomous work -- the same class of fact as
  ;; runtime-observer-error, which is admitted. Leaving it journal-only meant
  ;; the anomaly codelet could not see the agent's own machinery breaking.
  (:type "grounded-agency-tick-error" :class :stimulus :kind "runtime-health"
   :source "system" :urgency "background" :barrier nil
   :reason "the agent's autonomous work loop failed; its own machinery breaking must be noticeable")

  (:type "agent-appraisal-derived" :class :journal :group "deliberation internals"
   :reason "an intermediate appraisal step; the outcome it feeds is what matters")
  (:type "artifact-completed" :class :journal :group "operation sub-lifecycle"
   :reason "agent-operation-terminal already carries artifact identity and is admitted; admitting both would wake twice for one completion")
  (:type "artifact-validated" :class :journal :group "operation sub-lifecycle"
   :reason "a validation step inside an operation; the terminal reports the verdict")
  (:type "publication-candidate-validated" :class :journal :group "operation sub-lifecycle"
   :reason "a publication pipeline step; its withheld counterpart is likewise journal")
  (:type "conversation-history-transform" :class :journal :group "turn mechanics"
   :reason "turn bookkeeping")
  (:type "projection-state" :class :journal :group "observability and infrastructure"
   :reason "projection rebuild bookkeeping; a projection reporting on itself is not a reason to think")
  (:type "postgres-row-state" :class :journal :group "observability and infrastructure"
   :reason "storage-level record; no cognitive content")
  ;; ---------------------------------------------------------------------
  ;; Second coverage pass. Generalising the scanner to find helper-indirected
  ;; producers (%tick-proposal-log, %legacy-model-log, %turn-capture-log and
  ;; friends) surfaced 25 more written-but-unclassified types. Two of them
  ;; wanted admitting, which is again the argument for the check: the silent
  ;; default was wrong about the agent noticing its own failures.
  ;; ---------------------------------------------------------------------

  ;; The agent's own worker died. Same class of fact as runtime-observer-error.
  (:type "grounded-agency-worker-error" :class :stimulus :kind "runtime-health"
   :source "system" :urgency "background" :barrier nil
   :reason "the autonomous worker itself failed; the agent should notice its own machinery stopping")

  ;; A turn was not recorded. Losing the record of an exchange silently is a
  ;; memory-integrity failure, and the agent is the party that will later be
  ;; wrong about what happened.
  (:type "turn-capture-persistence-error" :class :stimulus :kind "runtime-health"
   :source "system" :urgency "timely" :barrier nil
   :reason "a turn failed to persist; the agent will later be wrong about its own history and should know")

  (:type "grounded-agency-after-operation-error" :class :journal :group "operation sub-lifecycle"
   :reason "a post-operation hook failed while the operation itself completed; the terminal carries the outcome")
  (:type "grounded-agency-idle-hook-error" :class :journal :group "operation sub-lifecycle"
   :reason "an idle reconciliation hook failed; no work was in flight to misreport")
  (:type "turn-capture-completion-hook-error" :class :journal :group "turn mechanics"
   :reason "a completion hook failed; the turn itself was captured")

  (:type "cognitive-call-start" :class :journal :group "deliberation internals"
   :reason "cognition instrumentation; the call's outcome is what matters")
  (:type "cognitive-call-end" :class :journal :group "deliberation internals"
   :reason "cognition instrumentation; the call's outcome is what matters")
  (:type "cognitive-artifact-call-start" :class :journal :group "deliberation internals"
   :reason "cognition instrumentation")
  (:type "cognitive-artifact-call-end" :class :journal :group "deliberation internals"
   :reason "cognition instrumentation")
  (:type "model-request" :class :journal :group "observability and infrastructure"
   :reason "model IO instrumentation; waking for the agent's own request would be circular")
  (:type "model-response" :class :journal :group "observability and infrastructure"
   :reason "model IO receipt or accepted private outcome; its owning thread consumes it without creating a new stimulus")

  (:type "conscious-interaction-claimed" :class :journal :group "turn mechanics"
   :reason "durable serialization claim; the admitted user-message already carries the stimulus")
  (:type "conscious-interaction-completed" :class :journal :group "turn mechanics"
   :reason "terminal interaction bookkeeping; any public agent-message is the conversational fact")
  (:type "conscious-interaction-failed" :class :journal :group "turn mechanics"
   :reason "terminal interaction bookkeeping exposed to the requesting channel, not a new thought trigger")
  (:type "conscious-interaction-outcome-unknown" :class :journal :group "turn mechanics"
   :reason "crash recovery stop marker preventing blind provider re-execution; operator retry is explicit")
  (:type "conscious-tool-operation-claimed" :class :journal :group "turn mechanics"
   :reason "durable execution claim; only the bounded result may enter a continuation")
  (:type "conscious-tool-operation-result" :class :journal :group "turn mechanics"
   :reason "private result consumed explicitly by the active interaction continuation, not a new attention trigger")
  (:type "conscious-tool-operation-failed" :class :journal :group "turn mechanics"
   :reason "closed terminal bookkeeping for the active interaction; it cannot authorize another execution")

  (:type "conscious-work-opened" :class :journal :group "cognitive work lifecycle"
   :reason "content-free work identity derived from an already-admitted stimulus")
  (:type "conscious-work-suspended" :class :journal :group "cognitive work lifecycle"
   :reason "explicit scheduling disposition; suspension must not wake itself")
  (:type "conscious-work-resumed" :class :journal :group "cognitive work lifecycle"
   :reason "explicit scheduling disposition; the original concern remains the attention root")
  (:type "conscious-work-completed" :class :journal :group "cognitive work lifecycle"
   :reason "terminal work bookkeeping; stimulus consumption is recorded separately")
  (:type "conscious-work-failed" :class :journal :group "cognitive work lifecycle"
   :reason "closed work failure disposition exposed through operational reporting")
  (:type "conscious-work-outcome-unknown" :class :journal :group "cognitive work lifecycle"
   :reason "uncertain provider or effect boundary; never an instruction to retry")

  (:type "latent-seeded" :class :journal :group "deliberation internals"
   :reason "latent-thought lifecycle; an intermediate step of reflection already under way")
  (:type "latent-transition" :class :journal :group "deliberation internals"
   :reason "latent-thought lifecycle")
  (:type "latent-thought-incubated" :class :journal :group "deliberation internals"
   :reason "latent-thought lifecycle")
  (:type "latent-thought-merged" :class :journal :group "deliberation internals"
   :reason "latent-thought lifecycle")
  (:type "latent-thought-ready" :class :journal :group "deliberation internals"
   :reason "latent-thought lifecycle; readiness is consumed by the tick machinery being dismantled")
  (:type "latent-thought-expired" :class :journal :group "deliberation internals"
   :reason "latent-thought lifecycle")

  (:type "external-signal" :class :journal :group "tick internals"
   :reason "a search result recorded inside tick proposal machinery; the tick loop is a legacy cognition owner being dismantled")
  (:type "tick-proposal-duplicate-merged" :class :journal :group "tick internals"
   :reason "tick proposal bookkeeping")
  (:type "tick-terminal" :class :journal :group "tick internals"
   :reason "tick lifecycle bookkeeping")

  (:type "conscious-runtime-plan-registered" :class :state :group "runtime composition"
   :reason "content-addressed non-secret plan required to replay open work")
  (:type "tool-turn-committed" :class :journal :group "turn mechanics"
   :reason "turn bookkeeping; the agent committed its own turn")
  (:type "turn-capture-complete" :class :journal :group "turn mechanics"
   :reason "turn bookkeeping")
  (:type "turn-capture-ready" :class :journal :group "turn mechanics"
   :reason "turn bookkeeping")

  (:type "pull-reciprocity-reply" :class :journal :group "effects already taken"
   :reason "a reply already delivered; awareness of what was published belongs in a publication projection")
  (:type "reciprocity-typed-label-recorded" :class :journal :group "deliberation internals"
   :reason "an outcome already decided")))
