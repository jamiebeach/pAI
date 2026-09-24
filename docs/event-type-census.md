# Event-type census

**GENERATED — do not edit.**

Source: `src/mind/conscious/event-type-census.sexp`.
Regenerate: `node scripts/generate-census-doc.js`.
Verify: `node scripts/generate-census-doc.js --check`.

Census version 20. 192 event types classified: 29 admitted as stimuli, 162 journal-only.

The admission table in `src/mind/conscious/policy.lisp` is built from
the same manifest at load time, so this document and the running
policy cannot disagree — there is nothing left to disagree.

## The classification rule

An event is a **stimulus** if a reasonable agent could need to *wake*
for it, or if ignoring it would leave something permanently unresolved.
Everything else is **journal-only**: written so the past is
reconstructible, not because anything should act on it.

The default is exclusion. A type absent from the manifest is never
admitted, so adding a stimulus is a deliberate decision — the reverse
default would flood attention with backup notifications.

## Admitted

| event type | kind | urgency | barrier | why it can wake the agent |
| --- | --- | --- | --- | --- |
| `peer-message-received` | `environment-change` | background | yes | authenticated local peer delivery is itself a retained private stimulus without inheriting operator authority; historical linked generic roots remain sole owners where present |
| `agent-stimulus-received` | `environment-change` | background | yes | retained adapter experience is eligible for private agent execution without inheriting operator authority |
| `user-message` | `user-message` | interactive | yes | a person is waiting |
| `turn-cancel-requested` | `cancellation` | interactive | yes | a person is retracting; acting on the retracted turn is worse than late |
| `stabilization-mode-changed` | `operator-control` | timely | yes | the operating envelope changed under the agent |
| `schedule-fired` | `schedule-due` | timely | yes | a commitment came due; mode discriminates delivered from pending |
| `agent-operation-terminal` | `tool-result` | timely | yes | work the agent started finished; dropping it leaves the operation awaited forever |
| `tool-result` | `tool-result` | timely | yes | legacy direct tool completion |
| `near-term-intention-created` | `intention-cue` | timely | yes | a commitment was made and now needs tracking |
| `near-term-intention-transition` | `intention-cue` | timely | yes | a promise changed state; a dropped transition leaves it mistracked |
| `conscious-curiosity-candidate-raised` | `intention-cue` | background | no | one coalesced motive revision may compete for private consideration without granting action authority |
| `recursive-curiosity-focus-opened` | `intention-cue` | background | no | the private reviewer chose one evidence-backed question for bounded recursive investigation without granting publication authority |
| `heap-pressure` | `runtime-health` | timely | yes | resource exhaustion is an anomaly the agent should notice about itself |
| `modulator-watchdog-triggered` | `runtime-health` | timely | no | a regulatory loop stalled |
| `runtime-observer-error` | `runtime-health` | background | no | an observer failed; the agent's self-monitoring is degraded |
| `contradiction-detected` | `project-change` | background | no | an unresolved contradiction must be able to wake its own codelet |
| `prediction-resolved` | `prediction-due` | background | no | a prediction came due; the discrepancy is the signal |
| `episode-boundary-detected` | `project-change` | background | no | a coherent episode closed |
| `self-model-revised` | `project-change` | background | no | the agent's model of itself changed |
| `self-mod-accepted` | `self-mod-result` | timely | yes | its own implementation changed |
| `self-mod-rejected` | `self-mod-result` | background | no | a proposal was refused; the reason is evidence |
| `self-mod-rolled-back` | `self-mod-result` | timely | yes | a change was reverted, possibly under it |
| `self-mod-auto-rollback` | `self-mod-result` | timely | yes | reverted automatically; the agent did not decide this |
| `recursive-root-failed` | `runtime-health` | background | no | terminal pre-provider context failure for one admitted recursive root; it closes presentation, preserves replay evidence and lets the continuing mind notice its own runtime anomaly |
| `recursive-curiosity-consolidation-failed` | `runtime-health` | background | no | terminal anomaly for one exact consolidation revision; raw-register fallback preserves liveness while the continuing mind may notice the fault |
| `recursive-curiosity-briefing-failed` | `runtime-health` | background | no | terminal anomaly for one exact briefing revision; admitting the failure allows the continuing mind to notice its own private-cognition machinery without retrying the provider boundary |
| `grounded-agency-tick-error` | `runtime-health` | background | no | the agent's autonomous work loop failed; its own machinery breaking must be noticeable |
| `grounded-agency-worker-error` | `runtime-health` | background | no | the autonomous worker itself failed; the agent should notice its own machinery stopping |
| `turn-capture-persistence-error` | `runtime-health` | timely | no | a turn failed to persist; the agent will later be wrong about its own history and should know |

### Payload-discriminated

These map to more than one classification depending on payload. A
discriminator may also reclassify a particular payload as
journal-only — a delivered scheduler notification being the case
that forced it.

| event type | discriminator |
| --- | --- |
| `schedule-fired` | `:schedule-mode` |
| `agent-operation-terminal` | `:operation-status` |
| `conscious-curiosity-candidate-raised` | `:motivation-candidate` |
| `recursive-curiosity-focus-opened` | `:motivation-candidate` |

## Journal-only, with reasons

The exclusions are the part worth reviewing: each is a claim that the
agent is not missing something.

**cognition**

- `recursive-stimulus-result` — durable private completion of an admitted stimulus root, not another stimulus
- `recursive-activity-opened` — frozen bounded source membership for one private activity, not a new stimulus
- `recursive-stimulus-disposition` — durable terminal interpretation of a completed retained stimulus, not a new stimulus
- `recursive-private-opportunity-selected` — durable fairness choice between generic stimuli and ordinary private work; selection is not a new stimulus

**fleet**

- `peer-board-notification-queued` — durable sender outbox entry for an authenticated board reply, not a new stimulus
- `peer-board-notification-delivered` — transport receipt for one queued board notification, not new cognition
- `peer-board-publication-intent` — frozen outbound board request and operation identity before transmission; recoverable effect evidence, not a stimulus

**effects already taken**

- `schedule-fired/notify` — an already-delivered notification is a published effect, not a reason to think again
- `pull-reciprocity-reply` — a reply already delivered; awareness of what was published belongs in a publication projection

**observability and infrastructure**

- `timing-trace` — no cognitive content
- `pg-backup` — no cognitive content
- `heap-health` — routine sample; the heap-pressure variant is admitted
- `projection-state` — projection rebuild bookkeeping; a projection reporting on itself is not a reason to think
- `postgres-row-state` — storage-level record; no cognitive content
- `model-request` — model IO instrumentation; waking for the agent's own request would be circular
- `model-response` — model IO receipt or accepted private outcome; its owning thread consumes it without creating a new stimulus

**turn mechanics**

- `agent-message` — the agent's own output; waking for it would be circular
- `conversation-episode-seal-opened` — durable provider-bound episode work is already owned by the quiet recursive step
- `conversation-episode-sealed` — a completed derived episode becomes recall evidence but must not recursively wake itself
- `conversation-episode-seal-failed` — the quiet recursive owner reports failure without a second scheduler wake
- `tool-call` — the agent issued it; only the result carries new information
- `conversation-context-budget` — turn bookkeeping
- `conversation-context-config-changed` — turn bookkeeping
- `context-curator-consumed` — turn bookkeeping
- `context-curator-fallback` — turn bookkeeping
- `conversation-episode-written` — turn bookkeeping
- `conversation-history-transform` — turn bookkeeping
- `turn-capture-completion-hook-error` — a completion hook failed; the turn itself was captured
- `conscious-interaction-claimed` — durable serialization claim; the admitted user-message already carries the stimulus
- `conscious-interaction-completed` — terminal interaction bookkeeping; any public agent-message is the conversational fact
- `conscious-interaction-failed` — terminal interaction bookkeeping exposed to the requesting channel, not a new thought trigger
- `conscious-interaction-outcome-unknown` — crash recovery stop marker preventing blind provider re-execution; operator retry is explicit
- `conscious-tool-operation-claimed` — durable execution claim; only the bounded result may enter a continuation
- `conscious-tool-operation-result` — private result consumed explicitly by the active interaction continuation, not a new attention trigger
- `conscious-tool-operation-failed` — closed terminal bookkeeping for the active interaction; it cannot authorize another execution
- `tool-turn-committed` — turn bookkeeping; the agent committed its own turn
- `turn-capture-complete` — turn bookkeeping
- `turn-capture-ready` — turn bookkeeping

**tick internals**

- `tick-start` — the tick loop is a legacy cognition owner being dismantled
- `tick-end` — the tick loop is a legacy cognition owner being dismantled
- `tick-note` — the tick loop is a legacy cognition owner being dismantled
- `tick-maintenance` — the tick loop is a legacy cognition owner being dismantled
- `tick-budget-limit` — the tick loop is a legacy cognition owner being dismantled
- `tick-continuity-fact` — the tick loop is a legacy cognition owner being dismantled
- `tick-initiative-proposed` — the tick loop is a legacy cognition owner being dismantled
- `reflection-pass` — the tick loop is a legacy cognition owner being dismantled
- `external-signal` — a search result recorded inside tick proposal machinery; the tick loop is a legacy cognition owner being dismantled
- `tick-proposal-duplicate-merged` — tick proposal bookkeeping
- `tick-terminal` — tick lifecycle bookkeeping

**memory bookkeeping**

- `memory-write` — a consequence of cognition; admitting it creates a think-write-think loop with no stop
- `memory-decay` — maintenance, not a reason to think
- `memory-importance-scored` — maintenance, not a reason to think
- `memory-use-recorded` — maintenance, not a reason to think
- `memory-superseded` — maintenance, not a reason to think
- `memory-quarantined` — maintenance, not a reason to think
- `memory-quarantine-changed` — maintenance, not a reason to think
- `legacy-memory-recall-used` — maintenance, not a reason to think
- `epistemic-admission-accepted` — admission bookkeeping
- `epistemic-admission-rejected` — admission bookkeeping
- `epistemic-admission-shadow` — admission bookkeeping

**memory authority**

- `memory-operation-state` — authoritative projection input; cognition already caused it and replay must not self-wake
- `memory-baseline-started` — migration framing for rebuild, not a new stimulus
- `memory-baseline-node` — bounded private baseline state for replay, not a new stimulus
- `memory-baseline-edge` — bounded private baseline state for replay, not a new stimulus
- `memory-baseline-committed` — hash-closed migration evidence, not a new stimulus

**deliberation internals**

- `initiative-candidate-scored` — an intermediate step of a decision already being made
- `initiative-decision` — an intermediate step of a decision already being made
- `candidate-nominated` — an intermediate step of a decision already being made
- `candidate-coincidence` — an intermediate step of a decision already being made
- `candidate-promoted` — an intermediate step of a decision already being made
- `candidate-aged-out` — an intermediate step of a decision already being made
- `explore-novelty-deferred` — an intermediate step of a decision already being made
- `explore-root-deferred` — an intermediate step of a decision already being made
- `explore-stance-committed` — an intermediate step of a decision already being made
- `explore-stance-deferred` — an intermediate step of a decision already being made
- `attention-schema-update` — an intermediate step of a decision already being made
- `attention-schema-divergence` — an intermediate step of a decision already being made
- `self-model-question-status` — an intermediate step of a decision already being made
- `self-model-question-migration` — an intermediate step of a decision already being made
- `reflection-no-novelty` — an intermediate step of a decision already being made
- `drive-near-threshold` — an intermediate step of a decision already being made
- `prediction-written` — the resolution is the signal, not the writing
- `soul-entry-added` — an outcome already decided
- `episode-flushed` — an outcome already decided
- `episode-replay` — an outcome already decided
- `reciprocity-canary-transition` — an outcome already decided
- `public-system-prompt-changed` — an outcome already decided
- `user-timezone-changed` — configuration, applied where it is read
- `scheduled-context-consumed` — acknowledgement bookkeeping
- `agent-appraisal-derived` — an intermediate appraisal step; the outcome it feeds is what matters
- `cognitive-call-start` — cognition instrumentation; the call's outcome is what matters
- `cognitive-call-end` — cognition instrumentation; the call's outcome is what matters
- `cognitive-artifact-call-start` — cognition instrumentation
- `cognitive-artifact-call-end` — cognition instrumentation
- `latent-seeded` — latent-thought lifecycle; an intermediate step of reflection already under way
- `latent-transition` — latent-thought lifecycle
- `latent-thought-incubated` — latent-thought lifecycle
- `latent-thought-merged` — latent-thought lifecycle
- `latent-thought-ready` — latent-thought lifecycle; readiness is consumed by the tick machinery being dismantled
- `latent-thought-expired` — latent-thought lifecycle
- `reciprocity-typed-label-recorded` — an outcome already decided

**operation sub-lifecycle**

- `agent-operation-claimed` — only the terminal is admitted; waking on each state would make one operation produce a dozen wakes
- `agent-operation-lease` — only the terminal is admitted
- `agent-process-transition` — only the terminal is admitted
- `operation-claimed` — only the terminal is admitted
- `operation-budget` — only the terminal is admitted
- `operation-class` — only the terminal is admitted
- `operation-cooldown` — only the terminal is admitted
- `operation-in-flight` — only the terminal is admitted
- `operation-not-claimable` — only the terminal is admitted
- `grounded-project-proposal-created` — only the terminal is admitted
- `grounded-project-proposal-reviewed` — only the terminal is admitted
- `publication-candidate-withheld` — only the terminal is admitted
- `artifact-completed` — agent-operation-terminal already carries artifact identity and is admitted; admitting both would wake twice for one completion
- `artifact-validated` — a validation step inside an operation; the terminal reports the verdict
- `publication-candidate-validated` — a publication pipeline step; its withheld counterpart is likewise journal
- `grounded-agency-after-operation-error` — a post-operation hook failed while the operation itself completed; the terminal carries the outcome
- `grounded-agency-idle-hook-error` — an idle reconciliation hook failed; no work was in flight to misreport

**self-modification internals**

- `self-mod-proposed` — a pipeline step; terminal outcomes are admitted
- `self-mod-provenance-recorded` — a pipeline step; terminal outcomes are admitted
- `self-mod-outcome-verdict` — a pipeline step; terminal outcomes are admitted

**consumption bookkeeping**

- `stimulus-consumed` — written by this runtime; admitting it would make consuming a stimulus produce a stimulus

**conscious pulse lifecycle**

- `pulse-opened` — attempt boundary; recovery input, never a new attention candidate
- `pulse-committed` — terminal commit and consumption acknowledgement; admitting it would self-wake
- `pulse-cancelled` — terminal cancellation record; its original trigger remains candidacy
- `pulse-failed` — terminal failure record; runtime health reporting owns escalation
- `pulse-recovered` — recovery disposition for an orphaned open pulse
- `concern-presented` — event-derived concern history consumed by the conscious projection
- `concern-deferred` — event-derived concern history consumed by the conscious projection

**conscious work lifecycle**

- `conscious-lifecycle-transition` — Q5 work coordination state; admitting bookkeeping would make a lifecycle transition wake on itself
- `conscious-lifecycle-result-rejected` — Q5 stale/invalid result disposition; the original result event remains the attention stimulus
- `conscious-lifecycle-source-rejected` — Q5 durable disposition of an illegal producer transition; the original producer event remains the attention stimulus
- `conscious-lifecycle-command-requested` — content-free operator command receipt; the producer creation event carries the attention transition
- `conscious-lifecycle-semantic-described` — Q5S semantic descriptor source; lifecycle truth remains the attention stimulus

**motivational dynamics**

- `conscious-curiosity-observed` — Q5M evidence; one coalesced motive candidate, not each recurrence, owns attention
- `recursive-curiosity-origin-context-recorded` — runtime-owned exact origin evidence for one curiosity observation; later projections may use it without waking attention independently
- `recursive-curiosity-follow-up-requested` — operator-authorized one-shot result-delivery commitment attached to an exact open motive
- `recursive-curiosity-follow-up-completed` — idempotent delivery receipt fulfilling one requested curiosity follow-up
- `conscious-curiosity-opportunity-observed` — Q5M opportunity evidence; a later content-free candidate event owns admission
- `conscious-curiosity-satisfaction-observed` — Q5M satisfaction evidence; suppressing a motive must not wake it again
- `recursive-curiosity-result` — private terminal finding; inspection owns it and admitting it would make completed curiosity wake itself
- `recursive-curiosity-focus-failed` — terminal durable failure for one private focus attempt; it preserves provider or protocol-quarantine evidence and prevents one unrecoverable root from blocking later attention
- `recursive-curiosity-review-opened` — sealed private review range; background bookkeeping must not wake attention on itself
- `recursive-curiosity-review-completed` — private review watermark and receipt; observations separately carry motivational evidence
- `recursive-curiosity-attention-opened` — sealed open-curiosity generation for one private recursive attention decision
- `recursive-curiosity-attention-declined` — revision-bound durable decline; the same unchanged register must not repeatedly consume inference
- `recursive-curiosity-attention-completed` — content-free receipt linking one sealed register generation to its chosen private focus
- `recursive-curiosity-attention-quiescent` — one content-free receipt that every bounded page of an unchanged open-curiosity generation has settled; quiet wakes remain silent until evidence changes
- `recursive-curiosity-consolidation-opened` — sealed bounded open-motive generation awaiting one non-destructive semantic presentation frame
- `recursive-curiosity-consolidation-completed` — validated presentation-only motive partition consumed by attention and briefing; it cannot mutate motive authority
- `recursive-curiosity-result-review-opened` — sealed private result review; lifecycle disposition remains a separate deterministic commit
- `recursive-curiosity-result-review-completed` — content-free close/refine/sustain receipt for one completed private investigation
- `recursive-curiosity-incorporation-opened` — sealed private retention judgment for one reviewed curiosity result
- `recursive-curiosity-incorporation-completed` — durable retained-or-declined receipt linking one result to memory and optional autonomous publication evidence
- `recursive-curiosity-briefing-opened` — sealed bounded private-cognition rows awaiting one compact briefing; it is bookkeeping inside an already-running quiet root
- `recursive-curiosity-briefing-completed` — private model-generated compression consumed explicitly by ordinary context; it must not wake attention on itself

**cognitive work lifecycle**

- `conscious-work-opened` — content-free work identity derived from an already-admitted stimulus
- `conscious-work-suspended` — explicit scheduling disposition; suspension must not wake itself
- `conscious-work-resumed` — explicit scheduling disposition; the original concern remains the attention root
- `conscious-work-completed` — terminal work bookkeeping; stimulus consumption is recorded separately
- `conscious-work-failed` — closed work failure disposition exposed through operational reporting
- `conscious-work-outcome-unknown` — uncertain provider or effect boundary; never an instruction to retry
