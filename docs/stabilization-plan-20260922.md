# Stabilization and promotion plan — 2026-09-22

This plan consolidates the current fleet, retrieval, and working-context work
before any further deployment to private agents. The public source candidate is
the canonical implementation. Private trees are deployment targets and evidence
sources only; they are not copied wholesale into this repository.

## Reconciliation correction — 2026-09-22

The current public candidate is **not yet a faithful reconciliation of the
development instance**. Source comparison after the generic-stimulus slices
found material development-only behavior in the recursive runtime: complete
working-request fitting and preview, experience search, shared stimulus
activity grouping, durable disposition/retry/consumption, peer publication
recovery, and a different quiet-opportunity policy. The public receipt-to-
generic-stimulus bridge is a locally qualified alternative, not evidence that
those existing behaviors were ported. Its tests cannot establish promotion
parity.

Before another full-suite promotion claim, review and port the development
implementation in coherent dependency groups, preserving public-only fixes
only where a source comparison and synthetic regression justify them:

The ordered source/target handoff is in
[development-source reconciliation slices](development-source-reconciliation-slices-20260922.md).

1. Working-request fit, preview, recent activity, and experience search.
2. Shared stimulus activity membership and exact thread context.
3. Durable dispositions, retries, consumption, and peer egress recovery.
4. Quiet-opportunity selection and queue pressure on the reconciled model.
5. Re-run the full public gates, then compare the exact candidate with each
   dirty private target using a reviewed allowlist before any promotion.

The prior full isolated Lisp run was intentionally interrupted when this gap
was identified. Its partial passing suites are diagnostic only. The private
instances were not modified.

## Safety boundary

- Preserve every unrelated dirty change in all three trees.
- Review and qualify one coherent slice at a time in `pAIProject`.
- Use synthetic fixtures only in public source and tests.
- Commit accepted public slices before promoting that exact commit.
- Promote with [the public-source promotion runbook](public-source-promotion-runbook.md)
  and an explicit file allowlist. Never reverse-sync a private working tree.
- Keep every downstream private instance unchanged until the public candidate
  passes the applicable gates.
- Back up each private target before promotion and retain a rollback receipt.

## Slice A — reliable fleet receipt and board-local replies

Current public working tree contains this slice. It includes authenticated local
receipt capture, operation IDs and idempotent retry, interrupted board/ledger
write recovery, explicit same-board replies, replyable message IDs, and bounded
ingress. It also includes the isolated-runner false-positive correction needed
to qualify ratios such as `0/1 failed` accurately.

Required evidence:

- offline ASDF load;
- wrap-chain completeness;
- `fleet-auth-tests.lisp`;
- `fleet-board-tests.lisp`;
- `fleet-identity-tests.lisp`;
- `fleet-join-tests.lisp`;
- `fleet-receipt-tests.lisp`;
- isolated-runner Python tests;
- event census and publication hygiene checks.

Qualification environment note: the immutable image creates an empty, writable
`/agent/state` directory so offline load does not depend on a host mount. The
candidate inventory is 230 suites, including this slice's receipt suite; the
qualification contract and its test must agree with that inventory.

Known limit: sender-side durable outbox/retry and autonomous peer attention are
not part of this slice. A successful receipt proves delivery into the receiving
agent's ledger, not that the agent noticed or answered it.

## Slice B — bounded semantic-memory retrieval (source-qualified)

Port the reviewed bounded retrieval implementation and its focused tests from
the private development evidence into public source manually. Do not copy
instance configuration, state, conversations, identities, or generated data.

Required evidence:

- exact retrieval retains correctness and integrity behavior;
- SQLite-derived retrieval is bounded and restart-safe;
- memory mutation/storage tests pass;
- large fixtures do not materialize all vectors or all candidate rows in memory.

This is a mandatory crash gate, not an optional optimization. The observed
failure allocated a generation-sized parsed-row/vector cache while scanning more
than eleven thousand records. Qualification must include the 12,000-record
fixture under a 256 MiB Lisp heap, repeated exact and lexical retrieval, retained
growth below 8 MiB, winner parity, and corruption in a non-winning row. A healthy
startup alone does not exercise or qualify this path.

Evidence recorded 2026-09-22: 219 distinct focused checks passed in fresh
processes. The 145-check bounded suite passed with a warmed compile cache and a
256 MiB Lisp heap, including 12,000 streamed records, repeated exact/lexical
searches, retained growth below 8 MiB, winner parity, exclusions, hydration, and
corruption of a non-winning row. The first cold 256 MiB attempt exhausted memory
while compiling dependencies before the fixture ran; this is recorded as a test
environment limitation, not retrieval evidence. No private deployment is implied.

## Slice C — sustained working context

Port activity storage, sustained-activity context/runtime/operator code, and
structural compaction as one reviewed dependency set. The recent complete
exchange must remain verbatim. Older completed exchanges may replace
intermediate model/tool payloads with a holistic summary and durable links to
their original ledger events.

Required evidence:

- activity storage and restart tests;
- recent and sustained context tests;
- compaction tests proving protected exchanges are retained;
- recursive-runtime tests;
- no automatic dependence on full event replay during ordinary startup.

This slice must also qualify the pre-summary path. Context assembly must not
construct an unbounded request merely in order to decide that the request is too
large. Oversized activities must settle or compact without exhausting the heap,
and the latest complete exchange plus current tool results must remain available.

Interim evidence recorded 2026-09-22: the canonical source now contains the
bounded SQLite activity reader, deterministic sustained-context projection,
explicit lifecycle/operator controls, recursive request integration, pure
request preview, and source-linked structural compaction. In fresh processes,
activity storage/restart/integrity passed 41/41, context projection passed
31/31, operator lifecycle passed 24/24, and recursive runtime/wire reconstruction
passed 39/39. This qualifies the deterministic pre-summary dependency set only;
the recent-evidence and pressure/compaction suites, web entry-point wiring, and
holistic persisted summary slice remain promotion gates.

## Slice D — holistic working-context summaries

Persist and reuse one holistic summary for a selected span, with durable source
event references. Summary accounting belongs to the operator turn rather than
the private-cognition budget.

Qualification caveat: source tests previously passed in the private development
tree, but live evidence showed no persisted summary rows or summary-model events.
The likely overlarge-source-span path remains unverified. This slice must not be
promoted as complete until a synthetic over-budget activity produces, persists,
reuses, and can invalidate a summary.

Source evidence recorded 2026-09-22: the canonical synthetic suite passed 95/95
in a fresh process. It forces an over-budget multi-exchange activity through one
whole-span model callback, validates source links and exact excerpts, reduces the
request by more than fourfold, persists the accepted projection in derived
SQLite, reuses it after backend reopen without regeneration, and invalidates the
cache when the model revision changes. Production wiring charges summary calls
to the admitted operator turn and records model request/response evidence. Live
canary evidence is still required before private promotion.

Regression evidence recorded from the same candidate image: sustained-activity
runtime 39/39, recursive-mind runtime 422/422, and conscious-conversation runtime
169/169. Offline ASDF load and wrap-chain completeness (1/1) also pass; the
coupling report remains at zero hard back-edges.

## Slice E — recursive hot-projection ownership (source-qualified interim)

Exclude knowledge-graph formation model request/response events from the
recursive hot projection, including when loading an older projection revision.
This is a bounded migration safeguard, not the final storage architecture.

Architectural target: ordinary operation reads row-backed derived state and
never rebuilds the agent by default. Explicit full replay remains a separately
invoked repair/verification operation with an intentionally large memory budget.
The monolithic checkpoint is transitional and must not become a new authority.

Until row-backed replacement lands, the interim projection must pass a pressure
test proving that oversized state is rejected before JSON serialization, the
previous checkpoint remains usable, and normal startup refuses an unbounded full
replay when the checkpoint is absent or stale. Explicit offline maintenance is
the only path allowed to request full replay.

Evidence recorded 2026-09-22: checkpoint pressure passed 8/8, recursive runtime
passed 422/422, and SQLite event authority passed 29/29 in fresh processes.
The cache restores a source-bound derived checkpoint, reads only a bounded tail
during normal advancement, compacts settled provider payloads in its derived
copy, excludes graph-formation-owned provider transcripts, preflights checkpoint
size before serialization, and refuses an implicit full replay above the
normal-operation threshold. The explicit rebuild launcher is the separate
high-memory recovery path. This does not make the checkpoint the final
architecture or establish long-duration live stability.

## Slice F — generic private stimulus admission (in progress)

Adapters may record a durable `agent-stimulus-received` event when something in
their environment is worth the agent considering. The common attention layer,
not a messaging-specific path, decides whether to act. The initial admission
rule projects this event as an `environment-change` with private audience and
external-content trust. It deliberately does not grant operator authority and
does not make peer receipts or recursive execution bookkeeping self-waking
stimuli.

Evidence recorded 2026-09-22: the canonical census was raised to version 15,
the generated census documentation was refreshed, and
`conscious-stimulus-tests.lisp` passed 100/100 in a fresh isolated container.
The bounded intake and private-root projection then passed in the focused
full-system recursive runtime suite (424/424). This qualifies admission and
root construction only. Environment observation seams, generic execution and
recovery, and peer delivery through this same path remain later slices.

The next focused recursive suite passed 427/427 after adding a registered
read-only observation seam. The native tool is advertised only when a concrete
adapter layer exists. Its resource identity and continuation fields are bounded;
the returned JSON must fit the retained tool-result limit in full. Interrupted
observation recovery and autonomous settlement remain open qualification gates.

Generic stimulus roots now use the existing private result boundary. An
accepted response is recorded as `recursive-stimulus-result`; replay checks its
thread, model call, and exact content before projecting the root to `done`.
The focused recursive runtime suite passed 430/430, and the census/stimulus
suite passed 100/100 after classifying the result as journal-only (census v16).
Offline ASDF load and wrap-chain completeness (1/1) passed. This establishes
durable completion for a root driven explicitly through the recursive executor;
quiet-cycle selection, interrupted read recovery, and automatic consumption
still require qualification.

Interrupted read-only observation recovery now reuses the recorded tool name,
arguments, model call, and tool call from the event ledger. It may repeat an
`observe-environment` read through the registered adapter and records the new
complete result under the original root. Other interrupted tools remain
outcome-unknown. The focused recursive suite passed 433/433, including the
interrupted projection and a synthetic recovered read. Quiet-cycle selection
and consumption are still open; this source has not been promoted to an agent.

The next selector slice reads retained `agent-stimulus-received` roots in ledger
order and projects each one before considering it eligible. Completed roots are
excluded, as are in-flight provider calls and failed or uncertain roots. It
does not yet run from the quiet cycle or define retries and consumption. The
focused recursive suite passed 437/437 in a fresh process, including agent
isolation, in-flight parking, unfinished selection, and completed exclusion.
This is deliberately not autonomous stimulus qualification or promotion evidence.

The quiet wake now advances at most one safely projected generic stimulus through
the existing recursive executor before other private work. A completed result
is excluded on the next wake by its durable ledger event; failed and uncertain
roots remain parked, and waiting operator input preempts execution. This adds
no new timer and no messaging-specific decision path. The focused recursive
suite passed 440/440 in a fresh process, including executor dispatch, idle,
and preemption checks. This is source-level autonomous execution, not a live
canary. Peer ingress, deliberate recovery/settlement, fairness under a
sustained queue, and full publication qualification were the next promotion
gates; the ingress slice is described below.

Peer ingress now has a source-level bridge: an authenticated, durable
`peer-message-received` receipt remains journal-only, while a second
`agent-stimulus-received` event copies its content and bounded board context,
causally linked to that receipt. The append is idempotent under transport
retry. A registered fleet adapter repairs a receipt whose linked stimulus was
not appended before interruption, using only the receiving agent's ledger;
the existing quiet wake then executes the generic root. The census is v17.
The isolated receipt suite, recursive runtime suite, and stimulus/census suite
passed in fresh processes: fleet receipt 42 checks, fleet board 31 checks,
recursive runtime 440/440, and stimulus/census 100/100. Offline ASDF load,
wrap-chain completeness (1/1), census generation check, and coupling report
(zero hard back-edges) also passed. The receipt fixture injects a failed
second append after the delivery receipt, verifies that the post is not
acknowledged, then proves an authenticated retry repairs the linked stimulus
without duplicating the board message. A separate fixture repairs an old
receipt from the receiving ledger after interruption. This is not evidence of
autonomous peer conversation:
long-running queue fairness, deliberate terminal/retry dispositions, a live
canary, and full publication qualification remain open.

The quiet wake now makes a durable, generic fairness choice when both a
stimulus and ordinary private work are available. The default registered seam
alternates opportunities based on the last ledger-recorded choice; a selected
stimulus gets one bounded quantum, while a private-work choice leaves the
stimulus pending. There is no new timer, model call, or hard-coded engagement
desire. No choice event is written when no stimulus competes. Census v18
classifies choice records as journal-only. The focused recursive suite passed
449/449, the stimulus/census suite passed 100/100, and the fleet receipt suite
passed 44 checks in fresh processes. These cover alternation, no idle decision
flood, quiet-step branch isolation, and recovery of the last choice after
reopening SQLite. Offline ASDF load, wrap-chain completeness (1/1), census
generation, and the zero-hard-back-edge coupling check also passed.
Sustained-queue pressure and live behavior remain qualification gates.

The generic selector now builds a one-pass causal index for retained roots and
projects only a root plus its directly caused events. Quiet arbitration asks
for the first safe candidate rather than materializing all pending roots. A
synthetic queue with 1,000 completed roots, one uncertain provider request,
and one fresh root selected the fresh root while making only two projection
calls, each over at most two events; the focused recursive suite passed
450/450. This removes the prior repeated full-hot-history scan from queue
selection. It is structural source evidence, not a 256 MiB heap-pressure or
live backlog result. Durable retry and terminal disposition policy remain open.

Interrupted registered `observe-environment` calls are now eligible for the
ordinary quiet wake only when the recorded tool identity, call IDs, bounded
arguments, and current read-only adapter all validate. The existing executor
then records a fresh observation result under the original root. Provider calls
with unknown outcomes and arbitrary interrupted effects remain parked, not
replayed. The focused recursive suite passed 453/453, including selector
exclusion before adapter registration and admission after registration. Failed
context-open roots are terminal in the current projector; explicit retry or
operator disposition remains a separate policy decision, not an implicit
repeat of potentially consequential work.

## Mandatory crash matrix

No private promotion is permitted until all rows below have public-source tests
and passing evidence from a fresh process.

| Failure path | Required containment | Promotion evidence |
| --- | --- | --- |
| exact/lexical memory retrieval | O(K) candidates; hydrate bounded winners only | 256 MiB, 12,000-row pressure suite |
| vector integrity hashing | stream encoding; never allocate full hexadecimal copies per row | derived-memory and exact-retrieval suites |
| recursive hot projection | preflight before serialization; bounded tail; no routine full replay | checkpoint-pressure and restart tests |
| working-context assembly | size complete request incrementally; compact before provider call | over-budget activity and compaction suites |
| summary persistence | bounded original span; persisted derived row; source-linked reuse | generation, reopen, reuse, invalidation tests |
| interrupted activity | recover membership and newest complete exchange without replaying tools | crash/restart lifecycle suite |

Passing transport tests is insufficient. Passing startup is also insufficient:
the canary must exercise retrieval, context pressure, restart, and continuation.

## Deferred or rejected during stabilization

- Prompt-only instructions intended to make continuation more action-oriented:
  rejected after live behavior continued to repeat inability claims.
- A new action/disposition execution boundary: deferred until the current code
  is clean and qualified; it is a substantial architectural change.
- New motivation or messaging-specific stimulus pathways: deferred. Future work
  should use a generic stimulus/attention/action pipeline with adapter-provided
  operational guidance.
- Further board UX work: deferred until transport and context correctness are
  stable, though unread ordering and local-time presentation remain known gaps.

## Promotion order

1. Qualify and commit Slice A in public source.
2. Port, qualify, and commit Slices B–E independently or in the smallest honest
   dependency groups.
3. Run the complete publication gates once the candidate is assembled.
4. Promote the exact public commit to one development instance first and run a
   bounded live canary.
5. If its context, restart, retrieval, and fleet behavior are healthy, promote
   the same commit to the second private instance.
6. Verify the second instance against its own ledger and configuration; never
   copy another instance's private state or persona data.

## Live canary evidence

For each private agent, record startup memory/time, first-message latency,
complete recent-turn continuity, tool-result retention, activity continuation,
one semantic-memory query, one received peer-message receipt, and one board-local
reply. A transport pass does not compensate for a context failure. Any failure
halts promotion and triggers rollback to the recorded pre-promotion state.
