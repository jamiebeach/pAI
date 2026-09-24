# Development-source reconciliation: migration slices — 2026-09-22

This is the handoff for reconciling a running private development source
into this depersonalized public candidate. It is **not** a
directory-copy list or a claim that the current public implementation has
runtime parity. The development tree is dirty; compare its working files,
not only its last commit. Preserve unrelated public edits. Do not copy
private configuration, event/derived databases, identities, credentials,
operator conversations, deployment receipts, or private documents.

For each slice: capture a focused source diff, identify public-only changes,
port the behavior through the existing seam, add synthetic regression cases,
run its isolated suites in fresh Lisp processes, and record accepted/rejected
alternatives. A same-named file or passing public-only test is not parity
evidence. The source anchors below identify *behavior to inspect*, not lines
to transplant mechanically. Recheck them against the working tree when work
starts.

## S0 — Inventory and semantic contract (required first)

Status: **inventory and contract decision recorded; implementation/fixture
qualification belongs to S2**. This is a design gate, not runtime parity.
Read-only comparison on 2026-09-22 found a semantic census difference, not a
line-ending difference: public v18 makes `peer-message-received` a journal
and appends a linked generic root in `peer-receipt.lisp`; development v19
makes the receipt itself a `peer-message` stimulus. Development
`peer-receipt.lisp` ends after the idempotent receipt append, while public
adds `peer-message-ensure-stimulus` and a reconciliation layer. The shared
receipt append is otherwise the same. Public recursive runtime only admits
`agent-stimulus-received` as a private generic root; development also admits
`peer-message-received` and records peer-specific settlements. This is an
actual execution-contract conflict, not merely a manifest update.

Decision for migration: use the **local durable receipt as the single new
peer stimulus root**, as in development. It contains message content and
authentication provenance in the receiver's ledger and needs no continuing
sender/board authority to reconstruct state. Retain `agent-stimulus-received`
for other adapters. For *legacy public* ledgers containing both receipt and
linked generic root, group by receipt ID (`caused_by`/`receipt_event_id`) and
select one runnable root according to recorded activity/settlement; never
execute both. Preserve both historical events. Do not append a new generic
bridge for new receipts after cutover. This compatibility rule needs a
synthetic mixed-ledger regression before S2 is complete.

Initial reviewed file allowlist (all paths relative to each tree):

| Slice | Compare/port these files | Conflict to resolve |
| --- | --- | --- |
| S0/S2 | `src/mind/conscious/event-type-census.sexp`, `peer-receipt.lisp`, `recursive-mind-runtime.lisp` | Direct peer root vs linked generic bridge; legacy dual-root suppression. |
| S1 | `src/mind/conscious/conversation-runtime.lisp`, `sustained-activity-context.lisp`, `sustained-activity-runtime.lisp`, `sustained-activity-operator.lisp`, `recursive-mind-runtime.lisp`, `src/adapters/sqlite/sqlite-derived-storage.lisp`, `sqlite-activity-storage.lisp`, `scripts/conscious-conversation.lisp`, `pai.asd` | Working-request fit, evidence search, activity preview and durable summary. |
| S3 | `src/adapters/web/web-fleet.lisp`, `src/mind/fleet/board.lisp`, `src/mind/conscious/peer-receipt.lisp`, `recursive-mind-runtime.lisp`, census | Publication idempotency, settlement and outbox. |
| S4 | `src/mind/conscious/recursive-mind-runtime.lisp`, `peer-receipt.lisp` | Candidate-selection seam inputs and public-only generic observer. |
| S5 | `src/kernel/heap-health.lisp`, `storage-substrate.lisp`, `src/adapters/sqlite/sqlite-storage.lisp`, `sqlite-event-authority.lisp`, `sqlite-derived-storage.lisp`, `sqlite-activity-storage.lisp`, `src/mind/memory/memory-nodes.lisp`, `scripts/rebuild-recursive-thread-checkpoint.lisp`, `pai.asd` | Incremental database state vs routine replay/checkpoint rebuild. |
| S6 | `src/adapters/web/web-fleet.lisp`, `web-terminal.lisp`, `assets/observability.js`, `src/mind/conscious/context-graph-runtime-adapter.lisp`, launch scripts | Only dependent UI, graph and startup behavior. |

Known no-port candidates by content comparison:
`src/kernel/event-log.lisp`, `src/mind/memory/memory-storage.lisp`,
`src/mind/conscious/stimulus.lisp`, `working-context-summary.lisp`,
`context-graph-budget.lisp`, `src/adapters/web/web.lisp`, and the graph
search tool/formation adapter. Recheck hashes when a slice starts; identical
source can still need new caller wiring or tests. Test files are *not* on a
copy allowlist: inspect development fixtures for private data and recreate
synthetic public cases. The source is the development working tree, not its
Git HEAD. No private tree has been written.

- Compare `pai.asd`, `src/mind/conscious/event-type-census.sexp`, all files
  named below, their tests, and public-only commits since divergence. Produce
  an allowlist and an explicit disposition for every conflicting behavior.
- Resolve the central event-contract conflict **before** changing ingress:
  development census v19 classifies `peer-message-received` as a stimulus of
  kind `peer-message`; public census v18 classifies it as a journal event and
  generates a separate `agent-stimulus-received`. Determine which event is
  authoritative, how legacy ledgers remain readable, and how duplicate
  admission/consumption is prevented. Do not quietly retain both pipelines.
- Check whether different hashes are semantic or merely line endings. Known
  identical source files include `src/kernel/event-log.lisp`,
  `src/mind/conscious/stimulus.lisp`, and
  `src/mind/conscious/working-context-summary.lisp`; do not re-port these
  without a behavioral reason.
- Exit evidence: a conflict table with chosen event schema, compatibility
  plan, file allowlist, and synthetic ledger fixtures for old/new receipts.

## S1 — Complete working request and recoverable evidence

Status: **source-qualified and full-suite qualified; private canary pending**.
`sqlite-experience-page`, its authority-port registration, and the bounded
`search-experience` tool were ported into public source on 2026-09-22. The
tool supports newest-first, time-windowed ledger pages and exact paged
conversation/tool evidence by event ID; it refuses another agent's exact
event and excludes provider-private reasoning. The public candidate already
contains `%recursive-fit-working-request`,
`%recursive-working-summary-provider`, and
`conscious-recursive-preview-activity` in the extracted
`src/mind/conscious/working-context-request.lisp`; its recursive model
boundary calls the fitter and rejects `over-budget` before a provider call.
Do **not** re-copy those functions from development's monolithic recursive
file. The public tree does **not** contain development's
`%recursive-search-experience` or `search-experience` tool wiring before this
sub-slice, despite the public activity instruction mentioning that tool.
Development's
`working-request-budget-tests.lisp`, `default-working-context-tests.lisp`,
`recent-activity-context-tests.lisp`, and
`experience-search-storage-tests.lisp` are absent from the public test
inventory except for the newly ported storage suite; review the remaining
cases for synthetic-port suitability before claiming S1. Evidence from the
read-only public test container: offline ASDF load passed; wrap-chain
completeness 1/1; `experience-search-storage-tests.lisp` 6/6;
`conscious-recursive-mind-runtime-tests.lisp` 459/459 (four new search
security/contract checks and two recent same-path execution-evidence checks);
`working-context-summary-tests.lisp` 96/96 (including complete-request
tool-schema budget and reopen/reuse of a derived summary);
`sustained-activity-context-tests.lisp` 31/31;
`sustained-activity-operator-tests.lisp` 24/24; and
`sustained-activity-runtime-tests.lisp` 39/39. The last suite already
asserts preview wire equals the actually assembled request, and its authority
fixtures cover scoped restart/reopen. The coupling report shows zero hard
back-edges (ten declared soft edges). The qualification inventory
contract reports 238 suites and its five Python tests pass. A stale scratch
SQLite cache made the summary suite non-repeatable; its fixture now removes
only its exact disposable test database before opening it. Development's
additional budget/default-context fixtures remain useful follow-up coverage,
not a known missing source behavior. The full isolated source run on
2026-09-23 passed 238/238 suites. No long-running private runtime canary has
run; S1 is not deployment-qualified.

Development anchors: `recursive-mind-runtime.lisp` at
`%recursive-fit-working-request` (~102),
`conscious-recursive-preview-activity` (~266), and
`%recursive-search-experience` (~1119). Compare
`conversation-runtime.lisp`, `sustained-activity-context.lisp`,
`sustained-activity-runtime.lisp`, `sustained-activity-operator.lisp`,
`sqlite-derived-storage.lisp`, `sqlite-activity-storage.lisp`,
`scripts/conscious-conversation.lisp`, and `pai.asd`.

Port the complete wire-budget decision (messages, tools, output allowance),
activity preview, holistic summary/reuse with ledger references, bounded
recent verbatim exchanges, and paged `search-experience` evidence retrieval.
Keep the event ledger authoritative; SQLite summaries/activity projections
must be rebuildable, not a second source of truth. Reconcile public working
context code rather than layering a second compactor. Candidate development
tests: `working-request-budget-tests.lisp`,
`default-working-context-tests.lisp`, `recent-activity-context-tests.lisp`,
`experience-search-storage-tests.lisp`,
`working-context-summary-tests.lisp`, and existing public sustained-activity
tests. Some are absent in public; inspect and port only synthetic cases.

Exit evidence: previews match the request actually sent, the newest complete
exchange survives fit, over-capacity requests yield an explicit bounded
outcome, summaries reuse durable projections, and an agent can retrieve an
original event from a summary reference after restart.

## S2 — Stimulus admission and shared activity membership

Status: **direct-receipt source cutover implemented; mixed-ledger and recovery
qualification still open**. New authenticated posts and notifications retain
one local `peer-message-received` event and no generic bridge. The event census
classifies that event as a background private stimulus (census v20). The normal
quiet-wake selector admits it through the shared stimulus executor. Resource
identity comes from projected context, so related direct receipts on the same
board thread batch together and their exact local content appears in the
leader's prompt. Historical receipt-plus-bridge pairs
still choose only the bridge. Completed historical direct-peer results and
dispositions suppress re-execution; an unfinished old-format direct peer turn
is parked rather than reinterpreted under the new thread grammar. The old
bridge producer and repair layer were removed. The recursive hot projection
includes both event grammars and has projector revision v3, so a compatible
database-state migration remains mandatory before either private canary.
The current focused fleet suite passes 96 checks, recursive runtime 478/478,
and checkpoint pressure 8/8; stimulus 100/100 passed at the preceding
source revision. That preceding revision passed the full fresh-process Lisp
runner 241/241 suites, zero failed or unqualified. Offline ASDF load,
wrap-chain completeness (1/1), census generation check, coupling check with
zero hard back-edges, and `git diff --check` pass. Python publication (16),
isolated-runner (15), and qualification-contract (5) tests pass after updating
the stale 238-suite expectation to 241; the contract reports 241 discovered,
240 default, one override, and no blocked profile. These are public source
checks, not private canary or exact-commit release qualification.

Earlier incremental evidence below describes stages superseded by this cutover.
`peer-message-admission-root` resolved a receipt to its
already-linked generic root in old ledgers, or to the receipt itself when no
legacy bridge exists. It rejects two competing linked roots and ignores
another agent's linked event. `fleet-receipt-tests.lisp` passes 47 checks,
including three new synthetic ownership checks. This is a preparatory pure
rule only; the existing public bridge still runs and direct peer receipts
are not yet admitted. The pure `stimulus-activity.lisp` projection now
validates exact activity ownership (at most eight unique members, leader
first, no conflicting claim, agent scope); its default resource key is
adapter-overridable through a registered seam. The isolated fleet receipt
suite includes four synthetic ownership checks.
The pure freeze builder now takes prequalified candidates and preserves at
most eight same-type, same-agent, same-resource members under a 48,000-
character follower budget, with original event IDs attached. The fleet
receipt suite passes 65 checks including bounded-batch and pending-root
projection regressions. The live generic selector now delegates eligibility
to this shared projection before checking per-root runnable state: frozen
followers cannot start a second turn, and a terminally settled leader does
not release them. Two synthetic live-selector checks bring the focused fleet
receipt suite to 80 checks. The next sub-slice wires generic activity opening
before the leader's first model request. A durable `recursive-activity-opened`
row freezes at most eight same-resource inputs; its exact contexts are included
in the leader's request. A completed leader causes each follower to acquire a
`covered` disposition and consumption, one repairable row per quiet step.
The SQLite-backed fixture verifies opening idempotence, frozen request context,
interrupted settlement repair, and persistence after authority reopen. A
further live-read check caught that `recursive-activity-opened` was absent
from the recursive hot-projection type set; it is now included there and in
the event census. The fixture now asserts the *same projection used by live
turns* sees the frozen activity and its follower context. Focused fleet
receipt tests pass 90 checks; recursive runtime tests pass 470/470. Direct
peer-root selection remains off.
The next preparatory sub-slice gives a direct peer receipt its private
recursive-root descriptor using only the receiving agent's authenticated
ledger payload. It validates agent ownership, required board identifiers,
content bounds, and trust provenance before presenting a generic private
purpose; it never fetches content from the board. Synthetic direct-root,
foreign-agent, and untrusted-payload checks bring the isolated fleet receipt
suite to 78 checks at that sub-slice, and the recursive runtime suite passes
470/470. The
descriptor remains dormant for normal intake: the linked generic bridge is
still appended, and direct peer roots are not yet selected for execution.
The 2026-09-23 read-only comparison against the development working tree
confirmed that development intake appends only `peer-message-received`, while
its recursive projection uses a dedicated `peer-message` root and
`recursive-peer-message-*` result/retry/disposition events. Public source
still uses `stimulus`/`recursive-stimulus-*` for its generic bridge. Do not
switch the census or remove the bridge by merely enabling the prepared
descriptor: the public projector and terminal settlement do not yet support
development's dedicated root kind. The prepared descriptor now preserves
the original sender, board owner, thread, message ID, and same-board versus
sender-board reply tool from the authenticated receipt. This is target
metadata for the transition, not a claim that a reply was made.
Development's
`%recursive-activity-membership` validates a single owner per receipt,
`%recursive-pending-stimuli` indexes consumption/dispositions in one pass,
and `%recursive-open-stimulus-activity` freezes at most eight related
receipts/48,000 characters before a model request. This depends on the
disposition and recovery events in S3; porting only the census/receipt
classification would make new peer roots visible without a complete
settlement path. Implement S2 and S3 as separately evidenced commits but
qualify their **combined** event transition before enabling direct peer
ingress. The public generic bridge remains active until that atomic cutover.

Cutover checklist from the current source comparison (not yet complete):

- [x] Preserve exact authenticated receipt content and board target in the
  receiving ledger; resolve legacy linked-root ownership without duplicating
  execution. Focused 89-check fleet receipt suite passes.
- [x] Choose the generic-stimulus contract for new direct receipts while
  retaining terminal ownership evidence from the old peer-specific grammar;
  synthetic mixed-ledger selection and fail-closed old in-flight checks pass.
- [x] Make live generic selection respect frozen follower ownership and
  terminal leader settlement; synthetic selector checks pass.
- [x] Wire bounded generic activity opening before the first model request
  and settle covered followers after a completed leader result. Live hot-
  projection, SQLite reopen, and interruption-repair fixtures pass; failed
  leaders remain parked for the recovery slice rather than consuming
  followers prematurely.
- [ ] Qualify failed/provider-unknown/tool-unknown recovery and board-correct
  publication evidence for direct receipts, not only legacy bridges.
- [x] Switch the census, intake, quiet selection, and hot type set together;
  cease new generic bridge appends. Synthetic mixed-ledger and duplicate-send
  fixtures pass; full restart and private-ledger migration remain open.

Development anchors: `recursive-mind-runtime.lisp` around
`%recursive-pending-stimuli` (~8808),
`%recursive-open-stimulus-activity` (~8840), and
`%recursive-reconcile-activity-settlements` (~8876). Compare
`peer-receipt.lisp`, `sustained-activity-*`, `src/kernel/activity-storage.lisp`,
`src/adapters/sqlite/sqlite-activity-storage.lisp`, the census, and the
public-only `recursive-stimulus-adapter-reconcile-one` path.

Port/merge admission, causal membership, replay-safe consumption and exact
thread-context refresh without losing the general stimulus abstraction.
Preserve the distinction between a receipt, an activity, and an action; a
new board event must not spawn two competing roots. Development currently
freezes a bounded related-experience window on activity opening; verify its
limits against public compaction and restart behavior. Candidate tests:
`sustained-activity-lifecycle-tests.lisp`,
`sustained-activity-compaction-tests.lisp`,
`environment-observation-tests.lisp`, `fleet-receipt-tests.lisp`, and public
`conscious-stimulus-tests.lisp`.

Exit evidence: synthetic receipt admitted once, joins the correct activity,
reads current thread state before reply, survives restart, and does not
re-trigger after terminal consumption. No board-specific hard back-edge in
the generic loop.

## S3 — Durable disposition, retry, and peer publication recovery

Status: **in progress; direct peer-root execution is now selected, but direct
settlement and failure recovery are not yet qualified for a canary**. New
direct receipts use the shared generic-stimulus result and safe fleet-tool
retry machinery. The older bridge disposition classifier remains for replay;
the old bridge producer is gone. A completed generic result already proves
terminal execution and suppresses a second turn. Do not add a peer-only
disposition/consumption pathway unless a concrete consumer requires it:
durable tool execution/result and board/outbox records should prove the
action, while frozen followers retain generic `covered` settlement. This
slice now has focused direct-root recovery checks: an uncertain provider call
stays parked; an interrupted validated fleet effect is selected only while
its publication adapter is registered; recovery uses the durable root/tool
operation key and journals one result; reprojecting that result continues the
turn without sending again. A synthetic crash after the board accepts a
same-board reply but before the local tool-result append now exercises the
same durable operation key on retry: reopening the board store retains one
reply, and retry appends one local result without a duplicate reply. The
recursive runtime suite passes 484/484.
The SQLite-backed fleet fixture passes 99 checks: a completed direct root
reconstructs as done from the authority after same-process reopen and in a
fresh Lisp process, with no pending duplicate turn; the compacted hot
projection retains its root and terminal marker. A synthetic two-authority
delivery fixture follows a same-board reply through the queued outbox to a
separate receiving SQLite ledger, verifies exact content/thread/parent there,
and suppresses a second send. The signed HTTP path and independent live
instances remain private-canary gates. The crash drill is an injected interruption
within one Lisp process, not a substitute for those gates. The paragraphs below record earlier S3
milestones.

Local board
reply and remote board post now accept an optional caller-supplied operation
ID (and remote post can address an explicit same-board thread/parent) while
ordinary operator calls keep generated IDs. The recursive tool boundary now
derives a SHA-256 operation key from the durable root/tool-call IDs and passes
it to either fleet publication adapter; the model's `post-fleet-message`
schema admits exact peer-board thread/parent only as a pair. The local board
store deduplicates a repeated reply with the same key. The isolated
`fleet-receipt-tests.lisp` passes 72 checks, `fleet-board-tests.lisp` passes
31, and recursive runtime tests pass 470/470. The public receiver now
authenticates and records a full local peer receipt; it still invokes the
legacy generic-stimulus bridge. Local replies journal one sender notification
outbox entry per board-message ID, attempt delivery, and leave a durable
undelivered entry if the peer is unavailable. The recursive quiet loop has
an injected, one-item retry seam, wired by the conversation launcher. This
is transport maintenance, not a board-check stimulus or curiosity timer.
The current retry implementation scans notification events in the ledger;
S5 must replace that operational scan with bounded indexed state. Completed
legacy peer bridges now receive a durable `recursive-stimulus-disposition`
and `stimulus-consumed`, including a repair path for interruption between
the two rows. `replied` requires a successful board-correct tool result plus
its durable execution arguments; remote-board replies also require a frozen
outbound intent targeting the sender's original thread. A model assertion,
error result, or post to another thread is classified as `absorbed` or
`publication-unverified`, never `replied`. This does **not** yet cover direct
peer receipts or failed-root retry/disposition. Complete ambiguous-outcome
recovery remains unported.
At that earlier milestone, no direct peer-root cutover had been made. That
statement is superseded by the source cutover above; this is still not an
autonomous peer-publication qualification.
An interrupted fleet tool can now be retried from its durable tool intent
with the same operation key. The fleet adapter journals a
`peer-board-publication-intent` containing the **resolved exact outbound
request before HTTP transmission**. This removes the earlier implicit-thread
ambiguity: a retry reuses the frozen target even if the remembered peer
thread has changed. A conflicting use of the same operation ID is refused.
The recovered tool result is journaled before the root proceeds. The rejected
alternative was retrying against whatever outbound thread happened to be
current after restart. Synthetic tests cover a timeout after peer acceptance,
thread-pointer change, identical resend, operation-ID conflict, and surviving
the SQLite authority reopen. Publication settlement still cannot infer
`replied` from model prose or an error tool result. The reconciliation pass
first indexes completed, settled and consumed roots in one traversal; it
inspects detailed publication evidence for only one unsettled candidate per
quiet step rather than rescanning history for every past stimulus. The SQLite
fixture verifies durable disposition, interrupted consumption repair, and
idempotence. Synthetic tool fixtures verify local/remote board correctness.
After this recovery sub-slice, the 2026-09-23 offline ASDF load and wrap-chain
completeness (1/1) pass; the complete isolated Lisp run passes 238/238 suites
with zero failed or unqualified. The full run initially exposed stale fixture
expectations for capacity, lifecycle reads, and derived schema version, plus
cross-process scratch residue in activity, replay, and JSONL fixtures. Those
fixtures were corrected and the affected suites passed independently before
the complete clean run. The census reference is regenerated from v19 after
adding the outbox, publication-intent, and disposition events. These are
source checks, not a private runtime canary or deployment qualification.

Development anchors: `recursive-mind-runtime.lisp` at
`%recursive-reconcile-root-abandoned-provider-request` (~8653),
`%recursive-settle-peer-message` (~9086), and
`%recursive-recover-peer-fleet-tool-outcome` (~9156). Compare
`src/adapters/web/web-fleet.lisp` (receipt acceptance ~394, notification
acceptance ~486, reply ~633, outbox flush ~687, post ~722),
`src/mind/fleet/board.lisp`, `peer-receipt.lisp`, and census events for
disposition/retry/outbox. Retain public-only read-only tool outcome recovery
if it is compatible; decide explicitly rather than overwriting it.

Port idempotent operation IDs and durable attempted/succeeded/uncertain
outcomes, terminal versus retryable dispositions, sender outbox delivery,
and recovery after provider/tool interruption. A model saying it replied is
not proof of a board write. Candidate tests: development/public
`fleet-board-tests.lisp`, `fleet-receipt-tests.lisp`,
`conscious-recursive-mind-runtime-tests.lisp`, plus new synthetic crash
between publication and result recording.

Exit evidence: no duplicate posts on restart/retry, no false `replied`
disposition, recoverable ambiguous outcomes, eventual retry where appropriate,
and durable terminal consumption. No live peer required for public fixtures.

## S4 — Quiet opportunity and capability selection

Development anchors: `recursive-mind-runtime.lisp` at
`recursive-stimulus-capabilities` and `%recursive-private-opportunity`
(~10970). Compare the public `recursive-environment-observation-available-p`,
`%recursive-pending-private-stimuli`, and
`%recursive-last-private-opportunity`. Development and public selection seams
have different inputs: `(candidates events)` versus `(candidates previous)`.

Reconcile bounded candidate selection, fairness/pressure evidence, available
tool guidance, and generic private work without a message-specific cron or
English-word validator. Document why any public-only policy is retained.
Test with synthetic user, peer, environmental, and quiet opportunities;
verify no starvation, repeated declaration-only loop, or unbounded tool set.
Do not represent this as proof of intrinsic curiosity; it qualifies only
the machinery that exposes opportunities to the model.

Graph-storage compatibility (public source candidate, 2026-09-23): private instance A's
`reviewed-context-graph-storage.lisp` is now present in the public tree with
identical normalized source content. Its three row tables are added by an
additive derived-format 4 migration; `%ccg-sync` restores and persists the
source-bound row projection, the launcher passes the derived backend through
all graph reads and formation, and graph-owner journals no longer inflate the
recursive conversation cache. The hot projector revision advances to v4 so
an older cache cannot silently omit the new ownership split. The graph
adapter fixture covers row persist/restore, changed-row writes, alias and
adjacency changes, and retrieval over a lifecycle opening without a source
packet. Offline ASDF load, focused graph adapter, recursive runtime 484/484,
derived SQLite 12/12, wrap-chain 1/1, and zero hard coupling back-edges pass.
The stronger private-data check now passes without placing private data in
the public tree: a SQLite backup of private instance A's event and derived databases was
tested in a network-isolated disposable container running the public source.
`qualify-reviewed-graph-snapshot.lisp` verified the snapshot's source binding
and restored all 3,893 reviewed-graph records at physical position 326802
(211 entities, 354 facts). Only these aggregate counts were emitted. The
snapshot remains in private instance A's private backup area for S5 qualification and must
be removed or retained by an explicit backup decision after that work. This
proves the reviewed-graph row format and restore path against private instance A's data;
bounded normal startup, full graph synchronization parity, and the offline
rebuild path still need qualification before either private install.

Canary reconciliation decision: the public quiet-opportunity seam keeps its
generic pending-stimulus selection and ledger-derived alternation. private instance A's
private selector separately tests whether private work is ready, while the
public selector always offers `private-work` beside a pending stimulus.
Both defer actual engagement to the model; no new timer or forced board check
is introduced. This behavioral difference is recorded for live fairness and
autonomous-communication observation, not expanded into another source
change before the first canary. If it causes starvation, reproduce it in a
synthetic fixture and reconcile that focused policy in the public tree.

Board-adapter reconciliation for the private instance A canary: the public source now keeps
its durable outbox and direct-receipt behavior while porting private instance A's signed,
bounded thread-observation endpoint and registered contextual board manual.
It also ports the operator board view's thread/reply styling and UTC labels.
The obsolete generic receipt bridge remains removed. A synthetic full-system
fleet fixture exercises observation paging, missing-parent references,
authenticated endpoint classification, unknown-owner refusal, and separation
of peer content from adapter guidance. The focused fixture passes after
correcting its complete authenticated receipt data. The first full diagnostic
run reached 242/243 suites with one unqualified fleet fixture before that
correction; it is **not** a final qualification receipt. Re-run the full
suite on the frozen candidate before promotion.

## S5 — Database-backed state, startup, and memory pressure (mandatory)

Status: **not yet qualified for common private deployment**. A read-only
2026-09-23 hash comparison found different working-file bytes for every S5
candidate listed below in development and public source; this is an inventory
signal, not proof that every difference is semantic. In particular, the new
activity event is now retained in the public recursive hot projection, but an
older checkpoint created without that event type cannot prove that it contains
all earlier activity rows. Before promoting this source to an existing large
ledger, qualify checkpoint revision/migration behavior and any necessary
explicit offline rebuild; never silently full-replay during ordinary startup.
Neither private instance has been modified by this source reconciliation.

Architecture correction (2026-09-23): **the S5 destination is not a better
serialized checkpoint**. It is indexed, incrementally maintained, derived
SQLite state with durable physical cursors and source/version binding; startup
reads that state and a bounded unapplied tail. The event ledger remains the
only authority. Explicit offline replay may rebuild the derived tables, but
normal operation should not require a large replay capsule or monolithic
checkpoint at all. The checkpoint work immediately below is a transitional
compatibility/fail-closed measure for the existing implementation, not the
final state architecture.

private instance A-canary cutover contract (2026-09-23): the normal launcher must not call
`conscious-storage-restore-event-sequence`,
`conscious-storage-refresh-checkpoint`,
`%recursive-thread-events-checkpoint-restore`, or either full-replay builder
to admit an existing authority. Today `sqlite-event-authority-prepare` invokes
the conscious restore even before it installs the event port; the port's
`:projection-events` callback invokes it again, cognition restore then folds
that returned list, and startup finally refreshes the capsule. Independently,
recursive consumers call `%recursive-thread-events`, which restores or
advances a whole-generation list. Both branches must move to durable row
state before the canary. A source-bound row watermark behind the ledger may
advance only by bounded pages; missing or invalid rows fail closed and direct
the operator to an explicit offline rebuild. No normal-startup compatibility
fallback may silently deserialize the old capsule or scan the full authority.

Cutover order: (1) materialize and parity-test lifecycle, semantic, inbox,
consumption and watermark sufficient state as row families, including
out-of-order/foreign acknowledgements and imported duplicate IDs; (2) replace
the conscious `:projection-events` contract with a projection-state read seam
and update cognition restore/refresh; (3) migrate recursive global selection,
root recovery, graph source context and remaining consumers to indexed root or
global pages, with bounded live cache only for current work; (4) build both
derived generations offline from a consistent private instance A backup, verify source
bindings, restart and tail catch-up on that copy, then promote a frozen public
commit under the private runbook. Retain old checkpoint readers only as
explicit maintenance/migration entry points until deletion is separately
qualified. Exact read/write paths and parity receipts belong in this ledger as
each slice lands; this paragraph is a gate, not evidence of implementation.
The first conscious-state separation landed in source on 2026-09-23:
`conscious-state-project` accepts a complete inbox projection plus committed
pulse sequence as a matched input pair and otherwise preserves its event-fold
path. A synthetic exact-JSON parity test passes with `events=nil` on the
indexed-input branch. A partial pair or an inbox evaluated under another
time, policy, agent, or consumer context is rejected. Inbox outputs now carry
those exact context fields; focused state and inbox suites pass 70/70 and
85/85 respectively, and conscious storage projection remains 16/16.
Because the composed state JSON has changed, the transitional conscious
capsule projector revision advances from v3 to v4; an older capsule must be
rejected rather than misread as equivalent. This revision change does not
authorize normal-startup rebuilding and is not the row-state cutover.
This is a pure evaluation seam only; no durable inbox rows, lifecycle rows,
or normal-startup reader are installed yet. The first durable scalar input,
`pai_conscious_pulse_v1`, is now an explicitly prepared derived row keyed by
agent and bound to the event source and physical frontier. It accumulates the
maximum valid `pulse-committed` sequence through bounded, filtered authority
pages; filtered gaps advance only to a captured frontier, concurrent writers
cannot overwrite a newer cursor, and read/restart verifies both source binding
and the scalar row's integrity digest. A mutated derived value is refused.
Synthetic parity/catch-up/tamper checks pass in the recursive-hot fixture (30/30)
and fresh-storage reopen/tail checks pass in its restart fixture (19/19).
The public source also prepared and caught up this scalar row on the
network-isolated disposable private instance A SQLite copy in one filtered page through
position 327,559; the existing recursive-hot rows remained source-bound at
that head. The probe reported counts only and did not touch private instance A's live files.
This row is **not used by normal cognition yet**; the inbox/lifecycle row
inputs and their shared frontier must land before the state seam can be cut
over without recomputation.

Historical-reference prerequisite (2026-09-23, public source only):
`storage-read-event-before-position` now reads the newest same-agent logical
event ID strictly before an exclusive physical position through the existing
`pai_events_agent_id_idx`. This is the authority lookup needed by incremental
lifecycle and semantic rows: an imported ledger may reuse an event ID, so a
plain latest-ID query could validate a reference using a future event. The
synthetic imported-history fixture proves the earlier and later duplicate
resolve differently at positions 4 and 5, that the frontier row is excluded,
and that another agent's partition cannot satisfy the reference. The focused
SQLite restart suite passes 23/23, offline ASDF load exits zero, and wrap-chain
completeness passes 1/1. This adds no durable lifecycle/semantic row yet and
does not change ordinary startup; the S5 canary gate remains open.
The lifecycle projector now exposes an ordered single-event fold step with an
external source-reference resolver and a mode that retains no seen-ID map.
The existing full-sequence entry point delegates to the same step, and a
synthetic streaming-resolver parity check passes in the focused lifecycle
suite (13/13). This makes the authority lookup usable by a row-backed
projector without duplicating Q5 transition policy, but the fold state has
not yet been persisted and the startup consumer still replays history.
The first durable lifecycle shadow is now implemented in public source as
`pai_lifecycle_v1_*` SQLite row families: per-lifecycle current rows, consumed
request IDs, invalid physical-position receipts, and a source-bound physical
cursor with row-family counts. It applies bounded authority pages in one
derived transaction, resolves source references at each exclusive physical
frontier, checks row digests and family cardinalities, and pins report reads
to a single SQLite snapshot. A pure Q5 step callback supplies transition
policy, preserving the adapter/cognitive seam. The isolated shadow suite
passes 12/12: full-fold parity, fresh storage reopen, bounded tail catch-up,
injected post-mutation rollback/retry, request-row tamper refusal, verified imported duplicate
IDs, and a future-reference rejection. Existing cognitive startup **does not
read these rows yet**; semantic and inbox state plus recursive consumer
cutovers are still required before any checkpoint-free canary claim.
The guarded, network-isolated offline probe on the existing disposable private instance A
SQLite backup reached physical position 327,559 in 2,115 bounded pages and
172,508 ms. The resulting cursor has highest logical ID 310,568, matching
the independent authority aggregate. It contains zero lifecycle/request/
invalid rows because that partition has zero `conscious-lifecycle%` events
(270,713 partition rows; 77,988 verified envelopes). This confirms build
completion and the expected empty row family on that specific backup, **not**
non-empty lifecycle parity against private instance A, a fresh-process row-reader restart,
or a live cutover. No live private instance A database was opened by the probe.
private instance A's runtime environment leaves `PAI_COGNITION_RUNTIME` unset, selecting
the default `:auto` cognition descriptor. The event authority now has an
explicit ledger-only opening mode in which conscious capsule copy/build/
restore is skipped and projection access fails closed. A synthetic one-event
database with a
checkpoint-free derived database opens twice without creating a conscious
checkpoint; verified replay still works and attempts to request the disabled
projection/refresh are refused (authority suite 39/39). The launcher contract
suite passes 18/18. A separate fresh, network-isolated process opened the
disposable private instance A backup in ledger-only mode at physical head 327,559 and
verified that conscious checkpoint metadata was unchanged; measured authority
open plus checks took 92 ms **after** the cold source/dependency load.
Crucial integration finding: private instance A's recursive conversation path calls
`%conscious-runtime-events` and `%conscious-runtime-install-projections` in
`%recursive-open-model-context` even when cognition is selected as `:auto`.
Therefore ledger-only mode is **not selected by the canonical launcher**:
doing so would fail the first model-context assembly. The attempted launcher
opt-in was removed before promotion. Conscious and recursive row inputs must
be cut over together at that consumer; the current default startup still
restores and refreshes the conscious checkpoint. No live private instance A startup has been
run on this source.
The same disposable authority partition contains 26 `peer-message-received`,
136 `stimulus-consumed`, 63 `conscious-curiosity-observed`, and 26
`conscious-curiosity-satisfaction-observed` rows, alongside zero Q5 lifecycle
events. These are aggregate type counts only. They show why an empty lifecycle
shadow cannot substitute for an inbox/consumption projection in private instance A's
context assembly. The next row cutover must preserve ordered stimulus and
consumption semantics and be tested with those nonempty event families.
An additional immutable, read-only SQL aggregate over that disposable backup
found 1,663 `user-message` rows, 2,312 `agent-message` rows, and 10,509/
10,483 `model-request`/`model-response` rows. In contrast, routine journal
families include 86,671 `heap-health` and 20,593 `memory-baseline-edge` rows.
This makes a filtered, source-bound attention input potentially much smaller
than a whole-ledger replay, but merely querying all historic admitted types
on every turn would still grow without bound and would not solve the many
recursive callers of `%recursive-thread-events`. The row cutover therefore
needs durable per-stimulus consumption/eligibility facts and bounded root or
activity readers, with a retained exact-ledger read for explicit recovery.
Rejected shortcut: enabling the existing ledger-only authority flag for the
`:auto` launcher now. It opens quickly but fails at first context assembly.

Read-only private instance A audit: private instance A already stores some derived state in row-oriented
tables (memory projection/applied events, knowledge/reviewed graph records,
working-context summaries, and activity storage). However, its recursive
hot thread still restores `recursive-thread-hot-v1` from a serialized events
checkpoint and fails ordinary startup above the replay threshold when that
checkpoint is absent or stale. Its conscious projection also still builds a
capsule checkpoint and restores a prefix plus tail. Thus private instance A made partial
progress toward database-backed state, but **has not removed checkpoint
dependence from core startup**. Do not copy those checkpoint paths as the
target design. Reconcile the row-oriented pieces and replace the remaining
whole-state dependencies consumer by consumer, with parity tests against
ledger replay and explicit recovery drills.

Consumer inventory: the conscious-state checkpoint in
`storage-projection.lisp` still collects and serializes a compacted `events`
vector; restore reconstructs that prefix and refresh rewrites it. The
canonical launcher calls that refresh during startup. The recursive hot
checkpoint likewise serializes its event list, and tail advancement copies
the list. A fail-closed launcher flag prevents an *unbounded rebuild from raw
authority* but does not eliminate this checkpoint read/rewrite cost. The
selected conscious runtime also calls `conscious-state-project` over the
restored event list at restore and later projection boundaries in
`runtime.lisp`; detaching the list afterward lowers retained heap but does
not make the computation incremental. This is a larger S5 gap than checkpoint
publication alone. The first replacement slice should be a shadow,
row-oriented recursive-hot
projection keyed by physical storage position with a source-bound watermark;
compare root-scoped pages and settlement compaction against the current
projection before changing any live consumer. Conscious state needs a
separate incremental-fold design. Its path is ordered ledger events to
lifecycle and semantic state, then inbox/concern inputs, then time- and
policy-dependent attention/state decisions. A rendered state snapshot is
not sufficient fold state: ordered references, consumed stimulus IDs,
watermark gaps, receipt validation, and prior semantic revisions must
survive. Start that branch with a shadow lifecycle projector that persists
rows plus a physical cursor in one transaction and compares against the
existing full fold across out-of-order receipts, duplicate requests,
restart, and interrupted writes. Leave live consumers on the old path until
parity is established. Do not claim checkpoint-free operation
until both consumers have been cut over and restart parity is demonstrated.

Checkpoint-compatibility sub-slice (2026-09-23, public source only): the
recursive hot projector revision was `recursive-thread-hot-v2` at this stage;
the direct-receipt cutover later advanced it to `recursive-thread-hot-v3`
because the retained type set changed again; the reviewed-graph owner split
now advances it to `recursive-thread-hot-v4`. The prior
`v1` revision was shared by development and public source even though their
retained event sets differed; its policy label therefore cannot certify
activity or direct-peer completeness. Restore rejects a `v1` checkpoint
before accepting its source seal. The focused recursive suite passes 471/471,
including a synthetic old checkpoint with a matching policy label, and the
checkpoint-pressure suite passes 8/8. The explicit offline rebuild script
now refuses to report recursive success unless a current, source-bound
checkpoint at the rebuilt head is readable; its 15 top-level forms parse
without executing a rebuild. Offline ASDF load and wrap-chain completeness
pass; coupling reports zero hard back-edges. A broader isolated run was
started but intentionally stopped after passing early suites; it is **not**
a full qualification result. This is a fail-closed compatibility
gate, **not** a completed S5 database-state migration or an instance rollout.
The checkpoint-pressure suite now also uses a real synthetic SQLite authority:
with a three-event ledger and a deliberately lowered replay threshold, it
checks ordinary refusal, explicit maintenance publication, fresh-generation
restore, and an appended tail. This proves the transition behavior, not
production-sized memory or startup bounds.

Launcher reconciliation (2026-09-23, public source only): private instance A's live
`scripts/conscious-conversation.lisp` passes
`:rebuild-stale-checkpoint-p nil`, whereas the public launcher had passed `t`.
The public launcher now matches private instance A's fail-closed ordinary-startup policy.
Explicit initialization/migration and the offline rebuild entry point remain
separate authorized paths. The missing-derived-database and stale-composition
branches in `sqlite-event-authority-prepare` now have focused synthetic tests.
The public legacy-checkpoint relocation branch is also now gated by explicit
rebuild authority; synthetic tests prove ordinary refusal leaves no derived
checkpoint and authorized relocation works. A missing derived database now
fails **before** creating an empty replacement on ordinary startup; otherwise
that failed attempt made the later explicit repair look like an interrupted
migration. The authority suite passes 33/33, including refusal, no-file-left,
and explicit rebuild. This is
not evidence that all automatic rebuild paths have been eliminated.
The canonical launcher flag is guarded by an interaction-runtime source
regression check (18/18).

Streaming integrity hash port (2026-09-23, public source only): reconciled
private instance A's chunked UTF-8 checkpoint digest into `storage-substrate.lisp`,
`sqlite-storage.lisp`, and `sqlite-derived-storage.lisp` without changing the
stored hash format. A synthetic long/Unicode parity fixture and a published
on-disk hash check pass (storage substrate 33/33; derived memory 12/12).
This removes one avoidable full-string allocation during publication/load,
but does not remove the checkpoint dependency or prove a material reduction
in the measured full-build peak. Repeating the 59 MiB synthetic fixture after
the port measured 4,708 ms and the **same** 853,079,792 allocated bytes and
723,501,824 peak-growth bytes for full build; warm tail was 12 ms and
1,952,128 peak-growth bytes. Timing/heap sampling varies between runs, but
the unchanged full-build allocation reinforces that the dominant cost is
elsewhere (stream/decode/projection materialization), not just hash input.
Heap-health reconciliation (2026-09-23, public source only): matched private instance A's
observational default—routine samples no longer request full copying GC
while a large replay generation may be live, and the critical autonomy guard
is 65% of dynamic space. Explicit diagnostic GC remains opt-in. Synthetic
heap-health checks pass 18/18. This reduces a known crash trigger but is not
a substitute for reducing peak live data.
Shadow recursive-hot storage (2026-09-23, public source only): an additive,
explicitly prepared SQLite adapter now stores selected compacted rows by
physical position with an atomic source-bound watermark. It applies bounded
authority pages and exposes bounded position/root reads. The v2 shadow
schema adds contiguous selected and per-root ordinals with sealed counts;
the prior v1 shadow tables are left untouched and cannot be silently reused.
Its 43/43 synthetic checks cover empty source, `NULL` roots, idempotent tail,
source/revision refusal, row tamper, first/middle/final/sole selected-row
deletion, rollback before watermark publication, and indexed root/type
presence-query bounds and tamper refusal. It is **not
installed in the runtime** and does not yet replace checkpoint reads. The
ordinal checks detect missing rows in the requested bounded slice, not
whole-database tampering or policy parity. Require exact active/settled/
protected-root parity before any reader cutover. Shadow preparation is
explicit and has not been incorporated into the versioned derived-database
first-run/migration contract; that must be qualified before live installation.
An initial real-SQLite selection fixture passes 18/18: it compares shadow
event IDs to the existing recursive projection for an unsettled synthetic
root, including graph-owned provider IO exclusion and activity retention.
It also proves the present **settlement divergence**: a terminal agent
message makes the old projection compact earlier provider payloads, while
the append-only shadow still exposes them. This is a cutover gate, not a
tolerated live difference. The fixture additionally verifies the indexed
terminal/protection lookups and shows that the existing pure compaction
function can reproduce the settled response shape from a root-scoped read,
while graph-protected roots preserve exact provider evidence in both paths.
The fixture derives settled/protected facts from the authority index and
compares full selected event payloads for active, settled, and
graph-protected roots after bounded read-time compaction, not just IDs or
markers. A separate 16/16 synthetic suite now verifies fresh-process reopen,
bounded tail application, ledger-only rebuild into a new derived database,
filtered-only cursor advancement, and a legacy import with rewound and
duplicate event IDs. These are small
fixtures, not a live reader cutover or a large-ledger restart benchmark.
The shadow writer now accepts an optional bounded event-type filter. With
that filter, SQLite excludes unrelated event bodies before JSON decoding;
the writer captures an authority frontier and advances the physical cursor
across filtered gaps only when a page has fewer selected rows than its limit.
Parity and restart fixtures use the recursive event vocabulary and make the
selector reject an unrelated event if it is accidentally decoded. This
reduces an avoidable cold-path cost but does not make the full conscious
state or recursive runtime checkpoint-free, nor does it audit every skipped
ledger row's payload integrity. The shadow page/root read now pins its
watermark, root count, and rows in one derived SQLite read transaction and
checks the source binding before returning. The three focused recursive-hot
suites pass after that change. Cross-process writer/read stress remains a
specific cutover gate; a source-only transaction change is not itself a
long-running concurrency qualification.
An offline build against private instance A's disposable 327,559-position SQLite snapshot
completed 19,222 selected recursive rows in 131 filtered pages, about 20.5
seconds after compilation. The derived snapshot grew from about 300 MiB to
333 MiB. The initial probe exposed a real 1.46 MiB `model-response` row above
the default 1 MiB per-event shadow allowance; the supported 8 MiB cap with
128-event pages completed. This is bounded migration-writer evidence only.
Before a live reader cutover, define a derived provider-payload retention
policy, qualify imported-history recovery boundaries, and replace whole-
generation consumers with indexed root/global queries; a fast offline build
does not make ordinary startup checkpoint-free.
`conscious-recursive-hot-root-page` is now a source-bound candidate reader:
it rejects a shadow behind the authority head, reads one bounded root page,
derives settlement and graph protection from the indexed ledger, and applies
the existing provider compaction policy on read. Synthetic active, stale,
and multi-page settled parity raise the focused recursive-hot suite to 21/21.
No live consumer uses it yet; the global selection and conscious-state paths
must be cut over separately before disabling recursive/conscious checkpoints.
`conscious-recursive-hot-page` adds a bounded, globally ordered companion.
It checks a current source-bound shadow watermark, derives settlement and
graph protection only for provider roots in the page from the indexed
authority, and refuses a source head that advances during the read. Exact
synthetic parity across active, settled, and graph-protected rows and a
filtered-only stale-physical-cursor refusal pass in the 23-check parity suite.
Neither candidate page reader is installed in a normal-runtime consumer;
conscious state still requires its own sufficient-state row migration.
First narrow consumer cutover (2026-09-23): failed-root duplicate suppression
now uses a source-bound indexed authority witness when SQLite's explicitly
prepared root index exists. It asks for the newest `recursive-root-failed`
receipt and does not load the whole recursive generation. An indexed read
error propagates; an absent index retains the prior unkeyed/list route until
offline preparation. The activity storage fixture proves readiness detection
and newest-row semantics (57/57); the SQLite recursive fixture disables the
whole-generation reader and still finds the latest receipt (24/24), while
the non-SQLite recursive runtime suite remains 484/484. A network-isolated
probe of the disposable private instance A authority copy reported that its root index was
already present; no preparation cost can be inferred from that zero-work
run. This removes one whole-list call on prepared SQLite instances, **not**
the recursive checkpoint dependency for normal startup.
The SQLite lookup is installed as a registered layer on the cognitive
`recursive-root-failure-receipt` seam, so the recursive mind has no direct
adapter-variable dependency; the 24-check parity fixture exercises that
layer and the coupling report remains at zero hard back-edges.
The preferred next design is root-scoped settlement/protection facts with
bounded read-time compaction (or an equivalently bounded materialized
update), so a terminal event does not force rewriting unrelated rows or a
whole-history projection. Verify active, settled, and graph-protected roots
with exact payload parity before changing readers.
The shadow event-type presence query is diagnostic only: a deleted lone
terminal or protection row could make a negative answer false. Do not use
its negative result for live disposition. A narrower authority-backed route
is now source-implemented: `storage-prepare-activity-index` already defines an opt-in
SQLite expression index on agent, `caused_by`, event type, and physical
sequence. The disposable `activity-storage-tests.lisp` suite passes 54/54,
including `EXPLAIN QUERY PLAN` evidence that SQLite uses this index. No new
ledger column or event backfill is needed. The generic
`storage-root-has-event-type-p` lookup accepts an agent/root, bounded type
set, and inclusive physical frontier or source-boundary seal. It reads one
SQLite snapshot, verifies a positive witness against the ledger hash and
indexed fields, and fails closed if the index is missing even for a negative
answer. Tests cover late append, partition, legacy import and tampering.
The recursive-hot fixture also checks frozen pre-settlement and protected-root
authority lookups. No live recursive reader uses the new lookup yet.
Index preparation remains explicit maintenance;
its scan cost on an existing private ledger needs an offline migration window
and measurement. A negative authority answer still assumes the authoritative
ledger itself has not been damaged; source-integrity audit remains necessary.
Read-time compaction would avoid a whole-root rewrite, but the shadow table
would still retain the unredacted provider payload on disk. Before live use,
decide and test a root-scoped retention/compaction policy for derived rows;
the authoritative ledger alone must retain the original forensic evidence.
Integrated source still passes offline ASDF load, wrap-chain completeness
(1/1), coupling with zero hard back-edges, and `git diff --check`. The full
fresh-process suite has **not** been rerun to completion on this combined
working tree; no commit or private promotion is authorized by these focused
results alone. These checks were repeated after adding the shadow adapter.
They were repeated again after the indexed root/type query and parity fixture:
offline ASDF load and wrap-chain completeness still pass; the focused shadow
and parity suites passed 43/43 and 18/18 at that earlier milestone; the
parity suite now passes 21/21 after the root-page reader checks. The authoritative
activity-storage suite passes 54/54; and the fresh-process restart/import
suite passes 16/16. Full release qualification is still pending; a later
combined-source isolated run passed 241/241 after the direct-receipt cutover,
but no exact commit/private migration has been qualified. The qualification
inventory currently discovers 241 Lisp suites (240 default-profile, one
override), with no blocked profiles; this
inventory is not a substitute for executing the full suite.

Measured baseline (2026-09-23, synthetic fixture, 59 MiB journal payload):
`conscious-storage-large-ledger-tests.lisp` passes 3/3. Its full checkpoint
build took 7,928 ms, allocated 853,079,792 bytes and sampled 723,501,824
bytes of peak heap growth. Restoring one appended tail event took 12 ms,
allocated 2,964,800 bytes and sampled 2,835,616 bytes of peak growth. These
are single-run measurements in the disposable test container, not guarantees
for private instance A or private instance B. The full-build cost remains an S5 pressure issue even
though the warm tail path is small. The SQLite authority suite passes 29/29.

S5 work ledger (public source candidate; no private promotion):

| Work item | State | Evidence / next gate |
| --- | --- | --- |
| Recursive checkpoint revision and fail-closed restore | Transitional source-qualified, uncommitted | v1 refusal 471/471; real SQLite lowered-threshold rebuild/restore/tail fixture passes. This must not become the long-term hot-state substrate. |
| Live launcher must not authorize stale-checkpoint rebuild | Source-qualified, uncommitted | Reconciled to private instance A's `nil` flag; launcher regression 18/18, authority refusal/no-file/explicit repair 33/33. |
| Ledger-only authority opening seam | Tested source capability, not selected by launcher | Existing SQLite authority opens twice without capsule build/restore and projection access fails closed (authority 39/39). A fresh offline process opened the disposable private instance A copy at head 327,559 without changing checkpoint metadata in 92 ms after load. Recursive conversation still requires the conscious projection even under `:auto`; launcher opt-in was removed after tracing `%recursive-open-model-context`. Cut over that consumer before enabling ledger-only startup. |
| Offline rebuild publishes a current sealed checkpoint | Source change, uncommitted | Script now checks revision/head/binding; syntax-read passed. Execute on large disposable ledger and verify warm restart. |
| Streaming integrity hash from private instance A | Source-qualified, uncommitted | Legacy digest parity passes 33/33; derived memory 12/12. Benchmark before claiming lower heap. |
| Embedding input byte envelope from private instance A | Source-qualified, uncommitted | Single and batch local embedding requests cap UTF-8 input without splitting characters; synthetic lab suite 63/63. This is a reliability port, not a checkpoint solution. |
| Heap-health default from private instance A | Source-qualified, uncommitted | Routine full GC disabled; 65% critical pause; synthetic checks 18/18. Long-running canary still required. |
| Bounded authority reads and durable projection updates | Partial | First failed-root duplicate-suppression lookup is source-bound and indexed when the root index exists; missing-index fallback still uses the old generation. Audit remaining global consumers, source-binding checks, partial writes, and warm tail. |
| Replace monolithic cognitive/recursive checkpoints with indexed derived rows | Open, primary S5 goal | Define per-consumer sufficient state and cursor; transact affected-row updates with each admitted event; verify parity, restart, interruption, and deliberate full rebuild. |
| Source-bound attention event rows and conscious-state candidate | Additive candidate, no runtime cutover | `sqlite-attention-shadow.lisp` persists only policy-selected inbox inputs in bounded transactions, including each selected event's physical predecessor ID so neutral journal gaps reconstruct the exact watermark. The v3 selector binds the admitted type set to a revision and compacts authorized reply and causal tool-call envelopes. `conscious-storage-indexed-state` joins current attention, lifecycle, and committed-pulse rows only when all three cursors equal the same authority head; it refuses stale or over-bound inputs. An earlier unique root is supported only when a filtered authority tail proves no lifecycle or pulse mutation after it; otherwise the read fails closed. The attention reader recovers the true maximum logical ID despite imported rewinds. Synthetic parity/reopen/tail/stale-cursor/deleted-row, pre-emptive acknowledgement, three authorized reply shapes, causal tool results, imported duplicate-ID/rewind, indexed current/as-of conscious-state parity, and cross-family staleness checks pass 37/37. This remains a historical selected-event index, **not** the final bounded active-stimulus/consumption state: reading all selected rows per turn can still grow without bound. It does not supply Q5 semantic descriptors, conversation context, or recursive checkpoint replacement. Do not enable ledger-only startup on this evidence alone. |

The network-isolated offline v1 attention build on the disposable private instance A backup
completed through physical position 327,559 in 2,115 bounded pages and
88,988 ms after source load. It selected 3,450 rows totaling 4,142,819
JSON bytes (largest selected row 112,499 bytes). The v2 build selected 5,108
rows in 81,430 ms. The v3 build, now including causal tool-call identities,
selected 5,910 rows in 59,097 ms. These are additive derived rows on a
disposable backup only, **not** a bounded-lifetime consumption projection or
live readiness. The
separate recursive hot shadow contains 19,222 rows / 132,554,549 JSON bytes
on the same backup; globally materializing that entire selection in Lisp on
each turn would still be a material heap risk. Root/activity-scoped consumer
reads remain essential before the canary.
An indexed-state read of that v1 snapshot then opened without ledger replay:
the current-head projection took 3,036 ms on its first isolated run and
584 ms on a subsequent fresh isolated run; the latest user-root as-of read
took 676/500 ms. It exposed **2,544 admitted barriers against a hard bound
of 256**, 110 consumed, and `degraded=yes` with watermark zero. This is a
real behavioral canary blocker, not merely a startup-performance issue.
Read-only aggregate SQL found 1,621 of 1,663 historical `user-message`
logical IDs had an earlier-causal, durable `agent-message`, but the old inbox
rule only recognized Q4.5 publication envelopes. The source now recognizes
the actual typed AUTO-TURN final/fallback and recursive-solicited public
reply shapes as event-derived completion, while refusing tool-bearing drafts
and private findings. It also uses durable tool-call-to-user-root causality
to settle only tool results that preceded the public reply; later results
stay pending. The selector and shadow table were versioned to v3 so older
rows cannot be silently adopted. A subtle imported-shape mismatch persisted:
the compacted legacy reply expressed an absent agent partition as JSON null,
which the inbox initially did not recognize. After explicitly accepting that
null only for the unversioned public-reply shape, the disposable v3 read
reported 188 admitted barriers (below the hard bound of 256), 2,466 consumed,
579 rejected, `degraded=no`, and watermark 820; the current read took 724 ms
and latest user-root as-of read 560 ms after source load. An initial
diagnostic-script edit caused an exit failure after printing both reads; after
repair, a fresh network-isolated process exited zero with the same counts,
704 ms current and 624 ms as-of read times. The 188 admitted items break down
as 49 `user-message`, 53 `operator-control`, 59 `tool-result`, 26
`environment-change`, and one `runtime-health` (aggregate only, no private
content). **Below the hard bound is not the same as safe historical
disposition:** before runtime cutover, prove why each remaining class is
pending and that migration will not re-enact stale controls or tool effects.
The focused attention, inbox, and checkpoint-projection suites pass 37/37,
85/85, and 16/16 in separate fresh processes. Offline ASDF load and
wrap-chain completeness (1/1) pass after this fix; `git diff --check` is
clean. These are focused source gates, not the full frozen-candidate run.
No live ledger was changed, and
normal runtime still does not consume the indexed state.
The next source-only S5 slice adds `conscious-storage-refresh-indexed` as an
incremental catch-up operation for already-prepared attention, lifecycle,
and pulse rows. It captures the authority head, caps page size and page count,
refuses missing row families instead of performing an implicit historical
build, and leaves an interrupted mixed-head set unreadable until the next
refresh. Synthetic tests cover missing preparation, over-limit tail,
partial-state refusal, and resumed catch-up; the attention suite is now
41/41. This is a maintenance primitive, **not** an installed launcher or
turn consumer. Semantic lifecycle descriptors, bounded conversation/context
readers, historical pending-item disposition, and recursive-hot consumer
cutover remain prerequisites to checkpoint-free normal startup.
An additional event-authority read port now exposes bounded, root-scoped
recent children through the already-prepared causal index. SQLite refuses a
missing index, ambiguous imported event ID, or misordered root/boundary;
there is no installed-authority fallback to whole replay. The context
assembler's recent tool-receipt section uses this port when installed,
hydrating only source IDs already selected for conversation evidence and at
most eight receipts for each of two eligible roots. It reuses the pure
same-agent/persona/channel/time filter before reading receipts. The input
bound matches the largest configured 128-event history profile; an initial
64-event cap was corrected after inspecting that profile. Isolated activity,
authority, and recursive-runtime fixtures pass 61/61, 41/41, and 487/487;
offline load, wrap-chain completeness (1/1), zero hard back-edges,
and `git diff --check` pass. **This is one actual context-section consumer
cutover, not the whole context or startup cutover:** `%recursive-open-model-context`
still reads the recursive generation for episodic/private-cognition context
and conscious projections. Those remaining readers must be migrated before
selecting ledger-only startup or claiming a private instance A canary.
On the same network-isolated disposable private instance A backup, aggregate authority SQL
found 191 `conversation-episode-sealed` events (latest physical position
326,641) and no user/agent dialogue after that latest seal, plus 110 private
curiosity-focus openings, 39 result events and 39 incorporation completions.
The derived backup has no episode-row tables or episode-graph checkpoint.
This makes an offline episode-row migration necessary; using only the latest
seal as a raw-dialogue frontier would be lossy because older unsealed pairs
can remain relevant. Do not silently replace the current episodic projector
with a tail-only read. These are counts and positions only; no private
message content was copied into this tree.
| Shadow recursive-hot rows | Source-qualified, uncommitted; no live reader | Additive v2 adapter and ASDF registration; 43/43 synthetic checks cover bounded pages/root/type reads, source-bound cursor, idempotent tail, deletion gaps, rollback and tamper refusal. Optional type-filtered source scan skips unrelated JSON bodies while preserving a sealed physical cursor. Root/global-page compaction/authority parity 23/23 for active, stale, settled, protected, and filtered-tail paths; fresh-process restart/legacy-import suite 16/16. A disposable private instance A-snapshot build selected 19,222 rows through position 327,559 in 20.5 seconds after raising the bounded per-event cap to accommodate a 1.46 MiB provider row. Retention policy, imported-history parity, migration contract, consumer cutover, and live reader cutover remain open. |
| Reviewed context-graph rows from private instance A | Source-qualified, uncommitted; no private install | Generic row module and additive derived-format 4 schema match private instance A's storage shape; graph read/write ports and launcher use the derived backend. Synthetic row restore/update and lifecycle-without-source tests pass. A disposable, consistent copy of private instance A's existing SQLite databases passed source-bound restoration of 3,893 reviewed rows (211 entities, 354 facts) at position 326,802 using public code; no private live source was modified. Full graph-sync tail parity, deliberate offline rebuild, and large-ledger normal-startup canary remain open. |
| Authoritative indexed root/type disposition lookup | Source-qualified, uncommitted; one guarded runtime consumer | Existing opt-in activity index; generic bounded source-boundary/physical-frontier lookup with verified positive witness and missing-index refusal. New non-mutating readiness check and newest-witness option let failed-root duplicate suppression avoid the hot generation on prepared SQLite instances. Focused storage 57/57, parity 24/24, recursive runtime 484/484. The disposable private instance A snapshot already had the index, so index-build timing is still unknown; private instance B readiness is unverified. |
| Shadow conscious lifecycle sufficient-state rows | Source shadow implemented; no live reader | Per-lifecycle, request, invalid and source-bound cursor rows with bounded transactional pages; parity/reopen/tail/post-mutation rollback/tamper/import checks 12/12. Position-exclusive source lookup handles duplicate IDs (SQLite restart 23/23); pure incremental fold 13/13, semantics 15/15, state 70/70. Disposable private instance A copy reached position 327,559 in 2,115 pages/172.5 s, with highest ID matching authority; zero lifecycle events mean non-empty parity is synthetic only. Fresh-process row-reader restart and cognitive read cutover remain open. |
| Full S5 qualification and private rollout | Open | Record cold/warm times and peak heap, full isolated suites, public commit, then separate private instance A and private instance B backup/canary receipts. |
An existing large ledger needs an explicitly provisioned offline checkpoint
rebuild before normal startup on this revision; the ordinary path must not
silently replay it. Rejected alternative: accept `v1` after an activity-only
spot check, because that would not prove the remainder of the retained event
set. Still to qualify: offline rebuild and current v4 restore on a large synthetic
ledger, bounded incremental startup/update, memory and restart measurements,
and each private ledger's migration receipt.

Compare `src/kernel/heap-health.lisp`, `storage-substrate.lisp`,
`src/adapters/sqlite/sqlite-storage.lisp`, `sqlite-event-authority.lisp`,
`sqlite-derived-storage.lisp`, `sqlite-activity-storage.lisp`,
`src/mind/memory/memory-nodes.lisp`, `recursive-mind-runtime.lisp`,
`scripts/rebuild-recursive-thread-checkpoint.lisp`, and `pai.asd`.
This slice is a **required implementation/reconciliation gate**, not just a
performance review. Make current operational state a bounded, indexed
database read/update path. On startup, load persisted state and apply only a
bounded/unapplied tail when necessary. On each event, update the relevant
durable projections incrementally; do not reconstruct the agent's whole
state or serialize/reload a giant in-memory checkpoint for ordinary turns.
Full ledger replay remains an explicit repair, migration, or verification
operation with a separate resource budget. Keep all derived rows rebuildable
from the authoritative event ledger and validate their source/version seals.

The development source is **not already at this target**: in
`sqlite-event-authority.lisp`, `%sqlite-authority-copy-legacy-checkpoint`
(~185) and `sqlite-event-authority-prepare` (~208–315) still call
`conscious-storage-build-checkpoint` under migration/rebuild conditions.
Inspect those branches and distinguish legitimate one-time migration from
accidental ordinary-startup rebuild. Also inspect `%sqlite-authority-map`
(~41) for unbounded generation reads. A simple copy of development files
will not satisfy this slice. Check query bounds, transaction behavior, and
heap pressure during prolonged activities.
Candidate tests: `recursive-checkpoint-pressure-tests.lisp`,
`sqlite-bounded-retrieval-tests.lisp`, `activity-storage-tests.lisp`,
`storage-substrate-tests.lisp`, and large-ledger projection tests.

Exit evidence: repeat cold and warm starts with a large synthetic ledger show
no full replay or whole-state rebuild on the ordinary path; bounded query,
startup time, and peak-memory measurements are recorded; an appended event
updates only affected durable state; interrupted projection writes recover
without silent divergence; derived data can be deliberately deleted/rebuilt;
and restart preserves activity continuity. Do not claim this solves every
historic heap exhaustion until a long-running canary confirms it.

## S6 — User-visible board, graph, and launch integration (after core)

Compare `src/adapters/web/web-fleet.lisp`, `web-terminal.lisp`,
`assets/observability.js`, `scripts/conscious-conversation.lisp`, CLI
launchers, `context-graph-runtime-adapter.lisp`, and related tests only for
behavior dependent on S1–S5: thread order/unread/timestamps, notification
visibility, activity preview, bounded graph search, and launch wiring.
These are **conditional** migrations, not permission to import private UI
state or unrelated graph experiments. The development and public graph files
differ, while graph budget and search-tool files are identical; isolate
actual behavioral deltas before editing. Exit evidence: isolated web/graph
tests plus manual local smoke using synthetic or disposable state.

## Final gate and deployment boundary

### First private-instance canary readiness — 2026-09-23

**Canary scope decision (operator, after the source-only S5 work):** keep the
existing checkpoint-dependent normal startup for the private instance A canary.
Checkpoint elimination, ledger-only launcher selection, episode-row migration,
and further shadow-state consumer cutovers are deferred until *after* source
reconciliation and first-instance behavior are assessed. This supersedes the
checkpoint-free prerequisite in the older critical path below; it does not
claim that the longer-term S5 target is complete. The canary must prove that
the exact candidate can restore private instance A's source-bound conscious and recursive
checkpoints without an implicit full replay or normal-start rebuild. A missing
or stale checkpoint fails closed; an explicitly authorized offline rebuild on
a disposable backup may be qualified before touching the live instance.
No new checkpoint-removal source work belongs on the canary critical path.
private instance A's running container mounts a persistent `pai-source` volume at
`/workspace`; changing the private checkout alone does not update that volume, and
rebuilding its image does not replace the volume. The private promotion tool
copies reviewed commit blobs into the private checkout but does not stop,
restart, or update the runtime volume. Treat checkout promotion and
stopped-instance volume deployment as two distinct, separately verified
operations, each with file hashes and rollback evidence. Never infer a live
canary from a checkout diff or a successful promotion receipt alone.
Read-only comparison found that private instance A's `pai-source` volume hashes match its
private checkout for `pai.asd`, `recursive-mind-runtime.lisp`, and
`reviewed-context-graph-storage.lisp`; all three differ from this public
working tree. Across 311 public Lisp/Python files under `src` and `scripts`,
158 currently hash-identical to the private checkout, 137 differ, and 16 are
absent there. These counts are an inventory, not an authorization to replace
137 private files. `pai.asd` additionally names public-only working-context,
experience-search, stimulus-activity, and shadow-state modules. Review the
exact canary allowlist and the target's overlapping edits before promotion.
Metadata-only inspection of the existing disposable state backup found
`conscious-runtime` at physical position 326,600 with projector revision v3
and a 4.44-million-character payload; the public candidate expects v6.
`recursive-thread-hot-projection` is at position 319,361 with revision v1
and a 73.6-million-character payload; the public candidate expects v4.
The reviewed-graph checkpoint is at position 326,802. Therefore the first
canary **requires an explicit, measured offline rebuild on a separate copy**
of private instance A's state, followed by normal-mode restore and warm restart. The
existing backup and live database were read-only during this inspection.
The explicit offline rebuild on a separate 8 GiB-capped, network-isolated
copy completed at ledger position 327,559: conscious v6 retained 3,450 of
270,713 authority events; recursive v4 retained 19,222 events and reported
about 1.22 GiB Lisp dynamic usage; reviewed graph restored 3,893 rows at its
own source-bound position 326,802. The rewritten checkpoint payloads are
approximately 3.55 million and 35.17 million characters respectively.
This demonstrates that current checkpoints can be prepared without writing
private instance A's live or original backup volumes. Fresh-process normal restore and warm
restart were then checked against that isolated copy at a 3 GiB Lisp heap:
ordinary authority prepare returned `opened` in about 532 ms and projected
3,133 conscious events plus 19,222 recursive events; a second fresh process
returned `opened` in about 536 ms with the same counts and completed reviewed
graph sync within about 4.9 seconds from process-local restore start. Neither
normal invocation set rebuild authority. These measurements exclude cold
ASDF/Quicklisp compilation and do not replace a live first-message/heap
canary; the source snapshot also predates private instance A's current live ledger head.

Provisional runtime source allowlist for private instance A (31 explicit paths; do not
replace whole directories). The `experience-search.lisp` entry is unchanged
in this public working tree but absent from private instance A and required by the updated
ASDF component list. Before promotion, compare every target hash against both
private instance A's checkout and its source volume, freeze an exact public commit, and
review all file-level differences and private-only behavior:

```text
pai.asd
scripts/conscious-conversation.lisp
scripts/rebuild-recursive-thread-checkpoint.lisp
src/adapters/sqlite/sqlite-activity-storage.lisp
src/adapters/sqlite/sqlite-attention-shadow.lisp
src/adapters/sqlite/sqlite-conscious-pulse-state.lisp
src/adapters/sqlite/sqlite-derived-storage.lisp
src/adapters/sqlite/sqlite-event-authority.lisp
src/adapters/sqlite/sqlite-lifecycle-shadow.lisp
src/adapters/sqlite/sqlite-recursive-hot-shadow.lisp
src/adapters/sqlite/sqlite-storage.lisp
src/adapters/web/web-fleet.lisp
src/kernel/activity-storage.lisp
src/kernel/event-log.lisp
src/kernel/heap-health.lisp
src/kernel/storage-substrate.lisp
src/mind/conscious/context-graph-runtime-adapter.lisp
src/mind/conscious/conversation-runtime.lisp
src/mind/conscious/event-type-census.sexp
src/mind/conscious/experience-search.lisp
src/mind/conscious/inbox.lisp
src/mind/conscious/lifecycle.lisp
src/mind/conscious/peer-receipt.lisp
src/mind/conscious/recursive-mind-runtime.lisp
src/mind/conscious/reviewed-context-graph-storage.lisp
src/mind/conscious/state.lisp
src/mind/conscious/stimulus-activity.lisp
src/mind/conscious/storage-projection.lisp
src/mind/conscious/working-context-request.lisp
src/mind/memory/memory-ledger-baseline.lisp
src/mind/memory/memory-nodes.lisp
```

Read-only checkout comparison on 2026-09-23: 24 of these 31 target paths
exist but have different bytes in private instance A; seven are absent there.
No allowlisted path is byte-identical. In particular, the recursive runtime
has large structural differences because the public tree has split several
concerns into registered modules. A promotion must use the reviewed allowlist
and an independent backup, not assume the private checkout or its mounted
source volume has already adopted these files. This inventory is not a
file-by-file behavioral approval.
Read-only hashes of the running source volume matched the private checkout for
all 24 existing allowlisted paths; the same seven paths are absent from both.
This confirms the checkout is representative for reviewing that allowlist,
but not that it is safe to overwrite or that the live state is qualified.
One reviewed non-identical behavior remains in
`context-graph-runtime-adapter.lisp`: private instance A alternates recent
coverage with historical repair for graph formation, while the public source
prefers due retry and then the first untouched batch. This is a scheduling
delta, not a projection format blocker. Do not silently claim behavior parity;
observe graph background work in the canary or make an explicit merge decision
before treating the target as fully reconciled. No scheduler source change was
made in this canary qualification pass.

At this earlier source-only checkpoint, the public tree was not yet installed
into private instance A. The later local canary receipt below supersedes this
deployment-status sentence; it does not qualify a commit or private instance B.

S2 source evidence on this working tree: intake and quiet selection now use
the authenticated receipt itself, with no new generic bridge. Exact local
content is frozen into its activity. Synthetic new/legacy ownership,
duplicate-send, old-terminal, and fail-closed old-in-flight fixtures pass in
`fleet-receipt-tests.lisp` (99 checks); stimulus 100/100, recursive runtime
484/484, and checkpoint pressure 8/8 pass as focused checks. The recursive
runtime checks include the injected board-acceptance/result-append crash,
board-store reopen, stable-key retry, and single-result/single-reply proof.
The prior source
revision passed a full isolated run of 241/241 suites, zero failed or
unqualified; that full result predates the latest safe fleet-recovery selection
change and is not a current-tree full qualification. A preliminary complete
suite attempt after the recovery change was stopped during the graph suites
to prioritize the remaining canary gaps; all suites reached had passed, but
the interrupted run is not a qualification receipt. The subsequent
fresh-process fixture changed one suite again, so full qualification must
run on the final frozen source. Offline ASDF load,
wrap-chain completeness (1/1), census-doc generation check, zero-hard-
back-edge coupling check, and the three Python profiles (16/15/5) pass.
These are source receipts; release gates must be rerun against an exact
frozen commit after remaining work.
Another full isolated run on 2026-09-23 progressed through the early graph
identity suites with no failures, then was deliberately interrupted while
S5 remained incomplete; it is also **not** a full-suite qualification.
After the candidate global-page reader was added, its focused parity suite
passed 23/23, offline ASDF load exited zero, wrap-chain completeness passed
1/1, `git diff --check` passed, and coupling reported zero hard back-edges.
Normal S5 startup remains checkpoint-dependent; neither private instance has
received this code.

Canary-scope continuity reconciliation, 2026-09-23: compared private instance A's
`conscious-conversation-history` with the public candidate. The public selector
still clipped individual messages and could omit the newest complete exchange;
the second assembly budget could then discard it again. Ported only private instance A's
complete-newest-exchange selection and detached final-budget reservation into
the public source, retaining the public event authority and other retrieval
modules. Added a synthetic over-budget question/answer regression. The focused
conversation suite passes **170/170** in a fresh process. This is a source
qualification, not evidence that private instance A has received the build. A complete
isolated suite run started before this final change is diagnostic only; rerun
against the frozen candidate before promotion.
The private identity-policy audit over all 715 public files now has four
findings, all in the unchanged literary-inspirations essay. The reconciliation
log was depersonalized, reducing the candidate's findings from 69 to those
four. The fictional character citation in that essay was explicitly accepted
as a public literary reference on 2026-09-23. The strict all-files scan still
reports those four hits; the deny policy was not changed. The historical
curated publication manifest excluded that essay, so a current curated audit
must still be generated and reviewed before any remote publication. A read-only
candidate audit of the other 714 tracked/untracked source files against the
unchanged private deny policy passes with zero findings; this excludes only
that accepted, unchanged essay and does not itself create an export receipt.
The first full-run attempt on this source found six recursive-mind fixture
failures after the board observer became a registered layer: the fixtures
still assumed no observer and eight rather than nine native tools. Updated the
synthetic expectations and explicitly removed/restored that registered layer
for the no-observer interruption case. The focused recursive suite now passes
**487/487**. The in-progress full run is diagnostic because it began before
that test-fixture change; a new full run is required.
File-level board comparison also found the private local reply write held the
board-accept lock used by snapshot observations; the public merge had omitted
that lock. Restored it around the local board write without changing the public
durable notification outbox. The focused fleet receipt suite passes. The next
full run must include this final source change.
A subsequent full-run attempt found one coarse-clock pacing fixture failure:
with a one-second interval, consecutive calls on opposite sides of a whole-
second boundary can have less than one elapsed second while meeting the
runtime's integer-clock check. The synthetic fixture now uses a two-second
interval to assert a real wait; production pacing was not changed. Its focused
conversation suite passes **170/170**. That failed full run is diagnostic,
not a release receipt.
On the frozen local image, a credential-free synthetic first start completed
in 135.9 s including cold dependency compilation; a second start reopened its
five-event ledger in 4.7 s without duplicate genesis or baseline events.
Removing only derived storage caused ordinary startup to fail closed as the
checkpoint-dependent canary policy requires. An explicit offline maintenance
run rebuilt the conscious and recursive checkpoints at ledger position 5,
but failed before a reviewed-graph receipt on this empty instance; the five
logical authority events remained unchanged. Thus the legacy empty-instance
derived-rebuild acceptance is **not qualified** on this candidate. The
successful large-ledger disposable backup rebuild and normal restore are
separate evidence; do not conflate them with the failed empty-state profile.
The next full runner aborted one graph fast-iteration subprocess while a
separate cold image compile was running: local FASL lookup reported a Docker
filesystem “Network is unreachable” error. Run alone afterward, the same
isolated graph suite passed **15/15**. The aborted full run is not a receipt;
repeat serially without concurrent compile work.

Critical path before the first canary:

1. Finish S3 qualification around the implemented direct-root source cutover.
   The mixed-ledger owner and terminal guards are in place; now prove
   unknown provider/tool outcomes, exact board-target publication, retry,
   generic terminal completion and frozen-follower settlement, fresh-process
   restart, and duplicate delivery using synthetic mixed ledgers. Focused
   uncertain-provider, interrupted-fleet-tool, fresh-process SQLite reopen,
   and injected board-acceptance/result-append crash checks now pass on small
   synthetic fixtures. A separate SQLite receiving ledger now sees the
   exact board-local reply via a stubbed transport; signed HTTP and independent
   live-instance delivery are still private-canary checks. Do not
   promote a source-only cutover as a canary.
2. Reconcile S4 opportunity selection and private instance A's extra reviewed graph
   component through public seams, preserving public-only activity/retrieval
   modules. Compare `pai.asd` component membership and behavior, not only
   hashes; S6 UI/graph differences need an explicit keep/port decision.
3. Verify checkpoint-dependent ordinary startup against a disposable private instance A
   backup, including checkpoint revision, source binding, restore/tail cost,
   peak heap, and warm restart. No implicit full replay or rebuild on normal
   start. Record any required explicit offline checkpoint repair separately;
   do not implement additional S5 redesign for this canary.
4. Freeze an exact depersonalized commit and run every required gate, including
   the complete fresh-process suite, Python profiles, first-run/rebuild,
   performance, provenance, and publication checks. A focused pass is not a
   release qualification. Record an allowlist and private-file conflicts.
5. Follow `docs/public-source-promotion-runbook.md` for private instance A alone: independent
   recoverable state/file backup, stopped-instance install, startup/restart,
   context/tool/memory/graph canary, one authenticated inbound peer receipt,
   and a board-local or sender-board reply whose receiving ledger confirms
   the outcome. Keep rollback available. Private instance B follows only after this passes.

Read-only component inventory at this gate: the public source now declares
`src/mind/conscious/reviewed-context-graph-storage`, matching private instance A's
reviewed-row component. Public source also declares the separate
working-context request, experience
search, stimulus-activity, and shadow recursive-hot modules that private instance A's file
does not. private instance B lacks more of those public modules. No directory copy is a
valid resolution of that difference.

### Shared-source checkpoint — 2026-09-23

Subsequent source-only S5 episodic-context cutover: the SQLite authority now
offers a source-bound, exact-five-type episodic input read, capped at 16,384
rows and 32 MiB and ending at the assembled turn's durable event position.
Context assembly selects it when episodic recall is enabled
instead of passing recursive hot history into the episode projector. On the
disposable private instance A backup at head 327,559, these types occupy 7,620 rows and
approximately 6.35 MiB of stored JSON. Synthetic typed-read/replay parity,
turn-boundary exclusion, and absent-boundary refusal pass in
`sqlite-event-authority-tests.lisp` (44/44); the recursive runtime
suite passes 487/487. This is a bounded transitional read, not indexed episode
rows, and the live launcher still restores the conscious checkpoint. The
conscious-state projection and recursive hot history remain full-generation
consumers. **No private instance A canary has been installed or launched from this tree.**

The current public S2 preparatory source passes the offline ASDF load,
wrap-chain completeness (1/1), and the full fresh-process Lisp run
(238/238 passed; zero failed or unqualified). The focused live-projection
fixture passes 90 fleet receipt checks; recursive runtime tests pass 470/470
and stimulus tests 100/100. Python publication (16), isolated-runner (15),
and qualification-contract (5) tests pass; the inventory reports 238 suites
with no blocked profiles. Generated census documentation is current,
`git diff --check` is clean, and the coupling report has zero hard back-edges.
These checks qualify a public **source candidate**, not a common private
deployment.

Before both existing agents can run the same source revision, complete the
remaining S2/S3 direct-receipt and retry transition; qualify the existing
checkpoint-dependent startup against each ledger without routine full replay;
then follow the public-source promotion runbook separately for
the first and second private instance. Each promotion needs an exact commit,
backup/rollback receipt, instance-local configuration retained outside this
tree, and a local canary. No private ledger, configuration, or deployment
state was imported into this source candidate.

### Local private-instance-A canary — 2026-09-24

The reviewed public working tree passed a **serial** fresh-process Lisp run
(243/243, zero failed or unqualified), separate observability (32/32) and
publication (1/1) performance profiles, offline load, wrap-chain completeness,
Python profiles, Docker worker, census and coupling checks before installation.
The public empty-instance rebuild gate is still unqualified; the operator
explicitly selected a backed-up **local canary first**, without commit or push.

Private instance A was stopped. Independent Docker-volume backups preserve its
complete source and its current state (excluding only older historical backup
copies). Key event/derived database files and the source manifest were verified
byte-for-byte. The 30 originally reviewed paths were installed individually;
the stopped ledger remained at storage position 327777. An explicit offline
rebuild completed conscious (270931 events, head 327777), recursive (19440
events, head 327777), and reviewed graph (3893 rows, head 326802). No normal
startup replay was authorized.

The first live launch failed before web readiness, without appending a ledger
event. A no-network, no-real-credential reproduction on a disposable copy
identified two **canary-blocking source mismatches**: the promoted startup
script passed `:initialize-p` to private instance A's older memory-ledger
authority, and its reasoning-effort validator rejected a valid saved effort
when reasoning was disabled. The public memory-ledger authority differs from
the private version only by additive explicit empty-instance initialization;
it became the 31st allowlisted file. The public startup validator was aligned
with the private behavior: validate the saved effort, apply it only when
reasoning is enabled. Focused provider CLI tests passed 24/24. A disposable
post-rebuild startup then restored the state, applied provider policy, opened
the web terminal, and reported `Startup complete` at ledger head 327777.

The two corrected files were installed and byte-verified in the live source.
An **operator-facing-only** live canary started successfully: durable restore
in about 14 seconds, total startup about 32 seconds, authority `opened` at
327777, authenticated web endpoint responsive on loopback, and SBCL still
running. Autonomous curiosity and peer reach-out were deliberately omitted
from this first launch. Private instance B was not touched. Source and
current-state backup identifiers are retained only in the private
deployment record. No private data was copied into the
public tree, and no commit or remote publication occurred. Remaining: a real
operator conversation/context/tool canary, graph and messaging observation,
then a decision on enabling autonomous behavior. The full suite predates the
narrow reasoning-validator correction; rerun it before any release claim.

After the slices: run the offline load, wrap-chain completeness check, and
isolated suite runner per `docs/qualification.md`; missing services and
incomplete fixtures are not passes. Record exact revision, test counts,
failures/gaps, and rejected alternatives. Only then use
`docs/public-source-promotion-runbook.md` to compare and promote an exact
public commit to each private deployment, with an explicit allowlist, backup,
rollback receipt, and separate local canaries. A second private instance
follows only after the first has been qualified; neither private tree is
modified by this handoff.

Not required by this reconciliation: speculative semantic task selection,
perfect completion inference, new model scoring/JEv integration, replacing
the event ledger, or importing private history. The development working-
context pseudocode itself marks some of these as future work, not behavior
available to migrate.

## Post-review recovery corrections — 2026-09-24

Selected-root execution now hydrates compacted model responses by exact event
ID before pure replay, verifying each against its hot projection. This restores
native tool-call evidence without mutating shared cache rows. Inferring tool
intent from execution receipts or retaining full provider bodies in every hot
row was rejected; neither is needed, and existing checkpoints remain usable.

Explicit recovery of an unenrolled pre-provider failure now retains the newly
appended activity reference before admitting its linked retry. The exact
membership-selection check remains intact.

Focused isolated suites passed: recursive runtime 492/492, activity operator
33/33. Fixtures cover completed tool-turn hydration, absent authority evidence,
unrelated-root isolation, cache immutability, and unenrolled failed-root retry.
Initial fixture metadata and syntax errors were corrected before these passing
runs. Broad-suite and live-restart qualification remain outstanding; these
results are not a release or first-run/rebuild qualification.
