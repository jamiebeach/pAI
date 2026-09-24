# Implemented architecture

This describes the current source, not a promise of live cognitive quality.
For active work and qualification evidence see [current status](current-status.md).

## Authority and derived state

The append-only event ledger is authoritative. SQLite event authority owns
ordered durable append and conditional idempotency. Derived memory, activity,
conscious-state, recursive and reviewed-graph projections bind to that authority;
none may become a second source of truth. Exact source event IDs retain lineage.
Derived integrity checks include changes made by a different SQLite connection.

Normal startup restores qualified checkpoints and bounded tails. The public
configuration launcher permits missing-derived recovery only through ledger
position 10,000. Ordinary live CLI startup does not inherit that permission.
Larger or stale/incomplete projections require stopped-instance maintenance;
successful partial rebuilding is not successful maintenance. Empty reviewed
graphs still publish a checkpoint. See [recovery](first-run-qualification.md).

The recursive runtime still retains a checkpoint-backed hot event projection.
Indexed row-backed readers and shadow/parity tests exist, but their existence
does not mean every live consumer has completed that migration. Full replay is
an explicit maintenance operation, not a routine dashboard refresh.

## Composition and effects

ASDF loading defines the system. Explicit initialization actions start it.
Registered seam layers compose adapter behavior; wrap chains and hard subsystem
back-edges are not extension mechanisms. Models propose work through validated
tool boundaries; model output and peer content grant no authority.

Host Python scripts own launch, instance configuration and maintenance. Lisp
owns cognitive projections and synthetic cognitive fixtures. Credentials,
personas, operator identities and captured conversations belong to deployments,
not the canonical source tree.

## Conversation and working context

An operator interaction admits a durable root. Request assembly selects bounded,
source-linked evidence, preserves the latest complete exchange and current tool
results, and fits before crossing the provider boundary. Sustained activities
retain membership and continuation state. Holistic summaries are derived,
source-linked, revision-bound projections, not replacements for ledger evidence.
Selected-root recovery hydrates exact compacted provider evidence when needed
without mutating the shared hot cache. Memory retrieval streams candidates and
hydrates bounded winners rather than retaining every parsed vector and row.

## Fleet and private stimuli

Authenticated peer delivery appends one self-contained local receipt. A new
receipt is itself the private stimulus root. Old ledgers with a linked generic
stimulus remain readable without executing both forms. Generic environment
stimuli and peer receipts share the existing quiet opportunity and executor;
they do not create a separate peer timer or motivational system.

Activities freeze bounded same-resource membership before execution. Completion,
disposition and consumption are durable facts. Interrupted registered reads can
be recovered; arbitrary unknown effects remain parked. Board publication uses
stable operation identity and durable intent checks. An agent saying "I replied"
is not a delivery receipt.

Exact observation coverage is adapter-owned: a successful retained board read
must match owner, thread, message ID, author and text. A later unstarted receipt
may then be marked covered with the observation event as proof. Retry boundaries
invalidate earlier attempt evidence; started, activity-owned or legacy-linked
receipts are not silently reassigned. Coverage and consumption are separately
repairable appends and do not imply a separate reply. Missing retained proof
fails closed; the runtime does not scan the full ledger to invent coverage.

## Observability and knowledge formation

The live dashboard separates process-local state from bounded durable history.
The peer inbox reads a snapshot of the retained recursive projection, reports its
ledger position, and emits bounded content-free rows. Counts are not lifetime
totals. Observability does not admit work, publish replies or repair state.

Knowledge formation has its own proposal/review and provenance boundaries over
eligible experience. A healthy seal path or graph startup does not prove that
formation opportunities are scheduled or admitted. Live gate/scheduler tracing
and model failure diagnosis remain separate work in the current plan.
