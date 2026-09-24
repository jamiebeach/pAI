# Current implementation and next work

Updated 2026-09-24. This is the current navigation and planning entry point.
Dated reconciliation documents preserve historical decisions and intermediate
failures; their old "pending" statements are not the current release status.

## Canonical source and qualification

The promoted runtime candidate is `1a67ccde`. Its 718-file inventory includes
exact late peer-notification coverage, a bounded content-free inbox view, and a
recursive capability-context correction that defers to the tools attached to a
request. The older unbounded decoded-memory cache, private executor/outbox and
ledger repair utility were not adopted. Keep canonical bounded retrieval and
external-connection integrity checks.

The exact candidate passed the 243-suite isolated Lisp run, offline load, wrap
completeness, both Python discovery profiles (with separately covered host-only
skips), the Docker worker, serial observability, synthetic first start/restart,
missing-derived recovery and explicit offline rebuild. Content, provenance and
secret checks passed. The required publication timing benchmark did **not** pass:
0.664 ms against its unchanged 0.55 ms limit. A paired baseline also failed
under reported host pressure. The operator accepted this timing limitation for
the current publication; neither the threshold nor the failed result was
rewritten as a pass. Dependency lock/notice work remains in
[dependency provenance](dependency-provenance.md).

## Replica promotion state

The development instance matches all 718 canonical files in both its checkout
and deployment. Its separate experimental extras, identity, configuration and
state remain local. Live conversation continuity, an actual memory-search tool
result, peer receipt, board-local reply and durable private-activity completion
were verified independently of model claims.

The second instance also matches all 718 canonical files in both locations and
has no active extra source. Its prior source variants were archived recoverably.
An independent stopped-state backup preceded installation. Offline conscious,
recursive and reviewed-graph checkpoint rebuilds passed with all original event
rows unchanged. The older event authority lacked the deliberately explicit
activity indexes: a first live request was rejected before admission, then
stopped-instance index preparation created all three. After restart, an
authenticated live request executed `search-memory`, returned one result and
produced a durable reply. Private identity and configuration stayed local and
unchanged. Backup paths, ledger identifiers and credentials are kept in the
private promotion receipt, not this repository.

This establishes source parity and bounded canaries, not correctness of every
long-lived cognitive behavior. Follow the [promotion runbook](public-source-promotion-runbook.md)
for future instances; checkpoint rebuilding and activity-index preparation are
distinct stopped-state maintenance operations.

## Next investigation, not part of source synchronization

1. Model-response failures: distinguish transport/provider errors, budget
   admission, parsing, cancellation and historical clusters using exact event
   IDs and current configuration. Do not infer the cause from a dashboard count.
2. Knowledge-graph formation: establish current eligible sealed episodes,
   formation events and checkpoints. Instrument the existing quiet-step gate
   and scheduler to distinguish "not called" from "called and declined".
   Historical agent reports are hypotheses, not fresh instrumentation.
3. Activity-on context assembly: inspect exact provider requests and source
   links; distinguish absent live instrumentation from omitted retained context.
   Measure redundant context before changing selection or fitting policy.

For each investigation, reproduce on synthetic or isolated state where possible,
make a small canonical change with regression coverage, validate in development,
then promote. Source parity is not proof that these behaviors have been fixed.

## Reading map

- [Architecture](architecture.md): implemented ownership and boundaries.
- [Development](development.md): setup and explicit initialization.
- [Qualification](qualification.md): required evidence and failure semantics.
- [Promotion](public-source-promotion-runbook.md): per-instance backup and canary.
- [First-run recovery](first-run-qualification.md): bounded startup vs maintenance.
- [Historical stabilization plan](stabilization-plan-20260922.md) and
  [migration slices](development-source-reconciliation-slices-20260922.md):
  decision history, not an active duplicate task list.
