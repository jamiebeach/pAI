# Current implementation and next work

Updated 2026-09-24. This is the current navigation and planning entry point.
Dated reconciliation documents preserve historical decisions and intermediate
failures; their old "pending" statements are not the current release status.

## Qualified baseline

Public commit `41d43dc1` reconciled the canonical implementation and passed the
243-suite isolated Lisp run, offline load, wrap completeness, both Python
discovery profiles, separately required worker and serial performance profiles,
and synthetic first start, restart, missing-derived recovery and explicit
offline rebuild. Environment-specific Python skips were covered separately.
These are source and synthetic-install results, not a claim that all long-lived
agent behavior is correct. Dependencies still need the lock/notice work recorded
in [dependency provenance](dependency-provenance.md).

The development deployment matched the 716-file baseline inventory. Its restart
and authenticated observability checks passed; private identity, configuration
and state remained separate. A second deployment remained held for the semantic
reconciliation described below. Private paths, histories and backup receipts are
deliberately outside this repository.

## Current reconciliation slice

Port exact late-notification coverage and a content-free peer backlog view to
the canonical stimulus pipeline, qualify the final candidate, then validate the
development deployment before promoting the same source to the second instance.
Keep canonical bounded retrieval and external-connection integrity checks;
do not replace them with a generation-sized decoded memory cache. The
[replica reconciliation record](source-replica-reconciliation-20260924.md)
records decisions. Promotion evidence belongs in a private receipt until its
non-private summary is verified.

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
