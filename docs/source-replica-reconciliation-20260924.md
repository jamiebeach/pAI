# Source replica reconciliation — 2026-09-24

Canonical source starts at `253d0a42`. A second private deployment was compared
read-only against its 715-file inventory. The host has 39 missing files, 59
substantive differences, and 88 line-ending-only differences. Its deployed
source has 48 missing files, 68 substantive differences, and 79 line-ending-only
differences. Host and deployed copies also differ; neither is a canonical replica.
Private file names, identities, configuration and captured content remain outside
this record.

## Accepted first slice

- Board messages with equal timestamps sort by message ID for deterministic
  observation paging and content revisions, including after reload.
- Observability includes peer and generic stimulus lifecycle events and bounded
  type-filter batches. Peer summaries expose provenance fields, not message text.
- Focused synthetic board and dashboard suites each passed 33 checks.

## Rejected or deferred alternatives

- Do not replace canonical idempotent fleet publication with the older private
  executor/outbox implementation. It would discard newer durable intent checks.
- Do not import the private ledger repair utility: it deletes authoritative
  event rows, contrary to the append-only authority contract. Preserve it only
  in private backup when cleaning source extras; never execute it here.
- Private peer-backlog UI depends on an alternate inspection/executor path.
  Canonical event-history visibility is accepted; that live inspector is not
  yet ported or qualified.
- The private exact-memory cache and observation-coverage changes still require
  semantic comparison. Differences are not automatically bugs or improvements.

## Promotion boundary

This slice is not a second-instance promotion or a release claim. Preserve
the target's dirty source before any replacement. Required first-run/rebuild
qualification remains open, along with the full promotion profile and the
remaining semantic review. Confirm the accepted slice in the development
instance before touching the second instance. Instance configuration, secrets,
state and private documents must never enter the canonical source inventory.
