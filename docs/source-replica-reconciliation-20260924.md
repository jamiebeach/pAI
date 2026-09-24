# Source replica reconciliation — 2026-09-24

The initial inventory and first-slice evidence below are historical. Public
source and both replica promotions have since advanced; see
[current status](current-status.md) for the current commit, qualification caveat
and promotion outcome.

## Remaining behavior port

The canonical stimulus activity module now contains exact observation coverage
and a bounded, content-free peer inbox projection. The web adapter owns exact
board-content comparison; the dashboard reads a cached projection snapshot
without triggering full replay or taking the model execution lock. Generic
completion and legacy peer outcomes are interpreted separately from delivery.
Coverage is one durable append per reconciliation step, with interruption repair.

The older full-memory cache is intentionally not ported: it retains decoded
generation-sized rows/vectors and omits the newer foreign-connection integrity
audit. Canonical streaming top-K retrieval and bounded winner hydration remain.
The older executor/outbox and destructive ledger repair utility are not imported.

Source qualification passed for local commit `7b9fb1c1`; the development restart
and new inbox endpoint passed. Conversation continuity passed. A subsequent
canonical capability-context correction passed the focused recursive suite and
live memory-tool canary; activity/peer/board checks and second-instance promotion
remain pending. See [current status](current-status.md)
for the qualification caveat and source-copy compatibility evidence. Source
backups retain rejected private variants outside the source inventory; instance
identity, configuration and state remain local.

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
- The private exact-memory cache was rejected after semantic comparison;
  canonical bounded retrieval remains. Exact observation coverage was ported
  through the registered board observer and tested separately.

## Promotion boundary

At the time of this first slice, second-instance promotion and full release
qualification remained open. Their later outcomes are recorded in
[current status](current-status.md). The boundary remains: preserve dirty target
source before replacement, confirm behavior in development first, and keep
instance configuration, secrets, state and private documents outside the
canonical inventory.
