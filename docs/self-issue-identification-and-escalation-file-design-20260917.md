# Self-issue identification and escalation — file design

Date: 2026-09-17

Status: design only, nothing implemented yet.

## Problem

Operational signal already exists and, after tonight's work, is genuinely
rich: provider failures now carry elapsed time and the provider's own error
message, not just a bare status; accounting anomalies, embedding-service
degraded/recovered transitions, and KG phase status are all either journaled
or logged. But nothing looks *backward* across that signal as a body of
evidence. A recurring failure is only as visible as whoever happens to be
watching a log at the right moment — tonight's mimo-v2.5 investigation is
exactly that: a real pattern, found only because an operator happened to be
staring at the observability dashboard and said something.

Two different audiences need this surfaced differently. The operator wants
to know something is wrong with their agent, in plain language, without
having to notice it themselves. A development session (Claude Code, in a
fresh context with no memory of what happened) wants a structured,
self-contained report it can act on without re-deriving the operational
history from scratch — the exact archaeology tonight's reporting
improvements were meant to reduce, one level up.

## Selected design

**A new curiosity source: operational self-review.** At each curiosity
wake, alongside investigating existing motives, review a bounded recent
window of operational signal events — provider failures (failure_code,
elapsed_seconds, reason), accounting anomalies, embedding degraded/recovered
transitions, KG phase status — for a pattern worth attention: for example
N failures sharing a failure_code within a window, or a status that has not
advanced across several cycles. This reuses the existing review/consolidate
machinery; it is a new *kind* of evidence feeding it, not a new loop.

A detected pattern becomes an ordinary curiosity motive, tagged with a
distinct motive kind (`operational-anomaly`) so it is structurally
distinguishable from intellectual curiosity — both to the human (a different
section, a different tone) and to the reasoning that reviews it (this kind
is never satisfied by speculation; only by corroborating evidence or an
explicit escalation).

Investigating this motive is private cognition, with explicit access to the
raw evidence event IDs it is reasoning about — never a fabricated diagnosis.
(Tonight's retracted chunking diagnosis, corrected only after measurement,
is the concrete lesson this design is built against: reason from evidence
cited by ID, or say you could not resolve it.)

Two terminal outcomes exist for this motive kind, beyond the ordinary
close/sustain/refine set:

- **resolved** — cites the evidence that explains the pattern (tonight's
  "the KG rebuild has no pacing against live traffic" would have been one).
- **escalate** — cannot resolve it with available evidence or authority.

Escalating writes two things:

1. A durable fact, `recursive-operational-issue-escalated` — the sacred
   event log's record, replayable like everything else.
2. A structured artifact at a fixed, known, per-instance location (instance
   state, not the source repo — see below) containing what was observed
   (the cited events, verbatim fields, not paraphrased), what was
   considered, why it could not be resolved, and a plain-language summary.
   This artifact is the actual hand-off: written so a fresh Claude Code
   session can act on it without first reconstructing what happened.

Escalation also reaches the operator through the existing curiosity
reach-out/briefing path — no new channel to the human, just a new reason to
use the one that exists: "I found something I can't resolve myself and
wrote it up."

The artifact lives in the instance's own state, not `docs/` in the shared
repo: it describes *this instance's* operational history, which is state,
not source, even when its content points at a root cause that is source.

## Authority and correction

No new authority. An escalation write is inert data, the same class as an
ordinary curiosity finding — it never mutates code, config, or acts on its
own conclusion. A later self-review pass that recognizes the same
underlying pattern already escalated should reference/supersede the earlier
artifact (mirroring the knowledge-frontier's supersession mechanic) rather
than duplicate it.

## Rejected alternatives

- **Pure mechanical thresholding, no model reasoning**, was rejected as the
  sole mechanism. A threshold decides when to *look*; whether the pattern is
  actually worth escalating — versus a known, already-explained blip — needs
  the same judgment already trusted for ordinary curiosity triage.
- **Writing the artifact into `docs/` in the source repo** was rejected:
  that conflates one instance's live operational history with versioned
  source, and a persistent instance has no business writing into a path
  that implies review and commit.
- **Giving this motive kind its own notification channel** was rejected —
  no new external-effect authority. It uses the reach-out/briefing channel
  that already exists for talking to the operator, same as everything else.
- **Letting her attempt a fix herself** was rejected for this slice. She
  reports; she does not repair. Self-modification of the substrate she runs
  on is a different, much larger authority question, deliberately deferred.

## Proving fixtures (once built)

- A scripted sequence of failure events within a bounded window produces
  exactly one `operational-anomaly` motive, not one per failure.
- A pattern with corroborating resolving evidence never escalates.
- An unresolved pattern escalates exactly once and does not re-escalate for
  the same evidence set on a later review cycle.
- The escalation artifact is well-formed, and its durable-event fields
  follow the same content-free discipline as existing failure journaling;
  the artifact itself (not the durable event) is where richer private
  reasoning is permitted, since it is written for a developer to read, not
  replayed as authority.

## Extension: status, not just a one-way drop

The escalation artifact above is a write-once drop — useful, but it gives
the instance no way to know whether anyone has actually seen it. The
natural extension, worth designing for even if not built in the first
slice: the artifact carries a small status field with a fixed vocabulary
(`raised` → `seen-by-operator` → `picked-up-by-dev` → `resolved` /
`wontfix`, plus a resolution note once terminal), and something on the
human/dev side is expected to advance it rather than just read it.

This is also the natural meeting point with the fleet-architecture
brainstorm (`docs/multi-instance-fleet-design.md`, still just a brainstorm,
nothing implemented): a shared board is exactly the kind of thing multiple
instances would want to read and write to, not just one. Worth keeping
this artifact's shape compatible with that later, rather than building a
single-instance-only format now and having to migrate it. Concretely: key
it by instance id from the start, even while only one instance uses it.

This does not change the first slice's scope (still observation +
escalation only) — it changes what the escalation *writes*, not what
triggers it.

## Handoff

First slice should be observation and escalation only — read-only over the
event log, one new curiosity motive kind, one new durable fact, one new
artifact write. Resolution logic (attempting an actual fix) is explicitly
out of scope here.

Tonight's mimo-v2.5 investigation is a natural retrofit test once this
exists: would this mechanism have caught the pattern on its own, and would
the resulting artifact have been enough for a fresh Claude Code session to
act on without the hour of live log archaeology it actually took?
