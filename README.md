# pAI

**One agent. One continuous, accumulating context. No sessions.**

Every day you work with AI tools, the context evaporates. Each new session
starts from zero: you re-explain the project, restate your constraints, and
rebuild the shared understanding that yesterday's session had finally
reached. The most valuable thing that develops between a person and an AI
system — the accumulated context of working together — is exactly what
current architectures throw away.

pAI is built around the opposite premise: context that accumulates and
compounds. One long-lived agent maintains a single, ever-deepening body of
shared history with you — your projects, your decisions, your terminology,
your reasoning, and the outcomes of past work. Week after week, the agent
starts from everything you have built together, not from nothing. The
hundredth conversation benefits from all ninety-nine before it.

This compounding changes what the agent can do. It remembers *why* a
decision was made, not just that it was made. It can connect a question
today to a conclusion reached months ago. It knows which facts came from
you, which it inferred, and which arrived from other sources — and it can
show its work, citing the specific evidence behind anything it claims to
know, rather than confabulating.

## How it works

The event log is the only authoritative state. Memory, knowledge graphs,
motivation, disposition, and identity are all deterministic, replayable
projections over append-only evidence. The entire cognitive state can be
rebuilt from the log at any time and verified against itself.

Authority is separated from suggestion. Models propose; typed kernel policy
authorizes. Nothing a model says grants itself effects, providers,
capabilities, or publication rights.

Every prompt is a fresh projection: compaction and window limits are
rendering concerns, not where truth lives. Beyond memory, motivation,
disposition, and identity are projections too — the agent develops its own
curiosities with stable identity and satisfaction receipts, which mature
into background cognition and work dockets, all structurally fenced from
unilateral action.

This is an architecture for agents that persist for months: that wake up
knowing what mattered yesterday, that can be audited end to end, and that
operate within explicit, enforceable bounds.

For how this compares to Claude Code, Codex, Hermes, Exo, and the wider
agent landscape, see [pAI in the Agent Landscape](docs/related-work.md).

## What exists today

- **Event-sourced substrate** (Common Lisp): append-only ledger,
  deterministic projections, replay-verified rebuilds. SQLite is the
  supported event authority.
- **Continuous-state runtime**: bounded attention, lifecycle work
  coordination, typed stimuli admission, deterministic pulse selection.
- **Knowledge graphs with provenance**: entities and facts carry evidence
  lineage, disclosure classes, and correction history. The agent can
  distinguish "you told me this" from "I inferred this" from "another agent
  told me this."
- **Motivational dynamics**: endogenous curiosities with stable
  identity, lifecycle phases, typed satisfaction receipts, and refractory
  bounds. Internal drives are tracked and visible, and never become
  unilateral action.
- **Disposition tracking**: a cross-motive projection over the same
  event spine — hedging, escalation, commitment — kept observational and
  holding no authority over behavior.
- **Multi-mind memory lineage**: designed support for multiple persistent
  minds sharing storage without collapsing autobiographies — direct
  experience, inheritance, and communication rendered distinctly.
- **Governed tools**: validated registry, observer stages, authority gates.
- **Model-agnostic**: works with any OpenAI-compatible endpoint — local
  (Ollama, LM Studio) or hosted (OpenRouter and others).
- **Browser interface** and operator CLI.

## Status

**Pre-release developer preview.** The published reconciliation baseline passed
the isolated source gates and synthetic installation/restart/rebuild checks.
Live cognitive behavior and dependency provenance still have open work; this
is not a production-readiness claim. See [current status and plan](docs/current-status.md)
and the [implemented architecture](docs/architecture.md).

Loading defines the system; explicit initialization starts its activities.
Every derived projection must remain rebuildable from the event log.

## Source map

- `src/kernel/`: event authority, tool dispatch, initialization, and seams.
- `src/mind/`: conversation, memory, knowledge, cognition, and observability.
- `src/adapters/`: storage, models, transports, and browser interface.
- `src/tools/`: agent-callable operations.
- `config/`: ontology and experimental provider/context/work profiles.
- `templates/`: generic instance templates.
- `scripts/pai_cli.py`: operator CLI.
- `tests/`: isolated Lisp suites and host-side tests.

The ASDF systems are `pai`, `pai-memory-access`, and `pai-context-graph`.
Provider settings and credentials belong to each installation.

## Development

See [development setup](docs/development.md) and the
[qualification contract](docs/qualification.md). The current dependency
evidence and remaining lock/notice work are in the
[provenance inventory](docs/dependency-provenance.md). Docker provides the
current development envelope. It starts an idle container so loading source
does not implicitly start a model-backed agent.

## Configuration templates

Copy `.env.example` to `.env`, replace every `replace-with-...` value, and
keep that file out of version control. Likewise, copy
`config/conscious-provider-profiles.example.json` and
`config/context-graph-ontology-models.example.json` to the corresponding
names without `.example` before enabling provider-backed runtime or
ontology-lab work. Model identifiers, context capacities, routing
guarantees, and price ceilings in these files are placeholders — verify them
against the provider you select. The zero-price placeholders fail closed by
design: nothing runs until you enter real values.

## License and contributions

The source is licensed under [MIT](LICENSE). The project is maintained by
its original author and is not accepting pull requests at this time.
Dependency and provenance review is still in progress.
