# Contributing

pAI is currently a maintainer-led developer preview. Issues and focused design
discussion are welcome. Pull requests may be closed until the package split and
public extension boundaries are stable.

Keep real identities, credentials, state, and captured conversations outside the
repository. Fixtures must be synthetic. Preserve the event log as authoritative
state, avoid load-time side effects, add no cross-subsystem wrap chains or hard
back-edges, and run suites in fresh Lisp processes.

Before proposing a change, follow [the development guide](docs/development.md)
and [qualification contract](docs/qualification.md). Explain the behavior change,
rejected alternatives, and the evidence that qualifies it. New dependencies must
include an exact version or content lock, upstream source, license classification,
and any required notice update.
