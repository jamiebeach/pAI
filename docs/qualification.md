# Qualification contract

Qualification has three independent obligations: source loads offline, structural
invariants hold, and purposeful tests execute. Passing one does not imply the others.

```sh
docker compose exec pai-dev sbcl --script tests/wrap-chain-completeness-tests.lisp src/
docker compose exec pai-dev sh tests/run-isolated.sh sbcl /workspace '*-tests.lisp'
docker compose exec pai-dev python3 -m unittest discover -s tests -p test_publication.py -v
docker compose exec pai-dev python3 -m unittest discover -s tests -p test_isolated_runner.py -v
docker compose exec pai-dev python3 -m unittest discover -s tests -p test_qualification_contract.py -v
docker compose exec pai-dev python3 scripts/qualification_contract.py
```

The offline load is documented in [development.md](development.md). On a host with
Node installed, run `node scripts/coupling-report.js`; hard back-edges must remain
zero. The development image does not currently install Node or a browser.

The synthetic install, restart, and ledger-only projection rebuild evidence is
recorded in [first-run-qualification.md](first-run-qualification.md).

The isolated runner starts a fresh Lisp process for each suite and runs suites
sequentially. Parallel qualification processes must not share one ASDF compile
cache. `PAI_TEST_STATE` selects a scratch root; the runner gives every suite its
own child directory. `PAI_QUICKLISP_SETUP` selects Quicklisp, and
`PAI_TEST_LOG_DIR` preserves complete suite output outside the source tree.
Empty selections, aborted processes, harness errors, failed assertions, and
unrecognized output all produce nonzero exit status. A later passing tally must
not hide an earlier failure. Assert-by-raising suites require an explicit success
marker and clean completion.

The public inventory contains 224 Lisp suites. Twenty-four inherited suites that
require the originating system's PostgreSQL clone were explicitly retired from
the public artifact because SQLite is the supported runtime authority. Their
names and exclusion reasons remain in the private curation decision record. This
is a product-boundary decision, not a conversion of their old failures into
passes. Retained SQLite suites must cover event append/replay, conditional writes,
memory and graph projections, restart, and rebuild. Three tests of the retired
flat image layout and private-era sealed source manifest are also excluded with
explicit reasons. Their current invariants are covered by ASDF load, source-index,
dispatch parity, wrap completeness, and candidate-manifest checks.

The retained 224-suite baseline passes from an immutable candidate image with
read-only source, no network, fresh Lisp processes, sequential execution, and
per-suite scratch. The serial observability and publication performance commands
also pass. Both Python discovery patterns pass. Their generic no-Docker run skips
the worker integration; its dedicated required profile runs separately with
`PAI_TEST_IMAGE` bound to the exact candidate image.

`tests/qualification-contract.json` declares one exceptional Lisp suite: the
required serial benchmark. Structural validation succeeds only when all 224
published suites are represented. The release command
`python3 scripts/qualification_contract.py --require-ready` requires every
declared profile to have a concrete command and no blocked profiles. The
Docker-worker command must pass against the exact candidate image.

Private inference-qualification receipts can be redirected outside a read-only
checkout with `PAI_PRIVATE_SCRATCH=/absolute/disposable/private/path`. The default
remains repository-local `.scratch` for existing development workflows. The
launcher rejects the repository root and filesystem root as private scratch;
all baseline, replay, resume, and receipt paths must remain below the selected
boundary.

Public fixture substitutions preserve test structure and use fictional people,
pets, tasks, and health examples. The deferred-work replay is a newly written
catalogue-indexing scenario; it preserves receipt identifiers, state transitions,
deadlines, and authority assertions. Its text is not a conversation transcript.

The hygiene workflow tests publication tooling on pull requests. The private
identity audit runs only in a trusted push/manual context and requires the
`PAI_PUBLICATION_IDENTITIES` secret. A missing policy fails the gate. A pattern
scan does not replace manual review or an independent secret scanner. Public
eval fixtures may not declare captured conversation/event provenance, and dated
operator approval scopes are rejected because private authorization is not
portable to a public repository.
