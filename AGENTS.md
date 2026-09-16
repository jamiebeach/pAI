# Working on pAI

Read [development](docs/development.md) and [qualification](docs/qualification.md)
before changing code. This is a pre-release source candidate.

- The event log is authoritative; everything derived must be rebuildable.
- Loading defines; explicit initialization starts. Use `define-init` actions.
- Use registered layers on seams. Add no wrap chains or hard subsystem back-edges.
- Keep real operator identities, private state, credentials, and captured
  conversations outside the source tree. Test data must be synthetic.
- Run each Lisp suite in a fresh process. Never use `tests/run-all.lisp`.
- Keep cognitive fixtures in Lisp; use dependency-free Python for host launchers.
- Preserve unrelated work. Document rejected alternatives and qualification gaps.
- No remote publication until content, provenance, test, and first-run gates pass.

Run the offline load, wrap-chain completeness check, and isolated suite runner.
The runner must return failure for failures and unqualified suites. Do not
classify missing services or incomplete fixtures as passing tests.
