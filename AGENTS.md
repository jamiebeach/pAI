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

## Work management

This project uses Beadwork for durable planning and issue state. Beadwork is
installed outside `PATH`; invoke it explicitly as:

```powershell
& 'Z:\beadwork\bw.exe' -C '<absolute-repository-path>' <command>
```

- Run `& 'Z:\beadwork\bw.exe' -C '<repo>' prime` before substantive project
  work. Read the selected issue with `show` before changing code.
- Track new nontrivial epics, issues, bugs, and tasks from the current frontier.
  Do not bulk-import completed history. Tiny read-only questions and urgent live
  operational interventions do not require a new issue; create or update one if
  they turn into source work.
- Before implementation, select an unblocked issue with `ready`, then `start`
  it, or create an appropriately scoped issue. Record important progress,
  decisions, rejected alternatives, blockers, and qualification gaps with
  `comment`. Use dependencies instead of informal ordering notes.
- Beadwork state is publishable Git state. Keep real operator identities,
  credentials, captured conversations, private runtime data, and private paths
  out of issue titles, descriptions, comments, and attachments. Use synthetic
  or content-free references where evidence is sensitive.
- Keep the canonical `Z:\dev\paiProject` checkout for integration,
  qualification, and release work. For isolated implementation, create Git
  worktrees below `Z:\dev\pai-worktrees\`, named `<issue-id>-<short-slug>`,
  with branches named `codex/<issue-id>-<short-slug>`. Never create worktrees
  inside another checkout.
- A source task is complete only after its required qualification evidence is
  recorded on the issue. Commit the source change, close the issue with a useful
  reason, and run `sync`. Do not treat Beadwork state as a substitute for source
  commits, tests, or the publication gates above.
