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
& 'Z:\beadwork\bw.exe' -C 'Z:\dev\paiProject' <command>
```

- `Z:\dev\paiProject` owns the only pAI Beadwork ledger. Always use that
  exact `-C` path, including when diagnosing Aria from `Z:\dev\pai` or Angel
  from `Z:\dev\paiAngel`. Never run `bw init` in either deployment checkout.
- Run `& 'Z:\beadwork\bw.exe' -C 'Z:\dev\paiProject' prime` before
  substantive project work. Read the selected issue with `show` before changing
  code.
- Track new nontrivial epics, issues, bugs, and tasks from the current frontier.
  Do not bulk-import completed history. Tiny read-only questions and urgent live
  operational interventions do not require a new issue; create or update one if
  they turn into source work.
- Before implementation, select an unblocked issue with `ready`, then `start`
  it, or create an appropriately scoped issue. Record important progress,
  decisions, rejected alternatives, blockers, and qualification gaps with
  `comment`. Use dependencies instead of informal ordering notes.
- Beadwork is local-only private Git state. Never run `bw sync`, push the
  `beadwork` branch, or configure a Beadwork remote; doing so would publish the
  ledger through the source repository. Private operational context may be
  recorded when it materially helps diagnosis, but minimize it and never store
  credentials, authentication material, or unrelated personal data there.
- Keep the canonical `Z:\dev\paiProject` checkout for integration,
  qualification, and release work. For isolated implementation, create Git
  worktrees below `Z:\dev\pai-worktrees\`, named `<issue-id>-<short-slug>`,
  with branches named `codex/<issue-id>-<short-slug>`. Never create worktrees
  inside another checkout.
- A source task is complete only after its required qualification evidence is
  recorded on the issue. Commit the source change, close the issue with a useful
  reason, and leave the local ledger committed by Beadwork. Do not run `sync`.
  Do not treat Beadwork state as a substitute for source commits, tests, or the
  publication gates above.

### Aria and Angel workflow

`Z:\dev\paiProject` is source authority. `Z:\dev\pai` is Aria's development
deployment and controlled experimental lane. `Z:\dev\paiAngel` is Angel's
production deployment. Neither deployment checkout is an independent source or
issue authority.

When a problem is first observed in Aria or Angel:

1. Create or update an issue in the canonical local ledger. Prefer compact,
   content-free evidence such as event IDs, timestamps, error classes, affected
   subsystem, model/profile name, and reproducible behavior. Private prompts,
   responses, log excerpts, or user context may be included when necessary to
   preserve the diagnosis because this ledger must remain local-only. Minimize
   copied content and never include credentials or authentication material. Use
   labels such as `found-by:aria`, `found-by:angel`, `area:runtime`, or
   `area:provider` when useful.
2. Record the deployment checkout's commit, branch, dirty-file list, relevant
   durable settings, and the bounded log/event window before experimenting.
   Preserve unrelated local changes and distinguish pre-existing drift from the
   experiment.
3. Trial-and-error diagnosis is allowed in Aria when it is the fastest safe way
   to identify a fix. Keep experiments reversible and hypothesis-scoped; change
   one causal variable at a time, retain exact commands and test observations in
   sanitized form, and do not push experimental Aria commits as canonical work.
   Destructive state changes, authority rewrites, paid expansion, or privacy
   relaxation still require their normal authorization.
4. Prefer reproducing Angel failures in Aria. Treat Angel as production:
   diagnostics may be read there, but source experiments should occur in Aria
   unless the operator explicitly authorizes a bounded production intervention.
5. Once an experiment identifies a solution, create or use the issue worktree
   below `Z:\dev\pai-worktrees\` from current `paiProject` `main`. Re-derive the
   minimal fix there; do not copy whole files blindly from Aria, because its
   checkout may contain unrelated drift. Add the regression test and run the
   required qualification in the canonical worktree.
6. Land the qualified change in `paiProject` first. Then deploy that canonical
   result to Aria, verify the original failure is resolved, and remove or
   reconcile the experimental delta. Deploy to Angel only from the qualified
   canonical result and verify it independently.
7. Record canonical commit, qualification results, deployments, and live
   verification on the issue before closing it. An emergency Aria-first hotfix
   is not complete until it has been canonicalized; an Angel intervention is
   not complete until canonical source and both relevant deployments agree.
