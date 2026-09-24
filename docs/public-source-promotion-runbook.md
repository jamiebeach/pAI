# Promoting a qualified public source candidate

Use this procedure to install a public commit into an existing private agent.
The public repository contains code and synthetic fixtures only; an instance's
identity, event authority, derived storage, credentials, configuration, and
captured conversations never travel with the candidate.

1. Record the exact public commit and require a clean public tree. Run the
   offline load, wrap-chain check, full isolated Lisp runner, both Python
   discovery profiles, publication and qualification-contract tests, census
   generation check, coupling report, and the required first-run/rebuild and
   performance profiles. Missing services, fixtures, or receipts are not passes.
2. Prepare a file-by-file allowlist of source, test, documentation, and launcher
   changes needed by that commit. Review each file for private content and
   compare it with the target's current version. Never copy a directory tree,
   `.git`, `config/instance.json`, secret files, state, or generated databases.
3. Stop only the target instance. Record its running image/commit, Git status,
   exact allowlist and hashes. Make a recoverable backup of every allowlisted
   target file plus the target's independent state backup, outside the public
   repository. A dirty overlapping target file requires an explicit merge
   decision; copying over it is not a default promotion step.
4. Apply only the reviewed allowlist to one development instance. Verify hashes
   against the candidate, run its local load/startup checks, and restart once.
   Keep the previous image and backup until the canary passes.
5. Canary startup memory/time, first-message latency, complete recent-turn
   continuity, retained tool results, activity continuation, semantic-memory
   retrieval, one durable peer receipt, and a board-local reply. Check the
   receiving ledger independently of the sender's claim. If any required check
   fails, stop promotion and restore the recorded target version and files.
6. Only after the first canary passes, repeat steps 2–5 for the next instance
   using its own allowlist comparison and backup. Never copy another agent's
   private state or configuration into it.

Record each command, result, candidate hash, target-file hash, backup location,
canary evidence, and rollback result in a private promotion receipt. The
public repository may retain a redacted gate summary, never the private receipt.
