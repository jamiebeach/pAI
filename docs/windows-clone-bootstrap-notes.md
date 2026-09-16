# Windows clone/bootstrap notes

What actually went wrong bootstrapping a second full instance (persistent
`pai-runtime` container, migrated event ledger and memory baseline, tailnet
web terminal) from a fresh `git clone` of this repository on Windows with
Docker Desktop, and what changed as a result. Kept close to the code so it
stays accurate as the substrate evolves; not a tutorial.

## 1. `.lock` files corrupted by a plain `git clone` on Windows

`docker/quicklisp.lock` and `docker/quicklisp-releases.lock` are sourced
inside the Linux build (`. /tmp/quicklisp.lock`). A fresh Windows clone with
the common `core.autocrlf=true` setting checks them out with CRLF line
endings, which corrupts every value they define (`QUICKLISP_BOOTSTRAP_SHA256=...\r`)
and fails the build with an opaque `sha256sum: no properly formatted
checksum lines found`.

`.gitattributes` already exists specifically to prevent this class of bug
for `.sh`/`.lisp`/`.asd`/`.sexp`/`.md`/`.json` -- `.lock` was simply missing
from the list. Fixed by adding `*.lock text eol=lf`.

## 2. A storage-boundary pathname check that only Linux had exercised

`scripts/conscious-conversation.lisp`'s `%conversation-path-contained-p`
called `uiop:ensure-pathname` directly on a raw string read from an
environment variable. `UIOP:ENSURE-PATHNAME` parses a bare string with Unix
namestring rules regardless of host OS, so a native Windows path like
`Z:/dev/pai/state/events.sqlite3` never satisfies `:want-absolute` --
it fails with `Invalid pathname ...: Expected an absolute pathname`, even
though `(pathname "Z:/dev/pai/state/events.sqlite3")` or
`uiop:parse-native-namestring` on the same string works fine.

This function does not exist in every checkout of this substrate -- it's a
newer addition -- so it had apparently only ever been exercised inside the
Linux container, never via a host-native Windows SBCL process. Fixed by
routing both arguments through `uiop:parse-native-namestring` before
`ensure-pathname`; a no-op on POSIX hosts where the two parsers already
agree.

## 3. SQLite over a Windows Docker Desktop bind mount

`scripts/import_container_state.py` installs a stopped instance's state
(mounted read-only from `.clone-state/` on the host) into a fresh named
volume, using `pai_cli.backup_sqlite_database`'s online SQLite-backup-API
copy to read the source. On Windows, a Docker Desktop bind mount of an NTFS
host path does not reliably support the file locking SQLite needs even for
a read-only open: `PRAGMA query_only=ON` succeeds, but the first real read
(`SELECT ... FROM ...`) fails with `unable to open database file`, even
though the file itself is completely intact and readable as plain bytes.

`backup_sqlite_database`'s online-copy safety exists to protect against a
*live* writer -- valuable where it's used elsewhere against a database that
might still be open. `import_container_state.py`'s own precondition is an
already-*stopped* source (its docstring says so), so that protection has
nothing to protect against here, and forcing it is what breaks on Windows.
Fixed by adding `_copy_stopped_sqlite_database`: a plain `shutil.copy2` of
the base file plus any `-wal`/`-shm` companions (never opens the source with
SQLite at all), with an integrity check against the destination copy
afterward (which lives on the target volume, not the bind mount, so it's
safe to open there). See
`tests/test_pai_container_state.py::test_import_recovers_a_still_open_source_wal_instead_of_dropping_it`.

If you hit `unable to open database file` from a *different* code path that
still uses the online-backup approach against a bind-mounted source, the
same fix applies: don't open the bind-mounted path with SQLite at all, copy
first.

## 4. Two instances sharing one local Docker image tag

`docker compose build` for a persistent runtime service historically tagged
the built image `pai-local:development` -- a fixed literal, not
project-scoped. Building a *second* instance cloned into a sibling
directory (or even just running `docker build -t pai-local:development .`
by habit) silently repoints that tag away from the first instance's image.
A container already running from the old image is unaffected (containers
pin to an image ID at creation, not a mutable tag), but the next
recreate/restart of the first instance would pull the second instance's
image instead -- wrong source tree, no error, no warning.

Fixed in `compose.yaml`: the image tag is now
`${COMPOSE_PROJECT_NAME:-pai}-local:development`. Compose derives
`COMPOSE_PROJECT_NAME` from the working directory by default, which already
separates most sibling clones, but directory names can still collide
(same name on a different drive, a renamed checkout, `docker build -t
pai-local:development .` run directly instead of through Compose). Set
`COMPOSE_PROJECT_NAME` explicitly in each instance's `.env`
(`.env.example` documents it) whenever that's a real possibility on a given
host. If a collision does happen, recovery is a plain rebuild of the
victim's own `pai-runtime` from its own source tree -- there is nothing to
"restore," the tag is just a pointer.

## 5. `PAI_AGENT_ID` had no CLI flag

`scripts/pai_cli.py` had no `--agent-id` option; `PAI_AGENT_ID` was always
set to the literal `q45-conversation-dev` (a name left over from an early
experiment) with no way to override it. This is an internal per-instance
storage-partition key, not identity -- persona and event/memory content
carry identity (see `README.md`, "identity is memory, not configuration")
-- so running an existing instance is unaffected either way. But it matters
for a migration that writes an event ledger directly (bypassing
`pai_cli.py`, e.g. via `conscious-conversation.lisp`'s own
`PAI_AGENT_ID`/`PAI_EVENT_STORAGE_MIGRATE` environment variables): if that
ledger is written under any other agent-id, `pai_cli.py`'s later runs won't
find it -- the storage-authority check reports the correctly-empty-looking
`q45-conversation-dev` partition as uninitialized, `Storage AUTHORITY-PREPARE
failed: SQLite event authority is uninitialized`, even though the database
file plainly has data in it under a different partition key.

Fixed by adding a real `--agent-id` flag (default unchanged, so an existing
instance keeps running under the same partition it always has). Whichever
value ends up in the event/memory database is the one every later
`pai_cli.py` invocation for that instance must keep using.

## What a migration into a fresh instance needs to get right, in order

1. `.env` exists (copied from `.env.example`) before the first `docker
   compose` invocation, or the same handful of variables must be repeated on
   every single `docker compose build`/`up`/`exec`/`run` call --
   `PAI_CONTEXT_GRAPH_PRIOR_EXPOSURE_MICROUSD` in particular is a required
   Compose interpolation variable with no default, so its absence fails even
   a read-only diagnostic `docker compose exec`.
2. If a migration writes the event ledger and memory baseline directly via
   `conscious-conversation.lisp` (rather than through `pai_cli.py`'s own
   `--agent-id`/`--migrate-*` flags), its `PAI_AGENT_ID` must match what
   `pai_cli.py` will use for every later run of that instance -- i.e. pass
   the same explicit `--agent-id` both times, or accept the
   `q45-conversation-dev` default on both sides.
3. Any SQLite file that will cross a Windows bind mount (host → container)
   should have its WAL checkpointed on the host first
   (`PRAGMA wal_checkpoint(TRUNCATE)`) even though `import_container_state.py`
   no longer strictly requires it (item 3 above) -- it keeps the file set
   simple to reason about and avoids depending on WAL recovery working
   correctly on first open of the copy.
4. Two named volumes only converge on the same content once
   `pai-state-import` has actually run against a *non-empty* `.clone-state/`
   -- an empty `pai-state` volume plus a plausible-looking `PAI_AGENT_ID`
   produces the same "uninitialized" error as item 2, for an unrelated
   reason (no ledger there at all yet).
