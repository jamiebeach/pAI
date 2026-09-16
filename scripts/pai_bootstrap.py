#!/usr/bin/env python3
"""Bootstrap a new pAI instance, or migrate a legacy agent's history into one.

Three subcommands, each independently useful:

  new              Scaffold a fresh instance directory: git clone this repo,
                    write config/instance.json and .env from their examples.

  checkpoint-wal    Truncate the WAL of one or more SQLite files in place.
                    Run this on the HOST before a SQLite file crosses a
                    Windows Docker Desktop bind mount (see
                    docs/windows-clone-bootstrap-notes.md, item 3) -- not
                    strictly required after that fix, but keeps the file set
                    simple to reason about.

  clone-legacy-agent
                    Migrate a legacy agent's event ledger and Postgres
                    memory into an existing instance directory's event
                    authority, using this repo's own proven tools
                    (scripts/prepare_agent_migration.py,
                    scripts/conscious-conversation.lisp's
                    PAI_EVENT_STORAGE_MIGRATE/PAI_MEMORY_STORAGE_MIGRATE
                    path) rather than reimplementing them. Stands up a
                    disposable, explicitly-labelled local Postgres clone
                    from a pg_dump, runs the cutover in migration-only
                    mode, checkpoints the result, and tears the clone down.
                    Never touches the legacy agent's live systems -- point
                    it at a pg_dump and a snapshot of the event ledger you
                    already took.

None of this replaces judgment: read docs/windows-clone-bootstrap-notes.md
and docs/clone-runbook.md first. This script automates the parts that are
purely mechanical and stops (rather than guessing) wherever a real decision
belongs to the operator -- most importantly, --destination-agent-id must
match whatever the target instance's pai_cli.py will use on every later
run (its own default is "q45-conversation-dev"; see notes doc item 5).
"""

from __future__ import annotations

import argparse
from contextlib import closing
import json
import os
from pathlib import Path
import shutil
import sqlite3
import subprocess
import sys
import tempfile
import time


REPO = Path(__file__).resolve().parent.parent
sys.path.insert(0, str(REPO / "scripts"))

import prepare_agent_migration  # noqa: E402


def _run(command: list[str], **kwargs) -> subprocess.CompletedProcess:
    print("+", " ".join(str(part) for part in command))
    return subprocess.run(command, check=True, **kwargs)


def _docker(*args: str, **kwargs) -> subprocess.CompletedProcess:
    return _run(["docker", *args], **kwargs)


# --------------------------------------------------------------------------
# new
# --------------------------------------------------------------------------

def cmd_new(args: argparse.Namespace) -> int:
    dest = Path(args.dest).resolve()
    if dest.exists() and any(dest.iterdir()):
        raise SystemExit(f"destination is not empty: {dest}")

    source = Path(args.source).resolve() if args.source else REPO
    _run(["git", "clone", str(source), str(dest)])

    instance_json = dest / "config" / "instance.json"
    example = dest / "config" / "instance.example.json"
    if instance_json.exists():
        print(f"{instance_json} already present, leaving it alone")
    else:
        config = json.loads(example.read_text(encoding="utf-8"))
        config["agent"]["id"] = args.agent_id
        config["agent"]["persona"] = args.persona
        instance_json.write_text(
            json.dumps(config, indent=2) + "\n", encoding="utf-8"
        )
        print(f"wrote {instance_json}")

    env_file = dest / ".env"
    env_example = dest / ".env.example"
    if env_file.exists():
        print(f"{env_file} already present, leaving it alone")
    elif env_example.is_file():
        text = env_example.read_text(encoding="utf-8")
        text = text.replace(
            "COMPOSE_PROJECT_NAME=replace-with-a-name-unique-across-every-instance-on-this-host",
            f"COMPOSE_PROJECT_NAME={args.agent_id}",
        )
        env_file.write_text(text, encoding="utf-8")
        print(
            f"wrote {env_file} from .env.example with COMPOSE_PROJECT_NAME "
            f"set to '{args.agent_id}' -- fill in the remaining "
            "replace-with-... placeholders (API keys, web password, KG "
            "budget) before starting pai-runtime"
        )

    print()
    print(f"New instance scaffolded at {dest}")
    print("Next: fill in the .env placeholders, then either")
    print(f"  cd {dest} && docker compose build pai-runtime")
    print("for a fresh empty instance, or run clone-legacy-agent to migrate")
    print("an existing agent's history into it first.")
    return 0


# --------------------------------------------------------------------------
# checkpoint-wal
# --------------------------------------------------------------------------

def checkpoint_wal(path: Path) -> str:
    with closing(sqlite3.connect(path)) as connection:
        (busy, log, checkpointed) = connection.execute(
            "PRAGMA wal_checkpoint(TRUNCATE)"
        ).fetchone()
    return f"{path}: busy={busy} log_frames={log} checkpointed_frames={checkpointed}"


def cmd_checkpoint_wal(args: argparse.Namespace) -> int:
    for raw in args.database:
        path = Path(raw)
        if not path.is_file():
            print(f"skipping (not a file): {path}", file=sys.stderr)
            continue
        print(checkpoint_wal(path))
    return 0


# --------------------------------------------------------------------------
# clone-legacy-agent
# --------------------------------------------------------------------------

def _wait_for_postgres(container: str, user: str, database: str, timeout: int = 30) -> None:
    deadline = time.monotonic() + timeout
    while time.monotonic() < deadline:
        completed = subprocess.run(
            ["docker", "exec", container, "pg_isready", "-U", user, "-d", database],
            capture_output=True,
        )
        if completed.returncode == 0:
            return
        time.sleep(1)
    raise SystemExit(f"Postgres in {container} never became ready")


def _stand_up_postgres_clone(
    *, network: str, container: str, user: str, password: str,
    database: str, host_port: int, pg_dump: Path,
) -> None:
    _docker(
        "network", "create", network,
        capture_output=True,
    )
    _docker(
        "run", "-d", "--name", container,
        "--label", "pai.dev.disposable=true",
        "--network", network,
        "-e", f"POSTGRES_USER={user}",
        "-e", f"POSTGRES_PASSWORD={password}",
        "-e", f"POSTGRES_DB={database}",
        "-p", f"127.0.0.1:{host_port}:5432",
        "pgvector/pgvector:pg16",
    )
    _wait_for_postgres(container, user, database)
    _docker("cp", str(pg_dump), f"{container}:/tmp/clone.sql")
    restore_env = dict(os.environ, MSYS_NO_PATHCONV="1")
    _run(
        ["docker", "exec", container, "psql", "-U", user, "-d", database,
         "-q", "-f", "/tmp/clone.sql"],
        env=restore_env,
    )
    _docker("exec", container, "rm", "-f", "/tmp/clone.sql")


def _teardown_postgres_clone(*, network: str, container: str) -> None:
    subprocess.run(["docker", "rm", "-f", container], capture_output=True)
    subprocess.run(["docker", "network", "rm", network], capture_output=True)


def _find_sbcl(explicit: str | None, instance_dir: Path | None = None) -> Path:
    relative = Path(".tools") / "sbcl" / "PFiles" / "Steel Bank Common Lisp" / "sbcl.exe"
    candidates = [
        Path(explicit) if explicit else None,
        Path(os.environ.get("PAI_SBCL", "")) if os.environ.get("PAI_SBCL") else None,
        # The instance being migrated owns its runtime before this repo does.
        (instance_dir / relative) if instance_dir else None,
        REPO / relative,
        Path(shutil.which("sbcl") or ""),
    ]
    for candidate in candidates:
        if candidate and candidate.is_file():
            return candidate
    raise SystemExit(
        "sbcl not found -- pass --sbcl, set PAI_SBCL, or install the "
        "project-local runtime (see docs/clone-runbook.md)"
    )


def _find_quicklisp_setup(explicit: str | None, instance_dir: Path | None = None) -> Path:
    relative = Path(".tools") / "quicklisp" / "setup.lisp"
    candidates = [
        Path(explicit) if explicit else None,
        Path(os.environ.get("PAI_QUICKLISP_SETUP", ""))
        if os.environ.get("PAI_QUICKLISP_SETUP") else None,
        (instance_dir / relative) if instance_dir else None,
        REPO / relative,
        Path.home() / "quicklisp" / "setup.lisp",
    ]
    for candidate in candidates:
        if candidate and candidate.is_file():
            return candidate
    raise SystemExit(
        "Quicklisp setup.lisp not found -- pass --quicklisp-setup or set "
        "PAI_QUICKLISP_SETUP"
    )


def cmd_clone_legacy_agent(args: argparse.Namespace) -> int:
    instance_dir = Path(args.instance_dir).resolve()
    if not (instance_dir / "scripts" / "conscious-conversation.lisp").is_file():
        raise SystemExit(f"{instance_dir} does not look like a pAI instance checkout")

    state_root = instance_dir / "state"
    migration_dir = instance_dir / ".clone-state" / "migration"
    migration_dir.mkdir(parents=True, exist_ok=True)
    ledger = migration_dir / f"{args.destination_persona_id}-ledger.jsonl"
    manifest = migration_dir / f"{args.destination_persona_id}-ledger-manifest.json"

    if ledger.exists() or manifest.exists():
        raise SystemExit(
            f"{ledger} or {manifest} already exists -- remove both to re-run, "
            "or point --instance-dir at a fresh instance"
        )

    print("== Phase 1: preparing the migration ledger ==")
    sources = [Path(args.legacy_events)]
    segment_sources: list[Path] = []
    if args.legacy_segment_directory:
        segment_sources = sorted(
            Path(args.legacy_segment_directory).glob("events-*.jsonl")
        )
    report = prepare_agent_migration.prepare_migration(
        [*sources, *segment_sources],
        ledger,
        manifest,
        source_agent_id=args.source_agent_id,
        destination_agent_id=args.destination_agent_id,
        destination_persona_id=args.destination_persona_id,
        not_before=args.not_before,
    )
    print(json.dumps(report, indent=2, sort_keys=True))
    if report["missing_id_count"]:
        print(
            f"warning: {report['missing_id_count']} source event IDs are "
            "missing (gaps in the legacy ledger) -- this is often expected "
            "history repair, but confirm before proceeding",
            file=sys.stderr,
        )

    if args.dry_run:
        print("--dry-run: stopping before touching Docker or the state directory")
        return 0

    network = f"{args.destination_persona_id}-clone-net"
    container = f"{args.destination_persona_id}-clone-postgres"
    print(f"== Phase 2: standing up disposable Postgres clone ({container}) ==")
    _stand_up_postgres_clone(
        network=network, container=container,
        user=args.pg_clone_user, password=args.pg_clone_password,
        database=args.pg_clone_database, host_port=args.pg_clone_port,
        pg_dump=Path(args.pg_dump),
    )

    try:
        print("== Phase 3: running the cutover (migration-only) ==")
        state_root.mkdir(parents=True, exist_ok=True)
        target_ledger = state_root / "events.jsonl"
        if target_ledger.exists():
            raise SystemExit(f"refusing to overwrite {target_ledger}")
        shutil.copy2(ledger, target_ledger)

        persona_file = state_root / "personas" / f"{args.destination_persona_id}.json"
        sbcl = _find_sbcl(args.sbcl, instance_dir)
        quicklisp_setup = _find_quicklisp_setup(args.quicklisp_setup, instance_dir)

        environment = dict(os.environ)
        environment.update(
            PAI_SOURCE_ROOT=(instance_dir / "src").as_posix(),
            PAI_TEMPLATES=(instance_dir / "templates").as_posix(),
            PAI_STATE_ROOT=state_root.as_posix(),
            PAI_QUICKLISP_SETUP=quicklisp_setup.as_posix(),
            PAI_AGENT_ID=args.destination_agent_id,
            PAI_CONVERSATION_PERSONA=args.destination_persona_id,
            PAI_EVENT_STORAGE_DATABASE=(state_root / "events.sqlite3").as_posix(),
            PAI_DERIVED_STORAGE_DATABASE=(state_root / "derived.sqlite3").as_posix(),
            PAI_EVENT_STORAGE_MIGRATE="1",
            PAI_EVENT_STORAGE_INITIALIZE="0",
            PAI_MEMORY_STORAGE_MIGRATE="1",
            PAI_MEMORY_MIGRATION_SOURCE="labelled-local-postgres-clone",
            PAI_PG_HOST="127.0.0.1",
            PAI_PG_PORT=str(args.pg_clone_port),
            PAI_PG_DATABASE=args.pg_clone_database,
            PAI_PG_USER=args.pg_clone_user,
            PAI_PG_PASSWORD=args.pg_clone_password,
            PAI_DEV_DATABASE_LABEL="clone",
            PAI_MIGRATION_ONLY="1",
        )
        if persona_file.is_file():
            environment["PAI_CONVERSATION_PERSONA_FILE"] = persona_file.as_posix()
        if args.own_voice_baseline:
            # Leaving these two unset is what makes the imported memory
            # baseline attribute as this instance's own native history
            # rather than third-party "migrated-semantic-memory" evidence.
            # See docs/windows-clone-bootstrap-notes.md and
            # legacy-agent-migration-rehearsal-file-design (private docs)
            # for the cross-agent alternative this deliberately skips.
            environment.pop("PAI_MEMORY_MIGRATION_SOURCE_AGENT_ID", None)
            environment.pop("PAI_MEMORY_MIGRATION_MANIFEST_SHA256", None)
        else:
            environment["PAI_MEMORY_MIGRATION_SOURCE_AGENT_ID"] = args.source_agent_id
            environment["PAI_MEMORY_MIGRATION_MANIFEST_SHA256"] = report["output_sha256"]

        _run(
            [str(sbcl), "--dynamic-space-size", "3072", "--script",
             str(instance_dir / "scripts" / "conscious-conversation.lisp")],
            cwd=instance_dir, env=environment,
        )
    finally:
        print(f"== tearing down disposable clone ({container}) ==")
        _teardown_postgres_clone(network=network, container=container)

    print("== Phase 4: checkpointing and staging for pai-state-import ==")
    for name in ("events.sqlite3", "derived.sqlite3"):
        path = state_root / name
        if path.is_file():
            print(checkpoint_wal(path))

    clone_state = instance_dir / ".clone-state"
    for name in ("events.sqlite3", "derived.sqlite3", "events.jsonl"):
        source_path = state_root / name
        if source_path.is_file():
            shutil.copy2(source_path, clone_state / name)
    personas_dir = state_root / "personas"
    if personas_dir.is_dir():
        shutil.copytree(personas_dir, clone_state / "personas", dirs_exist_ok=True)

    print()
    print(f"Staged for import at {clone_state}. Next:")
    print(f"  cd {instance_dir}")
    print("  docker compose run --rm pai-state-import")
    print("  docker compose up -d pai-runtime")
    print(
        "  docker compose exec pai-runtime python3 scripts/pai_cli.py "
        f"--state-dir /var/lib/pai --agent-id {args.destination_agent_id} "
        f"--persona {args.destination_persona_id} ..."
    )
    return 0


# --------------------------------------------------------------------------

def parse_args(argv: list[str] | None = None) -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    subparsers = parser.add_subparsers(dest="command", required=True)

    new_parser = subparsers.add_parser("new", help="scaffold a fresh instance directory")
    new_parser.add_argument("--dest", required=True)
    new_parser.add_argument(
        "--source", help="repo to clone from (default: this checkout)"
    )
    new_parser.add_argument("--agent-id", required=True)
    new_parser.add_argument("--persona", required=True)
    new_parser.set_defaults(func=cmd_new)

    checkpoint_parser = subparsers.add_parser(
        "checkpoint-wal", help="truncate the WAL of one or more SQLite files"
    )
    checkpoint_parser.add_argument("database", nargs="+")
    checkpoint_parser.set_defaults(func=cmd_checkpoint_wal)

    clone_parser = subparsers.add_parser(
        "clone-legacy-agent",
        help="migrate a legacy agent's ledger and memory into an instance",
    )
    clone_parser.add_argument("--instance-dir", required=True)
    clone_parser.add_argument("--legacy-events", required=True)
    clone_parser.add_argument("--legacy-segment-directory")
    clone_parser.add_argument("--pg-dump", required=True)
    clone_parser.add_argument("--source-agent-id", required=True)
    clone_parser.add_argument(
        "--destination-agent-id", default="q45-conversation-dev",
        help=(
            "must match the storage partition the destination instance's "
            "own pai_cli.py will use on every later run -- its default is "
            "q45-conversation-dev unless you pass --agent-id there too"
        ),
    )
    clone_parser.add_argument("--destination-persona-id", required=True)
    clone_parser.add_argument("--not-before", default="1970-01-01T00:00:00Z")
    clone_parser.add_argument(
        "--own-voice-baseline", action="store_true", default=True,
        help=(
            "default: imported memory attributes as this instance's own "
            "native history (source agent == destination agent is the "
            "normal case for a continuity migration)"
        ),
    )
    clone_parser.add_argument(
        "--cross-agent-evidence-baseline", dest="own_voice_baseline",
        action="store_false",
        help=(
            "imported memory attributes as third-party evidence "
            "(migrated-semantic-memory / source-agent:<id>) instead of "
            "this instance's own voice -- use for a rehearsal or lineage "
            "import into a genuinely different persona, not a continuity "
            "migration"
        ),
    )
    clone_parser.add_argument("--pg-clone-port", type=int, default=5435)
    clone_parser.add_argument("--pg-clone-user", default="pai")
    clone_parser.add_argument("--pg-clone-password", default="pai_local_clone_only")
    clone_parser.add_argument("--pg-clone-database", default="pai_memory")
    clone_parser.add_argument("--sbcl")
    clone_parser.add_argument("--quicklisp-setup")
    clone_parser.add_argument(
        "--dry-run", action="store_true",
        help="prepare the ledger and report on it, then stop",
    )
    clone_parser.set_defaults(func=cmd_clone_legacy_agent)

    return parser.parse_args(argv)


def main(argv: list[str] | None = None) -> int:
    args = parse_args(argv)
    return args.func(args)


if __name__ == "__main__":
    raise SystemExit(main())
