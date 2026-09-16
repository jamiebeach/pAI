#!/usr/bin/env python3
"""Canonical pAI operator CLI: chat plus bounded slash commands."""

from __future__ import annotations

import argparse
from contextlib import closing
from datetime import datetime, timezone
from decimal import Decimal, InvalidOperation
import hashlib
import ipaddress
import json
import os
from pathlib import Path
import re
import shutil
import sqlite3
import subprocess
import sys
import tempfile
from urllib.parse import urlsplit

from conscious_q4_cli import (
    local_sbcl,
    native_environment,
    quicklisp_setup,
    setup_local_lisp,
)


PERSONA_NAME = re.compile(r"^[a-z0-9][a-z0-9_-]{0,63}$")
PERSONA_FRAGMENT_LIMIT = 16_000


def validate_embedding_endpoint(value: str) -> str:
    """Admit only the local or Docker-host Nomic-compatible route."""
    parsed = urlsplit(value)
    if (
        parsed.scheme != "http"
        or parsed.hostname not in {
            "127.0.0.1", "localhost", "::1", "host.docker.internal"
        }
        or parsed.username is not None
        or parsed.password is not None
        or parsed.path != "/api/embeddings"
        or parsed.query
        or parsed.fragment
    ):
        raise SystemExit(
            "--embedding-endpoint must be a local /api/embeddings URL"
        )
    return value


def validate_persona_name(value: str) -> str:
    name = value.casefold()
    if not PERSONA_NAME.fullmatch(name):
        raise SystemExit("persona names use 1-64 lowercase letters, digits, '-' or '_'")
    return name


def validate_openrouter_model_slug(value: str) -> str:
    """Validate an explicit OpenRouter author/model identifier."""
    if (
        not isinstance(value, str)
        or not 1 <= len(value) <= 200
        or value.count("/") != 1
        or any(ord(character) < 33 or ord(character) > 126 for character in value)
    ):
        raise SystemExit(
            "--openrouter-model must be a bounded author/model slug without spaces"
        )
    author, model = value.split("/", 1)
    if not author or not model:
        raise SystemExit(
            "--openrouter-model must be a bounded author/model slug without spaces"
        )
    return value


def openrouter_reasoning_override(
    args: argparse.Namespace,
) -> tuple[str | None, str | None]:
    """Resolve the explicit reasoning contract for one OpenRouter session."""
    mode = args.openrouter_reasoning
    effort = args.openrouter_reasoning_effort
    if effort and mode not in (None, "enabled"):
        raise SystemExit(
            "--openrouter-reasoning-effort requires reasoning mode enabled"
        )
    if effort:
        mode = "enabled"
    elif mode == "enabled":
        # "Enabled" without an effort delegates an important cost/latency
        # choice to the model. Keep the session default explicit; specialist
        # calls may still override it request-locally.
        effort = "medium"
    elif mode is None and args.openrouter_model:
        # A profile's model-specific reasoning switch is not portable to an
        # arbitrary operator-selected model. Omission delegates to OpenRouter's
        # declared model default, including endpoints where reasoning is
        # mandatory.
        mode = "model-default"
    return mode, effort


def knowledge_graph_budget_environment(
    args: argparse.Namespace,
) -> dict[str, str]:
    """Return explicit cumulative and predecessor formation exposure."""
    value = args.knowledge_graph_budget_usd
    prior_value = args.knowledge_graph_prior_exposure_usd
    if value is None and prior_value is None:
        return {}
    if not args.knowledge_graph_formation:
        raise SystemExit(
            "knowledge-graph budget controls require --knowledge-graph-formation"
        )

    def microusd(raw: str | None, option: str, positive: bool) -> int:
        if raw is None:
            raise SystemExit(f"{option} is required")
        try:
            amount = Decimal(raw)
        except InvalidOperation as error:
            raise SystemExit(f"{option} must be a decimal amount") from error
        scaled = amount * Decimal(1_000_000)
        if (
            not amount.is_finite()
            or (amount <= 0 if positive else amount < 0)
            or scaled != scaled.to_integral_value()
            or scaled > Decimal(1_000_000_000)
        ):
            qualifier = "positive, " if positive else "non-negative, "
            raise SystemExit(
                f"{option} must be {qualifier}no greater than 1000, and "
                "specified to at most six decimal places"
            )
        return int(scaled)

    ceiling = microusd(value, "--knowledge-graph-budget-usd", True)
    result = {"PAI_CONTEXT_GRAPH_GENERATION_BUDGET_MICROUSD": str(ceiling)}
    if prior_value is not None:
        prior = microusd(
            prior_value, "--knowledge-graph-prior-exposure-usd", False
        )
        if prior > ceiling:
            raise SystemExit(
                "--knowledge-graph-prior-exposure-usd cannot exceed the "
                "cumulative knowledge-graph budget"
            )
        result["PAI_CONTEXT_GRAPH_PRIOR_EXPOSURE_MICROUSD"] = str(prior)
    return result


def context_graph_runtime_environment(args: argparse.Namespace) -> dict[str, str]:
    """Bind promotion as one reviewed owner/protocol profile."""
    profile = args.context_graph_runtime_profile
    if profile in (
        "reviewed-inference-v7",
        "reviewed-inference-v8",
        "reviewed-inference-v9",
    ):
        if not args.knowledge_graph_formation:
            raise SystemExit(
                f"--context-graph-runtime-profile {profile} "
                "requires --knowledge-graph-formation"
            )
        if (args.knowledge_graph_budget_usd is None
                or args.knowledge_graph_prior_exposure_usd is None):
            raise SystemExit(
                f"{profile} requires explicit cumulative budget "
                "and predecessor exposure"
            )
        authorization_id = args.context_graph_budget_authorization_id
        if not authorization_id:
            raise SystemExit(
                f"{profile} requires --context-graph-budget-authorization-id"
            )
        if (len(authorization_id) > 128
                or any(not (character.isalnum() or character in "-_.:")
                       for character in authorization_id)):
            raise SystemExit(
                "--context-graph-budget-authorization-id must be 1-128 "
                "letters, digits, '-', '_', '.', or ':'"
            )
        return {
            "PAI_CONTEXT_GRAPH_RUNTIME_PROFILE": profile,
            "PAI_CONTEXT_GRAPH_BUDGET_AUTHORIZATION_ID": authorization_id,
        }
    return {"PAI_CONTEXT_GRAPH_RUNTIME_PROFILE": profile}


def knowledge_graph_rebuild_environment(
    args: argparse.Namespace,
) -> dict[str, str]:
    """Bind a provider-backed run to graph formation and nothing else."""
    if not args.knowledge_graph_rebuild_only:
        return {"PAI_KNOWLEDGE_GRAPH_REBUILD_ONLY": "0"}
    if not args.knowledge_graph_formation:
        raise SystemExit(
            "--knowledge-graph-rebuild-only requires "
            "--knowledge-graph-formation"
        )
    forbidden = (
        args.recursive_tools
        or args.deliberate_curiosity
        or args.curiosity_reach_out
        or args.curiosity_briefing
        or curiosity_consolidation_enabled(args)
        or args.web
    )
    if forbidden:
        raise SystemExit(
            "--knowledge-graph-rebuild-only excludes recursive tools, web, "
            "curiosity, reach-out, briefing, and consolidation"
        )
    return {
        "PAI_KNOWLEDGE_GRAPH_REBUILD_ONLY": "1",
        # Provider-backed rebuilds issue several dependent graph phases.  A
        # small request-start interval prevents a fast phase from immediately
        # consuming the provider's burst allowance and wasting the whole
        # durable task attempt on a 429.  Normal conversational runtimes do
        # not receive this binding.
        "PAI_CONTEXT_GRAPH_PROVIDER_MIN_INTERVAL_SECONDS": "15",
    }


def recursive_tools_environment(
    args: argparse.Namespace, repo: Path, state: Path
) -> dict[str, str]:
    """Configure explicitly selected native development primitives."""
    if not args.recursive_tools:
        return {}
    if args.mind_loop != "recursive":
        raise SystemExit("--recursive-tools requires --mind-loop recursive")
    bash = (
        Path(r"C:\Program Files\Git\bin\bash.exe")
        if os.name == "nt"
        else Path("/bin/bash")
    )
    if not bash.is_file():
        raise SystemExit(f"recursive tools require Bash at {bash}")
    runtime_home = state / "restricted-runtime-home"
    runtime_home.mkdir(parents=True, exist_ok=True)
    return {
        "PAI_RECURSIVE_TOOLS": (
            "container-development-v1"
            if os.environ.get("PAI_CONTAINER_PROFILE") in {
                "workspace-development-v1", "persistent-runtime-v1"
            }
            else "host-native-development-v1"
        ),
        "PAI_RECURSIVE_WORKSPACE_ROOT": str(repo),
        "PAI_RECURSIVE_BASH": str(bash),
        "PAI_RESTRICTED_RUNTIME_HOME": str(runtime_home),
        "PAI_LISP_EVAL_REVIEW_LOG": str(
            repo / ".pai-review" / "lisp-evals.jsonl"
        ),
    }


def tool_runtime_environment(
    environment: dict[str, str], tool_environment: dict[str, str]
) -> dict[str, str]:
    """Return the explicit environment visible to host-native live evaluation."""
    keep = {
        "PATH", "PATHEXT", "SYSTEMROOT", "WINDIR", "COMSPEC",
        "TEMP", "TMP", "SBCL_HOME", "OPENROUTER_API_KEY",
        "BRAVE_API_KEY", "BRAVE_API_KEY_FILE",
    }
    permitted_pai = {
        "PAI_UNIFIED_CLI", "PAI_CLI_ANSI", "PAI_QUICKLISP_SETUP",
        "PAI_SOURCE_ROOT", "PAI_FILE_SEARCH_ROOT", "PAI_TEMPLATES",
        "PAI_ROOT", "PAI_STATE_ROOT", "PAI_EVENT_STORAGE_BACKEND",
        "PAI_EVENT_STORAGE_DATABASE", "PAI_DERIVED_STORAGE_DATABASE",
        "PAI_EVENT_STORAGE_MIGRATE", "PAI_EVENT_STORAGE_INITIALIZE",
        "PAI_MEMORY_STORAGE_MIGRATE", "PAI_NEAR_TERM_INTENTIONS",
        "PAI_COGNITION_RUNTIME", "PAI_CONVERSATION_LOOP",
        "PAI_OLLAMA_ENDPOINT", "PAI_PG_BACKUP", "PAI_PG_HOST",
        "PAI_PG_PORT", "PAI_DEV_DATABASE_LABEL", "PAI_AGENT_ID",
        "PAI_MEMORY_MIGRATION_SOURCE_AGENT_ID",
        "PAI_MEMORY_MIGRATION_MANIFEST_SHA256",
        "PAI_CONVERSATION_ENDPOINT", "PAI_CONVERSATION_MODEL",
        "PAI_CONVERSATION_PERSONA", "PAI_CONVERSATION_MAX_OUTPUT_TOKENS",
        "PAI_CONVERSATION_PERSONA_FILE",
        "PAI_CONVERSATION_CONTEXT_PROFILE",
        "PAI_CONSCIOUS_CONTEXT_PROFILES",
        "PAI_CONVERSATION_PROVIDER_PROFILE",
        "PAI_CONVERSATION_MODEL_OVERRIDE",
        "PAI_CONVERSATION_ZDR_OVERRIDE",
        "PAI_CONVERSATION_DATA_COLLECTION_OVERRIDE",
        "PAI_CONVERSATION_REASONING_OVERRIDE",
        "PAI_CONVERSATION_REASONING_EFFORT",
        "PAI_CONSCIOUS_PROVIDER_PROFILES",
        "PAI_CONVERSATION_REQUEST_LIMIT",
        "PAI_CONVERSATION_COST_CEILING_MICROUSD",
        "PAI_CONVERSATION_SHOW_REJECTED",
        "PAI_CONVERSATION_SHOW_MEMORY_CONTEXT", "PAI_STARTUP_VERBOSE",
        "PAI_RECURSIVE_LOOP_TRACE", "PAI_CURIOSITY_WAKE_SECONDS",
        "PAI_DELIBERATE_CURIOSITY", "PAI_CURIOSITY_REACH_OUT",
        "PAI_CURIOSITY_BRIEFING", "PAI_CURIOSITY_CONSOLIDATION",
        "PAI_AFFECT_BASELINE_EVENT_ID",
        "PAI_EPISODIC_MEMORY", "PAI_KNOWLEDGE_GRAPH_FORMATION",
        "PAI_KNOWLEDGE_GRAPH_REBUILD_ONLY",
        "PAI_CONTEXT_GRAPH_GENERATION_BUDGET_MICROUSD",
        "PAI_CONTEXT_GRAPH_PRIOR_EXPOSURE_MICROUSD",
        "PAI_CONTEXT_GRAPH_BUDGET_AUTHORIZATION_ID",
        "PAI_CONTEXT_GRAPH_RUNTIME_PROFILE",
        "PAI_PRIVATE_BUDGET_PERCENT",
        "PAI_CONTEXT_TRACE",
        "PAI_CONTEXT_TRACE_DIR", "PAI_LLM_DEBUG_MODE", "PAI_LLM_DEBUG_DIR",
        "PAI_WEB_ENABLED", "PAI_WEB_ADDRESS", "PAI_WEB_PORT",
        "PAI_WEB_FILE_MUTATION",
        "PAI_LISP_EVAL_REVIEW_LOG",
    }
    restricted = {
        key: value for key, value in environment.items()
        if key.upper() in keep or key in permitted_pai
    }
    # The web server and Lisp evaluator currently share one contained process.
    # Preserve its explicit Basic-auth credential only when the web capability
    # is selected; the container remains the security boundary for live eval.
    if environment.get("PAI_WEB_ENABLED") == "1":
        for key in ("PAI_WEB_USERNAME", "PAI_WEB_PASSWORD",
                    "PAI_WEB_PASSWORD_FILE"):
            if environment.get(key):
                restricted[key] = environment[key]
    restricted_home = Path(tool_environment["PAI_RESTRICTED_RUNTIME_HOME"])
    restricted_roaming = restricted_home / "AppData" / "Roaming"
    restricted_local = restricted_home / "AppData" / "Local"
    restricted_roaming.mkdir(parents=True, exist_ok=True)
    restricted_local.mkdir(parents=True, exist_ok=True)
    restricted.update(
        {
            "HOME": str(restricted_home),
            "USERPROFILE": str(restricted_home),
            "APPDATA": str(restricted_roaming),
            "LOCALAPPDATA": str(restricted_local),
            # Do not let ASDF inherit operator-account registries after the
            # environment boundary. The driver explicitly loads Quicklisp and
            # registers the pAI root, which are the only sources it needs.
            "CL_SOURCE_REGISTRY": (
                "(:source-registry :ignore-inherited-configuration)"
            ),
        }
    )
    if os.name == "nt":
        # Native SBCL's USER-HOMEDIR-PATHNAME follows the Windows pair even
        # when HOME/USERPROFILE are present. Point both at the same contained
        # runtime home or ASDF expands :HOME directives through NIL.
        drive = restricted_home.drive
        restricted["HOMEDRIVE"] = drive
        restricted["HOMEPATH"] = str(restricted_home)[len(drive):]
    restricted.update(tool_environment)
    return restricted


def normalize_persona_document(name: str, document: object) -> dict[str, object]:
    if not isinstance(document, dict) or document.get("schema_version") != 1:
        raise SystemExit("persona source must be a schema-version 1 JSON object")
    current = document.get("current")
    source = current if isinstance(current, dict) else document
    identity = source.get("identity")
    voice = source.get("voice")
    revision = source.get("revision", 0)
    if not isinstance(revision, int) or isinstance(revision, bool) or revision < 0:
        raise SystemExit("persona revision must be a non-negative integer")
    for label, value in (("identity", identity), ("voice", voice)):
        if (
            not isinstance(value, str)
            or not value.strip()
            or len(value) > PERSONA_FRAGMENT_LIMIT
            or "\x00" in value
        ):
            raise SystemExit(f"persona {label} must be non-empty bounded text")
    return {
        "schema_version": 1,
        "persona_id": name,
        "revision": revision,
        "identity": identity,
        "voice": voice,
    }


def require_ignored_storage(repo: Path, state: Path) -> None:
    try:
        state.relative_to(repo)
    except ValueError:
        return
    ignored = subprocess.run(
        ["git", "check-ignore", "--quiet", str(state)],
        cwd=repo,
        check=False,
        stdout=subprocess.DEVNULL,
        stderr=subprocess.PIPE,
        text=True,
    )
    if ignored.returncode == 1:
        raise SystemExit(
            "refusing private state inside a non-ignored repository path"
        )
    if ignored.returncode != 0:
        detail = (ignored.stderr or "unknown Git error").strip().splitlines()[0]
        raise SystemExit(f"could not verify private-state ignore policy: {detail}")


def import_persona(repo: Path, state: Path, name: str, source: Path) -> Path:
    require_ignored_storage(repo, state)
    try:
        document = json.loads(source.resolve().read_text(encoding="utf-8"))
    except (OSError, json.JSONDecodeError) as error:
        raise SystemExit(f"could not read persona source: {error}") from error
    profile = normalize_persona_document(name, document)
    directory = state / "personas"
    directory.mkdir(parents=True, exist_ok=True)
    target = directory / f"{name}.json"
    temporary = directory / f".{name}.json.tmp"
    temporary.write_text(
        json.dumps(profile, ensure_ascii=False, indent=2) + "\n", encoding="utf-8"
    )
    temporary.replace(target)
    return target


def export_event_ledger(repo: Path, database: Path, destination: Path) -> dict[str, int]:
    """Export exact authoritative event JSON after two independent checks."""
    database = database.resolve()
    destination = destination.resolve()
    if not database.is_file():
        raise SystemExit(f"SQLite event authority is absent at {database}")
    if destination.exists():
        raise SystemExit(f"refusing to overwrite event export {destination}")
    require_ignored_storage(repo, destination)
    destination.parent.mkdir(parents=True, exist_ok=True)
    temporary_name: str | None = None
    try:
        with closing(
            sqlite3.connect(f"file:{database.as_posix()}?mode=ro", uri=True)
        ) as connection:
            connection.execute("PRAGMA query_only=ON")
            connection.execute("BEGIN")
            integrity = connection.execute("PRAGMA integrity_check").fetchone()
            if integrity != ("ok",):
                raise SystemExit("SQLite integrity_check failed; no export was written")
            maximum_id = connection.execute(
                "SELECT COALESCE(MAX(event_id),0) FROM pai_events"
            ).fetchone()[0]
            with tempfile.NamedTemporaryFile(
                "w", encoding="utf-8", newline="\n", delete=False,
                dir=destination.parent, prefix=f".{destination.name}.", suffix=".tmp"
            ) as output:
                temporary_name = output.name
                count = 0
                for event_json, expected_hash in connection.execute(
                    "SELECT event_json,integrity_hash FROM pai_events "
                    "ORDER BY storage_sequence"
                ):
                    actual_hash = hashlib.sha256(event_json.encode("utf-8")).hexdigest()
                    if actual_hash.casefold() != expected_hash.casefold():
                        raise SystemExit(
                            f"event integrity mismatch at export row {count + 1}"
                        )
                    output.write(event_json)
                    output.write("\n")
                    count += 1

            # Reopen both sources and compare every exact row before publish.
            with open(temporary_name, encoding="utf-8", newline="") as exported:
                rows = connection.execute(
                    "SELECT event_json FROM pai_events ORDER BY storage_sequence"
                )
                verified = 0
                for (event_json,) in rows:
                    line = exported.readline()
                    if not line or line.removesuffix("\n").removesuffix("\r") != event_json:
                        raise SystemExit(
                            f"event export parity failed at row {verified + 1}"
                        )
                    verified += 1
                if exported.readline():
                    raise SystemExit("event export contains an unexpected extra row")
            if verified != count:
                raise SystemExit("event export row count changed during verification")
        os.replace(temporary_name, destination)
        temporary_name = None
        return {"event_count": count, "maximum_event_id": maximum_id}
    finally:
        if temporary_name:
            Path(temporary_name).unlink(missing_ok=True)


def backup_sqlite_database(source: Path, destination: Path) -> None:
    """Create and integrity-check one recoverable online SQLite backup."""
    source = source.resolve()
    destination = destination.resolve()
    if not source.is_file():
        return
    if destination.exists():
        raise SystemExit(f"refusing to overwrite SQLite backup {destination}")
    destination.parent.mkdir(parents=True, exist_ok=True)
    with closing(
        sqlite3.connect(f"file:{source.as_posix()}?mode=ro", uri=True)
    ) as current, closing(sqlite3.connect(destination)) as backup:
        current.execute("PRAGMA query_only=ON")
        current.backup(backup)
        source_integrity = current.execute("PRAGMA integrity_check").fetchone()
        backup_integrity = backup.execute("PRAGMA integrity_check").fetchone()
        if source_integrity != ("ok",) or backup_integrity != ("ok",):
            raise SystemExit("SQLite cutover backup integrity_check failed")


def backup_memory_cutover_state(state: Path) -> Path | None:
    """Back up the two databases a memory cutover can change."""
    # Projection first, authority second: concurrent progress can leave the
    # copied event ledger ahead (reconcilable), never the projection ahead.
    databases = [state / "derived.sqlite3", state / "events.sqlite3"]
    if not any(database.is_file() for database in databases):
        return None
    stamp = datetime.now(timezone.utc).strftime("%Y%m%dT%H%M%S.%fZ")
    directory = state / "backups" / f"pre-memory-cutover-{stamp}"
    for database in databases:
        backup_sqlite_database(database, directory / database.name)
    return directory


def _file_sha256(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as stream:
        for block in iter(lambda: stream.read(1024 * 1024), b""):
            digest.update(block)
    return digest.hexdigest()


def _sqlite_integrity(connection: sqlite3.Connection, label: str) -> None:
    rows = connection.execute("PRAGMA integrity_check").fetchall()
    if rows != [("ok",)]:
        raise SystemExit(f"{label} SQLite integrity_check failed")


def inspect_state_databases(events_path: Path, derived_path: Path) -> dict[str, object]:
    """Verify the copied authority/projection pair without mutating either file."""
    for label, path in (("event authority", events_path),
                        ("derived projection", derived_path)):
        if not path.is_file():
            raise SystemExit(f"{label} is absent at {path}")

    with closing(sqlite3.connect(
        f"file:{events_path.resolve().as_posix()}?mode=ro", uri=True
    )) as events:
        events.execute("PRAGMA query_only=ON")
        events.execute("BEGIN")
        _sqlite_integrity(events, "event authority")
        try:
            storage_id_row = events.execute(
                "SELECT meta_value FROM pai_storage_meta WHERE meta_key='storage_id'"
            ).fetchone()
            if not storage_id_row or not storage_id_row[0]:
                raise SystemExit("event authority storage_id is absent")
            storage_id = storage_id_row[0]
            event_count = 0
            maximum_event_id = 0
            maximum_position = 0
            for position, event_id, event_json, expected_hash in events.execute(
                "SELECT storage_sequence,event_id,event_json,integrity_hash "
                "FROM pai_events ORDER BY storage_sequence"
            ):
                actual_hash = hashlib.sha256(event_json.encode("utf-8")).hexdigest()
                if actual_hash.casefold() != expected_hash.casefold():
                    raise SystemExit(
                        f"event authority integrity mismatch at storage position {position}"
                    )
                event_count += 1
                maximum_event_id = max(maximum_event_id, event_id)
                maximum_position = max(maximum_position, position)
        except sqlite3.Error as error:
            raise SystemExit(f"event authority schema is invalid: {error}") from error

    with closing(sqlite3.connect(
        f"file:{derived_path.resolve().as_posix()}?mode=ro", uri=True
    )) as derived:
        derived.execute("PRAGMA query_only=ON")
        derived.execute("BEGIN")
        _sqlite_integrity(derived, "derived projection")
        try:
            imported = derived.execute(
                "SELECT node_count,edge_count,seal_hash FROM pai_memory_imports "
                "WHERE import_name='canonical'"
            ).fetchone()
            if not imported:
                raise SystemExit("derived projection has no sealed canonical import")
            _baseline_node_count, _baseline_edge_count, baseline_seal = imported
            actual_nodes = derived.execute(
                "SELECT COUNT(*) FROM pai_memory_nodes"
            ).fetchone()[0]
            actual_edges = derived.execute(
                "SELECT COUNT(*) FROM pai_memory_edges"
            ).fetchone()[0]
            # The import seal authenticates the immutable migration baseline,
            # not the current materialized generation. Ledger-applied memory
            # mutations legitimately change current row counts afterward.
            projection = derived.execute(
                "SELECT baseline_seal,storage_id,through_event_id,"
                "through_storage_position FROM pai_memory_projection "
                "WHERE projection_name='canonical'"
            ).fetchone()
            if not projection:
                raise SystemExit("derived projection authority binding is absent")
            projected_seal, projected_storage, through_id, through_position = projection
            if projected_seal != baseline_seal:
                raise SystemExit("derived projection baseline seal does not match import")
            if projected_storage != storage_id:
                raise SystemExit("derived projection is bound to another event authority")
            if through_id > maximum_event_id or through_position > maximum_position:
                raise SystemExit("derived projection is ahead of the event backup")
        except sqlite3.Error as error:
            raise SystemExit(f"derived projection schema is invalid: {error}") from error

    return {
        "storage_id": storage_id,
        "event_count": event_count,
        "maximum_event_id": maximum_event_id,
        "maximum_storage_position": maximum_position,
        "memory_node_count": actual_nodes,
        "memory_edge_count": actual_edges,
        "memory_baseline_seal": baseline_seal,
        "memory_through_event_id": through_id,
        "memory_through_storage_position": through_position,
    }


def create_state_backup(repo: Path, state: Path, destination: Path) -> dict[str, object]:
    """Publish one verified two-database snapshot; never overwrite a backup."""
    state = state.resolve()
    destination = destination.resolve()
    if destination.exists():
        raise SystemExit(f"refusing to overwrite state backup {destination}")
    try:
        destination.relative_to(state)
    except ValueError:
        pass
    else:
        raise SystemExit("state backup destination must be outside the live state")
    require_ignored_storage(repo, destination)
    destination.parent.mkdir(parents=True, exist_ok=True)
    temporary = Path(tempfile.mkdtemp(
        prefix=f".{destination.name}.", suffix=".tmp", dir=destination.parent
    ))
    order = ["derived.sqlite3", "events.sqlite3"]
    try:
        for name in order:
            backup_sqlite_database(state / name, temporary / name)
            if not (temporary / name).is_file():
                raise SystemExit(f"state backup requires {name}")
        evidence = inspect_state_databases(
            temporary / "events.sqlite3", temporary / "derived.sqlite3"
        )
        manifest: dict[str, object] = {
            "schema_version": 1,
            "status": "complete",
            "created_at": datetime.now(timezone.utc).isoformat(),
            "snapshot_order": order,
            "databases": {
                name: {
                    "bytes": (temporary / name).stat().st_size,
                    "sha256": _file_sha256(temporary / name),
                }
                for name in order
            },
            "evidence": evidence,
        }
        (temporary / "backup-manifest.json").write_text(
            json.dumps(manifest, indent=2, sort_keys=True) + "\n", encoding="utf-8"
        )
        os.replace(temporary, destination)
        return manifest
    finally:
        if temporary.exists():
            shutil.rmtree(temporary)


def verify_state_backup(backup: Path) -> dict[str, object]:
    """Verify a published backup pair and its content-free manifest."""
    backup = backup.resolve()
    manifest_path = backup / "backup-manifest.json"
    try:
        manifest = json.loads(manifest_path.read_text(encoding="utf-8"))
    except (OSError, json.JSONDecodeError) as error:
        raise SystemExit(f"could not read backup manifest: {error}") from error
    if (
        not isinstance(manifest, dict)
        or manifest.get("schema_version") != 1
        or manifest.get("status") != "complete"
        or manifest.get("snapshot_order") != ["derived.sqlite3", "events.sqlite3"]
        or not isinstance(manifest.get("databases"), dict)
        or not isinstance(manifest.get("evidence"), dict)
    ):
        raise SystemExit("backup manifest contract is invalid")
    for name in ("derived.sqlite3", "events.sqlite3"):
        path = backup / name
        declared = manifest["databases"].get(name)
        if not path.is_file() or not isinstance(declared, dict):
            raise SystemExit(f"backup manifest lacks {name}")
        if declared.get("bytes") != path.stat().st_size:
            raise SystemExit(f"backup size differs for {name}")
        if declared.get("sha256") != _file_sha256(path):
            raise SystemExit(f"backup digest differs for {name}")
    evidence = inspect_state_databases(
        backup / "events.sqlite3", backup / "derived.sqlite3"
    )
    if evidence != manifest["evidence"]:
        raise SystemExit("backup database evidence differs from manifest")
    return {"schema_version": 1, "status": "verified", **evidence}


def qualify_restore_runtime(repo: Path, backup: Path) -> dict[str, object]:
    """Boot a ledger-only disposable restore and prove derived-memory rebuild."""
    backup = backup.resolve()
    expected = verify_state_backup(backup)
    with tempfile.TemporaryDirectory(prefix="pai-restore-qualification-") as temporary:
        restored = Path(temporary) / "state"
        restored.mkdir()
        shutil.copy2(backup / "events.sqlite3", restored / "events.sqlite3")
        environment = os.environ.copy()
        for name in tuple(environment):
            if name.startswith("PAI_WEB_") or name == "OPENROUTER_API_KEY":
                environment.pop(name, None)
        completed = subprocess.run(
            [
                sys.executable,
                str(repo / "scripts" / "pai_cli.py"),
                "--provider", "local",
                "--state-dir", str(restored),
            ],
            cwd=repo,
            env=environment,
            input="/quit\n",
            text=True,
            capture_output=True,
            timeout=300,
            check=False,
        )
        if completed.returncode != 0 or "pAI is ready" not in completed.stdout:
            diagnostic = (completed.stdout + "\n" + completed.stderr)[-4000:]
            raise SystemExit(
                "providerless ledger-only restore did not reach ready state:\n"
                + diagnostic
            )
        rebuilt = inspect_state_databases(
            restored / "events.sqlite3", restored / "derived.sqlite3"
        )
        for key in (
            "storage_id", "event_count", "maximum_event_id",
            "memory_node_count", "memory_edge_count", "memory_baseline_seal",
        ):
            if rebuilt[key] != expected[key]:
                raise SystemExit(
                    f"ledger-only rebuild differs from backup evidence for {key}"
                )
        return {
            "schema_version": 1,
            "status": "runtime-qualified",
            "provider_calls": 0,
            "derived_rebuilt_from_ledger": True,
            **rebuilt,
        }


def parse_args(argv: list[str] | None = None) -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Chat with pAI and operate its contained lifecycle surface."
    )
    parser.add_argument("--provider", choices=("openrouter", "local"), default="openrouter")
    parser.add_argument("--endpoint", help="local provider endpoint override")
    parser.add_argument("--model", help="local provider model override")
    parser.add_argument(
        "--embedding-endpoint",
        default="http://127.0.0.1:11435/api/embeddings",
        help="local Nomic-compatible query embedding endpoint",
    )
    parser.add_argument(
        "--provider-profile", default="openrouter-human-renderer-mimo-v2.5"
    )
    parser.add_argument(
        "--openrouter-model",
        help=(
            "override the selected OpenRouter profile's model slug while "
            "retaining its routing, privacy, tool and maximum-price policy"
        ),
    )
    parser.add_argument(
        "--openrouter-zdr",
        choices=("require", "allow-non-zdr"),
        help=(
            "override per-request ZDR routing; default inherits the selected "
            "provider profile"
        ),
    )
    parser.add_argument(
        "--openrouter-data-collection",
        choices=("deny", "allow"),
        help=(
            "override whether routing may use providers that collect data; "
            "default inherits the selected provider profile"
        ),
    )
    parser.add_argument(
        "--openrouter-reasoning",
        choices=("profile", "model-default", "enabled", "disabled"),
        help=(
            "override reasoning behavior; model overrides default to "
            "model-default, which omits a model-specific profile switch"
        ),
    )
    parser.add_argument(
        "--openrouter-reasoning-effort",
        choices=("minimal", "low", "medium", "high", "max"),
        help="enable reasoning at an explicit OpenRouter effort",
    )
    parser.add_argument(
        "--request-limit", type=int,
        help=(
            "deprecated compatibility option; request counts are telemetry "
            "only and provider authority is cost-bounded"
        ),
    )
    parser.add_argument("--cost-ceiling-usd")
    parser.add_argument(
        "--knowledge-graph-budget-usd",
        help=(
            "cumulative durable knowledge-graph formation exposure ceiling; "
            "requires --knowledge-graph-formation and accepts at most six "
            "decimal places"
        ),
    )
    parser.add_argument(
        "--knowledge-graph-prior-exposure-usd",
        help=(
            "charged or outcome-unknown exposure inherited from predecessor "
            "graph generations; requires an explicit cumulative graph budget"
        ),
    )
    parser.add_argument(
        "--context-graph-runtime-profile",
        choices=(
            "direct-v6",
            "reviewed-inference-v7",
            "reviewed-inference-v8",
            "reviewed-inference-v9",
        ),
        default="direct-v6",
        help=(
            "atomic graph owner/protocol pair; reviewed inference starts a "
            "replacement projection and requires explicit budget accounting"
        ),
    )
    parser.add_argument(
        "--context-graph-budget-authorization-id",
        help=(
            "explicit cumulative authorization lineage shared by live graph "
            "formation and selected-phase qualification"
        ),
    )
    parser.add_argument(
        "--state-dir", type=Path,
        help="state directory (defaults to the persistent .clone-state)"
    )
    event_setup = parser.add_mutually_exclusive_group()
    event_setup.add_argument(
        "--migrate-events", action="store_true",
        help="explicitly import the existing JSONL ledger into SQLite"
    )
    operator_action = parser.add_mutually_exclusive_group()
    operator_action.add_argument(
        "--export-events", type=Path, metavar="DESTINATION_JSONL",
        help="export and verify the authoritative SQLite ledger, then exit"
    )
    event_setup.add_argument(
        "--initialize-events", action="store_true",
        help="explicitly initialize empty SQLite event history for a new state"
    )
    operator_action.add_argument(
        "--backup-state", type=Path, metavar="DESTINATION_DIRECTORY",
        help="create a verified online backup of SQLite authority and projections",
    )
    operator_action.add_argument(
        "--verify-restore", type=Path, metavar="BACKUP_DIRECTORY",
        help="verify a state backup without opening live state or a provider",
    )
    parser.add_argument(
        "--migrate-memory", action="store_true",
        help=(
            "one-time: append the sealed derived-memory import as a ledger "
            "baseline before selecting SQLite memory authority"
        ),
    )
    parser.add_argument(
        "--migration-only", action="store_true",
        help="perform the explicitly requested migrations, report, and exit",
    )
    parser.add_argument(
        "--clone-postgres-port", type=int,
        help=(
            "one-time memory migration source: labelled local PostgreSQL "
            "clone port (valid only with --migrate-memory)"
        ),
    )
    parser.add_argument(
        "--clone-postgres-host", default="127.0.0.1",
        help=(
            "one-time memory migration source host; restricted to local "
            "loopback or host.docker.internal"
        ),
    )
    parser.add_argument(
        "--memory-source-agent-id",
        help="source-agent identity recorded on a migrated semantic-memory baseline",
    )
    parser.add_argument(
        "--memory-migration-manifest-sha256",
        help="SHA-256 of the sealed migration manifest bound to the memory baseline",
    )
    parser.add_argument(
        "--persona", default="dev",
        help="generic 'dev' or a profile name below STATE/personas/"
    )
    parser.add_argument(
        "--import-persona", nargs=2, metavar=("NAME", "SOURCE_JSON"),
        dest="persona_import",
        help="import identity/voice into ignored local state, then exit"
    )
    parser.add_argument(
        "--context-profile", default="solicited-conversation-dev"
    )
    parser.add_argument(
        "--mind-loop", choices=("work-state", "recursive"),
        default="work-state",
        help="conversation owner; recursive selects the durable trampoline",
    )
    parser.add_argument(
        "--recursive-tools", action="store_true",
        help=(
            "enable native bash/lisp-eval under the selected execution "
            "envelope (V2)"
        ),
    )
    parser.add_argument(
        "--curiosity-wake-seconds", type=int, default=0,
        help=(
            "enable durable private curiosity and investigate at most one "
            "salient curiosity per interval (recursive loop only; 0 disables)"
        ),
    )
    parser.add_argument(
        "--deliberate-curiosity", action="store_true",
        help=(
            "also let ordinary conversation deliberately preserve an explicit "
            "'I should think about that' question (recursive loop only)"
        ),
    )
    parser.add_argument(
        "--curiosity-reach-out", action="store_true",
        help=(
            "allow one evidence-linked autonomous message when a reviewed "
            "private finding is unusually useful to share (recursive loop only)"
        ),
    )
    parser.add_argument(
        "--curiosity-briefing", action="store_true",
        help=(
            "allow quiet cognition to send bounded private-state rows to the "
            "configured model and durably cache a compact briefing "
            "(recursive loop only; default off)"
        ),
    )
    parser.add_argument(
        "--curiosity-consolidation", action=argparse.BooleanOptionalAction,
        default=None,
        help=(
            "allow quiet cognition to send bounded open-curiosity rows and "
            "persona orientation to the configured model for non-destructive "
            "semantic consolidation (on by default for the recursive loop; "
            "use --no-curiosity-consolidation to disable)"
        ),
    )
    parser.add_argument(
        "--affect-baseline-event-id", type=int,
        help=(
            "exclusive event ID immediately before the inspected affect window; "
            "enables the read-only /affect-inspect command"
        ),
    )
    parser.add_argument(
        "--episodic-memory", action="store_true",
        help=(
            "allow bounded provider sealing of quiet conversation episodes "
            "and automatic persona-scoped episodic context injection "
            "(recursive loop with a positive quiet interval only)"
        ),
    )
    parser.add_argument(
        "--knowledge-graph-formation", action="store_true",
        help=(
            "allow quiet provider-backed generic graph formation from sealed "
            "episodes (requires --episodic-memory and a positive quiet interval; "
            "recursive loop only; default off)"
        ),
    )
    parser.add_argument(
        "--knowledge-graph-rebuild-only", action="store_true",
        help=(
            "run the recursive timer only for formation from already sealed "
            "episodes; excludes episode sealing, curiosity, web, and tools"
        ),
    )
    parser.add_argument(
        "--private-budget-percent", type=int, default=30,
        help=(
            "maximum percentage of the total provider cost budget available "
            "to all private cognition; request counts are telemetry only "
            "(0-100; default 30)"
        ),
    )
    parser.add_argument(
        "--private-reasoning-effort",
        choices=("minimal", "low", "medium", "high", "max"),
        default="minimal",
        help=(
            "reasoning effort for autonomous recursive model calls; operator "
            "conversation keeps the selected OpenRouter reasoning profile "
            "(default minimal)"
        ),
    )
    parser.add_argument(
        "--loop-trace", choices=("off", "compact", "full"), default="compact",
        help="show observable recursive model/tool activity in the terminal",
    )
    parser.add_argument(
        "--context-trace", choices=("off", "metadata", "full"), default="off",
        help=(
            "write private credential-redacted model request/response diagnostics "
            "below the selected state directory"
        ),
    )
    parser.add_argument(
        "--show-rejected", action="store_true", help="show rejected private output"
    )
    parser.add_argument(
        "--show-memory-context", action="store_true",
        help="allow /memory-inspect to print selected private memory content",
    )
    parser.add_argument(
        "--verbose-startup", action="store_true",
        help="show ASDF/compiler diagnostics during startup"
    )
    parser.add_argument(
        "--web", action="store_true",
        help="run the authenticated web terminal alongside the CLI",
    )
    parser.add_argument(
        "--web-address",
        help="literal listen address; defaults to 127.0.0.1 with --web",
    )
    parser.add_argument(
        "--web-port", type=int,
        help="web listen port; defaults to 8080 with --web",
    )
    parser.add_argument(
        "--web-file-mutation", action="store_true",
        help="permit authenticated web file save/upload within the contained root",
    )
    parser.add_argument(
        "--setup", action="store_true", help="install project-local Lisp dependencies"
    )
    return parser.parse_args(argv)


def curiosity_consolidation_enabled(args: argparse.Namespace) -> bool:
    """Default consolidation on only inside the selected recursive loop."""
    return (
        args.mind_loop == "recursive"
        and args.curiosity_consolidation is not False
    )


def affect_inspection_environment(args: argparse.Namespace) -> dict[str, str]:
    """Retain one explicit, exclusive read-only affect window baseline."""
    baseline = args.affect_baseline_event_id
    if baseline is None:
        return {}
    if baseline < 0:
        raise SystemExit("--affect-baseline-event-id must be non-negative")
    if args.mind_loop != "recursive":
        raise SystemExit("--affect-baseline-event-id requires --mind-loop recursive")
    return {"PAI_AFFECT_BASELINE_EVENT_ID": str(baseline)}


def web_environment(
    args: argparse.Namespace, inherited: dict[str, str]
) -> dict[str, str]:
    """Validate and return web configuration without copying credentials."""
    if not args.web:
        if args.web_address is not None or args.web_port is not None:
            raise SystemExit("--web-address/--web-port require --web")
        if args.web_file_mutation:
            raise SystemExit("--web-file-mutation requires --web")
        return {}

    address = args.web_address or "127.0.0.1"
    try:
        parsed_address = ipaddress.ip_address(address)
    except ValueError as error:
        raise SystemExit("--web-address must be a literal IPv4 or IPv6 address") from error
    if not (
        parsed_address.is_loopback
        or parsed_address.is_private
        or parsed_address.is_unspecified
    ):
        raise SystemExit("--web-address must be loopback, private, or a wildcard address")
    port = args.web_port or 8080
    if not 1 <= port <= 65535:
        raise SystemExit("--web-port must be between 1 and 65535")

    if not parsed_address.is_loopback:
        password = inherited.get("PAI_WEB_PASSWORD")
        if not password:
            password_file = inherited.get("PAI_WEB_PASSWORD_FILE")
            if password_file:
                try:
                    password = Path(password_file).read_text(encoding="utf-8").strip()
                except OSError as error:
                    raise SystemExit(f"could not read PAI_WEB_PASSWORD_FILE: {error}") from error
        if not password or len(password) < 24:
            raise SystemExit(
                "non-loopback --web requires PAI_WEB_PASSWORD or "
                "PAI_WEB_PASSWORD_FILE with at least 24 characters"
            )

    result = {
        "PAI_WEB_ENABLED": "1",
        "PAI_WEB_ADDRESS": address,
        "PAI_WEB_PORT": str(port),
    }
    if args.web_file_mutation:
        result["PAI_WEB_FILE_MUTATION"] = "authenticated"
    return result


def memory_migration_environment(args: argparse.Namespace) -> dict[str, str]:
    """Return the only PostgreSQL configuration admitted by the CLI.

    Normal conversation startup intentionally receives no usable PostgreSQL
    database configuration.  The one-time migration is closed to a labelled
    loopback clone and a port the operator names explicitly.
    """
    port = args.clone_postgres_port
    host = args.clone_postgres_host
    source_agent_id = args.memory_source_agent_id
    manifest_sha256 = args.memory_migration_manifest_sha256
    if (source_agent_id is None) != (manifest_sha256 is None):
        raise SystemExit(
            "--memory-source-agent-id and --memory-migration-manifest-sha256 "
            "must be supplied together"
        )
    if (source_agent_id is not None or manifest_sha256 is not None) and not args.migrate_memory:
        raise SystemExit("memory source provenance is only valid with --migrate-memory")
    if args.migration_only and not (args.migrate_events or args.migrate_memory):
        raise SystemExit("--migration-only requires an explicit migration")
    if port is not None and not args.migrate_memory:
        raise SystemExit("--clone-postgres-port is only valid with --migrate-memory")
    if host != "127.0.0.1" and not args.migrate_memory:
        raise SystemExit("--clone-postgres-host is only valid with --migrate-memory")
    if not args.migrate_memory:
        return {}
    if port is None:
        raise SystemExit("--migrate-memory requires --clone-postgres-port")
    if not 1 <= port <= 65535:
        raise SystemExit("--clone-postgres-port must be between 1 and 65535")
    if host not in {"127.0.0.1", "localhost", "::1", "host.docker.internal"}:
        raise SystemExit("--clone-postgres-host must name an approved local host")
    result = {
        "PAI_MEMORY_MIGRATION_SOURCE": "labelled-local-postgres-clone",
        "PAI_PG_BACKUP": "off",
        "PAI_PG_HOST": host,
        "PAI_PG_PORT": str(port),
        "PAI_PG_DATABASE": "pai_memory",
        "PAI_PG_USER": "pai",
        "PAI_PG_PASSWORD": "pai_local_dev_only",
        "PAI_DEV_DATABASE_LABEL": "clone",
    }
    if source_agent_id is not None:
        if not re.fullmatch(r"[A-Za-z0-9][A-Za-z0-9_.-]{0,127}", source_agent_id):
            raise SystemExit("--memory-source-agent-id must be a stable identifier")
        if not re.fullmatch(r"[0-9a-f]{64}", manifest_sha256):
            raise SystemExit(
                "--memory-migration-manifest-sha256 must be lowercase SHA-256"
            )
        result.update(
            {
                "PAI_MEMORY_MIGRATION_SOURCE_AGENT_ID": source_agent_id,
                "PAI_MEMORY_MIGRATION_MANIFEST_SHA256": manifest_sha256,
            }
        )
    return result


def run(args: argparse.Namespace) -> int:
    if not 0 <= args.curiosity_wake_seconds <= 86400:
        raise SystemExit("--curiosity-wake-seconds must be between 0 and 86400")
    if args.curiosity_wake_seconds and args.mind_loop != "recursive":
        raise SystemExit("--curiosity-wake-seconds requires --mind-loop recursive")
    if args.deliberate_curiosity and args.mind_loop != "recursive":
        raise SystemExit("--deliberate-curiosity requires --mind-loop recursive")
    if args.curiosity_reach_out and args.mind_loop != "recursive":
        raise SystemExit("--curiosity-reach-out requires --mind-loop recursive")
    if args.curiosity_briefing and args.mind_loop != "recursive":
        raise SystemExit("--curiosity-briefing requires --mind-loop recursive")
    if args.curiosity_consolidation is True and args.mind_loop != "recursive":
        raise SystemExit("--curiosity-consolidation requires --mind-loop recursive")
    affect_environment = affect_inspection_environment(args)
    if args.episodic_memory and args.mind_loop != "recursive":
        raise SystemExit("--episodic-memory requires --mind-loop recursive")
    if args.episodic_memory and not args.curiosity_wake_seconds:
        raise SystemExit("--episodic-memory requires --curiosity-wake-seconds")
    if args.knowledge_graph_formation and args.mind_loop != "recursive":
        raise SystemExit("--knowledge-graph-formation requires --mind-loop recursive")
    if args.knowledge_graph_formation and not args.episodic_memory:
        raise SystemExit("--knowledge-graph-formation requires --episodic-memory")
    if args.knowledge_graph_formation and not args.curiosity_wake_seconds:
        raise SystemExit(
            "--knowledge-graph-formation requires --curiosity-wake-seconds"
        )
    if not 0 <= args.private_budget_percent <= 100:
        raise SystemExit("--private-budget-percent must be between 0 and 100")

    repo = Path(__file__).resolve().parent.parent
    if args.setup:
        setup_local_lisp(repo)
        return 0

    if args.verify_restore:
        print("Verifying backup and booting a disposable ledger-only restore...", flush=True)
        report = qualify_restore_runtime(repo, args.verify_restore)
        print(
            f"Qualified providerless restore with {report['event_count']} events, "
            f"{report['memory_node_count']} memory nodes, and "
            f"{report['memory_edge_count']} memory edges; derived state rebuilt "
            "from the event ledger"
        )
        return 0

    state = (args.state_dir or (repo / ".clone-state")).resolve()
    if args.state_dir:
        state.mkdir(parents=True, exist_ok=True)
    if not state.is_dir():
        raise SystemExit(f"clone state is absent at {state}")
    if args.backup_state:
        report = create_state_backup(repo, state, args.backup_state)
        print(
            f"Created verified state backup at {args.backup_state.resolve()} "
            f"through event {report['evidence']['maximum_event_id']}"
        )
        return 0
    migration_environment = memory_migration_environment(args)
    embedding_endpoint = validate_embedding_endpoint(args.embedding_endpoint)
    persona_name = validate_persona_name(args.persona)
    if args.persona_import:
        imported_name = validate_persona_name(args.persona_import[0])
        target = import_persona(repo, state, imported_name,
                                Path(args.persona_import[1]))
        print(f"Imported local persona {imported_name!r} at {target}")
        return 0
    persona_file: Path | None = None
    if persona_name != "dev":
        require_ignored_storage(repo, state)
        persona_file = state / "personas" / f"{persona_name}.json"
        if not persona_file.is_file():
            raise SystemExit(
                f"local persona {persona_name!r} is absent; use --import-persona"
            )

    if args.export_events:
        report = export_event_ledger(repo, state / "events.sqlite3", args.export_events)
        print(
            f"Exported {report['event_count']} verified events through id "
            f"{report['maximum_event_id']} to {args.export_events.resolve()}"
        )
        return 0

    provider_environment = {
        **knowledge_graph_budget_environment(args),
        **context_graph_runtime_environment(args),
        **knowledge_graph_rebuild_environment(args),
    }
    if args.provider == "local":
        provider_environment["PAI_CONVERSATION_PROVIDER_PROFILE"] = getattr(
            args, "local_provider_profile", "contained-cli-provider"
        )
    if args.provider == "openrouter":
        reasoning_mode, reasoning_effort = openrouter_reasoning_override(args)
        if not os.environ.get("OPENROUTER_API_KEY"):
            raise SystemExit("OPENROUTER_API_KEY is missing; no request was made")
        if args.request_limit is not None and args.request_limit <= 0:
            raise SystemExit("deprecated --request-limit, when supplied, must be positive")
        if args.request_limit is not None:
            print(
                "WARNING: --request-limit is deprecated and ignored; "
                "provider authority is cost-only.",
                file=sys.stderr,
            )
        if args.cost_ceiling_usd is None:
            raise SystemExit("OpenRouter requires an explicit --cost-ceiling-usd")
        try:
            ceiling = Decimal(args.cost_ceiling_usd)
        except InvalidOperation as error:
            raise SystemExit("--cost-ceiling-usd must be a decimal amount") from error
        microusd = ceiling * Decimal(1_000_000)
        if ceiling <= 0 or microusd != microusd.to_integral_value():
            raise SystemExit("--cost-ceiling-usd must be positive to six decimal places")
        profiles_path = repo / "config" / "conscious-provider-profiles.json"
        document = json.loads(profiles_path.read_text(encoding="utf-8"))
        profile = document.get("profiles", {}).get(args.provider_profile)
        if not isinstance(profile, dict) or profile.get("provider") != "openrouter":
            raise SystemExit("--provider-profile is not a declared OpenRouter profile")
        if profile.get("publication_role") != "proposal-only":
            raise SystemExit("provider profile is not authorized for conversation proposals")
        endpoint = profile.get("endpoint")
        model = (
            validate_openrouter_model_slug(args.openrouter_model)
            if args.openrouter_model
            else profile.get("model")
        )
        if args.endpoint or args.model:
            raise SystemExit(
                "OpenRouter endpoint and --model are profile-owned; use "
                "--openrouter-model to override only the model slug"
            )
        provider_environment = {
            **provider_environment,
            "PAI_CONVERSATION_PROVIDER_PROFILE": args.provider_profile,
            "PAI_CONSCIOUS_PROVIDER_PROFILES": str(profiles_path),
            "PAI_CONVERSATION_COST_CEILING_MICROUSD": str(int(microusd)),
        }
        if args.openrouter_model:
            provider_environment["PAI_CONVERSATION_MODEL_OVERRIDE"] = model
        if args.openrouter_zdr:
            provider_environment["PAI_CONVERSATION_ZDR_OVERRIDE"] = (
                args.openrouter_zdr
            )
        if args.openrouter_data_collection:
            provider_environment["PAI_CONVERSATION_DATA_COLLECTION_OVERRIDE"] = (
                args.openrouter_data_collection
            )
        if reasoning_mode:
            provider_environment["PAI_CONVERSATION_REASONING_OVERRIDE"] = (
                reasoning_mode
            )
        if reasoning_effort:
            provider_environment["PAI_CONVERSATION_REASONING_EFFORT"] = (
                reasoning_effort
            )
    else:
        if (
            args.openrouter_model
            or args.openrouter_zdr
            or args.openrouter_data_collection
            or args.openrouter_reasoning
            or args.openrouter_reasoning_effort
        ):
            raise SystemExit(
                "OpenRouter model/privacy/reasoning overrides require "
                "--provider openrouter"
            )
        endpoint = args.endpoint or "http://127.0.0.1:1234/v1/chat/completions"
        model = args.model or "qwen/qwen3.5-9b"

    if args.migrate_memory:
        backup = backup_memory_cutover_state(state)
        if backup:
            print(f"Verified pre-cutover SQLite backup: {backup}", flush=True)

    sbcl = local_sbcl(repo)
    environment = os.environ.copy()
    native = native_environment(sbcl)
    environment["PATH"] = native.get("PATH", environment.get("PATH", ""))
    if "SBCL_HOME" in native:
        environment["SBCL_HOME"] = native["SBCL_HOME"]
    environment.update(
        {
            "PAI_UNIFIED_CLI": "1",
            "PAI_CLI_ANSI": "1" if sys.stdout.isatty() else "0",
            "PAI_QUICKLISP_SETUP": str(quicklisp_setup(repo)),
            "PAI_SOURCE_ROOT": str(repo / "src"),
            "PAI_FILE_SEARCH_ROOT": str(repo),
            "PAI_TEMPLATES": str(repo / "templates"),
            "PAI_STATE_ROOT": str(state),
            "PAI_EVENT_STORAGE_DATABASE": str(state / "events.sqlite3"),
            "PAI_DERIVED_STORAGE_DATABASE": str(state / "derived.sqlite3"),
            "PAI_EVENT_STORAGE_MIGRATE": "1" if args.migrate_events else "0",
            "PAI_EVENT_STORAGE_INITIALIZE": (
                "1" if args.initialize_events else "0"
            ),
            "PAI_MEMORY_STORAGE_MIGRATE": "1" if args.migrate_memory else "0",
            "PAI_MIGRATION_ONLY": "1" if args.migration_only else "0",
            "PAI_NEAR_TERM_INTENTIONS": str(state / "near-term-intentions.json"),
            "PAI_COGNITION_RUNTIME": "conscious-state",
            "PAI_CONVERSATION_LOOP": args.mind_loop,
            "PAI_RECURSIVE_LOOP_TRACE": args.loop_trace,
            "PAI_CURIOSITY_WAKE_SECONDS": str(args.curiosity_wake_seconds),
            "PAI_DELIBERATE_CURIOSITY": (
                "1" if args.deliberate_curiosity else "0"
            ),
            "PAI_CURIOSITY_REACH_OUT": (
                "1" if args.curiosity_reach_out else "0"
            ),
            "PAI_CURIOSITY_BRIEFING": (
                "1" if args.curiosity_briefing else "0"
            ),
            "PAI_CURIOSITY_CONSOLIDATION": (
                "1" if curiosity_consolidation_enabled(args) else "0"
            ),
            "PAI_EPISODIC_MEMORY": "1" if args.episodic_memory else "0",
            "PAI_KNOWLEDGE_GRAPH_FORMATION": (
                "1" if args.knowledge_graph_formation else "0"
            ),
            "PAI_PRIVATE_BUDGET_PERCENT": str(args.private_budget_percent),
            "PAI_PRIVATE_REASONING_EFFORT": args.private_reasoning_effort,
            "PAI_CONTEXT_TRACE": args.context_trace,
            "PAI_OLLAMA_ENDPOINT": embedding_endpoint,
            "PAI_PG_BACKUP": "off",
            "PAI_PG_HOST": "127.0.0.1",
            # Any accidental PostgreSQL constructor reached during ordinary
            # startup fails before connecting. Only the explicit migration
            # environment above replaces this sentinel with a valid port.
            "PAI_PG_PORT": "0",
            "PAI_DEV_DATABASE_LABEL": "pai-cli-clone-no-database-connection",
            "PAI_AGENT_ID": getattr(args, "agent_id", "q45-conversation-dev"),
            "PAI_CONVERSATION_ENDPOINT": endpoint,
            "PAI_CONVERSATION_MODEL": model,
            "PAI_CONVERSATION_PERSONA": persona_name,
            "PAI_CONVERSATION_CONTEXT_PROFILE": args.context_profile,
            "PAI_CONSCIOUS_CONTEXT_PROFILES": str(
                repo / "config" / "conscious-context-profiles.json"
            ),
        }
    )
    for attribute, environment_name in (
        ("memory_embedding_model", "PAI_MEMORY_EMBEDDING_MODEL"),
        ("memory_embedding_revision", "PAI_MEMORY_EMBEDDING_REVISION"),
        ("memory_retrieval_embedding_model", "PAI_MEMORY_RETRIEVAL_EMBEDDING_MODEL"),
        ("memory_retrieval_embedding_revision", "PAI_MEMORY_RETRIEVAL_EMBEDDING_REVISION"),
        ("memory_vector_dimension", "PAI_MEMORY_VECTOR_DIMENSION"),
    ):
        value = getattr(args, attribute, None)
        if value is not None:
            environment[environment_name] = str(value)
    environment.update(migration_environment)
    environment.update(provider_environment)
    environment.update(affect_environment)
    environment.update(web_environment(args, environment))
    tool_environment = recursive_tools_environment(args, repo, state)
    if tool_environment:
        if tool_environment["PAI_RECURSIVE_TOOLS"] == "container-development-v1":
            warning = (
                "WARNING: recursive bash/lisp-eval are confined by the Docker "
                "execution envelope, but the mounted pAI workspace is writable."
            )
        else:
            warning = (
                "WARNING: recursive bash/lisp-eval are running with the current "
                "host user's authority; no filesystem sandbox is active."
            )
        print(warning, file=sys.stderr, flush=True)
        # Full live evaluation can read admitted API credentials. This is an
        # explicit operator tradeoff for the current cost-constrained keys;
        # unrelated inherited credentials remain outside the runtime.
        environment = tool_runtime_environment(environment, tool_environment)
    if persona_file:
        environment["PAI_CONVERSATION_PERSONA_FILE"] = str(persona_file)
    if args.show_rejected:
        environment["PAI_CONVERSATION_SHOW_REJECTED"] = "1"
    if args.show_memory_context:
        environment["PAI_CONVERSATION_SHOW_MEMORY_CONTEXT"] = "1"
    if args.verbose_startup:
        environment["PAI_STARTUP_VERBOSE"] = "1"

    host_cache = state / "host-cache"
    host_cache.mkdir(parents=True, exist_ok=True)
    environment["LOCALAPPDATA"] = str(host_cache)
    environment["XDG_CACHE_HOME"] = str(host_cache)

    completed = subprocess.run(
        [
            str(sbcl),
            "--dynamic-space-size",
            "3072",
            "--script",
            str(repo / "scripts" / "conscious-conversation.lisp"),
        ],
        cwd=repo,
        env=environment,
        check=False,
    )
    return completed.returncode


def main(argv: list[str] | None = None) -> int:
    return run(parse_args(argv))


if __name__ == "__main__":
    sys.exit(main())
