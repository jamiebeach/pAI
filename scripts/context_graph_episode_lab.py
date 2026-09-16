"""Local, provider-free episode replay and reviewed-graph exploration.

The committed code is generic.  A seed produced by this tool contains private
history and must remain below an ignored local directory such as ``.scratch``.
"""

from __future__ import annotations

import argparse
from contextlib import closing
import errno
import hashlib
import json
import math
import os
import queue
import re
import secrets
import shutil
import sqlite3
import subprocess
import sys
import tempfile
import threading
import time
from dataclasses import dataclass, field
from http import HTTPStatus
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer
from pathlib import Path
from typing import Any
from urllib.parse import urlparse

ROOT = Path(__file__).resolve().parents[1]
ASSET_DIR = ROOT / "scripts" / "context-graph-episode-lab"
SEED_SCHEMA_VERSION = 1
TOOL_REVISION = "context-graph-episode-lab-v1"
SEED_METADATA_ALLOWANCE_BYTES = 16 * 1024 * 1024
SEED_MINIMUM_FREE_RESERVE_BYTES = 1024 * 1024 * 1024
SAVED_RESULT_MAX_BYTES = 32 * 1024 * 1024


def canonical_bytes(value: Any) -> bytes:
    return json.dumps(value, sort_keys=True, separators=(",", ":"),
                      ensure_ascii=False).encode("utf-8")


def save_run_capture(result: dict[str, Any], output: Path) -> None:
    """Persist one immutable run artifact only when it fits the reopen bound."""
    from context_graph_receipt_case import save_capture
    if len(canonical_bytes(result)) + 1 > SAVED_RESULT_MAX_BYTES:
        raise ValueError("Saved result exceeds the write bound")
    save_capture(result, output)


def sha256_file(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as source:
        for block in iter(lambda: source.read(1024 * 1024), b""):
            digest.update(block)
    return digest.hexdigest()


def case_code_fingerprint() -> dict[str, str]:
    """Execution provenance, not the checkpoint semantic compatibility version."""
    sources = list((ROOT / "src").rglob("*.lisp"))
    sources += list((ROOT / "scripts").glob("context-graph-*.lisp"))
    sources += [ROOT / "pai.asd", Path(__file__)]
    return {path.relative_to(ROOT).as_posix(): sha256_file(path)
            for path in sorted(set(sources))}


def sqlite_read_only(path: Path) -> sqlite3.Connection:
    connection = sqlite3.connect(
        f"file:{path.resolve().as_posix()}?mode=ro&immutable=1",
                                 uri=True)
    connection.execute("PRAGMA query_only=ON")
    return connection


def sqlite_live_read_only(path: Path) -> sqlite3.Connection:
    """Open a live SQLite authority read-only while allowing WAL visibility."""
    connection = sqlite3.connect(
        f"file:{path.resolve().as_posix()}?mode=ro", uri=True
    )
    connection.execute("PRAGMA query_only=ON")
    return connection


def verify_sqlite(path: Path, required_table: str,
                  require_quiescent: bool = True) -> None:
    if not path.is_file():
        raise ValueError(f"SQLite snapshot is absent: {path}")
    wal = Path(str(path) + "-wal")
    if require_quiescent and wal.exists() and wal.stat().st_size:
        raise ValueError(f"SQLite snapshot is not quiescent: {path}")
    opener = sqlite_read_only if require_quiescent else sqlite_live_read_only
    with closing(opener(path)) as connection:
        result = connection.execute("PRAGMA quick_check").fetchone()
        if not result or result[0] != "ok":
            raise ValueError(f"SQLite quick_check failed: {path}")
        found = connection.execute(
            "SELECT 1 FROM sqlite_master WHERE type='table' AND name=?",
            (required_table,),
        ).fetchone()
        if not found:
            raise ValueError(f"SQLite snapshot lacks {required_table}: {path}")


def backup_sqlite(source: Path, destination: Path) -> None:
    """Create one transactionally consistent local copy of a live database."""
    with closing(sqlite_live_read_only(source)) as source_connection:
        with closing(sqlite3.connect(destination)) as destination_connection:
            source_connection.backup(destination_connection)
            destination_connection.commit()


def sqlite_allocated_bytes(path: Path, live: bool) -> int:
    """Return the complete database image size, including live WAL pages."""
    opener = sqlite_live_read_only if live else sqlite_read_only
    with closing(opener(path)) as connection:
        page_count = int(connection.execute("PRAGMA page_count").fetchone()[0])
        page_size = int(connection.execute("PRAGMA page_size").fetchone()[0])
    return page_count * page_size


def nearest_existing_directory(path: Path) -> Path:
    candidate = path.resolve()
    while not candidate.exists():
        parent = candidate.parent
        if parent == candidate:
            raise ValueError(f"No existing parent for seed output: {path}")
        candidate = parent
    if not candidate.is_dir():
        candidate = candidate.parent
    return candidate


def require_seed_capacity(output: Path, required_bytes: int) -> None:
    free_bytes = shutil.disk_usage(nearest_existing_directory(output.parent)).free
    minimum_bytes = required_bytes + SEED_MINIMUM_FREE_RESERVE_BYTES
    if free_bytes < minimum_bytes:
        raise ValueError(
            "Insufficient free space for immutable seed: "
            f"need {required_bytes} bytes plus "
            f"{SEED_MINIMUM_FREE_RESERVE_BYTES} bytes reserve; "
            f"only {free_bytes} bytes free"
        )


def harden_seed_files(directory: Path) -> None:
    """Apply read-only mode where supported; hashes remain the authority."""
    unsupported = {errno.EACCES, errno.EPERM, errno.ENOTSUP, errno.EOPNOTSUPP}
    for path in directory.iterdir():
        try:
            path.chmod(0o444)
        except OSError as condition:
            if condition.errno not in unsupported:
                raise


def decode_record(event: dict[str, Any]) -> dict[str, Any]:
    payload = event.get("payload")
    if not isinstance(payload, dict):
        return {}
    text = payload.get("record_json")
    if not isinstance(text, str):
        return {}
    try:
        value = json.loads(text)
    except (TypeError, ValueError):
        return {}
    return value if isinstance(value, dict) else {}


def unique_strings(values: list[Any], limit: int = 12) -> list[str]:
    result: list[str] = []
    seen: set[str] = set()
    for value in values:
        if not isinstance(value, str) or not value.strip():
            continue
        normalized = value.strip()
        key = normalized.casefold()
        if key not in seen:
            result.append(normalized)
            seen.add(key)
        if len(result) >= limit:
            break
    return result


def build_catalog(events_path: Path, agent_id: str, persona_id: str) -> dict[str, Any]:
    with closing(sqlite_read_only(events_path)) as connection:
        rows = connection.execute(
            "SELECT event_id,event_type,event_json FROM pai_events "
            "WHERE agent_id=? AND event_type IN "
            "('conversation-episode-sealed','context-graph-identity-opened',"
            "'context-graph-identity-phase','context-graph-identity-completed',"
            "'context-graph-identity-failed') ORDER BY event_id",
            (agent_id,),
        ).fetchall()

    sealed: dict[int, dict[str, Any]] = {}
    opens: dict[int, dict[str, Any]] = {}
    phases: dict[int, list[dict[str, Any]]] = {}
    terminals: dict[int, list[dict[str, Any]]] = {}
    for event_id, event_type, event_json in rows:
        try:
            event = json.loads(event_json)
        except (TypeError, ValueError):
            continue
        if not isinstance(event, dict):
            continue
        if event_type == "conversation-episode-sealed":
            payload = event.get("payload")
            if isinstance(payload, dict) and payload.get("persona_id") == persona_id:
                sealed[event_id] = event
        elif event_type == "context-graph-identity-opened":
            record = decode_record(event)
            episode_event_id = record.get("episode_event_id")
            if isinstance(episode_event_id, int):
                opens[event_id] = {"event": event, "record": record,
                                   "episode_event_id": episode_event_id}
        elif event_type == "context-graph-identity-phase":
            caused_by = event.get("caused_by")
            if isinstance(caused_by, int):
                phases.setdefault(caused_by, []).append(event)
        else:
            caused_by = event.get("caused_by")
            if isinstance(caused_by, int):
                terminals.setdefault(caused_by, []).append(event)

    by_episode: dict[int, list[tuple[int, dict[str, Any]]]] = {}
    for open_id, opening in opens.items():
        by_episode.setdefault(opening["episode_event_id"], []).append(
            (open_id, opening)
        )

    episodes: list[dict[str, Any]] = []
    counts = {"completed": 0, "failed": 0, "opened": 0, "queued": 0}
    for sealed_id, event in sealed.items():
        payload = event["payload"]
        attempts: list[dict[str, Any]] = []
        suggestions: list[Any] = list(payload.get("entities") or [])
        for open_id, opening in sorted(by_episode.get(sealed_id, [])):
            terminal_events = sorted(terminals.get(open_id, []),
                                     key=lambda row: row.get("id", 0))
            terminal = terminal_events[-1] if terminal_events else None
            terminal_type = terminal.get("type") if terminal else None
            terminal_record = decode_record(terminal) if terminal else {}
            result = terminal_record.get("result")
            proposal = result.get("proposal") if isinstance(result, dict) else None
            if isinstance(proposal, dict):
                for descriptor in proposal.get("entities") or []:
                    if isinstance(descriptor, dict):
                        suggestions.append(descriptor.get("label"))
            phase_rows = phases.get(open_id, [])
            attempts.append({
                "opened_event_id": open_id,
                "terminal_event_id": terminal.get("id") if terminal else None,
                "terminal_type": terminal_type,
                "protocol": opening["record"].get("formation_protocol"),
                "attempt": opening["record"].get("attempt"),
                "batch_index": opening["record"].get("batch_index"),
                "phase_count": len(phase_rows),
                "phase_outcomes": [
                    {"phase": decode_record(row).get("phase"),
                     "outcome": decode_record(row).get("outcome")}
                    for row in phase_rows
                ],
                "proposal_entity_count": len(proposal.get("entities") or [])
                if isinstance(proposal, dict) else 0,
                "proposal_relationship_count": len(
                    proposal.get("relationships") or []
                ) if isinstance(proposal, dict) else 0,
                "failure_class": terminal_record.get("failure_class"),
                "failure_reason": terminal_record.get("reason"),
            })
        terminal_types = [row.get("terminal_type") for row in attempts]
        if "context-graph-identity-completed" in terminal_types:
            status = "completed"
        elif "context-graph-identity-failed" in terminal_types:
            status = "failed"
        elif attempts:
            status = "opened"
        else:
            status = "queued"
        counts[status] += 1
        completed = next((row for row in reversed(attempts)
                          if row["terminal_type"] ==
                          "context-graph-identity-completed"), None)
        first_open = attempts[0]["opened_event_id"] if attempts else None
        episodes.append({
            "episode_event_id": sealed_id,
            "episode_id": payload.get("episode_id"),
            "sealed_at": payload.get("sealed_at"),
            "timestamp": event.get("timestamp"),
            "synopsis": payload.get("synopsis", ""),
            "subjects": unique_strings(list(payload.get("subjects") or [])),
            "entities": unique_strings(list(payload.get("entities") or [])),
            "status": status,
            "attempts": attempts,
            "before_event_id": max(0, first_open - 1) if first_open else sealed_id,
            "after_event_id": completed["terminal_event_id"]
            if completed else sealed_id,
            "query_suggestions": unique_strings(suggestions),
        })
    episodes.sort(key=lambda row: row["episode_event_id"], reverse=True)
    return {
        "schema_version": 1,
        "catalog_revision": TOOL_REVISION,
        "episode_count": len(episodes),
        "status_counts": counts,
        "episodes": episodes,
    }


def select_partition(events_path: Path, agent_id: str | None,
                     persona_id: str | None) -> tuple[str, str, int, int]:
    with closing(sqlite_read_only(events_path)) as connection:
        agents = [row[0] for row in connection.execute(
            "SELECT DISTINCT agent_id FROM pai_events ORDER BY agent_id"
        )]
        if agent_id is None:
            if len(agents) != 1:
                raise ValueError("--agent-id is required for a multi-agent ledger")
            agent_id = agents[0]
        if agent_id not in agents:
            raise ValueError("Selected agent partition is absent")
        persona_rows = connection.execute(
            "SELECT event_json FROM pai_events WHERE agent_id=? AND "
            "event_type='conversation-episode-sealed' ORDER BY event_id DESC",
            (agent_id,),
        )
        personas: set[str] = set()
        for (text,) in persona_rows:
            try:
                value = json.loads(text)
                candidate = value.get("payload", {}).get("persona_id")
            except (TypeError, ValueError, AttributeError):
                continue
            if isinstance(candidate, str) and candidate:
                personas.add(candidate)
        if persona_id is None:
            if len(personas) != 1:
                raise ValueError("--persona-id is required when the ledger does not identify exactly one persona")
            persona_id = next(iter(personas))
        if persona_id not in personas:
            raise ValueError("Selected persona partition is absent")
        head_position, head_event_id = connection.execute(
            "SELECT COALESCE(MAX(storage_sequence),0),COALESCE(MAX(event_id),0) "
            "FROM pai_events WHERE agent_id=?", (agent_id,)
        ).fetchone()
    return agent_id, persona_id, head_position, head_event_id


def recovery_position(derived_path: Path) -> int | None:
    with closing(sqlite_read_only(derived_path)) as connection:
        try:
            row = connection.execute(
                "SELECT baseline_storage_position FROM "
                "pai_memory_projection_binding WHERE projection_name='canonical'"
            ).fetchone()
        except sqlite3.DatabaseError:
            return None
    return int(row[0]) if row else None


def make_seed(options: argparse.Namespace) -> Path:
    events_source = options.events.resolve()
    derived_source = options.derived.resolve()
    output = options.output.resolve()
    if output.exists():
        raise ValueError(f"Seed directory already exists: {output}")
    live_backup = options.storage_mode == "sqlite-backup"
    source_events_hash: str | None = None
    source_derived_hash: str | None = None
    if live_backup:
        if not getattr(options, "confirm_live_snapshot", False):
            raise ValueError("sqlite-backup requires --confirm-live-snapshot")
        verify_sqlite(events_source, "pai_events", require_quiescent=False)
        verify_sqlite(derived_source, "pai_projection_checkpoints",
                      require_quiescent=False)
        required_bytes = (
            sqlite_allocated_bytes(events_source, live=True)
            + sqlite_allocated_bytes(derived_source, live=True)
            + SEED_METADATA_ALLOWANCE_BYTES
        )
    else:
        if not getattr(options, "confirm_quiescent_backup", False):
            raise ValueError("copy/reference seed requires a quiescent backup confirmation")
        verify_sqlite(events_source, "pai_events")
        verify_sqlite(derived_source, "pai_projection_checkpoints")
        source_events_hash = sha256_file(events_source)
        source_derived_hash = sha256_file(derived_source)
        required_bytes = (
            events_source.stat().st_size + derived_source.stat().st_size
            + SEED_METADATA_ALLOWANCE_BYTES
        )
    if options.storage_mode != "reference":
        require_seed_capacity(output, required_bytes)
    output.parent.mkdir(parents=True, exist_ok=True)
    staging = Path(tempfile.mkdtemp(prefix="context-graph-seed-",
                                    dir=str(output.parent)))
    try:
        if live_backup:
            backup_sqlite(events_source, staging / "events.sqlite3")
            backup_sqlite(derived_source, staging / "derived.sqlite3")
        elif options.storage_mode == "copy":
            shutil.copy2(events_source, staging / "events.sqlite3")
            shutil.copy2(derived_source, staging / "derived.sqlite3")
        local_storage = options.storage_mode != "reference"
        seed_events = staging / "events.sqlite3" if local_storage else events_source
        seed_derived = staging / "derived.sqlite3" if local_storage else derived_source
        if local_storage:
            verify_sqlite(seed_events, "pai_events")
            verify_sqlite(seed_derived, "pai_projection_checkpoints")
            copied_events_hash = sha256_file(seed_events)
            copied_derived_hash = sha256_file(seed_derived)
            if (not live_backup
                    and (copied_events_hash != source_events_hash
                         or copied_derived_hash != source_derived_hash)):
                raise ValueError("Copied SQLite authority hash changed")
        agent_id, persona_id, head_position, head_event_id = select_partition(
            seed_events, options.agent_id, options.persona_id
        )
        events_hash = (copied_events_hash if local_storage
                       else source_events_hash)
        derived_hash = (copied_derived_hash if local_storage
                        else source_derived_hash)
        catalog = build_catalog(seed_events, agent_id, persona_id)
        catalog_bytes = canonical_bytes(catalog)
        (staging / "catalog.json").write_bytes(catalog_bytes + b"\n")
        manifest = {
            "schema_version": SEED_SCHEMA_VERSION,
            "tool_revision": TOOL_REVISION,
            "created_at_utc": time.strftime("%Y-%m-%dT%H:%M:%SZ", time.gmtime()),
            "seed_kind": "immutable-event-replay-authority",
            "storage_mode": options.storage_mode,
            "events": {"file": str(events_source) if options.storage_mode == "reference" else "events.sqlite3",
                       "sha256": events_hash, "size": seed_events.stat().st_size},
            "derived": {"file": str(derived_source) if options.storage_mode == "reference" else "derived.sqlite3",
                        "sha256": derived_hash, "size": seed_derived.stat().st_size},
            "catalog": {"file": "catalog.json",
                        "sha256": hashlib.sha256(catalog_bytes + b"\n").hexdigest()},
            "partition": {"agent_id": agent_id, "persona_id": persona_id},
            "head_storage_position": head_position,
            "head_event_id": head_event_id,
            "recovery_start_storage_position": recovery_position(seed_derived),
            "episode_count": catalog["episode_count"],
        }
        (staging / "manifest.json").write_bytes(canonical_bytes(manifest) + b"\n")
        if local_storage:
            harden_seed_files(staging)
        staging.rename(output)
    except BaseException:
        shutil.rmtree(staging, ignore_errors=True)
        raise
    return output


def load_seed(seed: Path, verify_hashes: bool = True) -> tuple[dict[str, Any], dict[str, Any]]:
    manifest_path = seed / "manifest.json"
    manifest = json.loads(manifest_path.read_text(encoding="utf-8"))
    if manifest.get("schema_version") != SEED_SCHEMA_VERSION:
        raise ValueError("Unsupported episode laboratory seed")
    if manifest.get("tool_revision") != TOOL_REVISION:
        raise ValueError("Episode laboratory seed revision mismatch")
    for name in ("events", "derived", "catalog"):
        descriptor = manifest.get(name)
        if not isinstance(descriptor, dict) or not isinstance(descriptor.get("file"), str):
            raise ValueError(f"Seed {name} descriptor is invalid")
        if manifest.get("storage_mode") == "reference" and name in {"events", "derived"}:
            path = Path(descriptor["file"]).resolve()
        else:
            path = (seed / descriptor["file"]).resolve()
        if ((manifest.get("storage_mode") != "reference" or name == "catalog")
                and path.parent != seed.resolve()) or not path.is_file():
            raise ValueError(f"Seed {name} path escapes or is absent")
        if verify_hashes and sha256_file(path) != descriptor.get("sha256"):
            raise ValueError(f"Immutable seed {name} hash mismatch")
    catalog = json.loads((seed / manifest["catalog"]["file"]).read_text(
        encoding="utf-8"
    ))
    return manifest, catalog


@dataclass
class LispWorker:
    process: subprocess.Popen[str]
    lock: threading.Lock
    log: list[str]
    config_file: Path
    container_name: str | None = None
    code_fingerprint: dict[str, str] | None = None
    lines: queue.Queue = field(default_factory=lambda: queue.Queue(maxsize=256))
    stopped: threading.Event = field(default_factory=threading.Event)

    def __post_init__(self) -> None:
        def deliver(line: str | None) -> None:
            while not self.stopped.is_set():
                try:
                    self.lines.put(line, timeout=0.1)
                    return
                except queue.Full:
                    pass
        def read_output() -> None:
            assert self.process.stdout is not None
            try:
                for line in self.process.stdout:
                    deliver(line)
                    if self.stopped.is_set():
                        break
            finally:
                deliver(None)
        threading.Thread(target=read_output, daemon=True).start()

    def _line(self, deadline: float) -> str:
        try:
            line = self.lines.get(timeout=max(0, deadline - time.monotonic()))
        except queue.Empty:
            self.close()
            raise TimeoutError("Isolated Lisp worker deadline exceeded; result incomplete") from None
        if line is None:
            raise RuntimeError("Lisp worker stopped:\n" + "".join(self.log[-120:]))
        if not line.startswith(("KG-EPISODE-LAB-CHECKPOINT ", "KG-EPISODE-LAB-RESULT ")):
            self.log.append(line)
        del self.log[:-100]
        return line

    @classmethod
    def start(cls, seed: Path, manifest: dict[str, Any], mode: str,
              image: str, *, budget_database: Path | None = None,
              budget_volume: str | None = None,
              budget_policy: dict[str, Any] | None = None) -> "LispWorker":
        events_descriptor = manifest["events"]
        events_path = (Path(events_descriptor["file"]).resolve()
                       if manifest.get("storage_mode") == "reference"
                       else (seed / events_descriptor["file"]).resolve())
        use_docker = mode == "docker" or (mode == "auto" and shutil.which("sbcl") is None)
        if (budget_database is not None or budget_volume is not None) != (budget_policy is not None):
            raise ValueError("Budget storage and policy must be configured together")
        if budget_database is not None and budget_volume is not None:
            raise ValueError("Select a budget database path or Docker volume, not both")
        if budget_volume is not None and not re.fullmatch(r"[A-Za-z0-9_.-]{1,128}", budget_volume):
            raise ValueError("Invalid Docker budget volume name")
        if budget_volume is not None and not use_docker:
            raise ValueError("A Docker budget volume requires Docker worker mode")
        if budget_database is not None:
            budget_database = budget_database.resolve()
            if not budget_database.is_file():
                raise ValueError("Configured budget database does not exist")
        budget_worker_path = ("/budget-state/events.sqlite3" if budget_volume else
                              (f"/budget-host/{budget_database.name}" if use_docker and budget_database else
                               (str(budget_database) if budget_database else None)))
        config = {
            "input_kind": manifest.get("input_kind", "database"),
            "events": "/lab/events.sqlite3" if use_docker else str(events_path),
            "agent_id": manifest["partition"]["agent_id"],
            "persona_id": manifest["partition"]["persona_id"],
            "recovery_start_storage_position":
                manifest.get("recovery_start_storage_position"),
        }
        if budget_policy is not None:
            config["budget_database"] = budget_worker_path
            config["budget_policy"] = budget_policy
        config_descriptor, config_name = tempfile.mkstemp(
            prefix="kg-episode-lab-", suffix=".json")
        os.close(config_descriptor)
        config_file = Path(config_name)
        config_file.write_bytes(canonical_bytes(config))
        environment = os.environ.copy()
        environment["PAI_CONTEXT_GRAPH_EPISODE_LAB_CONFIG"] = (
            "/lab/config.json" if use_docker else str(config_file)
        )
        environment.setdefault("PAI_QUICKLISP_SETUP", "/opt/quicklisp/setup.lisp")
        container_name = "pai-episode-lab-" + secrets.token_hex(12) if use_docker else None
        if use_docker:
            command = [
                "docker", "run", "--rm", "--name", container_name, "-i", "--network", "none",
                "--read-only", "--cap-drop", "ALL", "--security-opt",
                "no-new-privileges:true", "--pids-limit", "256",
                "--memory", "4g", "--tmpfs",
                "/tmp:rw,nosuid,nodev,noexec,size=256m,mode=1777",
                "--tmpfs", "/agent/state:rw,nosuid,nodev,noexec,size=16m,mode=0700",
                "-v", "pai-context-graph-lisp-cache:/home/pai/.cache",
                "-e", "PAI_CONTEXT_GRAPH_EPISODE_LAB_CONFIG=/lab/config.json",
                "-e", "PAI_QUICKLISP_SETUP=/opt/quicklisp/setup.lisp",
                "-v", f"{ROOT.resolve()}:/workspace:ro",
                "-v", f"{events_path}:/lab/events.sqlite3:ro",
                "-v", f"{config_file.resolve()}:/lab/config.json:ro",
            ]
            if budget_volume:
                command.extend(["-v", f"{budget_volume}:/budget-state"])
            elif budget_database:
                command.extend(["-v", f"{budget_database.parent}:/budget-host"])
            command.extend([image, "sbcl", "--dynamic-space-size", "2048", "--noinform",
                            "--disable-debugger", "--script",
                            "/workspace/scripts/context-graph-episode-lab.lisp"])
        else:
            command = ["sbcl", "--dynamic-space-size", "2048", "--noinform", "--disable-debugger", "--script",
                       str(ROOT / "scripts" / "context-graph-episode-lab.lisp")]
        process: subprocess.Popen[str] | None = None
        try:
            fingerprint = case_code_fingerprint()
            process = subprocess.Popen(
                command,
                cwd=ROOT, env=environment, stdin=subprocess.PIPE,
                stdout=subprocess.PIPE, stderr=subprocess.STDOUT,
                text=True, encoding="utf-8", bufsize=1,
            )
            worker = cls(process=process, lock=threading.Lock(), log=[],
                         config_file=config_file, container_name=container_name,
                         code_fingerprint=fingerprint)
            worker._wait_ready()
            if fingerprint != case_code_fingerprint():
                worker.close()
                raise RuntimeError("Sources changed during worker startup; restart the isolated worker")
            return worker
        except BaseException:
            if process is not None and process.poll() is None:
                process.terminate()
                try:
                    process.wait(timeout=10)
                except subprocess.TimeoutExpired:
                    process.kill()
            config_file.unlink(missing_ok=True)
            raise

    def _wait_ready(self, timeout_seconds: float = 180) -> None:
        deadline = time.monotonic() + timeout_seconds
        while True:
            line = self._line(deadline)
            if line.strip() == "KG-EPISODE-LAB-READY":
                return

    def call(self, request: dict[str, Any], timeout_seconds: float = 30,
             on_checkpoint=None) -> dict[str, Any]:
        deadline = time.monotonic() + timeout_seconds
        if not self.lock.acquire(timeout=max(0, timeout_seconds)):
            raise TimeoutError("Lisp worker busy; request not submitted")
        try:
            if self.code_fingerprint is not None and self.code_fingerprint != case_code_fingerprint():
                raise RuntimeError("Sources changed since worker startup; restart the isolated worker (no replay submitted)")
            if self.process.poll() is not None:
                raise RuntimeError("Lisp worker is not running")
            assert self.process.stdin is not None
            assert self.process.stdout is not None
            sent: queue.Queue = queue.Queue(maxsize=1)
            encoded = canonical_bytes(request).decode("utf-8") + "\n"
            def write_request() -> None:
                try:
                    self.process.stdin.write(encoded)
                    self.process.stdin.flush()
                    sent.put(None)
                except Exception as condition:
                    sent.put(condition)
            threading.Thread(target=write_request, daemon=True).start()
            try:
                failure = sent.get(timeout=max(0, deadline - time.monotonic()))
            except queue.Empty:
                self.close()
                raise TimeoutError("Lisp worker input deadline exceeded; result incomplete") from None
            if failure is not None:
                raise RuntimeError("Lisp worker request could not be submitted") from failure
            while True:
                line = self._line(deadline)
                if line.startswith("KG-EPISODE-LAB-CHECKPOINT "):
                    if on_checkpoint is None:
                        raise RuntimeError("Unexpected checkpoint frame")
                    on_checkpoint(json.loads(line.split(" ", 1)[1]))
                    continue
                if line.startswith("KG-EPISODE-LAB-RESULT "):
                    payload = line.split(" ", 1)[1]
                    try:
                        return json.loads(payload)
                    except json.JSONDecodeError as condition:
                        raise RuntimeError(
                            "Lisp worker emitted a non-single-line result "
                            f"({len(payload)} characters; JSON error at "
                            f"{condition.lineno}:{condition.colno})"
                        ) from condition
                if line.startswith("KG-EPISODE-LAB-ERROR "):
                    error = json.loads(line.split(" ", 1)[1]).get("error")
                    raise RuntimeError(error or "Lisp worker request failed")
        finally:
            self.lock.release()

    def close(self) -> None:
        self.stopped.set()
        if self.container_name:
            # Only this randomly named, disposable, network-disabled lab worker.
            try:
                subprocess.run(["docker", "kill", self.container_name],
                               stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL,
                               timeout=5, check=False)
            except (OSError, subprocess.TimeoutExpired):
                self.log.append("Isolated container termination could not be confirmed\n")
        if self.process.poll() is None:
            self.process.terminate()
        try:
            self.process.wait(timeout=2)
        except subprocess.TimeoutExpired:
            self.process.kill()
            self.process.wait(timeout=2)
        for stream in (self.process.stdin, self.process.stdout):
            if stream:
                stream.close()
        self.config_file.unlink(missing_ok=True)


class WorkerBudgetGateway:
    """Thin client for the worker's startup-sealed event-ledger policy."""
    def __init__(self, worker: LispWorker):
        self.worker = worker

    def snapshot(self) -> dict[str, Any]:
        return self.worker.call({"operation": "budget-snapshot"}, 30)

    def reserve(self, reservation_id: str, request_digest: str,
                phase: str, reserved_microusd: int) -> dict[str, Any]:
        return self.worker.call({
            "operation": "budget-reserve", "reservation_id": reservation_id,
            "request_digest": request_digest, "phase": phase,
            "reserved_microusd": reserved_microusd,
        }, 30)

    def settle(self, reservation_id: str, request_digest: str,
               charged_microusd: int, receipt_id: str) -> dict[str, Any]:
        return self.worker.call({
            "operation": "budget-settle", "reservation_id": reservation_id,
            "request_digest": request_digest,
            "charged_microusd": charged_microusd, "receipt_id": receipt_id,
        }, 30)


class OpenRouterSelectedPhaseTransport:
    """One Phala-only request with strict routing and verified usage evidence."""
    def __init__(self, timeout_seconds: float = 180):
        if (not isinstance(timeout_seconds, (int, float))
                or isinstance(timeout_seconds, bool) or not math.isfinite(timeout_seconds)
                or not 1 <= timeout_seconds <= 180):
            raise ValueError("Provider timeout must be between 1 and 180 seconds")
        self.timeout_seconds = float(timeout_seconds)

    def call(self, request: dict[str, Any]) -> dict[str, Any]:
        from context_graph_resolution_lab import provider_call_with_deadline
        provider = request.get("provider") if isinstance(request, dict) else None
        if (not isinstance(provider, dict) or provider.get("only") != ["phala"]
                or provider.get("allow_fallbacks") is not False
                or provider.get("zdr") is not True
                or provider.get("data_collection") != "deny"):
            raise ValueError("Fresh phase request violates sealed provider routing")
        api_key = os.environ.get("OPENROUTER_API_KEY")
        if not api_key:
            raise ValueError("OPENROUTER_API_KEY is unavailable")
        response = provider_call_with_deadline(request, api_key, self.timeout_seconds)
        usage = response.get("usage") if isinstance(response, dict) else None
        cost = usage.get("cost") if isinstance(usage, dict) else None
        receipt_id = response.get("id") if isinstance(response, dict) else None
        if (not isinstance(cost, (int, float)) or isinstance(cost, bool)
                or not math.isfinite(cost) or cost < 0
                or not isinstance(receipt_id, str) or not receipt_id):
            raise ValueError("Provider response lacks verified cost or receipt identity")
        return {"response": response, "receipt_id": receipt_id,
                "charged_microusd": int(math.ceil(cost * 1_000_000))}


def replay_case(options: argparse.Namespace) -> dict[str, Any]:
    from context_graph_receipt_case import save_capture
    checkpoint_path = options.checkpoint.resolve()
    if (ROOT / ".scratch").resolve() not in options.output.resolve().parents:
        raise ValueError("Private results must remain below .scratch")
    if options.output.exists():
        raise ValueError("Run result already exists")
    frame = json.loads(checkpoint_path.read_text(encoding="utf-8"))
    contract = frame["contract"]
    events = []
    if options.receipts:
        case = json.loads(options.receipts.read_text(encoding="utf-8"))
        for attempt in case["attempts"]:
            opening = attempt["opening"]["event"]
            record = json.loads(opening["payload"]["record_json"])
            if record["episode_event_id"] == options.episode_id:
                events.extend([opening] + [r["event"] for r in attempt["receipts"]])
        events = sorted((e for e in events if e["id"] > contract["cutoff"]), key=lambda e: e["id"])
        if options.through_event_id is not None:
            events = [event for event in events if event["id"] <= options.through_event_id]
        if not events:
            raise ValueError("No recorded events selected after the baseline")
    manifest = {"input_kind": "checkpoint", "storage_mode": "reference",
                "events": {"file": str(checkpoint_path)}, "partition": contract,
                "recovery_start_storage_position": contract["recovery_position"]}
    worker = LispWorker.start(checkpoint_path.parent, manifest, options.worker_mode, options.image)
    try:
        queries = [{"query": query, "evidence_policy": policy, "maximum_paths": 20}
                   for query in options.query for policy in ("verified", "inferred")]
        if options.exact:
            queries.append({"exact_queries": options.exact})
        request = {"operation": "replay-case", "events": events, "queries": queries}
        if options.counterfactual:
            request["counterfactual"] = json.loads(options.counterfactual.read_text(encoding="utf-8"))
        elif options.counterfactual_event is not None:
            selected = next((event for event in events if event["id"] == options.counterfactual_event), None)
            if selected is None:
                raise ValueError("Counterfactual response event not in selected interval")
            record = json.loads(selected["payload"]["record_json"])
            if record.get("outcome") != "response":
                raise ValueError("Counterfactual event is not a response")
            request["counterfactual"] = {"event_id": selected["id"], "response": record["response"]}
        if options.response_set:
            if "counterfactual" not in request:
                raise ValueError("Response overrides require an explicit counterfactual target")
            for assignment in options.response_set:
                path, value = assignment.split("=", 1)
                keys = path.split("/")
                target = request["counterfactual"]["response"]
                for key in keys[:-1]:
                    target = target[int(key)] if isinstance(target, list) else target[key]
                key = int(keys[-1]) if isinstance(target, list) else keys[-1]
                if not isinstance(target[key], str):
                    raise ValueError("Inline response overrides require an existing string field")
                target[key] = value
        if options.expect_no_change:
            request["operation"] = "compare-case"
            request["expected_changes"] = []
        result = worker.call(request, 30)
        result["code_fingerprint"] = worker.code_fingerprint
        save_capture(result, options.output)
        if options.expect_no_change:
            return {"comparison": result["comparison"], "output": str(options.output),
                    "baseline_seconds": result["baseline"]["elapsed_seconds"],
                    "candidate_seconds": result["candidate"]["elapsed_seconds"],
                    "provider_calls": 0, "historical_fold_count": 0}
        return {"status": result["status"], "failure": result.get("failure"),
                "elapsed_seconds": result["elapsed_seconds"], "output": str(options.output),
                "before_nodes": result["before"]["node_count"],
                "before_edges": result["before"]["edge_count"],
                "after_nodes": (result.get("after") or {}).get("node_count"),
                "after_edges": (result.get("after") or {}).get("edge_count"),
                "provider_calls": result["provider_calls"], "historical_fold_count": 0}
    finally:
        worker.close()


def prepare_cases(options: argparse.Namespace) -> dict[str, Any]:
    """One explicitly requested compact fold, with streamed durable checkpoints."""
    from context_graph_receipt_case import _supported_contract, save_capture
    source = options.input.resolve()
    output = options.output.resolve()
    if (ROOT / ".scratch").resolve() not in output.parents:
        raise ValueError("Private checkpoints must remain below .scratch")
    if not 0 < options.deadline <= 180:
        raise ValueError("Preparation deadline must be between zero and 180 seconds")
    data = json.loads(source.read_text(encoding="utf-8"))
    contract = data["contract"]
    if (data.get("kind") != "private-preparation-input"
            or not _supported_contract({key: contract.get(key) for key in
                                        ("profile", "generation", "protocol",
                                         "ontology_revision")})):
        raise ValueError("Explicit supported preparation contract required")
    origin = hashlib.sha256(json.dumps([contract, data["events"]], sort_keys=True,
                                      separators=(",", ":")).encode()).hexdigest()
    if origin != data.get("origin_digest"):
        raise ValueError("Preparation input origin digest mismatch")
    resume = json.loads(options.resume.read_text(encoding="utf-8")) if options.resume else None
    output.mkdir(parents=True, exist_ok=False)
    save_capture({"input": str(source), "input_sha256": sha256_file(source),
                  "origin_digest": origin, "cuts": options.cut, "contract": contract},
                 output / "manifest.json")
    manifest = {"input_kind": "preparation", "storage_mode": "reference",
                "events": {"file": str(source)}, "partition": contract,
                "recovery_start_storage_position": contract["recovery_position"]}
    saved = []
    worker = None
    started = time.monotonic()
    def checkpoint(frame):
        path = output / f"checkpoint-{len(saved):03d}-{frame['contract']['cutoff']}.json"
        save_capture(frame, path)
        saved.append({"path": str(path), "status": frame["status"],
                      "cutoff": frame["contract"]["cutoff"], "digest": frame["digest"]})
        print(json.dumps({"checkpoint": saved[-1], "elapsed_seconds": time.monotonic() - started}),
              flush=True)
    try:
        worker = LispWorker.start(source.parent, manifest, options.worker_mode, options.image)
        request = {"operation": "prepare", "cuts": options.cut,
                   "deadline_seconds": options.deadline}
        if resume is not None:
            request["resume"] = resume
        result = worker.call(request, options.deadline + 15, on_checkpoint=checkpoint)
        result["checkpoints"] = saved
        save_capture(result, output / "result.json")
        return result
    except Exception as condition:
        save_capture({"status": "incomplete", "error": str(condition),
                      "checkpoints": saved, "diagnostic_tail": worker.log[-10:] if worker else []},
                     output / "incomplete.json")
        raise
    finally:
        if worker is not None:
            worker.close()


class CaseLibrary:
    """Hash-pinned file references; never copy a database or discover arbitrary files."""
    def __init__(self, path: Path):
        self.root = path.resolve().parent
        self.document = json.loads(path.read_text(encoding="utf-8"))
        if self.document.get("kind") != "private-case-library-v1":
            raise ValueError("Unsupported case library")
        self.cases = {}
        self.expectations = {}
        rows = []
        for item in self.document["cases"]:
            if item["id"] in self.cases:
                raise ValueError("Duplicate case id")
            checkpoint = self.read(item["checkpoint"])
            receipts = self.read(item["receipts"])
            if item.get("expectations") is not None:
                expectations = self.read(item["expectations"])
                if expectations.get("schema_version") != 1:
                    raise ValueError("Unsupported case expectation schema")
                exact_queries = [row.get("query") for row in expectations.get("exact_queries", [])]
                if (len(exact_queries) > 32 or any(not isinstance(query, str) or not query.strip()
                                                   for query in exact_queries)):
                    raise ValueError("Case expectation exact-query set is outside the replay bound")
                self.expectations[item["id"]] = expectations
            events = []
            attempts = []
            for attempt in receipts["attempts"]:
                opening = attempt["opening"]["event"]
                record = json.loads(opening["payload"]["record_json"])
                if record["episode_event_id"] == item["episode_id"]:
                    events.extend([opening] + [r["event"] for r in attempt["receipts"]])
                    attempts.append({"opened_event_id": opening["id"], "batch_index": record["batch_index"],
                                     "attempt": record.get("attempt", 1), "status": "recorded"})
            cut = checkpoint["contract"]["cutoff"]
            events = sorted((event for event in events if event["id"] > cut), key=lambda event: event["id"])
            if not events:
                raise ValueError("Case has no receipts after its checkpoint")
            phases = []
            for event in events:
                record = json.loads(event["payload"]["record_json"])
                if record.get("outcome") == "response":
                    phases.append({"event_id": event["id"], "phase": record["phase"]})
            self.cases[item["id"]] = (item, events)
            rows.append({"case_id": item["id"], "episode_id": item["id"],
                         "episode_event_id": item["episode_id"], "status": "completed",
                         "synopsis": item.get("label", "Checkpoint-backed recorded episode"),
                         "expectations_configured": item.get("expectations") is not None,
                         "before_event_id": cut, "after_event_id": events[-1]["id"],
                         "attempts": attempts, "query_suggestions": [], "phases": phases})
        if not rows or len(rows) > 12:
            raise ValueError("Case library requires 1–12 cases")
        self.catalog = {"episodes": rows, "episode_count": len(rows), "status_counts": {"completed": len(rows)}}

    def path(self, descriptor):
        path = (self.root / descriptor["file"]).resolve()
        if self.root not in path.parents or not path.is_file():
            raise ValueError("Case dependency escapes its private library")
        if sha256_file(path) != descriptor["sha256"]:
            raise ValueError("Immutable case dependency hash mismatch")
        return path

    def read(self, descriptor):
        return json.loads(self.path(descriptor).read_text(encoding="utf-8"))

    def run(self, worker, request):
        item, events = self.cases[request["case_id"]]
        if request.get("through_event_id") is not None:
            events = [e for e in events if e["id"] <= request["through_event_id"]]
        if not events:
            raise ValueError("No events in selected case interval")
        requested_queries = request.get("queries", [])
        if (not isinstance(requested_queries, list) or len(requested_queries) > 32
                or not all(isinstance(q, dict) for q in requested_queries)):
            raise ValueError("A replay accepts at most 32 query objects")
        queries = list(requested_queries)
        expectations = getattr(self, "expectations", {}).get(item["id"])
        if expectations is not None:
            exact = [row["query"] for row in expectations.get("exact_queries", [])]
            covered = {value for query in queries
                       for value in query.get("exact_queries", [])
                       if isinstance(value, str)}
            missing = [value for value in exact if value not in covered]
            if missing:
                queries.append({"exact_queries": missing})
                if len(queries) > 32:
                    raise ValueError("Pinned expectation audit exceeds the replay query bound")
        run_id = secrets.token_hex(12)
        message = {"operation": "replay-case", "checkpoint": self.read(item["checkpoint"]),
                   "events": events, "run_id": run_id, "queries": queries}
        if request.get("counterfactual") is not None:
            message["counterfactual"] = request["counterfactual"]
        if request.get("compare"):
            message["operation"] = "compare-case"
            if request.get("expect_no_change"):
                message["expected_changes"] = []
        try:
            result = worker.call(message, 30)
        except Exception as condition:
            save_run_capture({"status": "incomplete", "run_id": run_id, "error": str(condition),
                          "case_id": item["id"], "contract": message["checkpoint"]["contract"],
                          "baseline_digest": message["checkpoint"]["digest"],
                          "code_fingerprint": worker.code_fingerprint,
                          "provider_calls": 0, "historical_fold_count": 0},
                         self.root / "runs" / (run_id + ".json"))
            raise
        result["cost_usd"] = 0
        result["case_id"] = item["id"]
        result["code_fingerprint"] = worker.code_fingerprint
        result["saved_result"] = run_id
        result["expectation_audit_included"] = expectations is not None
        save_run_capture(result, self.root / "runs" / (run_id + ".json"))
        return result

    def saved_result(self, run_id):
        """Read one exact result without touching the worker or replaying anything."""
        if (not isinstance(run_id, str) or len(run_id) != 24
                or any(char not in "0123456789abcdef" for char in run_id)):
            raise ValueError("Invalid saved run id")
        path = (self.root / "runs" / (run_id + ".json")).resolve()
        if path.parent != (self.root / "runs").resolve() or self.root not in path.parents:
            raise ValueError("Saved result escapes its private library")
        if path.stat().st_size > SAVED_RESULT_MAX_BYTES:
            raise ValueError("Saved result exceeds the read bound")
        result = json.loads(path.read_text(encoding="utf-8"))
        result["saved_result"] = run_id
        result["reopened_without_replay"] = True
        return result

    def verify_saved(self, request):
        """Apply explicit hash-pinned expectations without replaying the case."""
        from context_graph_case_expectations import verify
        case_id = request.get("case_id")
        item, unused_events = self.cases.get(case_id, (None, None))
        del unused_events
        if item is None or item.get("expectations") is None:
            raise ValueError("Selected case has no explicit expectation matrix")
        result = verify(self.saved_result(request.get("run_id")),
                        self.read(item["expectations"]))
        result["case_id"] = case_id
        result["saved_result"] = request.get("run_id")
        result["replayed"] = False
        return result

    def plan_phase(self, worker, request):
        """Reconstruct exactly one ask; never authorize or dispatch a model call."""
        from context_graph_identity_lab import request_from_spec
        from context_graph_inference_qualification import phase_output_tokens
        from context_graph_lab import request_cost_bound
        item, events = self.cases[request["case_id"]]
        selected = next((e for e in events if e["id"] == request["event_id"]), None)
        if selected is None or selected["type"] != "context-graph-identity-phase":
            raise ValueError("Select one recorded phase response")
        record = json.loads(selected["payload"]["record_json"])
        if record.get("outcome") != "response":
            raise ValueError("Selected event is not a completed phase response")
        reservations = [e for e in events if e["id"] < selected["id"]
                        and e.get("caused_by") == selected.get("caused_by")
                        and e["type"] == "context-graph-identity-phase"
                        and (r := json.loads(e["payload"]["record_json"])).get("outcome") == "request"
                        and r.get("phase") == record["phase"]
                        and r.get("request_digest") == record["request_digest"]]
        if len(reservations) != 1:
            raise ValueError("Selected phase needs exactly one matching reservation")
        # Stop before the selected reservation: historical admission is not a new
        # budget grant. The production owner constructs the actual next request.
        prefix = [e for e in events if e["id"] < reservations[0]["id"]]
        frame = self.read(item["checkpoint"])
        replay = worker.call({"operation": "replay-case", "checkpoint": frame,
                              "events": prefix}, 30)
        if replay.get("status") != "awaiting-phase":
            raise ValueError("Exact prefix did not reach the selected phase: " +
                             str(replay.get("failure") or replay.get("status")))
        pending = [p["next"] for p in replay["pending"]
                   if p["opening_id"] == selected["caused_by"]]
        if (len(pending) != 1 or pending[0].get("status") != "request"
                or pending[0].get("phase") != record["phase"]):
            raise ValueError("Prefix reached a different phase; choose that phase explicitly")
        next_phase = pending[0]
        args = argparse.Namespace(
            model="meta/muse-glimmer-30b", max_output_tokens=phase_output_tokens(record["phase"]),
            max_prompt_price=.33, max_completion_price=1.21,
            openrouter_zdr="require", openrouter_data_collection="deny",
            openrouter_provider_only="phala", reasoning_policy="low")
        model_request = request_from_spec(next_phase["spec"], args)
        bound = request_cost_bound(model_request, args)
        plan_id = secrets.token_hex(12)
        result = {
            "mode": "fresh-phase-preflight", "status": "estimate-only",
            "saved_result": plan_id, "case_id": item["id"],
            "contract": frame["contract"], "baseline_digest": frame["digest"],
            "code_fingerprint": worker.code_fingerprint,
            "selected_response_event_id": selected["id"], "opening_id": selected["caused_by"],
            "phase": record["phase"], "request_digest": next_phase["request_digest"],
            "tool_name": next_phase["spec"]["tool_name"],
            "recorded_request_digest": record["request_digest"],
            "request_changed": next_phase["request_digest"] != record["request_digest"],
            "input_events_digest": replay["input_events_digest"],
            "request": model_request, "estimated_calls": 1,
            "maximum_request_cost_usd": bound, "per_request_ceiling_usd": .06,
            "within_per_request_ceiling": bound <= .06,
            "execution_authorized": False,
            "cumulative_budget_status": "unverified-no-reservation",
            "downstream_status": "unknown-until-selected-response; explicit-selection-required",
            "automatic_retries": 0, "provider_calls": 0, "cost_usd": 0,
            "historical_fold_count": 0, "authority_writes": 0,
            "elapsed_seconds": replay["elapsed_seconds"],
        }
        save_run_capture(result, self.root / "runs" / (plan_id + ".json"))
        return result

    def execute_phase(self, worker, request, budget_gateway=None, transport=None):
        """Execute exactly one selected phase through injected durable admission.

        Production serving deliberately supplies neither dependency until the
        shared event-ledger gateway is configured. Tests use fakes; there is no
        host-side allowance or retry fallback.
        """
        from context_graph_authority_lab import response_payload
        if budget_gateway is None or transport is None:
            raise ValueError("Fresh selected-phase execution is disabled: no shared durable gateway")
        plan = self.plan_phase(worker, request)
        confirmed = request.get("confirmed_request_digest")
        reservation_id = request.get("reservation_id")
        if confirmed != plan["request_digest"]:
            raise ValueError("Selected phase request digest was not explicitly confirmed")
        if (not isinstance(reservation_id, str) or not 1 <= len(reservation_id) <= 256):
            raise ValueError("Fresh selected phase requires a bounded reservation id")
        bound_microusd = int(math.ceil(plan["maximum_request_cost_usd"] * 1_000_000))
        if not plan["within_per_request_ceiling"] or not 1 <= bound_microusd <= 60000:
            raise ValueError("Selected phase exceeds the sealed per-request ceiling")
        reservation = budget_gateway.reserve(
            reservation_id, plan["request_digest"], plan["phase"], bound_microusd)
        run_id = secrets.token_hex(12)
        result_path = self.root / "runs" / (run_id + ".json")
        diagnostic = {
            "mode": "fresh-selected-phase", "status": "reserved",
            "saved_result": run_id, "case_id": request["case_id"],
            "code_fingerprint": worker.code_fingerprint,
            "selected_response_event_id": plan["selected_response_event_id"],
            "request_digest": plan["request_digest"], "reservation": reservation,
            "reserved_microusd": bound_microusd, "automatic_retries": 0,
            "provider_call_attempts": 0, "provider_calls": 0,
            "charged_microusd": bound_microusd, "charge_status": "pending-full-bound",
            "historical_fold_count": 0, "authority_writes": 1,
        }
        save_run_capture(diagnostic, self.root / "runs" / (run_id + "-admission.json"))
        # Any exception after reservation and before verified settlement leaves
        # the full bound pending. Never retry here.
        diagnostic.update(status="provider-outcome-unknown", provider_call_attempts=1,
                          provider_calls=1)
        try:
            received = transport.call(plan["request"])
        except BaseException as condition:
            diagnostic["error"] = f"{type(condition).__name__}: {condition}"
            save_run_capture(diagnostic, result_path)
            raise
        if (not isinstance(received, dict)
                or type(received.get("charged_microusd")) is not int
                or received["charged_microusd"] < 0
                or not isinstance(received.get("receipt_id"), str)
                or not received["receipt_id"]
                or not isinstance(received.get("response"), dict)):
            diagnostic.update(status="provider-outcome-unverified",
                              error="Transport outcome lacks verified settlement evidence")
            save_run_capture(diagnostic, result_path)
            raise ValueError(diagnostic["error"])
        budget_gateway.settle(reservation_id, plan["request_digest"],
                              received["charged_microusd"], received["receipt_id"])
        raw_path = self.root / "runs" / (run_id + "-provider-response.json")
        save_run_capture(received["response"], raw_path)
        diagnostic.update(status="settled-response-pending-validation",
                          provider_receipt_id=received["receipt_id"],
                          charged_microusd=received["charged_microusd"],
                          charge_status="verified", authority_writes=2,
                          fresh_response_path=raw_path.name)
        if received["charged_microusd"] > bound_microusd:
            diagnostic.update(status="settled-overrun",
                              error="Verified provider charge exceeded its reservation; ledger is poisoned")
            save_run_capture(diagnostic, result_path)
            raise ValueError("Verified provider charge exceeded its reservation; ledger is poisoned")
        try:
            payload = response_payload(received["response"], plan["tool_name"])
        except BaseException as condition:
            diagnostic.update(status="settled-response-invalid",
                              error=f"{type(condition).__name__}: {condition}")
            save_run_capture(diagnostic, result_path)
            raise
        item, events = self.cases[request["case_id"]]
        message = {
            "operation": "replay-case", "checkpoint": self.read(item["checkpoint"]),
            "events": events, "run_id": run_id, "queries": request.get("queries", []),
            "counterfactual": {"event_id": plan["selected_response_event_id"],
                               "response": payload,
                               "provenance": "fresh-selected-phase",
                               "reservation_id": reservation_id,
                               "provider_receipt_id": received["receipt_id"]},
        }
        try:
            result = worker.call(message, 30)
        except BaseException as condition:
            diagnostic.update(status="settled-replay-incomplete",
                              error=f"{type(condition).__name__}: {condition}")
            save_run_capture(diagnostic, result_path)
            raise
        result.update({
            "mode": "fresh-selected-phase", "case_id": item["id"],
            "saved_result": run_id, "code_fingerprint": worker.code_fingerprint,
            "selected_response_event_id": plan["selected_response_event_id"],
            "request_digest": plan["request_digest"], "reservation": reservation,
            "provider_receipt_id": received["receipt_id"], "provider_calls": 1,
            "charged_microusd": received["charged_microusd"],
            "cost_usd": received["charged_microusd"] / 1_000_000,
            "automatic_retries": 0, "historical_fold_count": 0,
            "fresh_response_path": raw_path.name,
            "downstream_status": ("complete" if result.get("status") == "complete"
                                    else "explicit-selection-required"),
        })
        save_run_capture(result, result_path)
        return result

    def compare_saved(self, worker, request):
        """Compare separately executed code versions without replaying either."""
        baseline = self.saved_result(request["baseline_run_id"])
        candidate = self.saved_result(request["candidate_run_id"])
        message = {"operation": "compare-results", "baseline": baseline, "candidate": candidate}
        expected_changes = request.get("expected_changes")
        if expected_changes is not None:
            if (not isinstance(expected_changes, list) or len(expected_changes) > 64
                    or not all(isinstance(key, str) and 1 <= len(key) <= 256
                               for key in expected_changes)
                    or len(set(expected_changes)) != len(expected_changes)):
                raise ValueError("Expected changes require at most 64 unique bounded keys")
            message["expected_changes"] = expected_changes
        elif request.get("expect_no_change"):
            message["expected_changes"] = []
        comparison = worker.call(message, 30)
        run_id = secrets.token_hex(12)
        result = {"mode": "saved-run-comparison", "status": comparison["status"],
                "saved_result": run_id, "comparison": comparison,
                "baseline_run_id": request["baseline_run_id"],
                "candidate_run_id": request["candidate_run_id"],
                "baseline_code_fingerprint": baseline.get("code_fingerprint"),
                "candidate_code_fingerprint": candidate.get("code_fingerprint"),
                "provider_calls": 0, "historical_fold_count": 0,
                "comparator_code_fingerprint": worker.code_fingerprint,
                "replayed": False}
        save_run_capture(result, self.root / "runs" / (run_id + ".json"))
        return result


class LabServer(ThreadingHTTPServer):
    daemon_threads = True

    def __init__(self, address: tuple[str, int], seed: Path,
                 manifest: dict[str, Any], catalog: dict[str, Any],
                 worker: LispWorker):
        super().__init__(address, LabHandler)
        self.seed = seed
        self.manifest = manifest
        self.catalog = catalog
        self.worker = worker
        self.csrf_token = secrets.token_urlsafe(32)
        self.case_library = None
        self.budget_gateway = None
        self.budget_status = None
        self.phase_transport = None


class LabHandler(BaseHTTPRequestHandler):
    server: LabServer

    def log_message(self, format_string: str, *arguments: Any) -> None:
        sys.stderr.write("episode-lab " + format_string % arguments + "\n")

    def _trusted_host(self) -> bool:
        port = self.server.server_address[1]
        if self.headers.get("Host") not in {f"127.0.0.1:{port}", f"localhost:{port}"}:
            self.send_error(HTTPStatus.FORBIDDEN, "Loopback host required")
            return False
        return True

    def _headers(self, status: HTTPStatus, content_type: str,
                 length: int) -> None:
        self.send_response(status)
        self.send_header("Content-Type", content_type)
        self.send_header("Content-Length", str(length))
        self.send_header("Cache-Control", "no-store")
        self.send_header("X-Content-Type-Options", "nosniff")
        self.send_header("Referrer-Policy", "no-referrer")
        self.send_header("Content-Security-Policy",
                         "default-src 'self'; script-src 'self'; style-src 'self'; "
                         "img-src 'self' data:; connect-src 'self'; frame-ancestors 'none'")
        self.end_headers()

    def _json(self, value: Any, status: HTTPStatus = HTTPStatus.OK) -> None:
        body = canonical_bytes(value)
        self._headers(status, "application/json; charset=utf-8", len(body))
        self.wfile.write(body)

    def _asset(self, name: str, content_type: str) -> None:
        path = (ASSET_DIR / name).resolve()
        if path.parent != ASSET_DIR.resolve() or not path.is_file():
            self.send_error(HTTPStatus.NOT_FOUND)
            return
        body = path.read_bytes()
        self._headers(HTTPStatus.OK, content_type, len(body))
        self.wfile.write(body)

    def do_GET(self) -> None:  # noqa: N802
        if not self._trusted_host():
            return
        route = urlparse(self.path).path
        if route == "/":
            self._asset("index.html", "text/html; charset=utf-8")
        elif route == "/app.js":
            self._asset("app.js", "text/javascript; charset=utf-8")
        elif route == "/styles.css":
            self._asset("styles.css", "text/css; charset=utf-8")
        elif route == "/api/config":
            self._json({
                "schema_version": 1,
                "csrf_token": self.server.csrf_token,
                "case_mode": self.server.case_library is not None,
                "fresh_phase_execution": ("enabled" if self.server.phase_transport is not None
                                            and self.server.budget_gateway is not None else "disabled"),
                "shared_budget_configured": self.server.budget_gateway is not None,
                "shared_budget_status": self.server.budget_status,
                "manifest": {
                    "tool_revision": self.server.manifest["tool_revision"],
                    "seed_kind": self.server.manifest["seed_kind"],
                    "head_event_id": self.server.manifest["head_event_id"],
                    "episode_count": self.server.manifest["episode_count"],
                    "events_sha256": self.server.manifest["events"]["sha256"],
                },
            })
        elif route == "/api/episodes":
            self._json(self.server.catalog)
        else:
            self.send_error(HTTPStatus.NOT_FOUND)

    def do_POST(self) -> None:  # noqa: N802
        if not self._trusted_host():
            return
        route = urlparse(self.path).path
        if route not in {"/api/query", "/api/graph", "/api/delta", "/api/case-run", "/api/case-phase", "/api/case-query", "/api/case-result", "/api/case-plan", "/api/case-execute", "/api/case-verify", "/api/case-compare-saved"}:
            self.send_error(HTTPStatus.NOT_FOUND)
            return
        if self.headers.get("X-PAI-Episode-Lab") != self.server.csrf_token:
            self._json({"error": "same-origin token missing"}, HTTPStatus.FORBIDDEN)
            return
        try:
            length = int(self.headers.get("Content-Length", "0"))
            if length <= 0 or length > 64 * 1024:
                raise ValueError("request body is outside the laboratory bound")
            value = json.loads(self.rfile.read(length))
            if not isinstance(value, dict):
                raise ValueError("request must be an object")
            if route.startswith("/api/case-"):
                if self.server.case_library is None:
                    raise ValueError("No checkpoint library is loaded")
                if route == "/api/case-run":
                    self._json(self.server.case_library.run(self.server.worker, value))
                elif route == "/api/case-result":
                    self._json(self.server.case_library.saved_result(value["run_id"]))
                elif route == "/api/case-plan":
                    self._json(self.server.case_library.plan_phase(self.server.worker, value))
                elif route == "/api/case-execute":
                    self._json(self.server.case_library.execute_phase(
                        self.server.worker, value, self.server.budget_gateway,
                        self.server.phase_transport))
                elif route == "/api/case-verify":
                    self._json(self.server.case_library.verify_saved(value))
                elif route == "/api/case-compare-saved":
                    self._json(self.server.case_library.compare_saved(self.server.worker, value))
                elif route == "/api/case-phase":
                    _, events = self.server.case_library.cases[value["case_id"]]
                    event = next(e for e in events if e["id"] == value["event_id"])
                    self._json(json.loads(event["payload"]["record_json"]))
                else:
                    self._json(self.server.worker.call({"operation": "case-query",
                                                        "run_id": value["run_id"], "side": value["side"],
                                                        "request": value["request"]}))
                return
            if self.server.case_library is not None:
                raise ValueError("Checkpoint mode forbids historical reconstruction routes")
            operation = route.rsplit("/", 1)[1]
            value["operation"] = operation
            result = self.server.worker.call(value)
            self._json(result)
        except (ValueError, RuntimeError, OSError, KeyError, StopIteration, json.JSONDecodeError) as condition:
            self._json({"error": str(condition)}, HTTPStatus.BAD_REQUEST)


def serve(options: argparse.Namespace) -> None:
    seed = options.seed.resolve()
    manifest, catalog = load_seed(seed)
    worker = LispWorker.start(seed, manifest, options.worker_mode, options.image)
    server = LabServer(("127.0.0.1", options.port), seed, manifest, catalog, worker)
    try:
        print(f"Context graph episode lab: http://127.0.0.1:{options.port}",
              flush=True)
        server.serve_forever()
    except KeyboardInterrupt:
        pass
    finally:
        server.server_close()
        worker.close()


def serve_cases(options):
    library = CaseLibrary(options.library)
    first = next(iter(library.cases.values()))[0]
    checkpoint = library.path(first["checkpoint"])
    frame = library.read(first["checkpoint"])
    manifest = {"input_kind": "checkpoint", "storage_mode": "reference",
                "events": {"file": str(checkpoint), "sha256": first["checkpoint"]["sha256"]},
                "partition": frame["contract"], "recovery_start_storage_position": frame["contract"]["recovery_position"],
                "tool_revision": TOOL_REVISION, "seed_kind": "checkpoint-library",
                "head_event_id": max(row["after_event_id"] for row in library.catalog["episodes"]),
                "episode_count": len(library.cases)}
    policy = None
    if options.budget_policy:
        policy_path = options.budget_policy.resolve()
        if ((ROOT / ".scratch").resolve() not in policy_path.parents
                or not policy_path.is_file() or policy_path.stat().st_size > 16 * 1024):
            raise ValueError("Budget policy must be a bounded private .scratch JSON file")
        policy = json.loads(policy_path.read_text(encoding="utf-8"))
        if not isinstance(policy, dict):
            raise ValueError("Budget policy must be an object")
    if (policy is not None) != bool(options.budget_volume or options.budget_database):
        raise ValueError("Budget policy and exactly one durable budget storage source are required together")
    if options.enable_fresh_phase_execution and policy is None:
        raise ValueError("Fresh phase execution requires startup-sealed shared budget policy and storage")
    if options.enable_fresh_phase_execution and not os.environ.get("OPENROUTER_API_KEY"):
        raise ValueError("Fresh phase execution requires OPENROUTER_API_KEY at server startup")
    worker = LispWorker.start(
        checkpoint.parent, manifest, options.worker_mode, options.image,
        budget_database=options.budget_database, budget_volume=options.budget_volume,
        budget_policy=policy)
    server = LabServer(("127.0.0.1", options.port), library.root, manifest, library.catalog, worker)
    server.case_library = library
    if policy is not None:
        server.budget_gateway = WorkerBudgetGateway(worker)
        server.budget_status = server.budget_gateway.snapshot()
    if options.enable_fresh_phase_execution:
        server.phase_transport = OpenRouterSelectedPhaseTransport(options.provider_timeout_seconds)
    try:
        print(f"Checkpoint episode lab: http://127.0.0.1:{options.port}", flush=True)
        server.serve_forever()
    finally:
        server.server_close()
        worker.close()


def plan_case_phase(options):
    library = CaseLibrary(options.library)
    item = library.cases[options.case_id][0]
    checkpoint = library.path(item["checkpoint"])
    frame = library.read(item["checkpoint"])
    manifest = {"input_kind": "checkpoint", "storage_mode": "reference",
                "events": {"file": str(checkpoint)}, "partition": frame["contract"],
                "recovery_start_storage_position": frame["contract"]["recovery_position"]}
    worker = LispWorker.start(checkpoint.parent, manifest, options.worker_mode, options.image)
    try:
        result = library.plan_phase(worker, {"case_id": options.case_id, "event_id": options.event_id})
        return {key: value for key, value in result.items()
                if key not in {"request", "contract", "code_fingerprint"}}
    finally:
        worker.close()


def parse_args(arguments: list[str] | None = None) -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    commands = parser.add_subparsers(dest="command", required=True)
    seed = commands.add_parser("seed", help="create a hash-pinned private seed")
    seed.add_argument("--events", type=Path, required=True)
    seed.add_argument("--derived", type=Path, required=True)
    seed.add_argument("--output", type=Path, required=True)
    seed.add_argument("--agent-id")
    seed.add_argument("--persona-id")
    seed.add_argument("--storage-mode",
                      choices=("copy", "reference", "sqlite-backup"),
                      default="copy",
                      help=("reference is hash-pinned without duplicating files; "
                            "sqlite-backup makes a consistent self-contained copy "
                            "from a live WAL database"))
    seed.add_argument(
        "--confirm-quiescent-backup", action="store_true",
        help="confirm inputs are backup files, not live runtime databases",
    )
    seed.add_argument(
        "--confirm-live-snapshot", action="store_true",
        help="authorize a read-only SQLite online backup of the live inputs",
    )
    run = commands.add_parser("serve", help="serve the loopback visualizer")
    run.add_argument("--seed", type=Path, required=True)
    run.add_argument("--port", type=int, default=8765)
    run.add_argument("--worker-mode", choices=("auto", "local", "docker"),
                     default="auto")
    run.add_argument("--image", default="pai-local:development")
    case_server = commands.add_parser("serve-cases", help="serve reusable checkpoint cases in the existing lab")
    case_server.add_argument("--library", type=Path, required=True)
    case_server.add_argument("--port", type=int, default=8765)
    case_server.add_argument("--worker-mode", choices=("auto", "local", "docker"), default="auto")
    case_server.add_argument("--image", default="pai-local:development")
    budget_storage = case_server.add_mutually_exclusive_group()
    budget_storage.add_argument("--budget-database", type=Path,
                                help="existing host event SQLite database (shared admission only)")
    budget_storage.add_argument("--budget-volume",
                                help="Docker volume containing /events.sqlite3 (shared admission only)")
    case_server.add_argument("--budget-policy", type=Path,
                             help="private startup-sealed cumulative authorization JSON")
    case_server.add_argument("--enable-fresh-phase-execution", action="store_true",
                             help="enable one explicitly confirmed provider phase; disabled by default")
    case_server.add_argument("--provider-timeout-seconds", type=float, default=180,
                             help="single-attempt provider wall-clock deadline (1-180 seconds)")
    phase = commands.add_parser("plan-phase", help="estimate one checkpoint-backed phase; never send")
    phase.add_argument("--library", type=Path, required=True)
    phase.add_argument("--case-id", required=True)
    phase.add_argument("--event-id", type=int, required=True)
    phase.add_argument("--worker-mode", choices=("auto", "local", "docker"), default="auto")
    phase.add_argument("--image", default="pai-local:development")
    verify = commands.add_parser("verify", help="verify seed hashes and catalog")
    verify.add_argument("--seed", type=Path, required=True)
    preparation = commands.add_parser("prepare-cases", help="one bounded compact preparation pass")
    preparation.add_argument("--input", type=Path, required=True)
    preparation.add_argument("--output", type=Path, required=True)
    preparation.add_argument("--cut", type=int, action="append", required=True)
    preparation.add_argument("--resume", type=Path)
    preparation.add_argument("--deadline", type=float, default=180)
    preparation.add_argument("--worker-mode", choices=("auto", "local", "docker"), default="auto")
    preparation.add_argument("--image", default="pai-local:development")
    replay = commands.add_parser("replay-case", help="inspect or replay a checkpoint without historical fold")
    replay.add_argument("--checkpoint", type=Path, required=True)
    replay.add_argument("--output", type=Path, required=True)
    replay.add_argument("--receipts", type=Path)
    replay.add_argument("--episode-id", type=int)
    replay.add_argument("--query", action="append", default=[])
    replay.add_argument("--exact", action="append", default=[])
    replay.add_argument("--counterfactual", type=Path, help="explicit synthetic phase-response replacement JSON")
    replay.add_argument("--counterfactual-event", type=int, help="synthetic no-op response for parity qualification")
    replay.add_argument("--response-set", action="append", default=[], help="synthetic string override: slash/path=value")
    replay.add_argument("--through-event-id", type=int, help="stop after this selected phase or batch")
    replay.add_argument("--expect-no-change", action="store_true", help="compare recorded and candidate in one worker; require an empty graph delta")
    replay.add_argument("--worker-mode", choices=("auto", "local", "docker"), default="auto")
    replay.add_argument("--image", default="pai-local:development")
    for command in ("capture-case", "capture-preparation"):
        receipts = commands.add_parser(command, help="capture bounded private replay evidence")
        receipts.add_argument("--events", type=Path, required=True)
        receipts.add_argument("--output", type=Path, required=True)
        receipts.add_argument("--agent-id", required=True)
        receipts.add_argument("--persona-id", required=True)
        if command == "capture-case":
            receipts.add_argument("--episode-id", type=int, action="append", required=True)
        receipts.add_argument("--cutoff", type=int, required=True)
        receipts.add_argument("--recovery-position", type=int, required=True)
        for field in ("profile", "generation", "protocol", "ontology-revision"):
            receipts.add_argument("--" + field, required=True)
    return parser.parse_args(arguments)


def main(arguments: list[str] | None = None) -> int:
    options = parse_args(arguments)
    try:
        if options.command in ("capture-case", "capture-preparation"):
            from context_graph_receipt_case import capture, capture_preparation, save_capture
            private_root = (ROOT / ".scratch").resolve()
            if private_root not in options.output.resolve().parents:
                raise ValueError("Private receipt cases must be saved below .scratch")
            capture_fn = capture if options.command == "capture-case" else capture_preparation
            extra = {"episode_ids": options.episode_id} if options.command == "capture-case" else {}
            result = capture_fn(options.events, agent_id=options.agent_id,
                             persona_id=options.persona_id, **extra,
                             cutoff=options.cutoff, recovery_position=options.recovery_position,
                             contract={key: getattr(options, key) for key in
                                       ("profile", "generation", "protocol", "ontology_revision")})
            save_capture(result, options.output)
            print(json.dumps({"counts": result.get("counts"), "metrics": result["metrics"],
                              "output": str(options.output)}, sort_keys=True))
        elif options.command == "prepare-cases":
            print(json.dumps(prepare_cases(options), sort_keys=True))
        elif options.command == "replay-case":
            print(json.dumps(replay_case(options), sort_keys=True))
        elif options.command == "serve-cases":
            serve_cases(options)
        elif options.command == "plan-phase":
            print(json.dumps(plan_case_phase(options), sort_keys=True))
        elif options.command == "seed":
            output = make_seed(options)
            manifest, catalog = load_seed(output)
            print(json.dumps({"seed": str(output),
                              "events_sha256": manifest["events"]["sha256"],
                              "episode_count": catalog["episode_count"],
                              "status_counts": catalog["status_counts"]},
                             indent=2))
        elif options.command == "verify":
            manifest, catalog = load_seed(options.seed.resolve())
            print(json.dumps({"status": "verified",
                              "events_sha256": manifest["events"]["sha256"],
                              "episode_count": catalog["episode_count"]}, indent=2))
        else:
            serve(options)
        return 0
    except (OSError, ValueError, RuntimeError, sqlite3.DatabaseError) as condition:
        print(f"context graph episode lab: {condition}", file=sys.stderr)
        return 2


if __name__ == "__main__":
    raise SystemExit(main())
