"""Verified event export stays inside the canonical pAI CLI surface."""

from __future__ import annotations

from contextlib import closing
import hashlib
import json
from pathlib import Path
import sqlite3
import subprocess
import tempfile
import unittest

import sys


REPO = Path(__file__).resolve().parent.parent
sys.path.insert(0, str(REPO / "scripts"))

from pai_cli import (  # noqa: E402
    backup_memory_cutover_state,
    create_state_backup,
    export_event_ledger,
    memory_migration_environment,
    parse_args,
    validate_agent_id,
    validate_embedding_endpoint,
    verify_state_backup,
    web_environment,
)


def make_database(path: Path, *, corrupt: bool = False) -> list[str]:
    rows = [
        json.dumps({"id": 1, "type": "user-message"}, separators=(",", ":")),
        json.dumps({"id": 2, "type": "agent-message"}, separators=(",", ":")),
    ]
    with closing(sqlite3.connect(path)) as connection:
        connection.execute(
            "CREATE TABLE pai_events ("
            "storage_sequence INTEGER PRIMARY KEY AUTOINCREMENT,"
            "event_id INTEGER NOT NULL,event_json TEXT NOT NULL,"
            "integrity_hash TEXT NOT NULL)"
        )
        for index, event_json in enumerate(rows, 1):
            digest = hashlib.sha256(event_json.encode("utf-8")).hexdigest()
            if corrupt and index == 2:
                digest = "0" * 64
            connection.execute(
                "INSERT INTO pai_events(event_id,event_json,integrity_hash) "
                "VALUES(?,?,?)",
                (index, event_json, digest),
            )
        connection.commit()
    return rows


def make_derived_database(path: Path, storage_id: str = "fixture-ledger") -> None:
    with closing(sqlite3.connect(path)) as connection:
        connection.executescript(
            "CREATE TABLE pai_derived_meta (meta_key TEXT PRIMARY KEY, "
            "meta_value TEXT NOT NULL);"
            "CREATE TABLE pai_memory_imports (import_name TEXT PRIMARY KEY, "
            "node_count INTEGER NOT NULL, edge_count INTEGER NOT NULL, "
            "seal_hash TEXT NOT NULL);"
            "CREATE TABLE pai_memory_nodes (id TEXT PRIMARY KEY);"
            "CREATE TABLE pai_memory_edges (id INTEGER PRIMARY KEY);"
            "CREATE TABLE pai_memory_projection (projection_name TEXT PRIMARY KEY, "
            "baseline_seal TEXT NOT NULL, storage_id TEXT NOT NULL, "
            "agent_id TEXT NOT NULL, through_event_id INTEGER NOT NULL, "
            "through_storage_position INTEGER NOT NULL, boundary_hash TEXT NOT NULL);"
        )
        connection.execute(
            "INSERT INTO pai_derived_meta VALUES('format_version','1')"
        )
        connection.execute(
            "INSERT INTO pai_memory_imports VALUES('canonical',0,0,'seal-1')"
        )
        connection.execute(
            "INSERT INTO pai_memory_projection VALUES"
            "('canonical','seal-1',?,'q45-conversation-dev',2,2,'boundary')",
            (storage_id,),
        )
        connection.commit()


class PaiCliStorageTests(unittest.TestCase):
    def test_migration_only_exits_before_runtime_workers_or_provider_policy(self) -> None:
        source = (REPO / "scripts" / "conscious-conversation.lisp").read_text(
            encoding="utf-8"
        )
        early_exit = source.index(
            "Migration-only startup completed; no cognition, provider policy, web, "
            "or conversation worker was started."
        )
        provider_policy = source.index(
            '(%conversation-startup-phase 5 "Applying contained provider policy")'
        )
        worker_start = source.index(
            '(%conversation-call "conscious-conversation-work-start")'
        )
        self.assertLess(early_exit, provider_policy)
        self.assertLess(provider_policy, worker_start)
        pre_exit = source[:early_exit]
        migration_branch = pre_exit[pre_exit.rfind("(unless"):]
        self.assertIn('(%conversation-call "cognition-runtime-install")', migration_branch)
        self.assertIn('"PAI_MIGRATION_ONLY"', migration_branch)
        self.assertIn("(%conversation-object", pre_exit)

    def test_recursive_mind_loop_is_explicit_and_legacy_remains_default(self) -> None:
        self.assertEqual(parse_args([]).mind_loop, "work-state")
        self.assertEqual(
            parse_args(["--mind-loop", "recursive"]).mind_loop,
            "recursive",
        )

    def test_general_backup_snapshots_derived_before_event_authority(self) -> None:
        with tempfile.TemporaryDirectory(prefix="pai-state-backup-") as temporary:
            root = Path(temporary)
            state = root / "state"
            state.mkdir()
            events = state / "events.sqlite3"
            make_database(events)
            with closing(sqlite3.connect(events)) as connection:
                connection.execute(
                    "CREATE TABLE pai_storage_meta "
                    "(meta_key TEXT PRIMARY KEY,meta_value TEXT NOT NULL)"
                )
                connection.execute(
                    "INSERT INTO pai_storage_meta VALUES('storage_id','fixture-ledger')"
                )
                connection.commit()
            make_derived_database(state / "derived.sqlite3")
            destination = root / "backup"
            report = create_state_backup(REPO, state, destination)
            self.assertEqual(
                ["derived.sqlite3", "events.sqlite3"], report["snapshot_order"]
            )
            self.assertEqual("complete", report["status"])
            self.assertTrue((destination / "backup-manifest.json").is_file())

    def test_backup_reports_current_rows_after_the_sealed_import_baseline(self) -> None:
        with tempfile.TemporaryDirectory(prefix="pai-state-backup-growth-") as temporary:
            root = Path(temporary)
            state = root / "state"
            state.mkdir()
            events = state / "events.sqlite3"
            make_database(events)
            with closing(sqlite3.connect(events)) as connection:
                connection.execute(
                    "CREATE TABLE pai_storage_meta "
                    "(meta_key TEXT PRIMARY KEY,meta_value TEXT NOT NULL)"
                )
                connection.execute(
                    "INSERT INTO pai_storage_meta VALUES('storage_id','fixture-ledger')"
                )
                connection.commit()
            derived = state / "derived.sqlite3"
            make_derived_database(derived)
            with closing(sqlite3.connect(derived)) as connection:
                connection.execute("INSERT INTO pai_memory_nodes VALUES('post-baseline')")
                connection.commit()
            report = create_state_backup(REPO, state, root / "backup")
            self.assertEqual(1, report["evidence"]["memory_node_count"])
            self.assertEqual(0, report["evidence"]["memory_edge_count"])

    def test_restore_verification_detects_database_tampering(self) -> None:
        with tempfile.TemporaryDirectory(prefix="pai-state-verify-") as temporary:
            root = Path(temporary)
            state = root / "state"
            state.mkdir()
            events = state / "events.sqlite3"
            make_database(events)
            with closing(sqlite3.connect(events)) as connection:
                connection.execute(
                    "CREATE TABLE pai_storage_meta "
                    "(meta_key TEXT PRIMARY KEY,meta_value TEXT NOT NULL)"
                )
                connection.execute(
                    "INSERT INTO pai_storage_meta VALUES('storage_id','fixture-ledger')"
                )
                connection.commit()
            make_derived_database(state / "derived.sqlite3")
            destination = root / "backup"
            create_state_backup(REPO, state, destination)
            verified = verify_state_backup(destination)
            self.assertEqual("verified", verified["status"])
            with open(destination / "events.sqlite3", "r+b") as stream:
                stream.seek(-1, 2)
                final = stream.read(1)
                stream.seek(-1, 2)
                stream.write(bytes([final[0] ^ 1]))
            with self.assertRaisesRegex(SystemExit, "digest"):
                verify_state_backup(destination)

    def test_backup_and_verify_commands_need_no_provider_credentials(self) -> None:
        args = parse_args(["--backup-state", "safe-copy"])
        self.assertEqual(Path("safe-copy"), args.backup_state)
        args = parse_args(["--verify-restore", "safe-copy"])
        self.assertEqual(Path("safe-copy"), args.verify_restore)

    def test_remote_web_requires_a_strong_uncommitted_credential(self) -> None:
        args = parse_args(
            ["--provider", "local", "--web", "--web-address", "0.0.0.0"]
        )
        with self.assertRaisesRegex(SystemExit, "PAI_WEB_PASSWORD"):
            web_environment(args, {})
        environment = web_environment(
            args, {"PAI_WEB_PASSWORD": "a-private-password-with-32-characters"}
        )
        self.assertEqual(
            {
                "PAI_WEB_ENABLED": "1",
                "PAI_WEB_ADDRESS": "0.0.0.0",
                "PAI_WEB_PORT": "8080",
            },
            environment,
        )
        self.assertNotIn("PAI_WEB_PASSWORD", environment)

    def test_web_file_mutation_is_separate_explicit_authority(self) -> None:
        args = parse_args(
            [
                "--provider", "local", "--web",
                "--web-file-mutation", "--web-port", "9090",
            ]
        )
        self.assertEqual(
            "authenticated", web_environment(args, {})["PAI_WEB_FILE_MUTATION"]
        )

    def test_web_options_are_closed_when_web_is_not_selected(self) -> None:
        args = parse_args(["--provider", "local", "--web-address", "127.0.0.1"])
        with self.assertRaisesRegex(SystemExit, "require --web"):
            web_environment(args, {})

    def test_embedding_endpoint_is_local_only(self) -> None:
        self.assertEqual(
            "http://127.0.0.1:11435/api/embeddings",
            validate_embedding_endpoint(
                "http://127.0.0.1:11435/api/embeddings"
            ),
        )
        self.assertEqual(
            "http://host.docker.internal:11434/api/embeddings",
            validate_embedding_endpoint(
                "http://host.docker.internal:11434/api/embeddings"
            ),
        )
        for endpoint in (
            "https://example.com/api/embeddings",
            "http://127.0.0.1:11435/not-embeddings",
            "file:///private/memory",
        ):
            with self.subTest(endpoint=endpoint), self.assertRaises(SystemExit):
                validate_embedding_endpoint(endpoint)

    def test_memory_cutover_backup_is_integrity_checked_and_non_destructive(self) -> None:
        with tempfile.TemporaryDirectory(prefix="pai-cli-backup-") as temporary:
            state = Path(temporary)
            source = state / "events.sqlite3"
            rows = make_database(source)
            directory = backup_memory_cutover_state(state)
            self.assertIsNotNone(directory)
            backup = directory / "events.sqlite3"
            self.assertTrue(backup.is_file())
            with closing(sqlite3.connect(backup)) as connection:
                stored = [
                    row[0]
                    for row in connection.execute(
                        "SELECT event_json FROM pai_events ORDER BY storage_sequence"
                    )
                ]
            self.assertEqual(rows, stored)
            with closing(sqlite3.connect(source)) as connection:
                original = [
                    row[0]
                    for row in connection.execute(
                        "SELECT event_json FROM pai_events ORDER BY storage_sequence"
                    )
                ]
            self.assertEqual(rows, original)

    def test_memory_migration_requires_explicit_labelled_clone_port(self) -> None:
        args = parse_args(["--provider", "local", "--migrate-memory"])
        with self.assertRaisesRegex(SystemExit, "--clone-postgres-port"):
            memory_migration_environment(args)

    def test_clone_port_is_closed_outside_one_time_memory_migration(self) -> None:
        args = parse_args(
            ["--provider", "local", "--clone-postgres-port", "5434"]
        )
        with self.assertRaisesRegex(SystemExit, "only valid with --migrate-memory"):
            memory_migration_environment(args)

    def test_memory_migration_environment_is_loopback_and_read_only_scoped(self) -> None:
        args = parse_args(
            [
                "--provider",
                "local",
                "--migrate-memory",
                "--clone-postgres-port",
                "5434",
            ]
        )
        self.assertEqual(
            {
                "PAI_MEMORY_MIGRATION_SOURCE": "labelled-local-postgres-clone",
                "PAI_PG_BACKUP": "off",
                "PAI_PG_HOST": "127.0.0.1",
                "PAI_PG_PORT": "5434",
                "PAI_PG_DATABASE": "pai_memory",
                "PAI_PG_USER": "pai",
                "PAI_PG_PASSWORD": "pai_local_dev_only",
                "PAI_DEV_DATABASE_LABEL": "clone",
            },
            memory_migration_environment(args),
        )

    def test_memory_migration_allows_only_explicit_local_clone_hosts(self) -> None:
        args = parse_args(
            [
                "--provider", "local", "--migrate-memory",
                "--clone-postgres-host", "host.docker.internal",
                "--clone-postgres-port", "5434",
            ]
        )
        self.assertEqual(
            "host.docker.internal",
            memory_migration_environment(args)["PAI_PG_HOST"],
        )

        args = parse_args(
            [
                "--provider", "local", "--migrate-memory",
                "--clone-postgres-host", "database.example.com",
                "--clone-postgres-port", "5434",
            ]
        )
        with self.assertRaisesRegex(SystemExit, "approved local host"):
            memory_migration_environment(args)

        args = parse_args(
            [
                "--provider", "local",
                "--clone-postgres-host", "host.docker.internal",
            ]
        )
        with self.assertRaisesRegex(SystemExit, "only valid with --migrate-memory"):
            memory_migration_environment(args)

    def test_memory_migration_can_bind_source_agent_manifest(self) -> None:
        digest = "a" * 64
        args = parse_args(
            [
                "--provider", "local", "--migrate-memory",
                "--clone-postgres-port", "5434",
                "--memory-source-agent-id", "source-agent",
                "--memory-migration-manifest-sha256", digest,
            ]
        )
        environment = memory_migration_environment(args)
        self.assertEqual(
            "source-agent", environment["PAI_MEMORY_MIGRATION_SOURCE_AGENT_ID"]
        )
        self.assertEqual(
            digest, environment["PAI_MEMORY_MIGRATION_MANIFEST_SHA256"]
        )

    def test_memory_migration_source_provenance_is_atomic(self) -> None:
        args = parse_args(
            [
                "--provider", "local", "--migrate-memory",
                "--clone-postgres-port", "5434",
                "--memory-source-agent-id", "source-agent",
            ]
        )
        with self.assertRaisesRegex(SystemExit, "must be supplied together"):
            memory_migration_environment(args)

    def test_memory_migration_source_agent_is_a_stable_identifier(self) -> None:
        args = parse_args(
            [
                "--provider", "local", "--migrate-memory",
                "--clone-postgres-port", "5434",
                "--memory-source-agent-id", "ambiguous:source",
                "--memory-migration-manifest-sha256", "a" * 64,
            ]
        )
        with self.assertRaisesRegex(SystemExit, "stable identifier"):
            memory_migration_environment(args)

    def test_migration_only_requires_a_selected_migration(self) -> None:
        args = parse_args(["--provider", "local", "--migration-only"])
        with self.assertRaisesRegex(SystemExit, "requires an explicit migration"):
            memory_migration_environment(args)

    def test_export_is_exact_and_verified(self) -> None:
        with tempfile.TemporaryDirectory(prefix="pai-cli-export-") as temporary:
            root = Path(temporary)
            database = root / "events.sqlite3"
            destination = root / "events-export.jsonl"
            rows = make_database(database)
            report = export_event_ledger(REPO, database, destination)
            self.assertEqual({"event_count": 2, "maximum_event_id": 2}, report)
            self.assertEqual(rows, destination.read_text(encoding="utf-8").splitlines())

    def test_export_never_overwrites(self) -> None:
        with tempfile.TemporaryDirectory(prefix="pai-cli-export-") as temporary:
            root = Path(temporary)
            database = root / "events.sqlite3"
            destination = root / "events-export.jsonl"
            make_database(database)
            destination.write_text("keep\n", encoding="utf-8")
            with self.assertRaises(SystemExit):
                export_event_ledger(REPO, database, destination)
            self.assertEqual("keep\n", destination.read_text(encoding="utf-8"))

    def test_corrupt_row_publishes_no_export(self) -> None:
        with tempfile.TemporaryDirectory(prefix="pai-cli-export-") as temporary:
            root = Path(temporary)
            database = root / "events.sqlite3"
            destination = root / "events-export.jsonl"
            make_database(database, corrupt=True)
            with self.assertRaises(SystemExit):
                export_event_ledger(REPO, database, destination)
            self.assertFalse(destination.exists())

    def test_cli_export_needs_no_provider_credentials(self) -> None:
        with tempfile.TemporaryDirectory(prefix="pai-cli-export-") as temporary:
            root = Path(temporary)
            destination = root / "events-export.jsonl"
            make_database(root / "events.sqlite3")
            completed = subprocess.run(
                [
                    sys.executable,
                    str(REPO / "scripts" / "pai_cli.py"),
                    "--state-dir",
                    str(root),
                    "--export-events",
                    str(destination),
                ],
                cwd=REPO,
                text=True,
                capture_output=True,
                check=False,
            )
            self.assertEqual(0, completed.returncode, completed.stderr)
            self.assertIn("Exported 2 verified events through id 2", completed.stdout)
            self.assertTrue(destination.is_file())


if __name__ == "__main__":
    unittest.main()
