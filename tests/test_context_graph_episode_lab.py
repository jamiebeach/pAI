"""Provider-free contracts for the immutable context-graph episode lab."""

from __future__ import annotations

import argparse
from contextlib import closing
import json
from pathlib import Path
import sqlite3
import sys
import tempfile
from types import SimpleNamespace
import unittest
from unittest.mock import patch


REPO = Path(__file__).resolve().parent.parent
sys.path.insert(0, str(REPO / "scripts"))

from context_graph_episode_lab import (  # noqa: E402
    build_catalog,
    load_seed,
    make_seed,
    sqlite_read_only,
)


AGENT_ID = "fixture-agent"
PERSONA_ID = "fixture-persona"


def event(event_id: int, event_type: str, payload: dict,
          caused_by: int | None = None) -> dict:
    value = {
        "id": event_id,
        "type": event_type,
        "agent_id": AGENT_ID,
        "timestamp": "2026-01-01T00:00:00Z",
        "payload": payload,
    }
    if caused_by is not None:
        value["caused_by"] = caused_by
    return value


def record_payload(record: dict) -> dict:
    return {
        "persona_id": PERSONA_ID,
        "generation": "identity-formation-owner-v6",
        "record_json": json.dumps(record, separators=(",", ":")),
    }


class ContextGraphEpisodeLabTests(unittest.TestCase):
    def setUp(self) -> None:
        self.temporary = tempfile.TemporaryDirectory()
        self.root = Path(self.temporary.name)
        self.events = self.root / "events.sqlite3"
        self.derived = self.root / "derived.sqlite3"
        self._make_events()
        self._make_derived()

    def tearDown(self) -> None:
        self.temporary.cleanup()

    def _make_events(self) -> None:
        rows = [
            event(1, "conversation-episode-sealed", {
                "persona_id": PERSONA_ID,
                "episode_id": "episode:fixture:completed",
                "sealed_at": 10,
                "synopsis": "A completed synthetic episode.",
                "subjects": ["replay"],
                "entities": ["Example Device"],
            }),
            event(2, "context-graph-identity-opened", record_payload({
                "episode_event_id": 1,
                "formation_protocol": "identity-formation-v9",
                "attempt": 1,
                "batch_index": 0,
            }), caused_by=1),
            event(3, "context-graph-identity-phase", record_payload({
                "phase": "facts", "outcome": "request",
            }), caused_by=2),
            event(4, "context-graph-identity-completed", record_payload({
                "result": {
                    "proposal": {
                        "entities": [{"label": "Example Sensor"}],
                        "relationships": [{"predicate": "observed"}],
                    }
                }
            }), caused_by=2),
            event(5, "conversation-episode-sealed", {
                "persona_id": PERSONA_ID,
                "episode_id": "episode:fixture:failed",
                "sealed_at": 20,
                "synopsis": "A failed synthetic episode.",
                "subjects": [], "entities": [],
            }),
            event(6, "context-graph-identity-opened", record_payload({
                "episode_event_id": 5,
                "formation_protocol": "identity-formation-v9",
                "attempt": 1, "batch_index": 0,
            }), caused_by=5),
            event(7, "context-graph-identity-failed", record_payload({
                "failure_class": "fixture-failure", "reason": "fixture",
            }), caused_by=6),
            event(8, "conversation-episode-sealed", {
                "persona_id": PERSONA_ID,
                "episode_id": "episode:fixture:opened",
                "sealed_at": 30,
                "synopsis": "An open synthetic episode.",
                "subjects": [], "entities": [],
            }),
            event(9, "context-graph-identity-opened", record_payload({
                "episode_event_id": 8,
                "formation_protocol": "identity-formation-v9",
                "attempt": 1, "batch_index": 0,
            }), caused_by=8),
            event(10, "conversation-episode-sealed", {
                "persona_id": PERSONA_ID,
                "episode_id": "episode:fixture:queued",
                "sealed_at": 40,
                "synopsis": "A queued synthetic episode.",
                "subjects": [], "entities": [],
            }),
            event(11, "user-message", {
                "persona_id": PERSONA_ID,
                "content": "SOURCE-TEXT-MUST-NOT-ENTER-CATALOG",
            }),
        ]
        with closing(sqlite3.connect(self.events)) as connection:
            connection.execute(
                "CREATE TABLE pai_events ("
                "storage_sequence INTEGER PRIMARY KEY,event_id INTEGER NOT NULL,"
                "storage_origin TEXT NOT NULL,agent_id TEXT NOT NULL,"
                "partition_status TEXT NOT NULL,event_type TEXT NOT NULL,"
                "occurred_at TEXT NOT NULL,event_json TEXT NOT NULL,"
                "integrity_hash TEXT NOT NULL)"
            )
            for position, value in enumerate(rows, 1):
                connection.execute(
                    "INSERT INTO pai_events VALUES (?,?,?,?,?,?,?,?,?)",
                    (position, value["id"], "fixture", AGENT_ID, "native",
                     value["type"], value["timestamp"],
                     json.dumps(value, separators=(",", ":")),
                     f"hash-{position}"),
                )
            connection.commit()

    def _make_derived(self) -> None:
        with closing(sqlite3.connect(self.derived)) as connection:
            connection.execute(
                "CREATE TABLE pai_projection_checkpoints (name TEXT)"
            )
            connection.execute(
                "CREATE TABLE pai_memory_projection_binding ("
                "projection_name TEXT PRIMARY KEY,"
                "baseline_storage_position INTEGER NOT NULL)"
            )
            connection.execute(
                "INSERT INTO pai_memory_projection_binding VALUES ('canonical',7)"
            )
            connection.commit()

    def test_catalog_classifies_lifecycle_without_raw_messages(self) -> None:
        catalog = build_catalog(self.events, AGENT_ID, PERSONA_ID)
        self.assertEqual(catalog["episode_count"], 4)
        self.assertEqual(
            catalog["status_counts"],
            {"completed": 1, "failed": 1, "opened": 1, "queued": 1},
        )
        encoded = json.dumps(catalog)
        self.assertNotIn("SOURCE-TEXT-MUST-NOT-ENTER-CATALOG", encoded)
        completed = next(row for row in catalog["episodes"]
                         if row["status"] == "completed")
        self.assertEqual(completed["before_event_id"], 1)
        self.assertEqual(completed["after_event_id"], 4)
        self.assertEqual(completed["query_suggestions"],
                         ["Example Device", "Example Sensor"])

    def test_reference_seed_is_hash_pinned_and_tamper_evident(self) -> None:
        seed = self.root / "seed"
        options = argparse.Namespace(
            events=self.events,
            derived=self.derived,
            output=seed,
            agent_id=None,
            persona_id=None,
            storage_mode="reference",
            confirm_quiescent_backup=True,
        )
        self.assertEqual(make_seed(options), seed)
        manifest, catalog = load_seed(seed)
        self.assertEqual(manifest["storage_mode"], "reference")
        self.assertEqual(manifest["recovery_start_storage_position"], 7)
        self.assertEqual(catalog["episode_count"], 4)
        catalog_path = seed / "catalog.json"
        catalog_path.write_bytes(catalog_path.read_bytes() + b" ")
        with self.assertRaisesRegex(ValueError, "catalog hash mismatch"):
            load_seed(seed)

    def test_sqlite_backup_seed_is_self_contained_and_verified(self) -> None:
        seed = self.root / "live-seed"
        options = argparse.Namespace(
            events=self.events,
            derived=self.derived,
            output=seed,
            agent_id=None,
            persona_id=None,
            storage_mode="sqlite-backup",
            confirm_quiescent_backup=False,
            confirm_live_snapshot=True,
        )
        self.assertEqual(make_seed(options), seed)
        manifest, catalog = load_seed(seed)
        self.assertEqual(manifest["storage_mode"], "sqlite-backup")
        self.assertEqual(manifest["events"]["file"], "events.sqlite3")
        self.assertEqual(manifest["derived"]["file"], "derived.sqlite3")
        self.assertTrue((seed / "events.sqlite3").is_file())
        self.assertTrue((seed / "derived.sqlite3").is_file())
        self.assertEqual(catalog["episode_count"], 4)

    def test_sqlite_snapshot_connection_is_query_only(self) -> None:
        with closing(sqlite_read_only(self.events)) as connection:
            self.assertEqual(
                connection.execute("SELECT COUNT(*) FROM pai_events").fetchone()[0],
                11,
            )
            with self.assertRaises(sqlite3.DatabaseError):
                connection.execute("DELETE FROM pai_events")

    def test_seed_refuses_to_consume_minimum_free_space_reserve(self) -> None:
        seed = self.root / "space-refused-seed"
        options = argparse.Namespace(
            events=self.events,
            derived=self.derived,
            output=seed,
            agent_id=None,
            persona_id=None,
            storage_mode="sqlite-backup",
            confirm_quiescent_backup=False,
            confirm_live_snapshot=True,
        )
        with patch(
            "context_graph_episode_lab.shutil.disk_usage",
            return_value=SimpleNamespace(free=1),
        ):
            with self.assertRaisesRegex(ValueError, "Insufficient free space"):
                make_seed(options)
        self.assertFalse(seed.exists())

    def test_hash_pinned_seed_survives_unsupported_mode_hardening(self) -> None:
        seed = self.root / "mode-portable-seed"
        options = argparse.Namespace(
            events=self.events,
            derived=self.derived,
            output=seed,
            agent_id=None,
            persona_id=None,
            storage_mode="sqlite-backup",
            confirm_quiescent_backup=False,
            confirm_live_snapshot=True,
        )
        with patch.object(
            Path, "chmod", side_effect=PermissionError(1, "unsupported")
        ):
            self.assertEqual(make_seed(options), seed)
        manifest, catalog = load_seed(seed)
        self.assertEqual(manifest["storage_mode"], "sqlite-backup")
        self.assertEqual(catalog["episode_count"], 4)

    def test_visualizer_does_not_depend_on_color_for_delta(self) -> None:
        assets = REPO / "scripts" / "context-graph-episode-lab"
        markup = (assets / "index.html").read_text(encoding="utf-8")
        styles = (assets / "styles.css").read_text(encoding="utf-8")
        script = (assets / "app.js").read_text(encoding="utf-8")

        self.assertIn("Emphasize delta", markup)
        self.assertIn("hexagon / thick edge", markup)
        self.assertIn("square / dashed edge", markup)
        self.assertIn("crossed circle / dotted edge", markup)
        self.assertIn("Not query-eligible", markup)
        self.assertIn("Inferred", markup)
        self.assertIn("PROPOSAL → ADMISSION", markup)
        self.assertIn("stroke-dasharray: 11 4", styles)
        self.assertIn("stroke-dasharray: 2 5", styles)
        self.assertIn("stroke-dasharray: 8 3 2 3", styles)
        self.assertIn(".formation-attempt.issue", styles)
        self.assertIn("svgElement('polygon'", script)
        self.assertIn("svgElement('rect'", script)
        self.assertIn("class: 'removal-cross'", script)
        self.assertIn("eligibilitySymbol", script)
        self.assertIn("inferenceSymbol", script)
        self.assertIn("`[${symbols[change] || '='}]", script)
        self.assertIn("renderFormationAudit", script)
        self.assertIn("item.label || item.claim_ref", script)
        self.assertIn("Check saved run against case expectations — no replay", markup)
        self.assertIn("/api/case-verify", script)
        self.assertIn("Execute exactly this phase — 1 provider call", markup)
        self.assertIn("/api/case-execute", script)


if __name__ == "__main__":
    unittest.main()
