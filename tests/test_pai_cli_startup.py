"""Canonical CLI startup must not append below the durable event watermark."""

from __future__ import annotations

from contextlib import closing
import json
from pathlib import Path
import sqlite3
import subprocess
import sys
import tempfile
import unittest


class PaiCliStartupTests(unittest.TestCase):
    def test_event_only_state_fails_closed_without_corrupting_ids(self) -> None:
        repo = Path(__file__).resolve().parent.parent
        with tempfile.TemporaryDirectory(prefix="pai-cli-startup-") as temporary:
            state = Path(temporary)
            event_file = state / "events.jsonl"
            with event_file.open("w", encoding="utf-8", newline="\n") as stream:
                for event_id in range(1, 4):
                    event = {
                        "schema_version": 1,
                        "id": event_id,
                        "timestamp": "2026-08-18T00:00:00Z",
                        "type": "user-message",
                        "agent_id": "q45-conversation-dev",
                        "payload": {
                            "text": f"seed {event_id}",
                            "metadata": {"source": "q4.5-conversation"},
                        },
                        "caused_by": None,
                        "tick_id": None,
                        "affect_snapshot": None,
                    }
                    stream.write(json.dumps(event, separators=(",", ":")) + "\n")
            source_before = event_file.read_bytes()

            completed = subprocess.run(
                [
                    sys.executable,
                    str(repo / "scripts" / "pai_cli.py"),
                    "--provider",
                    "local",
                    "--state-dir",
                    str(state),
                    "--migrate-events",
                ],
                cwd=repo,
                input="/quit\n",
                text=True,
                capture_output=True,
                timeout=180,
                check=False,
            )
            self.assertNotEqual(completed.returncode, 0)
            for phase in range(1, 5):
                self.assertIn(f"[startup {phase}/5]", completed.stdout)
            self.assertNotIn("WARNING:", completed.stderr)
            self.assertNotIn("caught WARNING", completed.stderr)
            self.assertIn("event ledger contains no complete memory baseline",
                          completed.stderr)
            self.assertEqual(source_before, event_file.read_bytes())
            events = [
                json.loads(line)
                for line in event_file.read_text(encoding="utf-8").splitlines()
                if line.strip()
            ]
            ids = [event["id"] for event in events]
            self.assertEqual(len(ids), len(set(ids)), events)
            self.assertEqual(ids, sorted(ids), events)
            self.assertTrue(all(event_id > 3 for event_id in ids[3:]), events)
            database = state / "events.sqlite3"
            self.assertTrue(database.is_file())
            with closing(sqlite3.connect(database)) as connection:
                stored_ids = [
                    row[0]
                    for row in connection.execute(
                        "SELECT event_id FROM pai_events ORDER BY storage_sequence"
                    )
                ]
            self.assertEqual([1, 2, 3], stored_ids)
            derived = state / "derived.sqlite3"
            self.assertTrue(derived.is_file())
            with closing(sqlite3.connect(derived)) as connection:
                self.assertEqual(
                    (1,),
                    connection.execute(
                        "SELECT count(*) FROM pai_projection_checkpoints"
                    ).fetchone(),
                )
            with closing(sqlite3.connect(database)) as connection:
                self.assertEqual(
                    (0,),
                    connection.execute(
                        "SELECT count(*) FROM pai_projection_checkpoints"
                    ).fetchone(),
                )


if __name__ == "__main__":
    unittest.main()
