"""Bounded transport/capture tests; cognitive decisions remain in Lisp."""
import json
from pathlib import Path
import sqlite3
import sys
import tempfile
import unittest

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "scripts"))
from context_graph_receipt_case import CONTRACT, capture, capture_preparation, save_capture


class ReceiptCaptureTests(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.addCleanup(self.temp.cleanup)
        self.path = Path(self.temp.name) / "events.sqlite3"
        self.db = sqlite3.connect(self.path)
        self.addCleanup(self.db.close)
        self.db.executescript("""
          CREATE TABLE pai_events(storage_sequence INTEGER PRIMARY KEY,
            agent_id TEXT,event_id INTEGER,event_type TEXT,event_json TEXT);
          CREATE INDEX aid ON pai_events(agent_id,event_id,storage_sequence);
          CREATE INDEX atid ON pai_events(agent_id,event_type,event_id,storage_sequence);
        """)
        self.add(1, "conversation-episode-sealed", {})
        for op, generation, terminal in ((10, "identity-formation-owner-v6", "empty"),
                                         (20, CONTRACT["generation"], "failed"),
                                         (30, CONTRACT["generation"], "reviewed")):
            self.add(op, "context-graph-identity-opened", {
                "episode_event_id": 1, "batch_index": 0,
                "formation_protocol": CONTRACT["protocol"],
                "ontology_revision": CONTRACT["ontology_revision"]}, generation=generation)
            typ = "failed" if terminal == "failed" else "completed"
            self.add(op + 1, "context-graph-identity-" + typ,
                     {"result": {"status": terminal}}, cause=op, generation=generation)

    def add(self, eid, typ, record, cause=None, generation=None):
        payload = {"persona_id": "fixture", "generation": generation,
                   "record_json": json.dumps(record)}
        event = {"id": eid, "type": typ, "payload": payload, "caused_by": cause}
        self.db.execute("INSERT INTO pai_events VALUES(?,?,?,?,?)",
                        (eid, "fixture-agent", eid, typ, json.dumps(event)))
        self.db.commit()

    def run_capture(self, **overrides):
        args = dict(agent_id="fixture-agent", persona_id="fixture", episode_ids=[1],
                    contract=CONTRACT, cutoff=40, recovery_position=0)
        args.update(overrides)
        return capture(self.path, **args)

    def test_generation_and_latest_attempt_are_separate(self):
        before = self.path.read_bytes()
        result = self.run_capture()
        self.assertEqual(result["counts"]["attempts"], 2)
        self.assertEqual(result["counts"]["reviewed"], 1)
        self.assertEqual(result["counts"]["failed"], 0)
        self.assertEqual(before, self.path.read_bytes())
        self.assertTrue(all("SEARCH" in plan for plan in result["metrics"]["query_plans"]
                            if "pai_events" in plan))

    def test_mismatch_refused(self):
        with self.assertRaises(ValueError):
            self.run_capture(contract={**CONTRACT, "profile": "direct-v6"})

    def test_cutoff_keeps_pending_unknown(self):
        result = self.run_capture(cutoff=30)
        self.assertEqual(result["counts"]["pending"], 1)

    def test_allowance_stops_without_partial_success(self):
        with self.assertRaises(TimeoutError):
            self.run_capture(maximum_bytes=20)

    def test_saved_case_cannot_be_overwritten(self):
        output = Path(self.temp.name) / "case.json"
        save_capture(self.run_capture(), output)
        with self.assertRaises(FileExistsError):
            save_capture({}, output)

    def test_preparation_selects_only_receipts_and_dependencies(self):
        root = json.loads(self.db.execute("SELECT event_json FROM pai_events WHERE event_id=1").fetchone()[0])
        root["payload"]["source_event_ids"] = [2]
        self.db.execute("UPDATE pai_events SET event_json=? WHERE event_id=1", (json.dumps(root),))
        self.add(2, "user-message", {})
        self.add(3, "user-message", {})
        self.add(4, "conversation-episode-sealed", {})
        before = self.path.read_bytes()
        result = capture_preparation(self.path, agent_id="fixture-agent", persona_id="fixture",
                                     contract=CONTRACT, cutoff=40, recovery_position=0)
        ids = {row["event"]["id"] for row in result["events"]}
        self.assertEqual(ids, {1, 2, 20, 21, 30, 31})
        self.assertEqual(result["metrics"]["receipt_count"], 4)
        self.assertEqual(result["metrics"]["episode_count"], 1)
        self.assertEqual(result["metrics"]["sealed_episode_count"], 2)
        self.assertEqual(result["metrics"]["unopened_sealed_episode_count"], 1)
        self.assertEqual(result["metrics"]["sealed_episode_count_after_latest_opened"], 1)
        self.assertEqual(result["metrics"]["first_sealed_episode_after_latest_opened"], 4)
        self.assertFalse(result["metrics"]["episode_coverage_complete"])
        self.assertEqual(before, self.path.read_bytes())


if __name__ == "__main__":
    unittest.main()
