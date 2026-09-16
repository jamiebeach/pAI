import json
from pathlib import Path
import tempfile
import unittest

from scripts.prepare_agent_migration import prepare_migration, read_canonical_events


def event(event_id, event_type, text=None, caused_by=None, timestamp=None):
    payload = {"text": text} if text is not None else {"value": event_id}
    return {
        "id": event_id,
        "timestamp": timestamp or f"2026-07-27T12:00:{event_id:02d}Z",
        "type": event_type,
        "payload": payload,
        "caused_by": caused_by,
    }


class PrepareAgentMigrationTests(unittest.TestCase):
    def write(self, path: Path, rows):
        path.write_text(
            "".join(json.dumps(row, separators=(",", ":")) + "\n" for row in rows),
            encoding="utf-8",
        )

    def test_overlap_is_deduplicated_and_historical_dialogue_is_non_stimulus(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            first = root / "first.jsonl"
            second = root / "second.jsonl"
            output = root / "prepared.jsonl"
            manifest = root / "manifest.json"
            user = event(1, "user-message", "My companion is called Ash.")
            assistant = event(2, "agent-message", "I will remember that.", 1)
            self.write(first, [user, assistant])
            self.write(second, [assistant, event(4, "tool-call")])

            report = prepare_migration(
                [first, second], output, manifest,
                source_agent_id="source-agent",
                destination_agent_id="destination-agent",
                destination_persona_id="destination-persona",
                not_before="2026-07-27T00:00:00Z",
            )
            rows = [json.loads(line) for line in output.read_text(encoding="utf-8").splitlines()]
            historical = rows[3:]
            self.assertEqual(1, report["duplicate_source_row_count"])
            self.assertEqual([3], report["missing_event_ids"])
            self.assertEqual([1, 2, 4], [row["id"] for row in rows[:3]])
            self.assertEqual(
                ["historical-user-message-imported", "historical-agent-message-imported"],
                [row["type"] for row in historical],
            )
            self.assertEqual(historical[0]["id"], historical[1]["caused_by"])
            metadata = historical[1]["payload"]["metadata"]
            self.assertEqual("source-agent", metadata["source_agent_id"])
            self.assertEqual(2, metadata["source_event_id"])
            self.assertEqual(64, len(metadata["source_event_sha256"]))
            self.assertEqual(report, json.loads(manifest.read_text(encoding="utf-8")))

    def test_conflicting_duplicate_id_fails_closed(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            first = root / "first.jsonl"
            second = root / "second.jsonl"
            self.write(first, [event(1, "user-message", "first")])
            self.write(second, [event(1, "user-message", "different")])
            with self.assertRaisesRegex(ValueError, "conflicting source events"):
                read_canonical_events([first, second])

    def test_cutoff_limits_normalized_dialogue_but_not_canonical_authority(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            source = root / "source.jsonl"
            output = root / "prepared.jsonl"
            manifest = root / "manifest.json"
            self.write(source, [
                event(1, "user-message", "before", timestamp="2026-07-26T23:59:59Z"),
                event(2, "user-message", "after", timestamp="2026-07-27T00:00:00Z"),
            ])
            report = prepare_migration(
                [source], output, manifest,
                source_agent_id="source-agent",
                destination_agent_id="destination-agent",
                destination_persona_id="destination-persona",
                not_before="2026-07-27T00:00:00Z",
            )
            rows = output.read_text(encoding="utf-8").splitlines()
            self.assertEqual(2, report["canonical_event_count"])
            self.assertEqual(1, report["normalized_dialogue_event_count"])
            self.assertEqual(3, len(rows))

    def test_existing_outputs_are_never_overwritten(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            source = root / "source.jsonl"
            output = root / "prepared.jsonl"
            manifest = root / "manifest.json"
            self.write(source, [event(1, "tool-call")])
            output.write_text("keep", encoding="utf-8")
            with self.assertRaisesRegex(ValueError, "refusing to overwrite"):
                prepare_migration(
                    [source], output, manifest,
                    source_agent_id="source-agent",
                    destination_agent_id="destination-agent",
                    destination_persona_id="destination-persona",
                    not_before="2026-07-27T00:00:00Z",
                )
            self.assertEqual("keep", output.read_text(encoding="utf-8"))

    def test_source_agent_id_cannot_ambiguate_provenance_ids(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            source = root / "source.jsonl"
            self.write(source, [event(1, "user-message", "hello")])
            with self.assertRaisesRegex(ValueError, "stable identifier"):
                prepare_migration(
                    [source], root / "prepared.jsonl", root / "manifest.json",
                    source_agent_id="ambiguous:source",
                    destination_agent_id="destination-agent",
                    destination_persona_id="destination-persona",
                    not_before="2026-07-27T00:00:00Z",
                )


if __name__ == "__main__":
    unittest.main()
