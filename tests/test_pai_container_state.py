"""Named-volume state import qualification."""

from __future__ import annotations

from contextlib import closing
import hashlib
import json
from pathlib import Path
import sqlite3
import sys
import tempfile
import unittest


REPO = Path(__file__).resolve().parent.parent
sys.path.insert(0, str(REPO / "scripts"))

from import_container_state import import_container_state  # noqa: E402
from tests.test_pai_cli_storage import (  # noqa: E402
    make_database,
    make_derived_database,
)


class PaiContainerStateTests(unittest.TestCase):
    def make_state(self, root: Path) -> Path:
        state = root / "source"
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
        personas = state / "personas"
        personas.mkdir()
        (personas / "fixture.json").write_text(
            '{"schema_version":1}\n', encoding="utf-8"
        )
        cache = state / "host-cache"
        cache.mkdir()
        (cache / "discard.fasl").write_bytes(b"cache")
        return state

    def test_import_preserves_state_and_excludes_host_cache(self) -> None:
        with tempfile.TemporaryDirectory(prefix="pai-volume-import-") as temporary:
            root = Path(temporary)
            source = self.make_state(root)
            target = root / "target"
            report = import_container_state(source, target)
            self.assertEqual(report["status"], "complete")
            self.assertTrue((target / "events.sqlite3").is_file())
            self.assertTrue((target / "personas" / "fixture.json").is_file())
            self.assertFalse((target / "host-cache").exists())
            manifest = json.loads(
                (target / "container-state-import.json").read_text(encoding="utf-8")
            )
            self.assertEqual(manifest["evidence"], report["evidence"])

    def test_import_recovers_a_still_open_source_wal_instead_of_dropping_it(
        self,
    ) -> None:
        # Regression: the copy step must never open the source database
        # with SQLite (that is the operation that fails "unable to open
        # database file" over a Windows Docker Desktop bind mount even
        # though the file itself is perfectly readable) -- it must be a
        # plain file copy. Proof that this still preserves durability: a
        # row committed to the WAL but not yet checkpointed into the main
        # file (only possible while a connection stays open) survives the
        # import and reads back from the copy.
        with tempfile.TemporaryDirectory(prefix="pai-volume-wal-") as temporary:
            root = Path(temporary)
            source = self.make_state(root)
            events = source / "events.sqlite3"
            connection = sqlite3.connect(events)
            try:
                connection.execute("PRAGMA journal_mode=WAL")
                event_json = json.dumps({"id": 3, "type": "user-message"})
                digest = hashlib.sha256(event_json.encode("utf-8")).hexdigest()
                connection.execute(
                    "INSERT INTO pai_events(event_id,event_json,integrity_hash) "
                    "VALUES(3,?,?)",
                    (event_json, digest),
                )
                connection.commit()
                self.assertTrue((source / "events.sqlite3-wal").is_file())
                target = root / "target"
                import_container_state(source, target)
            finally:
                connection.close()
            with closing(sqlite3.connect(target / "events.sqlite3")) as check:
                count = check.execute(
                    "SELECT COUNT(*) FROM pai_events"
                ).fetchone()[0]
            self.assertEqual(count, 3)

    def test_import_refuses_nonempty_target(self) -> None:
        with tempfile.TemporaryDirectory(prefix="pai-volume-nonempty-") as temporary:
            root = Path(temporary)
            source = self.make_state(root)
            target = root / "target"
            target.mkdir()
            (target / "existing").write_text("preserve", encoding="utf-8")
            with self.assertRaisesRegex(SystemExit, "not empty"):
                import_container_state(source, target)
            self.assertEqual(
                (target / "existing").read_text(encoding="utf-8"), "preserve"
            )


if __name__ == "__main__":
    unittest.main()
