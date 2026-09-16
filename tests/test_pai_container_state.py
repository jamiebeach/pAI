"""Named-volume state import qualification."""

from __future__ import annotations

from contextlib import closing
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
