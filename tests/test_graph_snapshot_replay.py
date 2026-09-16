"""Source preservation checks; synthetic SQLite only, no model or instance."""
from contextlib import closing
from pathlib import Path
import sqlite3
import sys
import tempfile
import unittest
from unittest.mock import patch

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "scripts"))
import graph_snapshot_replay as replay


class SnapshotTests(unittest.TestCase):
    def test_copy_preserves_source_and_rows(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            source = root / "source.sqlite3"
            with closing(sqlite3.connect(source)) as db, db:
                db.execute("CREATE TABLE fixture (value TEXT)")
                db.execute("INSERT INTO fixture VALUES ('fixture')")
            output = root / "output"
            output.mkdir()
            before = replay.file_set_hashes(source)
            copied, hashes = replay.copy_snapshot(source, output)
            self.assertEqual(hashes, before)
            self.assertEqual(replay.file_set_hashes(source), before)
            with closing(sqlite3.connect(copied)) as db:
                self.assertEqual(db.execute("SELECT value FROM fixture").fetchall(),
                                 [("fixture",)])

    def test_wal_commits_survive_without_opening_source(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            source = root / "source.sqlite3"
            db = sqlite3.connect(source)
            try:
                db.execute("PRAGMA journal_mode=WAL")
                db.execute("CREATE TABLE fixture (value INTEGER)")
                db.execute("INSERT INTO fixture VALUES (7)")
                db.commit()
                self.assertIsNotNone(replay.file_set_hashes(source)["-wal"])
                output = root / "output"
                output.mkdir()
                real_connect = sqlite3.connect
                def guarded_connect(path, *args, **kwargs):
                    self.assertNotEqual(Path(path).resolve(), source.resolve())
                    return real_connect(path, *args, **kwargs)
                before = replay.file_set_hashes(source)
                with patch.object(replay.sqlite3, "connect", side_effect=guarded_connect):
                    copied, _ = replay.copy_snapshot(source, output)
                self.assertEqual(before, replay.file_set_hashes(source))
                sealed = replay.file_set_hashes(copied)
                with closing(real_connect(copied.as_uri() + '?mode=ro', uri=True)) as copied_db:
                    self.assertEqual(copied_db.execute("SELECT value FROM fixture").fetchall(), [(7,)])
                self.assertEqual(sealed, replay.file_set_hashes(copied))
            finally:
                db.close()

    def test_changing_source_is_rejected_before_database_open(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            source = root / "source.sqlite3"
            source.write_bytes(b"synthetic-not-sqlite")
            output = root / "output"
            output.mkdir()
            real_copy = replay.shutil.copyfile
            def changing_copy(src, dst):
                result = real_copy(src, dst)
                source.write_bytes(b"changed")
                return result
            with patch.object(replay.shutil, "copyfile", side_effect=changing_copy), \
                 patch.object(replay.sqlite3, "connect") as connect:
                with self.assertRaisesRegex(ValueError, "changed while copying"):
                    replay.copy_snapshot(source, output)
                connect.assert_not_called()

    def test_missing_source_is_not_created(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            source = root / "missing.sqlite3"
            with self.assertRaisesRegex(ValueError, "absent"):
                replay.copy_snapshot(source, root)
            self.assertFalse(source.exists())


if __name__ == "__main__":
    unittest.main()
