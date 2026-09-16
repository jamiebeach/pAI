"""Local-only persona import and selection contract for the canonical CLI."""

from __future__ import annotations

import json
from pathlib import Path
import subprocess
import sys
import tempfile
import unittest
from unittest.mock import patch


REPO = Path(__file__).resolve().parent.parent
sys.path.insert(0, str(REPO / "scripts"))

from pai_cli import (  # noqa: E402
    import_persona,
    normalize_persona_document,
    require_ignored_storage,
    validate_persona_name,
)


class PaiCliPersonaTests(unittest.TestCase):
    def test_production_prompt_shape_imports_only_current_identity_and_voice(self) -> None:
        normalized = normalize_persona_document(
            "fixture",
            {
                "schema_version": 1,
                "current": {
                    "revision": 7,
                    "identity": "Private fixture identity",
                    "voice": "Private fixture voice",
                    "actor": "must-not-copy",
                },
                "history": [{"identity": "old private value"}],
            },
        )
        self.assertEqual(
            set(normalized),
            {"schema_version", "persona_id", "revision", "identity", "voice"},
        )
        self.assertEqual(normalized["persona_id"], "fixture")
        self.assertEqual(normalized["revision"], 7)

    def test_invalid_persona_name_and_empty_profile_fail_closed(self) -> None:
        with self.assertRaises(SystemExit):
            validate_persona_name("../tracked")
        with self.assertRaises(SystemExit):
            normalize_persona_document(
                "fixture",
                {"schema_version": 1, "identity": "", "voice": "voice"},
            )

    def test_import_writes_only_below_selected_local_state(self) -> None:
        with tempfile.TemporaryDirectory(prefix="pai-persona-source-") as source_dir:
            with tempfile.TemporaryDirectory(prefix="pai-persona-state-") as state_dir:
                source = Path(source_dir) / "source.json"
                source.write_text(
                    json.dumps(
                        {
                            "schema_version": 1,
                            "identity": "Local identity fixture",
                            "voice": "Local voice fixture",
                            "revision": 2,
                        }
                    ),
                    encoding="utf-8",
                )
                target = import_persona(REPO, Path(state_dir), "fixture", source)
                self.assertEqual(target.parent, Path(state_dir) / "personas")
                self.assertEqual(json.loads(target.read_text(encoding="utf-8"))["persona_id"],
                                 "fixture")

    def test_repository_clone_persona_directory_is_ignored_and_untracked(self) -> None:
        if not (REPO / ".git").is_dir():
            self.skipTest("deployed source volume intentionally has no Git metadata")
        probe = REPO / ".clone-state" / "personas" / "never-commit.json"
        ignored = subprocess.run(
            ["git", "check-ignore", "--quiet", str(probe)], cwd=REPO, check=False
        )
        tracked = subprocess.run(
            ["git", "ls-files", str(REPO / ".clone-state" / "personas")],
            cwd=REPO,
            check=True,
            capture_output=True,
            text=True,
        )
        self.assertEqual(ignored.returncode, 0)
        self.assertEqual(tracked.stdout.strip(), "")

    def test_private_state_check_distinguishes_git_failure_from_not_ignored(self) -> None:
        target = REPO / ".private-state-fixture"
        with patch(
            "pai_cli.subprocess.run",
            return_value=subprocess.CompletedProcess(
                args=[], returncode=128,
                stderr="fatal: detected dubious ownership in repository\n",
            ),
        ):
            with self.assertRaisesRegex(SystemExit, "could not verify.*dubious"):
                require_ignored_storage(REPO, target)
        with patch(
            "pai_cli.subprocess.run",
            return_value=subprocess.CompletedProcess(
                args=[], returncode=1, stderr="",
            ),
        ):
            with self.assertRaisesRegex(SystemExit, "non-ignored"):
                require_ignored_storage(REPO, target)


if __name__ == "__main__":
    unittest.main()
