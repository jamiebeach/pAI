import importlib.util
import json
from pathlib import Path
import tempfile
import unittest

ROOT = Path(__file__).parents[1]
SPEC = importlib.util.spec_from_file_location(
    "qualification_contract", ROOT / "scripts" / "qualification_contract.py")
MODULE = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(MODULE)


class QualificationContractTests(unittest.TestCase):
    def fixture(self):
        return {
            "schema_version": 1,
            "default_lisp_profile": "offline",
            "profiles": {
                "offline": {"state": "required"},
                "database": {"state": "blocked", "reason": "fixture pending"},
            },
            "lisp_suite_overrides": {"database-tests.lisp": "database"},
            "python_profiles": {"host": {"state": "required"}},
        }

    def run_fixture(self, contract=None, ready=False):
        temporary = tempfile.TemporaryDirectory()
        root = Path(temporary.name); (root / "tests").mkdir()
        for name in ["ordinary-tests.lisp", "database-tests.lisp"]:
            (root / "tests" / name).write_text("; synthetic\n")
        report = MODULE.validate(root, contract or self.fixture(), ready)
        return temporary, report

    def test_complete_contract_reports_every_suite(self):
        temporary, report = self.run_fixture()
        with temporary:
            self.assertTrue(report["passed"])
            self.assertEqual(report["lisp_suites_discovered"], 2)
            self.assertEqual(report["default_profile_suites"], 1)

    def test_release_gate_fails_while_profiles_are_blocked(self):
        temporary, report = self.run_fixture(ready=True)
        with temporary:
            self.assertFalse(report["passed"])
            self.assertIn("blocked-profiles-remain", report["errors"])

    def test_unknown_suite_and_profile_fail_validation(self):
        contract = self.fixture()
        contract["lisp_suite_overrides"] = {"missing-tests.lisp": "missing-profile"}
        temporary, report = self.run_fixture(contract)
        with temporary:
            self.assertFalse(report["passed"])
            self.assertEqual(report["errors"],
                             ["override-profile-missing", "override-suite-missing"])

    def test_blocked_profile_requires_a_reason(self):
        contract = self.fixture(); del contract["profiles"]["database"]["reason"]
        temporary, report = self.run_fixture(contract)
        with temporary:
            self.assertIn("blocked-profile-reason-missing", report["errors"])

    def test_repository_contract_is_structurally_valid(self):
        contract = json.loads((ROOT / "tests" / "qualification-contract.json").read_text())
        report = MODULE.validate(ROOT, contract)
        self.assertTrue(report["passed"], report)
        self.assertEqual(report["lisp_suites_discovered"], 243)


if __name__ == "__main__":
    unittest.main()
