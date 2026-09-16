"""Bounded artifact reopening and loaded-code provenance; no model or database."""
import json
from pathlib import Path
import sys
import tempfile
import unittest
from unittest.mock import Mock, patch

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "scripts"))
from context_graph_episode_lab import CaseLibrary, LispWorker, save_run_capture


class SavedResults(unittest.TestCase):
    def setUp(self):
        self.directory = tempfile.TemporaryDirectory()
        self.addCleanup(self.directory.cleanup)
        self.library = CaseLibrary.__new__(CaseLibrary)
        self.library.root = Path(self.directory.name)
        self.library.expectations = {}
        (self.library.root / "runs").mkdir()

    def test_reopen_needs_no_worker_and_preserves_evidence(self):
        run_id = "a" * 24
        result = {"status": "complete", "code_fingerprint": {"fixture": "pinned"},
                  "contract": {"profile": "fixture"}, "provider_calls": 0}
        path = self.library.root / "runs" / (run_id + ".json")
        path.write_text(json.dumps(result), encoding="utf-8")
        before = path.read_bytes()
        reopened = self.library.saved_result(run_id)
        self.assertTrue(reopened["reopened_without_replay"])
        self.assertEqual(reopened["code_fingerprint"], result["code_fingerprint"])
        self.assertEqual(path.read_bytes(), before)

    def test_exact_id_only(self):
        for value in ("../secret", "A" * 24, "a" * 25, None, 123):
            with self.subTest(value=value), self.assertRaises(ValueError):
                self.library.saved_result(value)

    def test_replay_forwards_bounded_query_set(self):
        self.library.cases = {"fixture": ({"id": "fixture", "checkpoint": {}}, [{"id": 1}])}
        self.library.read = Mock(return_value={"contract": {}, "digest": "fixture"})
        worker = Mock(code_fingerprint={"fixture": "hash"})
        worker.call.return_value = {"status": "complete"}
        queries = [{"query": "fixture-pet", "evidence_policy": "inferred"}]
        self.library.run(worker, {"case_id": "fixture", "queries": queries})
        self.assertEqual(worker.call.call_args.args[0]["queries"], queries)
        worker.reset_mock()
        with self.assertRaises(ValueError):
            self.library.run(worker, {"case_id": "fixture", "queries": queries * 33})
        worker.call.assert_not_called()

    def test_replay_includes_hash_pinned_expectation_audit(self):
        self.library.cases = {"fixture": ({"id": "fixture", "checkpoint": {}}, [{"id": 1}])}
        self.library.expectations = {"fixture": {
            "exact_queries": [{"query": "Fixture child"}, {"query": "Absent fixture"}]}}
        self.library.read = Mock(return_value={"contract": {}, "digest": "fixture"})
        worker = Mock(code_fingerprint={"fixture": "hash"})
        worker.call.return_value = {"status": "complete"}
        result = self.library.run(worker, {"case_id": "fixture"})
        self.assertEqual(worker.call.call_args.args[0]["queries"], [
            {"exact_queries": ["Fixture child", "Absent fixture"]}])
        self.assertTrue(result["expectation_audit_included"])

    def test_saved_comparison_does_not_request_replay(self):
        left, right = "a" * 24, "b" * 24
        for run_id, fingerprint in ((left, "old"), (right, "new")):
            (self.library.root / "runs" / (run_id + ".json")).write_text(
                json.dumps({"code_fingerprint": {"fixture": fingerprint}}), encoding="utf-8")
        worker = Mock(code_fingerprint={"comparator": "current"})
        worker.call.return_value = {"status": "passed", "evidence_loss": []}
        result = self.library.compare_saved(worker, {"baseline_run_id": left,
                    "candidate_run_id": right, "expect_no_change": True})
        message = worker.call.call_args.args[0]
        self.assertEqual(message["operation"], "compare-results")
        self.assertEqual(message["expected_changes"], [])
        self.assertFalse(result["replayed"])
        self.assertEqual(result["baseline_code_fingerprint"], {"fixture": "old"})
        self.assertEqual(result["candidate_code_fingerprint"], {"fixture": "new"})
        self.assertEqual(self.library.saved_result(result["saved_result"])["comparison"]["status"], "passed")

    def test_changed_source_refuses_before_submission(self):
        import threading
        worker = LispWorker.__new__(LispWorker)
        worker.lock = threading.Lock()
        worker.code_fingerprint = {"fixture": "old"}
        worker.process = Mock()
        with patch("context_graph_episode_lab.case_code_fingerprint", return_value={"fixture": "new"}):
            with self.assertRaisesRegex(RuntimeError, "no replay submitted"):
                worker.call({"operation": "replay-case"})
        worker.process.stdin.write.assert_not_called()
        self.assertFalse(worker.lock.locked())

    def test_failed_run_is_persisted_without_replay(self):
        self.library.cases = {"fixture": ({"id": "fixture", "checkpoint": {}}, [{"id": 1}])}
        self.library.read = Mock(return_value={"contract": {"profile": "fixture"},
                                              "digest": "baseline"})
        worker = Mock(code_fingerprint={"fixture": "hash"})
        worker.call.side_effect = TimeoutError("bounded fixture timeout")
        run_id = "c" * 24
        with patch("context_graph_episode_lab.secrets.token_hex", return_value=run_id):
            with self.assertRaises(TimeoutError):
                self.library.run(worker, {"case_id": "fixture"})
        saved = self.library.saved_result(run_id)
        self.assertEqual(saved["status"], "incomplete")
        self.assertIn("bounded fixture timeout", saved["error"])
        self.assertEqual(saved["provider_calls"], 0)
        self.assertEqual(saved["historical_fold_count"], 0)
        self.assertTrue(saved["reopened_without_replay"])
        worker.call.assert_called_once()

    def test_oversized_run_is_refused_before_file_creation(self):
        path = self.library.root / "runs" / ("d" * 24 + ".json")
        with patch("context_graph_episode_lab.SAVED_RESULT_MAX_BYTES", 128):
            with self.assertRaisesRegex(ValueError, "write bound"):
                save_run_capture({"payload": "x" * 256}, path)
        self.assertFalse(path.exists())

    def test_oversized_existing_run_is_refused_without_reading(self):
        run_id = "e" * 24
        path = self.library.root / "runs" / (run_id + ".json")
        path.write_bytes(b"{}")
        with patch("context_graph_episode_lab.SAVED_RESULT_MAX_BYTES", 1):
            with patch.object(Path, "read_text", side_effect=AssertionError("must not read")):
                with self.assertRaisesRegex(ValueError, "read bound"):
                    self.library.saved_result(run_id)


if __name__ == "__main__":
    unittest.main()
