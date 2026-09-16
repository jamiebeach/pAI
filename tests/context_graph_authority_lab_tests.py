import copy
import json
from pathlib import Path
import sys
import tempfile
from types import SimpleNamespace
import unittest
from unittest.mock import patch

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "scripts"))
import context_graph_authority_lab as lab
import context_graph_resolution_lab as driver


class AuthorityLabTests(unittest.TestCase):
    def test_quality_comparison_requires_explicit_execution_before_network(self):
        import context_graph_simple_quality_lab as quality
        argv = ["quality", "--prior", "missing.json", "--corpus", "missing.json", "--experiment", "test"]
        with patch.object(sys, "argv", argv), patch.object(quality.urllib.request, "urlopen") as network:
            with patch("sys.stderr"), self.assertRaises(SystemExit) as stopped:
                quality.main()
            self.assertEqual(stopped.exception.code, 2)
            network.assert_not_called()

    def setUp(self):
        self.corpus = {"source_kind": "synthetic-controlled", "cases": [{"case_id": "one", "timestamp": 100,
            "sources": [{"source_id": "one", "speaker_id": "operator", "kind": "original-utterance", "text": "I own a cat."}]}],
            "queries": [{"query": "cat", "expected_count": 1, "expected_object_label": "Mira"}]}
        self.args = SimpleNamespace(model="vendor/model", max_output_tokens=100, max_prompt_price=.2,
            max_completion_price=.4, openrouter_zdr="require", openrouter_data_collection="deny",
            openrouter_provider_only=None, reasoning_policy="low", cost_ceiling_usd=.01, request_limit=2,
            transient_retries=0)
        self.spec = {"adapter_revision": "kg-authority-lab-model-v1", "tool_name": "review-knowledge-graph-evidence",
                     "schema": {"type": "object"}, "system": "Lisp-owned instructions.", "input": {"exact_source": "Evidence"}}

    def test_synthetic_corpus_and_held_out_contract(self):
        lab.validate_corpus(self.corpus)
        self.corpus["source_kind"] = "private-export"
        with self.assertRaises(ValueError): lab.validate_corpus(self.corpus)

    def test_speaker_binding_cannot_be_inferred_from_model_fields(self):
        self.corpus["cases"][0]["sources"][0]["speaker_id"] = "unknown"
        with self.assertRaises(ValueError): lab.validate_corpus(self.corpus)

    def test_boolean_timestamp_not_an_event_time(self):
        self.corpus["cases"][0]["timestamp"] = True
        with self.assertRaises(ValueError): lab.validate_corpus(self.corpus)

    def test_transport_preserves_lisp_schema_and_input_without_expectations(self):
        before = copy.deepcopy(self.spec)
        request = lab.request_from_spec(self.spec, self.args)
        self.assertEqual(self.spec, before)
        self.assertEqual(request["tools"][0]["function"]["parameters"], self.spec["schema"])
        self.assertEqual(json.loads(request["messages"][1]["content"]), self.spec["input"])
        self.assertNotIn("expected_count", json.dumps(request))

    def test_privacy_is_required_and_pin_disallows_fallback(self):
        self.args.openrouter_provider_only = "Provider"
        request = lab.request_from_spec(self.spec, self.args)
        self.assertTrue(request["provider"]["zdr"])
        self.assertFalse(request["provider"]["allow_fallbacks"])
        self.args.openrouter_zdr = "allow-non-zdr"
        with self.assertRaises(ValueError): lab.request_from_spec(self.spec, self.args)

    def test_existing_ledger_denies_over_budget_before_transport(self):
        with tempfile.TemporaryDirectory() as directory, patch.object(driver.base, "openrouter_call") as transport:
            self.args.cost_ceiling_usd = .0000001
            calls = driver.Calls(Path(directory), self.args)
            with self.assertRaises(driver.BudgetDeferred): calls.call("extraction", lab.request_from_spec(self.spec, self.args))
            transport.assert_not_called()
            self.assertEqual(calls.rows, [])

    def test_native_response_must_have_correct_tool_and_not_be_truncated(self):
        response = {"choices": [{"finish_reason": "tool_calls", "message": {"tool_calls": [
            {"function": {"name": self.spec["tool_name"], "arguments": "{\"schema_version\":2}"}}]}}]}
        self.assertEqual(lab.response_payload(response, self.spec["tool_name"]), {"schema_version": 2})
        response["choices"][0]["finish_reason"] = "length"
        with self.assertRaises(ValueError): lab.response_payload(response, self.spec["tool_name"])
        response["choices"][0]["finish_reason"] = "stop"
        response["choices"][0]["message"]["refusal"] = "Refused"
        with self.assertRaises(ValueError): lab.response_payload(response, self.spec["tool_name"])
        with self.assertRaises(ValueError): lab.response_payload({"choices": [{"finish_reason": "stop", "message": {"content": "{}"}}]}, self.spec["tool_name"])

    def test_exact_duplicate_native_calls_collapse_but_competing_calls_fail(self):
        function = {"name": self.spec["tool_name"],
                    "arguments": "{\"schema_version\":2}"}
        response = {"choices": [{"finish_reason": "tool_calls", "message": {
            "tool_calls": [
                {"type": "function", "id": "first", "function": function},
                {"type": "function", "id": "second", "function": copy.deepcopy(function)},
            ]}}]}
        self.assertEqual(lab.response_payload(response, self.spec["tool_name"]),
                         {"schema_version": 2})
        response["choices"][0]["message"]["tool_calls"][1]["function"] = {
            "name": self.spec["tool_name"], "arguments": "{\"schema_version\":3}"}
        with self.assertRaisesRegex(ValueError, "distinct competing calls"):
            lab.response_payload(response, self.spec["tool_name"])

    def test_retrieval_reports_enduring_ids_and_requires_complete_scan(self):
        actual = [{"rows": [{"fact_id": "fact", "object": {"entity_id": "enduring", "label": "Mira"}}], "scan_complete": True}]
        self.assertTrue(lab.evaluate(self.corpus["queries"], actual)["passed"])
        self.assertEqual(lab.evaluate(self.corpus["queries"], actual)["queries"][0]["object_entity_ids"], ["enduring"])
        actual[0]["scan_complete"] = False
        self.assertFalse(lab.evaluate(self.corpus["queries"], actual)["passed"])
        self.assertFalse(lab.evaluate(self.corpus["queries"], [])["passed"])

    def test_default_run_seals_without_constructing_a_provider_ledger(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            corpus_path = root / "corpus.json"
            corpus_path.write_text(json.dumps(self.corpus), encoding="utf-8")
            args = copy.copy(self.args)
            args.authority_cases, args.artifacts = corpus_path, root / "artifacts"
            args.authority_recorded_responses, args.execute = None, False
            args.authority_simple = True
            args.protocol_retries, args.provider_timeout_seconds = 0, 30
            for stage in ("extraction", "review", "resolution"):
                setattr(args, f"{stage}_model", None)
                setattr(args, f"{stage}_provider_only", None)
            with patch.object(driver, "Calls") as calls, patch.object(driver, "run_lisp_bundle", return_value={"status": "accepted", "value": self.spec}):
                self.assertEqual(lab.run(args, Path(__file__).resolve().parents[1]), 0)
                calls.assert_not_called()
            self.assertEqual(len(list(args.artifacts.glob("*/seal.json"))), 1)
            bundle = json.loads(next(args.artifacts.glob("*/episode-01-extraction-input/bundle.json")).read_text())
            self.assertEqual(bundle["authority_operation"], "simple-model-session")

    def test_staged_dry_run_starts_with_entities_without_network(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            corpus_path = root / "corpus.json"
            corpus_path.write_text(json.dumps(self.corpus), encoding="utf-8")
            args = copy.copy(self.args)
            args.authority_cases, args.artifacts = corpus_path, root / "artifacts"
            args.authority_recorded_responses, args.execute = None, False
            args.authority_staged = True
            args.protocol_retries, args.provider_timeout_seconds = 0, 30
            for stage in ("extraction", "review", "resolution"):
                setattr(args, f"{stage}_model", None)
                setattr(args, f"{stage}_provider_only", None)
            with patch.object(driver, "Calls") as calls, patch.object(driver, "run_lisp_bundle", return_value={"status": "accepted", "value": self.spec}):
                self.assertEqual(lab.run(args, Path(__file__).resolve().parents[1]), 0)
                calls.assert_not_called()
            bundle = json.loads(next(args.artifacts.glob("*/episode-01-entities-input/bundle.json")).read_text())
            self.assertEqual(bundle["authority_operation"], "staged-model-session")
            self.assertIsNone(bundle["step"]["entity_selection"])
            self.assertIsNone(bundle["step"]["fact_request_digest"])

    def test_missing_recorded_response_never_falls_back_to_provider(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            corpus_path, responses = root / "corpus.json", root / "responses.json"
            corpus_path.write_text(json.dumps(self.corpus), encoding="utf-8")
            responses.write_text("{}", encoding="utf-8")
            args = copy.copy(self.args)
            args.authority_cases, args.artifacts = corpus_path, root / "artifacts"
            args.authority_recorded_responses, args.execute = responses, False
            args.protocol_retries, args.provider_timeout_seconds = 0, 30
            for stage in ("extraction", "review", "resolution"):
                setattr(args, f"{stage}_model", None)
                setattr(args, f"{stage}_provider_only", None)
            with patch.object(driver, "Calls") as calls, patch.object(driver, "run_lisp_bundle", return_value={"status": "accepted", "value": self.spec}):
                with self.assertRaisesRegex(ValueError, "missing recorded response"):
                    lab.run(args, Path(__file__).resolve().parents[1])
                calls.assert_not_called()
            report = json.loads(next(args.artifacts.glob("*/report.json")).read_text())
            self.assertEqual(report["provider_calls"], 0)


if __name__ == "__main__": unittest.main()
