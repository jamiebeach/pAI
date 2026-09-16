import json
import sys
import tempfile
import unittest
from pathlib import Path
from types import SimpleNamespace


sys.path.insert(0, str(Path(__file__).resolve().parent.parent / "scripts"))
import context_graph_evidence_calibration as calibration


class ContextGraphEvidenceCalibrationTests(unittest.TestCase):
    def fixture(self):
        return {
            "schema_version": 1,
            "minimum_accuracy": 0.75,
            "minimum_direct_precision": 1.0,
            "minimum_direct_recall": 1.0,
            "expectations": [
                {"episode_id": "episode:1", "claim_ref": "entity:a",
                 "claim": "A is named.", "expected_verdict": "DIRECTLY_EVIDENCED"},
                {"episode_id": "episode:1", "claim_ref": "relationship:0",
                 "claim": "A relation is implied.", "expected_verdict": "REASONABLE_INFERENCE"},
            ],
        }

    def test_evaluation_exposes_direct_evidence_false_positive(self):
        result = calibration.evaluate(self.fixture(), {
            ("episode:1", "entity:a"): "DIRECTLY_EVIDENCED",
            ("episode:1", "relationship:0"): "DIRECTLY_EVIDENCED",
        })
        self.assertEqual("fail", result["status"])
        self.assertEqual("fail", result["verified_boundary_status"])
        self.assertEqual(0.5, result["accuracy"])
        self.assertEqual(0.5, result["direct_precision"])
        self.assertEqual(1.0, result["direct_recall"])

    def test_missing_expected_claim_fails_accuracy(self):
        result = calibration.evaluate(self.fixture(), {
            ("episode:1", "entity:a"): "DIRECTLY_EVIDENCED",
        })
        self.assertEqual("fail", result["status"])
        self.assertIsNone(result["checks"][1]["actual_verdict"])

    def test_verified_boundary_can_pass_while_exact_inference_labels_fail(self):
        result = calibration.evaluate(self.fixture(), {
            ("episode:1", "entity:a"): "DIRECTLY_EVIDENCED",
            ("episode:1", "relationship:0"): "UNSUPPORTED",
        })
        self.assertEqual("fail", result["exact_verdict_status"])
        self.assertEqual("pass", result["verified_boundary_status"])

    def test_fixture_validation_fails_closed(self):
        fixture = self.fixture()
        fixture["expectations"][0]["unexpected"] = True
        with tempfile.TemporaryDirectory() as directory:
            path = Path(directory) / "fixture.json"
            path.write_text(json.dumps(fixture), encoding="utf-8")
            with self.assertRaisesRegex(ValueError, "unknown or missing keys"):
                calibration.load_fixture(path)

    def test_saved_request_regeneration_uses_current_prompt_and_fixed_cases(self):
        fixture = self.fixture()
        payload = {
            "episode_id": "episode:1",
            "sealed_episode": {"synopsis": "A is named and a relation is implied."},
            "claims": [
                {"claim_ref": "entity:a", "kind": "entity",
                 "claim": {"type": "person", "label": "A"}},
                {"claim_ref": "entity:b", "kind": "entity",
                 "claim": {"type": "concept", "label": "B"}},
                {"claim_ref": "relationship:0", "kind": "relationship",
                 "claim": {"subject_ref": "a", "predicate": "related_to", "object_ref": "b"}},
            ],
            "verified_prior_graph": {"entities": [], "facts": []},
        }
        rows = [{"episode_id": "episode:1", "request": {
            "model": "old/model",
            "messages": [{"role": "system", "content": "old"},
                         {"role": "user", "content": json.dumps(payload)}],
            "max_tokens": 100,
        }}]
        args = SimpleNamespace(
            model="openai/gpt-oss-120b", review_output_tokens=4096,
            openrouter_data_collection="deny", openrouter_zdr="allow-non-zdr",
            max_prompt_price=0.05, max_completion_price=0.20,
            openrouter_provider_only="DeepInfra", reasoning_policy="low",
        )
        regenerated = calibration.regenerate_requests(rows, fixture, args)
        request = regenerated[0]["request"]
        self.assertEqual("openai/gpt-oss-120b", request["model"])
        self.assertIn("cannot be DIRECTLY_EVIDENCED", request["messages"][0]["content"])
        self.assertEqual(["DeepInfra"], request["provider"]["only"])
        self.assertEqual(3, len(calibration.lab.evidence_claims(regenerated[0]["proposal"])))


if __name__ == "__main__":
    unittest.main()
