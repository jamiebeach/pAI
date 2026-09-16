import copy
import json
from pathlib import Path
import sys
from types import SimpleNamespace
import unittest

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "scripts"))
import context_graph_adjudication_lab as lab


class AdjudicationLabTests(unittest.TestCase):
    def setUp(self):
        self.packet = {"schema_version": 1, "episode_id": "fixture",
            "sources": [{"source_id": "source:fixture", "speaker_id": "operator",
                         "kind": "original-utterance", "text": "I own a blue notebook.",
                         "text_sha256": "fixture-sha"}]}
        self.candidate = {"entities": [
            {"claim_ref": "candidate:entity:p", "local_ref": "p", "type": "person",
             "name": "Operator", "classifications": []},
            {"claim_ref": "candidate:entity:n", "local_ref": "n", "type": "object",
             "name": "Blue notebook", "classifications": ["notebook"]}],
            "facts": [{"claim_ref": "candidate:fact:0", "subject_ref": "p",
                       "predicate": "owns", "object_ref": "n",
                       "fact": "Operator owns a blue notebook.", "grounding": {},
                       "temporal": {}}]}
        self.review = {"schema_version": 1, "verdict": "CANDIDATE_BETTER",
            "candidate_omissions": [], "unsupported_candidate_claims": [],
            "supported_improvements": [{"claim_ref": "candidate:fact:0",
                                         "reason": "Preserves the typed ownership fact."}],
            "summary": "The candidate is more precise and remains source-grounded."}
        self.repair = {"priority": "repair"}
        self.monitor = {"priority": "review"}

    def response(self, payload=None):
        payload = payload or self.review
        return {"choices": [{"finish_reason": "tool_calls", "message": {
            "tool_calls": [{"function": {
                "name": "adjudicate-context-graph-candidate",
                "arguments": json.dumps(payload)}}]}}]}

    def test_deterministic_repair_regression_overrides_model(self):
        decision, precheck = lab.deterministic_decision(
            self.candidate, self.candidate,
            [self.monitor], [self.repair, self.repair], self.review)
        self.assertEqual("KEEP_PRIOR", decision)
        self.assertTrue(precheck["repair_regression"])

    def test_accept_requires_strict_repair_improvement_and_clean_review(self):
        decision, _ = lab.deterministic_decision(
            {"entities": [], "facts": []}, self.candidate,
            [self.repair, self.monitor], [self.monitor], self.review)
        self.assertEqual("ACCEPT_CANDIDATE", decision)
        for key, value in (("candidate_omissions", [{"description": "Missing color",
                "source_id": "source:fixture", "quote": "blue"}]),
                           ("unsupported_candidate_claims", [{
                "claim_ref": "candidate:fact:0", "reason": "Unsupported"}])):
            blocked = copy.deepcopy(self.review)
            blocked[key] = value
            self.assertEqual("INDETERMINATE", lab.deterministic_decision(
                {"entities": [], "facts": []}, self.candidate,
                [self.repair], [], blocked)[0])

    def test_model_cannot_accept_without_deterministic_improvement(self):
        self.assertEqual("INDETERMINATE", lab.deterministic_decision(
            {"entities": [], "facts": []}, self.candidate,
            [self.monitor], [self.monitor], self.review)[0])
        prior = copy.deepcopy(self.review)
        prior["verdict"] = "PRIOR_BETTER"
        self.assertEqual("KEEP_PRIOR", lab.deterministic_decision(
            {"entities": [], "facts": []}, self.candidate,
            [self.repair], [], prior)[0])

    def test_exact_semantic_equivalence_can_retire_structural_debt(self):
        equivalent = copy.deepcopy(self.review)
        equivalent.update(verdict="EQUIVALENT", supported_improvements=[])
        reordered = {"entities": list(reversed(self.candidate["entities"])),
                     "facts": list(reversed(self.candidate["facts"]))}
        decision, precheck = lab.deterministic_decision(
            self.candidate, reordered, [self.repair], [], equivalent)
        self.assertEqual("ACCEPT_CANDIDATE", decision)
        self.assertTrue(precheck["semantically_equivalent"])

    def test_response_requires_exact_source_quote_and_candidate_claim(self):
        self.assertEqual(self.review, lab.validate_response(
            self.response(), self.packet, self.candidate))
        bad_quote = copy.deepcopy(self.review)
        bad_quote["candidate_omissions"] = [{"description": "Missing detail",
            "source_id": "source:fixture", "quote": "a green notebook"}]
        with self.assertRaises(ValueError):
            lab.validate_response(self.response(bad_quote), self.packet, self.candidate)
        bad_ref = copy.deepcopy(self.review)
        bad_ref["supported_improvements"][0]["claim_ref"] = "candidate:fact:99"
        with self.assertRaises(ValueError):
            lab.validate_response(self.response(bad_ref), self.packet, self.candidate)

    def test_request_excludes_hidden_expectations_and_retains_privacy_policy(self):
        case = {"case_id": "fixture", "expectations": {"secret": "hidden oracle"}}
        args = SimpleNamespace(model="openai/gpt-oss-120b", review_output_tokens=8192,
            max_prompt_price=.2, max_completion_price=.4,
            openrouter_provider_only="deepinfra", reasoning_policy="low")
        request = lab.adjudication_request(case, self.packet, self.candidate,
            self.candidate, [self.repair], [], args)
        encoded = json.dumps(request)
        self.assertNotIn("hidden oracle", encoded)
        self.assertIn("I own a blue notebook.", encoded)
        self.assertEqual(["deepinfra"], request["provider"]["only"])
        self.assertTrue(request["provider"]["zdr"])
        self.assertEqual("deny", request["provider"]["data_collection"])

    def test_manifest_is_bounded_and_oracle_is_local(self):
        corpus = {"cases": [{"case_id": "fixture"}]}
        manifest = {"schema_version": 1,
            "source_kind": "synthetic-controlled-adjudication",
            "cases": [{"case_id": "fixture", "prior_artifacts": "artifacts/context-graph-lab/prior",
                       "candidate_artifacts": "artifacts/context-graph-lab/candidate",
                       "expected_decision": "ACCEPT_CANDIDATE"}]}
        lab.validate_manifest(manifest, corpus)
        invalid = copy.deepcopy(manifest)
        invalid["cases"][0]["expected_decision"] = "MODEL_DECIDES"
        with self.assertRaises(ValueError):
            lab.validate_manifest(invalid, corpus)


if __name__ == "__main__":
    unittest.main()
