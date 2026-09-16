import copy
import json
from pathlib import Path
import sys
import tempfile
import time
from types import SimpleNamespace
import unittest
from unittest.mock import patch
sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "scripts"))
import context_graph_grounding_lab as grounding
import context_graph_resolution_lab as driver

def deadline_fixture_worker(connection, request, api_key):
    if request.get("stall"): time.sleep(10)
    connection.send((True, {"fixture": True}))
    connection.close()

class GroundingLabTests(unittest.TestCase):
    def setUp(self):
        self.args = SimpleNamespace(model="openai/gpt-oss-120b", max_output_tokens=8192,
            review_output_tokens=8192, max_prompt_price=.2, max_completion_price=.4,
            openrouter_data_collection="deny", openrouter_provider_only="deepinfra",
            openrouter_zdr="require", reasoning_policy="low", cost_ceiling_usd=.2, request_limit=25)
        self.case = {"case_id": "fixture", "timestamp": "2026-01-01T00:00:00Z",
            "sources": [{"source_id": "s", "speaker_id": "operator", "kind": "original-utterance",
                         "text": "I use a notebook."}], "expectations": {"secret-answer": "must not reach model"}}
        self.episode, self.packet = grounding.source_episode(self.case)
        self.proposal = {"schema_version": 1, "entities": [
            {"local_ref": "p", "kind": "person", "label": "Operator", "aliases": [], "classifications": [], "identity_action": "NEW", "existing_node_id": None},
            {"local_ref": "o", "kind": "object", "label": "Notebook", "aliases": [], "classifications": ["notebook"], "identity_action": "NEW", "existing_node_id": None}],
            "relationships": [{"subject_ref": "p", "object_ref": "o", "predicate": "uses", "relationship_action": "ASSERT",
                "fact": "Operator uses a notebook.", "grounding": {"schema_version": 1, "scope": "assertion", "polarity": "positive",
                    "attributed_to_ref": "p", "evidence": [{"source_id": "s", "quote": "I use a notebook."}]},
                "temporal": {"schema_version": 1, "character": "standing-disposition",
                    "occurred_at": None, "valid_from": None, "valid_until": None}}]}

    def test_expectations_never_in_provider_request(self):
        for request in (grounding.formation_request(self.episode, self.packet, self.args, None),
                        grounding.review_request(self.episode, self.packet, self.proposal, self.args)):
            encoded = json.dumps(request)
            self.assertNotIn("secret-answer", encoded)
            self.assertIn("I use a notebook.", encoded)
            self.assertEqual(request["provider"]["only"], ["deepinfra"])
            self.assertTrue(request["provider"]["zdr"])
            self.assertEqual(request["provider"]["data_collection"], "deny")

    def test_controlled_corpus_accepts_prior_agent_as_a_derived_source_kind(self):
        corpus = {"source_kind": "synthetic-controlled", "cases": [
            dict(self.case, sources=[dict(self.case["sources"][0],
                                          kind="prior-agent-utterance")]),
        ]}
        grounding.validate_corpus(corpus)

    def test_quote_and_scope_validation(self):
        self.assertEqual([], grounding.validate_proposal(self.proposal, self.packet, None))
        for key, value in (("scope", "invented"), ("attributed_to_ref", "missing"),
                           ("evidence", [{"source_id": "s", "quote": "Invented"}])):
            candidate = copy.deepcopy(self.proposal)
            candidate["relationships"][0]["grounding"][key] = value
            self.assertEqual([0], [row["relationship_index"] for row in
                                   grounding.validate_proposal(candidate, self.packet, None)])

    def test_joke_proposal_and_temporal_contract_are_explicit(self):
        request = grounding.formation_request(self.episode, self.packet, self.args, None)
        relation = request["tools"][0]["function"]["parameters"]["properties"]["relationships"]["items"]
        self.assertIn("joke", relation["properties"]["grounding"]["properties"]["scope"]["enum"])
        self.assertIn("proposal", relation["properties"]["grounding"]["properties"]["scope"]["enum"])
        self.assertIn("temporal", relation["required"])
        entity = request["tools"][0]["function"]["parameters"]["properties"]["entities"]["items"]
        self.assertIn("classifications", entity["required"])
        self.assertIn("every schema_version value remains exactly 1",
                      request["messages"][0]["content"])
        self.assertNotIn("grounding-v2", request["messages"][0]["content"])
        candidate = copy.deepcopy(self.proposal)
        candidate["relationships"][0]["temporal"]["character"] = "invented"
        self.assertEqual([0], [row["relationship_index"] for row in
                               grounding.validate_proposal(candidate, self.packet, None)])

    def test_repair_residuals_are_diagnostics_not_evidence(self):
        residual = {"schema_version": 1, "residual_key": "cgr:fixture",
            "status": "pending", "source_episode_id": "fixture",
            "kind": "relationship-rejected", "detail": {"reason": "fixture"},
            "priority": "repair", "source_evidence": [],
            "examined_revision": {}, "attempt_fingerprint": "fingerprint",
            "reconsider_when": ["formation-policy-revision"]}
        prior = {"proposal": {"entities": [], "facts": []}}
        request = grounding.formation_request(
            self.episode, self.packet, self.args, None, [residual], prior)
        prompt = request["messages"][0]["content"]
        payload = json.loads(request["messages"][1]["content"])
        self.assertIn("diagnostics, never evidence", prompt)
        self.assertIn("complete replacement formation", prompt)
        self.assertEqual([residual], payload["repair_residuals"])
        self.assertEqual(prior, payload["prior_formation"])
        self.assertEqual(grounding.RESIDUAL_GUIDANCE_REVISION,
                         payload["repair_revision"])
        self.assertNotIn("secret-answer", json.dumps(request))

    def test_review_distinguishes_scoped_proposition_from_embedded_truth(self):
        prompt = grounding.review_request(
            self.episode, self.packet, self.proposal, self.args)["messages"][0]["content"]
        self.assertIn("embedded proposition is true", prompt)
        self.assertIn("absent from the triple endpoints", prompt)
        self.assertIn("hypothesis scope, unknown polarity", prompt)

    def test_explicit_temporal_phrases_are_normalized_from_source_only(self):
        episode = dict(self.episode, occurred_at="2026-06-08T18:00:00Z")
        fact = copy.deepcopy(self.proposal["relationships"][0])
        fact["grounding"]["evidence"] = [{"source_id": "s", "quote":
            "I worked there until the end of April 2026. Since May 2026 I work here."}]
        temporal, changes = grounding.deterministic_temporal(episode, fact)
        self.assertEqual("2026-04-30", temporal["valid_until"])
        self.assertEqual("2026-05-01", temporal["valid_from"])
        self.assertEqual(2, len(changes))

        fact["grounding"]["evidence"][0]["quote"] = "Yesterday I visited for an hour."
        temporal, changes = grounding.deterministic_temporal(episode, fact)
        self.assertEqual("2026-06-07", temporal["occurred_at"])
        self.assertEqual("relative-day-from-sealed-episode", changes[0]["rule"])

    def test_common_lisp_universal_time_anchors_relative_day(self):
        episode = dict(self.episode, occurred_at="3997081010")
        fact = copy.deepcopy(self.proposal["relationships"][0])
        fact["grounding"]["evidence"][0]["quote"] = "Yesterday I used a notebook."
        temporal, changes = grounding.deterministic_temporal(episode, fact)
        self.assertEqual("2026-08-29", temporal["occurred_at"])
        self.assertEqual("relative-day-from-sealed-episode", changes[0]["rule"])

    def test_temporal_normalizer_removes_unsupported_model_value(self):
        fact = copy.deepcopy(self.proposal["relationships"][0])
        fact["temporal"]["occurred_at"] = "model-supplied"
        fact["grounding"]["evidence"][0]["quote"] = "I use a notebook."
        temporal, changes = grounding.deterministic_temporal(self.episode, fact)
        self.assertIsNone(temporal["occurred_at"])
        self.assertEqual("unsupported-model-time-removed", changes[0]["rule"])

    def test_source_relative_time_overrides_unsupported_model_value(self):
        fact = copy.deepcopy(self.proposal["relationships"][0])
        fact["temporal"]["occurred_at"] = "2026-12-31"
        fact["grounding"]["evidence"][0]["quote"] = "Yesterday I used a notebook."
        temporal, changes = grounding.deterministic_temporal(self.episode, fact)
        self.assertEqual("2025-12-31", temporal["occurred_at"])
        self.assertEqual({"relative-day-from-sealed-episode",
                          "unsupported-model-time-removed"},
                         {row["rule"] for row in changes})

    def test_temporal_normalizer_does_not_reverse_not_until(self):
        fact = copy.deepcopy(self.proposal["relationships"][0])
        fact["grounding"]["evidence"][0]["quote"] = "I will not start until May 2026."
        temporal, changes = grounding.deterministic_temporal(self.episode, fact)
        self.assertIsNone(temporal["valid_from"])
        self.assertIsNone(temporal["valid_until"])
        self.assertEqual([], changes)

    def test_one_typed_signature_error_is_locally_rejected(self):
        candidate = copy.deepcopy(self.proposal)
        candidate["relationships"].append(copy.deepcopy(candidate["relationships"][0]))
        candidate["relationships"][1].update(
            predicate="has_gap", fact="Operator has the notebook as a gap.")
        rejected = grounding.validate_proposal(candidate, self.packet, {
            "ontology": {
                "entity_types": [{"name": "person"}, {"name": "object"}],
                "predicates": [{"name": "uses", "subject_types": ["person"],
                                "object_types": ["object"]},
                               {"name": "has_gap", "subject_types": ["person"],
                                "object_types": ["gap"]}],
            }})
        self.assertEqual([1], [row["relationship_index"] for row in rejected])
        retained = driver.base.proposal_without_rejected_relationships(candidate, rejected)
        self.assertEqual(1, len(retained["relationships"]))

    def test_one_inexact_quote_is_locally_rejected(self):
        candidate = copy.deepcopy(self.proposal)
        candidate["relationships"][0]["grounding"]["evidence"][0]["quote"] = "I ... notebook."
        rejected = grounding.validate_proposal(candidate, self.packet, None)
        self.assertEqual([0], [row["relationship_index"] for row in rejected])
        self.assertEqual("quote-does-not-match-original-source", rejected[0]["reason"])

    def test_resolution_prompt_distinguishes_new_from_unresolved(self):
        request = driver.resolution_request(self.episode, self.proposal["entities"], [], self.args)
        prompt = request["messages"][0]["content"]
        self.assertIn("candidate set is empty", prompt)
        self.assertIn("source identity itself is ambiguous", prompt)

    def test_automatic_routing_survives_runner_and_all_request_consumers(self):
        with tempfile.TemporaryDirectory() as temp, patch("run_lisp_test.find_sbcl", return_value=Path("/fixture/sbcl")):
            root = Path(temp)
            corpus = root / "cases.json"
            corpus.write_text(json.dumps({"source_kind": "synthetic-controlled", "cases": [self.case]}), encoding="utf-8")
            args = copy.deepcopy(self.args)
            args.openrouter_provider_only = None
            args.grounding_cases, args.artifacts = corpus, root / "run"
            args.reuse_extractions, args.execute = None, False
            args.provider_timeout_seconds = 180
            with patch.dict(driver.os.environ):
                grounding.run(args, Path(__file__).resolve().parents[1])
            seal = json.loads((args.artifacts / "seal.json").read_text(encoding="utf-8"))
            self.assertIsNone(seal["provider"])
            self.assertEqual(grounding.GROUNDING_PROTOCOL_REVISION,
                             seal["protocol_revision"])
            for request in (grounding.formation_request(self.episode, self.packet, args, None),
                            grounding.review_request(self.episode, self.packet, self.proposal, args),
                            driver.resolution_request(self.episode, [], [], args)):
                policy = request["provider"]
                self.assertNotIn("only", policy)
                self.assertTrue(policy["zdr"])
                self.assertTrue(policy["require_parameters"])
                self.assertEqual(policy["data_collection"], "deny")
                self.assertEqual(policy["max_price"], {"prompt": .2, "completion": .4})
                self.assertEqual(request["model"], "openai/gpt-oss-120b")

    def test_scope_survives_actual_normalizer(self):
        reviews = {"entity:p": {"verdict": "DIRECTLY_EVIDENCED", "evidence": "Speaker"},
                   "entity:o": {"verdict": "DIRECTLY_EVIDENCED", "evidence": "Object"},
                   "relationship:0": {"verdict": "DIRECTLY_EVIDENCED", "evidence": "Exact statement"}}
        result, temporal_changes = grounding.normalize(
            self.episode, self.packet, self.proposal, reviews)
        self.assertEqual(result["proposal"]["facts"][0]["grounding"], self.proposal["relationships"][0]["grounding"])
        self.assertEqual(result["proposal"]["facts"][0]["temporal"], self.proposal["relationships"][0]["temporal"])
        self.assertEqual(result["source_packet"], self.packet)
        self.assertEqual([], temporal_changes)

    def test_reuse_requires_exact_request_and_response_receipts(self):
        request = grounding.formation_request(self.episode, self.packet, self.args, None)
        response = {"choices": [{"finish_reason": "stop"}]}
        with tempfile.TemporaryDirectory() as temp:
            root = Path(temp)
            (root / "calls.json").write_text(json.dumps([{
                "stage": "fixture-extract", "status": "received",
                "request_sha256": driver.base.canonical_sha256(request),
                "response_sha256": driver.base.canonical_sha256(response)}]), encoding="utf-8")
            (root / "response-01-fixture-extract.json").write_text(
                json.dumps(response), encoding="utf-8")
            reused, row = grounding.reuse_received_response(
                root, "fixture-extract", request)
            self.assertEqual(response, reused)
            self.assertEqual("received", row["status"])
            changed = copy.deepcopy(request)
            changed["messages"][0]["content"] += " changed"
            self.assertEqual((None, None), grounding.reuse_received_response(
                root, "fixture-extract", changed))

    def test_residuals_are_stable_and_revision_sensitive(self):
        reviews = {"entity:p": {"verdict": "DIRECTLY_EVIDENCED", "evidence": "Speaker"},
                   "entity:o": {"verdict": "DIRECTLY_EVIDENCED", "evidence": "Object"},
                   "relationship:0": {"verdict": "DIRECTLY_EVIDENCED", "evidence": "Statement"}}
        formation, _ = grounding.normalize(
            self.episode, self.packet, self.proposal, reviews)
        formation["proposal"]["facts"][0]["predicate"] = "related_to"
        formation["proposal"]["facts"][0]["temporal"]["character"] = "event"
        formation["proposal"]["entities"].append({
            "local_ref": "unused", "type": "concept", "name": "unused",
            "aliases": [], "action": "NEW", "existing_id": None,
            "evidence_status": "direct", "evidence_note": "fixture",
            "classifications": []})
        rejected = [{"relationship_index": 2, "reason": "fixture-rejection"}]
        decisions = {"missing": {"local_ref": "missing", "action": "UNRESOLVED",
                                 "existing_id": None, "reason": "ambiguous"}}
        first = grounding.formation_residuals(
            self.episode, self.packet, formation, rejected, decisions, "ontology-a")
        second = grounding.formation_residuals(
            self.episode, self.packet, formation, rejected, decisions, "ontology-a")
        revised = grounding.formation_residuals(
            self.episode, self.packet, formation, rejected, decisions, "ontology-b")
        self.assertEqual(first, second)
        self.assertEqual({row["kind"] for row in first}, {
            "relationship-rejected", "identity-unresolved", "event-time-unresolved",
            "predicate-specificity-unresolved", "entity-utility-unresolved"})
        self.assertEqual([row["residual_key"] for row in first],
                         [row["residual_key"] for row in revised])
        self.assertNotEqual([row["attempt_fingerprint"] for row in first],
                            [row["attempt_fingerprint"] for row in revised])
        self.assertEqual({row["priority"] for row in first}, {"repair", "review"})

    def test_offline_residual_replay_makes_no_provider_calls(self):
        reviews = {"entity:p": {"verdict": "DIRECTLY_EVIDENCED", "evidence": "Speaker"},
                   "entity:o": {"verdict": "DIRECTLY_EVIDENCED", "evidence": "Object"},
                   "relationship:0": {"verdict": "DIRECTLY_EVIDENCED", "evidence": "Statement"}}
        formation, _ = grounding.normalize(
            self.episode, self.packet, self.proposal, reviews)
        with tempfile.TemporaryDirectory() as temp:
            root = Path(temp)
            source, output = root / "source", root / "output"
            source.mkdir(); output.mkdir()
            (source / "outcomes.json").write_text(json.dumps([{
                "case_id": "fixture", "status": "applied",
                "structural_relationship_rejections": []}]), encoding="utf-8")
            (source / "formations.json").write_text(
                json.dumps([formation]), encoding="utf-8")
            (source / "fixture-decisions.json").write_text(json.dumps({
                "p": {"local_ref": "p", "action": "NEW", "existing_id": None,
                      "reason": "new"},
                "o": {"local_ref": "o", "action": "NEW", "existing_id": None,
                      "reason": "new"}}), encoding="utf-8")
            corpus = {"source_kind": "synthetic-controlled", "cases": [self.case]}
            args = SimpleNamespace(residuals_from=[source])
            with patch.object(driver.base, "openrouter_call") as provider:
                self.assertEqual(0, grounding.replay_residuals(
                    args, Path(__file__).resolve().parents[1], corpus, output))
            provider.assert_not_called()
            report = json.loads((output / "report.json").read_text(encoding="utf-8"))
            self.assertEqual(0, report["provider_calls"])
            self.assertEqual(0, report["live_writes"])
            self.assertEqual({}, report["by_priority"])

    def test_residual_collection_is_bounded(self):
        reviews = {"entity:p": {"verdict": "DIRECTLY_EVIDENCED", "evidence": "Speaker"},
                   "entity:o": {"verdict": "DIRECTLY_EVIDENCED", "evidence": "Object"},
                   "relationship:0": {"verdict": "DIRECTLY_EVIDENCED", "evidence": "Statement"}}
        formation, _ = grounding.normalize(
            self.episode, self.packet, self.proposal, reviews)
        seed = formation["proposal"]["facts"][0]
        formation["proposal"]["facts"] = [dict(seed, predicate="related_to",
            fact=f"bounded fixture {index}") for index in range(193)]
        with self.assertRaises(ValueError):
            grounding.formation_residuals(
                self.episode, self.packet, formation, ontology_sha256="ontology")

    def test_residual_comparison_excludes_deferred_cases(self):
        def row(key, case, priority):
            return {"residual_key": key, "source_episode_id": case,
                    "priority": priority}
        previous = {
            "applied": [row("same", "applied", "repair"),
                        row("gone", "applied", "review")],
            "deferred": [row("not-cleared", "deferred", "repair")]}
        result = grounding.compare_residuals(
            previous, [row("same", "applied", "repair"),
                       row("new", "applied", "review")], ["applied"])
        self.assertEqual(["applied"], result["compared_case_ids"])
        self.assertEqual(["gone"], result["cleared_keys"])
        self.assertNotIn("not-cleared", result["cleared_keys"])
        self.assertEqual(1, result["by_priority"]["repair"]["previous_count"])

    def test_offline_renormalization_removes_unsupported_time_without_provider(self):
        proposal = copy.deepcopy(self.proposal)
        proposal["relationships"][0]["temporal"]["occurred_at"] = self.case["timestamp"]
        reviews = {
            "entity:p": {"claim_ref": "entity:p", "verdict": "DIRECTLY_EVIDENCED",
                         "evidence": "speaker"},
            "entity:o": {"claim_ref": "entity:o", "verdict": "DIRECTLY_EVIDENCED",
                         "evidence": "object"},
            "classification:o:0": {"claim_ref": "classification:o:0",
                                     "verdict": "DIRECTLY_EVIDENCED",
                                     "evidence": "notebook classification"},
            "relationship:0": {"claim_ref": "relationship:0",
                               "verdict": "DIRECTLY_EVIDENCED",
                               "evidence": "statement"}}
        with tempfile.TemporaryDirectory() as temp:
            root = Path(temp)
            source, output = root / "source", root / "output"
            source.mkdir(); output.mkdir()
            (source / "outcomes.json").write_text(json.dumps([{
                "case_id": "fixture", "status": "applied",
                "structural_relationship_rejections": []}]), encoding="utf-8")
            (source / "fixture-proposal.json").write_text(
                json.dumps(proposal), encoding="utf-8")
            (source / "fixture-reviews.json").write_text(
                json.dumps(reviews), encoding="utf-8")
            args = SimpleNamespace(renormalize_from=[source])
            corpus = {"source_kind": "synthetic-controlled", "queries": [],
                      "cases": [self.case]}
            graph = {"graph": {"entity_count": 2, "fact_count": 1}}
            with patch.object(driver, "run_graph", return_value=graph), \
                    patch.object(driver.base, "openrouter_call") as provider:
                self.assertEqual(0, grounding.replay_normalized_formations(
                    args, Path(__file__).resolve().parents[1], corpus, output))
            provider.assert_not_called()
            formation = json.loads(
                (output / "formations.json").read_text(encoding="utf-8"))[0]
            self.assertIsNone(
                formation["proposal"]["facts"][0]["temporal"]["occurred_at"])
            outcome = json.loads(
                (output / "outcomes.json").read_text(encoding="utf-8"))[0]
            self.assertEqual("unsupported-model-time-removed",
                             outcome["temporal_normalizations"][0]["changes"][0]["rule"])

    def test_configured_attempt_ceiling_is_exact(self):
        request = {"max_tokens": 8192}
        with tempfile.TemporaryDirectory() as directory, patch.object(driver.base, "openrouter_call", return_value={}) as transport, patch.dict(driver.os.environ, {"OPENROUTER_API_KEY": "fixture"}):
            calls = driver.Calls(Path(directory), self.args)
            for index in range(self.args.request_limit):
                calls.call(str(index), request)
            with self.assertRaises(ValueError): calls.call("over-limit", request)
            self.assertEqual(transport.call_count, self.args.request_limit)

    def test_overbound_cost_poisoned_and_not_erased(self):
        with tempfile.TemporaryDirectory() as directory, patch.object(driver.base, "openrouter_call", return_value={"usage": {"cost": .3}}) as transport, patch.dict(driver.os.environ, {"OPENROUTER_API_KEY": "fixture"}):
            calls = driver.Calls(Path(directory), self.args)
            with self.assertRaises(ValueError): calls.call("one", {"max_tokens": 8192})
            with self.assertRaises(ValueError): calls.call("two", {"max_tokens": 8192})
            self.assertEqual(transport.call_count, 1)
            self.assertEqual(calls.rows[0]["charged_usd"], .3)

    def test_same_stage_cannot_retry_and_prior_calls_count(self):
        with tempfile.TemporaryDirectory() as directory, patch.object(driver.base, "openrouter_call") as transport:
            calls = driver.Calls(Path(directory), self.args, [{"stage": "failed-extract", "bound_usd": .19}])
            with self.assertRaises(ValueError): calls.call("failed-extract", {"max_tokens": 8192})
            self.args.request_limit = 1
            with self.assertRaises(ValueError): calls.call("untouched-extract", {"max_tokens": 8192})
            transport.assert_not_called()

    def test_provider_worker_deadline_without_network(self):
        with patch.object(driver, "_provider_worker", deadline_fixture_worker):
            with self.assertRaises(TimeoutError):
                driver.provider_call_with_deadline({"stall": True}, "fixture", .1)
            self.assertEqual(driver.provider_call_with_deadline({}, "fixture", 5), {"fixture": True})

    def test_transport_failure_stops_following_cases(self):
        with tempfile.TemporaryDirectory() as directory, patch.object(driver.base, "openrouter_call", side_effect=RuntimeError("fixture overload")) as transport, patch.dict(driver.os.environ, {"OPENROUTER_API_KEY": "fixture"}):
            calls = driver.Calls(Path(directory), self.args)
            with self.assertRaises(RuntimeError): calls.call("one", {"max_tokens": 8192})
            with self.assertRaises(ValueError): calls.call("two", {"max_tokens": 8192})
            continued = driver.Calls(Path(directory), self.args, calls.rows)
            with self.assertRaises(ValueError): continued.call("three", {"max_tokens": 8192})
            self.assertEqual(transport.call_count, 1)

    def test_case_ids_cannot_escape_artifact_directory_or_repeat(self):
        corpus = {"source_kind": "synthetic-controlled", "cases": [self.case]}
        grounding.validate_corpus(corpus)
        for cases in ([dict(self.case, case_id="../escape")], [self.case, self.case]):
            with self.assertRaises(ValueError): grounding.validate_corpus(dict(corpus, cases=cases))

if __name__ == "__main__": unittest.main()
