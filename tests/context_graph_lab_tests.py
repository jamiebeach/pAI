import json
import sys
import tempfile
import unittest
from pathlib import Path
from types import SimpleNamespace
from unittest.mock import patch


sys.path.insert(0, str(Path(__file__).resolve().parent.parent / "scripts"))
import context_graph_lab as lab


def proposal(person="Casey", requirement="non-color-only encoding"):
    return {
        "schema_version": 1,
        "entities": [
            {
                "local_ref": "person", "kind": "person", "label": person,
                "aliases": [], "identity_action": "NEW", "existing_node_id": None,
            },
            {
                "local_ref": "need", "kind": "accessibility_need",
                "label": requirement, "aliases": [],
                "identity_action": "NEW", "existing_node_id": None,
            },
        ],
        "relationships": [
            {
                "subject_ref": "person", "predicate": "requires",
                "object_ref": "need", "relationship_action": "ASSERT",
            }
        ],
    }


def v12_proposal():
    return {
        "schema_version": 1,
        "entities": [
            {"local_ref": "person", "kind": "person", "label": "Casey", "aliases": [],
             "identity_action": "NEW", "existing_node_id": None},
            {"local_ref": "condition", "kind": "condition", "label": "Anemia", "aliases": [],
             "identity_action": "NEW", "existing_node_id": None},
        ],
        "relationships": [
            {"subject_ref": "person", "predicate": "has_condition",
             "object_ref": "condition", "relationship_action": "ASSERT"},
        ],
    }


def evidence_review_response(candidate, verdict="DIRECTLY_EVIDENCED"):
    review = {
        "schema_version": 1,
        "claim_reviews": [
            {"claim_ref": row["claim_ref"], "verdict": verdict,
             "evidence": "The sealed episode states this claim."}
            for row in lab.evidence_claims(candidate)
        ],
    }
    return {
        "choices": [{"message": {"role": "assistant", "content": None, "tool_calls": [{
            "id": "review", "type": "function", "function": {
                "name": lab.EVIDENCE_REVIEW_TOOL_NAME,
                "arguments": json.dumps(review),
            },
        }]}}],
        "usage": {"cost": 0},
    }


def args(**changes):
    values = {
        "provider": "mock", "model": None, "execute": False, "validate": False,
        "request_limit": None, "cost_ceiling_usd": None,
        "max_prompt_price": 0.20, "max_completion_price": 0.40,
        "max_output_tokens": 4096, "openrouter_zdr": "require",
        "openrouter_data_collection": "deny", "episode_order": "oldest",
        "episode_limit": 2, "mock_proposals": None,
        "ontology": None,
        "reasoning_policy": "default",
        "review_evidence": False, "review_output_tokens": 4096,
        "openrouter_provider_only": None,
        "evidence_policy": "all",
    }
    values.update(changes)
    return SimpleNamespace(**values)


def episode_event(event_id, episode_id, synopsis):
    return {
        "id": event_id,
        "type": "conversation-episode-sealed",
        "occurred_at": f"2026-01-0{event_id}T12:00:00Z",
        "payload": {
            "episode_id": episode_id, "synopsis": synopsis,
            "subjects": [], "entities": [], "retrieval_cues": [],
            "broader_categories": [], "unresolved_threads": [],
        },
    }


class ContextGraphLabTests(unittest.TestCase):
    def test_evidence_review_requires_exact_coverage_and_rejects_prior_graph_claim(self):
        candidate = v12_proposal()
        review = lab.extract_evidence_review(evidence_review_response(candidate))
        indexed = lab.validate_evidence_review(review, candidate)
        self.assertEqual(3, len(indexed))
        review["claim_reviews"][0]["verdict"] = "SUPPORTED_BY_PRIOR_GRAPH"
        with self.assertRaisesRegex(ValueError, "invalid or unsupported"):
            lab.validate_evidence_review(review, candidate)

    def test_prior_graph_verdict_requires_an_exact_verified_claim(self):
        candidate = v12_proposal()
        review = lab.extract_evidence_review(evidence_review_response(candidate))
        for row in review["claim_reviews"]:
            row["verdict"] = "SUPPORTED_BY_PRIOR_GRAPH"
            row["evidence"] = "The exact claim occurs in the verified neighborhood."
        prior = {
            "entities": [
                {"type": "person", "name": "Casey"},
                {"type": "condition", "name": "Anemia"},
            ],
            "facts": [{
                "subject_type": "person", "subject_name": "Casey",
                "predicate": "has_condition",
                "object_type": "condition", "object_name": "Anemia",
                "source_episode_id": "episode:prior",
            }],
        }
        self.assertEqual(3, len(lab.validate_evidence_review(review, candidate, prior)))
        prior["facts"][0]["predicate"] = "related_to"
        with self.assertRaisesRegex(ValueError, "invalid or unsupported"):
            lab.validate_evidence_review(review, candidate, prior)

    def test_prior_neighborhood_is_bounded_to_relevant_direct_facts(self):
        candidate = v12_proposal()
        direct = lab.extract_evidence_review(evidence_review_response(candidate))
        indexed = lab.validate_evidence_review(direct, candidate)
        formation = lab.normalize_formation(
            lab.episode_record(episode_event(1, "episode:prior", "Casey has anemia.")),
            candidate, "lab:prior", evidence_reviews=indexed,
        )
        inference = json.loads(json.dumps(formation))
        inference["episode"]["episode_id"] = "episode:inference"
        inference["proposal"]["facts"][0]["evidence_status"] = "inference"
        neighborhood = lab.prior_verified_neighborhood(
            [formation, inference], candidate, maximum_facts=1,
        )
        self.assertEqual(1, len(neighborhood["facts"]))
        self.assertEqual("episode:prior", neighborhood["facts"][0]["source_episode_id"])
        self.assertEqual(2, len(neighborhood["entities"]))

    def test_evidence_review_removes_unsupported_entity_and_incident_fact(self):
        candidate = v12_proposal()
        review = lab.extract_evidence_review(evidence_review_response(candidate))
        review["claim_reviews"][1]["verdict"] = "UNSUPPORTED"
        review["claim_reviews"][1]["evidence"] = "The condition is absent."
        indexed = lab.validate_evidence_review(review, candidate)
        entities, relationships, counts = lab.evidence_review_rejections(candidate, indexed)
        normalized = lab.normalize_formation(
            lab.episode_record(episode_event(1, "episode:1", "A sealed episode.")),
            candidate, "lab:1", relationships, entities,
        )
        self.assertEqual({"condition"}, entities)
        self.assertEqual(1, counts["UNSUPPORTED"])
        self.assertEqual(
            "evidence-review:rejected-entity-endpoint", relationships[0]["reason"],
        )
        self.assertEqual(["person"], [row["local_ref"] for row in normalized["proposal"]["entities"]])
        self.assertEqual([], normalized["proposal"]["facts"])

    def test_alias_is_reviewed_and_can_be_removed_without_losing_entity(self):
        candidate = v12_proposal()
        candidate["entities"][0]["aliases"] = ["Caspian"]
        review = lab.extract_evidence_review(evidence_review_response(candidate))
        alias = next(row for row in review["claim_reviews"]
                     if row["claim_ref"].startswith("alias:"))
        alias["verdict"] = "CONTRADICTED"
        alias["evidence"] = "The operator identifies this as inspiration, not an alias."
        indexed = lab.validate_evidence_review(review, candidate)
        filtered = lab.proposal_without_rejected_aliases(candidate, indexed)
        self.assertEqual([], filtered["entities"][0]["aliases"])
        self.assertEqual("Casey", filtered["entities"][0]["label"])
        self.assertEqual(2, len(filtered["entities"]))

    def test_classification_is_reviewed_and_retained_in_normalized_entity(self):
        candidate = v12_proposal()
        candidate["entities"][1]["classifications"] = ["health condition", "guess"]
        review = lab.extract_evidence_review(evidence_review_response(candidate))
        guess = next(row for row in review["claim_reviews"]
                     if row["claim_ref"] == "classification:condition:1")
        guess.update(verdict="UNSUPPORTED", evidence="The source does not state this trait.")
        indexed = lab.validate_evidence_review(review, candidate)
        filtered = lab.proposal_without_rejected_classifications(candidate, indexed)
        normalized = lab.normalize_formation(
            lab.episode_record(episode_event(1, "episode:1", "Casey has anemia.")),
            filtered, "lab:1", evidence_reviews=indexed,
        )
        condition = next(row for row in normalized["proposal"]["entities"]
                         if row["local_ref"] == "condition")
        self.assertEqual(["health condition"], condition["classifications"])

    def test_uniquely_reversed_typed_relationship_is_repaired_before_review(self):
        candidate = v12_proposal()
        candidate["relationships"][0].update(
            subject_ref="condition", object_ref="person",
        )
        root = Path(__file__).resolve().parent.parent
        selected = lab.load_selected_ontology(
            root / "config/context-graph-upper-ontology-v1.2.json",
        )
        repaired, repairs = lab.repair_reversed_typed_relationships(candidate, selected)
        self.assertEqual("person", repaired["relationships"][0]["subject_ref"])
        self.assertEqual("condition", repaired["relationships"][0]["object_ref"])
        self.assertEqual("unique-typed-direction", repairs[0]["reason"])

    def test_normalized_fact_retains_review_status_and_note(self):
        candidate = v12_proposal()
        review = lab.extract_evidence_review(
            evidence_review_response(candidate, "REASONABLE_INFERENCE")
        )
        indexed = lab.validate_evidence_review(review, candidate)
        normalized = lab.normalize_formation(
            lab.episode_record(episode_event(1, "episode:1", "Casey may have anemia.")),
            candidate, "lab:1", evidence_reviews=indexed,
        )
        fact = normalized["proposal"]["facts"][0]
        self.assertEqual("inference", fact["evidence_status"])
        self.assertEqual("The sealed episode states this claim.", fact["evidence_note"])

    def test_provider_formation_and_evidence_review_are_both_required(self):
        root = Path(__file__).resolve().parent.parent
        candidate = v12_proposal()
        provider_args = args(
            provider="openrouter", model="vendor/model", execute=True,
            request_limit=2, cost_ceiling_usd=0.1, episode_limit=1,
            ontology=root / "config" / "context-graph-upper-ontology-v1.2.json",
            review_evidence=True,
        )
        outcomes = [lab.proposal_response(candidate), evidence_review_response(candidate)]
        with tempfile.TemporaryDirectory() as directory, patch.dict(
            lab.os.environ, {"OPENROUTER_API_KEY": "fixture"}
        ), patch.object(lab, "openrouter_call", side_effect=outcomes):
            output = Path(directory)
            bundle, seal = lab.build_formation_bundle(
                [episode_event(1, "episode:1", "Casey has anemia.")], [],
                {"sha256": "fixture"}, provider_args, output,
            )
            self.assertEqual(1, len(bundle["formations"]))
            self.assertEqual(2, seal["request_attempts"])
            self.assertTrue((output / "evidence-review-validations.jsonl").is_file())

    def test_second_review_receives_only_verified_prior_neighborhood(self):
        root = Path(__file__).resolve().parent.parent
        candidate = v12_proposal()
        first_review = evidence_review_response(candidate)
        prior_review = evidence_review_response(candidate, "SUPPORTED_BY_PRIOR_GRAPH")
        outcomes = [
            lab.proposal_response(candidate), first_review,
            lab.proposal_response(candidate), prior_review,
        ]
        calls = []

        def provider(request, _key):
            calls.append(request)
            return outcomes.pop(0)

        provider_args = args(
            provider="openrouter", model="vendor/model", execute=True,
            request_limit=4, cost_ceiling_usd=0.1, episode_limit=2,
            ontology=root / "config" / "context-graph-upper-ontology-v1.2.json",
            review_evidence=True,
        )
        events = [
            episode_event(1, "episode:1", "Casey has anemia."),
            episode_event(2, "episode:2", "Casey has anemia again."),
        ]
        with tempfile.TemporaryDirectory() as directory, patch.dict(
            lab.os.environ, {"OPENROUTER_API_KEY": "fixture"}
        ), patch.object(lab, "openrouter_call", side_effect=provider):
            bundle, seal = lab.build_formation_bundle(
                events, [], {"sha256": "fixture"}, provider_args, Path(directory),
            )
        prior = json.loads(calls[3]["messages"][1]["content"])["verified_prior_graph"]
        self.assertEqual(1, len(prior["facts"]))
        self.assertEqual("has_condition", prior["facts"][0]["predicate"])
        second_fact = bundle["formations"][1]["proposal"]["facts"][0]
        self.assertEqual("prior-graph", second_fact["evidence_status"])
        self.assertEqual(4, seal["request_attempts"])

    def test_selected_v12_ontology_constrains_tool_and_typed_signatures(self):
        root = Path(__file__).resolve().parent.parent
        selected = lab.load_selected_ontology(
            root / "config" / "context-graph-upper-ontology-v1.2.json"
        )
        tool = lab.formation_tool(selected)["function"]["parameters"]
        entity_enum = tool["properties"]["entities"]["items"]["properties"]["kind"]["enum"]
        predicate_enum = tool["properties"]["relationships"]["items"]["properties"]["predicate"]["enum"]
        self.assertIn("organism", entity_enum)
        self.assertIn("classified_as", predicate_enum)
        valid = {
            "schema_version": 1,
            "entities": [
                {"local_ref": "p", "kind": "person", "label": "Casey", "aliases": [],
                 "identity_action": "NEW", "existing_node_id": None},
                {"local_ref": "c", "kind": "condition", "label": "Condition", "aliases": [],
                 "identity_action": "NEW", "existing_node_id": None},
            ],
            "relationships": [
                {"subject_ref": "p", "predicate": "has_condition", "object_ref": "c",
                 "relationship_action": "ASSERT"},
            ],
        }
        lab.validate_proposal(valid, selected)
        valid["relationships"][0]["subject_ref"] = "c"
        valid["relationships"][0]["object_ref"] = "p"
        with self.assertRaisesRegex(ValueError, "violates.*signature"):
            lab.validate_proposal(valid, selected)
        rejected = lab.validate_proposal(
            valid, selected, reject_signature_mismatches=False,
        )
        self.assertEqual("typed-signature-incompatible", rejected[0]["reason"])

    def test_normalization_omits_only_rejected_relationships(self):
        raw = proposal()
        rejected = [{
            "subject_ref": "person", "predicate": "requires", "object_ref": "need",
        }]
        normalized = lab.normalize_formation(
            lab.episode_record(episode_event(1, "episode:1", "A sealed episode.")),
            raw, "lab:1", rejected,
        )
        self.assertEqual(2, len(normalized["proposal"]["entities"]))
        self.assertEqual([], normalized["proposal"]["facts"])

    def test_bounded_relationship_repair_omits_bad_edges_without_rewriting_claims(self):
        root = Path(__file__).resolve().parent.parent
        selected = lab.load_selected_ontology(
            root / "config" / "context-graph-upper-ontology-v1.2.json"
        )
        candidate = v12_proposal()
        candidate["relationships"].extend([
            {"subject_ref": "person", "predicate": "invented_relation",
             "object_ref": "condition", "relationship_action": "ASSERT"},
            {"subject_ref": "missing", "predicate": "has_condition",
             "object_ref": "condition", "relationship_action": "ASSERT"},
            {"subject_ref": "person", "predicate": "has_condition",
             "object_ref": "condition", "relationship_action": "ASSERT"},
        ])
        rejected = lab.validate_proposal(
            candidate, selected, reject_signature_mismatches=False,
            reject_relationship_errors=False,
        )
        self.assertEqual(
            ["predicate-outside-selected-ontology", "unknown-entity-reference",
             "duplicate-relationship"],
            [row["reason"] for row in rejected],
        )
        repaired = lab.proposal_without_rejected_relationships(candidate, rejected)
        self.assertEqual([candidate["relationships"][0]], repaired["relationships"])
        self.assertIs(candidate["entities"], repaired["entities"])

    def test_bounded_relationship_repair_records_malformed_edge_by_index(self):
        candidate = v12_proposal()
        candidate["relationships"].append({"predicate": "has_condition"})
        rejected = lab.validate_proposal(
            candidate, reject_relationship_errors=False,
        )
        self.assertEqual("unknown-or-missing-relationship-keys", rejected[0]["reason"])
        repaired = lab.proposal_without_rejected_relationships(candidate, rejected)
        self.assertEqual(1, len(repaired["relationships"]))

    def test_selected_v12_rejects_invented_entity_kind(self):
        root = Path(__file__).resolve().parent.parent
        selected = lab.load_selected_ontology(
            root / "config" / "context-graph-upper-ontology-v1.2.json"
        )
        candidate = proposal()
        with self.assertRaisesRegex(ValueError, "outside the selected ontology"):
            lab.validate_proposal(candidate, selected)

    def test_valid_proposal_and_native_response_are_accepted(self):
        candidate = proposal()
        lab.validate_proposal(candidate)
        self.assertEqual(candidate, lab.extract_proposal(lab.proposal_response(candidate)))

    def test_unknown_proposal_key_fails_closed(self):
        candidate = proposal()
        candidate["explanation"] = "not part of authority"
        with self.assertRaisesRegex(ValueError, "top-level keys"):
            lab.validate_proposal(candidate)

    def test_invalid_or_missing_native_tool_call_fails_closed(self):
        response = {"choices": [{"message": {"role": "assistant", "content": "<tool_call>"}}]}
        with self.assertRaisesRegex(ValueError, "exactly one native tool call"):
            lab.extract_proposal(response)

    def test_cost_bound_uses_request_bytes_and_output_ceiling(self):
        request = lab.formation_request(
            lab.episode_record(episode_event(1, "episode:1", "A sealed episode.")),
            args(model="vendor/model"),
        )
        bound = lab.request_cost_bound(request, args(model="vendor/model"))
        self.assertGreater(bound, 4096 * 0.40 / 1_000_000)

    def test_optional_reasoning_can_be_excluded_for_formation(self):
        request = lab.formation_request(
            lab.episode_record(episode_event(1, "episode:1", "A sealed episode.")),
            args(model="vendor/model", reasoning_policy="off"),
        )
        self.assertEqual({"enabled": False, "exclude": True}, request["reasoning"])

    def test_formation_and_review_can_be_pinned_to_one_provider(self):
        candidate_args = args(
            model="vendor/model", openrouter_provider_only="DeepInfra",
        )
        episode = lab.episode_record(episode_event(1, "episode:1", "A sealed episode."))
        formation = lab.formation_request(episode, candidate_args)
        review = lab.evidence_review_request(episode, v12_proposal(), candidate_args)
        self.assertEqual(["DeepInfra"], formation["provider"]["only"])
        self.assertEqual(["DeepInfra"], review["provider"]["only"])
        self.assertIn("Every sealed_episode field is evidence", review["messages"][0]["content"])
        self.assertIn(
            "the verdict cannot be DIRECTLY_EVIDENCED",
            review["messages"][0]["content"],
        )
        self.assertIn("A stated desire, intention, or roadmap", review["messages"][0]["content"])

    def test_mock_formation_uses_distinct_real_shaped_episodes(self):
        events = [
            episode_event(1, "episode:1", "One requirement."),
            episode_event(2, "episode:2", "Another requirement."),
        ]
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            proposals = root / "proposals.json"
            proposals.write_text(
                json.dumps([proposal(), proposal("Morgan", "large text")]),
                encoding="utf-8",
            )
            bundle, seal = lab.build_formation_bundle(
                events, ["requirements"], {"sha256": "fixture"},
                args(mock_proposals=proposals), root,
            )
            self.assertEqual(2, len(bundle["formations"]))
            self.assertEqual(2, seal["accepted_count"])
            self.assertTrue((root / "formation-validations.jsonl").is_file())

    def test_one_invalid_proposal_does_not_block_a_later_episode(self):
        events = [
            episode_event(1, "episode:1", "One requirement."),
            episode_event(2, "episode:2", "Another requirement."),
        ]
        invalid = proposal()
        invalid["entities"][0]["identity_action"] = "LINK_EXISTING"
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            proposals = root / "proposals.json"
            proposals.write_text(json.dumps([invalid, proposal()]), encoding="utf-8")
            bundle, seal = lab.build_formation_bundle(
                events, [], {"sha256": "fixture"},
                args(mock_proposals=proposals), root,
            )
            self.assertEqual(1, len(bundle["formations"]))
            self.assertEqual(1, seal["accepted_count"])
            self.assertEqual(1, seal["failure_count"])
            self.assertEqual(0, seal["charged_cost_usd"])

    def test_invalid_shape_still_uses_valid_reported_provider_cost(self):
        events = [episode_event(1, "episode:1", "One requirement.")]
        response = lab.proposal_response(proposal())
        response["choices"][0]["message"]["tool_calls"][0]["function"]["arguments"] = json.dumps(
            {"schema_version": 1, "entities": "invalid"}
        )
        response["usage"]["cost"] = 0.00025
        provider_args = args(
            provider="openrouter", model="vendor/model", execute=True,
            request_limit=1, cost_ceiling_usd=0.01,
        )
        with tempfile.TemporaryDirectory() as directory, patch.dict(
            lab.os.environ, {"OPENROUTER_API_KEY": "fixture"}
        ), patch.object(lab, "openrouter_call", return_value=response):
            root = Path(directory)
            with self.assertRaisesRegex(SystemExit, "no provider formation"):
                lab.build_formation_bundle(
                    events, [], {"sha256": "fixture"}, provider_args, root,
                )
            validation = json.loads(
                (root / "formation-validations.jsonl").read_text().strip()
            )
            seal = json.loads((root / "formation-seal.json").read_text())
            self.assertEqual(0.00025, validation["charged_cost_usd"])
            self.assertEqual("reported", validation["accounting"])
            self.assertEqual(0.00025, seal["charged_cost_usd"])

    def test_definitive_http_rejection_costs_zero_and_later_episode_continues(self):
        events = [
            episode_event(1, "episode:1", "One requirement."),
            episode_event(2, "episode:2", "Another requirement."),
        ]
        outcomes = [
            lab.OpenRouterHTTPError(404, "no eligible route"),
            lab.proposal_response(proposal()),
        ]

        def call(_request, _key):
            outcome = outcomes.pop(0)
            if isinstance(outcome, Exception):
                raise outcome
            return outcome

        provider_args = args(
            provider="openrouter", model="vendor/model", execute=True,
            request_limit=2, cost_ceiling_usd=0.01,
        )
        with tempfile.TemporaryDirectory() as directory, patch.dict(
            lab.os.environ, {"OPENROUTER_API_KEY": "fixture"}
        ), patch.object(lab, "openrouter_call", side_effect=call):
            root = Path(directory)
            bundle, seal = lab.build_formation_bundle(
                events, [], {"sha256": "fixture"}, provider_args, root,
            )
            validations = [
                json.loads(line)
                for line in (root / "formation-validations.jsonl").read_text().splitlines()
            ]
            self.assertEqual("provider-rejected", validations[0]["status"])
            self.assertEqual(0, validations[0]["charged_cost_usd"])
            self.assertEqual(1, len(bundle["formations"]))
            self.assertEqual(1, seal["accepted_count"])

    def test_held_out_evaluation_checks_results_and_predicates(self):
        result = {
            "queries": [{
                "query": "presentation accessibility", "result_count": 1,
                "facts": [{"predicate": "requires", "fact": "Uses non-color encoding"}],
            }]
        }
        evaluated = lab.evaluate_result(
            result,
            [{
                "query": "presentation accessibility", "minimum_results": 1,
                "any_predicates": ["requires"], "any_terms": ["non-color"],
            }],
        )
        self.assertTrue(evaluated["passed"])

    def test_openrouter_validate_requires_explicit_bounded_configuration(self):
        candidate = SimpleNamespace(
            mode="form", formation_limit=256, episode_limit=2,
            provider="openrouter", mock_proposals=None, model="vendor/model",
            execute=False, validate=True, request_limit=2,
            cost_ceiling_usd=0.01, max_prompt_price=0.20,
            max_completion_price=0.40, max_output_tokens=4096,
            review_output_tokens=4096, review_evidence=False, ontology=None,
            openrouter_provider_only=None,
        )
        lab.validate_args(candidate)
        candidate.request_limit = 1
        with self.assertRaisesRegex(SystemExit, "cover every selected episode"):
            lab.validate_args(candidate)


if __name__ == "__main__":
    unittest.main()
