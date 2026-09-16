import copy
from contextlib import closing
import json
from pathlib import Path
import sqlite3
import sys
import tempfile
from types import SimpleNamespace
import unittest
from unittest.mock import patch

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "scripts"))
import context_graph_resolution_lab as lab


class ResolutionTests(unittest.TestCase):
    def setUp(self):
        self.entities = [{"local_ref": "p", "type": "person", "name": "Operator"}]
        self.sets = [{"local_ref": "p", "candidates": [
            {"entity_id": "id:one", "type": "person", "name": "Operator"}]}]
        self.row = {"local_ref": "p", "action": "LINK_EXISTING",
                    "existing_id": "id:one", "reason": "Same explicitly identified speaker."}

    def response(self, rows):
        return {"choices": [{"finish_reason": "tool_calls", "message": {"tool_calls": [
            {"function": {"name": "resolve-entities", "arguments": json.dumps({"decisions": rows})}}]}}]}

    def test_exact_supplied_target(self):
        result = lab.validate_resolution(self.response([self.row]), self.entities, self.sets)
        self.assertEqual(result["p"]["existing_id"], "id:one")

    def test_strict_schema_resolution_payload(self):
        response = {"choices": [{"finish_reason": "stop", "message": {
            "content": json.dumps({"decisions": [self.row]}),
        }}]}
        result = lab.validate_resolution(response, self.entities, self.sets)
        self.assertEqual("id:one", result["p"]["existing_id"])

    def test_tool_contract_translates_to_strict_json_schema(self):
        request = lab.resolution_request(
            {"episode_id": "fixture", "content": "{}"}, self.entities, self.sets,
            SimpleNamespace(
                model="vendor/model", max_output_tokens=100,
                max_prompt_price=0.2, max_completion_price=0.4,
                openrouter_provider_only=None, reasoning_policy="off",
                openrouter_zdr="allow-non-zdr",
                openrouter_data_collection="deny",
            ),
        )
        structured = lab.strict_json_request(request)
        self.assertNotIn("tools", structured)
        self.assertNotIn("tool_choice", structured)
        contract = structured["response_format"]["json_schema"]
        self.assertTrue(contract["strict"])
        self.assertEqual(1, contract["schema"]["properties"]["decisions"]["minItems"])

    def test_unknown_target(self):
        self.row["existing_id"] = "id:invented"
        with self.assertRaises(ValueError):
            lab.validate_resolution(self.response([self.row]), self.entities, self.sets)

    def test_type_mismatch(self):
        self.sets[0]["candidates"][0]["type"] = "place"
        with self.assertRaises(ValueError):
            lab.validate_resolution(self.response([self.row]), self.entities, self.sets)

    def test_missing_and_duplicate(self):
        for rows in ([], [self.row, self.row]):
            with self.assertRaises(ValueError):
                lab.validate_resolution(self.response(rows), self.entities, self.sets)

    def test_unresolved_drops_incident_facts_but_not_input(self):
        formation = {"proposal": {"entities": copy.deepcopy(self.entities), "facts": [
            {"subject_ref": "p", "object_ref": "x"}]}}
        self.row.update(action="UNRESOLVED", existing_id=None)
        output = lab.apply_decisions(formation, {"p": self.row})
        self.assertEqual(output["proposal"], {"entities": [], "facts": []})
        self.assertEqual(len(formation["proposal"]["entities"]), 1)

    def test_length_finish_not_parsed_as_complete(self):
        response = self.response([self.row])
        response["choices"][0]["finish_reason"] = "length"
        with self.assertRaises(ValueError):
            lab.validate_resolution(response, self.entities, self.sets)

    def test_new_cannot_smuggle_target(self):
        self.row["action"] = "NEW"
        with self.assertRaises(ValueError):
            lab.validate_resolution(self.response([self.row]), self.entities, self.sets)

    def test_runtime_participants_translate_to_production_descriptors(self):
        result = lab.participant_descriptors({
            "operator": {"name": "FixtureOperator", "aliases": ["operator"]},
            "active-persona": {"name": "FixtureAgent", "aliases": []},
        })
        self.assertEqual(["operator", "active-persona"],
                         [row["role"] for row in result])
        self.assertEqual(["person", "agent"], [row["kind"] for row in result])
        self.assertEqual(["FixtureOperator", "FixtureAgent"], [row["label"] for row in result])

    def test_invalid_participant_descriptor_fails_before_lisp(self):
        with self.assertRaises(ValueError):
            lab.participant_descriptors({
                "operator": {"name": "", "aliases": []},
            })

    def test_exact_source_packet_hashes_private_evidence(self):
        episode = {"content": json.dumps({"source_evidence": [{
            "source_id": "event:1", "speaker_id": "operator",
            "kind": "original-utterance", "text": "A fact.",
        }]})}
        packet = lab.exact_source_packet(episode)
        self.assertEqual(1, packet["schema_version"])
        self.assertEqual(
            lab.hashlib.sha256(b"A fact.").hexdigest(),
            packet["sources"][0]["text_sha256"],
        )

    def test_review_reuse_retains_received_protocol_retry_receipts(self):
        rows = [
            {"stage": "episode-01-extract", "status": "received"},
            {"stage": "episode-01-review", "status": "received"},
            {"stage": "episode-01-review-protocol-retry-1", "status": "received"},
            {"stage": "episode-01-resolve", "status": "received"},
        ]
        selected = lab.reusable_extraction_review_rows(rows, 1, 1)
        self.assertEqual([
            "episode-01-extract", "episode-01-review",
            "episode-01-review-protocol-retry-1",
        ], [row["stage"] for row in selected])

    def test_review_reuse_requires_complete_base_receipts(self):
        with self.assertRaisesRegex(ValueError, "incomplete or invalid"):
            lab.reusable_extraction_review_rows([
                {"stage": "episode-01-extract", "status": "received"},
                {"stage": "episode-01-review-protocol-retry-1",
                 "status": "received"},
            ], 1, 1)

    def test_explicit_grounded_user_memory_can_supplement_one_episode(self):
        utterance = {"id": 10, "type": "user-message", "timestamp": "t1",
                     "payload": {"text": "Current question."}}
        memory = {"id": 20, "type": "memory-baseline-node", "payload": {"node": {
            "scalar_json": json.dumps({"content": "Original health statement.",
                "created_at": "t0", "grounding_status": "grounded",
                "origin_class": "lived-user",
                "epistemic_metadata": {"role": "user"}})}}}
        sealed = {"id": 30, "payload": {"episode_id": "episode:fixture",
            "persona_id": "fixtureagent", "source_event_ids": [10]}}
        with tempfile.TemporaryDirectory() as directory:
            database = Path(directory) / "events.sqlite3"
            with closing(sqlite3.connect(database)) as connection:
                connection.execute("CREATE TABLE pai_events (storage_sequence INTEGER, event_json TEXT)")
                connection.executemany("INSERT INTO pai_events VALUES (?, ?)", [
                    (1, json.dumps(utterance)), (2, json.dumps(memory))])
                connection.commit()
            episodes = lab.exact_utterance_episodes(database, [sealed], {"30": [20]})
        sources = json.loads(episodes[0]["content"])["source_evidence"]
        self.assertEqual(["event:10", "memory-event:20"],
                         [row["source_id"] for row in sources])
        self.assertEqual("original-utterance", sources[1]["kind"])

    def test_ungrounded_supplemental_memory_is_rejected(self):
        memory = {"id": 20, "type": "memory-baseline-node", "payload": {"node": {
            "scalar_json": json.dumps({"content": "A claim.", "created_at": "t0",
                "grounding_status": "ungrounded", "origin_class": "lived-user",
                "epistemic_metadata": {"role": "user"}})}}}
        sealed = {"id": 30, "payload": {"episode_id": "episode:fixture",
            "persona_id": "fixtureagent", "source_event_ids": []}}
        with tempfile.TemporaryDirectory() as directory:
            database = Path(directory) / "events.sqlite3"
            with closing(sqlite3.connect(database)) as connection:
                connection.execute("CREATE TABLE pai_events (storage_sequence INTEGER, event_json TEXT)")
                connection.execute("INSERT INTO pai_events VALUES (?, ?)",
                                   (1, json.dumps(memory)))
                connection.commit()
            with self.assertRaisesRegex(ValueError, "grounded lived evidence"):
                lab.exact_utterance_episodes(database, [sealed], {"30": [20]})

    def test_multiple_choices_rejected(self):
        response = self.response([self.row])
        response["choices"] *= 2
        with self.assertRaises(ValueError):
            lab.validate_resolution(response, self.entities, self.sets)

    def test_protocol_retry_uses_a_fresh_receipt_stage(self):
        class Calls:
            def __init__(self, responses):
                self.responses, self.stages = list(responses), []

            def call(self, stage, request):
                self.stages.append(stage)
                return self.responses.pop(0)

        calls = Calls([{"invalid": True}, self.response([self.row])])
        with patch.object(lab.time, "sleep") as sleep:
            result = lab.validated_native_call(
                calls, "episode-01-resolve", {},
                lambda response: lab.validate_resolution(
                    response, self.entities, self.sets,
                ), 1,
            )
        self.assertEqual("id:one", result["p"]["existing_id"])
        self.assertEqual(
            ["episode-01-resolve", "episode-01-resolve-protocol-retry-1"],
            calls.stages,
        )
        sleep.assert_called_once_with(1)

    def test_budget_blocks_before_network(self):
        args = SimpleNamespace(max_output_tokens=100, max_prompt_price=0.2,
                               max_completion_price=0.4, cost_ceiling_usd=0.00000001)
        with tempfile.TemporaryDirectory() as directory, patch.object(lab.base, "openrouter_call") as call:
            calls = lab.Calls(Path(directory), args)
            with self.assertRaises(ValueError):
                calls.call("test", {"max_tokens": 100})
            call.assert_not_called()

    def test_receipt_and_missing_usage_charge(self):
        args = SimpleNamespace(max_output_tokens=100, max_prompt_price=0.2,
                               max_completion_price=0.4, cost_ceiling_usd=0.2)
        with tempfile.TemporaryDirectory() as directory, patch.object(lab.base, "openrouter_call", return_value={}), patch.dict(lab.os.environ, {"OPENROUTER_API_KEY": "fixture"}):
            calls = lab.Calls(Path(directory), args)
            calls.call("test", {"max_tokens": 100})
            self.assertEqual(calls.rows[0]["charged_usd"], calls.reserved)
            self.assertTrue((Path(directory) / "request-01-test.json").exists())
            self.assertTrue((Path(directory) / "response-01-test.json").exists())

    def test_transport_failure_retained_without_retry(self):
        args = SimpleNamespace(max_output_tokens=100, max_prompt_price=0.2,
                               max_completion_price=0.4, cost_ceiling_usd=0.2)
        with tempfile.TemporaryDirectory() as directory, patch.object(lab.base, "openrouter_call", side_effect=RuntimeError("fixture failure")) as call, patch.dict(lab.os.environ, {"OPENROUTER_API_KEY": "fixture"}):
            calls = lab.Calls(Path(directory), args)
            with self.assertRaises(RuntimeError):
                calls.call("test", {"max_tokens": 100})
            self.assertEqual(call.call_count, 1)
            self.assertEqual(calls.rows[0]["status"], "failed")
            self.assertEqual(calls.rows[0]["charged_usd"], calls.reserved)

    def test_continuation_keeps_previous_cost_reservation(self):
        args = SimpleNamespace(max_output_tokens=100, max_prompt_price=0.2,
                               max_completion_price=0.4, cost_ceiling_usd=0.2)
        with tempfile.TemporaryDirectory() as directory, patch.object(lab.base, "openrouter_call") as call:
            calls = lab.Calls(Path(directory), args, [{"bound_usd": 0.2}])
            with self.assertRaises(ValueError):
                calls.call("test", {"max_tokens": 100})
            call.assert_not_called()

    def test_invalid_relationship_isolated_without_rewriting_valid_fact(self):
        proposal = {"schema_version": 1, "entities": [
            {"local_ref": "p", "kind": "person", "label": "Operator", "aliases": [],
             "identity_action": "NEW", "existing_node_id": None},
            {"local_ref": "o", "kind": "object", "label": "Item", "aliases": [],
             "identity_action": "NEW", "existing_node_id": None}],
            "relationships": [
                {"subject_ref": "p", "predicate": "uses", "object_ref": "o", "relationship_action": "ASSERT"},
                {"subject_ref": "p", "predicate": "uses", "object_ref": "p", "relationship_action": "ASSERT"}]}
        rejected = lab.base.validate_proposal(proposal, reject_signature_mismatches=False,
                                              reject_relationship_errors=False)
        clean = lab.base.proposal_without_rejected_relationships(proposal, rejected)
        self.assertEqual(len(rejected), 1)
        self.assertEqual(clean["relationships"], proposal["relationships"][:1])
        self.assertEqual(len(proposal["relationships"]), 2)
        lab.base.validate_proposal(clean)

    def test_stage_request_policy_is_explicit_and_falls_back(self):
        args = SimpleNamespace(model="default/model", openrouter_provider_only=None,
            extraction_model="extract/model", extraction_provider_only="DeepInfra",
            review_model="review/model", review_provider_only="CoreWeave",
            resolution_model=None, resolution_provider_only=None)
        self.assertEqual(lab.stage_policy(args), {
            "extraction": {"model": "extract/model", "provider_only": "DeepInfra"},
            "review": {"model": "review/model", "provider_only": "CoreWeave"},
            "resolution": {"model": "default/model", "provider_only": None}})

    def retry_args(self, **extra):
        return SimpleNamespace(**dict(dict(max_output_tokens=100, max_prompt_price=.2,
            max_completion_price=.4, cost_ceiling_usd=.2, transient_retries=2,
            request_limit=25), **extra))

    def test_transient_attempts_keep_receipts_and_reservations(self):
        with tempfile.TemporaryDirectory() as temp, patch.object(lab.base, "openrouter_call", side_effect=[
                lab.base.OpenRouterHTTPError(429, "busy"),
                {"usage": {"cost": 0}}]) as transport, patch.object(lab.time, "sleep") as sleep, \
                patch.object(lab.random, "uniform", return_value=0), \
                patch.dict(lab.os.environ, {"OPENROUTER_API_KEY": "fixture"}):
            calls = lab.Calls(Path(temp), self.retry_args())
            calls.call("review", {"max_tokens": 100})
            self.assertEqual(transport.call_count, 2)
            self.assertEqual(transport.call_args_list[0], transport.call_args_list[1])
            self.assertEqual(calls.reserved, sum(r["bound_usd"] for r in calls.rows))
            self.assertEqual([r["status"] for r in calls.rows], ["transient-failed", "received"])
            sleep.assert_called_once_with(10)
            self.assertTrue((Path(temp) / "error-01-review.json").exists())

    def test_exhaustion_defers_without_poisoning_other_stages(self):
        with tempfile.TemporaryDirectory() as temp, patch.object(lab.base, "openrouter_call", side_effect=[
                lab.base.OpenRouterHTTPError(503, "busy")]*3 + [{}]), \
                patch.object(lab.time, "sleep"), patch.object(lab.random, "uniform", return_value=0), \
                patch.dict(lab.os.environ, {"OPENROUTER_API_KEY": "fixture"}):
            calls = lab.Calls(Path(temp), self.retry_args())
            with self.assertRaises(lab.ProviderDeferred): calls.call("review", {"max_tokens": 100})
            self.assertFalse(calls.poisoned)
            calls.call("other", {"max_tokens": 100})
            self.assertEqual(len(calls.rows), 4)

    def test_permanent_http_error_is_not_retried(self):
        with tempfile.TemporaryDirectory() as temp, patch.object(lab.base, "openrouter_call", side_effect=
                lab.base.OpenRouterHTTPError(400, "schema")) as transport, \
                patch.dict(lab.os.environ, {"OPENROUTER_API_KEY": "fixture"}):
            with self.assertRaises(lab.base.OpenRouterHTTPError):
                lab.Calls(Path(temp), self.retry_args()).call("extract", {"max_tokens": 100})
            self.assertEqual(transport.call_count, 1)

    def test_retry_after_long_delay_defers_instead_of_sleeping(self):
        with tempfile.TemporaryDirectory() as temp, patch.object(lab.base, "openrouter_call", side_effect=
                lab.base.OpenRouterHTTPError(429, "busy", retry_after="120")) as transport, \
                patch.object(lab.time, "sleep") as sleep, \
                patch.dict(lab.os.environ, {"OPENROUTER_API_KEY": "fixture"}):
            calls = lab.Calls(Path(temp), self.retry_args())
            with self.assertRaises(lab.ProviderDeferred): calls.call("review", {"max_tokens": 100})
            self.assertEqual(transport.call_count, 1)
            sleep.assert_not_called()
            self.assertGreater(calls.rows[-1]["not_before"], lab.time.time())

    def test_retry_budget_checked_before_second_dispatch(self):
        request = {"max_tokens": 100}
        args = self.retry_args()
        args.cost_ceiling_usd = lab.base.request_cost_bound(request, args) * 1.5
        with tempfile.TemporaryDirectory() as temp, patch.object(lab.base, "openrouter_call", side_effect=
                lab.base.OpenRouterHTTPError(429, "busy")) as transport, \
                patch.object(lab.time, "sleep"), patch.dict(lab.os.environ, {"OPENROUTER_API_KEY": "fixture"}):
            calls = lab.Calls(Path(temp), args)
            with self.assertRaises(ValueError): calls.call("review", request)
            self.assertEqual(transport.call_count, 1)

    def test_resume_replays_success_without_new_network_or_cost(self):
        with tempfile.TemporaryDirectory() as temp, patch.object(lab.base, "openrouter_call", return_value={"ok": True}) as transport, \
                patch.dict(lab.os.environ, {"OPENROUTER_API_KEY": "fixture"}):
            root = Path(temp)
            (root / "old").mkdir(); (root / "new").mkdir()
            old = lab.Calls(root / "old", self.retry_args())
            request = {"max_tokens": 100}
            old.call("review", request)
            resumed = lab.Calls(root / "new", self.retry_args(), old.rows, root / "old")
            self.assertEqual(resumed.call("review", request), {"ok": True})
            self.assertEqual(transport.call_count, 1)
            self.assertEqual(resumed.reserved, old.reserved)
            blocked = lab.Calls(root / "new", self.retry_args(), old.rows + [
                {"stage": "later", "bound_usd": .01, "status": "admitted"}], root / "old")
            self.assertEqual(blocked.call("review", request), {"ok": True})
            with self.assertRaises(ValueError): blocked.call("new", request)
            with self.assertRaises(ValueError): resumed.call("review", dict(request, changed=True))
            receipt = Path(old.rows[-1]["response_path"])
            receipt.write_text('{"changed":true}', encoding="utf-8")
            with self.assertRaises(ValueError): resumed.call("review", request)

    def test_resume_deferred_waits_until_not_before_and_keeps_old_budget(self):
        with tempfile.TemporaryDirectory() as temp, patch.object(lab.base, "openrouter_call", side_effect=[
                lab.base.OpenRouterHTTPError(429, "busy", "120"), {}]) as transport, \
                patch.dict(lab.os.environ, {"OPENROUTER_API_KEY": "fixture"}):
            root = Path(temp)
            (root / "old").mkdir(); (root / "new").mkdir()
            old = lab.Calls(root / "old", self.retry_args())
            request = {"max_tokens": 100}
            with self.assertRaises(lab.ProviderDeferred): old.call("review", request)
            resumed = lab.Calls(root / "new", self.retry_args(), old.rows, root / "old")
            with self.assertRaises(lab.ProviderDeferred): resumed.call("review", request)
            self.assertEqual(transport.call_count, 1)
            with patch.object(lab.time, "time", return_value=old.rows[-1]["not_before"] + 1):
                resumed.call("review", request)
            self.assertEqual(transport.call_count, 2)
            self.assertEqual(len(resumed.rows), 2)
            self.assertEqual(resumed.reserved, old.reserved * 2)

    def test_http_date_retry_after(self):
        with patch.object(lab.time, "time", return_value=0):
            self.assertEqual(lab.retry_delay(lab.base.OpenRouterHTTPError(429, "busy", "Thu, 01 Jan 1970 00:00:40 GMT"), 0), 40)

    def test_unfinished_admission_cannot_be_replayed_or_retried(self):
        with tempfile.TemporaryDirectory() as temp, patch.object(lab.base, "openrouter_call") as transport:
            calls = lab.Calls(Path(temp), self.retry_args(), [{"stage": "review", "bound_usd": .01,
                "status": "admitted"}], Path(temp))
            with self.assertRaises(ValueError): calls.call("review", {"max_tokens": 100})
            transport.assert_not_called()

    def test_worker_preserves_structured_http_failure(self):
        class Connection:
            def send(self, value): self.value = value
            def close(self): self.closed = True
        connection = Connection()
        with patch.object(lab.base, "openrouter_call", side_effect=lab.base.OpenRouterHTTPError(429, "busy", "15")):
            lab._provider_worker(connection, {}, "fixture")
        self.assertEqual(connection.value, (False, {"http_status": 429, "detail": "busy", "retry_after": "15"}))
        self.assertTrue(connection.closed)

    def test_received_invalid_model_shape_is_not_regenerated(self):
        response = {"choices": [{"message": {"content": None}, "finish_reason": "stop"}]}
        with tempfile.TemporaryDirectory() as temp, patch.object(lab.base, "openrouter_call", return_value=response) as transport, \
                patch.dict(lab.os.environ, {"OPENROUTER_API_KEY": "fixture"}):
            calls = lab.Calls(Path(temp), self.retry_args())
            with self.assertRaises(ValueError): lab.base.extract_proposal(calls.call("extract", {"max_tokens": 100}))
            with self.assertRaises(ValueError): calls.call("extract", {"max_tokens": 100})
            self.assertEqual(transport.call_count, 1)

    def test_main_does_not_resolve_an_empty_reviewed_entity_set(self):
        def reply(name, payload):
            return {"choices": [{"finish_reason": "tool_calls", "message": {"tool_calls": [
                {"function": {"name": name, "arguments": json.dumps(payload)}}]}}]}
        proposal = {"schema_version": 1, "entities": [{"local_ref": "p", "kind": "person",
            "label": "Operator", "aliases": [], "classifications": [],
            "identity_action": "NEW", "existing_node_id": None}], "relationships": []}
        review = {"schema_version": 1, "claim_reviews": [{"claim_ref": "entity:p", "verdict": "UNSUPPORTED", "evidence": "Not supported."}]}
        episode = {"episode_id": "fixture", "occurred_at": "2026-01-01T00:00:00Z",
            "learned_at": "2026-01-01T00:00:00Z", "content": json.dumps({
                "source_evidence": [{"source_id": "event:1", "speaker_id": "operator",
                    "kind": "original-utterance", "text": "A fixture."}]})}
        with tempfile.TemporaryDirectory() as temp:
            root = Path(temp)
            cases = root / "cases.json"
            cases.write_text(json.dumps({"sealed_event_ids": [1], "queries": []}), encoding="utf-8")
            with patch.object(sys, "argv", ["lab", "--event-db", str(root / "events"), "--cases", str(cases),
                    "--artifacts", str(root / "run"), "--execute"]), \
                    patch.object(lab.base, "snapshot_database"), patch.object(lab.base, "sha256_file", return_value="fixture"), \
                    patch.object(lab.base, "load_events", return_value=[{"id": 1, "type": "conversation-episode-sealed"}]), \
                    patch.object(lab, "exact_utterance_episodes", return_value=[episode]), \
                    patch.object(lab.Calls, "call", side_effect=[reply(lab.base.FORMATION_TOOL_NAME, proposal),
                        reply(lab.base.EVIDENCE_REVIEW_TOOL_NAME, review)]) as call, \
                    patch.object(lab, "run_graph", return_value={"graph": {"entity_count": 0}, "compact_queries": []}):
                self.assertEqual(lab.main(), 1)
            outcome = json.loads((root / "run/outcomes.json").read_text(encoding="utf-8"))[0]
            self.assertEqual(outcome["status"], "withheld", outcome)
            self.assertEqual(outcome["reason"], "no-directly-evidenced-entities")
            self.assertEqual(call.call_count, 2)


if __name__ == "__main__":
    unittest.main()
