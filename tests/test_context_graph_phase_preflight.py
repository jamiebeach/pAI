"""Fresh-phase planning must never dispatch or reuse a historical budget grant."""
import json
from pathlib import Path
import sys
import tempfile
import unittest
from unittest.mock import Mock, patch

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "scripts"))
from context_graph_episode_lab import CaseLibrary, OpenRouterSelectedPhaseTransport


def phase_event(event_id, outcome):
    return {"id": event_id, "type": "context-graph-identity-phase", "caused_by": 10,
            "payload": {"record_json": json.dumps({"phase": "review", "outcome": outcome,
                         "request_digest": "recorded-digest", "reserved_microusd": 60000})}}


class PhasePreflight(unittest.TestCase):
    def setUp(self):
        directory = tempfile.TemporaryDirectory()
        self.addCleanup(directory.cleanup)
        self.library = CaseLibrary.__new__(CaseLibrary)
        self.library.root = Path(directory.name)
        self.library.cases = {"fixture": ({"id": "fixture", "checkpoint": {}},
                              [{"id": 10}, phase_event(11, "request"), phase_event(12, "response")])}
        self.library.read = Mock(return_value={"contract": {"cutoff": 9}, "digest": "sealed"})
        self.worker = Mock(code_fingerprint={"fixture": "hash"})
        self.next = {"status": "request", "phase": "review", "request_digest": "recorded-digest",
                     "spec": {"adapter_revision": "identity-formation-review-v2", "tool_name": "review",
                              "schema": {"type": "object", "properties": {}}, "system": "Fixture",
                              "input": {"evidence": "Neutral source"}}}
        self.worker.call.return_value = {"status": "awaiting-phase", "pending": [
            {"opening_id": 10, "next": self.next}], "input_events_digest": "prefix", "elapsed_seconds": .1}

    def test_exact_prefix_and_no_authorization(self):
        result = self.library.plan_phase(self.worker, {"case_id": "fixture", "event_id": 12})
        self.assertEqual(self.worker.call.call_args.args[0]["events"], [{"id": 10}])
        self.assertFalse(result["execution_authorized"])
        self.assertEqual(result["cumulative_budget_status"], "unverified-no-reservation")
        self.assertEqual(result["estimated_calls"], 1)
        self.assertEqual(result["provider_calls"], 0)
        provider = result["request"]["provider"]
        self.assertEqual(provider["only"], ["phala"])
        self.assertFalse(provider["allow_fallbacks"])
        self.assertTrue(provider["zdr"])
        self.assertEqual(provider["data_collection"], "deny")
        self.assertEqual(self.library.saved_result(result["saved_result"])["request_digest"], "recorded-digest")

    def test_changed_selected_digest_is_explicit(self):
        self.next["request_digest"] = "candidate-digest"
        result = self.library.plan_phase(self.worker, {"case_id": "fixture", "event_id": 12})
        self.assertTrue(result["request_changed"])
        self.assertFalse(result["execution_authorized"])

    def test_refused_prefix_never_continues(self):
        self.worker.call.return_value = {"status": "refused", "failure": "digest mismatch"}
        with self.assertRaisesRegex(ValueError, "Exact prefix"):
            self.library.plan_phase(self.worker, {"case_id": "fixture", "event_id": 12})

    def test_wrong_phase_and_nonresponse_refuse(self):
        self.next["phase"] = "facts"
        with self.assertRaisesRegex(ValueError, "different phase"):
            self.library.plan_phase(self.worker, {"case_id": "fixture", "event_id": 12})
        with self.assertRaisesRegex(ValueError, "not a completed"):
            self.library.plan_phase(self.worker, {"case_id": "fixture", "event_id": 11})

    def test_fresh_phase_requires_durable_admission_and_settles_once(self):
        class Budget:
            def __init__(self):
                self.reservations = []
                self.settlements = []
            def reserve(self, *values):
                self.reservations.append(values)
                return {"status": "reserved", "event_id": 20}
            def settle(self, *values):
                self.settlements.append(values)

        class Transport:
            def call(self, request):
                self.request = request
                return {
                    "charged_microusd": 1234, "receipt_id": "provider-receipt",
                    "response": {"choices": [{"finish_reason": "tool_calls", "message": {
                        "tool_calls": [{"function": {"name": "review",
                                                       "arguments": json.dumps({"verdict": "supported"})}}]
                    }}]},
                }

        candidate = {"status": "complete", "mode": "fresh-selected-phase",
                     "before": {}, "after": {}, "delta": {}}
        self.worker.call.side_effect = [self.worker.call.return_value, candidate]
        budget, transport = Budget(), Transport()
        result = self.library.execute_phase(
            self.worker,
            {"case_id": "fixture", "event_id": 12,
             "confirmed_request_digest": "recorded-digest",
             "reservation_id": "selected-attempt"},
            budget, transport)
        self.assertEqual(len(budget.reservations), 1)
        self.assertEqual(len(budget.settlements), 1)
        self.assertEqual(budget.settlements[0],
                         ("selected-attempt", "recorded-digest", 1234, "provider-receipt"))
        replay = self.worker.call.call_args.args[0]
        self.assertEqual(replay["counterfactual"]["provenance"], "fresh-selected-phase")
        self.assertEqual(replay["counterfactual"]["response"], {"verdict": "supported"})
        self.assertEqual(result["provider_calls"], 1)
        self.assertEqual(result["automatic_retries"], 0)
        self.assertEqual(result["downstream_status"], "complete")
        self.assertTrue((self.library.root / "runs" /
                         (result["saved_result"] + "-provider-response.json")).is_file())

    def test_fresh_phase_is_disabled_and_digest_mismatch_never_reserves(self):
        with self.assertRaisesRegex(ValueError, "disabled"):
            self.library.execute_phase(self.worker, {"case_id": "fixture", "event_id": 12})
        budget = Mock()
        with self.assertRaisesRegex(ValueError, "digest"):
            self.library.execute_phase(
                self.worker,
                {"case_id": "fixture", "event_id": 12,
                 "confirmed_request_digest": "wrong", "reservation_id": "attempt"},
                budget, Mock())
        budget.reserve.assert_not_called()

    def test_unknown_transport_outcome_stays_reserved_without_retry_or_settlement(self):
        budget = Mock()
        budget.reserve.return_value = {"status": "reserved"}
        transport = Mock()
        transport.call.side_effect = TimeoutError("unknown provider outcome")
        with self.assertRaises(TimeoutError):
            self.library.execute_phase(
                self.worker,
                {"case_id": "fixture", "event_id": 12,
                 "confirmed_request_digest": "recorded-digest", "reservation_id": "unknown"},
                budget, transport)
        budget.reserve.assert_called_once()
        transport.call.assert_called_once()
        budget.settle.assert_not_called()
        diagnostics = [json.loads(path.read_text(encoding="utf-8"))
                       for path in (self.library.root / "runs").glob("*.json")]
        unknown = [row for row in diagnostics
                   if row.get("status") == "provider-outcome-unknown"]
        self.assertEqual(len(unknown), 1)
        self.assertEqual(unknown[0]["charge_status"], "pending-full-bound")
        self.assertEqual(unknown[0]["automatic_retries"], 0)

    def test_real_transport_is_single_attempt_and_requires_strict_receipt(self):
        request = {"provider": {"only": ["phala"], "allow_fallbacks": False,
                                "zdr": True, "data_collection": "deny"}}
        response = {"id": "generation-fixture", "usage": {"cost": .0012341}}
        transport = OpenRouterSelectedPhaseTransport(30)
        with patch.dict("os.environ", {"OPENROUTER_API_KEY": "fixture-key"}), patch(
                "context_graph_resolution_lab.provider_call_with_deadline",
                return_value=response) as provider_call:
            result = transport.call(request)
        provider_call.assert_called_once_with(request, "fixture-key", 30.0)
        self.assertEqual(result["receipt_id"], "generation-fixture")
        self.assertEqual(result["charged_microusd"], 1235)
        self.assertIs(result["response"], response)

    def test_real_transport_refuses_routing_and_unverified_usage_before_settlement(self):
        transport = OpenRouterSelectedPhaseTransport(30)
        with patch.dict("os.environ", {"OPENROUTER_API_KEY": "fixture-key"}):
            with self.assertRaisesRegex(ValueError, "routing"):
                transport.call({"provider": {"only": ["phala"], "allow_fallbacks": True,
                                               "zdr": True, "data_collection": "deny"}})
            with patch("context_graph_resolution_lab.provider_call_with_deadline",
                       return_value={"id": "fixture", "usage": {}}):
                with self.assertRaisesRegex(ValueError, "verified cost"):
                    transport.call({"provider": {"only": ["phala"], "allow_fallbacks": False,
                                                   "zdr": True, "data_collection": "deny"}})


if __name__ == "__main__":
    unittest.main()
