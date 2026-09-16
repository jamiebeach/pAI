import sys
from pathlib import Path
import unittest

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "scripts"))
from context_graph_case_expectations import verify


class CaseExpectations(unittest.TestCase):
    def fixture(self):
        return {
            "status": "complete", "provider_calls": 0,
            "contract": {"profile": "fixture-profile"},
            "admission_trace": [{"claim_trace": [{
                "selection": "retained", "accepted_evidence_records": [{"source": "one"}],
                "proposed_relationship": {"predicate": "parent_of", "fact": "A is parent of B."},
                "review": {"verdict": "REASONABLE_INFERENCE", "source_reading": "inference"},
                "identity_endpoints": {
                    "subject": {"status": "resolved"}, "object": {"status": "resolved"},
                    "attributed_to": {"participant_role": "operator"}},
            }]}],
            "queries": [{"after": {"exact_query_results": [
                {"query": "A", "absence_confirmed": False, "match_count": 1,
                 "scan_complete": True, "matches": [{"node_id": "a", "node_kind": "person"}]},
                {"query": "operator", "absence_confirmed": False, "match_count": 1,
                 "scan_complete": True, "matches": [{"node_id": "op", "node_kind": "person"}]},
                {"query": "missing", "absence_confirmed": True, "match_count": 0,
                 "scan_complete": True, "matches": []}]}}],
        }

    def spec(self):
        return {"schema_version": 1, "expectation_id": "neutral-v1",
                "run": {"status": "complete", "provider_calls": 0},
                "contract": {"profile": "fixture-profile"},
                "claims": [{"predicate": "parent_of", "selection": "retained",
                            "verdict": "REASONABLE_INFERENCE", "subject_identity": "resolved",
                            "object_identity": "resolved", "minimum_evidence_records": 1}],
                "exact_queries": [{"query": "A", "absence_confirmed": False,
                                   "match_count": 1, "scan_complete": True,
                                   "node_kind": "person"},
                                  {"query": "missing", "absence_confirmed": True,
                                   "match_count": 0, "scan_complete": True}],
                "distinct_exact_entities": [["A", "operator"]]}

    def test_explicit_matrix_passes(self):
        self.assertEqual(verify(self.fixture(), self.spec())["status"], "passed")

    def test_output_is_not_an_oracle(self):
        run = self.fixture()
        run["admission_trace"][0]["claim_trace"][0]["selection"] = "omitted"
        result = verify(run, self.spec())
        self.assertEqual(result["status"], "failed")
        self.assertEqual(result["failure_count"], 1)

    def test_missing_query_and_identity_merge_fail(self):
        run = self.fixture()
        run["queries"][0]["after"]["exact_query_results"][1]["matches"][0]["node_id"] = "a"
        result = verify(run, self.spec())
        self.assertEqual(result["status"], "failed")
        self.assertTrue(any(row["kind"] == "distinct-exact-entities" for row in result["failures"]))

    def test_qualification_graph_assertions(self):
        run = {"status": "complete", "database_write_count": 0,
               "nodes": [
                   {"node_id": "operator", "label": "operator", "node_kind": "person",
                    "classifications": []},
                   {"node_id": "child", "label": "Child A", "node_kind": "person",
                    "classifications": ["daughter"]}],
               "edges": [{"from_node_id": "operator", "to_node_id": "child",
                          "predicate": "parent_of", "source_basis": "original",
                          "query_eligible": True}]}
        spec = {"schema_version": 1, "run": {"status": "complete",
                                                "database_write_count": 0},
                "nodes": [{"label": "Child A", "node_kind": "person",
                           "classification": "daughter"}],
                "edges": [{"from_label": "operator", "to_label": "Child A",
                           "predicate": "parent_of", "source_basis": "original",
                           "query_eligible": True}]}
        self.assertEqual(verify(run, spec)["status"], "passed")
        run["edges"][0]["predicate"] = "related_to"
        result = verify(run, spec)
        self.assertEqual(result["status"], "failed")
        self.assertEqual(result["failures"][0]["kind"], "edge")


if __name__ == "__main__":
    unittest.main()
