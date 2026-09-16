import json
import sys
import tempfile
import unittest
from pathlib import Path


sys.path.insert(0, str(Path(__file__).resolve().parent.parent / "scripts"))
import context_graph_ontology_lab as lab


def ontology():
    entity_types = [
        {"name": name, "definition": f"Definition of {name}.",
         "inclusion_rule": "Include grounded instances.",
         "exclusion_rule": "Exclude unsupported instances."}
        for name in ("agent", "person", "artifact", "concept")
    ]
    predicates = []
    for name, inverse in (("creates", "created_by"), ("created_by", "creates"),
                          ("knows", "known_by"), ("known_by", "knows")):
        predicates.append({
            "name": name, "definition": f"Definition of {name}.",
            "subject_types": ["person"], "object_types": ["concept"],
            "inverse": inverse, "symmetric": False, "transitive": False,
        })
    return {
        "schema_version": 1,
        "ontology_name": "Fixture context ontology",
        "design_principles": ["Ground claims.", "Reuse identity.", "Bound extensions."],
        "entity_types": entity_types,
        "predicates": predicates,
        "baseline_type_mappings": [
            {"source": "human", "disposition": "MAP", "target": "person",
            },
            {"source": "topic", "disposition": "EXTENSION", "target": "topic",
            },
        ],
        "baseline_predicate_mappings": [
            {"source": "made", "disposition": "MAP", "target": "creates",
            },
            {"source": "mentions", "disposition": "DROP", "target": None,
            },
        ],
        "extension_policy": {
            "proposal_threshold": "Require repeated or operator-salient evidence.",
            "required_evidence": "Require sealed episode provenance.",
            "consolidation_rule": "Merge equivalent extensions after review.",
        },
        "risks": ["Over-generalization.", "Vocabulary fragmentation."],
    }


def model_profiles():
    """Synthetic model matrix; active provider choices stay in local config."""
    return [
        {
            "model": f"fixture/provider-model-{index}",
            "label": f"Fixture model {index}",
            "maximum_prompt_usd_per_million": 0.1 + index / 100,
            "maximum_completion_usd_per_million": 0.2 + index / 100,
            "reasoning": {"effort": "low"} if index <= 2
                         else {"enabled": False},
        }
        for index in range(1, 7)
    ]


def write_model_profiles(directory):
    path = Path(directory) / "profiles.json"
    path.write_text(json.dumps({"schema_version": 1,
                                "profiles": model_profiles()}),
                    encoding="utf-8")
    return path


class ContextGraphOntologyLabTests(unittest.TestCase):
    def test_valid_ontology_reports_compression(self):
        metrics = lab.validate_ontology(ontology(), ["human", "topic"], ["made", "mentions"])
        self.assertEqual(4, metrics["entity_type_count"])
        self.assertEqual(4, metrics["predicate_count"])
        self.assertEqual(2.0, metrics["compression_ratio"])

    def test_missing_baseline_mapping_fails_closed(self):
        candidate = ontology()
        candidate["baseline_type_mappings"].pop()
        with self.assertRaisesRegex(ValueError, "cover the baseline exactly"):
            lab.validate_ontology(candidate, ["human", "topic"], ["made", "mentions"])

    def test_unknown_predicate_signature_type_fails_closed(self):
        candidate = ontology()
        candidate["predicates"][0]["subject_types"] = ["unknown"]
        with self.assertRaisesRegex(ValueError, "absent entity type"):
            lab.validate_ontology(candidate, ["human", "topic"], ["made", "mentions"])

    def test_signature_may_explicitly_cover_full_bounded_type_vocabulary(self):
        candidate = ontology()
        extra_names = [f"type_{index}" for index in range(5, 16)]
        candidate["entity_types"].extend(
            {"name": name, "definition": f"Definition of {name}.",
             "inclusion_rule": "Include grounded instances.",
             "exclusion_rule": "Exclude unsupported instances."}
            for name in extra_names
        )
        all_names = [row["name"] for row in candidate["entity_types"]]
        candidate["predicates"][0]["subject_types"] = all_names
        metrics = lab.validate_ontology(
            candidate, ["human", "topic"], ["made", "mentions"],
        )
        self.assertEqual(15, metrics["entity_type_count"])

    def test_descriptor_shape_is_deterministically_checked(self):
        candidate = ontology()
        candidate["entity_types"][0]["comment"] = "not authoritative"
        with self.assertRaisesRegex(ValueError, "unknown or missing keys"):
            lab.validate_ontology(candidate, ["human", "topic"], ["made", "mentions"])

    def test_profiles_are_exact_and_cost_bounds_are_positive(self):
        with tempfile.TemporaryDirectory() as directory:
            profiles = lab.load_profiles(write_model_profiles(directory))
            self.assertEqual(6, len(profiles))
            self.assertEqual(6, len({p["model"] for p in profiles}))
            self.assertEqual("low", profiles[0]["reasoning"]["effort"])
            self.assertFalse(profiles[-1]["reasoning"]["enabled"])
            request = lab.request_for(
                profiles[0], [{"episode_id": "episode:1", "content": json.dumps({"synopsis": "Fixture."})}],
                ["human"], ["made"], 1024,
            )
            self.assertGreater(lab.request_bound(request, profiles[0], 1024), 0)

    def test_non_object_profile_document_fails_cleanly(self):
        with tempfile.TemporaryDirectory() as directory:
            path = Path(directory) / "profiles.json"
            path.write_text("[]", encoding="utf-8")
            with self.assertRaisesRegex(SystemExit, "configuration is invalid"):
                lab.load_profiles(path)

    def test_provider_error_object_is_not_treated_as_a_choice(self):
        with self.assertRaisesRegex(ValueError, "provider returned error object: code=502"):
            lab.response_document({"error": {"code": 502, "message": "upstream"}})

    def test_context_window_log_is_exact_credential_free_and_per_model(self):
        with tempfile.TemporaryDirectory() as directory:
            output = Path(directory)
            profile = lab.load_profiles(write_model_profiles(output))[0]
            request = lab.request_for(
                profile, [{"episode_id": "episode:1", "content": json.dumps({"synopsis": "Fixture."})}],
                ["human"], ["made"], 1024,
            )
            lab.write_context_window_logs(output, [request])
            record = json.loads((output / "context-windows.jsonl").read_text(encoding="utf-8"))
            self.assertEqual(request["messages"], record["messages"])
            self.assertEqual(request["response_format"], record["response_format"])
            self.assertNotIn("api_key", json.dumps(record).lower())
            self.assertEqual(64, len(record["request_sha256"]))
            self.assertEqual(1, len(list(output.glob("context-window-01-*.json"))))


if __name__ == "__main__":
    unittest.main()
