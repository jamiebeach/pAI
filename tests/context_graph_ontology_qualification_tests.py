import json
import sys
import unittest
from pathlib import Path


sys.path.insert(0, str(Path(__file__).resolve().parent.parent / "scripts"))
import context_graph_ontology_qualification as qualification


class ContextGraphOntologyQualificationTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        path = (
            Path(__file__).resolve().parent.parent
            / "config" / "context-graph-upper-ontology-v1.2.json"
        )
        cls.wrapper = json.loads(path.read_text(encoding="utf-8"))
        cls.ontology = cls.wrapper["ontology"]

    def test_v12_generalizes_living_and_world_entities(self):
        names = {row["name"] for row in self.ontology["entity_types"]}
        self.assertTrue({
            "organism", "object", "place", "phenomenon", "other_thing",
        }.issubset(names))
        self.assertNotIn("animal", names)
        self.assertNotIn("plant", names)

    def test_v12_maps_legacy_animal_and_pet_to_organism(self):
        mappings = {
            row["source"]: row["target"]
            for row in self.ontology["baseline_type_mappings"]
        }
        self.assertEqual("organism", mappings["animal"])
        self.assertEqual("organism", mappings["pet"])

    def test_v12_classification_keeps_domain_kinds_out_of_upper_types(self):
        predicates = {row["name"]: row for row in self.ontology["predicates"]}
        classified = predicates["classified_as"]
        self.assertEqual(["concept"], classified["object_types"])
        for entity_type in ("organism", "object", "place", "phenomenon", "other_thing"):
            self.assertIn(entity_type, classified["subject_types"])

    def test_v12_supports_location_without_making_places_objects(self):
        predicates = {row["name"]: row for row in self.ontology["predicates"]}
        located = predicates["located_at"]
        self.assertEqual(["place"], located["object_types"])
        self.assertIn("phenomenon", located["subject_types"])
        self.assertIn("object", located["subject_types"])

    def test_v12_other_thing_is_governed_fallback(self):
        types = {row["name"]: row for row in self.ontology["entity_types"]}
        fallback = types["other_thing"]
        text = " ".join(fallback.values()).lower()
        self.assertIn("fallback", text)
        self.assertIn("extension", text)
        self.assertIn("only", text)

    def test_mapping_admits_valid_facts_and_rejects_bad_signatures(self):
        ontology = {
            "entity_types": [
                {"name": "person"}, {"name": "system"},
                {"name": "concept"}, {"name": "artifact"},
            ],
            "predicates": [{
                "name": "operates", "subject_types": ["person"],
                "object_types": ["system"],
            }],
            "baseline_type_mappings": [
                {"source": "human", "disposition": "MAP", "target": "person"},
                {"source": "service", "disposition": "MAP", "target": "system"},
                {"source": "idea", "disposition": "MAP", "target": "concept"},
            ],
            "baseline_predicate_mappings": [
                {"source": "controls", "disposition": "MAP", "target": "operates"},
                {"source": "mentions", "disposition": "DROP", "target": None},
            ],
        }
        graph = {
            "entities": [
                {"entity_id": "p", "entity_type": "human", "name": "Casey", "aliases": []},
                {"entity_id": "s", "entity_type": "service", "name": "Runtime", "aliases": []},
                {"entity_id": "c", "entity_type": "idea", "name": "A concept", "aliases": []},
            ],
            "facts": [
                {"fact_id": "f1", "subject_id": "p", "predicate": "controls",
                 "object_id": "s", "fact": "Casey operates Runtime.",
                 "source_episode_ids": ["episode:1"], "valid_at": "1", "created_at": "1"},
                {"fact_id": "f2", "subject_id": "c", "predicate": "controls",
                 "object_id": "s", "fact": "A malformed signature.",
                 "source_episode_ids": ["episode:1"], "valid_at": "1", "created_at": "1"},
                {"fact_id": "f3", "subject_id": "p", "predicate": "mentions",
                 "object_id": "c", "fact": "A dropped relation.",
                 "source_episode_ids": ["episode:1"], "valid_at": "1", "created_at": "1"},
            ],
        }
        formations, report = qualification.map_baseline_graph(graph, ontology)
        self.assertEqual(1, report["admitted_fact_count"])
        self.assertEqual(1, report["signature_rejected_count"])
        self.assertEqual(1, report["dropped_by_policy_count"])
        self.assertEqual(1, len(formations))
        self.assertEqual("operates", formations[0]["proposal"]["facts"][0]["predicate"])


if __name__ == "__main__":
    unittest.main()
