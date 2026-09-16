#!/usr/bin/env python3
"""Qualify one selected upper ontology against a real database-backed graph."""

from __future__ import annotations

import argparse
import json
import sys
from datetime import datetime, timezone
from pathlib import Path
from types import SimpleNamespace

import context_graph_lab as graph_lab
import context_graph_ontology_lab as ontology_lab


def parse_args() -> argparse.Namespace:
    root = Path(__file__).resolve().parent.parent
    parser = argparse.ArgumentParser(description="Replay a baseline graph through a selected ontology.")
    parser.add_argument("--event-db", type=Path, required=True)
    parser.add_argument("--baseline-result", type=Path, required=True)
    parser.add_argument(
        "--ontology", type=Path,
        default=root / "config" / "context-graph-upper-ontology-v1.2.json",
    )
    parser.add_argument("--query", action="append", default=[])
    parser.add_argument("--runtime", choices=("host", "docker"), default="docker")
    parser.add_argument("--image", default="pai-local:development")
    parser.add_argument("--artifacts", type=Path)
    return parser.parse_args()


def load_candidate(path: Path, baseline_path: Path) -> dict:
    wrapper = json.loads(path.read_text(encoding="utf-8"))
    required = {
        "schema_version", "ontology_revision", "status", "source", "repairs",
        "ontology", "selected_ontology_sha256", "curation",
    }
    if not isinstance(wrapper, dict) or set(wrapper) != required or wrapper["schema_version"] != 1:
        raise ValueError("selected ontology wrapper has unknown or missing keys")
    if wrapper["status"] not in {"lab-candidate", "lab-qualified"}:
        raise ValueError("selected ontology is not eligible for lab qualification")
    baseline_types, baseline_predicates = ontology_lab.baseline_vocabulary(baseline_path)
    metrics = ontology_lab.validate_ontology(
        wrapper["ontology"], baseline_types, baseline_predicates,
    )
    return {"wrapper": wrapper, "metrics": metrics}


def runtime_ontology(ontology: dict) -> dict:
    return {
        "entity_types": [row["name"] for row in ontology["entity_types"]],
        "edge_types": [
            {
                "name": row["name"],
                "subject_types": row["subject_types"],
                "object_types": row["object_types"],
            }
            for row in ontology["predicates"]
        ],
    }


def map_baseline_graph(graph: dict, ontology: dict) -> tuple[list[dict], dict]:
    type_mappings = {row["source"]: row for row in ontology["baseline_type_mappings"]}
    predicate_mappings = {
        row["source"]: row for row in ontology["baseline_predicate_mappings"]
    }
    signatures = {
        row["name"]: (set(row["subject_types"]), set(row["object_types"]))
        for row in ontology["predicates"]
    }
    entities = {row["entity_id"]: row for row in graph["entities"]}
    mapped_entities: dict[str, dict] = {}
    for entity_id, row in entities.items():
        mapping = type_mappings.get(row["entity_type"])
        if not mapping or mapping["disposition"] != "MAP":
            continue
        mapped_entities[entity_id] = {
            "source_id": entity_id,
            "type": mapping["target"],
            "name": row["name"],
            "aliases": row.get("aliases") or [],
        }

    grouped: dict[str, list[dict]] = {}
    counts = {
        "input_fact_count": len(graph["facts"]),
        "evidence_fact_count": 0,
        "admitted_fact_count": 0,
        "dropped_by_policy_count": 0,
        "signature_rejected_count": 0,
        "unmapped_entity_count": 0,
    }
    rejections = []
    for fact in graph["facts"]:
        mapping = predicate_mappings.get(fact["predicate"])
        if not mapping or mapping["disposition"] != "MAP":
            counts["dropped_by_policy_count"] += 1
            continue
        subject = mapped_entities.get(fact["subject_id"])
        obj = mapped_entities.get(fact["object_id"])
        if not subject or not obj:
            counts["unmapped_entity_count"] += 1
            continue
        predicate = mapping["target"]
        subject_types, object_types = signatures[predicate]
        if subject["type"] not in subject_types or obj["type"] not in object_types:
            counts["signature_rejected_count"] += 1
            rejections.append({
                "fact_id": fact["fact_id"], "source_predicate": fact["predicate"],
                "mapped_predicate": predicate, "subject_type": subject["type"],
                "object_type": obj["type"], "reason": "typed-signature-incompatible",
            })
            continue
        source_ids = fact.get("source_episode_ids") or []
        if not source_ids:
            counts["signature_rejected_count"] += 1
            rejections.append({
                "fact_id": fact["fact_id"], "reason": "missing-episode-provenance",
            })
            continue
        counts["admitted_fact_count"] += 1
        counts["evidence_fact_count"] += len(source_ids)
        for episode_id in source_ids:
            grouped.setdefault(episode_id, []).append({
                "fact": fact, "subject": subject, "object": obj,
                "predicate": predicate,
            })

    formations = []
    for episode_id, rows in sorted(grouped.items()):
        episode_entities: dict[str, dict] = {}
        for row in rows:
            episode_entities[row["subject"]["source_id"]] = row["subject"]
            episode_entities[row["object"]["source_id"]] = row["object"]
        references = {
            entity_id: f"entity:{index}"
            for index, entity_id in enumerate(sorted(episode_entities), 1)
        }
        first = rows[0]["fact"]
        formations.append({
            "episode": {
                "episode_id": episode_id,
                "occurred_at": str(first.get("valid_at") or first.get("created_at") or "unknown"),
                "learned_at": str(first.get("created_at") or first.get("valid_at") or "unknown"),
                "content": "Ontology qualification replay of sealed graph evidence.",
            },
            "proposal": {
                "entities": [
                    {
                        "local_ref": references[entity_id], "type": entity["type"],
                        "name": entity["name"], "aliases": entity["aliases"],
                        "action": "NEW", "existing_id": None,
                    }
                    for entity_id, entity in sorted(episode_entities.items())
                ],
                "facts": [
                    {
                        "subject_ref": references[row["subject"]["source_id"]],
                        "predicate": row["predicate"],
                        "object_ref": references[row["object"]["source_id"]],
                        "fact": row["fact"]["fact"], "supersedes_fact_id": None,
                    }
                    for row in rows
                ],
            },
            "source_formation_id": f"selected-ontology:{episode_id}",
        })
    counts["formation_count"] = len(formations)
    counts["rejections"] = rejections
    return formations, counts


def main() -> int:
    args = parse_args()
    repo = Path(__file__).resolve().parent.parent
    run_id = datetime.now(timezone.utc).strftime("%Y%m%dT%H%M%SZ")
    output = (args.artifacts or repo / "artifacts" / "context-graph-lab" / "ontology-qualification") / run_id
    output.mkdir(parents=True, exist_ok=False)
    snapshot = output / "events-snapshot.sqlite3"
    graph_lab.snapshot_database(args.event_db, snapshot)
    receipt = graph_lab.snapshot_receipt(snapshot)
    events = graph_lab.load_events(snapshot, None)
    available_episodes = {
        event["payload"]["episode_id"]
        for event in events if event.get("type") == "conversation-episode-sealed"
    }
    selected = load_candidate(args.ontology, args.baseline_result)
    baseline = json.loads(args.baseline_result.read_text(encoding="utf-8"))
    formations, mapping_report = map_baseline_graph(
        baseline["graph"], selected["wrapper"]["ontology"],
    )
    used_episodes = {row["episode"]["episode_id"] for row in formations}
    missing_episodes = sorted(used_episodes - available_episodes)
    if missing_episodes:
        raise SystemExit("mapped evidence references episodes absent from the selected database")
    queries = args.query or ["operator agent system", "retrieval gap", "migration project"]
    bundle = {
        "schema_version": 1,
        "source": {
            "kind": "selected-ontology-real-graph-replay",
            "snapshot_receipt": receipt,
            "ontology_revision": selected["wrapper"]["ontology_revision"],
            "selected_ontology_sha256": selected["wrapper"]["selected_ontology_sha256"],
            "mapping_report": mapping_report,
        },
        "ontology": runtime_ontology(selected["wrapper"]["ontology"]),
        "formations": formations,
        "queries": queries,
    }
    bundle_path = output / "bundle.json"
    result_path = output / "result.json"
    report_path = output / "qualification.json"
    bundle_path.write_text(json.dumps(bundle, ensure_ascii=False), encoding="utf-8")
    runtime_args = SimpleNamespace(runtime=args.runtime, image=args.image)
    code = graph_lab.run_lisp(repo, bundle_path, result_path, runtime_args)
    if code:
        return code
    result = json.loads(result_path.read_text(encoding="utf-8"))
    report = {
        "schema_version": 1,
        "ontology_revision": selected["wrapper"]["ontology_revision"],
        "ontology_metrics": selected["metrics"],
        "snapshot_receipt": receipt,
        "mapping_report": mapping_report,
        "result_entity_count": result["graph"]["entity_count"],
        "result_fact_count": result["graph"]["fact_count"],
        "query_result_counts": [
            {"query": row["query"], "result_count": row["result_count"]}
            for row in result["queries"]
        ],
        "passed": (
            mapping_report["admitted_fact_count"] > 0
            and result["graph"]["fact_count"] == mapping_report["admitted_fact_count"]
            and all(row["result_count"] > 0 for row in result["queries"])
        ),
    }
    report_path.write_text(json.dumps(report, indent=2), encoding="utf-8")
    print(
        "PASS selected ontology qualification: "
        f"revision={report['ontology_revision']} admitted={mapping_report['admitted_fact_count']} "
        f"dropped={mapping_report['dropped_by_policy_count']} "
        f"rejected={mapping_report['signature_rejected_count']} "
        f"facts={report['result_fact_count']} queries={report['query_result_counts']} "
        f"artifact={report_path}"
    )
    return 0 if report["passed"] else 2


if __name__ == "__main__":
    sys.exit(main())
