#!/usr/bin/env python3
"""Verify explicit source-to-query expectations for one saved episode-lab run.

Expectation files containing private labels or statements belong below .scratch.
This verifier never runs replay, scans history, calls a provider, or mutates a run.
"""
from __future__ import annotations

import argparse
import json
from pathlib import Path
from typing import Any


def _claims(run: dict[str, Any]) -> list[dict[str, Any]]:
    return [claim for batch in run.get("admission_trace", [])
            for claim in batch.get("claim_trace", [])]


def _exact(run: dict[str, Any]) -> dict[str, dict[str, Any]]:
    return {row["query"]: row for query in run.get("queries", [])
            for row in query.get("after", {}).get("exact_query_results", [])}


def _matches_fields(row: dict[str, Any], selector: dict[str, Any],
                    ignored: set[str]) -> bool:
    return all(row.get(key) == value for key, value in selector.items()
               if key not in ignored)


def _matches(row: dict[str, Any], selector: dict[str, Any]) -> bool:
    relationship = row.get("proposed_relationship", {})
    review = row.get("review") if isinstance(row.get("review"), dict) else {}
    endpoints = row.get("identity_endpoints", {})
    values = {
        "predicate": relationship.get("predicate"),
        "statement": relationship.get("fact"),
        "selection": row.get("selection"),
        "verdict": review.get("verdict"),
        "source_reading": review.get("source_reading"),
        "subject_identity": endpoints.get("subject", {}).get("status"),
        "object_identity": endpoints.get("object", {}).get("status"),
        "attribution_role": endpoints.get("attributed_to", {}).get("participant_role"),
    }
    return all(values.get(key) == value for key, value in selector.items()
               if key != "expected_count" and key != "minimum_evidence_records")


def verify(run: dict[str, Any], spec: dict[str, Any]) -> dict[str, Any]:
    failures: list[dict[str, Any]] = []
    if spec.get("schema_version") != 1:
        raise ValueError("Unsupported expectation schema")
    for key, expected in spec.get("run", {}).items():
        if run.get(key) != expected:
            failures.append({"kind": "run", "field": key,
                             "expected": expected, "actual": run.get(key)})
    contract = run.get("contract", {})
    for key, expected in spec.get("contract", {}).items():
        if contract.get(key) != expected:
            failures.append({"kind": "contract", "field": key,
                             "expected": expected, "actual": contract.get(key)})
    claims = _claims(run)
    for index, selector in enumerate(spec.get("claims", [])):
        selected = [row for row in claims if _matches(row, selector)]
        expected_count = selector.get("expected_count", 1)
        if len(selected) != expected_count:
            failures.append({"kind": "claim", "index": index,
                             "expected_count": expected_count, "actual_count": len(selected),
                             "selector": selector})
            continue
        minimum = selector.get("minimum_evidence_records")
        if minimum is not None and any(len(row.get("accepted_evidence_records", [])) < minimum
                                       for row in selected):
            failures.append({"kind": "claim-evidence", "index": index,
                             "minimum": minimum, "selector": selector})
    exact = _exact(run)
    for expectation in spec.get("exact_queries", []):
        query = expectation.get("query")
        row = exact.get(query)
        if row is None:
            failures.append({"kind": "exact-query", "query": query, "reason": "missing"})
            continue
        for key in ("absence_confirmed", "match_count", "scan_complete"):
            if key in expectation and row.get(key) != expectation[key]:
                failures.append({"kind": "exact-query", "query": query, "field": key,
                                 "expected": expectation[key], "actual": row.get(key)})
        kinds = {match.get("node_kind") for match in row.get("matches", [])}
        if "node_kind" in expectation and expectation["node_kind"] not in kinds:
            failures.append({"kind": "exact-query", "query": query,
                             "expected_node_kind": expectation["node_kind"],
                             "actual_node_kinds": sorted(str(value) for value in kinds)})
    for pair in spec.get("distinct_exact_entities", []):
        left, right = (_exact(run).get(pair[0]), _exact(run).get(pair[1]))
        left_ids = {row.get("node_id") for row in (left or {}).get("matches", [])}
        right_ids = {row.get("node_id") for row in (right or {}).get("matches", [])}
        if not left_ids or not right_ids or left_ids & right_ids:
            failures.append({"kind": "distinct-exact-entities", "queries": pair,
                             "left_ids": sorted(str(value) for value in left_ids),
                             "right_ids": sorted(str(value) for value in right_ids)})
    nodes = run.get("nodes", [])
    node_labels = {row.get("node_id"): row.get("label") for row in nodes}
    for index, selector in enumerate(spec.get("nodes", [])):
        required_classification = selector.get("classification")
        selected = [row for row in nodes
                    if _matches_fields(row, selector,
                                       {"expected_count", "classification"})
                    and (required_classification is None
                         or required_classification in row.get("classifications", []))]
        expected_count = selector.get("expected_count", 1)
        if len(selected) != expected_count:
            failures.append({"kind": "node", "index": index,
                             "expected_count": expected_count,
                             "actual_count": len(selected), "selector": selector})
    for index, selector in enumerate(spec.get("edges", [])):
        selected = []
        for row in run.get("edges", []):
            augmented = dict(row)
            augmented["from_label"] = node_labels.get(row.get("from_node_id"))
            augmented["to_label"] = node_labels.get(row.get("to_node_id"))
            if _matches_fields(augmented, selector, {"expected_count"}):
                selected.append(row)
        expected_count = selector.get("expected_count", 1)
        if len(selected) != expected_count:
            failures.append({"kind": "edge", "index": index,
                             "expected_count": expected_count,
                             "actual_count": len(selected), "selector": selector})
    return {"schema_version": 1, "status": "passed" if not failures else "failed",
            "expectation_id": spec.get("expectation_id"),
            "claim_count": len(claims), "failure_count": len(failures),
            "failures": failures, "provider_calls": 0, "historical_fold_count": 0}


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--run", type=Path, required=True)
    parser.add_argument("--expectations", type=Path, required=True)
    options = parser.parse_args()
    run = json.loads(options.run.read_text(encoding="utf-8"))
    spec = json.loads(options.expectations.read_text(encoding="utf-8"))
    result = verify(run, spec)
    print(json.dumps(result, sort_keys=True))
    return 0 if result["status"] == "passed" else 1


if __name__ == "__main__":
    raise SystemExit(main())
