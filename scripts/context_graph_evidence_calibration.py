#!/usr/bin/env python3
"""Score saved context-graph evidence reviews against explicit expected labels."""

from __future__ import annotations

import argparse
import copy
import json
import os
import sys
from datetime import datetime, timezone
from pathlib import Path
from types import SimpleNamespace

import context_graph_lab as lab


VERDICTS = {
    "DIRECTLY_EVIDENCED",
    "SUPPORTED_BY_PRIOR_GRAPH",
    "REASONABLE_INFERENCE",
    "UNSUPPORTED",
    "CONTRADICTED",
}


def load_fixture(path: Path) -> dict:
    document = json.loads(path.read_text(encoding="utf-8"))
    if not isinstance(document, dict) or set(document) != {
        "schema_version", "minimum_accuracy", "minimum_direct_precision",
        "minimum_direct_recall", "expectations"
    }:
        raise ValueError("calibration fixture has unknown or missing keys")
    expectations = document["expectations"]
    if document["schema_version"] != 1 or not isinstance(expectations, list) or not expectations:
        raise ValueError("calibration fixture is invalid")
    for row in expectations:
        if not isinstance(row, dict) or set(row) != {
            "episode_id", "claim_ref", "claim", "expected_verdict"
        }:
            raise ValueError("calibration expectation has unknown or missing keys")
        if not all(isinstance(row[key], str) and row[key] for key in (
            "episode_id", "claim_ref", "claim"
        )) or row["expected_verdict"] not in VERDICTS:
            raise ValueError("calibration expectation is invalid")
    for key in ("minimum_accuracy", "minimum_direct_precision", "minimum_direct_recall"):
        if not isinstance(document[key], (int, float)) or not 0 <= document[key] <= 1:
            raise ValueError(f"{key} must be between zero and one")
    return document


def load_actual_reviews(path: Path) -> dict[tuple[str, str], str]:
    actual: dict[tuple[str, str], str] = {}
    for line in path.read_text(encoding="utf-8").splitlines():
        if not line.strip():
            continue
        row = json.loads(line)
        episode_id = row.get("episode_id")
        review = lab.extract_evidence_review(row.get("response"))
        for claim in review["claim_reviews"]:
            key = (episode_id, claim["claim_ref"])
            if key in actual:
                raise ValueError("saved reviews duplicate an episode claim")
            actual[key] = claim["verdict"]
    return actual


def load_source_requests(path: Path) -> list[dict]:
    rows = []
    for line in path.read_text(encoding="utf-8").splitlines():
        if line.strip():
            row = json.loads(line)
            if not isinstance(row, dict) or not isinstance(row.get("request"), dict):
                raise ValueError("saved request ledger row is invalid")
            rows.append(row)
    return rows


def proposal_from_claims(claims: list[dict]) -> dict:
    entities = []
    relationships = []
    for row in claims:
        claim_ref = row.get("claim_ref")
        claim = row.get("claim")
        if row.get("kind") == "entity" and isinstance(claim_ref, str) and isinstance(claim, dict):
            entities.append({
                "local_ref": claim_ref.removeprefix("entity:"),
                "kind": claim["type"], "label": claim["label"], "aliases": [],
                "identity_action": "NEW", "existing_node_id": None,
            })
        elif (
            row.get("kind") == "relationship"
            and isinstance(claim_ref, str) and isinstance(claim, dict)
        ):
            relationships.append({
                "subject_ref": claim["subject_ref"], "predicate": claim["predicate"],
                "object_ref": claim["object_ref"], "relationship_action": "ASSERT",
            })
        else:
            raise ValueError("saved request claim is invalid")
    proposal = {"schema_version": 1, "entities": entities, "relationships": relationships}
    lab.validate_proposal(proposal, reject_signature_mismatches=False)
    return proposal


def regenerate_requests(rows: list[dict], fixture: dict, args: argparse.Namespace) -> list[dict]:
    wanted = {row["episode_id"] for row in fixture["expectations"]}
    selected = []
    for row in rows:
        if row.get("episode_id") not in wanted:
            continue
        request = copy.deepcopy(row["request"])
        if (
            not isinstance(request.get("messages"), list) or len(request["messages"]) != 2
            or not isinstance(request["messages"][1].get("content"), str)
        ):
            raise ValueError("saved review request has invalid messages")
        payload = json.loads(request["messages"][1]["content"])
        proposal_from_claims(payload["claims"])
        prior = payload.get("verified_prior_graph") or {"entities": [], "facts": []}
        request["model"] = args.model
        request["messages"][0]["content"] = lab.evidence_review_system_message(
            bool(prior.get("entities") or prior.get("facts"))
        )
        request["max_tokens"] = args.review_output_tokens
        request["provider"] = {
            "sort": "price", "require_parameters": True,
            "data_collection": args.openrouter_data_collection,
            "zdr": args.openrouter_zdr == "require",
            "max_price": {
                "prompt": args.max_prompt_price,
                "completion": args.max_completion_price,
            },
        }
        if args.openrouter_provider_only:
            request["provider"]["only"] = [args.openrouter_provider_only]
        if args.reasoning_policy == "low":
            request["reasoning"] = {"effort": "low", "exclude": True}
        elif args.reasoning_policy == "off":
            request["reasoning"] = {"enabled": False, "exclude": True}
        else:
            request.pop("reasoning", None)
        selected.append({
            "episode_id": row["episode_id"], "request": request,
            "proposal": proposal_from_claims(payload["claims"]), "prior_graph": prior,
        })
    if {row["episode_id"] for row in selected} != wanted:
        raise ValueError("saved request ledger does not cover every calibration episode")
    return selected


def execute_reviews(rows: list[dict], args: argparse.Namespace, output_dir: Path) -> Path:
    bounds = [lab.evidence_review_cost_bound(row["request"], args) for row in rows]
    seal = {
        "schema_version": 1,
        "mode": "evidence-review-calibration",
        "model": args.model,
        "case_count": len(rows),
        "request_limit": args.request_limit,
        "cost_ceiling_usd": args.cost_ceiling_usd,
        "maximum_request_cost_bounds_usd": bounds,
        "maximum_total_cost_bound_usd": sum(bounds),
        "executed": bool(args.execute),
        "reasoning_policy": args.reasoning_policy,
        "openrouter_provider_only": args.openrouter_provider_only,
    }
    output_dir.mkdir(parents=True, exist_ok=False)
    lab.write_jsonl(output_dir / "review-requests.jsonl", [
        {"episode_id": row["episode_id"], "request": row["request"],
         "admitted_bound_usd": bound}
        for row, bound in zip(rows, bounds)
    ])
    (output_dir / "calibration-seal.json").write_text(
        json.dumps(seal, ensure_ascii=False, indent=2), encoding="utf-8"
    )
    if sum(bounds) > args.cost_ceiling_usd:
        raise ValueError("sealed calibration bounds exceed --cost-ceiling-usd")
    if not args.execute:
        print(json.dumps(seal, ensure_ascii=False, indent=2))
        return output_dir / "review-responses.jsonl"
    responses = []
    validations = []
    spent = 0.0
    attempts = 0
    for row, bound in zip(rows, bounds):
        if attempts >= args.request_limit or spent + bound > args.cost_ceiling_usd:
            validations.append({"episode_id": row["episode_id"], "status": "paused-budget"})
            continue
        attempts += 1
        try:
            response = lab.openrouter_call(row["request"], os.environ["OPENROUTER_API_KEY"])
            usage = response.get("usage") if isinstance(response, dict) else None
            reported = usage.get("cost") if isinstance(usage, dict) else None
            charged = reported if isinstance(reported, (int, float)) and 0 <= reported <= bound else bound
            spent += charged
            review = lab.extract_evidence_review(response)
            indexed = lab.validate_evidence_review(review, row["proposal"], row["prior_graph"])
            responses.append({"episode_id": row["episode_id"], "response": response})
            validations.append({
                "episode_id": row["episode_id"], "status": "accepted",
                "charged_cost_usd": charged,
                "accounting": "reported" if charged == reported else "bounded-fallback",
                "claim_count": len(indexed),
            })
        except Exception as error:
            validations.append({
                "episode_id": row["episode_id"], "status": "rejected",
                "reason": str(error), "charged_cost_usd": bound,
                "accounting": "bounded-fallback",
            })
            spent += bound
    response_path = output_dir / "review-responses.jsonl"
    lab.write_jsonl(response_path, responses)
    lab.write_jsonl(output_dir / "review-validations.jsonl", validations)
    seal.update({
        "request_attempts": attempts, "charged_cost_usd": spent,
        "accepted_count": sum(row["status"] == "accepted" for row in validations),
        "failure_count": sum(row["status"] != "accepted" for row in validations),
    })
    (output_dir / "calibration-seal.json").write_text(
        json.dumps(seal, ensure_ascii=False, indent=2), encoding="utf-8"
    )
    return response_path


def evaluate(fixture: dict, actual: dict[tuple[str, str], str]) -> dict:
    checks = []
    predicted_direct = 0
    correct_predicted_direct = 0
    expected_direct = 0
    for expected in fixture["expectations"]:
        key = (expected["episode_id"], expected["claim_ref"])
        observed = actual.get(key)
        passed = observed == expected["expected_verdict"]
        if expected["expected_verdict"] == "DIRECTLY_EVIDENCED":
            expected_direct += 1
        if observed == "DIRECTLY_EVIDENCED":
            predicted_direct += 1
            if passed:
                correct_predicted_direct += 1
        checks.append({
            **expected,
            "actual_verdict": observed,
            "passed": passed,
        })
    correct = sum(1 for row in checks if row["passed"])
    accuracy = correct / len(checks)
    direct_precision = (
        correct_predicted_direct / predicted_direct if predicted_direct else 1.0
    )
    direct_recall = correct_predicted_direct / expected_direct if expected_direct else 1.0
    verified_boundary_passed = (
        direct_precision >= fixture["minimum_direct_precision"]
        and direct_recall >= fixture["minimum_direct_recall"]
    )
    exact_passed = (
        accuracy >= fixture["minimum_accuracy"]
        and verified_boundary_passed
    )
    return {
        "schema_version": 1,
        "status": "pass" if exact_passed else "fail",
        "exact_verdict_status": "pass" if exact_passed else "fail",
        "verified_boundary_status": "pass" if verified_boundary_passed else "fail",
        "expectation_count": len(checks),
        "correct_count": correct,
        "accuracy": accuracy,
        "predicted_direct_count": predicted_direct,
        "correct_predicted_direct_count": correct_predicted_direct,
        "direct_precision": direct_precision,
        "expected_direct_count": expected_direct,
        "direct_recall": direct_recall,
        "minimum_accuracy": fixture["minimum_accuracy"],
        "minimum_direct_precision": fixture["minimum_direct_precision"],
        "minimum_direct_recall": fixture["minimum_direct_recall"],
        "checks": checks,
    }


def main() -> int:
    parser = argparse.ArgumentParser(
        description="Score a saved evidence-review ledger against explicit expected labels."
    )
    parser.add_argument("--fixture", type=Path, required=True)
    source = parser.add_mutually_exclusive_group(required=True)
    source.add_argument("--review-responses", type=Path)
    source.add_argument("--source-requests", type=Path)
    parser.add_argument("--output", type=Path)
    parser.add_argument("--artifacts", type=Path, default=Path("artifacts/context-graph-calibration"))
    parser.add_argument("--model")
    parser.add_argument("--execute", action="store_true")
    parser.add_argument("--validate", action="store_true")
    parser.add_argument("--request-limit", type=int, default=4)
    parser.add_argument("--cost-ceiling-usd", type=float, default=0.01)
    parser.add_argument("--max-prompt-price", type=float, default=0.05)
    parser.add_argument("--max-completion-price", type=float, default=0.20)
    parser.add_argument("--review-output-tokens", type=int, default=4096)
    parser.add_argument("--reasoning-policy", choices=("default", "off", "low"), default="low")
    parser.add_argument("--openrouter-zdr", choices=("require", "allow-non-zdr"), default="allow-non-zdr")
    parser.add_argument("--openrouter-data-collection", choices=("deny", "allow"), default="deny")
    parser.add_argument("--openrouter-provider-only", default="DeepInfra")
    args = parser.parse_args()
    fixture = load_fixture(args.fixture)
    response_path = args.review_responses
    if args.source_requests:
        if not args.model or args.execute == args.validate:
            parser.error("--source-requests requires --model and exactly one of --execute/--validate")
        if args.execute and not os.environ.get("OPENROUTER_API_KEY"):
            parser.error("OPENROUTER_API_KEY is missing; no request was made")
        run_id = datetime.now(timezone.utc).strftime("%Y%m%dT%H%M%SZ")
        output_dir = args.artifacts / run_id
        rows = regenerate_requests(load_source_requests(args.source_requests), fixture, args)
        response_path = execute_reviews(rows, args, output_dir)
        if args.validate:
            return 0
        if args.output is None:
            args.output = output_dir / "evidence-calibration.json"
    result = evaluate(fixture, load_actual_reviews(response_path))
    encoded = json.dumps(result, ensure_ascii=False, indent=2)
    if args.output:
        args.output.write_text(encoded + "\n", encoding="utf-8")
    print(encoded)
    return 0 if result["status"] == "pass" else 2


if __name__ == "__main__":
    sys.exit(main())
