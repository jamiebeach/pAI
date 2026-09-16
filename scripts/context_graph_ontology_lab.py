#!/usr/bin/env python3
"""Compare ontology architecture across models on one fixed real corpus."""

from __future__ import annotations

import argparse
import hashlib
import json
import os
import re
import sys
from datetime import datetime, timezone
from pathlib import Path

import context_graph_lab as graph_lab


TOKEN_RE = re.compile(r"^[a-z0-9][a-z0-9_.:-]*$")
DISPOSITIONS = {"MAP", "EXTENSION", "DROP"}


def parse_args() -> argparse.Namespace:
    root = Path(__file__).resolve().parent.parent
    parser = argparse.ArgumentParser(
        description="Compare compact ontology proposals over one fixed pAI snapshot."
    )
    parser.add_argument("--event-db", type=Path, required=True)
    parser.add_argument("--baseline-result", type=Path, required=True)
    parser.add_argument(
        "--model-config", type=Path,
        default=root / "config" / "context-graph-ontology-models.json",
    )
    parser.add_argument("--episode-limit", type=int, default=4)
    parser.add_argument("--request-limit", type=int, required=True)
    parser.add_argument("--cost-ceiling-usd", type=float, required=True)
    parser.add_argument("--max-output-tokens", type=int, default=10240)
    parser.add_argument("--validate", action="store_true")
    parser.add_argument("--execute", action="store_true")
    parser.add_argument("--artifacts", type=Path)
    return parser.parse_args()


def load_profiles(path: Path) -> list[dict]:
    document = json.loads(path.read_text(encoding="utf-8"))
    if not isinstance(document, dict):
        raise SystemExit("ontology model configuration is invalid")
    profiles = document.get("profiles")
    if document.get("schema_version") != 1 or not isinstance(profiles, list) or not profiles:
        raise SystemExit("ontology model configuration is invalid")
    seen = set()
    for profile in profiles:
        required = {
            "model", "label", "maximum_prompt_usd_per_million",
            "maximum_completion_usd_per_million", "reasoning",
        }
        if not isinstance(profile, dict) or set(profile) != required:
            raise SystemExit("ontology model profile has unknown or missing keys")
        model = profile["model"]
        if not isinstance(model, str) or not model or model in seen:
            raise SystemExit("ontology model slug is invalid or duplicated")
        if any(
            not isinstance(profile[key], (int, float)) or profile[key] <= 0
            for key in (
                "maximum_prompt_usd_per_million",
                "maximum_completion_usd_per_million",
            )
        ):
            raise SystemExit("ontology model profile prices must be positive")
        if not isinstance(profile["reasoning"], dict):
            raise SystemExit("ontology model reasoning policy is invalid")
        seen.add(model)
    return profiles


def validate_args(args: argparse.Namespace, profiles: list[dict]) -> None:
    if args.validate == args.execute:
        raise SystemExit("exactly one of --validate or --execute is required")
    if not 1 <= args.episode_limit <= 16:
        raise SystemExit("--episode-limit must be between 1 and 16")
    if args.request_limit != len(profiles):
        raise SystemExit("--request-limit must equal the sealed model count")
    if args.cost_ceiling_usd <= 0:
        raise SystemExit("--cost-ceiling-usd must be positive")
    if not 1024 <= args.max_output_tokens <= 16384:
        raise SystemExit("--max-output-tokens must be between 1024 and 16384")
    if args.execute and not os.environ.get("OPENROUTER_API_KEY"):
        raise SystemExit("OPENROUTER_API_KEY is missing; no request was made")


def baseline_vocabulary(path: Path) -> tuple[list[str], list[str]]:
    document = json.loads(path.read_text(encoding="utf-8"))
    graph = document.get("graph") if isinstance(document, dict) else None
    if not isinstance(graph, dict):
        raise SystemExit("baseline result has no graph")
    types = sorted({row["entity_type"] for row in graph.get("entities", [])})
    predicates = sorted({row["predicate"] for row in graph.get("facts", [])})
    if not types or not predicates:
        raise SystemExit("baseline graph has no ontology vocabulary")
    return types, predicates


def ontology_schema() -> dict:
    token = {"type": "string", "pattern": TOKEN_RE.pattern, "maxLength": 80}
    mapping = {
        "type": "object", "additionalProperties": False,
        "properties": {
            "source": token,
            "disposition": {"type": "string", "enum": sorted(DISPOSITIONS)},
            "target": {"anyOf": [token, {"type": "null"}]},
        },
        "required": ["source", "disposition", "target"],
    }
    return {
        "type": "object", "additionalProperties": False,
        "properties": {
            "schema_version": {"type": "integer", "enum": [1]},
            "ontology_name": {"type": "string", "maxLength": 120},
            "design_principles": {
                "type": "array", "minItems": 3, "maxItems": 6,
                "items": {"type": "string", "maxLength": 160},
            },
            "entity_types": {
                "type": "array", "minItems": 4, "maxItems": 20,
                "items": {
                    "type": "object", "additionalProperties": False,
                    "properties": {
                        "name": token,
                        "definition": {"type": "string", "maxLength": 240},
                        "inclusion_rule": {"type": "string", "maxLength": 240},
                        "exclusion_rule": {"type": "string", "maxLength": 240},
                    },
                    "required": ["name", "definition", "inclusion_rule", "exclusion_rule"],
                },
            },
            "predicates": {
                "type": "array", "minItems": 4, "maxItems": 36,
                "items": {
                    "type": "object", "additionalProperties": False,
                    "properties": {
                        "name": token,
                        "definition": {"type": "string", "maxLength": 240},
                        "subject_types": {
                            "type": "array", "minItems": 1, "maxItems": 12,
                            "items": token, "uniqueItems": True,
                        },
                        "object_types": {
                            "type": "array", "minItems": 1, "maxItems": 12,
                            "items": token, "uniqueItems": True,
                        },
                        "inverse": {"anyOf": [token, {"type": "null"}]},
                        "symmetric": {"type": "boolean"},
                        "transitive": {"type": "boolean"},
                    },
                    "required": [
                        "name", "definition", "subject_types", "object_types",
                        "inverse", "symmetric", "transitive",
                    ],
                },
            },
            "baseline_type_mappings": {
                "type": "array", "maxItems": 128, "items": mapping,
            },
            "baseline_predicate_mappings": {
                "type": "array", "maxItems": 128, "items": mapping,
            },
            "extension_policy": {
                "type": "object", "additionalProperties": False,
                "properties": {
                    "proposal_threshold": {"type": "string", "maxLength": 240},
                    "required_evidence": {"type": "string", "maxLength": 240},
                    "consolidation_rule": {"type": "string", "maxLength": 240},
                },
                "required": ["proposal_threshold", "required_evidence", "consolidation_rule"],
            },
            "risks": {
                "type": "array", "minItems": 2, "maxItems": 6,
                "items": {"type": "string", "maxLength": 200},
            },
        },
        "required": [
            "schema_version", "ontology_name", "design_principles", "entity_types",
            "predicates", "baseline_type_mappings", "baseline_predicate_mappings",
            "extension_policy", "risks",
        ],
    }


def request_for(
    profile: dict, episodes: list[dict], types: list[str], predicates: list[str],
    max_output_tokens: int,
) -> dict:
    evidence = [
        {"episode_id": row["episode_id"], "sealed_episode": json.loads(row["content"])}
        for row in episodes
    ]
    prompt = {
        "task": "Design a compact reusable upper ontology for a persistent personal AI context graph.",
        "requirements": [
            "Generalize across domains and future episodes rather than mirroring episode wording.",
            "Keep entity types and predicates compact, distinct, and operationally defined.",
            "Every predicate must declare valid subject and object types.",
            "Use lowercase normalized tokens for every type and predicate name.",
            "A non-null inverse must name another predicate fully declared in this response; otherwise use null.",
            "Map every baseline term exactly once to MAP, EXTENSION, or DROP without per-term prose.",
            "MAP targets must name a proposed core type or predicate.",
            "EXTENSION targets name a normalized proposed extension; DROP targets are null.",
            "Do not infer facts about the operator; design vocabulary only.",
        ],
        "baseline_entity_types": types,
        "baseline_predicates": predicates,
        "sealed_episodes": evidence,
    }
    return {
        "model": profile["model"],
        "messages": [
            {
                "role": "system",
                "content": (
                    "You are designing an ontology, not extracting graph facts. Produce one "
                    "compact, reusable schema under the supplied strict JSON contract."
                ),
            },
            {
                "role": "user",
                "content": json.dumps(prompt, ensure_ascii=False, separators=(",", ":")),
            },
        ],
        "temperature": 0.1,
        "max_tokens": max_output_tokens,
        "reasoning": profile["reasoning"],
        "response_format": {
            "type": "json_schema",
            "json_schema": {
                "name": "personal_context_upper_ontology",
                "strict": True,
                "schema": ontology_schema(),
            },
        },
        "provider": {
            "sort": "price",
            "require_parameters": True,
            "data_collection": "deny",
            "zdr": False,
            "max_price": {
                "prompt": profile["maximum_prompt_usd_per_million"],
                "completion": profile["maximum_completion_usd_per_million"],
            },
        },
    }


def request_bound(request: dict, profile: dict, max_output_tokens: int) -> float:
    size = len(json.dumps(request, ensure_ascii=False, separators=(",", ":")).encode("utf-8"))
    return (
        (size + 1024) * profile["maximum_prompt_usd_per_million"] / 1_000_000
        + max_output_tokens * profile["maximum_completion_usd_per_million"] / 1_000_000
    )


def context_window_record(request: dict, ordinal: int) -> dict:
    """Return the exact credential-free model context plus its request contract."""
    encoded = json.dumps(
        request, ensure_ascii=False, separators=(",", ":"), sort_keys=True,
    ).encode("utf-8")
    return {
        "schema_version": 1,
        "ordinal": ordinal,
        "model": request["model"],
        "messages": request["messages"],
        "reasoning": request["reasoning"],
        "response_format": request["response_format"],
        "max_tokens": request["max_tokens"],
        "temperature": request["temperature"],
        "request_sha256": hashlib.sha256(encoded).hexdigest(),
    }


def write_context_window_logs(output: Path, requests: list[dict]) -> None:
    records = [context_window_record(request, index) for index, request in enumerate(requests, 1)]
    graph_lab.write_jsonl(output / "context-windows.jsonl", records)
    for record in records:
        safe_model = re.sub(r"[^a-zA-Z0-9_.-]+", "_", record["model"]).strip("_")
        path = output / f"context-window-{record['ordinal']:02d}-{safe_model}.json"
        path.write_text(json.dumps(record, indent=2, ensure_ascii=False), encoding="utf-8")


def response_document(response: dict) -> dict:
    if isinstance(response, dict) and isinstance(response.get("error"), dict):
        error = response["error"]
        raise ValueError(
            f"provider returned error object: code={error.get('code')} "
            f"message={error.get('message', '')}"
        )
    choices = response.get("choices") if isinstance(response, dict) else None
    if not isinstance(choices, list) or len(choices) != 1:
        raise ValueError("response has no unique choice")
    message = choices[0].get("message") if isinstance(choices[0], dict) else None
    content = message.get("content") if isinstance(message, dict) else None
    if not isinstance(content, str) or not content:
        raise ValueError("response has no structured content")
    try:
        return json.loads(content)
    except json.JSONDecodeError as error:
        raise ValueError("structured content is invalid JSON") from error


def _unique_names(rows: object, label: str) -> set[str]:
    if not isinstance(rows, list) or not rows:
        raise ValueError(f"{label} must be a non-empty array")
    names = []
    for row in rows:
        if not isinstance(row, dict) or not isinstance(row.get("name"), str):
            raise ValueError(f"{label} descriptor is invalid")
        if not TOKEN_RE.fullmatch(row["name"]):
            raise ValueError(f"{label} name is invalid")
        names.append(row["name"])
    if len(names) != len(set(names)):
        raise ValueError(f"{label} names are duplicated")
    return set(names)


def _bounded_text(value: object, label: str, maximum: int) -> None:
    if not isinstance(value, str) or not value.strip() or len(value) > maximum:
        raise ValueError(f"{label} is invalid")


def _bounded_text_array(value: object, label: str, minimum: int, maximum: int,
                        item_maximum: int) -> None:
    if not isinstance(value, list) or not minimum <= len(value) <= maximum:
        raise ValueError(f"{label} is invalid")
    for item in value:
        _bounded_text(item, label, item_maximum)


def validate_mappings(rows: object, sources: list[str], targets: set[str], label: str) -> dict:
    if not isinstance(rows, list):
        raise ValueError(f"{label} mappings are not an array")
    if {row.get("source") for row in rows if isinstance(row, dict)} != set(sources):
        raise ValueError(f"{label} mappings do not cover the baseline exactly")
    counts = {name: 0 for name in DISPOSITIONS}
    seen = set()
    for row in rows:
        if not isinstance(row, dict) or set(row) != {"source", "disposition", "target"}:
            raise ValueError(f"{label} mapping has unknown or missing keys")
        source, disposition, target = row["source"], row["disposition"], row["target"]
        if not isinstance(source, str) or not TOKEN_RE.fullmatch(source):
            raise ValueError(f"{label} mapping source is invalid")
        if source in seen or disposition not in DISPOSITIONS:
            raise ValueError(f"{label} mapping is duplicated or invalid")
        if disposition == "DROP" and target is not None:
            raise ValueError(f"{label} DROP mapping must have a null target")
        if disposition == "MAP" and target not in targets:
            raise ValueError(f"{label} MAP target is not in the core ontology")
        if disposition == "EXTENSION" and (
            not isinstance(target, str) or not TOKEN_RE.fullmatch(target)
        ):
            raise ValueError(f"{label} EXTENSION target is invalid")
        counts[disposition] += 1
        seen.add(source)
    return counts


def validate_ontology(document: dict, baseline_types: list[str], baseline_predicates: list[str]) -> dict:
    required = {
        "schema_version", "ontology_name", "design_principles", "entity_types",
        "predicates", "baseline_type_mappings", "baseline_predicate_mappings",
        "extension_policy", "risks",
    }
    if not isinstance(document, dict) or set(document) != required or document["schema_version"] != 1:
        raise ValueError("ontology has unknown or missing top-level keys")
    _bounded_text(document["ontology_name"], "ontology name", 120)
    _bounded_text_array(document["design_principles"], "design principles", 3, 6, 160)
    types = _unique_names(document["entity_types"], "entity type")
    predicates = _unique_names(document["predicates"], "predicate")
    if not 4 <= len(types) <= 20 or not 4 <= len(predicates) <= 36:
        raise ValueError("ontology vocabulary size is outside the sealed bounds")
    for row in document["entity_types"]:
        if set(row) != {"name", "definition", "inclusion_rule", "exclusion_rule"}:
            raise ValueError("entity type has unknown or missing keys")
        for field in ("definition", "inclusion_rule", "exclusion_rule"):
            _bounded_text(row[field], f"entity type {field}", 240)
    for row in document["predicates"]:
        if set(row) != {
            "name", "definition", "subject_types", "object_types", "inverse",
            "symmetric", "transitive",
        }:
            raise ValueError("predicate has unknown or missing keys")
        _bounded_text(row["definition"], "predicate definition", 240)
        subject_types, object_types = row["subject_types"], row["object_types"]
        if (
            not isinstance(subject_types, list) or not 1 <= len(subject_types) <= 20
            or len(subject_types) != len(set(subject_types))
            or not isinstance(object_types, list) or not 1 <= len(object_types) <= 20
            or len(object_types) != len(set(object_types))
        ):
            raise ValueError("predicate signature is invalid")
        if not set(subject_types).issubset(types) or not set(object_types).issubset(types):
            raise ValueError("predicate signature references an absent entity type")
        if row["inverse"] is not None and (
            not isinstance(row["inverse"], str) or row["inverse"] not in predicates
        ):
            raise ValueError("predicate inverse is absent")
        if not isinstance(row["symmetric"], bool) or not isinstance(row["transitive"], bool):
            raise ValueError("predicate algebra flags are invalid")
    type_counts = validate_mappings(
        document["baseline_type_mappings"], baseline_types, types, "entity type"
    )
    predicate_counts = validate_mappings(
        document["baseline_predicate_mappings"], baseline_predicates, predicates, "predicate"
    )
    policy = document["extension_policy"]
    if not isinstance(policy, dict) or set(policy) != {
        "proposal_threshold", "required_evidence", "consolidation_rule",
    }:
        raise ValueError("extension policy has unknown or missing keys")
    for field in ("proposal_threshold", "required_evidence", "consolidation_rule"):
        _bounded_text(policy[field], f"extension policy {field}", 240)
    _bounded_text_array(document["risks"], "risks", 2, 6, 200)
    return {
        "entity_type_count": len(types),
        "predicate_count": len(predicates),
        "type_mapping_dispositions": type_counts,
        "predicate_mapping_dispositions": predicate_counts,
        "baseline_vocabulary_count": len(baseline_types) + len(baseline_predicates),
        "core_vocabulary_count": len(types) + len(predicates),
        "compression_ratio": (len(types) + len(predicates)) / (
            len(baseline_types) + len(baseline_predicates)
        ),
    }


def main() -> int:
    args = parse_args()
    profiles = load_profiles(args.model_config)
    validate_args(args, profiles)
    root = Path(__file__).resolve().parent.parent
    run_id = datetime.now(timezone.utc).strftime("%Y%m%dT%H%M%SZ")
    output = (args.artifacts or root / "artifacts" / "context-graph-lab" / "ontology-matrix") / run_id
    output.mkdir(parents=True, exist_ok=False)
    snapshot = output / "events-snapshot.sqlite3"
    graph_lab.snapshot_database(args.event_db, snapshot)
    receipt = graph_lab.snapshot_receipt(snapshot)
    events = graph_lab.load_events(snapshot, None)
    episodes = graph_lab.sealed_episodes(events, "newest", args.episode_limit)
    baseline_types, baseline_predicates = baseline_vocabulary(args.baseline_result)
    requests = [
        request_for(profile, episodes, baseline_types, baseline_predicates, args.max_output_tokens)
        for profile in profiles
    ]
    bounds = [
        request_bound(request, profile, args.max_output_tokens)
        for request, profile in zip(requests, profiles)
    ]
    seal = {
        "schema_version": 1,
        "snapshot_receipt": receipt,
        "episode_ids": [row["episode_id"] for row in episodes],
        "baseline_entity_type_count": len(baseline_types),
        "baseline_predicate_count": len(baseline_predicates),
        "models": [row["model"] for row in profiles],
        "request_limit": args.request_limit,
        "cost_ceiling_usd": args.cost_ceiling_usd,
        "maximum_request_cost_bounds_usd": bounds,
        "maximum_total_cost_bound_usd": sum(bounds),
        "executed": args.execute,
    }
    (output / "seal.json").write_text(json.dumps(seal, indent=2), encoding="utf-8")
    graph_lab.write_jsonl(output / "requests.jsonl", requests)
    write_context_window_logs(output, requests)
    if sum(bounds) > args.cost_ceiling_usd:
        raise SystemExit("ontology matrix cost bounds exceed the cumulative ceiling")
    if args.validate:
        print(
            f"PASS ontology matrix seal: models={len(profiles)} episodes={len(episodes)} "
            f"maximum_total_cost_bound_usd={sum(bounds):.8f} artifact={output / 'seal.json'}"
        )
        return 0

    responses = []
    validations = []
    candidates = []
    spent = 0.0
    reported_spend = 0.0
    attempts = 0
    for profile, request, bound in zip(profiles, requests, bounds):
        if attempts >= args.request_limit or spent + bound > args.cost_ceiling_usd:
            validations.append({"model": profile["model"], "status": "paused-budget"})
            continue
        attempts += 1
        try:
            response = graph_lab.openrouter_call(request, os.environ["OPENROUTER_API_KEY"])
        except Exception as error:
            definitive = (
                isinstance(error, graph_lab.OpenRouterHTTPError)
                and error.status in graph_lab.KNOWN_HTTP_REJECTIONS
            )
            charged = 0 if definitive else bound
            spent += charged
            validations.append({
                "model": profile["model"],
                "status": "provider-rejected" if definitive else "provider-failed",
                "reason": str(error), "charged_cost_usd": charged,
            })
            continue
        responses.append({"model": profile["model"], "response": response})
        usage = response.get("usage") if isinstance(response, dict) else None
        reported = usage.get("cost") if isinstance(usage, dict) else None
        charged = reported if isinstance(reported, (int, float)) and 0 <= reported <= bound else bound
        if isinstance(reported, (int, float)) and reported >= 0:
            reported_spend += reported
        spent += charged
        try:
            document = response_document(response)
            metrics = validate_ontology(document, baseline_types, baseline_predicates)
            candidates.append({"model": profile["model"], "ontology": document, "metrics": metrics})
            validations.append({
                "model": profile["model"], "status": "accepted",
                "charged_cost_usd": charged, "metrics": metrics,
            })
        except (TypeError, ValueError, KeyError) as error:
            validations.append({
                "model": profile["model"], "status": "rejected",
                "reason": str(error), "charged_cost_usd": charged,
            })
    graph_lab.write_jsonl(output / "responses.jsonl", responses)
    graph_lab.write_jsonl(output / "validations.jsonl", validations)
    graph_lab.write_jsonl(output / "candidates.jsonl", candidates)
    seal.update({
        "request_attempts": attempts,
        "charged_cost_usd": spent,
        "provider_reported_cost_usd": reported_spend,
        "accepted_count": len(candidates),
        "failure_count": len(validations) - len(candidates),
    })
    (output / "seal.json").write_text(json.dumps(seal, indent=2), encoding="utf-8")
    print(
        f"ONTOLOGY-MATRIX-DONE accepted={len(candidates)}/{len(profiles)} "
        f"cost=${spent:.8f} artifact={output}"
    )
    return 0 if candidates else 2


if __name__ == "__main__":
    sys.exit(main())
