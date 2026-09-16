#!/usr/bin/env python3
"""Build and query a disposable context graph from a real pAI event DB."""

from __future__ import annotations

import argparse
import copy
import hashlib
import json
import os
import re
import sqlite3
import subprocess
import sys
import urllib.error
import urllib.request
from datetime import datetime, timezone
from pathlib import Path


OPENROUTER_ENDPOINT = "https://openrouter.ai/api/v1/chat/completions"
TOKEN_RE = re.compile(r"^[a-z0-9][a-z0-9_.:-]*$")
MODEL_RE = re.compile(r"^[A-Za-z0-9._-]+/[A-Za-z0-9._:-]+$")
FORMATION_TOOL_NAME = "write-context-graph-formation"
EVIDENCE_REVIEW_TOOL_NAME = "review-context-graph-formation"
MAX_PRIOR_GRAPH_FACTS = 24
MAX_PRIOR_GRAPH_BYTES = 32768
KNOWN_HTTP_REJECTIONS = {400, 401, 402, 403, 404, 405, 406, 413, 415, 422, 429}


class OpenRouterHTTPError(RuntimeError):
    def __init__(self, status: int, detail: str, retry_after: str | None = None):
        super().__init__(f"OpenRouter HTTP {status}: {detail}")
        self.status = status
        self.detail = detail
        self.retry_after = retry_after


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Replay real sealed KG formations into the standalone Lisp graph."
    )
    parser.add_argument("--event-db", type=Path, required=True)
    parser.add_argument("--agent-id")
    parser.add_argument("--mode", choices=("replay", "form"), default="replay")
    parser.add_argument("--query", action="append", default=[])
    parser.add_argument(
        "--evidence-policy",
        choices=("all", "reviewed", "verified", "direct-only"), default="all",
    )
    parser.add_argument("--formation-limit", type=int, default=256)
    parser.add_argument("--episode-limit", type=int, default=4)
    parser.add_argument("--episode-order", choices=("oldest", "newest"), default="newest")
    parser.add_argument("--provider", choices=("mock", "openrouter"), default="mock")
    parser.add_argument("--mock-proposals", type=Path)
    parser.add_argument("--model")
    parser.add_argument("--ontology", type=Path)
    parser.add_argument("--execute", action="store_true")
    parser.add_argument("--validate", action="store_true")
    parser.add_argument("--request-limit", type=int)
    parser.add_argument("--cost-ceiling-usd", type=float)
    parser.add_argument("--max-prompt-price", type=float, default=0.20)
    parser.add_argument("--max-completion-price", type=float, default=0.40)
    parser.add_argument("--max-output-tokens", type=int, default=4096)
    parser.add_argument(
        "--reasoning-policy", choices=("default", "off", "low"), default="default",
    )
    parser.add_argument("--review-evidence", action="store_true")
    parser.add_argument("--review-output-tokens", type=int, default=4096)
    parser.add_argument(
        "--openrouter-zdr", choices=("require", "allow-non-zdr"), default="require"
    )
    parser.add_argument(
        "--openrouter-data-collection", choices=("deny", "allow"), default="deny"
    )
    parser.add_argument("--openrouter-provider-only")
    parser.add_argument("--evaluation-file", type=Path)
    parser.add_argument("--artifacts", type=Path)
    parser.add_argument("--runtime", choices=("host", "docker"), default="docker")
    parser.add_argument("--image", default="pai-local:development")
    return parser.parse_args()


def validate_args(args: argparse.Namespace) -> None:
    if args.formation_limit < 1 or args.formation_limit > 4096:
        raise SystemExit("--formation-limit must be between 1 and 4096")
    if args.episode_limit < 1 or args.episode_limit > 32:
        raise SystemExit("--episode-limit must be between 1 and 32")
    if args.mode == "replay":
        forbidden = (
            args.execute
            or args.validate
            or args.model
            or args.mock_proposals
            or args.request_limit
            or args.cost_ceiling_usd
        )
        if forbidden:
            raise SystemExit("provider formation options require --mode form")
        return
    if args.provider == "mock":
        if args.execute or args.validate or args.model or args.request_limit or args.cost_ceiling_usd:
            raise SystemExit("paid execution options require --provider openrouter")
        if not args.mock_proposals:
            raise SystemExit("mock formation requires --mock-proposals")
        return
    if not args.model or len(args.model) > 200 or not MODEL_RE.fullmatch(args.model):
        raise SystemExit("OpenRouter formation requires a bounded author/model slug")
    if args.execute == args.validate:
        raise SystemExit("OpenRouter formation requires exactly one of --execute or --validate")
    if not args.request_limit or not 1 <= args.request_limit <= 32:
        raise SystemExit("OpenRouter formation requires --request-limit between 1 and 32")
    if args.request_limit < args.episode_limit:
        raise SystemExit("--request-limit must cover every selected episode")
    if not args.cost_ceiling_usd or args.cost_ceiling_usd <= 0:
        raise SystemExit("OpenRouter formation requires a positive --cost-ceiling-usd")
    if args.max_prompt_price <= 0 or args.max_completion_price <= 0:
        raise SystemExit("OpenRouter maximum prices must be positive")
    if args.openrouter_provider_only and (
        len(args.openrouter_provider_only) > 80
        or not re.fullmatch(r"[A-Za-z0-9 ._-]+", args.openrouter_provider_only)
    ):
        raise SystemExit("OpenRouter provider name is invalid")
    if not 128 <= args.max_output_tokens <= 8192:
        raise SystemExit("--max-output-tokens must be between 128 and 8192")
    if not 128 <= args.review_output_tokens <= 8192:
        raise SystemExit("--review-output-tokens must be between 128 and 8192")
    if args.review_evidence and args.provider != "openrouter":
        raise SystemExit("evidence review currently requires --provider openrouter")
    if args.review_evidence and not args.ontology:
        raise SystemExit("evidence review requires a selected ontology")
    required_calls = args.episode_limit * (2 if args.review_evidence else 1)
    if args.request_limit < required_calls:
        raise SystemExit("--request-limit must cover formation and evidence-review calls")
    if args.execute and not os.environ.get("OPENROUTER_API_KEY"):
        raise SystemExit("OPENROUTER_API_KEY is missing; no request was made")


def snapshot_database(source: Path, target: Path) -> None:
    if not source.is_file():
        raise SystemExit(f"event database does not exist: {source}")
    source_uri = f"file:{source.resolve().as_posix()}?mode=ro"
    with sqlite3.connect(source_uri, uri=True) as source_db:
        with sqlite3.connect(target) as target_db:
            source_db.backup(target_db)


def sha256_file(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as stream:
        for block in iter(lambda: stream.read(1024 * 1024), b""):
            digest.update(block)
    return digest.hexdigest()


def snapshot_receipt(snapshot: Path) -> dict:
    with sqlite3.connect(f"file:{snapshot.as_posix()}?mode=ro", uri=True) as db:
        event_count, maximum_sequence = db.execute(
            "SELECT COUNT(*), COALESCE(MAX(storage_sequence), 0) FROM pai_events"
        ).fetchone()
    return {
        "sha256": sha256_file(snapshot),
        "event_count": event_count,
        "maximum_storage_sequence": maximum_sequence,
    }


def load_events(snapshot: Path, agent_id: str | None) -> list[dict]:
    with sqlite3.connect(f"file:{snapshot.as_posix()}?mode=ro", uri=True) as db:
        sql = (
            "SELECT event_json FROM pai_events "
            "WHERE event_type IN "
            "('conversation-episode-sealed','knowledge-graph-formation-sealed')"
        )
        params: tuple[str, ...] = ()
        if agent_id:
            sql += " AND agent_id=?"
            params = (agent_id,)
        sql += " ORDER BY storage_sequence"
        return [json.loads(row[0]) for row in db.execute(sql, params)]


def episode_record(event: dict) -> dict:
    payload = event["payload"]
    occurred_at = event.get("occurred_at") or str(payload.get("sealed_at", "unknown"))
    return {
        "episode_id": payload["episode_id"],
        "occurred_at": occurred_at,
        "learned_at": occurred_at,
        "content": json.dumps(
            {
                key: payload.get(key)
                for key in (
                    "synopsis",
                    "subjects",
                    "entities",
                    "retrieval_cues",
                    "broader_categories",
                    "unresolved_threads",
                )
            },
            ensure_ascii=False,
            separators=(",", ":"),
        ),
    }


def canonical_sha256(document: dict) -> str:
    encoded = json.dumps(
        document, ensure_ascii=False, sort_keys=True, separators=(",", ":"),
    ).encode("utf-8")
    return hashlib.sha256(encoded).hexdigest()


def load_selected_ontology(path: Path | None) -> dict | None:
    if path is None:
        return None
    wrapper = json.loads(path.read_text(encoding="utf-8"))
    required = {
        "schema_version", "ontology_revision", "status", "source", "repairs",
        "ontology", "selected_ontology_sha256", "curation",
    }
    if not isinstance(wrapper, dict) or set(wrapper) != required:
        raise SystemExit("selected ontology wrapper has unknown or missing keys")
    if wrapper["schema_version"] != 1 or wrapper["status"] != "lab-qualified":
        raise SystemExit("formation requires a lab-qualified selected ontology")
    if canonical_sha256(wrapper["ontology"]) != wrapper["selected_ontology_sha256"]:
        raise SystemExit("selected ontology integrity hash is invalid")
    return wrapper


def runtime_ontology(selected: dict) -> dict:
    ontology = selected["ontology"]
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


def formation_tool(selected: dict | None = None) -> dict:
    token = {"type": "string", "maxLength": 80, "pattern": TOKEN_RE.pattern}
    kind = token
    predicate = token
    if selected:
        ontology = selected["ontology"]
        kind = {"type": "string", "enum": [row["name"] for row in ontology["entity_types"]]}
        predicate = {"type": "string", "enum": [row["name"] for row in ontology["predicates"]]}
    return {
        "type": "function",
        "function": {
            "name": FORMATION_TOOL_NAME,
            "strict": True,
            "description": "Form a small typed context graph from one sealed episode.",
            "parameters": {
                "type": "object",
                "additionalProperties": False,
                "properties": {
                    "schema_version": {"type": "integer", "enum": [1]},
                    "entities": {
                        "type": "array",
                        "minItems": 1,
                        "maxItems": 24,
                        "items": {
                            "type": "object",
                            "additionalProperties": False,
                            "properties": {
                                "local_ref": {"type": "string", "maxLength": 80},
                                "kind": kind,
                                "label": {"type": "string", "maxLength": 240},
                                "aliases": {
                                    "type": "array",
                                    "maxItems": 8,
                                    "uniqueItems": True,
                                    "items": {"type": "string", "maxLength": 240},
                                },
                                "classifications": {
                                    "type": "array",
                                    "maxItems": 8,
                                    "uniqueItems": True,
                                    "items": {"type": "string", "maxLength": 80},
                                },
                                "identity_action": {"type": "string", "enum": ["NEW"]},
                                "existing_node_id": {"type": "null"},
                            },
                            "required": [
                                "local_ref", "kind", "label", "aliases",
                                "classifications",
                                "identity_action", "existing_node_id",
                            ],
                        },
                    },
                    "relationships": {
                        "type": "array",
                        "maxItems": 48,
                        "items": {
                            "type": "object",
                            "additionalProperties": False,
                            "properties": {
                                "subject_ref": {"type": "string", "maxLength": 80},
                                "predicate": predicate,
                                "object_ref": {"type": "string", "maxLength": 80},
                                "relationship_action": {
                                    "type": "string", "enum": ["ASSERT"]
                                },
                            },
                            "required": [
                                "subject_ref", "predicate", "object_ref",
                                "relationship_action",
                            ],
                        },
                    },
                },
                "required": ["schema_version", "entities", "relationships"],
            },
        },
    }


def formation_request(
    episode: dict, args: argparse.Namespace, selected: dict | None = None,
) -> dict:
    ontology_instruction = "Use normalized generic kinds and predicates."
    if selected:
        ontology = selected["ontology"]
        contract = {
            "revision": selected["ontology_revision"],
            "entity_types": [
                {"name": row["name"], "definition": row["definition"]}
                for row in ontology["entity_types"]
            ],
            "predicates": [
                {
                    "name": row["name"], "definition": row["definition"],
                    "subject_types": row["subject_types"],
                    "object_types": row["object_types"],
                }
                for row in ontology["predicates"]
            ],
        }
        ontology_instruction = (
            "Use only the following fixed upper ontology. Domain kinds such as plant, "
            "dog, material, or weather kind are concept entities linked with classified_as, "
            "not new entity types. Use other_thing only when no declared type fits. "
            f"ONTOLOGY={json.dumps(contract, ensure_ascii=False, separators=(',', ':'))}"
        )
    messages = [
        {
            "role": "system",
            "content": (
                "Form a compact, generic personal context graph from the supplied sealed "
                "episode. Extract independently useful entities and typed relationships, "
                "not every noun. Prefer durable operator requirements, preferences, people, "
                "projects, constraints, capabilities, events and their explicit relations. "
                "Use only evidence in the episode. Do not infer sensitive or unstated facts. "
                "Give useful entities a small set of broad grounded classification strings "
                "for retrieval, but do not invent traits. "
                f"{ontology_instruction} Call write-context-graph-"
                "formation exactly once. This isolated pass has no eligible existing IDs, "
                "so every identity_action must be NEW and existing_node_id must be null."
            ),
        },
        {
            "role": "user",
            "content": json.dumps(
                {"episode_id": episode["episode_id"], "sealed_episode": json.loads(episode["content"])},
                ensure_ascii=False,
                separators=(",", ":"),
            ),
        },
    ]
    request = {
        "model": args.model or "mock",
        "messages": messages,
        "temperature": 0.1,
        "max_tokens": args.max_output_tokens,
        "tools": [formation_tool(selected)],
        "tool_choice": "required",
        "provider": {
            "sort": "price",
            "require_parameters": True,
            "data_collection": args.openrouter_data_collection,
            "zdr": args.openrouter_zdr == "require",
            "max_price": {
                "prompt": args.max_prompt_price,
                "completion": args.max_completion_price,
            },
        },
    }
    if args.openrouter_provider_only:
        request["provider"]["only"] = [args.openrouter_provider_only]
    if args.reasoning_policy == "off":
        request["reasoning"] = {"enabled": False, "exclude": True}
    elif args.reasoning_policy == "low":
        request["reasoning"] = {"effort": "low", "exclude": True}
    return request


def request_cost_bound(request: dict, args: argparse.Namespace) -> float:
    encoded = json.dumps(request, ensure_ascii=False, separators=(",", ":")).encode("utf-8")
    input_upper = len(encoded) + 1024
    return (
        input_upper * args.max_prompt_price / 1_000_000
        + args.max_output_tokens * args.max_completion_price / 1_000_000
    )


def evidence_review_reserve_bound(request: dict, args: argparse.Namespace) -> float:
    encoded = json.dumps(request, ensure_ascii=False, separators=(",", ":")).encode("utf-8")
    input_upper = (
        len(encoded) + args.max_output_tokens * 4 + MAX_PRIOR_GRAPH_BYTES + 8192
    )
    return (
        input_upper * args.max_prompt_price / 1_000_000
        + args.review_output_tokens * args.max_completion_price / 1_000_000
    )


def evidence_review_cost_bound(request: dict, args: argparse.Namespace) -> float:
    encoded = json.dumps(request, ensure_ascii=False, separators=(",", ":")).encode("utf-8")
    input_upper = len(encoded) + 1024
    return (
        input_upper * args.max_prompt_price / 1_000_000
        + args.review_output_tokens * args.max_completion_price / 1_000_000
    )


def openrouter_call(request: dict, api_key: str) -> dict:
    body = json.dumps(request, ensure_ascii=False, separators=(",", ":")).encode("utf-8")
    http_request = urllib.request.Request(
        OPENROUTER_ENDPOINT,
        data=body,
        headers={"Authorization": f"Bearer {api_key}", "Content-Type": "application/json"},
        method="POST",
    )
    try:
        with urllib.request.urlopen(http_request, timeout=180) as response:
            return json.loads(response.read().decode("utf-8"))
    except urllib.error.HTTPError as error:
        detail = error.read(4096).decode("utf-8", errors="replace")
        raise OpenRouterHTTPError(error.code, detail, (error.headers or {}).get("Retry-After")) from error


def extract_proposal(response: dict) -> dict:
    if not isinstance(response, dict):
        raise ValueError("provider response is not an object")
    choices = response.get("choices")
    if not isinstance(choices, list) or len(choices) != 1:
        raise ValueError("provider response must contain exactly one choice")
    message = choices[0].get("message")
    calls = message.get("tool_calls") if isinstance(message, dict) else None
    if not isinstance(calls, list) or len(calls) != 1:
        raise ValueError("provider response must contain exactly one native tool call")
    function = calls[0].get("function")
    if not isinstance(function, dict) or function.get("name") != FORMATION_TOOL_NAME:
        raise ValueError("provider response selected the wrong native tool")
    arguments = function.get("arguments")
    if not isinstance(arguments, str):
        raise ValueError("native tool arguments are not encoded JSON")
    try:
        return json.loads(arguments)
    except json.JSONDecodeError as error:
        raise ValueError("native tool arguments are invalid JSON") from error


def validate_proposal(
    proposal: dict, selected: dict | None = None,
    reject_signature_mismatches: bool = True,
    reject_relationship_errors: bool = True,
) -> list[dict]:
    if not isinstance(proposal, dict) or set(proposal) != {
        "schema_version", "entities", "relationships"
    }:
        raise ValueError("proposal has unknown or missing top-level keys")
    entities = proposal["entities"]
    relationships = proposal["relationships"]
    if proposal["schema_version"] != 1 or not isinstance(entities, list) or not 1 <= len(entities) <= 24:
        raise ValueError("proposal entity collection is invalid")
    if not isinstance(relationships, list) or len(relationships) > 48:
        raise ValueError("proposal relationship collection is invalid")
    refs: set[str] = set()
    kinds_by_ref: dict[str, str] = {}
    allowed_types = None
    signatures = None
    if selected:
        ontology = selected["ontology"]
        allowed_types = {row["name"] for row in ontology["entity_types"]}
        signatures = {
            row["name"]: (set(row["subject_types"]), set(row["object_types"]))
            for row in ontology["predicates"]
        }
    for entity in entities:
        allowed_entity_keys = {
            "local_ref", "kind", "label", "aliases", "classifications",
            "identity_action", "existing_node_id",
        }
        if (not isinstance(entity, dict)
                or set(entity) - allowed_entity_keys
                or set(entity) < (allowed_entity_keys - {"classifications"})):
            raise ValueError("entity descriptor has unknown or missing keys")
        ref, kind, label, aliases = (
            entity["local_ref"], entity["kind"], entity["label"], entity["aliases"]
        )
        if not isinstance(ref, str) or not ref or len(ref) > 80 or ref in refs:
            raise ValueError("entity local_ref is invalid or duplicated")
        if not isinstance(kind, str) or len(kind) > 80 or not TOKEN_RE.fullmatch(kind):
            raise ValueError("entity kind is invalid")
        if allowed_types is not None and kind not in allowed_types:
            raise ValueError("entity kind is outside the selected ontology")
        if not isinstance(label, str) or not label or len(label) > 240:
            raise ValueError("entity label is invalid")
        if not isinstance(aliases, list) or len(aliases) > 8 or any(
            not isinstance(alias, str) or not alias or len(alias) > 240
            for alias in aliases
        ) or len(set(aliases)) != len(aliases):
            raise ValueError("entity aliases are invalid")
        classifications = entity.get("classifications") or []
        if (not isinstance(classifications, list) or len(classifications) > 8
                or len(set(classifications)) != len(classifications)
                or any(not isinstance(item, str) or not item or len(item) > 80
                       for item in classifications)):
            raise ValueError("entity classifications are invalid")
        if entity["identity_action"] != "NEW" or entity["existing_node_id"] is not None:
            raise ValueError("isolated formation may only create NEW identities")
        refs.add(ref)
        kinds_by_ref[ref] = kind
    seen_relationships: set[tuple[str, str, str]] = set()
    relationship_rejections: list[dict] = []
    for relationship_index, relationship in enumerate(relationships):
        if not isinstance(relationship, dict) or set(relationship) != {
            "subject_ref", "predicate", "object_ref", "relationship_action"
        }:
            if reject_relationship_errors:
                raise ValueError("relationship has unknown or missing keys")
            relationship_rejections.append({
                "relationship_index": relationship_index,
                "reason": "unknown-or-missing-relationship-keys",
            })
            continue
        subject, predicate, obj = (
            relationship["subject_ref"], relationship["predicate"], relationship["object_ref"]
        )
        triple = (subject, predicate, obj)
        reason = None
        if subject not in refs or obj not in refs:
            reason = "unknown-entity-reference"
        elif subject == obj:
            reason = "self-relationship"
        elif (
            not isinstance(predicate, str) or len(predicate) > 80
            or not TOKEN_RE.fullmatch(predicate)
        ):
            reason = "invalid-predicate"
        elif relationship["relationship_action"] != "ASSERT":
            reason = "invalid-relationship-action"
        elif triple in seen_relationships:
            reason = "duplicate-relationship"
        if reason:
            if reject_relationship_errors:
                raise ValueError(
                    "relationship is invalid, duplicated, or references an unknown entity"
                )
            relationship_rejections.append({
                "relationship_index": relationship_index,
                "subject_ref": subject, "predicate": predicate, "object_ref": obj,
                "reason": reason,
            })
            continue
        if signatures is not None:
            signature = signatures.get(predicate)
            if signature is None:
                if reject_relationship_errors:
                    raise ValueError("relationship predicate is outside the selected ontology")
                relationship_rejections.append({
                    "relationship_index": relationship_index,
                    "subject_ref": subject, "predicate": predicate, "object_ref": obj,
                    "reason": "predicate-outside-selected-ontology",
                })
                continue
            subject_types, object_types = signature
            if kinds_by_ref[subject] not in subject_types or kinds_by_ref[obj] not in object_types:
                rejection = {
                    "relationship_index": relationship_index,
                    "subject_ref": subject, "predicate": predicate, "object_ref": obj,
                    "subject_type": kinds_by_ref[subject], "object_type": kinds_by_ref[obj],
                    "reason": "typed-signature-incompatible",
                }
                if reject_signature_mismatches:
                    raise ValueError("relationship violates the selected ontology signature")
                relationship_rejections.append(rejection)
                continue
        seen_relationships.add(triple)
    return relationship_rejections


def proposal_without_rejected_relationships(
    proposal: dict, rejected_relationships: list[dict],
) -> dict:
    """Return the exact proposal with only deterministically rejected edges omitted."""
    rejected_indices = {
        row["relationship_index"] for row in rejected_relationships
        if isinstance(row.get("relationship_index"), int)
    }
    return {
        "schema_version": proposal["schema_version"],
        "entities": proposal["entities"],
        "relationships": [
            row for index, row in enumerate(proposal["relationships"])
            if index not in rejected_indices
        ],
    }


def proposal_response(proposal: dict) -> dict:
    return {
        "choices": [{
            "message": {
                "role": "assistant", "content": None,
                "tool_calls": [{
                    "id": "mock-formation", "type": "function",
                    "function": {
                        "name": FORMATION_TOOL_NAME,
                        "arguments": json.dumps(proposal, ensure_ascii=False, separators=(",", ":")),
                    },
                }],
            }
        }],
        "usage": {"prompt_tokens": 0, "completion_tokens": 0, "total_tokens": 0, "cost": 0},
    }


def evidence_claims(proposal: dict) -> list[dict]:
    claims = [
        {
            "claim_ref": f"entity:{row['local_ref']}",
            "kind": "entity",
            "claim": {"type": row["kind"], "label": row["label"]},
        }
        for row in proposal["entities"]
    ]
    claims.extend(
        {
            "claim_ref": f"alias:{row['local_ref']}:{index}",
            "kind": "alias",
            "claim": {
                "entity_ref": row["local_ref"], "label": row["label"],
                "alias": alias,
            },
        }
        for row in proposal["entities"]
        for index, alias in enumerate(row.get("aliases") or [])
    )
    claims.extend(
        {
            "claim_ref": f"classification:{row['local_ref']}:{index}",
            "kind": "classification",
            "claim": {
                "entity_ref": row["local_ref"], "label": row["label"],
                "classification": classification,
            },
        }
        for row in proposal["entities"]
        for index, classification in enumerate(row.get("classifications") or [])
    )
    claims.extend(
        {
            "claim_ref": f"relationship:{index}",
            "kind": "relationship",
            "claim": {
                "subject_ref": row["subject_ref"], "predicate": row["predicate"],
                "object_ref": row["object_ref"],
            },
        }
        for index, row in enumerate(proposal["relationships"])
    )
    return claims


def evidence_review_tool(claim_count: int) -> dict:
    return {
        "type": "function",
        "function": {
            "name": EVIDENCE_REVIEW_TOOL_NAME,
            "strict": True,
            "description": "Classify every proposed graph claim against the sealed evidence.",
            "parameters": {
                "type": "object", "additionalProperties": False,
                "properties": {
                    "schema_version": {"type": "integer", "enum": [1]},
                    "claim_reviews": {
                        "type": "array", "minItems": claim_count, "maxItems": claim_count,
                        "items": {
                            "type": "object", "additionalProperties": False,
                            "properties": {
                                "claim_ref": {"type": "string", "maxLength": 100},
                                "verdict": {
                                    "type": "string",
                                    "enum": [
                                        "DIRECTLY_EVIDENCED", "SUPPORTED_BY_PRIOR_GRAPH",
                                        "REASONABLE_INFERENCE", "UNSUPPORTED", "CONTRADICTED",
                                    ],
                                },
                                "evidence": {"type": "string", "minLength": 1, "maxLength": 600},
                            },
                            "required": ["claim_ref", "verdict", "evidence"],
                        },
                    },
                },
                "required": ["schema_version", "claim_reviews"],
            },
        },
    }


def evidence_review_system_message(prior_available: bool) -> str:
    return (
        "Review every proposed graph claim only against the supplied sealed episode. "
        "Every sealed_episode field is evidence, including entities, subjects, "
        "retrieval_cues, broader_categories, and unresolved_threads—not only synopsis. "
        "An entity explicitly named by the operator is directly evidenced as a mentioned "
        "entity, but its type and relationships still require appropriate wording. An "
        "entity named only inside a prior-agent utterance is not directly evidenced as an "
        "external or operator-personal entity merely because the agent mentioned it. "
        "A prior-agent utterance directly establishes only the agent's own statement, "
        "action, intention, or experience; it does not establish an unconfirmed external "
        "or operator-personal fact. The operator's explicit assertion or correction can "
        "directly establish the operator-personal fact it states and supersedes a "
        "conflicting assistant assertion. Review every proposed alias independently: "
        "naming inspiration does not make the inspiration an alias unless the operator "
        "actually uses that name for the subject. Review every classification independently; "
        "admit only an explicit or faithful broad category, never an unstated trait. "
        "Judge evidence, not whether a claim seems generally plausible or true. "
        "DIRECTLY_EVIDENCED means explicit or faithful paraphrase. "
        "For a relationship, DIRECTLY_EVIDENCED requires the predicate itself to be "
        "stated or faithfully paraphrased; merely discussing, recalling, delivering, "
        "participating in, or appearing beside an entity does not directly establish "
        "works_on, related_to, authored, operates, or another stronger predicate. "
        "If your evidence explanation depends on words such as implies, suggests, or "
        "indicates, the verdict cannot be DIRECTLY_EVIDENCED. "
        "Use REASONABLE_INFERENCE, not UNSUPPORTED, when explicit role or participation "
        "evidence materially supports the relationship without stating its predicate. "
        "A stated desire, intention, or roadmap for a future action is a faithful direct "
        "basis for a plans_* predicate. A generic related_to edge is directly evidenced "
        "when the episode explicitly connects the two endpoints, even if no more specific "
        "relationship is stated. "
        "REASONABLE_INFERENCE means supported but not stated. UNSUPPORTED means absent. "
        "CONTRADICTED means the episode conflicts with it. "
        + (
            "SUPPORTED_BY_PRIOR_GRAPH is allowed only when the exact entity or exact "
            "typed relationship appears in verified_prior_graph. "
            if prior_available else
            "No prior graph is supplied, so never use SUPPORTED_BY_PRIOR_GRAPH. "
        )
        + "Return exactly one review per claim_ref "
        "through review-context-graph-formation."
    )


def evidence_review_request(
    episode: dict, proposal: dict, args: argparse.Namespace,
    prior_graph: dict | None = None,
) -> dict:
    claims = evidence_claims(proposal)
    prior = prior_graph or {"entities": [], "facts": []}
    prior_available = bool(prior["entities"] or prior["facts"])
    messages = [
        {
            "role": "system",
            "content": evidence_review_system_message(prior_available),
        },
        {
            "role": "user",
            "content": json.dumps(
                {
                    "episode_id": episode["episode_id"],
                    "sealed_episode": json.loads(episode["content"]),
                    "claims": claims,
                    "verified_prior_graph": prior,
                },
                ensure_ascii=False, separators=(",", ":"),
            ),
        },
    ]
    request = {
        "model": args.model, "messages": messages, "temperature": 0,
        "max_tokens": args.review_output_tokens,
        "tools": [evidence_review_tool(len(claims))], "tool_choice": "required",
        "provider": {
            "sort": "price", "require_parameters": True,
            "data_collection": args.openrouter_data_collection,
            "zdr": args.openrouter_zdr == "require",
            "max_price": {
                "prompt": args.max_prompt_price,
                "completion": args.max_completion_price,
            },
        },
    }
    if args.openrouter_provider_only:
        request["provider"]["only"] = [args.openrouter_provider_only]
    if args.reasoning_policy == "off":
        request["reasoning"] = {"enabled": False, "exclude": True}
    elif args.reasoning_policy == "low":
        request["reasoning"] = {"effort": "low", "exclude": True}
    return request


def extract_evidence_review(response: dict) -> dict:
    if not isinstance(response, dict):
        raise ValueError("evidence-review response is not an object")
    choices = response.get("choices")
    message = choices[0].get("message") if isinstance(choices, list) and len(choices) == 1 else None
    calls = message.get("tool_calls") if isinstance(message, dict) else None
    if not isinstance(calls, list) or len(calls) != 1:
        raise ValueError("evidence review must contain exactly one native tool call")
    function = calls[0].get("function")
    if not isinstance(function, dict) or function.get("name") != EVIDENCE_REVIEW_TOOL_NAME:
        raise ValueError("evidence review selected the wrong native tool")
    arguments = function.get("arguments")
    if not isinstance(arguments, str):
        raise ValueError("evidence-review arguments are not encoded JSON")
    try:
        return json.loads(arguments)
    except json.JSONDecodeError as error:
        raise ValueError("evidence-review arguments are invalid JSON") from error


def _canonical_graph_entity(kind: str, label: str) -> tuple[str, str]:
    return kind.strip().lower(), " ".join(label.strip().lower().split())


def _proposal_claim_keys(proposal: dict) -> dict[str, tuple]:
    by_ref = {row["local_ref"]: row for row in proposal["entities"]}
    keys: dict[str, tuple] = {}
    for row in proposal["entities"]:
        keys[f"entity:{row['local_ref']}"] = (
            "entity", *_canonical_graph_entity(row["kind"], row["label"]),
        )
        for index, alias in enumerate(row.get("aliases") or []):
            keys[f"alias:{row['local_ref']}:{index}"] = (
                "alias", *_canonical_graph_entity(row["kind"], row["label"]),
                " ".join(alias.strip().lower().split()),
            )
        for index, classification in enumerate(row.get("classifications") or []):
            keys[f"classification:{row['local_ref']}:{index}"] = (
                "classification",
                *_canonical_graph_entity(row["kind"], row["label"]),
                " ".join(classification.strip().lower().split()),
            )
    for index, row in enumerate(proposal["relationships"]):
        subject, obj = by_ref[row["subject_ref"]], by_ref[row["object_ref"]]
        keys[f"relationship:{index}"] = (
            "fact", *_canonical_graph_entity(subject["kind"], subject["label"]),
            row["predicate"], *_canonical_graph_entity(obj["kind"], obj["label"]),
        )
    return keys


def _prior_graph_keys(prior_graph: dict | None) -> set[tuple]:
    if not prior_graph:
        return set()
    keys = {
        ("entity", *_canonical_graph_entity(row["type"], row["name"]))
        for row in prior_graph.get("entities", [])
    }
    keys.update(
        (
            "fact", *_canonical_graph_entity(row["subject_type"], row["subject_name"]),
            row["predicate"], *_canonical_graph_entity(row["object_type"], row["object_name"]),
        )
        for row in prior_graph.get("facts", [])
    )
    return keys


def validate_evidence_review(
    review: dict, proposal: dict, prior_graph: dict | None = None,
) -> dict[str, dict]:
    if not isinstance(review, dict) or set(review) != {"schema_version", "claim_reviews"}:
        raise ValueError("evidence review has unknown or missing keys")
    rows = review["claim_reviews"]
    expected = {row["claim_ref"] for row in evidence_claims(proposal)}
    if review["schema_version"] != 1 or not isinstance(rows, list) or len(rows) != len(expected):
        raise ValueError("evidence review collection is invalid")
    indexed: dict[str, dict] = {}
    claim_keys = _proposal_claim_keys(proposal)
    prior_keys = _prior_graph_keys(prior_graph)
    verdicts = {
        "DIRECTLY_EVIDENCED", "SUPPORTED_BY_PRIOR_GRAPH", "REASONABLE_INFERENCE",
        "UNSUPPORTED", "CONTRADICTED",
    }
    for row in rows:
        if not isinstance(row, dict) or set(row) != {"claim_ref", "verdict", "evidence"}:
            raise ValueError("evidence-review row has unknown or missing keys")
        claim_ref, verdict, evidence = row["claim_ref"], row["verdict"], row["evidence"]
        if (
            claim_ref not in expected or claim_ref in indexed or verdict not in verdicts
            or (
                verdict == "SUPPORTED_BY_PRIOR_GRAPH"
                and claim_keys[claim_ref] not in prior_keys
            )
            or not isinstance(evidence, str) or not 1 <= len(evidence) <= 600
        ):
            raise ValueError("evidence-review row is invalid or unsupported")
        indexed[claim_ref] = row
    if set(indexed) != expected:
        raise ValueError("evidence review does not cover the proposal exactly")
    return indexed


def prior_verified_neighborhood(
    formations: list[dict], proposal: dict, maximum_facts: int = MAX_PRIOR_GRAPH_FACTS,
) -> dict:
    """Return a bounded direct-evidence neighborhood relevant to PROPOSAL."""
    proposal_text = " ".join(
        [row["label"] for row in proposal["entities"]]
        + [row["predicate"] for row in proposal["relationships"]]
    ).lower()
    tokens = {
        token for token in re.findall(r"[a-z0-9_]+", proposal_text) if len(token) >= 3
    }
    candidates: list[tuple[int, str, dict]] = []
    for formation in formations:
        entities = {
            row["local_ref"]: row for row in formation["proposal"]["entities"]
        }
        episode_id = formation["episode"]["episode_id"]
        for fact in formation["proposal"]["facts"]:
            if fact.get("evidence_status") != "direct":
                continue
            subject, obj = entities[fact["subject_ref"]], entities[fact["object_ref"]]
            row = {
                "subject_type": subject["type"], "subject_name": subject["name"],
                "predicate": fact["predicate"],
                "object_type": obj["type"], "object_name": obj["name"],
                "source_episode_id": episode_id,
            }
            text = " ".join(str(value).lower() for value in row.values())
            score = sum(1 for token in tokens if token in text)
            if score:
                key = "|".join(str(value) for value in row.values())
                candidates.append((score, key, row))
    candidates.sort(key=lambda item: (-item[0], item[1]))
    facts = [row for _, _, row in candidates[:maximum_facts]]
    entity_rows = {
        _canonical_graph_entity(row[key_type], row[key_name])
        for row in facts
        for key_type, key_name in (
            ("subject_type", "subject_name"), ("object_type", "object_name"),
        )
    }
    entities = [
        {"type": kind, "name": name} for kind, name in sorted(entity_rows)
    ]
    neighborhood = {"entities": entities, "facts": facts}
    encoded = json.dumps(neighborhood, ensure_ascii=False, separators=(",", ":")).encode()
    if len(encoded) > MAX_PRIOR_GRAPH_BYTES:
        raise ValueError("bounded prior graph exceeds its byte ceiling")
    return neighborhood


def evidence_review_rejections(
    proposal: dict, reviews: dict[str, dict],
) -> tuple[set[str], list[dict], dict[str, int]]:
    rejected_verdicts = {"UNSUPPORTED", "CONTRADICTED"}
    rejected_entities = {
        row["local_ref"] for row in proposal["entities"]
        if reviews[f"entity:{row['local_ref']}"]["verdict"] in rejected_verdicts
    }
    rejected_relationships = []
    counts: dict[str, int] = {}
    for row in reviews.values():
        counts[row["verdict"]] = counts.get(row["verdict"], 0) + 1
    for index, relationship in enumerate(proposal["relationships"]):
        review = reviews[f"relationship:{index}"]
        rejected_endpoint = (
            relationship["subject_ref"] in rejected_entities
            or relationship["object_ref"] in rejected_entities
        )
        if review["verdict"] in rejected_verdicts or rejected_endpoint:
            rejected_relationships.append({
                "subject_ref": relationship["subject_ref"],
                "predicate": relationship["predicate"],
                "object_ref": relationship["object_ref"],
                "reason": (
                    "evidence-review:rejected-entity-endpoint"
                    if rejected_endpoint and review["verdict"] not in rejected_verdicts
                    else f"evidence-review:{review['verdict'].lower()}"
                ),
            })
    return rejected_entities, rejected_relationships, counts


def proposal_without_rejected_aliases(
    proposal: dict, reviews: dict[str, dict],
) -> dict:
    """Detach aliases that lack direct evidence without discarding their entity."""
    result = copy.deepcopy(proposal)
    for entity in result["entities"]:
        aliases = entity.get("aliases") or []
        entity["aliases"] = [
            alias for index, alias in enumerate(aliases)
            if reviews[f"alias:{entity['local_ref']}:{index}"]["verdict"]
            == "DIRECTLY_EVIDENCED"
        ]
    return result


def proposal_without_rejected_classifications(
    proposal: dict, reviews: dict[str, dict],
) -> dict:
    """Detach classifications that lack direct evidence without losing entities."""
    result = copy.deepcopy(proposal)
    for entity in result["entities"]:
        classifications = entity.get("classifications") or []
        entity["classifications"] = [
            item for index, item in enumerate(classifications)
            if reviews[f"classification:{entity['local_ref']}:{index}"]["verdict"]
            == "DIRECTLY_EVIDENCED"
        ]
    return result


def repair_reversed_typed_relationships(
    proposal: dict, selected: dict,
) -> tuple[dict, list[dict]]:
    """Swap endpoints only when the selected signature proves one valid direction."""
    result = copy.deepcopy(proposal)
    kinds = {row["local_ref"]: row["kind"] for row in result["entities"]}
    signatures = {
        row["name"]: (set(row["subject_types"]), set(row["object_types"]))
        for row in selected["ontology"]["predicates"]
    }
    repairs = []
    for index, relationship in enumerate(result["relationships"]):
        signature = signatures.get(relationship.get("predicate"))
        subject = kinds.get(relationship.get("subject_ref"))
        obj = kinds.get(relationship.get("object_ref"))
        if not signature or not subject or not obj:
            continue
        subject_types, object_types = signature
        forward = subject in subject_types and obj in object_types
        reverse = obj in subject_types and subject in object_types
        if not forward and reverse:
            old_subject, old_object = (
                relationship["subject_ref"], relationship["object_ref"],
            )
            relationship["subject_ref"], relationship["object_ref"] = (
                old_object, old_subject,
            )
            repairs.append({
                "relationship_index": index,
                "predicate": relationship["predicate"],
                "from": [old_subject, old_object],
                "to": [old_object, old_subject],
                "reason": "unique-typed-direction",
            })
    return result, repairs


def ontology_from_formations(formations: list[dict]) -> dict:
    entity_types: set[str] = set()
    signatures: dict[str, tuple[set[str], set[str]]] = {}
    for formation in formations:
        entities = formation["proposal"]["entities"]
        by_ref = {row["local_ref"]: row for row in entities}
        for entity in entities:
            entity_types.add(entity["type"])
        for fact in formation["proposal"]["facts"]:
            subject_types, object_types = signatures.setdefault(
                fact["predicate"], (set(), set())
            )
            subject_types.add(by_ref[fact["subject_ref"]]["type"])
            object_types.add(by_ref[fact["object_ref"]]["type"])
    return {
        "entity_types": sorted(entity_types),
        "edge_types": [
            {
                "name": predicate,
                "subject_types": sorted(subject_types),
                "object_types": sorted(object_types),
            }
            for predicate, (subject_types, object_types) in sorted(signatures.items())
        ],
    }


def normalize_formation(
    episode: dict, raw: dict, source_id: str | int,
    rejected_relationships: list[dict] | None = None,
    rejected_entities: set[str] | None = None,
    evidence_reviews: dict[str, dict] | None = None,
) -> dict:
    raw_entities = raw.get("entities") or []
    by_ref = {row["local_ref"]: row for row in raw_entities}
    rejected_entity_refs = rejected_entities or set()
    verdict_status = {
        "DIRECTLY_EVIDENCED": "direct",
        "SUPPORTED_BY_PRIOR_GRAPH": "prior-graph",
        "REASONABLE_INFERENCE": "inference",
    }
    reviews = evidence_reviews or {}
    entities = []
    for row in raw_entities:
        if row["local_ref"] in rejected_entity_refs:
            continue
        review = reviews.get(f"entity:{row['local_ref']}")
        entities.append({
            "local_ref": row["local_ref"],
            "type": row["kind"],
            "name": row["label"],
            "aliases": row.get("aliases") or [],
            "classifications": row.get("classifications") or [],
            # Opaque IDs from another projection are never imported. Exact
            # canonical identity is resolved inside this disposable graph.
            "action": "NEW",
            "existing_id": None,
            "evidence_status": verdict_status.get(
                review.get("verdict") if review else None, "unreviewed",
            ),
            "evidence_note": review.get("evidence") if review else "not evidence-reviewed",
        })
    facts = []
    rejected_indices = {
        row["relationship_index"]
        for row in (rejected_relationships or [])
        if isinstance(row.get("relationship_index"), int)
    }
    rejected_triples = {
        (row["subject_ref"], row["predicate"], row["object_ref"])
        for row in (rejected_relationships or [])
        if all(key in row for key in ("subject_ref", "predicate", "object_ref"))
    }
    for index, row in enumerate(raw.get("relationships") or []):
        if index in rejected_indices:
            continue
        if row.get("relationship_action") != "ASSERT":
            continue
        if (row["subject_ref"], row["predicate"], row["object_ref"]) in rejected_triples:
            continue
        if row["subject_ref"] in rejected_entity_refs or row["object_ref"] in rejected_entity_refs:
            continue
        subject = by_ref.get(row["subject_ref"])
        obj = by_ref.get(row["object_ref"])
        if not subject or not obj:
            continue
        review = reviews.get(f"relationship:{index}")
        facts.append({
            "subject_ref": row["subject_ref"],
            "predicate": row["predicate"],
            "object_ref": row["object_ref"],
            "fact": f"{subject['label']} {row['predicate'].replace('_', ' ')} {obj['label']}",
            "supersedes_fact_id": None,
            "evidence_status": verdict_status.get(
                review.get("verdict") if review else None, "unreviewed",
            ),
            "evidence_note": review.get("evidence") if review else "not evidence-reviewed",
        })
    return {
        "episode": episode,
        "proposal": {"entities": entities, "facts": facts},
        "source_formation_id": source_id,
    }


def build_replay_bundle(
    events: list[dict], queries: list[str], limit: int, receipt: dict
) -> dict:
    episodes = {
        event["payload"]["episode_id"]: episode_record(event)
        for event in events
        if event.get("type") == "conversation-episode-sealed"
    }
    formations: list[dict] = []
    for event in events:
        if event.get("type") != "knowledge-graph-formation-sealed":
            continue
        if len(formations) >= limit:
            break
        payload = event["payload"]
        source_ids = payload.get("source_episode_ids") or []
        if len(source_ids) != 1 or source_ids[0] not in episodes:
            continue
        raw = payload["proposal"]
        formations.append(normalize_formation(episodes[source_ids[0]], raw, event["id"]))
    if not formations:
        raise SystemExit("no replayable sealed KG formations were found")
    return {
        "schema_version": 1,
        "source": {
            "kind": "pai-event-database-snapshot",
            "snapshot_receipt": receipt,
            "episode_count": len(episodes),
            "formation_count": len(formations),
        },
        "ontology": ontology_from_formations(formations),
        "formations": formations,
        "queries": queries or ["operator preferences requirements"],
    }


def sealed_episodes(events: list[dict], order: str, limit: int) -> list[dict]:
    episodes = [
        episode_record(event)
        for event in events
        if event.get("type") == "conversation-episode-sealed"
    ]
    if order == "newest":
        episodes.reverse()
    return episodes[:limit]


def write_jsonl(path: Path, rows: list[dict]) -> None:
    path.write_text(
        "".join(json.dumps(row, ensure_ascii=False, separators=(",", ":")) + "\n" for row in rows),
        encoding="utf-8",
    )


def build_formation_bundle(
    events: list[dict], queries: list[str], receipt: dict,
    args: argparse.Namespace, output_dir: Path,
) -> tuple[dict | None, dict]:
    episodes = sealed_episodes(events, args.episode_order, args.episode_limit)
    if not episodes:
        raise SystemExit("no sealed episodes were found")
    selected = load_selected_ontology(getattr(args, "ontology", None))
    requests = [formation_request(episode, args, selected) for episode in episodes]
    bounds = [request_cost_bound(request, args) for request in requests]
    review_reserves = (
        [evidence_review_reserve_bound(request, args) for request in requests]
        if args.review_evidence else []
    )
    maximum_total_bound = sum(bounds) + sum(review_reserves)
    seal = {
        "schema_version": 1,
        "mode": "provider-formation",
        "provider": args.provider,
        "model": args.model or "mock",
        "episode_count": len(episodes),
        "request_limit": args.request_limit or 0,
        "cost_ceiling_usd": args.cost_ceiling_usd or 0,
        "maximum_request_cost_bounds_usd": bounds,
        "maximum_evidence_review_reserves_usd": review_reserves,
        "maximum_total_cost_bound_usd": maximum_total_bound,
        "snapshot_receipt": receipt,
        "executed": bool(args.execute),
        "ontology_revision": selected["ontology_revision"] if selected else None,
        "ontology_sha256": selected["selected_ontology_sha256"] if selected else None,
        "evidence_review_enabled": bool(args.review_evidence),
        "evidence_review_output_tokens": args.review_output_tokens,
        "reasoning_policy": args.reasoning_policy,
        "openrouter_provider_only": args.openrouter_provider_only,
    }
    (output_dir / "formation-seal.json").write_text(
        json.dumps(seal, ensure_ascii=False, indent=2), encoding="utf-8"
    )
    write_jsonl(output_dir / "formation-requests.jsonl", requests)
    if args.provider == "openrouter" and args.validate:
        if maximum_total_bound > args.cost_ceiling_usd:
            raise SystemExit("sealed formation request bounds exceed --cost-ceiling-usd")
        return None, seal

    mock_proposals = None
    if args.provider == "mock":
        mock_proposals = json.loads(args.mock_proposals.read_text(encoding="utf-8"))
        if not isinstance(mock_proposals, list) or len(mock_proposals) != len(episodes):
            raise SystemExit("mock proposal count must equal the selected episode count")
    responses: list[dict] = []
    validations: list[dict] = []
    review_requests: list[dict] = []
    review_responses: list[dict] = []
    review_validations: list[dict] = []
    formations: list[dict] = []
    spent = 0.0
    attempts = 0
    for index, (episode, request, bound) in enumerate(zip(episodes, requests, bounds)):
        if args.provider == "openrouter":
            if attempts >= args.request_limit or spent + bound > args.cost_ceiling_usd:
                validations.append({
                    "episode_id": episode["episode_id"], "status": "paused-budget",
                    "admitted_bound_usd": bound,
                })
                continue
            attempts += 1
            try:
                response = openrouter_call(request, os.environ["OPENROUTER_API_KEY"])
            except Exception as error:
                definitive_rejection = (
                    isinstance(error, OpenRouterHTTPError)
                    and error.status in KNOWN_HTTP_REJECTIONS
                )
                charged = 0 if definitive_rejection else bound
                spent += charged
                validations.append({
                    "episode_id": episode["episode_id"],
                    "status": "provider-rejected" if definitive_rejection else "provider-failed",
                    "reason": str(error), "charged_cost_usd": charged,
                    "accounting": "definitive-rejection" if definitive_rejection else "bounded-fallback",
                })
                continue
        else:
            response = proposal_response(mock_proposals[index])
        responses.append({"episode_id": episode["episode_id"], "response": response})
        usage = response.get("usage") if isinstance(response, dict) else None
        reported = usage.get("cost") if isinstance(usage, dict) else None
        charged = (
            reported
            if isinstance(reported, (int, float)) and 0 <= reported <= bound
            else bound
        )
        spent += charged
        accounting = "reported" if charged == reported else "bounded-fallback"
        try:
            proposal = extract_proposal(response)
            structural_rejections = validate_proposal(
                proposal, selected, reject_signature_mismatches=False,
                reject_relationship_errors=False,
            )
            admitted_proposal = proposal_without_rejected_relationships(
                proposal, structural_rejections,
            )
            rejected_entities: set[str] = set()
            evidence_rejections: list[dict] = []
            evidence_counts: dict[str, int] = {}
            indexed_review: dict[str, dict] = {}
            if args.review_evidence:
                prior_graph = prior_verified_neighborhood(formations, admitted_proposal)
                review_request = evidence_review_request(
                    episode, admitted_proposal, args, prior_graph,
                )
                review_bound = evidence_review_cost_bound(review_request, args)
                review_requests.append({
                    "episode_id": episode["episode_id"], "request": review_request,
                    "admitted_bound_usd": review_bound,
                })
                if attempts >= args.request_limit or spent + review_bound > args.cost_ceiling_usd:
                    review_validations.append({
                        "episode_id": episode["episode_id"], "status": "paused-budget",
                        "admitted_bound_usd": review_bound,
                    })
                    validations.append({
                        "episode_id": episode["episode_id"], "status": "review-paused-budget",
                        "charged_cost_usd": charged, "accounting": accounting,
                    })
                    continue
                attempts += 1
                try:
                    review_response = openrouter_call(
                        review_request, os.environ["OPENROUTER_API_KEY"],
                    )
                except Exception as error:
                    definitive_rejection = (
                        isinstance(error, OpenRouterHTTPError)
                        and error.status in KNOWN_HTTP_REJECTIONS
                    )
                    review_charged = 0 if definitive_rejection else review_bound
                    spent += review_charged
                    review_validations.append({
                        "episode_id": episode["episode_id"],
                        "status": "provider-rejected" if definitive_rejection else "provider-failed",
                        "reason": str(error), "charged_cost_usd": review_charged,
                        "accounting": (
                            "definitive-rejection" if definitive_rejection else "bounded-fallback"
                        ),
                    })
                    validations.append({
                        "episode_id": episode["episode_id"], "status": "review-failed",
                        "reason": str(error),
                        "charged_cost_usd": charged + review_charged,
                    })
                    continue
                review_responses.append({
                    "episode_id": episode["episode_id"], "response": review_response,
                })
                review_usage = review_response.get("usage") if isinstance(review_response, dict) else None
                review_reported = review_usage.get("cost") if isinstance(review_usage, dict) else None
                review_charged = (
                    review_reported
                    if isinstance(review_reported, (int, float))
                    and 0 <= review_reported <= review_bound
                    else review_bound
                )
                spent += review_charged
                review_accounting = (
                    "reported" if review_charged == review_reported else "bounded-fallback"
                )
                try:
                    review = extract_evidence_review(review_response)
                    indexed_review = validate_evidence_review(
                        review, admitted_proposal, prior_graph,
                    )
                    admitted_proposal = proposal_without_rejected_aliases(
                        admitted_proposal, indexed_review,
                    )
                    admitted_proposal = proposal_without_rejected_classifications(
                        admitted_proposal, indexed_review,
                    )
                    rejected_entities, evidence_rejections, evidence_counts = (
                        evidence_review_rejections(admitted_proposal, indexed_review)
                    )
                    review_validations.append({
                        "episode_id": episode["episode_id"], "status": "accepted",
                        "admitted_bound_usd": review_bound,
                        "charged_cost_usd": review_charged,
                        "accounting": review_accounting,
                        "verdict_counts": evidence_counts,
                        "rejected_entity_refs": sorted(rejected_entities),
                        "rejected_relationships": evidence_rejections,
                    })
                except (TypeError, ValueError, KeyError) as error:
                    review_validations.append({
                        "episode_id": episode["episode_id"], "status": "rejected",
                        "reason": str(error), "charged_cost_usd": review_charged,
                        "accounting": review_accounting,
                    })
                    validations.append({
                        "episode_id": episode["episode_id"], "status": "review-rejected",
                        "reason": str(error),
                        "charged_cost_usd": charged + review_charged,
                    })
                    continue
            all_relationship_rejections = structural_rejections + evidence_rejections
            formations.append(normalize_formation(
                episode, admitted_proposal, f"lab:{index + 1}", evidence_rejections,
                rejected_entities, indexed_review,
            ))
            validations.append({
                "episode_id": episode["episode_id"],
                "status": (
                    "accepted-with-relationship-rejections"
                    if all_relationship_rejections or rejected_entities else "accepted"
                ),
                "admitted_bound_usd": bound,
                "charged_cost_usd": charged,
                "accounting": accounting,
                "rejected_relationship_count": len(structural_rejections),
                "rejected_relationships": structural_rejections,
                "evidence_verdict_counts": evidence_counts,
                "evidence_rejected_entity_count": len(rejected_entities),
                "evidence_rejected_relationship_count": len(evidence_rejections),
            })
        except (TypeError, ValueError, KeyError) as error:
            validations.append({
                "episode_id": episode["episode_id"], "status": "rejected",
                "reason": str(error), "charged_cost_usd": charged,
                "accounting": accounting,
            })
    write_jsonl(output_dir / "formation-responses.jsonl", responses)
    write_jsonl(output_dir / "formation-validations.jsonl", validations)
    if args.review_evidence:
        write_jsonl(output_dir / "evidence-review-requests.jsonl", review_requests)
        write_jsonl(output_dir / "evidence-review-responses.jsonl", review_responses)
        write_jsonl(output_dir / "evidence-review-validations.jsonl", review_validations)
    seal["request_attempts"] = attempts
    seal["charged_cost_usd"] = spent
    seal["accepted_count"] = len(formations)
    seal["failure_count"] = len(validations) - len(formations)
    (output_dir / "formation-seal.json").write_text(
        json.dumps(seal, ensure_ascii=False, indent=2), encoding="utf-8"
    )
    if not formations:
        raise SystemExit("no provider formation passed deterministic validation")
    return {
        "schema_version": 1,
        "source": {
            "kind": "pai-event-database-snapshot-provider-formation",
            "snapshot_receipt": receipt,
            "episode_count": len(episodes),
            "formation_count": len(formations),
            "model": args.model or "mock",
        },
        "ontology": runtime_ontology(selected) if selected else ontology_from_formations(formations),
        "formations": formations,
        "queries": queries or ["operator preferences requirements"],
    }, seal


def run_lisp(repo: Path, bundle: Path, output: Path, args: argparse.Namespace) -> int:
    env = os.environ.copy()
    if args.runtime == "docker":
        command = [
            "docker", "run", "--rm", "--network", "none",
            "--tmpfs", "/tmp",
            "-e", "PAI_QUICKLISP_SETUP=/opt/quicklisp/setup.lisp",
            "-e", "PAI_CONTEXT_GRAPH_ASD=/pai/pai-context-graph.asd",
            "-e", "PAI_CONTEXT_GRAPH_BUNDLE=/lab/bundle.json",
            "-e", "PAI_CONTEXT_GRAPH_OUTPUT=/lab/result.json",
            "-v", f"{repo.resolve()}:/pai:ro",
            "-v", f"{bundle.parent.resolve()}:/lab",
            "-w", "/pai", args.image,
            "sbcl", "--script", "/pai/scripts/context-graph-lab.lisp",
        ]
    else:
        from conscious_q4_cli import local_sbcl, native_environment, quicklisp_setup

        sbcl = local_sbcl(repo)
        env = native_environment(sbcl)
        cache = repo / ".clone-state" / "host-cache"
        cache.mkdir(parents=True, exist_ok=True)
        env.update(
            {
                "LOCALAPPDATA": str(cache),
                "XDG_CACHE_HOME": str(cache),
                "PAI_QUICKLISP_SETUP": str(quicklisp_setup(repo)),
                "PAI_CONTEXT_GRAPH_ASD": str(repo / "pai-context-graph.asd"),
                "PAI_CONTEXT_GRAPH_BUNDLE": str(bundle),
                "PAI_CONTEXT_GRAPH_OUTPUT": str(output),
            }
        )
        command = [str(sbcl), "--script",
                   str(repo / "scripts" / "context-graph-lab.lisp")]
    return subprocess.run(command, cwd=repo, env=env, check=False).returncode


def load_evaluation(path: Path | None) -> list[dict]:
    if path is None:
        return []
    document = json.loads(path.read_text(encoding="utf-8"))
    rows = document.get("queries") if isinstance(document, dict) else None
    if not isinstance(rows, list) or not rows:
        raise SystemExit("evaluation file requires a non-empty queries array")
    for row in rows:
        if (
            not isinstance(row, dict)
            or set(row) - {"query", "minimum_results", "any_predicates", "any_terms"}
            or not isinstance(row.get("query"), str)
            or not row["query"]
        ):
            raise SystemExit("evaluation query descriptor is invalid")
        if not isinstance(row.get("minimum_results", 1), int) or row.get("minimum_results", 1) < 0:
            raise SystemExit("evaluation minimum_results is invalid")
        for key in ("any_predicates", "any_terms"):
            values = row.get(key, [])
            if not isinstance(values, list) or any(not isinstance(value, str) or not value for value in values):
                raise SystemExit(f"evaluation {key} is invalid")
    return rows


def evaluate_result(result: dict, specifications: list[dict]) -> dict:
    by_query = {row["query"]: row for row in result["queries"]}
    checks = []
    for specification in specifications:
        query = specification["query"]
        row = by_query.get(query, {"result_count": 0, "facts": []})
        facts = row.get("facts") or []
        predicates = {fact.get("predicate", "") for fact in facts}
        text = json.dumps(facts, ensure_ascii=False).lower()
        expected_predicates = specification.get("any_predicates", [])
        expected_terms = specification.get("any_terms", [])
        passed = (
            row.get("result_count", 0) >= specification.get("minimum_results", 1)
            and (not expected_predicates or any(value in predicates for value in expected_predicates))
            and (not expected_terms or any(value.lower() in text for value in expected_terms))
        )
        checks.append({
            "query": query, "passed": passed,
            "result_count": row.get("result_count", 0),
        })
    return {
        "schema_version": 1,
        "passed": all(row["passed"] for row in checks),
        "checks": checks,
    }


def main() -> int:
    args = parse_args()
    validate_args(args)
    repo = Path(__file__).resolve().parent.parent
    run_id = datetime.now(timezone.utc).strftime("%Y%m%dT%H%M%SZ")
    output_dir = (args.artifacts or repo / "artifacts" / "context-graph-lab") / run_id
    output_dir.mkdir(parents=True, exist_ok=False)
    snapshot = output_dir / "events-snapshot.sqlite3"
    bundle_path = output_dir / "bundle.json"
    result_path = output_dir / "result.json"
    snapshot_database(args.event_db, snapshot)
    receipt = snapshot_receipt(snapshot)
    events = load_events(snapshot, args.agent_id)
    evaluation = load_evaluation(args.evaluation_file)
    queries = list(args.query)
    for row in evaluation:
        if row["query"] not in queries:
            queries.append(row["query"])
    if args.mode == "replay":
        bundle = build_replay_bundle(events, queries, args.formation_limit, receipt)
    else:
        bundle, seal = build_formation_bundle(events, queries, receipt, args, output_dir)
        if bundle is None:
            print(
                "PASS context graph formation seal: "
                f"episodes={seal['episode_count']} requests={seal['request_limit']} "
                f"maximum_total_cost_bound_usd={seal['maximum_total_cost_bound_usd']:.8f} "
                f"artifact={output_dir / 'formation-seal.json'}"
            )
            return 0
    bundle["evidence_policy"] = args.evidence_policy
    bundle_path.write_text(json.dumps(bundle, ensure_ascii=False), encoding="utf-8")
    code = run_lisp(repo, bundle_path, result_path, args)
    if code:
        return code
    result = json.loads(result_path.read_text(encoding="utf-8"))
    graph = result["graph"]
    statuses: dict[str, int] = {}
    for application in result["applications"]:
        status = application["status"]
        statuses[status] = statuses.get(status, 0) + 1
    query_counts = [
        {"query": row["query"], "result_count": row["result_count"]}
        for row in result["queries"]
    ]
    evaluation_result = None
    if evaluation:
        evaluation_result = evaluate_result(result, evaluation)
        (output_dir / "evaluation.json").write_text(
            json.dumps(evaluation_result, ensure_ascii=False, indent=2), encoding="utf-8"
        )
    print(
        "PASS context graph lab: "
        f"entities={graph['entity_count']} facts={graph['fact_count']} "
        f"applications={statuses} queries={query_counts} "
        f"evaluation={evaluation_result['passed'] if evaluation_result else 'not-requested'} "
        f"result={result_path}"
    )
    return 0 if not evaluation_result or evaluation_result["passed"] else 2


if __name__ == "__main__":
    sys.exit(main())
