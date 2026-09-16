"""Bounded source-coverage and regression adjudication for graph candidates."""
from __future__ import annotations

import json
import re
from pathlib import Path

import context_graph_lab as base
import context_graph_grounding_lab as grounding
import context_graph_resolution_lab as driver

REVISION = "context-graph-candidate-adjudication-v1"
VERDICTS = {"CANDIDATE_BETTER", "PRIOR_BETTER", "EQUIVALENT", "INDETERMINATE"}
DECISIONS = {"ACCEPT_CANDIDATE", "KEEP_PRIOR", "INDETERMINATE"}


def _safe_artifact(repo, value):
    root = (repo / "artifacts/context-graph-lab").resolve()
    path = (repo / value).resolve()
    try:
        path.relative_to(root)
    except ValueError as error:
        raise ValueError("adjudication artifacts must remain inside the graph lab") from error
    return path


def validate_manifest(manifest, corpus):
    if not isinstance(manifest, dict) or set(manifest) != {"schema_version", "source_kind", "cases"}:
        raise ValueError("adjudication manifest has unknown or missing keys")
    rows = manifest["cases"]
    available = {row["case_id"] for row in corpus["cases"]}
    if (manifest["schema_version"] != 1
            or manifest["source_kind"] != "synthetic-controlled-adjudication"
            or not isinstance(rows, list) or not 1 <= len(rows) <= 8):
        raise ValueError("adjudication manifest is invalid or oversized")
    seen = set()
    for row in rows:
        if set(row) != {"case_id", "prior_artifacts", "candidate_artifacts",
                       "expected_decision"}:
            raise ValueError("adjudication case has unknown or missing keys")
        case_id = row["case_id"]
        if (case_id not in available or case_id in seen
                or row["expected_decision"] not in DECISIONS
                or not all(isinstance(row[key], str) and row[key]
                           for key in ("prior_artifacts", "candidate_artifacts"))):
            raise ValueError("adjudication case is invalid or duplicated")
        seen.add(case_id)


def compact_formation(formation, prefix):
    entities = [{"claim_ref": f"{prefix}:entity:{row['local_ref']}",
                 "local_ref": row["local_ref"], "type": row["type"],
                 "name": row["name"], "classifications": row["classifications"]}
                for row in formation["proposal"]["entities"]]
    facts = []
    for index, row in enumerate(formation["proposal"]["facts"]):
        facts.append({"claim_ref": f"{prefix}:fact:{index}",
            "subject_ref": row["subject_ref"], "predicate": row["predicate"],
            "object_ref": row["object_ref"], "fact": row["fact"],
            "grounding": row["grounding"], "temporal": row["temporal"]})
    return {"entities": entities, "facts": facts}


def adjudication_tool():
    omission = {"type": "object", "additionalProperties": False,
        "properties": {"description": {"type": "string", "maxLength": 500},
            "source_id": {"type": "string", "maxLength": 180},
            "quote": {"type": "string", "maxLength": 1000}},
        "required": ["description", "source_id", "quote"]}
    claim = {"type": "object", "additionalProperties": False,
        "properties": {"claim_ref": {"type": "string", "maxLength": 180},
            "reason": {"type": "string", "maxLength": 500}},
        "required": ["claim_ref", "reason"]}
    return {"type": "function", "function": {
        "name": "adjudicate-context-graph-candidate", "strict": True,
        "description": "Compare a prior and candidate graph formation against original source.",
        "parameters": {"type": "object", "additionalProperties": False,
            "properties": {"schema_version": {"type": "integer", "enum": [1]},
                "verdict": {"type": "string", "enum": sorted(VERDICTS)},
                "candidate_omissions": {"type": "array", "maxItems": 12,
                                        "items": omission},
                "unsupported_candidate_claims": {"type": "array", "maxItems": 12,
                                                  "items": claim},
                "supported_improvements": {"type": "array", "maxItems": 12,
                                           "items": claim},
                "summary": {"type": "string", "maxLength": 1200}},
            "required": ["schema_version", "verdict", "candidate_omissions",
                         "unsupported_candidate_claims", "supported_improvements",
                         "summary"]}}}


def adjudication_request(case, packet, prior, candidate, prior_residuals,
                         candidate_residuals, args):
    request = {"model": args.model, "temperature": 0,
        "max_tokens": args.review_output_tokens,
        "messages": [{"role": "system", "content": (
            "Independently compare two context-graph formations against ONLY the original "
            "source_packet. The prior and candidate are untrusted proposals, not evidence. "
            "Check whether the candidate omits any useful directly expressed proposition, "
            "adds unsupported meaning, changes subject/object binding, polarity, speech-act "
            "scope, or temporal meaning, and whether it makes a genuine grounded improvement. "
            "An episode timestamp records when the utterance was captured; it is not evidence "
            "that an event occurred then. Unknown time is preferable to invented precision. "
            "Generic predicates may be less useful without being false. Residuals are structural "
            "diagnostics, never evidence. CANDIDATE_BETTER requires preserved source coverage, "
            "no unsupported candidate claim, and at least one material grounded improvement. "
            "Return exactly one native adjudicate-context-graph-candidate call.")},
            {"role": "user", "content": json.dumps({"case_id": case["case_id"],
                "source_packet": packet, "prior": prior, "candidate": candidate,
                "prior_residuals": prior_residuals,
                "candidate_residuals": candidate_residuals},
                ensure_ascii=False, separators=(",", ":"))}],
        "tools": [adjudication_tool()], "tool_choice": "required",
        "provider": {"sort": "price", "require_parameters": True,
            "data_collection": "deny", "zdr": True,
            "max_price": {"prompt": args.max_prompt_price,
                          "completion": args.max_completion_price}}}
    if args.openrouter_provider_only:
        request["provider"]["only"] = [args.openrouter_provider_only]
    if args.reasoning_policy == "low":
        request["reasoning"] = {"effort": "low", "exclude": True}
    return request


def validate_response(response, packet, candidate):
    if not isinstance(response, dict) or len(response.get("choices", [])) != 1:
        raise ValueError("adjudication requires exactly one choice")
    choice = response["choices"][0]
    if choice.get("finish_reason") == "length":
        raise ValueError("adjudication output is incomplete")
    calls = choice.get("message", {}).get("tool_calls", [])
    if (len(calls) != 1
            or calls[0].get("function", {}).get("name") !=
            "adjudicate-context-graph-candidate"):
        raise ValueError("adjudication requires one native tool call")
    payload = json.loads(calls[0]["function"]["arguments"])
    if set(payload) != {"schema_version", "verdict", "candidate_omissions",
                        "unsupported_candidate_claims", "supported_improvements",
                        "summary"}:
        raise ValueError("adjudication payload has unknown or missing keys")
    if (payload["schema_version"] != 1 or payload["verdict"] not in VERDICTS
            or not isinstance(payload["summary"], str)
            or not 1 <= len(payload["summary"]) <= 1200):
        raise ValueError("adjudication verdict or summary is invalid")
    sources = {row["source_id"]: row["text"] for row in packet["sources"]}
    omissions = payload["candidate_omissions"]
    if not isinstance(omissions, list) or len(omissions) > 12:
        raise ValueError("adjudication omissions are invalid")
    for row in omissions:
        if (not isinstance(row, dict)
                or set(row) != {"description", "source_id", "quote"}
                or row["source_id"] not in sources
                or not isinstance(row["description"], str)
                or not 1 <= len(row["description"]) <= 500
                or not isinstance(row["quote"], str) or not row["quote"]
                or row["quote"] not in sources[row["source_id"]]):
            raise ValueError("adjudication omission lacks exact source evidence")
    candidate_refs = {row["claim_ref"] for kind in ("entities", "facts")
                      for row in candidate[kind]}
    for key in ("unsupported_candidate_claims", "supported_improvements"):
        rows = payload[key]
        if not isinstance(rows, list) or len(rows) > 12:
            raise ValueError("adjudication claim collection is invalid")
        for row in rows:
            if (not isinstance(row, dict) or set(row) != {"claim_ref", "reason"}
                    or row["claim_ref"] not in candidate_refs
                    or not isinstance(row["reason"], str)
                    or not 1 <= len(row["reason"]) <= 500):
                raise ValueError("adjudication references an unknown candidate claim")
    return payload


def _semantic_signature(formation):
    def rows(kind):
        cleaned = []
        for row in formation[kind]:
            value = {key: item for key, item in row.items() if key != "claim_ref"}
            cleaned.append(json.dumps(value, sort_keys=True, ensure_ascii=False,
                                      separators=(",", ":")))
        return sorted(cleaned)
    return {"entities": rows("entities"), "facts": rows("facts")}


def deterministic_decision(prior, candidate, prior_residuals,
                           candidate_residuals, review):
    prior_repairs = sum(row["priority"] == "repair" for row in prior_residuals)
    candidate_repairs = sum(row["priority"] == "repair" for row in candidate_residuals)
    semantically_equivalent = (_semantic_signature(prior) ==
                               _semantic_signature(candidate))
    precheck = {"prior_repair_count": prior_repairs,
                "candidate_repair_count": candidate_repairs,
                "repair_regression": candidate_repairs > prior_repairs,
                "semantically_equivalent": semantically_equivalent}
    if precheck["repair_regression"] or review["verdict"] == "PRIOR_BETTER":
        return "KEEP_PRIOR", precheck
    if (semantically_equivalent
            and review["verdict"] == "EQUIVALENT"
            and not review["candidate_omissions"]
            and not review["unsupported_candidate_claims"]
            and candidate_repairs < prior_repairs):
        return "ACCEPT_CANDIDATE", precheck
    if (review["verdict"] == "CANDIDATE_BETTER"
            and not review["candidate_omissions"]
            and not review["unsupported_candidate_claims"]
            and candidate_repairs < prior_repairs
            and review["supported_improvements"]):
        return "ACCEPT_CANDIDATE", precheck
    return "INDETERMINATE", precheck


def _load_residuals(directory, case, packet, saved, ontology_sha):
    path = directory / "residuals.json"
    if path.is_file():
        rows = [row for row in json.loads(path.read_text(encoding="utf-8"))
                if row["source_episode_id"] == case["case_id"]]
    else:
        episode, _ = grounding.source_episode(case)
        rows = grounding.formation_residuals(
            episode, packet, saved["formation"],
            saved["outcome"].get("structural_relationship_rejections", []),
            saved["decisions"], ontology_sha)
    expected = {row["source_id"]: row["text_sha256"] for row in packet["sources"]}
    for row in rows:
        if (row.get("source_episode_id") != case["case_id"]
                or {item["source_id"]: item["text_sha256"]
                    for item in row.get("source_evidence", [])} != expected):
            raise ValueError("adjudication residual provenance is invalid")
    return rows


def run(args, repo, corpus):
    manifest_path = args.adjudication_manifest.resolve()
    manifest = json.loads(manifest_path.read_text(encoding="utf-8"))
    validate_manifest(manifest, corpus)
    cases = {row["case_id"]: row for row in corpus["cases"]}
    selected = base.load_selected_ontology(repo / "config/context-graph-upper-ontology-v1.2.json")
    ontology_sha = base.canonical_sha256(selected)
    directory = args.artifacts.resolve()
    directory.mkdir(parents=True, exist_ok=False)
    materials = []
    for row in manifest["cases"]:
        case = cases[row["case_id"]]
        _episode, packet = grounding.source_episode(case)
        prior_dir = _safe_artifact(repo, row["prior_artifacts"])
        candidate_dir = _safe_artifact(repo, row["candidate_artifacts"])
        prior_cases, prior_receipts = grounding.load_saved_cases([prior_dir])
        candidate_cases, candidate_receipts = grounding.load_saved_cases([candidate_dir])
        if row["case_id"] not in prior_cases or row["case_id"] not in candidate_cases:
            raise ValueError("adjudication case lacks an applied formation")
        prior, candidate = prior_cases[row["case_id"]], candidate_cases[row["case_id"]]
        materials.append({"manifest": row, "case": case, "packet": packet,
            "prior": compact_formation(prior["formation"], "prior"),
            "candidate": compact_formation(candidate["formation"], "candidate"),
            "prior_residuals": _load_residuals(
                prior_dir, case, packet, prior, ontology_sha),
            "candidate_residuals": _load_residuals(
                candidate_dir, case, packet, candidate, ontology_sha),
            "receipts": {"prior": prior_receipts, "candidate": candidate_receipts}})
    reuse_directory = (_safe_artifact(repo, args.reuse_adjudications_from)
                       if args.reuse_adjudications_from else None)
    seal = {"schema_version": 1, "revision": REVISION,
        "manifest_sha256": base.sha256_file(manifest_path),
        "corpus_sha256": base.canonical_sha256(corpus),
        "model": args.model, "provider": args.openrouter_provider_only,
        "reused_adjudications_from": (str(reuse_directory.relative_to(repo))
                                      if reuse_directory else None),
        "zdr": True, "data_collection": "deny", "materials": [
            {"case_id": row["case"]["case_id"], "receipts": row["receipts"]}
            for row in materials]}
    driver.save(directory / "seal.json", seal)
    if not args.execute:
        print("SEALED adjudication corpus; no provider requests", flush=True)
        return 0
    calls = driver.Calls(directory, args)
    driver.save(directory / "calls.json", calls.rows)
    outcomes = []
    reused_receipts = []
    for row in materials:
        case_id = row["case"]["case_id"]
        outcome = {"case_id": case_id}
        try:
            request = adjudication_request(
                row["case"], row["packet"], row["prior"], row["candidate"],
                row["prior_residuals"], row["candidate_residuals"], args)
            stage = case_id + "-adjudicate"
            if reuse_directory:
                response, receipt = grounding.reuse_received_response(
                    reuse_directory, stage, request)
                if response is None:
                    raise ValueError("adjudication receipt does not match sealed request")
                reused_receipts.append(receipt)
            else:
                response = calls.call(stage, request)
            review = validate_response(response, row["packet"], row["candidate"])
            decision, precheck = deterministic_decision(
                row["prior"], row["candidate"], row["prior_residuals"],
                row["candidate_residuals"], review)
            outcome.update(status="adjudicated", decision=decision,
                           expected_decision=row["manifest"]["expected_decision"],
                           matches_expectation=(decision == row["manifest"]["expected_decision"]),
                           deterministic_precheck=precheck, review=review)
        except Exception as error:
            outcome.update(status="deferred" if isinstance(error, driver.ProviderDeferred)
                           else "failed", error=str(error))
        outcomes.append(outcome)
        driver.save(directory / "outcomes.json", outcomes)
        print(case_id, outcome["status"], flush=True)
        if calls.poisoned:
            break
    matches = sum(row.get("matches_expectation") is True for row in outcomes)
    report = {"schema_version": 1, "cases": len(materials),
        "adjudicated": sum(row["status"] == "adjudicated" for row in outcomes),
        "expected_decisions_matched": matches,
        "provider_calls": len(calls.rows), "reserved_usd": calls.reserved,
        "reused_provider_receipts": len(reused_receipts),
        "charged_usd": sum(row.get("charged_usd", row["bound_usd"])
                           for row in calls.rows),
        "semantic_status": ("PASS" if matches == len(materials) else
                            "requires-review"),
        "live_writes": 0}
    if reused_receipts:
        driver.save(directory / "reused-adjudication-receipts.json", {
            "schema_version": 1,
            "source": str(reuse_directory.relative_to(repo)),
            "receipts": reused_receipts})
    driver.save(directory / "report.json", report)
    print(json.dumps(report), flush=True)
    return 0 if matches == len(materials) else 1
