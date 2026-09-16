"""Opt-in synthetic authority adapter. Lisp owns schemas, context and decisions.

Uses the existing lab transport/budget ledger, with no new retry mechanism.
Default invocation seals only; --execute remains an explicit paid-run action.
"""
from __future__ import annotations
import copy
import json
import os
from pathlib import Path
import time

import context_graph_lab as base


def validate_corpus(corpus):
    import context_graph_grounding_lab as grounding
    grounding.validate_corpus(corpus)
    if set(corpus) != {"source_kind", "cases", "queries"}:
        raise ValueError("authority corpus has unknown or missing fields")
    for case in corpus["cases"]:
        if set(case) != {"case_id", "timestamp", "sources"}:
            raise ValueError("authority episode has unknown fields")
        if type(case["timestamp"]) is not int or case["timestamp"] <= 0:
            raise ValueError("authority lab requires positive integer source timestamps")
        for source in case["sources"]:
            if source["speaker_id"] not in {"operator", "active-persona"}:
                raise ValueError("synthetic speaker must have an explicit lab role")
            expected = "original-utterance" if source["speaker_id"] == "operator" else "prior-agent-utterance"
            if source["kind"] != expected:
                raise ValueError("synthetic source kind conflicts with its role")
    queries = corpus["queries"]
    if not isinstance(queries, list) or not 1 <= len(queries) <= 32:
        raise ValueError("supply 1..32 held-out query expectations")
    for row in queries:
        if (not isinstance(row, dict) or set(row) != {"query", "expected_count", "expected_object_label"}
                or not isinstance(row["query"], str) or not 1 <= len(row["query"]) <= 1000
                or type(row["expected_count"]) is not int or not 0 <= row["expected_count"] <= 50
                or row["expected_object_label"] is not None and
                (not isinstance(row["expected_object_label"], str) or not 1 <= len(row["expected_object_label"]) <= 240)):
            raise ValueError("invalid held-out expectation")


def request_from_spec(spec, args):
    """Transport only: the production Lisp result supplies prompt and schema."""
    if set(spec) != {"adapter_revision", "tool_name", "schema", "system", "input"}:
        raise ValueError("invalid Lisp model specification")
    if spec["adapter_revision"] != "kg-authority-lab-model-v1":
        raise ValueError("unknown model adapter generation")
    if args.openrouter_zdr != "require" or args.openrouter_data_collection != "deny":
        raise ValueError("authority lab requires ZDR and denied data collection")
    request = {
        "model": args.model, "temperature": 0, "max_tokens": args.max_output_tokens,
        "messages": [{"role": "system", "content": spec["system"]},
                     {"role": "user", "content": json.dumps(spec["input"], ensure_ascii=False, separators=(",", ":"))}],
        "tools": [{"type": "function", "function": {
            "name": spec["tool_name"], "strict": True, "parameters": copy.deepcopy(spec["schema"]),
            "description": "Return the closed graph proposal or independent review requested by the runtime."}}],
        "tool_choice": "required",
        "provider": {"sort": "price", "require_parameters": True, "data_collection": "deny", "zdr": True,
                     "max_price": {"prompt": args.max_prompt_price, "completion": args.max_completion_price}},
    }
    if args.openrouter_provider_only:
        request["provider"].update(only=[args.openrouter_provider_only], allow_fallbacks=False)
    if args.reasoning_policy == "low":
        request["reasoning"] = {"effort": "low", "exclude": True}
    return request


def evaluate(expected, actual):
    rows = []
    for expectation, result in zip(expected, actual):
        hits = result["rows"]
        label = expectation["expected_object_label"]
        rows.append({"query": expectation["query"], "passed":
                     len(hits) == expectation["expected_count"] and result["scan_complete"] is True
                     and (label is None or bool(hits) and all(hit["object"]["label"] == label for hit in hits)),
                     "fact_ids": [hit["fact_id"] for hit in hits],
                     "object_entity_ids": [hit["object"]["entity_id"] for hit in hits]})
    return {"passed": len(actual) == len(expected) and all(row["passed"] for row in rows), "queries": rows}


def response_payload(response, tool_name):
    import context_graph_resolution_lab as driver
    choices = response.get("choices") if isinstance(response, dict) else None
    if not isinstance(choices, list) or len(choices) != 1 or not isinstance(choices[0], dict):
        raise ValueError("authority response requires one completed native call")
    choice, message = choices[0], choices[0].get("message")
    if (choice.get("finish_reason") not in {"stop", "tool_calls"} or not isinstance(message, dict)
            or message.get("refusal") or not isinstance(message.get("tool_calls"), list)
            or not message["tool_calls"]):
        raise ValueError("refused, incomplete or non-native authority response")
    calls = message["tool_calls"]
    first = calls[0]
    if len(calls) > 1:
        signature = (first.get("type"), first.get("function"))
        if any((call.get("type"), call.get("function")) != signature
               for call in calls[1:]):
            raise ValueError("authority response contains distinct competing calls")
        response = json.loads(json.dumps(response))
        response["choices"][0]["message"]["tool_calls"] = [
            response["choices"][0]["message"]["tool_calls"][0]
        ]
    return driver.structured_payload(response, tool_name)


def run(args, repo):
    import context_graph_resolution_lab as driver
    staged = getattr(args, "authority_staged", False)
    if staged and getattr(args, "authority_simple", False):
        raise ValueError("select simple or staged authority mode, not both")
    if args.authority_cases.stat().st_size > 200000:
        raise ValueError("authority corpus exceeds byte bound")
    corpus = json.loads(args.authority_cases.read_text(encoding="utf-8"))
    validate_corpus(corpus)
    if args.execute and args.authority_recorded_responses:
        raise ValueError("recorded responses and paid execution are mutually exclusive")
    if args.openrouter_zdr != "require" or args.transient_retries or args.protocol_retries:
        raise ValueError("authority lab requires ZDR and zero automatic retries")
    selected = base.load_selected_ontology(repo / "config/context-graph-upper-ontology-v1.2.json")
    directory = args.artifacts.resolve() / f"authority-{time.time_ns()}"
    directory.mkdir(parents=True, exist_ok=False)
    sources = ["scripts/context_graph_authority_lab.py", "scripts/context_graph_resolution_lab.py",
               "scripts/context_graph_lab.py", "scripts/context-graph-lab.lisp", "scripts/context-graph-authority-session.lisp",
               "scripts/conscious_q4_cli.py", "pai-context-graph.asd", "pai-memory-access.asd"]
    sources += [path.relative_to(repo).as_posix() for path in sorted((repo / "src/mind/knowledge/context-graph").glob("*.lisp"))]
    sources += [path.relative_to(repo).as_posix() for path in sorted((repo / "src/mind/knowledge/memory-access").glob("*.lisp"))]
    seal = {"schema_version": 1, "adapter_revision": "kg-authority-lab-model-v1", "corpus": corpus,
            "proposal_language": "staged-v1" if staged else "simple-v1" if getattr(args, "authority_simple", False) else "raw-v4",
            "corpus_sha256": base.canonical_sha256(corpus), "ontology_sha256": base.canonical_sha256(selected),
            "source_sha256": {name: base.sha256_file(repo / name) for name in sources},
            "stage_policy": driver.stage_policy(args), "cost_ceiling_usd": args.cost_ceiling_usd,
            "request_limit": args.request_limit, "max_output_tokens": args.max_output_tokens,
            "max_prompt_price": args.max_prompt_price, "max_completion_price": args.max_completion_price,
            "provider_timeout_seconds": args.provider_timeout_seconds, "zdr": True, "data_collection": "deny"}
    driver.save(directory / "seal.json", seal)
    recorded = None
    if args.authority_recorded_responses:
        if args.authority_recorded_responses.stat().st_size > 4000000:
            raise ValueError("recorded responses exceed byte bound")
        recorded = json.loads(args.authority_recorded_responses.read_text(encoding="utf-8"))
        if not isinstance(recorded, dict):
            raise ValueError("recorded response map must be an object")
        driver.save(directory / "recorded-response-manifest.json", {
            "sha256": base.sha256_file(args.authority_recorded_responses), "provider_calls": 0})
    if args.execute and not os.environ.get("OPENROUTER_API_KEY"):
        raise ValueError("approved execution needs OPENROUTER_API_KEY")
    calls = driver.Calls(directory, args) if args.execute else None
    history, outcomes = [], []
    bundle = {"schema_version": 2, "authority_operation": "staged-model-session" if staged else "simple-model-session" if getattr(args, "authority_simple", False) else "model-session", "source_kind": corpus["source_kind"],
              "ontology": base.runtime_ontology(selected), "ontology_revision": selected["ontology_revision"],
              "history": history, "step": None, "queries": [row["query"] for row in corpus["queries"]]}

    def lisp(name):
        if any(base.sha256_file(repo / name) != digest for name, digest in seal["source_sha256"].items()):
            raise ValueError("Lisp/adapter source changed after the run was sealed")
        target = directory / name
        target.mkdir()
        driver.save(target / "bundle.json", bundle)
        return driver.run_lisp_bundle(repo, target)

    final = None
    try:
        for number, episode in enumerate(corpus["cases"], 1):
            prefix = f"episode-{number:02d}"
            step = {"episode": episode, "proposal": None, "review": None, "request_digest": None}
            if staged:
                step.update(entity_selection=None, entity_request_digest=None, fact_request_digest=None)
            bundle["step"] = step
            stages = ([("entities", "entity_selection"), ("facts", "proposal")] if staged else [("extraction", "proposal")]) + [("review", "review")]
            for stage, field in stages:
                built = lisp(f"{prefix}-{stage}-input")
                if built["status"] != "accepted":
                    raise ValueError(f"{stage} stopped by Lisp preflight: {built['diagnostics']}")
                spec = built["value"]
                request = request_from_spec(spec, driver.stage_args(args, "extraction" if stage in {"entities", "facts"} else stage))
                driver.save(directory / f"{prefix}-{stage}-request.json", request)
                if not args.execute and recorded is None:
                    print(f"SEALED {directory}; no provider requests", flush=True)
                    return 0
                key = f"{prefix}-{stage}"
                if recorded is not None:
                    if key not in recorded:
                        raise ValueError(f"missing recorded response: {key}; no provider fallback")
                    response = recorded[key]
                    driver.save(directory / f"{key}-recorded-response.json", response)
                else:
                    if any(base.sha256_file(repo / name) != digest for name, digest in seal["source_sha256"].items()):
                        raise ValueError("adapter source changed before dispatch")
                    response = calls.call(key, request)
                step[field] = response_payload(response, spec["tool_name"])
                if stage == "entities": step["entity_request_digest"] = built["request_digest"]
                if stage == "facts": step["fact_request_digest"] = built["request_digest"]
                if stage == "review":
                    step["request_digest"] = built["request_digest"]
            final = lisp(f"{prefix}-application")
            outcomes.append({"case_id": episode["case_id"], "status": final["status"],
                             "value": final["value"], "diagnostics": final["diagnostics"]})
            history.append(copy.deepcopy(step))
            driver.save(directory / "history.json", history)
        report = {"status": "completed", "outcomes": outcomes, "retrieval": evaluate(corpus["queries"], final["queries"]),
                  "provider_calls": len(calls.rows) if calls else 0, "reserved_usd": calls.reserved if calls else 0,
                  "live_writes": 0, "semantic_evidence": "fresh-provider" if args.execute else "supplied-responses"}
        driver.save(directory / "report.json", report)
        print(f"AUTHORITY-LAB {directory} retrieval_passed={report['retrieval']['passed']}", flush=True)
        return 0 if report["retrieval"]["passed"] else 1
    except Exception as error:
        driver.save(directory / "report.json", {"status": "stopped", "error": str(error), "outcomes": outcomes,
                    "provider_calls": len(calls.rows) if calls else 0, "reserved_usd": calls.reserved if calls else 0, "live_writes": 0})
        raise
