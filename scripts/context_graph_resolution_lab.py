#!/usr/bin/env python3
"""Sequential, disposable formation/resolution experiment; never a live writer."""
from __future__ import annotations

import argparse
import copy
import hashlib
import json
import math
import multiprocessing
import os
from pathlib import Path
import subprocess
import re
import random
import sqlite3
import time
from contextlib import closing
from email.utils import parsedate_to_datetime

import context_graph_lab as base
import context_graph_grounding_lab as grounding


PARTICIPANT_ROLES = ("operator", "active-persona")


def participant_descriptors(participants):
    """Translate sealed lab identities into the production normalizer contract."""
    rows = []
    for role in PARTICIPANT_ROLES:
        descriptor = (participants or {}).get(role)
        if not isinstance(descriptor, dict):
            continue
        name = descriptor.get("name")
        aliases = descriptor.get("aliases") or []
        if (not isinstance(name, str) or not name.strip()
                or not isinstance(aliases, list)
                or any(not isinstance(alias, str) or not alias.strip()
                       for alias in aliases)):
            raise ValueError("participant descriptor is invalid")
        rows.append({
            "role": role,
            "kind": "person" if role == "operator" else "agent",
            "label": name.strip(),
            "aliases": [alias.strip() for alias in aliases],
            "existing_node_id": descriptor.get("existing_node_id"),
        })
    return rows


def canonicalize_runtime_participants(repo, directory, proposal, participants):
    """Invoke the production-owned Lisp participant normalizer."""
    if not participants:
        return copy.deepcopy(proposal), []
    directory.mkdir(parents=True, exist_ok=True)
    save(directory / "bundle.json", {
        "participant_normalization": {
            "proposal": proposal,
            "descriptors": participant_descriptors(participants),
        },
    })
    result = run_lisp_bundle(repo, directory)
    return result["proposal"], result["repairs"]


def exact_source_packet(episode):
    payload = json.loads(episode["content"])
    sources = copy.deepcopy(payload["source_evidence"])
    for source in sources:
        source["text_sha256"] = hashlib.sha256(
            source["text"].encode("utf-8")
        ).hexdigest()
    return {"schema_version": 1, "sources": sources}


def reusable_extraction_review_rows(rows, episode_count, protocol_retries):
    """Select complete extraction/review batches, including paid schema retries."""
    selected, stages = [], set()
    pattern = re.compile(
        r"episode-(\d+)-(extract|review(?:-protocol-retry-(\d+))?)"
    )
    for row in rows:
        match = pattern.fullmatch(str(row.get("stage", "")))
        if not match:
            continue
        episode_number = int(match.group(1))
        retry_number = int(match.group(3)) if match.group(3) else None
        if (not 1 <= episode_number <= episode_count
                or retry_number is not None
                and not 1 <= retry_number <= protocol_retries
                or row.get("status") != "received"
                or row["stage"] in stages):
            raise ValueError("review reuse receipts are incomplete or invalid")
        stages.add(row["stage"])
        selected.append(copy.deepcopy(row))
    for episode_number in range(1, episode_count + 1):
        prefix = f"episode-{episode_number:02d}"
        if ({f"{prefix}-extract", f"{prefix}-review"} - stages):
            raise ValueError("review reuse receipts are incomplete or invalid")
        retries = [number for number in range(1, protocol_retries + 1)
                   if f"{prefix}-review-protocol-retry-{number}" in stages]
        if retries and retries != list(range(1, retries[-1] + 1)):
            raise ValueError("review reuse retry receipts are not contiguous")
    return selected


def stage_args(args, stage):
    """Return a request-only policy view for one semantic stage."""
    result = copy.copy(args)
    result.model = getattr(args, f"{stage}_model") or args.model
    configured = getattr(args, f"{stage}_provider_only")
    result.openrouter_provider_only = (configured if configured is not None
                                       else args.openrouter_provider_only)
    return result


def stage_policy(args):
    return {stage: {"model": stage_args(args, stage).model,
                    "provider_only": stage_args(args, stage).openrouter_provider_only}
            for stage in ("extraction", "review", "resolution")}


def save(path: Path, value) -> None:
    temporary = path.with_suffix(path.suffix + ".tmp")
    with temporary.open("w", encoding="utf-8") as stream:
        json.dump(value, stream, ensure_ascii=False, indent=2)
        stream.flush()
        os.fsync(stream.fileno())
    os.replace(temporary, path)


class ProviderDeferred(RuntimeError):
    """Transient work retained for a later explicitly resumed lab invocation."""


class BudgetDeferred(ProviderDeferred, ValueError):
    """No new attempt admitted; existing evidence stays available."""


def retry_delay(error, attempt):
    header = error.retry_after
    if header:
        try:
            delay = float(header)
        except ValueError:
            try:
                delay = parsedate_to_datetime(header).timestamp() - time.time()
            except (ValueError, TypeError, OverflowError):
                delay = float("nan")
        if math.isfinite(delay) and delay >= 0:
            return delay
    return (10, 30, 90)[min(attempt, 2)] + random.uniform(0, 2)


def _provider_worker(connection, request, api_key):
    try:
        connection.send((True, base.openrouter_call(request, api_key)))
    except base.OpenRouterHTTPError as error:
        connection.send((False, {"http_status": error.status, "detail": error.detail,
                                 "retry_after": error.retry_after}))
    except Exception as error:
        connection.send((False, {"error": str(error)}))
    finally:
        connection.close()


def provider_call_with_deadline(request, api_key, seconds):
    "One request in a killable worker; a timeout never authorizes a retry."
    context = multiprocessing.get_context("spawn")
    receiver, sender = context.Pipe(duplex=False)
    worker = context.Process(target=_provider_worker, args=(sender, request, api_key))
    worker.start()
    sender.close()
    try:
        if not receiver.poll(seconds):
            raise TimeoutError("provider wall-clock deadline exceeded; outcome unknown")
        ok, result = receiver.recv()
        if not ok:
            if "http_status" in result:
                raise base.OpenRouterHTTPError(result["http_status"], result["detail"], result["retry_after"])
            raise RuntimeError(result["error"])
        return result
    finally:
        receiver.close()
        if worker.is_alive(): worker.terminate()
        worker.join()


def strict_json_request(request):
    """Translate one strict native-tool contract into strict JSON Schema output."""
    result = copy.deepcopy(request)
    tools = result.pop("tools", None)
    result.pop("tool_choice", None)
    if not isinstance(tools, list) or len(tools) != 1:
        raise ValueError("structured retry requires exactly one tool contract")
    function = tools[0].get("function")
    if not isinstance(function, dict) or not isinstance(function.get("parameters"), dict):
        raise ValueError("structured retry tool contract is invalid")
    result["response_format"] = {
        "type": "json_schema",
        "json_schema": {
            "name": function["name"].replace("-", "_"),
            "strict": True,
            "schema": function["parameters"],
        },
    }
    return result


def structured_payload(response, native_name):
    """Read one native call or one strict-schema JSON object, never prose."""
    if not isinstance(response, dict) or len(response.get("choices", [])) != 1:
        raise ValueError("structured response requires exactly one choice")
    choice = response["choices"][0]
    if choice.get("finish_reason") == "length":
        raise ValueError("structured response is incomplete")
    message = choice.get("message")
    if not isinstance(message, dict):
        raise ValueError("structured response message is absent")
    calls = message.get("tool_calls")
    if isinstance(calls, list) and len(calls) == 1:
        function = calls[0].get("function")
        if not isinstance(function, dict) or function.get("name") != native_name:
            raise ValueError("structured response selected the wrong native tool")
        encoded = function.get("arguments")
    elif calls in (None, []):
        encoded = message.get("content")
    else:
        raise ValueError("structured response contains multiple native calls")
    if not isinstance(encoded, str):
        raise ValueError("structured response contains no encoded object")
    try:
        payload = json.loads(encoded)
    except json.JSONDecodeError as error:
        raise ValueError("structured response contains invalid JSON") from error
    if not isinstance(payload, dict):
        raise ValueError("structured response payload is not an object")
    return payload


def validated_native_call(calls, stage, request, validator, retries,
                          retry_request=None):
    """Retry a received but invalid native response under a fresh receipt."""
    last_error = None
    for attempt in range(retries + 1):
        receipt_stage = (stage if attempt == 0
                         else f"{stage}-protocol-retry-{attempt}")
        response = calls.call(
            receipt_stage, request if attempt == 0 else (retry_request or request),
        )
        try:
            return validator(response)
        except (TypeError, ValueError, KeyError, json.JSONDecodeError) as error:
            last_error = error
            if attempt == retries:
                raise
            time.sleep(min(2 ** attempt, 5))
    raise last_error


def resolution_request(episode, entities, candidate_sets, args):
    request = base.formation_request(episode, args)
    request["messages"] = [
        {"role": "system", "content": (
            "Resolve each extracted entity against ONLY its supplied existing candidates. "
            "Identity means the same real-world individual or concept, not merely related "
            "or similarly named. Use episode context and candidate relationships. "
            "LINK_EXISTING only when the evidence supports identity; copy the exact ID. "
            "When the source directly identifies an entity and no supplied candidate is that "
            "same identity, choose NEW—even when unrelated candidates exist or the candidate "
            "set is empty. NEW means a distinct entity, not an alias. Use UNRESOLVED only "
            "when the source identity itself is ambiguous, including a likely existing "
            "referent missing from a truncated candidate set. "
            "Same-name people may be different. A discussion of retrieval failure is not "
            "the underlying health condition. Do not manufacture missing personal facts. "
            "Return exactly one decision for every supplied entity; omission is invalid. "
            "Provide a short evidence-based reason for every decision. Call resolve-entities once."
        )},
        {"role": "user", "content": json.dumps({
            "episode": episode, "entities": entities, "candidate_sets": candidate_sets,
        }, ensure_ascii=False)},
    ]
    request["tools"] = [{"type": "function", "function": {
        "name": "resolve-entities", "description": "Propose identity decisions.",
        "parameters": {"type": "object", "additionalProperties": False,
            "properties": {"decisions": {"type": "array",
                "minItems": len(entities), "maxItems": len(entities),
                "items": {"type": "object", "additionalProperties": False,
                    "properties": {
                        "local_ref": {"type": "string"},
                        "action": {"type": "string", "enum": ["NEW", "LINK_EXISTING", "UNRESOLVED"]},
                        "existing_id": {"anyOf": [{"type": "string"}, {"type": "null"}]},
                        "reason": {"type": "string", "maxLength": 600}},
                    "required": ["local_ref", "action", "existing_id", "reason"]}}},
            "required": ["decisions"]}}}]
    return request


def validate_resolution(response, entities, candidate_sets):
    payload = structured_payload(response, "resolve-entities")
    if set(payload) != {"decisions"} or not isinstance(payload["decisions"], list):
        raise ValueError("invalid resolution object")
    expected = {e["local_ref"]: e for e in entities}
    candidates = {c["local_ref"]: {r["entity_id"]: r for r in c["candidates"]}
                  for c in candidate_sets}
    decisions = {}
    for row in payload["decisions"]:
        if set(row) != {"local_ref", "action", "existing_id", "reason"}:
            raise ValueError("invalid resolution fields")
        ref, action, target = row["local_ref"], row["action"], row["existing_id"]
        if ref not in expected or ref in decisions or action not in {"NEW", "LINK_EXISTING", "UNRESOLVED"}:
            raise ValueError("invalid or repeated resolution")
        if not isinstance(row["reason"], str) or not 1 <= len(row["reason"]) <= 600:
            raise ValueError("resolution reason missing or oversized")
        if action == "LINK_EXISTING":
            candidate = candidates.get(ref, {}).get(target)
            if not candidate or candidate["type"] != expected[ref]["type"]:
                raise ValueError("resolution target absent or type-incompatible")
        elif target is not None:
            raise ValueError("non-link resolution must have null target")
        decisions[ref] = row
    if set(decisions) != set(expected):
        raise ValueError("resolution did not cover every extracted entity")
    return decisions


def apply_decisions(formation, decisions):
    result = copy.deepcopy(formation)
    entities = result["proposal"]["entities"]
    unresolved = {ref for ref, row in decisions.items() if row["action"] == "UNRESOLVED"}
    result["proposal"]["entities"] = [e for e in entities if e["local_ref"] not in unresolved]
    for entity in result["proposal"]["entities"]:
        row = decisions[entity["local_ref"]]
        entity["action"], entity["existing_id"] = row["action"], row["existing_id"]
    result["proposal"]["facts"] = [f for f in result["proposal"]["facts"]
        if f["subject_ref"] not in unresolved and f["object_ref"] not in unresolved]
    return result


def exact_utterance_episodes(snapshot, selected_events, supplemental_memory_ids=None):
    """Hydrate episode utterances plus explicitly selected grounded memory sources."""
    with closing(sqlite3.connect(f"file:{Path(snapshot).as_posix()}?mode=ro", uri=True)) as database:
        rows = database.execute(
            "SELECT event_json FROM pai_events ORDER BY storage_sequence"
        ).fetchall()
    by_id = {}
    for (encoded,) in rows:
        event = json.loads(encoded)
        if isinstance(event.get("id"), int):
            by_id[event["id"]] = event
    episodes = []
    for sealed in selected_events:
        payload = sealed["payload"]
        evidence = []
        for event_id in payload.get("source_event_ids", []):
            event = by_id.get(event_id)
            if not event or event.get("type") not in {"user-message", "agent-message"}:
                continue
            body = event.get("payload") or {}
            text = body.get("text")
            if not isinstance(text, str) or not text:
                continue
            evidence.append({
                "source_id": f"event:{event_id}",
                "speaker_id": "operator" if event["type"] == "user-message"
                              else payload["persona_id"],
                "kind": "original-utterance" if event["type"] == "user-message"
                        else "prior-agent-utterance",
                "timestamp": event.get("timestamp"),
                "text": text,
            })
        supplemental = (supplemental_memory_ids or {}).get(str(sealed["id"]), [])
        if (not isinstance(supplemental, list) or len(supplemental) > 8
                or len(set(supplemental)) != len(supplemental)
                or any(not isinstance(event_id, int) for event_id in supplemental)):
            raise ValueError("supplemental memory selection is invalid")
        for event_id in supplemental:
            event = by_id.get(event_id)
            if not event or event.get("type") != "memory-baseline-node":
                raise ValueError("supplemental source is not a baseline memory node")
            node = (event.get("payload") or {}).get("node") or {}
            encoded = node.get("scalar_json")
            try:
                memory = json.loads(encoded)
            except (TypeError, json.JSONDecodeError) as error:
                raise ValueError("supplemental memory scalar is invalid") from error
            metadata = memory.get("epistemic_metadata") or {}
            if (memory.get("grounding_status") != "grounded"
                    or memory.get("origin_class") not in {"lived-user", "lived-agent-action"}
                    or metadata.get("role") not in {"user", "assistant"}
                    or not isinstance(memory.get("content"), str)
                    or not memory["content"].strip()):
                raise ValueError("supplemental memory lacks grounded lived evidence")
            evidence.append({
                "source_id": f"memory-event:{event_id}",
                "speaker_id": ("operator" if metadata["role"] == "user"
                               else payload["persona_id"]),
                "kind": ("original-utterance" if metadata["role"] == "user"
                         else "prior-agent-utterance"),
                "timestamp": memory.get("created_at"),
                "text": memory["content"],
            })
        if not evidence:
            raise ValueError(f"episode {payload['episode_id']} has no exact utterances")
        episodes.append({
            "episode_id": payload["episode_id"],
            "occurred_at": sealed.get("occurred_at")
                           or str(payload.get("sealed_at", "unknown")),
            "content": json.dumps({
                "episode_id": payload["episode_id"],
                "persona_id": payload["persona_id"],
                "source_evidence": evidence,
            }, ensure_ascii=False),
        })
    return episodes


def run_lisp_bundle(repo, directory):
    from conscious_q4_cli import local_sbcl, native_environment, quicklisp_setup
    sbcl = local_sbcl(repo)
    env = native_environment(sbcl)
    cache = Path(os.environ.get("PAI_LAB_CACHE_DIR", repo / ".clone-state" / "host-cache"))
    cache.mkdir(parents=True, exist_ok=True)
    env.update(LOCALAPPDATA=str(cache), XDG_CACHE_HOME=str(cache),
               PAI_CONTEXT_GRAPH_ASD=(repo / "pai-context-graph.asd").as_posix(),
               PAI_CONTEXT_GRAPH_BUNDLE=str(directory / "bundle.json"),
               PAI_CONTEXT_GRAPH_OUTPUT=str(directory / "result.json"),
               PAI_QUICKLISP_SETUP=str(quicklisp_setup(repo)))
    run = subprocess.run([str(sbcl), "--script",
                          str(repo / "scripts/context-graph-lab.lisp")],
                         env=env, capture_output=True, text=True, timeout=120)
    (directory / "lisp.log").write_text(run.stdout + run.stderr, encoding="utf-8")
    if run.returncode:
        raise ValueError("Lisp replay failed; inspect private lisp.log")
    return json.loads((directory / "result.json").read_text(encoding="utf-8"))


def run_graph(repo, directory, selected, formations, queries, entities=(), explicit=True):
    directory.mkdir(parents=True, exist_ok=True)
    graph_formations = copy.deepcopy(formations)
    for formation in graph_formations:
        episode = formation["episode"]
        episode.setdefault("learned_at", episode["occurred_at"])
        if len(episode["content"]) > 12000:
            episode["content"] = json.dumps({
                "evidence_source": "exact-utterances-only",
                "sealed_content_sha256": hashlib.sha256(
                    episode["content"].encode("utf-8")
                ).hexdigest(),
                "sealed_content_path": "../seal.json",
            }, sort_keys=True)
    bundle = {"ontology": base.runtime_ontology(selected), "formations": graph_formations,
              "queries": queries, "evidence_policy": "verified",
              "identity_policy": "explicit" if explicit else "canonical",
              "candidate_entities": list(entities), "source": {"kind": "isolated-resolution-lab"}}
    save(directory / "bundle.json", bundle)
    return run_lisp_bundle(repo, directory)


class Calls:
    def __init__(self, directory, args, prior_rows=(), resume_directory=None):
        self.directory, self.args, self.rows = directory, args, copy.deepcopy(list(prior_rows))
        self.resume_directory = resume_directory
        self.prior_count = len(self.rows)
        self.reserved = sum(r["bound_usd"] for r in self.rows)
        self.poisoned = any(r.get("status") in {"failed", "admitted"} or
                            r.get("charged_usd", r["bound_usd"]) > r["bound_usd"] for r in self.rows)

    @property
    def budget_used_usd(self):
        """Settled receipt charges plus full bounds for unsettled/unknown calls.

        Keep reserved as a gross audit total, never as accumulated spending.
        Legacy receipts may settle only after their stored response is verified.
        Missing usage or missing receipts retain the entire admitted bound.
        """
        total = 0.0
        for row in self.rows:
            amount = row["bound_usd"]
            if (not isinstance(amount, (float, int)) or isinstance(amount, bool)
                    or not math.isfinite(amount) or amount < 0):
                raise ValueError("invalid admitted cost bound")
            path, digest = row.get("response_path"), row.get("response_sha256")
            if row.get("status") == "received" and path and digest:
                response = json.loads(Path(path).read_text(encoding="utf-8"))
                if base.canonical_sha256(response) != digest:
                    raise ValueError("settled response receipt changed")
                cost = response.get("usage", {}).get("cost")
                if (isinstance(cost, (float, int)) and not isinstance(cost, bool)
                        and math.isfinite(cost) and cost >= 0):
                    if cost != row.get("charged_usd") or cost > row["bound_usd"]:
                        raise ValueError("settled receipt cost differs from ledger or bound")
                    amount = cost
            total += amount
        return total

    def call(self, stage, request):
        matching = [(i, r) for i, r in enumerate(self.rows) if r.get("stage") == stage]
        if matching:
            if not self.resume_directory or any(i >= self.prior_count for i, _ in matching):
                raise ValueError("a lab stage may not be retried outside its bounded attempt batch")
            if any(r.get("request_sha256") != base.canonical_sha256(request) for _, r in matching):
                raise ValueError("resumed stage request differs from its sealed receipt")
            i, last = matching[-1]
            if last["status"] == "received":
                path = Path(last.get("response_path") or
                            Path(last.get("reused_from", self.resume_directory)) / f"response-{i+1:02d}-{stage}.json")
                response = json.loads(path.read_text(encoding="utf-8"))
                if last.get("response_sha256") and last["response_sha256"] != base.canonical_sha256(response):
                    raise ValueError("resumed response receipt changed")
                return response
            if last["status"] not in {"transient-failed", "invalid-response"}:
                raise ValueError("only explicit failed calls can be resumed")
            if (last["status"] == "transient-failed"
                    and last.get("not_before", 0) > time.time()):
                raise ProviderDeferred("provider retry time has not arrived")
        if self.poisoned:
            raise ValueError("lab accounting is poisoned")
        for attempt in range(getattr(self.args, "transient_retries", 0) + 1):
            try:
                return self._attempt(stage, request)
            except base.OpenRouterHTTPError as error:
                if error.status not in {429, 502, 503, 504}:
                    raise
                delay = retry_delay(error, attempt)
                self.rows[-1]["not_before"] = time.time() + delay
                save(self.directory / "calls.json", self.rows)
                if attempt >= getattr(self.args, "transient_retries", 0) or delay > 60:
                    raise ProviderDeferred("transient provider failure; request retained for later resumption") from error
                time.sleep(delay)

    def _attempt(self, stage, request):
        if request.get("max_tokens") != self.args.max_output_tokens:
            raise ValueError("request output bound differs from admitted policy")
        bound = base.request_cost_bound(request, self.args)
        if len(json.dumps(request).encode()) > 160000:
            raise ValueError("request exceeds lab byte ceiling")
        if len(self.rows) >= getattr(self.args, "request_limit", 18) or self.budget_used_usd + bound > self.args.cost_ceiling_usd:
            raise BudgetDeferred("lab budget exhausted before dispatch")
        index = len(self.rows) + 1
        save(self.directory / f"request-{index:02d}-{stage}.json", request)
        # Preserve gross reservations for audit; only outstanding exposure uses them.
        self.reserved += bound
        row = {"stage": stage, "bound_usd": bound, "status": "admitted",
               "request_sha256": base.canonical_sha256(request)}
        self.rows.append(row)
        save(self.directory / "calls.json", self.rows)
        try:
            deadline = getattr(self.args, "provider_timeout_seconds", None)
            response = (provider_call_with_deadline(request, os.environ["OPENROUTER_API_KEY"], deadline)
                        if deadline else base.openrouter_call(request, os.environ["OPENROUTER_API_KEY"]))
            save(self.directory / f"response-{index:02d}-{stage}.json", response)
            row["status"] = "received"
            row["response_path"] = str(self.directory / f"response-{index:02d}-{stage}.json")
            row["response_sha256"] = base.canonical_sha256(response)
            cost = response.get("usage", {}).get("cost")
            row["charged_usd"] = (cost if isinstance(cost, (float, int)) and
                                   not isinstance(cost, bool) and math.isfinite(cost) and cost >= 0 else bound)
            if row["charged_usd"] > bound:
                self.poisoned = True
                raise ValueError("provider cost exceeded the admitted bound")
            return response
        except base.OpenRouterHTTPError as error:
            row.update(status="transient-failed" if error.status in {429, 502, 503, 504} else "http-rejected",
                       http_status=error.status, retry_after=error.retry_after, error=str(error), charged_usd=bound)
            save(self.directory / f"error-{index:02d}-{stage}.json", {
                "http_status": error.status, "retry_after": error.retry_after, "body": error.detail})
            raise
        except Exception as error:
            self.poisoned = True
            row.update(status="failed", error=str(error))
            row.setdefault("charged_usd", bound)
            raise
        finally:
            save(self.directory / "calls.json", self.rows)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--event-db", type=Path)
    parser.add_argument("--authority-cases", type=Path, help="Opt-in synthetic sequential authority extraction/review lab")
    parser.add_argument("--authority-simple", action="store_true", help="Use the opt-in simple proposal language with production Lisp expansion")
    parser.add_argument("--authority-staged", action="store_true", help="Separate entity and typed-fact extraction before independent review")
    parser.add_argument("--authority-recorded-responses", type=Path, help="Authority mode only: offline stage response map; never falls back to a provider")
    parser.add_argument("--grounding-cases", type=Path, help="Controlled synthetic source-grounding corpus")
    parser.add_argument("--grounding-case-id", action="append",
                        help="Controlled mode only: run one named case; repeat for a bounded subset")
    parser.add_argument("--grounding-provider", choices=("deepinfra", "automatic"), default="deepinfra",
                        help="controlled corpus routing; automatic removes provider pin but retains privacy and price gates")
    parser.add_argument("--continue-unattempted-from", type=Path,
                        help="Controlled mode only: skip all previously attempted cases and retain their accounting")
    parser.add_argument("--residuals-from", type=Path, action="append",
                        help="Controlled mode only: offline residual replay from saved artifact receipts")
    parser.add_argument("--residual-guidance-from", type=Path, action="append",
                        help="Controlled mode only: use repair residuals from saved formation receipts")
    parser.add_argument("--renormalize-from", type=Path, action="append",
                        help="Controlled mode only: rebuild from saved reviewed proposals without a provider")
    parser.add_argument("--adjudication-manifest", type=Path,
                        help="Controlled mode only: compare prior and candidate formations")
    parser.add_argument("--reuse-adjudications-from", type=Path,
                        help="Adjudication only: replay exact verified response receipts")
    parser.add_argument("--cases", type=Path,
                        help="Private fixed JSON with sealed event IDs and held-out queries")
    parser.add_argument("--artifacts", type=Path, required=True)
    parser.add_argument("--model", default="openai/gpt-oss-120b")
    for stage in ("extraction", "review", "resolution"):
        parser.add_argument(f"--{stage}-model")
        parser.add_argument(f"--{stage}-provider-only")
    parser.add_argument("--cost-ceiling-usd", type=float, default=0.20)
    parser.add_argument("--request-limit", type=int, default=18)
    parser.add_argument("--transient-retries", type=int, choices=range(3), default=0,
                        help="additional attempts per stage for HTTP 429/502/503/504 (0..2)")
    parser.add_argument("--protocol-retries", type=int, choices=range(3), default=0,
                        help="additional fresh receipts for invalid native responses (0..2)")
    parser.add_argument("--resume-from", type=Path,
                        help="Replay verified receipts and resume transient-deferred stages in a new artifact directory")
    parser.add_argument("--provider-timeout-seconds", type=float, default=180,
                        help="whole provider-call deadline; timeout retains reservation and never retries")
    parser.add_argument("--execute", action="store_true")
    parser.add_argument("--reuse-extractions", type=Path,
                        help="Continue an extraction-only run; never resend its paid requests")
    parser.add_argument("--reuse-reviews", type=Path,
                        help="Reuse verified extraction and review receipts; recompute state-dependent resolutions")
    parser.add_argument("--openrouter-zdr", choices=("require", "allow-non-zdr"), default="require")
    args = parser.parse_args()
    if not math.isfinite(args.provider_timeout_seconds) or args.provider_timeout_seconds <= 0:
        parser.error("provider timeout must be finite and positive")
    if not 1 <= args.request_limit <= 32:
        parser.error("request-limit must be between 1 and 32 for this lab")
    if not math.isfinite(args.cost_ceiling_usd) or not 0 < args.cost_ceiling_usd <= 0.50:
        parser.error("initial lab ceiling must be positive and at most $0.50")
    args.max_prompt_price, args.max_completion_price = 0.20, 0.40
    args.max_output_tokens, args.review_output_tokens = 8192, 8192
    args.openrouter_data_collection = "deny"
    args.openrouter_provider_only = ("deepinfra" if args.grounding_cases and args.grounding_provider == "deepinfra" else None)
    args.reasoning_policy = "low"
    for stage, policy in stage_policy(args).items():
        if (not policy["model"] or len(policy["model"]) > 200
                or not base.MODEL_RE.fullmatch(policy["model"])):
            parser.error(f"{stage} model is not a bounded author/model slug")
        provider = policy["provider_only"]
        if provider and (len(provider) > 80 or not re.fullmatch(r"[A-Za-z0-9 ._-]+", provider)):
            parser.error(f"{stage} provider name is invalid")
    repo = Path(__file__).resolve().parents[1]
    if args.authority_cases:
        if any((args.event_db, args.cases, args.grounding_cases, args.grounding_case_id,
                args.continue_unattempted_from, args.residuals_from, args.residual_guidance_from,
                args.renormalize_from, args.adjudication_manifest, args.reuse_adjudications_from,
                args.resume_from, args.reuse_extractions, args.reuse_reviews)):
            parser.error("authority lab cannot combine with legacy corpus, database or receipt modes")
        import context_graph_authority_lab
        return context_graph_authority_lab.run(args, repo)
    if args.authority_recorded_responses or args.authority_simple or args.authority_staged:
        parser.error("authority recorded responses and simple mode require --authority-cases")
    if args.grounding_cases:
        if args.resume_from or any(getattr(args, f"{s}_{field}") for s in ("extraction", "review", "resolution")
                                   for field in ("model", "provider_only")):
            parser.error("stage routing and receipt resume are currently real-corpus mode only")
        if args.event_db or args.cases:
            parser.error("controlled grounding mode cannot open a database or private corpus")
        if args.residuals_from and args.residual_guidance_from:
            parser.error("offline residual replay and guided formation are mutually exclusive")
        if args.renormalize_from and (args.residuals_from or
                                      args.residual_guidance_from):
            parser.error("normalization replay cannot combine with residual modes")
        if args.adjudication_manifest and any((args.residuals_from,
                                               args.residual_guidance_from,
                                               args.renormalize_from,
                                               args.reuse_extractions,
                                               args.continue_unattempted_from)):
            parser.error("adjudication cannot combine with formation replay modes")
        if args.reuse_adjudications_from and not args.adjudication_manifest:
            parser.error("adjudication receipt replay requires adjudication mode")
        if args.residual_guidance_from and (args.reuse_extractions or
                                            args.continue_unattempted_from):
            parser.error("guided formation cannot reuse or continue prior requests")
        if args.adjudication_manifest:
            import context_graph_adjudication_lab
            import context_graph_grounding_lab
            corpus = json.loads(args.grounding_cases.read_text(encoding="utf-8"))
            context_graph_grounding_lab.validate_corpus(corpus)
            return context_graph_adjudication_lab.run(args, repo, corpus)
        return context_graph_grounding_lab.run(args, repo)
    if not args.event_db or not args.cases:
        parser.error("event-db and cases are required outside controlled grounding mode")
    if args.residuals_from:
        parser.error("residual replay is only available in controlled grounding mode")
    if args.residual_guidance_from:
        parser.error("residual guidance is only available in controlled grounding mode")
    if args.renormalize_from:
        parser.error("normalization replay is only available in controlled grounding mode")
    if args.adjudication_manifest:
        parser.error("adjudication is only available in controlled grounding mode")
    if args.reuse_adjudications_from:
        parser.error("adjudication receipt replay is only available in adjudication mode")
    if args.continue_unattempted_from:
        parser.error("unattempted continuation is only available in controlled mode")
    if sum(value is not None for value in
           (args.resume_from, args.reuse_extractions, args.reuse_reviews)) > 1:
        parser.error("resume-from, reuse-extractions, and reuse-reviews are mutually exclusive")
    selected = base.load_selected_ontology(repo / "config/context-graph-upper-ontology-v1.2.json")
    directory = args.artifacts.resolve()
    directory.mkdir(parents=True, exist_ok=False)
    snapshot = directory / "events-snapshot.sqlite3"
    base.snapshot_database(args.event_db.resolve(), snapshot)
    before = base.sha256_file(snapshot)
    cases = json.loads(args.cases.read_text(encoding="utf-8"))
    ids = cases["sealed_event_ids"]
    if not 1 <= len(ids) <= 6 or len(set(ids)) != len(ids):
        raise ValueError("select 1-6 distinct sealed episode events")
    events = [e for e in base.load_events(snapshot, cases.get("agent_id")) if e["id"] in ids]
    if len(events) != len(ids) or any(e["type"] != "conversation-episode-sealed" for e in events):
        raise ValueError("case event missing or not a sealed episode")
    episodes = exact_utterance_episodes(
        snapshot, events, cases.get("supplemental_memory_event_ids"),
    )
    if len({ep["episode_id"] for ep in episodes}) != len(episodes):
        raise ValueError("case corpus repeats an episode")
    save(directory / "seal.json", {"model": args.model, "stage_policy": stage_policy(args), "snapshot_sha256": before,
        "cases": cases, "episodes": episodes, "evidence_source": "exact-utterances-only",
        "cost_ceiling_usd": args.cost_ceiling_usd,
        "maximum_calls": args.request_limit, "provider_zdr": args.openrouter_zdr, "data_collection": "deny",
        "transient_retries": args.transient_retries,
        "protocol_retries": args.protocol_retries,
        "provider_timeout_seconds": args.provider_timeout_seconds})
    if not args.execute:
        print("SEALED", len(episodes), "episodes; no provider calls", flush=True)
        return 0
    prior_rows = []
    resume = args.resume_from.resolve() if args.resume_from else None
    if resume:
        previous = json.loads((resume / "seal.json").read_text(encoding="utf-8"))
        if (previous["episodes"] != episodes or previous["cases"] != cases
                or previous.get("stage_policy") != stage_policy(args)
                or previous["provider_zdr"] != args.openrouter_zdr
                or previous["data_collection"] != "deny"
                or previous.get("protocol_retries", 0) != args.protocol_retries):
            raise ValueError("resume requires the identical corpus and request policy")
        prior_rows = json.loads((resume / "calls.json").read_text(encoding="utf-8"))
        save(directory / "resumed-from.json", {"directory": str(resume), "prior_calls": len(prior_rows)})
    reuse = args.reuse_extractions.resolve() if args.reuse_extractions else None
    if reuse:
        prior_seal = json.loads((reuse / "seal.json").read_text(encoding="utf-8"))
        all_prior_rows = json.loads((reuse / "calls.json").read_text(encoding="utf-8"))
        prior_rows = [row for row in all_prior_rows
                      if row.get("stage", "").endswith("-extract")]
        extraction = stage_args(args, "extraction")
        prior_extraction = prior_seal.get("stage_policy", {}).get("extraction", {
            "model": prior_seal["model"], "provider_only": None})
        if (prior_extraction != {"model": extraction.model,
                                 "provider_only": extraction.openrouter_provider_only}
                or prior_seal["episodes"] != episodes
                or prior_seal["provider_zdr"] != args.openrouter_zdr
                or len(prior_rows) != len(episodes)
                or any(r["stage"] != f"episode-{i+1:02d}-extract" or r["status"] != "received"
                       for i, r in enumerate(prior_rows))):
            raise ValueError("reuse requires the identical, completed extraction-only corpus")
        for row in prior_rows:
            row["reused_from"] = str(reuse)
    reuse_reviews = args.reuse_reviews.resolve() if args.reuse_reviews else None
    if reuse_reviews:
        prior_seal = json.loads((reuse_reviews / "seal.json").read_text(encoding="utf-8"))
        all_prior_rows = json.loads((reuse_reviews / "calls.json").read_text(encoding="utf-8"))
        prior_rows = reusable_extraction_review_rows(
            all_prior_rows, len(episodes), args.protocol_retries,
        )
        current_policy = stage_policy(args)
        prior_policy = prior_seal.get("stage_policy", {})
        if (prior_seal["episodes"] != episodes
                or prior_seal["provider_zdr"] != args.openrouter_zdr
                or prior_seal.get("protocol_retries", 0) != args.protocol_retries
                or any(prior_policy.get(stage) != current_policy[stage]
                       for stage in ("extraction", "review"))):
            raise ValueError("review reuse requires the identical, completed extraction/review corpus")
        for row in prior_rows:
            row["reused_from"] = str(reuse_reviews)
    calls = Calls(directory, args, prior_rows,
                  resume_directory=resume or reuse_reviews)
    save(directory / "calls.json", calls.rows)
    formations, baseline, outcomes = [], [], []
    queries = cases["queries"]
    for index, episode in enumerate(episodes):
        stage = f"episode-{index+1:02d}"
        outcome = {"episode_id": episode["episode_id"]}
        try:
            packet = exact_source_packet(episode)
            extraction_args = stage_args(args, "extraction")
            review_args = stage_args(args, "review")
            resolution_args = stage_args(args, "resolution")
            request = grounding.formation_request(
                episode, packet, extraction_args, selected,
            )
            request["messages"][0]["content"] += (
                " Separate source-discussion from the things it discusses. A retrieval failure "
                "is not a condition. Preserve who reported a claim; do not turn an assistant's "
                "speculation into an operator fact. Return atomic relationships, not narrative nodes. "
                "The speaker_id operator is one stable participant: classify only the entity "
                "representing that speaker as operator. The configured speaking persona is one "
                "stable participant: classify only its entity as active-persona. Do not infer "
                "either role from a merely equal label."
            )
            if reuse or reuse_reviews:
                row = next(row for row in prior_rows
                           if row["stage"] == stage + "-extract")
                if row["request_sha256"] != base.canonical_sha256(request):
                    raise ValueError("saved extraction request differs from current policy")
                response_path = Path(row.get("response_path", ""))
                if not response_path.is_absolute():
                    response_path = (reuse or reuse_reviews) / response_path
                response = json.loads(response_path.read_text(encoding="utf-8"))
            else:
                response = calls.call(stage + "-extract", request)
            if response["choices"][0].get("finish_reason") == "length":
                raise ValueError("extraction output incomplete")
            proposal = base.extract_proposal(response)
            proposal, participant_repairs = canonicalize_runtime_participants(
                repo, directory / (stage + "-participants-extract"),
                proposal, cases.get("participants"),
            )
            outcome["participant_repairs"] = participant_repairs
            proposal, direction_repairs = base.repair_reversed_typed_relationships(
                proposal, selected,
            )
            outcome["structural_relationship_repairs"] = direction_repairs
            rejected_shapes = grounding.validate_proposal(
                proposal, packet, selected,
            )
            outcome["structural_relationship_rejections"] = rejected_shapes
            proposal = base.proposal_without_rejected_relationships(proposal, rejected_shapes)
            if grounding.validate_proposal(proposal, packet, selected):
                raise ValueError("grounded proposal remains structurally invalid")
            review_request = grounding.review_request(
                episode, packet, proposal, review_args,
            )
            review_request["messages"][0]["content"] += (
                " A prior-agent-utterance directly establishes only the active persona's "
                "own statement, action, intention, or experience. It never directly establishes "
                "an unconfirmed external or operator-personal proposition embedded in that "
                "statement. An original-utterance from speaker_id operator may directly establish "
                "the operator-personal proposition it states or corrects. A grounded lived-user "
                "memory source has the same authority as its captured original utterance."
            )
            reviews = validated_native_call(
                calls, stage + "-review", review_request,
                lambda response: base.validate_evidence_review(
                    structured_payload(
                        response, base.EVIDENCE_REVIEW_TOOL_NAME,
                    ), proposal,
                ), args.protocol_retries, strict_json_request(review_request),
            )
            proposal = base.proposal_without_rejected_aliases(proposal, reviews)
            proposal = base.proposal_without_rejected_classifications(
                proposal, reviews,
            )
            rejected_entities, rejected_edges, counts = base.evidence_review_rejections(proposal, reviews)
            # Participant roles are runtime facts derived from the sealed speaker
            # envelope, not model-authored classifications subject to semantic review.
            proposal, _ = canonicalize_runtime_participants(
                repo, directory / (stage + "-participants-review"),
                proposal, cases.get("participants"),
            )
            formation, temporal_normalizations = grounding.normalize(
                episode, packet, proposal, reviews,
            )
            outcome["temporal_normalizations"] = temporal_normalizations
            # Only direct source-grounded entities/facts enter the resolution experiment.
            accepted = {e["local_ref"] for e in formation["proposal"]["entities"]
                        if e["evidence_status"] == "direct"}
            formation["proposal"]["entities"] = [e for e in formation["proposal"]["entities"] if e["local_ref"] in accepted]
            formation["proposal"]["facts"] = [f for f in formation["proposal"]["facts"]
                if f["evidence_status"] == "direct" and f["subject_ref"] in accepted and f["object_ref"] in accepted]
            entities = formation["proposal"]["entities"]
            outcome["review_counts"] = counts
            if not entities:
                outcome.update(status="withheld", reason="no-directly-evidenced-entities")
            else:
                current = run_graph(repo, directory / (stage + "-candidates"), selected,
                                    formations, [], entities)
                candidate_sets = current["candidate_sets"]
                resolution = resolution_request(
                    episode, entities, candidate_sets, resolution_args,
                )
                decisions = validated_native_call(
                    calls, stage + "-resolve", resolution,
                    lambda response: validate_resolution(
                        response, entities, candidate_sets,
                    ), args.protocol_retries, strict_json_request(resolution),
                )
                resolved = apply_decisions(formation, decisions)
                result = run_graph(repo, directory / (stage + "-accepted"), selected,
                                   formations + [resolved], queries)
                formations.append(resolved)
                baseline.append(formation)
                save(directory / (stage + "-decisions.json"), decisions)
                outcome.update(status="applied", decisions=decisions,
                               graph_entities=result["graph"]["entity_count"],
                               graph_facts=result["graph"]["fact_count"])
        except Exception as error:
            outcome.update(status="deferred" if isinstance(error, ProviderDeferred) else "failed", error=str(error))
        outcomes.append(outcome)
        save(directory / "outcomes.json", outcomes)
        print(stage, outcome["status"], "(see local receipt)" if "error" in outcome else "", flush=True)
    result = run_graph(repo, directory / "final", selected, formations, queries)
    canonical = run_graph(repo, directory / "canonical-baseline", selected, baseline, queries, explicit=False)
    unchanged = before == base.sha256_file(snapshot)
    report = {"snapshot_unchanged": unchanged, "episodes_applied": len(formations),
              "episodes_selected": len(episodes), "calls": len(calls.rows),
              "reserved_usd": calls.reserved,
              "charged_usd": sum(r.get("charged_usd", r["bound_usd"]) for r in calls.rows),
              "resolved_entities": result["graph"]["entity_count"],
              "canonical_entities": canonical["graph"]["entity_count"],
              "compact_characters": [len(json.dumps(q, ensure_ascii=False)) for q in result["compact_queries"]],
              "semantic_status": "requires-human-evidence-review"}
    save(directory / "report.json", report)
    print(json.dumps(report), flush=True)
    return 0 if unchanged and len(formations) == len(episodes) else 1


if __name__ == "__main__":
    raise SystemExit(main())
