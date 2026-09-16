#!/usr/bin/env python3
"""Resume one sealed inference qualification after an explicit failed call.

Received responses are replayed into a fresh network-disabled Lisp worker.
Only the last explicit transport or unusable-output failure may be dispatched again, and
the predecessor's cumulative accounting remains binding. Higher total ceilings
must be supplied explicitly and are recorded as a limit amendment.
"""
from __future__ import annotations

import argparse
import json
import math
import os
from pathlib import Path
import time

import context_graph_authority_lab as authority
import context_graph_episode_lab as episode_lab
import context_graph_identity_lab as identity_lab
import context_graph_lab as base
import context_graph_resolution_lab as driver
import context_graph_inference_qualification as qualification


ROOT = Path(__file__).resolve().parents[1]
LAUNCHER_DRIFT_ALLOWLIST = {
    "scripts/context_graph_inference_qualification.py",
    "scripts/context_graph_inference_qualification_resume.py",
    "scripts/context_graph_authority_lab.py",
    "scripts/context-graph-inference-qualification.lisp",
    "scripts/context_graph_resolution_lab.py",
}
REVIEWED_ADMISSION_RENORMALIZATION_ALLOWLIST = {
    "src/mind/knowledge/context-graph/model-adapter.lisp",
}


def load_json(path: Path):
    return json.loads(path.read_text(encoding="utf-8"))


def existing_private_run(path: Path) -> Path:
    resolved = path.resolve()
    scratch = qualification.private_scratch_root()
    if scratch not in resolved.parents or not resolved.is_dir():
        raise ValueError("resume source must be an existing run below private scratch")
    return resolved


def receipt_roots(path: Path, seal: dict) -> set[Path]:
    """Return the verified private directories that may own response receipts."""
    roots = {path.resolve()}
    candidates = list(seal.get("receipt_roots", []))
    if seal.get("resumed_from"):
        candidates.append(seal["resumed_from"])
    scratch = qualification.private_scratch_root()
    for candidate in candidates:
        root = Path(candidate).resolve()
        if scratch not in root.parents or not root.is_dir():
            raise ValueError("resume receipt root is outside private scratch")
        roots.add(root)
    return roots


def validate_source_drift(sealed_hashes: dict, current_hashes: dict,
                          renormalize_reviewed_admission: bool = False) \
        -> dict[str, dict[str, str]]:
    """Classify explicitly permitted drift; reject every other source change."""
    if not isinstance(sealed_hashes, dict):
        raise ValueError("predecessor source seal is absent")
    permitted = set(LAUNCHER_DRIFT_ALLOWLIST)
    if renormalize_reviewed_admission:
        permitted.update(REVIEWED_ADMISSION_RENORMALIZATION_ALLOWLIST)
    drift = {}
    for name, sealed_digest in sealed_hashes.items():
        current_digest = current_hashes.get(name)
        if not isinstance(current_digest, str):
            raise ValueError("a predecessor source is absent")
        if current_digest != sealed_digest:
            if name not in permitted:
                raise ValueError("a formation-semantic predecessor source changed")
            drift[name] = {
                "sealed_sha256": sealed_digest,
                "current_sha256": current_digest,
                "classification": (
                    "reviewed-admission-renormalization"
                    if name in REVIEWED_ADMISSION_RENORMALIZATION_ALLOWLIST
                    else "qualification-launcher"
                ),
            }
    return drift


def validate_predecessor(path: Path, seed: Path, manifest: dict,
                         renormalize_reviewed_admission: bool = False) \
        -> tuple[dict, list[dict], dict[str, dict[str, str]]]:
    seal = load_json(path / "seal.json")
    report = load_json(path / "report.json")
    original_rows = load_json(path / "calls.json")
    rows = qualification.effective_qualification_rows(original_rows, report)
    if not (
        isinstance(seal, dict)
        and seal.get("schema_version") == 1
        and seal.get("protocol") == qualification.PROTOCOL
        and seal.get("execute") is True
        and seal.get("automatic_retries") == 0
        and seal.get("database_writes") == 0
        and seal.get("zdr") is True
        and seal.get("data_collection") == "deny"
        and seal.get("provider") == "phala"
        and seal.get("request_ceiling_usd") == qualification.REQUEST_CEILING_USD
        and isinstance(seal.get("cost_ceiling_usd"), (int, float))
        and math.isfinite(seal["cost_ceiling_usd"])
        and 0 < seal["cost_ceiling_usd"] <= qualification.MAX_TRIAL_CAP_USD
        and report.get("status") == "stopped"
        and report.get("database_writes") == 0
        and isinstance(rows, list)
        and rows
        and ((rows[-1].get("status") == "transient-failed"
              and rows[-1].get("http_status") in {429, 502, 503, 504})
             or rows[-1].get("status") == "invalid-response"
             or (rows[-1].get("status") == "received"
                 and (report.get("error")
                      == "Invalid graph authority input: APPLICATION_ORDER_INVALID"
                      or (report.get("error")
                          == "refused, incomplete or non-native authority response"
                          and qualification.replayable_native_response(
                              load_json(Path(rows[-1]["response_path"])))))))
        and qualification.qualification_call_count(rows)
            <= seal.get("maximum_calls", -1)
        and seal.get("seed_manifest_sha256")
            == base.sha256_file(seed / "manifest.json")
        and seal.get("event_authority_sha256") == manifest["events"]["sha256"]
        and seal.get("head_event_id") == manifest["head_event_id"]
    ):
        raise ValueError("predecessor is not a resumable sealed qualification")
    selected = seal.get("episode_event_ids")
    if not isinstance(selected, list):
        raise ValueError("predecessor episode selection is invalid")
    current_hashes = qualification.source_hashes()
    sealed_hashes = seal.get("source_sha256")
    drift = validate_source_drift(
        sealed_hashes, current_hashes, renormalize_reviewed_admission
    )
    roots = receipt_roots(path, seal)
    for row in rows:
        if row.get("status") in {"received", "invalid-response"}:
            response = Path(row.get("response_path", "")).resolve()
            if (not any(root in response.parents for root in roots)
                    or not response.is_file()):
                raise ValueError("received response is outside the resume chain")
            if base.canonical_sha256(load_json(response)) != row.get("response_sha256"):
                raise ValueError("predecessor response receipt changed")
    seal["receipt_roots"] = sorted(str(root) for root in roots)
    return seal, rows, drift


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--resume", type=Path, required=True)
    parser.add_argument("--seed", type=Path, required=True)
    parser.add_argument("--output", type=Path, required=True)
    parser.add_argument("--maximum-calls", type=int)
    parser.add_argument("--cap-usd", type=float)
    parser.add_argument(
        "--renormalize-reviewed-admission", action="store_true",
        help=("Permit only the reviewed-admission adapter to differ, then "
              "replay and digest-check every sealed receipt before continuing."),
    )
    mode = parser.add_mutually_exclusive_group()
    mode.add_argument("--preflight-only", action="store_true")
    mode.add_argument("--snapshot-only", action="store_true")
    parser.add_argument("--worker-mode", choices=("auto", "docker", "host"),
                        default="auto")
    parser.add_argument("--image", default="pai-local:development")
    return parser.parse_args()


def main() -> int:
    options = parse_args()
    if (not options.preflight_only and not options.snapshot_only
            and not os.environ.get("OPENROUTER_API_KEY")):
        raise SystemExit("OPENROUTER_API_KEY is absent; no provider request was made")
    resume = existing_private_run(options.resume)
    seed = options.seed.resolve()
    manifest, catalog = episode_lab.load_seed(seed, verify_hashes=True)
    old_seal, prior_rows, launcher_drift = validate_predecessor(
        resume, seed, manifest, options.renormalize_reviewed_admission
    )
    selected = old_seal["episode_event_ids"]
    qualification.validate_episode_selection(catalog, selected)
    previous_maximum_calls = old_seal["maximum_calls"]
    previous_cap_usd = old_seal["cost_ceiling_usd"]
    maximum_calls = (previous_maximum_calls if options.maximum_calls is None
                     else options.maximum_calls)
    cap_usd = previous_cap_usd if options.cap_usd is None else options.cap_usd
    if (type(maximum_calls) is not int
            or not previous_maximum_calls <= maximum_calls
            <= qualification.MAX_TRIAL_CALLS):
        raise SystemExit(
            "--maximum-calls may retain or explicitly increase the predecessor "
            f"ceiling up to {qualification.MAX_TRIAL_CALLS}"
        )
    if (not isinstance(cap_usd, (int, float)) or isinstance(cap_usd, bool)
            or not math.isfinite(cap_usd)
            or not previous_cap_usd <= cap_usd
            <= qualification.MAX_TRIAL_CAP_USD):
        raise SystemExit(
            "--cap-usd may retain or explicitly increase the predecessor "
            f"ceiling up to {qualification.MAX_TRIAL_CAP_USD:.2f}"
        )
    lineage = qualification.prior_budget_lineage(
        resume, seed, manifest, selected, cap_usd, maximum_calls,
        _require_remaining=False,
    )
    if (not options.preflight_only and not options.snapshot_only
            and (lineage["qualified_call_count"] >= maximum_calls
                 or lineage["exposure_usd"] >= cap_usd)):
        raise SystemExit(
            "qualification cumulative call or cost ceiling is exhausted; "
            "no provider request was made"
        )
    local_ledger = driver.Calls(
        resume,
        argparse.Namespace(cost_ceiling_usd=cap_usd,
                           request_limit=maximum_calls),
        prior_rows=prior_rows,
    )
    local_qualified_calls = qualification.qualification_call_count(prior_rows)
    inherited_attempts = lineage["call_count"] - len(prior_rows)
    inherited_calls = (
        lineage["qualified_call_count"] - local_qualified_calls
    )
    inherited_exposure = lineage["exposure_usd"] - local_ledger.budget_used_usd
    output = qualification.require_private_output(options.output)
    output.mkdir(parents=True, exist_ok=False)

    hashes = qualification.source_hashes()
    seal = dict(old_seal)
    seal.update(
        source_sha256=hashes,
        resumed_from=str(resume),
        predecessor_seal_sha256=base.sha256_file(resume / "seal.json"),
        predecessor_calls_sha256=base.sha256_file(resume / "calls.json"),
        predecessor_call_count=len(prior_rows),
        predecessor_total_call_count=lineage["call_count"],
        predecessor_total_qualified_call_count=lineage["qualified_call_count"],
        predecessor_total_exposure_usd=lineage["exposure_usd"],
        predecessor_maximum_calls=previous_maximum_calls,
        predecessor_cost_ceiling_usd=previous_cap_usd,
        maximum_calls=maximum_calls,
        cost_ceiling_usd=cap_usd,
        validated_launcher_drift=launcher_drift,
        reviewed_admission_renormalization=(
            options.renormalize_reviewed_admission
        ),
    )
    driver.save(output / "seal.json", seal)
    args = argparse.Namespace(
        model=seal["model"], max_output_tokens=2048,
        max_prompt_price=.33, max_completion_price=1.21,
        cost_ceiling_usd=cap_usd - inherited_exposure,
        request_limit=(maximum_calls - inherited_calls
                       + len(prior_rows) - local_qualified_calls),
        provider_timeout_seconds=180,
        transient_retries=0, openrouter_zdr="require",
        openrouter_data_collection="deny", openrouter_provider_only="phala",
        reasoning_policy="low",
    )
    calls = driver.Calls(output, args, prior_rows=prior_rows,
                         resume_directory=resume)
    sealed_baseline = seal.get("baseline_checkpoint")
    baseline_checkpoint = None
    if sealed_baseline is not None:
        if not isinstance(sealed_baseline, dict) or not sealed_baseline.get("path"):
            raise ValueError("sealed baseline checkpoint descriptor is invalid")
        baseline_checkpoint = qualification.load_baseline_checkpoint(
            Path(sealed_baseline["path"]), manifest
        )
        if not all(baseline_checkpoint.get(key) == sealed_baseline.get(key)
                   for key in ("sha256", "digest", "contract")):
            raise ValueError("sealed baseline checkpoint changed")
    worker = None
    report = {
        "schema_version": 1, "status": "resuming",
        "provider_calls": inherited_calls + local_qualified_calls,
        "provider_attempts": inherited_attempts + len(prior_rows),
        "charged_usd": inherited_exposure + calls.budget_used_usd,
        "prior_provider_calls": inherited_calls,
        "prior_exposure_usd": inherited_exposure,
        "database_writes": 0,
    }
    try:
        qualification.ensure_sources_unchanged(hashes)
        worker = qualification.QualificationWorker.start(
            seed, manifest, selected, seal["observed_at"],
            options.worker_mode, options.image,
            seal.get("baseline_event_id"),
            baseline_checkpoint,
            seal.get("episode_batches"),
            seal.get("episode_batch_call_limits"),
        )
        driver.save(output / "worker-ready.json", worker.ready)
        workload_upper_bound = qualification.workload_request_upper_bound(
            worker.ready
        )
        required_call_cap = qualification.required_cumulative_call_cap(
            inherited_calls, worker.ready
        )
        report.update(
            source_batch_count=worker.ready["source_batch_count"],
            source_batch_counts=worker.ready["source_batch_counts"],
            request_count_upper_bound=workload_upper_bound,
            uncounted_failed_attempts=(len(prior_rows)
                                       - local_qualified_calls),
            required_cumulative_call_cap=required_call_cap,
        )
        if options.preflight_only:
            report.update(status="sealed-preflight-no-calls")
            driver.save(output / "report.json", report)
            print(json.dumps({
                "status": report["status"],
                "provider_calls": report["provider_calls"],
                "required_cumulative_call_cap": required_call_cap,
                "database_writes": 0,
                "artifacts": str(output),
            }), flush=True)
            return 0
        if not options.snapshot_only and maximum_calls < required_call_cap:
            report.update(status="stopped-insufficient-call-cap")
            driver.save(output / "report.json", report)
            print(json.dumps({
                "status": report["status"],
                "provider_calls": report["provider_calls"],
                "required_cumulative_call_cap": required_call_cap,
                "database_writes": 0,
                "artifacts": str(output),
            }), flush=True)
            return 2
        result = worker.call({"operation": "next"})
        step = 0
        last_fresh_call_completed = None
        while result.get("status") == "request":
            step += 1
            qualification.ensure_sources_unchanged(hashes)
            phase = result["phase"]
            args.max_output_tokens = qualification.phase_output_tokens(phase)
            request = identity_lab.request_from_spec(result["spec"], args)
            request["max_tokens"] = args.max_output_tokens
            bound = base.request_cost_bound(request, args)
            if bound > seal["request_ceiling_usd"]:
                raise ValueError("resumed request exceeds its sealed ceiling")
            driver.save(output / f"step-{step:02d}-request.json", request)
            driver.save(output / f"step-{step:02d}-worker.json", result)
            stage = (f"task-{result['task_ordinal'] + 1:02d}-"
                     f"batch-{result['batch_index']:02d}-{phase}")
            matching = [row for row in calls.rows
                        if row.get("stage") == stage]
            if (options.snapshot_only and matching
                    and matching[-1].get("status") == "transient-failed"):
                if matching[-1].get("request_sha256") \
                        != base.canonical_sha256(request):
                    raise ValueError("snapshot request differs from sealed receipt")
                snapshot = worker.call({"operation": "snapshot"})
                driver.save(output / "result.json", snapshot)
                report.update(
                    status="completed-partial-snapshot",
                    pending_phase=snapshot.get("pending_phase"),
                    baseline=snapshot["baseline"], after=snapshot["after"],
                    changed_nodes=len(snapshot["nodes"]),
                    changed_edges=len(snapshot["edges"]),
                    formation_attempts=len(snapshot["formation_attempts"]),
                )
                driver.save(output / "report.json", report)
                print(json.dumps({
                    "status": report["status"],
                    "provider_calls": report["provider_calls"],
                    "database_writes": 0,
                    "pending_phase": report["pending_phase"],
                    "changed_nodes": report["changed_nodes"],
                    "changed_edges": report["changed_edges"],
                    "artifacts": str(output),
                }), flush=True)
                return 0
            if last_fresh_call_completed is not None:
                delay = (seal.get("minimum_call_interval_seconds", 0)
                         - (time.monotonic() - last_fresh_call_completed))
                if delay > 0:
                    time.sleep(delay)
            prior_count = len(calls.rows)
            response = calls.call(stage, request)
            if len(calls.rows) > prior_count:
                last_fresh_call_completed = time.monotonic()
            payload = authority.response_payload(
                response, result["spec"]["tool_name"]
            )
            result = worker.call({
                "operation": "response", "phase": phase,
                "request_digest": result["request_digest"],
                "response": payload,
            })
        if result.get("status") != "complete":
            raise ValueError("qualification worker returned no terminal result")
        qualification.ensure_sources_unchanged(hashes)
        driver.save(output / "result.json", result)
        report.update(
            status="completed-requires-human-review",
            provider_calls=(inherited_calls
                            + qualification.qualification_call_count(calls.rows)),
            provider_attempts=inherited_attempts + len(calls.rows),
            charged_usd=inherited_exposure + calls.budget_used_usd,
            reserved_usd=calls.reserved,
            baseline=result["baseline"], after=result["after"],
            changed_nodes=len(result["nodes"]),
            changed_edges=len(result["edges"]),
            formation_attempts=len(result["formation_attempts"]),
        )
        driver.save(output / "report.json", report)
        print(json.dumps({
            "status": report["status"],
            "provider_calls": report["provider_calls"],
            "charged_usd": report["charged_usd"],
            "changed_nodes": report["changed_nodes"],
            "changed_edges": report["changed_edges"],
            "artifacts": str(output),
        }), flush=True)
        return 0
    except Exception as error:
        report.update(
            status="stopped", error=str(error),
            provider_calls=(inherited_calls
                            + qualification.qualification_call_count(calls.rows)),
            provider_attempts=inherited_attempts + len(calls.rows),
            charged_usd=inherited_exposure + calls.budget_used_usd,
            outstanding_exposure_usd=(inherited_exposure
                                      + calls.budget_used_usd),
        )
        driver.save(output / "report.json", report)
        raise
    finally:
        if worker is not None:
            worker.close()
            (output / "worker.log").write_text(
                "".join(worker.log), encoding="utf-8"
            )


if __name__ == "__main__":
    raise SystemExit(main())
