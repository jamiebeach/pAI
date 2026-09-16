#!/usr/bin/env python3
"""Capped V14 qualification against selected immutable private episodes.

The Lisp worker owns source authentication, formation, review, admission and
graph deltas.  This launcher owns only an explicit OpenRouter transport ledger.
It is a dry run unless ``--execute`` is supplied with finite approved cost and
call ceilings.
Artifacts may contain private source text and therefore must stay below the
configured private scratch boundary.
"""
from __future__ import annotations

import argparse
import json
import math
import os
from pathlib import Path
import shutil
import subprocess
import tempfile
import time

import context_graph_authority_lab as authority
import context_graph_episode_lab as episode_lab
import context_graph_identity_lab as identity_lab
import context_graph_lab as base
import context_graph_resolution_lab as driver


ROOT = Path(__file__).resolve().parents[1]
PROTOCOL = "identity-formation-v14"
ONTOLOGY_REVISION = "personal-context-core-glm53-v1.3"
MAX_PRIVATE_EPISODES = 6
MAX_CALLS_PER_BATCH = 14
MAX_TRIAL_CALLS = 256
REQUEST_CEILING_USD = 0.06
MAX_TRIAL_CAP_USD = 5.0
CL_UNIX_EPOCH_OFFSET = 2_208_988_800
VERIFIED_TRANSIENT_HTTP_FAILURES = {429, 502, 503, 504}


def private_scratch_root() -> Path:
    """Return the explicit private artifact boundary, outside source if needed."""
    configured = os.environ.get("PAI_PRIVATE_SCRATCH")
    resolved = Path(configured).resolve() if configured else (ROOT / ".scratch").resolve()
    if resolved == ROOT.resolve() or resolved == Path(resolved.anchor):
        raise ValueError("private scratch must be a bounded directory")
    return resolved


def qualification_call_count(rows: list[dict]) -> int:
    """Count usable/unknown calls, not explicit transport or output failures."""
    return sum(
        not ((row.get("status") == "transient-failed"
              and row.get("http_status") in VERIFIED_TRANSIENT_HTTP_FAILURES)
             or row.get("status") == "invalid-response")
        for row in rows
    )


def effective_qualification_rows(rows: list[dict], report: dict) -> list[dict]:
    """Classify a legacy terminal empty/length response without altering its run."""
    effective = [dict(row) for row in rows]
    if (effective and report.get("status") == "stopped"
            and report.get("error") == "refused, incomplete or non-native authority response"
            and effective[-1].get("status") == "received"):
        response_path = effective[-1].get("response_path")
        response = (json.loads(Path(response_path).read_text(encoding="utf-8"))
                    if response_path else None)
        if not replayable_native_response(response):
            effective[-1].update(
                status="invalid-response",
                original_status="received",
                failure_kind="incomplete-or-non-native-authority-response",
                error=report["error"],
            )
    return effective


def phase_output_tokens(phase: str) -> int:
    if phase == "review":
        return 8192
    if phase in {"new-identity-groups", "new-identities", "facts"}:
        return 4096
    return 2048


def replayable_native_response(response: object) -> bool:
    """Admit only a completed single native tool call into a replay prefix."""
    if not isinstance(response, dict):
        return False
    choices = response.get("choices")
    if not isinstance(choices, list) or len(choices) != 1:
        return False
    choice = choices[0]
    if not isinstance(choice, dict) or choice.get("finish_reason") not in {
            "stop", "tool_calls"}:
        return False
    message = choice.get("message")
    if (not isinstance(message, dict) or message.get("refusal")
            or not isinstance(message.get("tool_calls"), list)
            or not message["tool_calls"]):
        return False
    calls = message["tool_calls"]
    signature = (calls[0].get("type"), calls[0].get("function"))
    return all((call.get("type"), call.get("function")) == signature
               for call in calls[1:])


def output_limit_extension_compatible(
        sealed_request: object, current_request: object) -> bool:
    """Allow replay only when the request differs by a larger output limit."""
    if not isinstance(sealed_request, dict) or not isinstance(current_request, dict):
        return False
    old_limit = sealed_request.get("max_tokens")
    new_limit = current_request.get("max_tokens")
    if (type(old_limit) is not int or type(new_limit) is not int
            or new_limit < old_limit):
        return False
    sealed = dict(sealed_request)
    current = dict(current_request)
    sealed.pop("max_tokens", None)
    current.pop("max_tokens", None)
    return sealed == current


def replay_request_matches(candidate: dict, current_request: dict) -> bool:
    current_sha = base.canonical_sha256(current_request)
    if candidate.get("request_sha256") == current_sha:
        return True
    response_path = Path(candidate.get("response_path", "")).resolve()
    request_path = response_path.with_name(
        response_path.name.replace("response-", "request-", 1)
    )
    scratch = private_scratch_root()
    if (scratch not in request_path.parents or not request_path.is_file()
            or request_path.name == response_path.name):
        return False
    sealed_request = json.loads(request_path.read_text(encoding="utf-8"))
    return (
        base.canonical_sha256(sealed_request)
            == candidate.get("request_sha256")
        and output_limit_extension_compatible(sealed_request, current_request)
    )


def workload_request_upper_bound(worker_ready: dict) -> int:
    """Verify the worker's content-free batch inventory and derive its bound."""
    counts = worker_ready.get("source_batch_counts")
    if (not isinstance(counts, list) or not counts
            or any(type(count) is not int or count < 0 for count in counts)
            or sum(counts) <= 0):
        raise ValueError("qualification worker returned invalid source batch counts")
    total = sum(counts)
    limits = worker_ready.get("source_batch_call_limits")
    if (not isinstance(limits, list) or len(limits) != total
            or any(type(limit) is not int
                   or not 1 <= limit <= MAX_CALLS_PER_BATCH
                   for limit in limits)):
        raise ValueError("qualification worker returned invalid batch call limits")
    upper = sum(limits)
    if (worker_ready.get("source_batch_count") != total
            or worker_ready.get("request_count_upper_bound") != upper):
        raise ValueError("qualification worker request upper bound is inconsistent")
    return upper


def validate_worker_contract(worker_ready: dict) -> None:
    if not (
        worker_ready.get("schema_version") == 1
        and worker_ready.get("protocol") == PROTOCOL
        and worker_ready.get("ontology_revision") == ONTOLOGY_REVISION
        and worker_ready.get("database_write_count") == 0
    ):
        raise ValueError("qualification worker contract is incompatible")


def required_cumulative_call_cap(prior_calls: int, worker_ready: dict,
                                 transient_overhead: int = 0) -> int:
    if (type(prior_calls) is not int or prior_calls < 0
            or type(transient_overhead) is not int or transient_overhead < 0):
        raise ValueError("qualification call lineage is invalid")
    return (prior_calls + workload_request_upper_bound(worker_ready)
            + transient_overhead)


def validate_episode_selection(catalog: dict, selected: list[int]) -> list[dict]:
    if (not 1 <= len(selected) <= MAX_PRIVATE_EPISODES
            or len(selected) != len(set(selected))
            or any(type(value) is not int or value <= 0 for value in selected)):
        raise ValueError("select one to six distinct positive episode event IDs")
    if selected != sorted(selected):
        raise ValueError("acceptance episodes must be supplied in authority order")
    by_id = {row.get("episode_event_id"): row
             for row in catalog.get("episodes", [])}
    rows = []
    for event_id in selected:
        row = by_id.get(event_id)
        if not isinstance(row, dict):
            raise ValueError(f"selected event {event_id} is absent from the seed catalog")
        if row.get("status") not in {"completed", "queued"}:
            raise ValueError(
                f"selected event {event_id} is neither completed nor queued"
            )
        rows.append(row)
    return rows


def parse_episode_batches(values: list[str] | None,
                          selected: list[int]) -> list[dict] | None:
    """Parse an optional ordered, duplicate-free EVENT:BATCH qualification scope."""
    if not values:
        return None
    tasks: list[dict] = []
    seen: set[tuple[int, int]] = set()
    selected_set = set(selected)
    for value in values:
        try:
            event_text, batch_text = value.split(":", 1)
            event_id = int(event_text)
            batch_index = int(batch_text)
        except (ValueError, AttributeError) as error:
            raise ValueError("episode batches must use EVENT_ID:BATCH_INDEX") from error
        key = (event_id, batch_index)
        if event_id not in selected_set or batch_index < 0 or key in seen:
            raise ValueError("episode batch scope is foreign, negative, or duplicated")
        seen.add(key)
        tasks.append({"episode_event_id": event_id, "batch_index": batch_index})
    expected = sorted(seen)
    if [(row["episode_event_id"], row["batch_index"]) for row in tasks] != expected:
        raise ValueError("episode batches must be supplied in authority order")
    return tasks


def parse_episode_batch_call_limits(
        values: list[str] | None,
        episode_batches: list[dict] | None) -> list[dict] | None:
    """Seal an exact per-task call bound for incremental qualification."""
    if not values:
        return None
    if episode_batches is None:
        raise ValueError("batch call limits require an explicit episode batch scope")
    limits: list[dict] = []
    for value in values:
        try:
            event_text, batch_text, limit_text = value.split(":", 2)
            event_id = int(event_text)
            batch_index = int(batch_text)
            maximum_calls = int(limit_text)
        except (ValueError, AttributeError) as error:
            raise ValueError(
                "batch call limits must use EVENT_ID:BATCH_INDEX:MAXIMUM_CALLS"
            ) from error
        if not 1 <= maximum_calls <= MAX_CALLS_PER_BATCH:
            raise ValueError("batch call limits must be between 1 and 14")
        limits.append({
            "episode_event_id": event_id,
            "batch_index": batch_index,
            "maximum_calls": maximum_calls,
        })
    expected = [(row["episode_event_id"], row["batch_index"])
                for row in episode_batches]
    actual = [(row["episode_event_id"], row["batch_index"])
              for row in limits]
    if actual != expected:
        raise ValueError("batch call limits must cover the exact ordered batch scope")
    return limits


def replay_task_prefix_compatible(
        old_tasks: object, new_tasks: object, selected: list[int]) -> bool:
    """Allow a bounded task scope to resume an earlier unscoped first batch."""
    if not isinstance(new_tasks, list) or not new_tasks or not selected:
        return False
    if isinstance(old_tasks, list) and old_tasks == new_tasks:
        return True
    first = {"episode_event_id": selected[0], "batch_index": 0}
    if new_tasks[0] != first:
        return False
    if old_tasks is None:
        return True
    return isinstance(old_tasks, list) and bool(old_tasks) \
        and old_tasks[0] == new_tasks[0]


def require_private_output(path: Path) -> Path:
    resolved = path.resolve()
    scratch = private_scratch_root()
    if resolved == scratch or scratch not in resolved.parents:
        raise ValueError("private qualification artifacts must be below private scratch")
    if resolved.exists():
        raise ValueError(f"qualification output already exists: {resolved}")
    return resolved


def load_baseline_checkpoint(path: Path | None, manifest: dict) -> dict | None:
    """Pin one private lab checkpoint; never reconstruct on a mismatch."""
    if path is None:
        return None
    resolved = path.resolve()
    scratch = private_scratch_root()
    if scratch not in resolved.parents or not resolved.is_file():
        raise ValueError("baseline checkpoint must be an existing private artifact")
    if resolved.stat().st_size > episode_lab.SAVED_RESULT_MAX_BYTES:
        raise ValueError("baseline checkpoint exceeds the read bound")
    outer = json.loads(resolved.read_text(encoding="utf-8"))
    contract = outer.get("contract")
    if not (outer.get("status") == "target"
            and isinstance(outer.get("digest"), str)
            and isinstance(contract, dict)
            and contract.get("agent_id") == manifest["partition"]["agent_id"]
            and contract.get("persona_id") == manifest["partition"]["persona_id"]
            and contract.get("protocol") == PROTOCOL
            and contract.get("ontology_revision") == ONTOLOGY_REVISION):
        raise ValueError("baseline checkpoint contract is incompatible")
    return {"path": resolved, "sha256": base.sha256_file(resolved),
            "digest": outer["digest"], "contract": contract}


class QualificationWorker:
    def __init__(self, process: subprocess.Popen[str], config_file: Path):
        self.process = process
        self.config_file = config_file
        self.log: list[str] = []
        self.ready: dict | None = None

    @classmethod
    def start(cls, seed: Path, manifest: dict, selected: list[int],
              observed_at: int, mode: str, image: str,
              baseline_event_id: int | None = None,
              baseline_checkpoint: dict | None = None,
              episode_batches: list[dict] | None = None,
              episode_batch_call_limits: list[dict] | None = None
              ) -> "QualificationWorker":
        descriptor = manifest["events"]
        events_path = (Path(descriptor["file"]).resolve()
                       if manifest.get("storage_mode") == "reference"
                       else (seed / descriptor["file"]).resolve())
        use_docker = mode == "docker" or (mode == "auto" and shutil.which("sbcl") is None)
        config = {
            "events": "/lab/events.sqlite3" if use_docker else str(events_path),
            "agent_id": manifest["partition"]["agent_id"],
            "persona_id": manifest["partition"]["persona_id"],
            "recovery_start_storage_position":
                manifest.get("recovery_start_storage_position"),
            "head_event_id": manifest["head_event_id"],
            "baseline_event_id": (
                manifest["head_event_id"]
                if baseline_event_id is None else baseline_event_id
            ),
            "baseline_checkpoint_path": (
                "/lab/baseline.json" if use_docker and baseline_checkpoint
                else (str(baseline_checkpoint["path"])
                      if baseline_checkpoint else None)
            ),
            "baseline_checkpoint_digest": (
                baseline_checkpoint["digest"] if baseline_checkpoint else None
            ),
            "baseline_checkpoint_contract": (
                baseline_checkpoint["contract"] if baseline_checkpoint else None
            ),
            "maximum_calls_per_batch": MAX_CALLS_PER_BATCH,
            "protocol": PROTOCOL,
            "ontology_revision": ONTOLOGY_REVISION,
            "episode_event_ids": selected,
            "episode_batches": episode_batches,
            "episode_batch_call_limits": episode_batch_call_limits,
            "observed_at": observed_at,
        }
        descriptor_id, config_name = tempfile.mkstemp(
            prefix="kg-inference-qualification-", suffix=".json")
        os.close(descriptor_id)
        config_file = Path(config_name)
        config_file.write_bytes(episode_lab.canonical_bytes(config))
        environment = os.environ.copy()
        if use_docker:
            command = [
                "docker", "run", "--rm", "-i", "--network", "none",
                "--read-only", "--cap-drop", "ALL", "--security-opt",
                "no-new-privileges:true", "--pids-limit", "256",
                "--memory", "4g", "--tmpfs",
                "/tmp:rw,nosuid,nodev,noexec,size=256m,mode=1777",
                "--tmpfs", "/agent/state:rw,nosuid,nodev,noexec,size=16m,mode=0700",
                "-v", "pai-context-graph-lisp-cache:/home/pai/.cache",
                "-e", "PAI_CONTEXT_GRAPH_INFERENCE_QUALIFICATION_CONFIG=/lab/config.json",
                "-e", "PAI_QUICKLISP_SETUP=/opt/quicklisp/setup.lisp",
                "-e", "PAI_REPOSITORY_ROOT=/workspace/",
                "-v", f"{ROOT.resolve()}:/workspace:ro",
                "-v", f"{events_path}:/lab/events.sqlite3:ro",
                "-v", f"{config_file.resolve()}:/lab/config.json:ro",
                *(["-v", f"{baseline_checkpoint['path']}:/lab/baseline.json:ro"]
                  if baseline_checkpoint else []),
                image, "sbcl", "--dynamic-space-size", "3072",
                "--noinform", "--disable-debugger",
                "--script", "/workspace/scripts/context-graph-inference-qualification.lisp",
            ]
        else:
            environment.update(
                PAI_CONTEXT_GRAPH_INFERENCE_QUALIFICATION_CONFIG=str(config_file),
                PAI_QUICKLISP_SETUP=os.environ.get(
                    "PAI_QUICKLISP_SETUP", str(Path.home() / "quicklisp" / "setup.lisp")),
                PAI_REPOSITORY_ROOT=str(ROOT.resolve()) + os.sep,
            )
            command = ["sbcl", "--noinform", "--disable-debugger", "--script",
                       str(ROOT / "scripts" / "context-graph-inference-qualification.lisp")]
        process: subprocess.Popen[str] | None = None
        try:
            process = subprocess.Popen(
                command, cwd=ROOT, env=environment, stdin=subprocess.PIPE,
                stdout=subprocess.PIPE, stderr=subprocess.STDOUT,
                text=True, encoding="utf-8", bufsize=1,
            )
            worker = cls(process, config_file)
            worker._wait_ready()
            return worker
        except BaseException:
            if process is not None and process.poll() is None:
                process.terminate()
                try:
                    process.wait(timeout=10)
                except subprocess.TimeoutExpired:
                    process.kill()
            config_file.unlink(missing_ok=True)
            raise

    def _wait_ready(self) -> None:
        assert self.process.stdout is not None
        deadline = time.monotonic() + 240
        prefix = "KG-INFERENCE-QUALIFICATION-READY "
        while time.monotonic() < deadline:
            line = self.process.stdout.readline()
            if line == "" and self.process.poll() is not None:
                raise RuntimeError(
                    f"qualification worker stopped during startup "
                    f"(exit {self.process.returncode}):\n" + "".join(self.log[-40:]))
            self.log.append(line)
            if line.startswith(prefix):
                self.ready = json.loads(line[len(prefix):])
                return
        raise TimeoutError("qualification worker did not become ready")

    def call(self, request: dict) -> dict:
        if self.process.poll() is not None:
            raise RuntimeError("qualification worker is not running")
        assert self.process.stdin is not None
        assert self.process.stdout is not None
        self.process.stdin.write(
            episode_lab.canonical_bytes(request).decode("utf-8") + "\n")
        self.process.stdin.flush()
        result_prefix = "KG-INFERENCE-QUALIFICATION-RESULT "
        error_prefix = "KG-INFERENCE-QUALIFICATION-ERROR "
        while True:
            line = self.process.stdout.readline()
            if line == "" and self.process.poll() is not None:
                raise RuntimeError("qualification worker stopped:\n" +
                                   "".join(self.log[-40:]))
            self.log.append(line)
            if line.startswith(result_prefix):
                return json.loads(line[len(result_prefix):])
            if line.startswith(error_prefix):
                detail = json.loads(line[len(error_prefix):]).get("error")
                raise RuntimeError(detail or "qualification worker request failed")

    def close(self) -> None:
        try:
            if self.process.stdin:
                self.process.stdin.close()
            self.process.wait(timeout=15)
        except subprocess.TimeoutExpired:
            self.process.terminate()
            try:
                self.process.wait(timeout=10)
            except subprocess.TimeoutExpired:
                self.process.kill()
        finally:
            self.config_file.unlink(missing_ok=True)


def source_hashes() -> dict[str, str]:
    names = {
        "scripts/context_graph_inference_qualification.py",
        "scripts/context-graph-inference-qualification.lisp",
        "scripts/context_graph_inference_qualification_resume.py",
        "scripts/context_graph_episode_lab.py",
        "scripts/context-graph-episode-lab-core.lisp",
        "scripts/context_graph_authority_lab.py",
        "scripts/context_graph_identity_lab.py",
        "scripts/context_graph_resolution_lab.py",
        "scripts/context_graph_lab.py",
        "src/mind/conscious/context-graph-runtime-adapter.lisp",
        "config/context-graph-upper-ontology-v1.2.json",
        "config/conscious-provider-profiles.json",
    }
    # Formation replays the full pAI system, so seal every Lisp source and ASD
    # definition that can affect the result, not only the graph subsystem.
    names.update(path.relative_to(ROOT).as_posix()
                 for path in ROOT.glob("*.asd"))
    names.update(path.relative_to(ROOT).as_posix()
                 for path in (ROOT / "src").rglob("*.lisp"))
    return {name: base.sha256_file(ROOT / name) for name in sorted(names)}


def ensure_sources_unchanged(hashes: dict[str, str]) -> None:
    if any(base.sha256_file(ROOT / name) != digest
           for name, digest in hashes.items()):
        raise ValueError("qualification source changed after the run was sealed")


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--seed", type=Path, required=True)
    parser.add_argument("--episode-event-id", type=int, action="append", required=True)
    parser.add_argument(
        "--episode-batch", action="append",
        help="optionally qualify only an ordered EVENT_ID:BATCH_INDEX subset",
    )
    parser.add_argument(
        "--episode-batch-call-limit", action="append",
        help=("seal EVENT_ID:BATCH_INDEX:MAXIMUM_CALLS for every selected "
              "batch (requires --episode-batch)"),
    )
    parser.add_argument("--output", type=Path, required=True)
    parser.add_argument(
        "--baseline-checkpoint", type=Path,
        help="open this immutable lab checkpoint instead of an empty graph",
    )
    parser.add_argument("--execute", action="store_true")
    parser.add_argument("--cap-usd", type=float)
    parser.add_argument("--maximum-calls", type=int)
    parser.add_argument("--prior-run", type=Path)
    parser.add_argument(
        "--replay-prefix-from", type=Path,
        help=("reuse a digest-matching received-response prefix from the same "
              "sealed --prior-run without counting those responses as calls"),
    )
    parser.add_argument("--minimum-call-interval-seconds", type=float,
                        default=15.0)
    parser.add_argument("--worker-mode", choices=("auto", "docker", "host"),
                        default="auto")
    parser.add_argument("--image", default="pai-local:development")
    return parser.parse_args()


def prior_budget_lineage(path: Path, seed: Path, manifest: dict,
                         selected: list[int], cap_usd: float,
                         maximum_calls: int, _seen: set[Path] | None = None,
                         _require_remaining: bool = True) -> dict:
    """Verify a sealed predecessor and conservatively carry its exposure."""
    resolved = path.resolve()
    seen = set() if _seen is None else _seen
    if resolved in seen:
        raise ValueError("prior qualification budget lineage contains a cycle")
    seen.add(resolved)
    scratch = private_scratch_root()
    if scratch not in resolved.parents or not resolved.is_dir():
        raise ValueError("prior qualification must be below private scratch")
    seal = json.loads((resolved / "seal.json").read_text(encoding="utf-8"))
    report = json.loads((resolved / "report.json").read_text(encoding="utf-8"))
    rows = json.loads((resolved / "calls.json").read_text(encoding="utf-8"))
    if not (
        isinstance(seal, dict) and seal.get("schema_version") == 1
        and seal.get("protocol") in {
            "identity-formation-v10", "identity-formation-v11",
            "identity-formation-v12", "identity-formation-v13", PROTOCOL
        }
        and (seal.get("protocol") != PROTOCOL
             or seal.get("ontology_revision") == ONTOLOGY_REVISION)
        and seal.get("execute") is True
        and seal.get("database_writes") == 0
        and seal.get("automatic_retries") == 0
        and seal.get("provider") == "phala" and seal.get("zdr") is True
        and seal.get("data_collection") == "deny"
        and seal.get("request_ceiling_usd") == REQUEST_CEILING_USD
        and seal.get("episode_event_ids") == selected
        and seal.get("seed_manifest_sha256")
            == base.sha256_file(seed / "manifest.json")
        and seal.get("event_authority_sha256") == manifest["events"]["sha256"]
        and seal.get("head_event_id") == manifest["head_event_id"]
        and type(seal.get("maximum_calls")) is int
        and 0 < seal["maximum_calls"] <= maximum_calls
        and isinstance(seal.get("cost_ceiling_usd"), (int, float))
        and not isinstance(seal.get("cost_ceiling_usd"), bool)
        and math.isfinite(seal["cost_ceiling_usd"])
        and 0 < seal["cost_ceiling_usd"] <= cap_usd
        and report.get("status") in {
            "stopped", "completed-requires-human-review"
        }
        and report.get("database_writes") == 0
        and isinstance(rows, list) and rows
        and all(isinstance(row, dict)
                and isinstance(row.get("bound_usd"), (int, float))
                and not isinstance(row.get("bound_usd"), bool)
                and math.isfinite(row["bound_usd"])
                and 0 <= row["bound_usd"] <= REQUEST_CEILING_USD
                for row in rows)
    ):
        raise ValueError("prior qualification budget lineage is invalid")
    inherited = seal.get("prior_run")
    if inherited is None:
        inherited_calls = 0
        inherited_qualified_calls = 0
        inherited_exposure = 0.0
    else:
        if not isinstance(inherited, dict) or not inherited.get("path"):
            raise ValueError("embedded prior qualification lineage is invalid")
        parent = prior_budget_lineage(
            Path(inherited["path"]), seed, manifest, selected, cap_usd,
            maximum_calls, seen,
        )
        if not (
            inherited.get("call_count") == parent["call_count"]
            and math.isclose(inherited.get("exposure_usd", -1),
                             parent["exposure_usd"], abs_tol=1e-12)
            and inherited.get("seal_sha256") == parent["seal_sha256"]
            and inherited.get("calls_sha256") == parent["calls_sha256"]
        ):
            raise ValueError("embedded prior qualification lineage changed")
        inherited_calls = parent["call_count"]
        inherited_qualified_calls = parent["qualified_call_count"]
        inherited_exposure = parent["exposure_usd"]
    ledger_args = argparse.Namespace(cost_ceiling_usd=cap_usd,
                                     request_limit=maximum_calls)
    effective_rows = effective_qualification_rows(rows, report)
    ledger = driver.Calls(resolved, ledger_args, prior_rows=effective_rows)
    call_count = inherited_calls + len(rows)
    qualified_call_count = (
        inherited_qualified_calls + qualification_call_count(effective_rows)
    )
    legacy_qualified_call_count = (
        inherited_qualified_calls + qualification_call_count(rows)
    )
    exposure = inherited_exposure + ledger.budget_used_usd
    if (report.get("provider_calls") not in {
            call_count, qualified_call_count, legacy_qualified_call_count}
            or qualified_call_count > seal["maximum_calls"]
            or not math.isfinite(exposure) or exposure < 0
            or exposure > seal["cost_ceiling_usd"]):
        raise ValueError("prior qualification budget lineage is invalid")
    if (_require_remaining
            and (qualified_call_count >= maximum_calls or exposure >= cap_usd)):
        raise ValueError("prior qualification exposure exhausts the trial cap")
    return {
        "path": str(resolved), "call_count": call_count,
        "qualified_call_count": qualified_call_count,
        "exposure_usd": exposure,
        "seal_sha256": base.sha256_file(resolved / "seal.json"),
        "calls_sha256": base.sha256_file(resolved / "calls.json"),
    }


def replay_prefix_rows(path: Path) -> tuple[dict, list[dict]]:
    """Load locally replayable received responses from a sealed private run."""
    resolved = path.resolve()
    scratch = private_scratch_root()
    if scratch not in resolved.parents or not resolved.is_dir():
        raise ValueError("replay source must be an existing run below private scratch")
    seal = json.loads((resolved / "seal.json").read_text(encoding="utf-8"))
    replay_path = resolved / "replay.json"
    if replay_path.is_file():
        rows = json.loads(replay_path.read_text(encoding="utf-8"))
    else:
        ledger = json.loads((resolved / "calls.json").read_text(encoding="utf-8"))
        rows = [row for row in ledger if row.get("status") == "received"]
    if not isinstance(rows, list):
        raise ValueError("replay response index is invalid")
    normalized = []
    for row in rows:
        if not isinstance(row, dict):
            raise ValueError("replay response receipt is invalid")
        response = Path(row.get("response_path", "")).resolve()
        if (not isinstance(row.get("stage"), str)
                or not isinstance(row.get("request_sha256"), str)
                or not isinstance(row.get("response_sha256"), str)
                or scratch not in response.parents or not response.is_file()
                or base.canonical_sha256(
                    json.loads(response.read_text(encoding="utf-8")))
                   != row["response_sha256"]):
            raise ValueError("replay response receipt is invalid")
        response_value = json.loads(response.read_text(encoding="utf-8"))
        if not replayable_native_response(response_value):
            break
        normalized.append({
            "stage": row["stage"],
            "request_sha256": row["request_sha256"],
            "request_digest": row.get("request_digest"),
            "response_path": str(response),
            "response_sha256": row["response_sha256"],
        })
    return seal, normalized


def main() -> int:
    options = parse_args()
    if (not math.isfinite(options.minimum_call_interval_seconds)
            or not 0 <= options.minimum_call_interval_seconds <= 60):
        raise SystemExit("--minimum-call-interval-seconds must be between 0 and 60")
    if options.execute:
        if (options.cap_usd is None or isinstance(options.cap_usd, bool)
                or not math.isfinite(options.cap_usd)
                or not 0 < options.cap_usd <= MAX_TRIAL_CAP_USD):
            raise SystemExit(
                f"--execute requires an approved finite --cap-usd no greater than "
                f"{MAX_TRIAL_CAP_USD:.2f}")
        if (type(options.maximum_calls) is not int
                or not 1 <= options.maximum_calls <= MAX_TRIAL_CALLS):
            raise SystemExit(
                f"--maximum-calls must be explicit and between 1 and "
                f"{MAX_TRIAL_CALLS} with --execute"
            )
        if not os.environ.get("OPENROUTER_API_KEY"):
            raise SystemExit("OPENROUTER_API_KEY is absent; no provider request was made")
    elif options.cap_usd is not None:
        raise SystemExit("--cap-usd is meaningful only with --execute")
    elif options.maximum_calls is not None:
        raise SystemExit("--maximum-calls is meaningful only with --execute")
    if options.prior_run and not options.execute:
        raise SystemExit("--prior-run is meaningful only with --execute")
    if options.replay_prefix_from and not options.execute:
        raise SystemExit("--replay-prefix-from is meaningful only with --execute")
    if options.replay_prefix_from and not options.prior_run:
        raise SystemExit("--replay-prefix-from requires --prior-run")
    if (options.replay_prefix_from and
            options.replay_prefix_from.resolve() != options.prior_run.resolve()):
        raise SystemExit("--replay-prefix-from must name the sealed --prior-run")

    seed = options.seed.resolve()
    manifest, catalog = episode_lab.load_seed(seed, verify_hashes=True)
    baseline_checkpoint = load_baseline_checkpoint(
        options.baseline_checkpoint, manifest
    )
    selected_rows = validate_episode_selection(catalog, options.episode_event_id)
    episode_batches = parse_episode_batches(
        options.episode_batch, options.episode_event_id
    )
    episode_batch_call_limits = parse_episode_batch_call_limits(
        options.episode_batch_call_limit, episode_batches
    )
    baseline_event_id = selected_rows[0].get("before_event_id")
    if (not isinstance(baseline_event_id, int)
            or not 0 < baseline_event_id <= manifest["head_event_id"]):
        raise ValueError("first selected episode has no valid authority cutoff")
    replay_seal = None
    replay_candidates: list[dict] = []
    if options.replay_prefix_from:
        replay_seal, replay_candidates = replay_prefix_rows(
            options.replay_prefix_from
        )
        old_tasks = replay_seal.get("episode_batches")
        if not (
            replay_seal.get("seed_manifest_sha256")
                == base.sha256_file(seed / "manifest.json")
            and replay_seal.get("event_authority_sha256")
                == manifest["events"]["sha256"]
            and replay_seal.get("head_event_id") == manifest["head_event_id"]
            and replay_seal.get("baseline_event_id") == baseline_event_id
            and replay_seal.get("protocol") == PROTOCOL
            and replay_seal.get("ontology_revision") == ONTOLOGY_REVISION
            and replay_seal.get("episode_event_ids") == options.episode_event_id
            and isinstance(replay_seal.get("observed_at"), int)
            and replay_task_prefix_compatible(
                old_tasks, episode_batches, options.episode_event_id
            )
        ):
            raise ValueError("replay source authority or task prefix is incompatible")
    output = require_private_output(options.output)
    output.mkdir(parents=True, exist_ok=False)
    observed_at = (replay_seal["observed_at"] if replay_seal
                   else int(time.time()) + CL_UNIX_EPOCH_OFFSET)
    hashes = source_hashes()
    maximum_calls = (
        options.maximum_calls if options.execute
        else (sum(row["maximum_calls"]
                  for row in episode_batch_call_limits)
              if episode_batch_call_limits
              else ((len(episode_batches) * MAX_CALLS_PER_BATCH)
                    if episode_batches else MAX_TRIAL_CALLS))
    )
    prior = (
        prior_budget_lineage(options.prior_run, seed, manifest,
                             options.episode_event_id, options.cap_usd,
                             maximum_calls)
        if options.prior_run else None
    )
    prior_attempts = prior["call_count"] if prior else 0
    prior_calls = prior["qualified_call_count"] if prior else 0
    prior_exposure = prior["exposure_usd"] if prior else 0.0
    remaining_calls = maximum_calls - prior_calls
    remaining_cap = ((options.cap_usd - prior_exposure)
                     if options.execute else options.cap_usd)
    seal = {
        "schema_version": 1,
        "protocol": PROTOCOL,
        "ontology_revision": ONTOLOGY_REVISION,
        "seed_manifest_sha256": base.sha256_file(seed / "manifest.json"),
        "event_authority_sha256": manifest["events"]["sha256"],
        "head_event_id": manifest["head_event_id"],
        "baseline_event_id": baseline_event_id,
        "baseline_checkpoint": (
            {"path": str(baseline_checkpoint["path"]),
             "sha256": baseline_checkpoint["sha256"],
             "digest": baseline_checkpoint["digest"],
             "contract": baseline_checkpoint["contract"]}
            if baseline_checkpoint else None
        ),
        "episode_event_ids": options.episode_event_id,
        "episode_batches": episode_batches,
        "episode_batch_call_limits": episode_batch_call_limits,
        "episode_count": len(selected_rows),
        "observed_at": observed_at,
        "source_sha256": hashes,
        "model": "meta/muse-glimmer-30b",
        "provider": "phala",
        "zdr": True,
        "data_collection": "deny",
        "maximum_calls": maximum_calls,
        "prior_run": prior,
        "replay_prefix_from": (
            {"path": str(options.replay_prefix_from.resolve()),
             "seal_sha256": base.sha256_file(
                 options.replay_prefix_from / "seal.json")}
            if options.replay_prefix_from else None
        ),
        "remaining_calls_at_start": remaining_calls,
        "remaining_cap_usd_at_start": remaining_cap,
        "minimum_call_interval_seconds": options.minimum_call_interval_seconds,
        "request_ceiling_usd": REQUEST_CEILING_USD,
        "cost_ceiling_usd": options.cap_usd,
        "execute": options.execute,
        "automatic_retries": 0,
        "database_writes": 0,
    }
    driver.save(output / "seal.json", seal)
    args = argparse.Namespace(
        model="meta/muse-glimmer-30b", max_output_tokens=2048,
        max_prompt_price=.33, max_completion_price=1.21,
        cost_ceiling_usd=remaining_cap, request_limit=remaining_calls,
        provider_timeout_seconds=180, transient_retries=0,
        openrouter_zdr="require", openrouter_data_collection="deny",
        openrouter_provider_only="phala", reasoning_policy="low",
    )
    calls = driver.Calls(output, args) if options.execute else None
    worker: QualificationWorker | None = None
    report = {"schema_version": 1, "status": "preparing",
              "provider_calls": prior_calls,
              "provider_attempts": prior_attempts,
              "charged_usd": prior_exposure,
              "prior_provider_calls": prior_calls,
              "prior_provider_attempts": prior_attempts,
              "prior_exposure_usd": prior_exposure, "database_writes": 0}
    try:
        worker = QualificationWorker.start(
            seed, manifest, options.episode_event_id, observed_at,
            options.worker_mode, options.image, baseline_event_id,
            baseline_checkpoint,
            episode_batches, episode_batch_call_limits,
        )
        driver.save(output / "worker-ready.json", worker.ready)
        validate_worker_contract(worker.ready)
        workload_upper_bound = workload_request_upper_bound(worker.ready)
        required_call_cap = required_cumulative_call_cap(
            prior_calls, worker.ready
        )
        report.update(
            source_batch_count=worker.ready["source_batch_count"],
            source_batch_counts=worker.ready["source_batch_counts"],
            request_count_upper_bound=workload_upper_bound,
            cumulative_request_count_upper_bound=required_call_cap,
        )
        if (options.execute and not replay_candidates
                and required_call_cap > maximum_calls):
            report.update(status="stopped-insufficient-call-cap")
            driver.save(output / "report.json", report)
            print(json.dumps({
                "status": report["status"],
                "provider_calls": prior_calls,
                "required_cumulative_call_cap": required_call_cap,
                "database_writes": 0,
                "artifacts": str(output),
            }), flush=True)
            return 2
        result = worker.call({"operation": "next"})
        step = 0
        last_call_completed = None
        replayed = 0
        replay_active = bool(replay_candidates)
        replay_receipts: list[dict] = []
        provider_bound_checked = False
        while result.get("status") == "request":
            step += 1
            ensure_sources_unchanged(hashes)
            phase = result["phase"]
            args.max_output_tokens = phase_output_tokens(phase)
            request = identity_lab.request_from_spec(result["spec"], args)
            request["max_tokens"] = args.max_output_tokens
            bound = base.request_cost_bound(request, args)
            if bound > REQUEST_CEILING_USD:
                raise ValueError(
                    f"request bound {bound:.6f} exceeds the sealed "
                    f"{REQUEST_CEILING_USD:.2f} per-request ceiling")
            driver.save(output / f"step-{step:02d}-request.json", request)
            driver.save(output / f"step-{step:02d}-worker.json", result)
            if not options.execute:
                report.update(status="sealed-no-calls", next_phase=phase,
                              request_bound_usd=bound, worker_ready=worker.ready)
                driver.save(output / "report.json", report)
                print(f"SEALED {output}; no provider requests", flush=True)
                return 0
            ensure_sources_unchanged(hashes)
            stage = (f"task-{result['task_ordinal'] + 1:02d}-"
                     f"batch-{result['batch_index']:02d}-{phase}")
            request_sha256 = base.canonical_sha256(request)
            candidate = (replay_candidates[replayed]
                         if replay_active and replayed < len(replay_candidates)
                         else None)
            if (candidate is not None
                    and candidate["stage"] == stage
                    and replay_request_matches(candidate, request)
                    and (candidate["request_digest"] is None
                         or candidate["request_digest"]
                            == result["request_digest"])):
                response = json.loads(
                    Path(candidate["response_path"]).read_text(encoding="utf-8")
                )
                payload = authority.response_payload(
                    response, result["spec"]["tool_name"]
                )
                replay_receipts.append({
                    **candidate,
                    "request_digest": result["request_digest"],
                    "reused": True,
                })
                replayed += 1
                driver.save(output / "replay.json", replay_receipts)
                result = worker.call({
                    "operation": "response",
                    "phase": phase,
                    "request_digest": result["request_digest"],
                    "response": payload,
                })
                continue
            replay_active = False
            if not provider_bound_checked:
                adjusted_required_call_cap = (
                    prior_calls + workload_upper_bound - replayed
                )
                report.update(
                    replayed_response_count=replayed,
                    cumulative_request_count_upper_bound=(
                        adjusted_required_call_cap
                    ),
                )
                if adjusted_required_call_cap > maximum_calls:
                    report.update(status="stopped-insufficient-call-cap")
                    driver.save(output / "report.json", report)
                    print(json.dumps({
                        "status": report["status"],
                        "provider_calls": prior_calls,
                        "replayed_response_count": replayed,
                        "required_cumulative_call_cap": (
                            adjusted_required_call_cap
                        ),
                        "database_writes": 0,
                        "artifacts": str(output),
                    }), flush=True)
                    return 2
                provider_bound_checked = True
            if last_call_completed is not None:
                delay = (options.minimum_call_interval_seconds
                         - (time.monotonic() - last_call_completed))
                if delay > 0:
                    time.sleep(delay)
            response = calls.call(stage, request)
            last_call_completed = time.monotonic()
            received = calls.rows[-1]
            payload = authority.response_payload(
                response, result["spec"]["tool_name"]
            )
            replay_receipts.append({
                "stage": stage,
                "request_sha256": request_sha256,
                "request_digest": result["request_digest"],
                "response_path": received["response_path"],
                "response_sha256": received["response_sha256"],
                "reused": False,
            })
            driver.save(output / "replay.json", replay_receipts)
            result = worker.call({
                "operation": "response",
                "phase": phase,
                "request_digest": result["request_digest"],
                "response": payload,
            })
        if result.get("status") != "complete":
            raise ValueError("qualification worker returned no terminal result")
        driver.save(output / "result.json", result)
        report.update(
            status="completed-requires-human-review",
            provider_calls=(prior_calls + qualification_call_count(calls.rows)
                            if calls else prior_calls),
            provider_attempts=(prior_attempts + len(calls.rows)
                               if calls else prior_attempts),
            charged_usd=prior_exposure + (calls.budget_used_usd if calls else 0),
            reserved_usd=calls.reserved if calls else 0,
            baseline=result["baseline"], after=result["after"],
            changed_nodes=len(result["nodes"]),
            changed_edges=len(result["edges"]),
            formation_attempts=len(result["formation_attempts"]),
            replayed_response_count=replayed,
        )
        driver.save(output / "report.json", report)
        print(json.dumps({"status": report["status"],
                          "provider_calls": report["provider_calls"],
                          "charged_usd": report["charged_usd"],
                          "changed_nodes": report["changed_nodes"],
                          "changed_edges": report["changed_edges"],
                          "artifacts": str(output)}), flush=True)
        return 0
    except Exception as error:
        effective_rows = (effective_qualification_rows(
            calls.rows, {"status": "stopped", "error": str(error)})
                          if calls else [])
        report.update(
            status="stopped", error=str(error),
            provider_calls=prior_calls + qualification_call_count(effective_rows),
            provider_attempts=(prior_attempts + len(calls.rows)
                               if calls else prior_attempts),
            charged_usd=prior_exposure + (calls.budget_used_usd if calls else 0),
            outstanding_exposure_usd=(prior_exposure
                                      + (calls.budget_used_usd if calls else 0)),
        )
        driver.save(output / "report.json", report)
        raise
    finally:
        if worker is not None:
            worker.close()
            (output / "worker.log").write_text("".join(worker.log), encoding="utf-8")


if __name__ == "__main__":
    raise SystemExit(main())
