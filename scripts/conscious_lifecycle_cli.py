#!/usr/bin/env python3
"""Inspect a real Q5 lifecycle across native pAI process restarts."""

from __future__ import annotations

import argparse
import json
import os
from pathlib import Path
import subprocess

from conscious_q4_cli import local_sbcl, native_environment, quicklisp_setup


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Run one providerless Q5 lifecycle action in a fresh process."
    )
    parser.add_argument(
        "--state-dir",
        type=Path,
        help="durable demo state (default: .q5-lifecycle-state in the repository)",
    )
    commands = parser.add_subparsers(dest="action", required=True)

    create = commands.add_parser("create", help="create one real deferred intention")
    create.add_argument("--subject", required=True)
    create.add_argument("--aim", required=True)

    commands.add_parser("inspect", help="rebuild and inspect existing lifecycle state")

    ready = commands.add_parser("ready", help="record a bounded ready result summary")
    ready.add_argument("--result-summary", required=True)

    complete = commands.add_parser(
        "complete", help="observe a matching public result and satisfy ready work"
    )
    complete.add_argument("--observed-reply", required=True)

    commands.add_parser("cancel", help="cancel the current active intention")
    return parser.parse_args()


def main() -> int:
    args = parse_args()
    repo = Path(__file__).resolve().parent.parent
    state = (args.state_dir or repo / ".q5-lifecycle-state").resolve()
    state.mkdir(parents=True, exist_ok=True)
    sbcl = local_sbcl(repo)
    environment = os.environ.copy()
    environment.pop("OPENROUTER_API_KEY", None)
    environment.pop("PAI_API_KEY", None)
    native = native_environment(sbcl)
    environment["PATH"] = native.get("PATH", environment.get("PATH", ""))
    if "SBCL_HOME" in native:
        environment["SBCL_HOME"] = native["SBCL_HOME"]
    environment.update(
        {
            "PAI_QUICKLISP_SETUP": str(quicklisp_setup(repo)),
            "PAI_SOURCE_ROOT": str(repo / "src"),
            "PAI_TEMPLATES": str(repo / "templates"),
            "PAI_STATE_ROOT": str(state),
            "PAI_NEAR_TERM_INTENTIONS": str(state / "near-term-intentions.json"),
            "PAI_AGENT_ID": "q5-lifecycle-dev",
            "PAI_COGNITION_RUNTIME": "conscious-state",
            "PAI_PG_BACKUP": "off",
            "PAI_MODEL_ENDPOINT": "http://127.0.0.1:1/q5-provider-disabled",
            "PAI_Q5_LIFECYCLE_ACTION": args.action,
        }
    )
    values = {
        "PAI_Q5_LIFECYCLE_SUBJECT": getattr(args, "subject", None),
        "PAI_Q5_LIFECYCLE_AIM": getattr(args, "aim", None),
        # Retain the Q5-era environment name as a private launcher/Lisp
        # compatibility seam; the operator-facing concept is a bounded result
        # summary, never the artifact itself.
        "PAI_Q5_LIFECYCLE_ARTIFACT": getattr(args, "result_summary", None),
        "PAI_Q5_LIFECYCLE_OBSERVED_REPLY": getattr(args, "observed_reply", None),
    }
    environment.update({key: value for key, value in values.items() if value is not None})
    cache = state / "host-cache"
    cache.mkdir(parents=True, exist_ok=True)
    environment["LOCALAPPDATA"] = str(cache)
    environment["XDG_CACHE_HOME"] = str(cache)

    completed = subprocess.run(
        [
            str(sbcl),
            "--dynamic-space-size",
            "3072",
            "--script",
            str(repo / "scripts" / "conscious-lifecycle-scenario.lisp"),
        ],
        cwd=repo,
        env=environment,
        capture_output=True,
        text=True,
        errors="replace",
        check=False,
    )
    begin = "CONSCIOUS-Q5-SCENARIO-BEGIN"
    end = "CONSCIOUS-Q5-SCENARIO-END"
    if completed.returncode or begin not in completed.stdout or end not in completed.stdout:
        diagnostic = (completed.stderr or completed.stdout).strip()
        raise SystemExit(diagnostic or f"Q5 scenario failed with exit {completed.returncode}")
    payload = completed.stdout.split(begin, 1)[1].split(end, 1)[0].strip()
    print(json.dumps(json.loads(payload), indent=2, ensure_ascii=False))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
