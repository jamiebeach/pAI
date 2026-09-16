#!/usr/bin/env python3
"""Launch an operator-sealed OpenRouter synthetic context experiment."""

from __future__ import annotations

import argparse
import json
import os
import subprocess
import sys
from pathlib import Path

from conscious_q4_cli import local_sbcl, native_environment, quicklisp_setup


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Run a sealed OpenRouter context laboratory corpus."
    )
    parser.add_argument("--execute", action="store_true", help="permit paid requests")
    parser.add_argument("--validate", action="store_true", help="load Lisp and validate the seal without network")
    parser.add_argument("--approved-requests", type=int)
    parser.add_argument("--cost-ceiling-usd", type=float)
    parser.add_argument(
        "--seal",
        default="openrouter-experiment-seal.json",
        help="seal filename below dev/context-lab/",
    )
    return parser.parse_args()


def main() -> int:
    args = parse_args()
    repo = Path(__file__).resolve().parent.parent
    seal_dir = (repo / "dev" / "context-lab").resolve()
    seal_path = (seal_dir / args.seal).resolve()
    if seal_path.parent != seal_dir or not seal_path.is_file():
        raise SystemExit("--seal must name an existing file directly below dev/context-lab/")
    seal = json.loads(seal_path.read_text(encoding="utf-8"))
    fixture = (repo / seal["fixture"]).resolve()
    if fixture.parent != seal_dir or not fixture.is_file():
        raise SystemExit("sealed fixture must be an existing file below dev/context-lab/")

    if not args.execute and not args.validate:
        print(json.dumps(seal, indent=2))
        print("CHECK-ONLY: add --execute with the exact approved request and cost bounds.")
        return 0
    if args.execute and seal.get("status") != "approved":
        raise SystemExit(
            f"experiment seal is {seal.get('status', 'not-approved')}; "
            "no further paid run is authorized"
        )
    if args.execute and args.approved_requests != seal["request_limit"]:
        raise SystemExit("--approved-requests must exactly match the sealed request limit")
    if args.execute and args.cost_ceiling_usd != seal["cumulative_cost_ceiling_usd"]:
        raise SystemExit("--cost-ceiling-usd must exactly match the sealed ceiling")
    if args.execute and not os.environ.get("OPENROUTER_API_KEY"):
        raise SystemExit("OPENROUTER_API_KEY is missing; no request was made")

    sbcl = local_sbcl(repo)
    environment = os.environ.copy()
    native = native_environment(sbcl)
    environment["PATH"] = native.get("PATH", environment.get("PATH", ""))
    if "SBCL_HOME" in native:
        environment["SBCL_HOME"] = native["SBCL_HOME"]
    environment.update(
        {
            "PAI_CONTEXT_LAB_LIBRARY_ONLY": "1",
            "PAI_QUICKLISP_SETUP": str(quicklisp_setup(repo)),
            "PAI_SOURCE_ROOT": str(repo / "src"),
            "PAI_TEMPLATES": str(repo / "templates"),
            "PAI_CONTEXT_LAB_FIXTURE": str(fixture),
            "PAI_OPENROUTER_EXPERIMENT_SEAL": str(seal_path),
            "PAI_CONSCIOUS_PROVIDER_PROFILES": str(repo / "config" / "conscious-provider-profiles.json"),
            "PAI_CONSCIOUS_CONTEXT_PROFILES": str(repo / "config" / "conscious-context-profiles.json"),
        }
    )
    if args.validate:
        environment["PAI_OPENROUTER_VALIDATE_ONLY"] = "1"
    if args.execute:
        environment["PAI_OPENROUTER_EXECUTE"] = "1"
    cache = repo / ".clone-state" / "host-cache"
    cache.mkdir(parents=True, exist_ok=True)
    environment["LOCALAPPDATA"] = str(cache)
    environment["XDG_CACHE_HOME"] = str(cache)

    completed = subprocess.run(
        [str(sbcl), "--dynamic-space-size", "3072", "--script",
         str(repo / "scripts" / "openrouter-context-lab.lisp")],
        cwd=repo,
        env=environment,
        check=False,
    )
    return completed.returncode


if __name__ == "__main__":
    sys.exit(main())
