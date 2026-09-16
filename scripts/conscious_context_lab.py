#!/usr/bin/env python3
"""Run side-effect-free conscious context windows against a loopback model."""

from __future__ import annotations

import argparse
import json
import os
import subprocess
import sys
from pathlib import Path
from urllib.parse import urlparse

from conscious_q4_cli import local_sbcl, native_environment, quicklisp_setup, setup_local_lisp


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Explore named Q4 context assemblies without runtime state or effects."
    )
    parser.add_argument("scenarios", nargs="*", help="scenario ids (default: all)")
    parser.add_argument("--list", action="store_true", help="list scenario ids and exit")
    parser.add_argument("--model", default="qwen/qwen3.5-9b")
    parser.add_argument(
        "--endpoint", default="http://127.0.0.1:1234/api/v1/chat"
    )
    parser.add_argument("--max-output-tokens", type=int, default=2048)
    parser.add_argument(
        "--context-profile",
        default="context-lab",
        help="named profile from config/conscious-context-profiles.json",
    )
    parser.add_argument("--setup", action="store_true")
    parser.add_argument(
        "--fixture",
        default="scenarios.json",
        help="fixture filename below dev/context-lab/",
    )
    return parser.parse_args()


def main() -> int:
    args = parse_args()
    repo = Path(__file__).resolve().parent.parent
    fixture_dir = (repo / "dev" / "context-lab").resolve()
    fixture = (fixture_dir / args.fixture).resolve()
    if fixture.parent != fixture_dir or not fixture.is_file():
        raise SystemExit("--fixture must name an existing file below dev/context-lab/")
    data = json.loads(fixture.read_text(encoding="utf-8"))

    if args.list:
        for scenario in data["scenarios"]:
            print(f"{scenario['id']}: {scenario['description']}")
        return 0
    if args.setup:
        setup_local_lisp(repo)
        return 0
    if not 256 <= args.max_output_tokens <= 4096:
        raise SystemExit("--max-output-tokens must be between 256 and 4096")
    parsed = urlparse(args.endpoint)
    if parsed.scheme not in {"http", "https"} or parsed.hostname not in {
        "127.0.0.1", "localhost", "::1"
    }:
        raise SystemExit("context laboratory endpoint must be loopback-only")

    known = {scenario["id"] for scenario in data["scenarios"]}
    unknown = set(args.scenarios) - known
    if unknown:
        raise SystemExit(f"unknown context-lab scenario(s): {', '.join(sorted(unknown))}")

    sbcl = local_sbcl(repo)
    environment = os.environ.copy()
    native = native_environment(sbcl)
    environment["PATH"] = native.get("PATH", environment.get("PATH", ""))
    if "SBCL_HOME" in native:
        environment["SBCL_HOME"] = native["SBCL_HOME"]
    environment.update(
        {
            "PAI_QUICKLISP_SETUP": str(quicklisp_setup(repo)),
            "PAI_SOURCE_ROOT": str(repo / "src"),
            "PAI_TEMPLATES": str(repo / "templates"),
            "PAI_STATE_ROOT": str(repo / ".clone-state"),
            "PAI_CONTEXT_LAB_FIXTURE": str(fixture),
            "PAI_CONTEXT_LAB_SCENARIOS": ",".join(args.scenarios) if args.scenarios else "all",
            "PAI_CONTEXT_LAB_ENDPOINT": args.endpoint,
            "PAI_CONTEXT_LAB_MODEL": args.model,
            "PAI_CONTEXT_LAB_MAX_OUTPUT_TOKENS": str(args.max_output_tokens),
            "PAI_CONTEXT_LAB_CONTEXT_PROFILE": args.context_profile,
            "PAI_CONSCIOUS_CONTEXT_PROFILES": str(
                repo / "config" / "conscious-context-profiles.json"
            ),
        }
    )
    cache = repo / ".clone-state" / "host-cache"
    cache.mkdir(parents=True, exist_ok=True)
    environment["LOCALAPPDATA"] = str(cache)
    environment["XDG_CACHE_HOME"] = str(cache)

    completed = subprocess.run(
        [str(sbcl), "--dynamic-space-size", "3072", "--script",
         str(repo / "scripts" / "conscious-context-lab.lisp")],
        cwd=repo,
        env=environment,
        check=False,
    )
    return completed.returncode


if __name__ == "__main__":
    sys.exit(main())
