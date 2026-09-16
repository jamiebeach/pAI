#!/usr/bin/env python3
"""Run disposable recursive-curiosity scenarios through production functions."""

from __future__ import annotations

import argparse
import os
import subprocess
import sys
from datetime import datetime, timezone
from pathlib import Path

from conscious_q4_cli import local_sbcl, native_environment, quicklisp_setup


SCENARIOS = (
    "attention-starvation",
    "attention-quiescence",
    "private-briefing",
    "curiosity-consolidation",
)


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Exercise recursive curiosity against disposable event histories."
    )
    parser.add_argument("--scenario", choices=("all", *SCENARIOS), default="all")
    parser.add_argument("--provider", choices=("mock", "openrouter"), default="mock")
    parser.add_argument("--model")
    parser.add_argument("--execute", action="store_true", help="permit paid OpenRouter requests")
    parser.add_argument("--validate", action="store_true", help="validate paid-mode bounds without network")
    parser.add_argument("--request-limit", type=int)
    parser.add_argument("--cost-ceiling-usd", type=float)
    parser.add_argument("--max-prompt-price", type=float, default=0.20,
                        help="OpenRouter maximum prompt USD per million tokens")
    parser.add_argument("--max-completion-price", type=float, default=0.40,
                        help="OpenRouter maximum completion USD per million tokens")
    parser.add_argument(
        "--openrouter-zdr",
        choices=("require", "allow-non-zdr"),
        default="require",
        help="OpenRouter ZDR routing policy for the sealed lab",
    )
    parser.add_argument("--runtime", choices=("host", "docker"), default="host")
    parser.add_argument("--image", default="pai-local:development")
    parser.add_argument("--artifacts", type=Path)
    return parser.parse_args()


def provider_environment(args: argparse.Namespace) -> dict[str, str]:
    model = args.model or (
        "scripted-native-tools-v1" if args.provider == "mock" else "xiaomi/mimo-v2.5"
    )
    values = {
        "PAI_CURIOSITY_LAB_PROVIDER": args.provider,
        "PAI_CURIOSITY_LAB_MODEL": model,
        "PAI_CURIOSITY_LAB_EXECUTE": "1" if args.execute else "0",
        "PAI_CURIOSITY_LAB_VALIDATE": "1" if args.validate else "0",
        "PAI_CURIOSITY_LAB_REQUEST_LIMIT": str(args.request_limit or 0),
        "PAI_CURIOSITY_LAB_COST_CEILING_USD": str(args.cost_ceiling_usd or 0),
        "PAI_CURIOSITY_LAB_MAX_PROMPT_PRICE": str(args.max_prompt_price),
        "PAI_CURIOSITY_LAB_MAX_COMPLETION_PRICE": str(args.max_completion_price),
        "PAI_CURIOSITY_LAB_ZDR": args.openrouter_zdr,
    }
    return values


def validate_provider_args(args: argparse.Namespace) -> None:
    if args.provider == "mock":
        if args.execute or args.validate or args.request_limit or args.cost_ceiling_usd:
            raise SystemExit("paid execution/validation bounds require --provider openrouter")
        return
    if args.scenario == "all":
        raise SystemExit("OpenRouter runs require one explicit --scenario")
    if not args.model:
        raise SystemExit("--provider openrouter requires an explicit --model slug")
    if args.execute == args.validate:
        raise SystemExit("OpenRouter requires exactly one of --execute or --validate")
    if not args.request_limit or args.request_limit < 1:
        raise SystemExit("OpenRouter requires --request-limit >= 1")
    if args.request_limit != 2:
        raise SystemExit("the current sealed OpenRouter scenarios require --request-limit 2")
    if not args.cost_ceiling_usd or args.cost_ceiling_usd <= 0:
        raise SystemExit("OpenRouter requires a positive --cost-ceiling-usd")
    if args.max_prompt_price <= 0 or args.max_completion_price <= 0:
        raise SystemExit("OpenRouter maximum prices must be positive")
    if args.execute and not os.environ.get("OPENROUTER_API_KEY"):
        raise SystemExit("OPENROUTER_API_KEY is missing; no request was made")


def run_host(repo: Path, scenario: str, args: argparse.Namespace, output: Path) -> int:
    sbcl = local_sbcl(repo)
    environment = native_environment(sbcl)
    environment.update(
        {
            "PAI_QUICKLISP_SETUP": str(quicklisp_setup(repo)),
            "PAI_ROOT": str(repo),
            "PAI_CURIOSITY_LAB_SCENARIO": scenario,
            "PAI_CURIOSITY_LAB_ARTIFACT_DIR": str(output),
        }
    )
    environment.update(provider_environment(args))
    if args.execute:
        environment["OPENROUTER_API_KEY"] = os.environ["OPENROUTER_API_KEY"]
    cache = repo / ".clone-state" / "host-cache"
    cache.mkdir(parents=True, exist_ok=True)
    environment["LOCALAPPDATA"] = str(cache)
    environment["XDG_CACHE_HOME"] = str(cache)
    completed = subprocess.run(
        [
            str(sbcl),
            "--dynamic-space-size",
            "3072",
            "--script",
            str(repo / "scripts" / "curiosity-lab.lisp"),
        ],
        cwd=repo,
        env=environment,
        check=False,
    )
    return completed.returncode


def run_docker(repo: Path, scenario: str, args: argparse.Namespace, output: Path) -> int:
    output.mkdir(parents=True, exist_ok=True)
    command = ["docker", "run", "--rm"]
    if not args.execute:
        command.extend(["--network", "none"])
    command.extend(["--tmpfs", "/agent/state", "--tmpfs", "/tmp"])
    command.extend(["-e", f"PAI_CURIOSITY_LAB_SCENARIO={scenario}"])
    command.extend(["-e", "PAI_CURIOSITY_LAB_ARTIFACT_DIR=/artifacts"])
    command.extend(["-e", "PAI_QUICKLISP_SETUP=/opt/quicklisp/setup.lisp"])
    command.extend(["-e", "PAI_ROOT=/pai"])
    for name, value in provider_environment(args).items():
        command.extend(["-e", f"{name}={value}"])
    if args.execute:
        command.extend(["-e", "OPENROUTER_API_KEY"])
    command.extend(
        [
            "-v", f"{repo.resolve()}:/pai:ro",
            "-v", f"{output.resolve()}:/artifacts",
            "-w", "/pai",
            args.image,
            "sbcl", "--dynamic-space-size", "3072",
            "--script", "/pai/scripts/curiosity-lab.lisp",
        ]
    )
    completed = subprocess.run(
        command,
        cwd=repo,
        check=False,
    )
    return completed.returncode


def main() -> int:
    args = parse_args()
    validate_provider_args(args)
    repo = Path(__file__).resolve().parent.parent
    root = (args.artifacts or repo / "artifacts" / "curiosity-lab").resolve()
    scenarios = SCENARIOS if args.scenario == "all" else (args.scenario,)
    run_id = datetime.now(timezone.utc).strftime("%Y%m%dT%H%M%SZ")
    for scenario in scenarios:
        output = root / scenario / run_id
        output.mkdir(parents=True, exist_ok=False)
        if args.runtime == "docker":
            code = run_docker(repo, scenario, args, output)
        else:
            code = run_host(repo, scenario, args, output)
        if code:
            return code
        print(f"PASS {scenario}: {output}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
