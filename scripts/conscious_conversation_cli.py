#!/usr/bin/env python3
"""Run the Q4.5 conscious conversation natively; Docker owns Postgres only."""

from __future__ import annotations

import argparse
import os
from pathlib import Path
import subprocess
import sys

from conscious_q4_cli import (
    local_sbcl, native_environment, quicklisp_setup, setup_local_lisp,
)


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Hold a persisted conversation with the :conscious-state runtime."
    )
    parser.add_argument(
        "--endpoint",
        default="http://127.0.0.1:1234/api/v1/chat",
        help="loopback LM Studio native chat or OpenAI-compatible endpoint",
    )
    parser.add_argument(
        "--model", default="qwen/qwen3.5-9b", help="explicit local model id"
    )
    parser.add_argument(
        "--context-profile",
        default="solicited-conversation-dev",
        help="named profile from config/conscious-context-profiles.json",
    )
    parser.add_argument("--message", help="run one turn and exit instead of prompting")
    parser.add_argument(
        "--show-rejected",
        action="store_true",
        help="print rejected private model output for this disposable dev run",
    )
    parser.add_argument(
        "--setup", action="store_true", help="install project-local Lisp dependencies"
    )
    return parser.parse_args()


def main() -> int:
    args = parse_args()
    repo = Path(__file__).resolve().parent.parent
    if args.setup:
        setup_local_lisp(repo)
        return 0
    state = repo / ".clone-state"
    if not state.is_dir():
        raise SystemExit(f"clone state is absent at {state}")

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
            "PAI_STATE_ROOT": str(state),
            "PAI_COGNITION_RUNTIME": "conscious-state",
            "PAI_PG_BACKUP": "off",
            "PAI_PG_HOST": "127.0.0.1",
            "PAI_DEV_DATABASE_LABEL": "q45-clone-no-database-connection",
            "PAI_AGENT_ID": "q45-conversation-dev",
            "PAI_CONVERSATION_ENDPOINT": args.endpoint,
            "PAI_CONVERSATION_MODEL": args.model,
            "PAI_CONVERSATION_PERSONA": "dev",
            "PAI_CONVERSATION_CONTEXT_PROFILE": args.context_profile,
            "PAI_CONSCIOUS_CONTEXT_PROFILES": str(
                repo / "config" / "conscious-context-profiles.json"
            ),
        }
    )
    # ASDF's Windows cache normally lives under the user's LocalAppData.
    # Keep its path-preserving cache hierarchy in the disposable workspace.
    host_cache = state / "host-cache"
    host_cache.mkdir(parents=True, exist_ok=True)
    environment["LOCALAPPDATA"] = str(host_cache)
    environment["XDG_CACHE_HOME"] = str(host_cache)
    if args.message:
        environment["PAI_CONVERSATION_MESSAGE"] = args.message
    if args.show_rejected:
        environment["PAI_CONVERSATION_SHOW_REJECTED"] = "1"

    completed = subprocess.run(
        [str(sbcl), "--dynamic-space-size", "3072", "--script",
         str(repo / "scripts" / "conscious-conversation.lisp")],
        cwd=repo,
        env=environment,
        check=False,
    )
    return completed.returncode


if __name__ == "__main__":
    sys.exit(main())
