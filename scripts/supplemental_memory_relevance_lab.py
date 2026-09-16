#!/usr/bin/env python3
"""Run fixed supplemental-memory fixtures through production Lisp offline."""
from __future__ import annotations

import argparse
import hashlib
import json
import os
from pathlib import Path
import subprocess


def run(repo: Path, fixture: Path, output: Path) -> dict:
    from conscious_q4_cli import local_sbcl, native_environment, quicklisp_setup

    sbcl = local_sbcl(repo)
    env = native_environment(sbcl)
    cache = repo / ".clone-state" / "host-cache"
    cache.mkdir(parents=True, exist_ok=True)
    env.update(
        LOCALAPPDATA=str(cache),
        XDG_CACHE_HOME=str(cache),
        PAI_QUICKLISP_SETUP=str(quicklisp_setup(repo)),
        PAI_REPOSITORY=str(repo) + os.sep,
        PAI_SUPPLEMENTAL_MEMORY_BUNDLE=str(fixture),
        PAI_SUPPLEMENTAL_MEMORY_OUTPUT=str(output),
    )
    completed = subprocess.run(
        [str(sbcl), "--script", str(repo / "scripts/supplemental-memory-relevance-lab.lisp")],
        env=env, capture_output=True, text=True, timeout=180,
    )
    if completed.returncode:
        raise RuntimeError(completed.stdout + completed.stderr)
    result = json.loads(output.read_text(encoding="utf-8"))
    result["fixture_sha256"] = hashlib.sha256(fixture.read_bytes()).hexdigest()
    result["durable_writes"] = 0
    output.write_text(json.dumps(result, indent=2) + "\n", encoding="utf-8")
    return result


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("--fixture", type=Path, required=True)
    parser.add_argument("--output", type=Path, required=True)
    args = parser.parse_args()
    repo = Path(__file__).resolve().parents[1]
    result = run(repo, args.fixture.resolve(), args.output.resolve())
    print(f"supplemental-memory-relevance-lab: {result['case_count']} cases, 0 durable writes")


if __name__ == "__main__":
    main()
