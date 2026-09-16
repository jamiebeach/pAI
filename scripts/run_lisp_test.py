#!/usr/bin/env python3
"""Run one inherited Lisp suite in a clean native SBCL process.

This is the cross-platform counterpart to tests/run-isolated.sh.  It keeps
test orchestration out of PowerShell while preserving the one-suite-per-image
isolation contract.
"""

from __future__ import annotations

import argparse
import os
from pathlib import Path
import subprocess
import sys
import re


def parser() -> argparse.ArgumentParser:
    result = argparse.ArgumentParser()
    result.add_argument("suite", help="suite filename below tests/, or an absolute path")
    result.add_argument(
        "--clone-postgres-port",
        type=int,
        help=(
            "connect read-only qualification suites to the local labelled "
            "PostgreSQL clone on this port"
        ),
    )
    return result


def find_sbcl(repo: Path) -> Path:
    configured = os.environ.get("PAI_SBCL")
    candidates = [
        Path(configured) if configured else None,
        repo / ".tools" / "sbcl" / "PFiles" / "Steel Bank Common Lisp" / "sbcl.exe",
        repo / ".tools" / "sbcl" / "bin" / "sbcl",
    ]
    for candidate in candidates:
        if candidate and candidate.is_file():
            return candidate.resolve()
    raise SystemExit("native SBCL not found; set PAI_SBCL or run the local Lisp setup")


def main() -> int:
    args = parser().parse_args()
    repo = Path(__file__).resolve().parent.parent
    suite = Path(args.suite)
    if not suite.is_absolute():
        suite = repo / "tests" / suite
    suite = suite.resolve()
    tests = (repo / "tests").resolve()
    if suite.parent != tests or not suite.is_file():
        raise SystemExit(f"suite must be an existing file directly below {tests}")

    sbcl = find_sbcl(repo)
    environment = os.environ.copy()
    environment.update({"PAI_ROOT": str(repo) + os.sep, "SUITE": str(suite)})
    if args.clone_postgres_port is not None:
        if not 1 <= args.clone_postgres_port <= 65535:
            raise SystemExit("--clone-postgres-port must be between 1 and 65535")
        environment.update(
            {
                "PAI_PG_BACKUP": "off",
                "PAI_PG_HOST": "127.0.0.1",
                "PAI_PG_PORT": str(args.clone_postgres_port),
                "PAI_PG_DATABASE": "pai_memory",
                "PAI_PG_USER": "pai",
                "PAI_PG_PASSWORD": "pai_local_dev_only",
                "PAI_DEV_DATABASE_LABEL": "clone",
            }
        )
    # Select an OS/runtime SQLite library without committing a machine path or
    # requiring a PowerShell harness. An explicit operator setting wins.
    sqlite_candidates = []
    if os.name == "nt":
        windows = Path(os.environ.get("SystemRoot", r"C:\Windows"))
        sqlite_candidates.append(windows / "System32" / "winsqlite3.dll")
        sqlite_candidates.append(
            Path(sys.executable).resolve().parent / "DLLs" / "sqlite3.dll"
        )
    for sqlite_library in sqlite_candidates:
        if sqlite_library.is_file():
            environment.setdefault("PAI_SQLITE_LIBRARY", str(sqlite_library))
            environment["PATH"] = (
                f"{sqlite_library.parent}{os.pathsep}{environment.get('PATH', '')}"
            )
            break
    if sbcl.name.casefold() == "sbcl.exe":
        environment.setdefault("SBCL_HOME", str(sbcl.parent))
        git_openssl = Path(r"C:\Program Files\Git\mingw64\bin")
        if (git_openssl / "libcrypto-3-x64.dll").is_file():
            environment["PATH"] = f"{git_openssl}{os.pathsep}{environment.get('PATH', '')}"

    configured_quicklisp = os.environ.get("PAI_QUICKLISP_SETUP")
    quicklisp_candidates = [
        Path(configured_quicklisp) if configured_quicklisp else None,
        repo / ".tools" / "quicklisp" / "setup.lisp",
    ]
    quicklisp = next(
        (candidate.resolve() for candidate in quicklisp_candidates
         if candidate and candidate.is_file()),
        None,
    )
    command = [str(sbcl), "--dynamic-space-size", "3072", "--non-interactive"]
    if quicklisp:
        command.extend(["--load", str(quicklisp)])
    command.extend(["--eval", "(require :asdf)",
                    "--load", str(tests / "isolated-harness.lisp")])
    completed = subprocess.run(
        command,
        cwd=repo,
        env=environment,
        stdout=subprocess.PIPE,
        stderr=subprocess.STDOUT,
        text=True,
        errors="replace",
        check=False,
    )
    print(completed.stdout, end="")
    tally = re.findall(r"(\d+) passed, (\d+) failed", completed.stdout)
    if tally:
        return 1 if int(tally[-1][1]) else completed.returncode
    if "HARNESS-ERR:" in completed.stdout:
        return 1
    return completed.returncode


if __name__ == "__main__":
    sys.exit(main())
