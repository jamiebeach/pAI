#!/usr/bin/env python3
"""Run the contained Q4 captured-deliberation demo on the disposable clone.

The cognitive exercise itself is Common Lisp (`clone-conscious-q4.lisp`).
This file is only a cross-platform Docker launcher: no shell evaluation,
PowerShell policy, third-party Python package, provider credential, or chat
transport is involved.
"""

from __future__ import annotations

import argparse
import os
import shutil
import subprocess
import sys
import urllib.request
from pathlib import Path


FIXTURES = (
    "q4-publication-candidate",
    "q4-tool-proposal",
    "q4-yield",
    "q4-invalid-mixed",
)


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Exercise Q4 through the selected :conscious-state runtime."
    )
    parser.add_argument(
        "fixture",
        nargs="?",
        choices=FIXTURES,
        default="q4-publication-candidate",
        help="Named captured-output fixture (default: %(default)s).",
    )
    parser.add_argument(
        "--runtime",
        choices=("host", "docker"),
        default="host",
        help="Run pAI in host SBCL (default) or in the legacy Docker image.",
    )
    parser.add_argument(
        "--setup",
        action="store_true",
        help="Install/update project-local Quicklisp dependencies, then exit.",
    )
    return parser.parse_args()


def docker_path(path: Path) -> str:
    """Use Docker Desktop's portable forward-slash volume spelling."""
    return path.resolve().as_posix()


def run_checked(arguments: list[str]) -> None:
    try:
        completed = subprocess.run(arguments, check=False)
    except FileNotFoundError:
        raise SystemExit("docker was not found on PATH") from None
    if completed.returncode:
        raise SystemExit(completed.returncode)


def local_sbcl(repo: Path) -> Path:
    configured = os.environ.get("PAI_SBCL")
    discovered = shutil.which("sbcl")
    candidates = [Path(configured)] if configured else []
    if os.name == "nt":
        candidates.append(
            repo / ".tools" / "sbcl" / "PFiles" /
            "Steel Bank Common Lisp" / "sbcl.exe"
        )
    candidates.append(Path(discovered) if discovered else None)
    for candidate in candidates:
        if candidate and candidate.is_file():
            return candidate.resolve()
    raise SystemExit(
        "native SBCL was not found; set PAI_SBCL or install the project-local runtime"
    )


def quicklisp_setup(repo: Path) -> Path:
    configured = os.environ.get("PAI_QUICKLISP_SETUP")
    candidates = [
        Path(configured) if configured else None,
        repo / ".tools" / "quicklisp" / "setup.lisp",
        Path.home() / "quicklisp" / "setup.lisp",
    ]
    for candidate in candidates:
        if candidate and candidate.is_file():
            return candidate.resolve()
    raise SystemExit(
        "Quicklisp was not found; run this CLI once with --setup"
    )


def native_environment(sbcl: Path) -> dict[str, str]:
    environment = os.environ.copy()
    if sbcl.name.casefold() == "sbcl.exe" and "SBCL_HOME" not in environment:
        environment["SBCL_HOME"] = str(sbcl.parent)
    git_openssl = Path(r"C:\Program Files\Git\mingw64\bin")
    if os.name == "nt" and (git_openssl / "libcrypto-3-x64.dll").is_file():
        environment["PATH"] = f"{git_openssl}{os.pathsep}{environment.get('PATH', '')}"
    return environment


def setup_local_lisp(repo: Path) -> None:
    sbcl = local_sbcl(repo)
    tools = repo / ".tools"
    bootstrap = tools / "quicklisp.lisp"
    tools.mkdir(parents=True, exist_ok=True)
    if not bootstrap.is_file():
        try:
            urllib.request.urlretrieve(
                "https://beta.quicklisp.org/quicklisp.lisp", bootstrap
            )
        except OSError as condition:
            raise SystemExit(f"could not download Quicklisp: {condition}") from None
    completed = subprocess.run(
        [
            str(sbcl),
            "--noinform",
            "--non-interactive",
            "--load",
            str(repo / "scripts" / "bootstrap-local-lisp.lisp"),
        ],
        check=False,
        cwd=repo,
        env=native_environment(sbcl),
    )
    if completed.returncode:
        raise SystemExit(completed.returncode)


def clone_port(postgres: str) -> str:
    configured = os.environ.get("PAI_CLONE_PG_PORT")
    if configured:
        if configured.isdigit() and 1 <= int(configured) <= 65535:
            return configured
        raise SystemExit(f"invalid PAI_CLONE_PG_PORT: {configured!r}")
    try:
        completed = subprocess.run(
            ["docker", "port", postgres, "5432/tcp"],
            check=False,
            capture_output=True,
            text=True,
        )
    except FileNotFoundError:
        raise SystemExit("docker was not found on PATH") from None
    if completed.returncode:
        raise SystemExit(completed.returncode)
    for line in completed.stdout.splitlines():
        address, separator, port = line.rpartition(":")
        if separator and port.isdigit() and address in ("127.0.0.1", "localhost"):
            return port
    raise SystemExit(
        f"{postgres!r} does not publish PostgreSQL on a loopback host port"
    )


def host_environment(repo: Path, state: Path, postgres: str) -> dict[str, str]:
    environment = os.environ.copy()
    environment.update(
        {
            "PAI_PG_BACKUP": "off",
            "PAI_PG_HOST": "127.0.0.1",
            "PAI_PG_PORT": clone_port(postgres),
            "PAI_PG_DATABASE": "pai_memory",
            "PAI_PG_USER": "pai",
            "PAI_PG_PASSWORD": "pai_local_dev_only",
            "PAI_DEV_DATABASE_LABEL": postgres,
            "PAI_COGNITION_RUNTIME": "conscious-state",
            "PAI_Q4_FIXTURE": "q4-publication-candidate",
            "PAI_SOURCE_ROOT": str(repo / "src"),
            "PAI_TEMPLATES": str(repo / "templates"),
            "PAI_STATE_ROOT": str(state),
            "PAI_QUICKLISP_SETUP": str(quicklisp_setup(repo)),
        }
    )
    return environment


def run_host(repo: Path, state: Path, postgres: str, fixture: str) -> None:
    sbcl = local_sbcl(repo)
    environment = host_environment(repo, state, postgres)
    native = native_environment(sbcl)
    environment["PATH"] = native.get("PATH", environment.get("PATH", ""))
    if "SBCL_HOME" in native:
        environment["SBCL_HOME"] = native["SBCL_HOME"]
    environment["PAI_Q4_FIXTURE"] = fixture
    try:
        completed = subprocess.run(
            [
                str(sbcl),
                "--dynamic-space-size",
                "4096",
                "--non-interactive",
                "--load",
                str(repo / "scripts" / "clone-conscious-q4.lisp"),
            ],
            check=False,
            cwd=repo,
            env=environment,
        )
    except FileNotFoundError:
        raise SystemExit(f"SBCL was not found at {sbcl}") from None
    if completed.returncode:
        raise SystemExit(completed.returncode)


def main() -> int:
    args = parse_args()
    repo = Path(__file__).resolve().parent.parent
    if args.setup:
        setup_local_lisp(repo)
        return 0
    state = repo / ".clone-state"
    if not state.is_dir():
        raise SystemExit(
            f"clone state is absent at {state}; restore the disposable clone first"
        )

    postgres = os.environ.get("PAI_CLONE_POSTGRES", "pai-clone-postgres")
    if "clone" not in postgres.casefold():
        raise SystemExit(f"refusing non-clone Postgres container name: {postgres!r}")
    run_checked(["docker", "start", postgres])

    if args.runtime == "host":
        run_host(repo, state, postgres, args.fixture)
        return 0

    image = os.environ.get("PAI_IMAGE", "pai:dev")
    network = os.environ.get("PAI_CLONE_NET", "pai-clone-net")
    command = [
        "docker",
        "run",
        "--rm",
        "--network",
        network,
        "-v",
        f"{docker_path(repo)}:/pai:ro",
        "-v",
        f"{docker_path(state)}:/agent/state",
        "-e",
        "PAI_PG_BACKUP=off",
        "-e",
        f"PAI_PG_HOST={postgres}",
        "-e",
        "PAI_PG_PORT=5432",
        "-e",
        "PAI_PG_DATABASE=pai_memory",
        "-e",
        "PAI_PG_USER=pai",
        "-e",
        "PAI_PG_PASSWORD=pai_local_dev_only",
        "-e",
        "PAI_COGNITION_RUNTIME=conscious-state",
        "-e",
        f"PAI_Q4_FIXTURE={args.fixture}",
        "--entrypoint",
        "sbcl",
        image,
        "--dynamic-space-size",
        "4096",
        "--non-interactive",
        "--load",
        "/pai/scripts/clone-conscious-q4.lisp",
    ]
    run_checked(command)
    return 0


if __name__ == "__main__":
    sys.exit(main())
