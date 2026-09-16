#!/usr/bin/env python3
"""Provider-free graph/final-context replay of an explicit, quiescent snapshot.

Never opens the input database: copies its SQLite file set, verifies stable
bytes, then backs up the copy. Detailed output is private, not source evidence.
This qualifies lexical graph context, not live embedding retrieval or answers.
"""
from __future__ import annotations

import argparse
from contextlib import closing
import hashlib
import json
import os
from pathlib import Path
import shutil
import sqlite3
import subprocess
import sys

from run_lisp_test import find_sbcl


def file_set_hashes(database: Path) -> dict[str, str | None]:
    result = {}
    for suffix in ("", "-wal", "-shm", "-journal"):
        path = Path(str(database) + suffix)
        if not path.exists():
            result[suffix] = None
            continue
        digest = hashlib.sha256()
        with path.open("rb") as stream:
            for block in iter(lambda: stream.read(1024 * 1024), b""):
                digest.update(block)
        result[suffix] = digest.hexdigest()
    return result


def copy_snapshot(source: Path, output: Path) -> tuple[Path, dict]:
    before = file_set_hashes(source)
    if before[""] is None:
        raise ValueError("snapshot database is absent")
    copied = output / "copied.sqlite3"
    for suffix, digest in before.items():
        if digest is not None:
            shutil.copyfile(Path(str(source) + suffix), Path(str(copied) + suffix))
    if before != file_set_hashes(source) or before != file_set_hashes(copied):
        raise ValueError("snapshot changed while copying; supply a quiescent snapshot")
    replay = output / "replay.sqlite3"
    # Recovery/checkpoint side effects, if needed, affect only our new copy.
    with closing(sqlite3.connect(copied)) as src, closing(sqlite3.connect(replay)) as dst:
        src.backup(dst)
        if dst.execute("PRAGMA integrity_check").fetchone() != ("ok",):
            raise ValueError("snapshot integrity check failed")
        # Backup retains WAL journal mode. Normalize only this disposable copy
        # so a read-only reader does not create fresh WAL/SHM sidecars.
        if dst.execute("PRAGMA journal_mode=DELETE").fetchone() != ("delete",):
            raise ValueError("could not seal disposable replay journal mode")
    return replay, before


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--snapshot", type=Path, required=True)
    parser.add_argument("--metadata", type=Path, required=True)
    parser.add_argument("--cases", type=Path, required=True,
                        help="private JSON array of query strings")
    parser.add_argument("--output", type=Path, required=True,
                        help="new private artifact directory; must not exist")
    parser.add_argument("--character-budget", type=int, default=1600)
    args = parser.parse_args()
    if args.character_budget <= 0:
        parser.error("character budget must be positive")
    metadata = json.loads(args.metadata.read_text(encoding="utf-8"))
    for key in ("agent_id", "persona_id", "event_storage_id"):
        if not isinstance(metadata.get(key), str) or not metadata[key]:
            parser.error("metadata requires explicit partition and event-storage identity")
    cases = json.loads(args.cases.read_text(encoding="utf-8"))
    if not isinstance(cases, list) or not 1 <= len(cases) <= 32 or not all(
            isinstance(query, str) and 0 < len(query) <= 2048 for query in cases):
        parser.error("cases must contain 1..32 nonempty query strings, each <=2048 characters")
    source = args.snapshot.resolve(strict=True)
    output = args.output.resolve()
    output.mkdir(parents=True, exist_ok=False)
    replay, original_hashes = copy_snapshot(source, output)
    replay_hashes = file_set_hashes(replay)
    config = dict(metadata, queries=cases, database=str(replay),
                  character_budget=args.character_budget,
                  result=str(output / "private-contexts.json"))
    config_path = output / "private-config.json"
    config_path.write_text(json.dumps(config), encoding="utf-8")
    repo = Path(__file__).resolve().parent.parent
    sbcl = find_sbcl(repo)
    env = os.environ.copy()
    # No provider or runtime credentials are needed by this replay.
    for key in list(env):
        if any(part in key.upper() for part in ("API_KEY", "TOKEN", "PASSWORD")):
            env.pop(key)
    env.update(PAI_ROOT=str(repo) + os.sep,
               SUITE=str(repo / "scripts" / "graph-snapshot-replay.lisp"),
               PAI_TEST_STATE=str(output / "runtime"),
               PAI_GRAPH_REPLAY_CONFIG=str(config_path))
    if os.name == "nt":
        env.setdefault("SBCL_HOME", str(sbcl.parent))
        env.setdefault("PAI_SQLITE_LIBRARY", str(Path(env.get("SystemRoot", r"C:\Windows"))
                                                  / "System32" / "winsqlite3.dll"))
        openssl = Path(r"C:\Program Files\Git\mingw64\bin")
        if openssl.is_dir():
            env["PATH"] = str(openssl) + os.pathsep + env.get("PATH", "")
    quicklisp = Path(env.get("PAI_QUICKLISP_SETUP", str(repo / ".tools/quicklisp/setup.lisp")))
    command = [str(sbcl), "--dynamic-space-size", "3072", "--non-interactive"]
    if quicklisp.is_file():
        command.extend(["--load", str(quicklisp)])
    command.extend(["--eval", "(require :asdf)", "--load",
                    str(repo / "tests/isolated-harness.lisp")])
    try:
        with (output / "private-runtime.log").open("w", encoding="utf-8") as log:
            completed = subprocess.run(command, cwd=repo, env=env, stdout=log,
                                       stderr=subprocess.STDOUT, timeout=300, check=False)
    finally:
        unchanged = original_hashes == file_set_hashes(source)
        replay_unchanged = replay_hashes == file_set_hashes(replay)
        (output / "integrity.json").write_text(json.dumps({
            "input_unchanged": unchanged, "replay_unchanged": replay_unchanged,
            "input_file_hashes": original_hashes}), encoding="utf-8")
    if not unchanged or not replay_unchanged:
        raise RuntimeError("database file set changed during read-only replay")
    result = output / "private-contexts.json"
    if completed.returncode or not result.is_file():
        raise RuntimeError("replay failed; inspect the private runtime log")
    rows = json.loads(result.read_text(encoding="utf-8"))
    if len(rows) != len(cases):
        raise RuntimeError("incomplete replay result")
    for index, row in enumerate(rows, 1):
        report = row["attention_report"]
        print(f"case {index}: paths={row['search_result'].get('path_count', 0)} "
              f"typed={report.get('typed_record_count', 0)} "
              f"source={report.get('source_record_count', 0)} "
              f"final={len(row['assembled']['private_request'])} "
              f"elapsed_ms={row['elapsed_ms']}")
    print("Input and replay SQLite file sets unchanged. No provider calls.")
    return 0


if __name__ == "__main__":
    sys.exit(main())
