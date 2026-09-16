#!/usr/bin/env python3
"""Copy one stopped pAI state tree into an empty container state volume."""

from __future__ import annotations

import argparse
from datetime import datetime, timezone
import json
import os
from pathlib import Path
import shutil
import sys
import tempfile


REPO = Path(__file__).resolve().parent.parent
sys.path.insert(0, str(REPO / "scripts"))

from pai_cli import backup_sqlite_database, inspect_state_databases  # noqa: E402


EXCLUDED_DIRECTORIES = {
    "asdf-cache", "host-cache", "restricted-runtime-home",
    "backups", "conversation-backups", "postgres-backups",
}
EXCLUDED_FILES = {
    "events.sqlite3", "events.sqlite3-wal", "events.sqlite3-shm",
    "derived.sqlite3", "derived.sqlite3-wal", "derived.sqlite3-shm",
}


def _reject_symlinks(root: Path) -> None:
    for path in (root, *root.rglob("*")):
        if path.is_symlink():
            raise SystemExit(f"state import refuses symbolic link {path}")


def import_container_state(source: Path, target: Path) -> dict[str, object]:
    """Install a verified snapshot into an empty target; leave source unchanged."""
    source = source.resolve()
    target = target.resolve()
    if not source.is_dir():
        raise SystemExit(f"state import source is absent at {source}")
    target.mkdir(parents=True, exist_ok=True)
    if any(target.iterdir()):
        raise SystemExit(f"state import target is not empty: {target}")
    _reject_symlinks(source)

    staging = Path(tempfile.mkdtemp(prefix=".pai-state-import-", dir=target))
    try:
        for entry in source.iterdir():
            if entry.name in EXCLUDED_DIRECTORIES or entry.name in EXCLUDED_FILES:
                continue
            destination = staging / entry.name
            if entry.is_dir():
                shutil.copytree(entry, destination)
            elif entry.is_file():
                shutil.copy2(entry, destination)

        # Projection first and authority second preserves the established
        # backup ordering: the event ledger may be ahead, never the projection.
        backup_sqlite_database(
            source / "derived.sqlite3", staging / "derived.sqlite3"
        )
        backup_sqlite_database(
            source / "events.sqlite3", staging / "events.sqlite3"
        )
        evidence = inspect_state_databases(
            staging / "events.sqlite3", staging / "derived.sqlite3"
        )
        manifest: dict[str, object] = {
            "schema_version": 1,
            "status": "complete",
            "created_at": datetime.now(timezone.utc).isoformat(),
            "source": "operator-mounted-stopped-state",
            "excluded_cache_directories": sorted(EXCLUDED_DIRECTORIES),
            "evidence": evidence,
        }
        (staging / "container-state-import.json").write_text(
            json.dumps(manifest, indent=2, sort_keys=True) + "\n",
            encoding="utf-8",
        )
        for entry in tuple(staging.iterdir()):
            os.replace(entry, target / entry.name)
        staging.rmdir()
        return manifest
    finally:
        if staging.exists():
            shutil.rmtree(staging)


def main() -> int:
    parser = argparse.ArgumentParser(
        description="Install stopped pAI state into an empty named volume."
    )
    parser.add_argument("source", type=Path)
    parser.add_argument("target", type=Path)
    args = parser.parse_args()
    report = import_container_state(args.source, args.target)
    evidence = report["evidence"]
    print(
        "Imported verified state through event "
        f"{evidence['maximum_event_id']} at storage position "
        f"{evidence['maximum_storage_position']}"
    )
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
