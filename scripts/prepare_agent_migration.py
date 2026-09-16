"""Prepare an immutable legacy-agent ledger for a pAI migration rehearsal.

The source ledger is retained as canonical JSON events.  Historical public
dialogue is additionally represented by non-stimulus migration events so the
current episode projector can consume it without treating old user messages as
new work.  Source-agent and exact source-event provenance remain explicit.
"""

from __future__ import annotations

import argparse
from datetime import datetime, timezone
import hashlib
import json
import os
from pathlib import Path
import re
import tempfile
from typing import Iterable


MIGRATION_SOURCE = "historical-agent-migration-v1"
AGENT_ID = re.compile(r"^[A-Za-z0-9][A-Za-z0-9_.-]{0,127}$")
HISTORICAL_TYPES = {
    "user-message": "historical-user-message-imported",
    "agent-message": "historical-agent-message-imported",
}


def _canonical_json(value: object) -> str:
    return json.dumps(
        value, ensure_ascii=False, sort_keys=True, separators=(",", ":")
    )


def _sha256_text(value: str) -> str:
    return hashlib.sha256(value.encode("utf-8")).hexdigest()


def _sha256_file(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as stream:
        for block in iter(lambda: stream.read(1024 * 1024), b""):
            digest.update(block)
    return digest.hexdigest()


def _required_token(value: object, label: str, maximum: int = 128) -> str:
    if (
        not isinstance(value, str)
        or not value.strip()
        or len(value) > maximum
        or "\x00" in value
    ):
        raise ValueError(f"{label} must be non-empty bounded text")
    return value


def _required_agent_id(value: object, label: str) -> str:
    value = _required_token(value, label)
    if not AGENT_ID.fullmatch(value):
        raise ValueError(f"{label} must be a stable identifier")
    return value


def _parse_timestamp(value: object, label: str) -> datetime:
    if not isinstance(value, str) or not value:
        raise ValueError(f"{label} must be an ISO-8601 timestamp")
    try:
        parsed = datetime.fromisoformat(value.replace("Z", "+00:00"))
    except ValueError as error:
        raise ValueError(f"{label} must be an ISO-8601 timestamp") from error
    if parsed.tzinfo is None:
        raise ValueError(f"{label} must include a timezone")
    return parsed.astimezone(timezone.utc)


def read_canonical_events(
    sources: Iterable[Path],
) -> tuple[list[dict[str, object]], dict[str, object]]:
    """Read, de-duplicate and conflict-check ordered ledger sources by ID."""
    by_id: dict[int, tuple[str, dict[str, object], str, int]] = {}
    source_rows: list[dict[str, object]] = []
    duplicate_count = 0
    for source in sources:
        path = source.resolve()
        if not path.is_file():
            raise ValueError(f"ledger source is absent: {path}")
        row_count = 0
        with path.open(encoding="utf-8") as stream:
            for line_number, line in enumerate(stream, 1):
                if not line.strip():
                    continue
                row_count += 1
                try:
                    event = json.loads(line)
                except json.JSONDecodeError as error:
                    raise ValueError(
                        f"malformed JSON at {path}:{line_number}: {error}"
                    ) from error
                if not isinstance(event, dict):
                    raise ValueError(f"event at {path}:{line_number} is not an object")
                event_id = event.get("id")
                if (
                    not isinstance(event_id, int)
                    or isinstance(event_id, bool)
                    or event_id <= 0
                ):
                    raise ValueError(
                        f"event at {path}:{line_number} has no positive integer ID"
                    )
                _required_token(event.get("type"), "event type", 180)
                _parse_timestamp(event.get("timestamp"), "event timestamp")
                canonical_digest = _sha256_text(_canonical_json(event))
                prior = by_id.get(event_id)
                if prior:
                    if canonical_digest != prior[0]:
                        raise ValueError(
                            "conflicting source events share ID "
                            f"{event_id}: {prior[2]}:{prior[3]} and "
                            f"{path}:{line_number}"
                        )
                    duplicate_count += 1
                else:
                    by_id[event_id] = (
                        canonical_digest, event, str(path), line_number
                    )
        source_rows.append(
            {
                "path": str(path),
                "byte_length": path.stat().st_size,
                "row_count": row_count,
                "sha256": _sha256_file(path),
            }
        )
    events = [by_id[event_id][1] for event_id in sorted(by_id)]
    ids = [event["id"] for event in events]
    missing_ids = (
        sorted(set(range(ids[0], ids[-1] + 1)).difference(ids)) if ids else []
    )
    report = {
        "source_file_count": len(source_rows),
        "source_files": source_rows,
        "canonical_event_count": len(events),
        "duplicate_source_row_count": duplicate_count,
        "first_event_id": ids[0] if ids else None,
        "maximum_source_event_id": ids[-1] if ids else None,
        "missing_id_count": len(missing_ids),
        "missing_event_ids": missing_ids if len(missing_ids) <= 1000 else None,
    }
    return events, report


def normalized_historical_events(
    events: list[dict[str, object]],
    *,
    source_agent_id: str,
    destination_agent_id: str,
    destination_persona_id: str,
    not_before: datetime,
) -> tuple[list[dict[str, object]], dict[str, int]]:
    """Create non-stimulus historical dialogue receipts with exact lineage."""
    source_agent_id = _required_agent_id(source_agent_id, "source agent ID")
    destination_agent_id = _required_token(
        destination_agent_id, "destination agent ID"
    )
    destination_persona_id = _required_token(
        destination_persona_id, "destination persona ID"
    )
    maximum_id = max((int(event["id"]) for event in events), default=0)
    eligible: list[dict[str, object]] = []
    for event in events:
        source_type = event.get("type")
        payload = event.get("payload")
        text = payload.get("text") if isinstance(payload, dict) else None
        if (
            source_type in HISTORICAL_TYPES
            and isinstance(text, str)
            and text
            and _parse_timestamp(event.get("timestamp"), "event timestamp")
            >= not_before
        ):
            eligible.append(event)

    id_map = {
        int(event["id"]): maximum_id + ordinal
        for ordinal, event in enumerate(eligible, 1)
    }
    normalized: list[dict[str, object]] = []
    unpaired_agent_count = 0
    for event in eligible:
        source_id = int(event["id"])
        source_type = str(event["type"])
        source_caused_by = event.get("caused_by")
        caused_by = None
        if source_type == "agent-message":
            if isinstance(source_caused_by, int) and source_caused_by in id_map:
                caused_by = id_map[source_caused_by]
            else:
                unpaired_agent_count += 1
        canonical = _canonical_json(event)
        normalized.append(
            {
                "schema_version": 2,
                "id": id_map[source_id],
                "agent_id": destination_agent_id,
                "timestamp": event["timestamp"],
                "type": HISTORICAL_TYPES[source_type],
                "payload": {
                    "text": event["payload"]["text"],
                    "metadata": {
                        "source": MIGRATION_SOURCE,
                        "persona_id": destination_persona_id,
                        "source_agent_id": source_agent_id,
                        "source_event_id": source_id,
                        "source_event_type": source_type,
                        "source_event_sha256": _sha256_text(canonical),
                        "source_caused_by": (
                            source_caused_by
                            if isinstance(source_caused_by, int)
                            else None
                        ),
                    },
                },
                "caused_by": caused_by,
            }
        )
    return normalized, {
        "normalized_dialogue_event_count": len(normalized),
        "normalized_user_message_count": sum(
            event["type"] == "historical-user-message-imported"
            for event in normalized
        ),
        "normalized_agent_message_count": sum(
            event["type"] == "historical-agent-message-imported"
            for event in normalized
        ),
        "unpaired_agent_message_count": unpaired_agent_count,
    }


def prepare_migration(
    sources: list[Path],
    output: Path,
    manifest: Path,
    *,
    source_agent_id: str,
    destination_agent_id: str,
    destination_persona_id: str,
    not_before: str,
) -> dict[str, object]:
    if output.exists() or manifest.exists():
        raise ValueError("refusing to overwrite migration output or manifest")
    cutoff = _parse_timestamp(not_before, "not-before")
    events, source_report = read_canonical_events(sources)
    historical, dialogue_report = normalized_historical_events(
        events,
        source_agent_id=source_agent_id,
        destination_agent_id=destination_agent_id,
        destination_persona_id=destination_persona_id,
        not_before=cutoff,
    )
    report: dict[str, object] = {
        "schema_version": 1,
        "migration_revision": MIGRATION_SOURCE,
        "source_agent_id": source_agent_id,
        "destination_agent_id": destination_agent_id,
        "destination_persona_id": destination_persona_id,
        "dialogue_not_before": cutoff.isoformat().replace("+00:00", "Z"),
        **source_report,
        **dialogue_report,
        "output_event_count": len(events) + len(historical),
    }
    output.parent.mkdir(parents=True, exist_ok=True)
    manifest.parent.mkdir(parents=True, exist_ok=True)
    output_temp: str | None = None
    manifest_temp: str | None = None
    try:
        with tempfile.NamedTemporaryFile(
            "w", encoding="utf-8", newline="\n", delete=False,
            dir=output.parent, prefix=f".{output.name}.", suffix=".tmp"
        ) as stream:
            output_temp = stream.name
            for event in (*events, *historical):
                stream.write(_canonical_json(event))
                stream.write("\n")
        report["output_sha256"] = _sha256_file(Path(output_temp))
        with tempfile.NamedTemporaryFile(
            "w", encoding="utf-8", newline="\n", delete=False,
            dir=manifest.parent, prefix=f".{manifest.name}.", suffix=".tmp"
        ) as stream:
            manifest_temp = stream.name
            json.dump(report, stream, ensure_ascii=False, indent=2)
            stream.write("\n")
        os.replace(output_temp, output)
        output_temp = None
        os.replace(manifest_temp, manifest)
        manifest_temp = None
    finally:
        if output_temp:
            Path(output_temp).unlink(missing_ok=True)
        if manifest_temp:
            Path(manifest_temp).unlink(missing_ok=True)
    return report


def parse_args(argv: list[str] | None = None) -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Prepare a provenance-preserving legacy-agent ledger import."
    )
    parser.add_argument("--source", type=Path, action="append", required=True)
    parser.add_argument(
        "--segment-directory", type=Path, action="append", default=[],
        help="append sorted events-*.jsonl sources from this directory",
    )
    parser.add_argument("--output-ledger", type=Path, required=True)
    parser.add_argument("--manifest", type=Path, required=True)
    parser.add_argument("--source-agent-id", required=True)
    parser.add_argument("--destination-agent-id", required=True)
    parser.add_argument("--destination-persona-id", required=True)
    parser.add_argument("--not-before", required=True)
    return parser.parse_args(argv)


def main(argv: list[str] | None = None) -> int:
    args = parse_args(argv)
    try:
        segment_sources = [
            path
            for directory in args.segment_directory
            for path in sorted(directory.glob("events-*.jsonl"))
        ]
        report = prepare_migration(
            [*args.source, *segment_sources],
            args.output_ledger,
            args.manifest,
            source_agent_id=args.source_agent_id,
            destination_agent_id=args.destination_agent_id,
            destination_persona_id=args.destination_persona_id,
            not_before=args.not_before,
        )
    except (OSError, ValueError) as error:
        raise SystemExit(str(error)) from error
    print(json.dumps(report, ensure_ascii=False, sort_keys=True))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
