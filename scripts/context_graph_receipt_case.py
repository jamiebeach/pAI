"""Bounded private receipt capture for the episode lab; no cognitive decisions."""
from __future__ import annotations

import hashlib
import json
import sqlite3
import time
from pathlib import Path

CONTRACT = {
    "profile": "reviewed-inference-v9",
    "generation": "identity-formation-owner-v9",
    "protocol": "identity-formation-v14",
    "ontology_revision": "personal-context-core-glm53-v1.3",
}

SUPPORTED_CONTRACTS = (
    {
        "profile": "reviewed-inference-v8",
        "generation": "identity-formation-owner-v8",
        "protocol": "identity-formation-v13",
        "ontology_revision": "personal-context-core-glm53-v1.2",
    },
    CONTRACT,
)


def _supported_contract(contract: dict) -> bool:
    return any(contract == supported for supported in SUPPORTED_CONTRACTS)


def capture(events: Path, *, agent_id: str, persona_id: str,
            episode_ids: list[int], contract: dict, cutoff: int,
            recovery_position: int, deadline_seconds: float = 10,
            maximum_rows: int = 4096, maximum_bytes: int = 16 * 1024 * 1024) -> dict:
    """Read indexed openings and selected attempt intervals in one transaction.

    This exports recorded evidence, not a graph or an admission verdict. The
    source envelope in an opening carries exact source text; outputs are private.
    """
    if not _supported_contract(contract):
        raise ValueError("Explicit supported profile/generation/protocol/ontology required")
    if (not agent_id or not persona_id or not episode_ids or len(episode_ids) > 6
            or len(set(episode_ids)) != len(episode_ids)
            or any(type(x) is not int or x <= 0 or x > cutoff for x in episode_ids)
            or recovery_position < 0 or deadline_seconds <= 0):
        raise ValueError("Invalid bounded case selection")
    started = time.monotonic()
    rows_read = bytes_read = 0
    plans = []
    connection = sqlite3.connect(events.resolve().as_uri() + "?mode=ro", uri=True)
    connection.execute("PRAGMA query_only=ON")
    connection.set_progress_handler(
        lambda: int(time.monotonic() - started >= deadline_seconds), 1000)

    def read(sql: str, params: tuple) -> list[dict]:
        nonlocal rows_read, bytes_read
        plan = [row[3] for row in connection.execute("EXPLAIN QUERY PLAN " + sql, params)]
        if any("SCAN pai_events" in row for row in plan):
            raise ValueError("Case lookup would scan event authority")
        plans.extend(plan)
        result = []
        for position, text in connection.execute(sql, params):
            rows_read += 1
            bytes_read += len(text.encode("utf-8"))
            if (rows_read > maximum_rows or bytes_read > maximum_bytes
                    or time.monotonic() - started >= deadline_seconds):
                raise TimeoutError("Receipt capture exceeded its bounded allowance")
            event = json.loads(text)
            result.append({"storage_position": position, "event": event,
                           "capture_sha256": hashlib.sha256(text.encode()).hexdigest()})
        return result

    def record(row: dict) -> dict:
        return json.loads(row["event"]["payload"]["record_json"])

    try:
        connection.execute("BEGIN")
        roots = []
        for episode_id in episode_ids:
            found = read("SELECT storage_sequence,event_json FROM pai_events "
                         "WHERE agent_id=? AND event_id=? ORDER BY storage_sequence",
                         (agent_id, episode_id))
            if len(found) != 1 or found[0]["event"]["type"] != "conversation-episode-sealed":
                raise ValueError("Selected episode is absent, ambiguous, or unsealed")
            roots.extend(found)
        openings = read("SELECT storage_sequence,event_json FROM pai_events "
                        "WHERE agent_id=? AND event_type=? AND event_id<=? "
                        "AND storage_sequence>? "
                        "AND json_extract(event_json,'$.payload.generation')=? "
                        "AND json_extract(event_json,'$.payload.persona_id')=? "
                        "AND json_extract(json_extract(event_json,'$.payload.record_json'),"
                        "'$.episode_event_id') IN (" + ",".join("?" for _ in episode_ids) + ") "
                        "ORDER BY event_id,storage_sequence LIMIT ?",
                        (agent_id, "context-graph-identity-opened", cutoff,
                         recovery_position, contract["generation"], persona_id,
                         *episode_ids, maximum_rows + 1))
        selected = []
        for index, row in enumerate(openings):
            event = row["event"]
            payload = event["payload"]
            if (payload.get("generation") != contract["generation"]
                    or payload.get("persona_id") != persona_id):
                continue
            opening = record(row)
            if opening.get("episode_event_id") not in episode_ids:
                continue
            if (opening.get("formation_protocol") != contract["protocol"]
                    or opening.get("ontology_revision") != contract["ontology_revision"]):
                raise ValueError("Selected opening disagrees with run contract")
            next_id = connection.execute(
                "SELECT MIN(event_id) FROM pai_events WHERE agent_id=? AND event_type=? "
                "AND event_id>? AND event_id<=?",
                (agent_id, "context-graph-identity-opened", event["id"], cutoff)).fetchone()[0]
            end = next_id - 1 if next_id is not None else cutoff
            receipts = read("SELECT storage_sequence,event_json FROM pai_events "
                            "WHERE agent_id=? AND event_id>? AND event_id<=? "
                            "ORDER BY event_id,storage_sequence LIMIT ?",
                            (agent_id, event["id"], end, maximum_rows + 1))
            bound = [r for r in receipts if r["event"].get("caused_by") == event["id"]
                     and r["event"].get("payload", {}).get("generation") == contract["generation"]
                     and r["event"].get("payload", {}).get("persona_id") == persona_id]
            terminal = [r for r in bound if r["event"]["type"] in
                        ("context-graph-identity-completed", "context-graph-identity-failed")]
            if len(terminal) > 1:
                raise ValueError("Ambiguous attempt terminals")
            selected.append({"opening": row, "receipts": bound,
                             "interval_complete": bool(terminal),
                             "graph_application": "unknown-requires-production-replay"})
        latest = {}
        for attempt in selected:
            opening = record(attempt["opening"])
            latest[(opening["episode_event_id"], opening["batch_index"])] = attempt
        counts = {"selected_episodes": len(episode_ids), "observed_batches": len(latest),
                  "attempts": len(selected), "reviewed": 0, "empty": 0,
                  "failed": 0, "pending": 0}
        for attempt in latest.values():
            terminals = [r for r in attempt["receipts"] if r["event"]["type"] in
                         ("context-graph-identity-completed", "context-graph-identity-failed")]
            if not terminals:
                counts["pending"] += 1
            elif terminals[0]["event"]["type"] == "context-graph-identity-failed":
                counts["failed"] += 1
            else:
                status = record(terminals[0]).get("result", {}).get("status")
                if status not in ("reviewed", "empty"):
                    raise ValueError("Unrecognized completion status")
                counts[status] += 1
        return {"schema_version": 1, "kind": "private-receipt-case",
                "contract": {**contract, "agent_id": agent_id, "persona_id": persona_id,
                             "cutoff": cutoff, "recovery_position": recovery_position},
                "capture_code_sha256": hashlib.sha256(Path(__file__).read_bytes()).hexdigest(),
                "roots": roots, "attempts": selected, "counts": counts,
                "metrics": {"elapsed_seconds": time.monotonic() - started,
                            "rows_read": rows_read, "bytes_read": bytes_read,
                            "query_plans": sorted(set(plans)), "provider_calls": 0,
                            "authority_writes": 0},
                "limitations": ["No graph reconstruction or admission judgment",
                                "Capture hashes are not authority integrity verification",
                                "Observed batches are not total episode coverage",
                                "Attempt intervals assume sequential owner execution; incomplete intervals stay unknown"]}
    finally:
        connection.close()


def save_capture(result: dict, output: Path) -> None:
    """Never overwrite a prior private case."""
    output.parent.mkdir(parents=True, exist_ok=True)
    with output.open("x", encoding="utf-8") as stream:
        json.dump(result, stream, ensure_ascii=False, sort_keys=True)
        stream.write("\n")


def capture_preparation(events: Path, *, agent_id: str, persona_id: str,
                        contract: dict, cutoff: int, recovery_position: int,
                        deadline_seconds: float = 10, maximum_rows: int = 20000,
                        maximum_bytes: int = 64 * 1024 * 1024) -> dict:
    """Export a generation's receipts and exact source dependencies, not all history.

    Lisp reconstructs and authenticates source envelopes. This function only
    selects immutable event data under a single read-only SQLite transaction.
    """
    if (not _supported_contract(contract) or not agent_id or not persona_id or cutoff <= 0
            or recovery_position < 0 or deadline_seconds <= 0):
        raise ValueError("Explicit preparation contract required")
    started = time.monotonic()
    selected: dict[int, dict] = {}
    size = 0
    stage = "begin"
    plans: set[str] = set()
    connection = sqlite3.connect(events.resolve().as_uri() + "?mode=ro", uri=True)
    connection.execute("PRAGMA query_only=ON")
    connection.set_progress_handler(lambda: int(time.monotonic() - started >= deadline_seconds), 1000)

    def read(where, params):
        nonlocal size
        sql = ("SELECT event_id,storage_sequence,event_json FROM pai_events WHERE agent_id=? AND "
               + where + " ORDER BY event_id,storage_sequence")
        plan = [r[3] for r in connection.execute("EXPLAIN QUERY PLAN " + sql, (agent_id, *params))]
        if any("SCAN pai_events" in p for p in plan):
            raise ValueError("Preparation would scan authority")
        plans.update(plan)
        result = []
        for event_id, position, raw in connection.execute(sql, (agent_id, *params)):
            if time.monotonic() - started >= deadline_seconds:
                raise TimeoutError("Preparation capture deadline exceeded")
            row = {"storage_position": position, "event": json.loads(raw),
                   "capture_sha256": hashlib.sha256(raw.encode()).hexdigest()}
            if event_id in selected:
                if selected[event_id] != row:
                    raise ValueError("Ambiguous event identity")
            else:
                selected[event_id] = row
                size += len(raw.encode())
                if len(selected) > maximum_rows or size > maximum_bytes:
                    raise ValueError("Preparation capture bound exceeded")
            result.append(row)
        return result

    def exact(event_id):
        if event_id in selected:
            return selected[event_id]["event"]
        rows = read("event_id=? AND event_id<=?", (event_id, cutoff))
        if len(rows) != 1:
            raise ValueError("Preparation source dependency missing or ambiguous")
        return rows[0]["event"]

    def aggregate(where, params):
        sql = ("SELECT COUNT(*),MIN(event_id),MAX(event_id) FROM pai_events "
               "WHERE agent_id=? AND " + where)
        plan = [r[3] for r in connection.execute(
            "EXPLAIN QUERY PLAN " + sql, (agent_id, *params))]
        if any("SCAN pai_events" in p for p in plan):
            raise ValueError("Preparation coverage lookup would scan authority")
        plans.update(plan)
        return connection.execute(sql, (agent_id, *params)).fetchone()

    try:
        connection.execute("BEGIN")
        receipts = []
        first_opening = 0
        for kind in ("opened", "phase", "completed", "failed"):
            stage = "receipts:" + kind
            found = read(
                "event_type=? AND event_id>=? AND event_id<=? AND storage_sequence>? "
                "AND json_extract(event_json,'$.payload.generation')=? "
                "AND json_extract(event_json,'$.payload.persona_id')=?",
                ("context-graph-identity-" + kind, first_opening, cutoff, recovery_position,
                 contract["generation"], persona_id))
            receipts.extend(found)
            if kind == "opened":
                if not found:
                    raise ValueError("No selected generation openings at this cut")
                first_opening = min(row["event"]["id"] for row in found)
        episodes = set()
        for row in receipts:
            event = row["event"]
            if event["type"] == "context-graph-identity-opened":
                record = json.loads(event["payload"]["record_json"])
                if (record.get("formation_protocol") != contract["protocol"]
                        or record.get("ontology_revision") != contract["ontology_revision"]):
                    raise ValueError("Preparation opening contract mismatch")
                episodes.add(record["episode_event_id"])
        stage = "episode-coverage"
        sealed_count, first_sealed, last_sealed = aggregate(
            "event_type=? AND event_id<=? AND storage_sequence>?",
            ("conversation-episode-sealed", cutoff, recovery_position))
        latest_selected = max(episodes)
        sealed_after_latest, first_after_latest, _ = aggregate(
            "event_type=? AND event_id<=? AND storage_sequence>? AND event_id>?",
            ("conversation-episode-sealed", cutoff, recovery_position,
             latest_selected))
        stage = "source-dependencies"
        def fetch_ids(ids):
            missing = sorted(set(ids) - selected.keys())
            for offset in range(0, len(missing), 256):
                chunk = missing[offset:offset + 256]
                read("event_id IN (" + ",".join("?" for _ in chunk) + ") AND event_id<=?",
                     (*chunk, cutoff))
            if any(event_id not in selected for event_id in ids):
                raise ValueError("Preparation source dependency missing")
        fetch_ids(episodes)
        dependencies = set()
        for episode_id in sorted(episodes):
            episode = selected[episode_id]["event"]
            if episode["type"] != "conversation-episode-sealed":
                raise ValueError("Preparation episode is not sealed")
            dependencies.update(episode["payload"]["source_event_ids"])
        fetch_ids(dependencies)
        stage = "confirmation-dependencies"
        confirmations = read(
            "event_type=? AND event_id<=? AND storage_sequence>? "
            "AND json_extract(event_json,'$.payload.persona_id')=?",
            ("context-graph-confirmation-resolved", cutoff, recovery_position, persona_id))
        for row in confirmations:
            payload = row["event"]["payload"]
            request = exact(payload["request_event_id"])
            source = exact(payload["source_user_event_id"])
            # Preserve every intervening publication/user message, including
            # intervening-user invalidation checked by production confirmation.
            read("event_id>? AND event_id<? AND event_type IN ('agent-message','user-message')",
                 (request["id"], source["id"]))
        ordered = sorted(selected.values(), key=lambda row: row["storage_position"])
        identity = {**contract, "agent_id": agent_id, "persona_id": persona_id,
                    "cutoff": cutoff, "recovery_position": recovery_position}
        origin = hashlib.sha256(json.dumps([identity, ordered], sort_keys=True,
                                          separators=(",", ":")).encode()).hexdigest()
        return {"schema_version": 1, "kind": "private-preparation-input", "contract": identity,
                "origin_digest": origin, "events": ordered,
                "metrics": {"elapsed_seconds": time.monotonic() - started,
                            "rows_read": len(selected), "bytes_read": size,
                            "receipt_count": len(receipts), "episode_count": len(episodes),
                            "sealed_episode_count": sealed_count,
                            "unopened_sealed_episode_count": sealed_count - len(episodes),
                            "sealed_episode_count_after_latest_opened": sealed_after_latest,
                            "first_sealed_episode_event_id": first_sealed,
                            "last_sealed_episode_event_id": last_sealed,
                            "first_sealed_episode_after_latest_opened": first_after_latest,
                            "first_opened_episode_event_id": min(episodes),
                            "last_opened_episode_event_id": latest_selected,
                            "episode_coverage_complete": sealed_count == len(episodes),
                            "confirmation_count": len(confirmations), "query_plans": sorted(plans),
                            "provider_calls": 0, "authority_writes": 0}}
    except sqlite3.OperationalError as condition:
        if "interrupted" in str(condition):
            raise TimeoutError(f"Preparation capture deadline at {stage}; "
                               f"{len(selected)} rows, {size} bytes selected; no artifact saved") from condition
        raise
    finally:
        connection.close()
