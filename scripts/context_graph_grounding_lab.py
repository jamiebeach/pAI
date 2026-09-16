"""Grounding experiment implementation for context_graph_resolution_lab.py.

Reuses its provider ledger and Lisp runner; no separate CLI or live database.
Only explicitly synthetic cases are accepted by this first tranche.
"""
from __future__ import annotations
import copy
import hashlib
import json
import os
import re
import shutil
from calendar import monthrange
from datetime import datetime, timedelta, timezone
from pathlib import Path
import context_graph_lab as base

SCOPES = ["assertion", "question", "hypothesis", "intention", "reported-speech",
          "retrieval-outcome", "joke", "proposal", "unresolved"]
TEMPORAL_CHARACTERS = ["event", "temporary-state", "ongoing-state",
                       "standing-disposition", "timeless", "unspecified"]
GROUNDING_PROTOCOL_REVISION = "source-packet-grounding-scope-time"
RESIDUAL_GUIDANCE_REVISION = "formation-repair-residual-guidance-v1"
MONTHS = {name.lower(): index for index, name in enumerate(
    ("January", "February", "March", "April", "May", "June", "July",
     "August", "September", "October", "November", "December"), 1)}


def validate_corpus(corpus):
    if not isinstance(corpus, dict) or corpus.get("source_kind") != "synthetic-controlled":
        raise ValueError("this tranche accepts only explicitly synthetic controlled input")
    cases = corpus.get("cases")
    if not isinstance(cases, list) or not 1 <= len(cases) <= 8:
        raise ValueError("select 1..8 controlled cases")
    seen = set()
    for case in cases:
        case_id = case.get("case_id")
        if not isinstance(case_id, str) or not re.fullmatch(r"[A-Za-z0-9][A-Za-z0-9_-]{0,79}", case_id) or case_id in seen:
            raise ValueError("case ID is unsafe or duplicated")
        seen.add(case_id)
        sources = case.get("sources")
        if not isinstance(sources, list) or not 1 <= len(sources) <= 32:
            raise ValueError("case sources are missing or oversized")
        ids = set()
        for source in sources:
            if set(source) != {"source_id", "speaker_id", "kind", "text"}:
                raise ValueError("source record has unknown fields")
            if not all(isinstance(source[k], str) and 0 < len(source[k]) <= 180 for k in ("source_id", "speaker_id")):
                raise ValueError("invalid source identity")
            if source["source_id"] in ids or source["kind"] not in {
                    "original-utterance", "prior-agent-utterance", "tool-observation",
                    "generated-summary", "retrieval-metadata"}:
                raise ValueError("source kind or uniqueness invalid")
            ids.add(source["source_id"])
            if not isinstance(source["text"], str) or not 0 < len(source["text"]) <= 12000:
                raise ValueError("invalid source text")


def source_episode(case):
    sources = copy.deepcopy(case["sources"])
    for source in sources:
        source["text_sha256"] = hashlib.sha256(source["text"].encode("utf-8")).hexdigest()
    packet = {"schema_version": 1, "sources": sources}
    return {"episode_id": case["case_id"], "occurred_at": case["timestamp"],
            "learned_at": case["timestamp"], "content": json.dumps(packet, ensure_ascii=False)}, packet


def formation_request(episode, packet, args, selected, repair_residuals=(),
                      prior_formation=None):
    request = base.formation_request(episode, args, selected)
    entity = request["tools"][0]["function"]["parameters"]["properties"]["entities"]["items"]
    entity["properties"]["classifications"] = {
        "type": "array", "maxItems": 8, "uniqueItems": True,
        "items": {"type": "string", "maxLength": 120}}
    entity["required"].append("classifications")
    grounding = {"type": "object", "additionalProperties": False, "properties": {
        "schema_version": {"type": "integer", "enum": [1]},
        "scope": {"type": "string", "enum": SCOPES},
        "polarity": {"type": "string", "enum": ["positive", "negative", "unknown"]},
        "attributed_to_ref": {"anyOf": [{"type": "string"}, {"type": "null"}]},
        "evidence": {"type": "array", "minItems": 1, "maxItems": 8,
            "items": {"type": "object", "additionalProperties": False, "properties": {
                "source_id": {"type": "string"}, "quote": {"type": "string"}},
                "required": ["source_id", "quote"]}}},
        "required": ["schema_version", "scope", "polarity", "attributed_to_ref", "evidence"]}
    temporal = {"type": "object", "additionalProperties": False, "properties": {
        "schema_version": {"type": "integer", "enum": [1]},
        "character": {"type": "string", "enum": TEMPORAL_CHARACTERS},
        "occurred_at": {"anyOf": [{"type": "string", "maxLength": 80}, {"type": "null"}]},
        "valid_from": {"anyOf": [{"type": "string", "maxLength": 80}, {"type": "null"}]},
        "valid_until": {"anyOf": [{"type": "string", "maxLength": 80}, {"type": "null"}]}},
        "required": ["schema_version", "character", "occurred_at", "valid_from",
                     "valid_until"]}
    relation = request["tools"][0]["function"]["parameters"]["properties"]["relationships"]["items"]
    relation["properties"].update(fact={"type": "string", "maxLength": 1000},
                                  grounding=grounding, temporal=temporal)
    relation["required"].extend(["fact", "grounding", "temporal"])
    request["messages"][0]["content"] += (
        " This request uses the source-packet grounding protocol. Follow the supplied "
        "JSON schema literally: every schema_version value remains exactly 1. "
        "The supplied records are original "
        "utterances with explicit speaker IDs, not a generated episode synopsis. "
        "Classify each proposition by its speech-act scope and polarity. Preserve "
        "questions, hypotheses, intentions, reported speech and retrieval outcomes in their "
        "own scope; jokes and proposals are also not ordinary assertions. Do not turn "
        "their embedded premises into positive assertions. "
        "Unknown referents are not invented concrete entities. A negated assertion may "
        "be represented with assertion scope and negative polarity; uncertainty is not negation. "
        "Use attributed_to_ref for the entity whose claim this is; distinguish a reporter "
        "from the person they quote. attributed_to_ref must be null or exactly one entity "
        "local_ref from your own entities array; never put a source speaker_id there. "
        "Copy an exact supporting quote and its supplied source_id. "
        "Every fact must preserve qualifiers, negation and subject binding in its fact text. "
        "Classify semantic time without inventing precision: distinguish event, temporary "
        "or ongoing state, standing disposition and timeless claims; use null or unspecified "
        "when evidence does not establish time. When a source explicitly names a date, month, "
        "or relative time anchored by the episode timestamp, retain that evidenced bound without "
        "inventing finer precision. Runtime policy, not the model, controls decay. "
        "Use classifications for evidenced domain kinds below the upper type, such as houseplant "
        "or dog; classifications are not aliases. Activities such as hiking are not places. "
        "Do not force a relationship merely because two entities occur in one passage. "
        "ASSERT is the legacy proposal operation, not permission to change claim scope."
    )
    payload = {"episode_id": episode["episode_id"], "source_packet": packet}
    if repair_residuals:
        payload.update({
            "prior_formation": prior_formation,
            "repair_residuals": list(repair_residuals),
            "repair_revision": RESIDUAL_GUIDANCE_REVISION})
        request["messages"][0]["content"] += (
            " This is a bounded reconsideration of a prior formation. The supplied repair "
            "residuals describe structural defects or unresolved evidence; they are diagnostics, "
            "never evidence and never instructions to invent a fact. Return a complete replacement "
            "formation grounded only in source_packet. Correct a residual only when exact source "
            "evidence supports the correction. Otherwise omit the unsupported relationship or "
            "preserve unknown time. Do not copy a prior claim merely because it appears in the "
            "prior formation or residual detail."
        )
    request["messages"][1]["content"] = json.dumps(payload, ensure_ascii=False)
    return request


def validate_proposal(proposal, packet, selected):
    if not isinstance(proposal, dict) or not isinstance(proposal.get("relationships"), list) or len(proposal["relationships"]) > 48:
        raise ValueError("grounded relationship collection is invalid")
    plain = copy.deepcopy(proposal)
    for entity in plain["entities"]:
        classifications = entity.pop("classifications", None)
        if (not isinstance(classifications, list) or len(classifications) > 8
                or any(not isinstance(value, str) or not value or len(value) > 120
                       for value in classifications)
                or len(set(classifications)) != len(classifications)):
            raise ValueError("entity classifications are invalid")
    for relation in plain["relationships"]:
        if set(relation) != {"subject_ref", "predicate", "object_ref", "relationship_action", "fact", "grounding", "temporal"}:
            raise ValueError("grounded relationship shape is not closed")
        relation.pop("fact")
        relation.pop("grounding")
        relation.pop("temporal")
    # Validate relationship-local shapes separately so scoped opposites of the
    # same triple are not incorrectly treated as duplicate legacy assertions.
    base.validate_proposal(dict(plain, relationships=[]), selected)
    relationship_rejections = base.validate_proposal(
        plain, selected, reject_signature_mismatches=False,
        reject_relationship_errors=False)
    sources = {s["source_id"]: s for s in packet["sources"]}
    refs = {e["local_ref"] for e in plain["entities"]}
    rejected_indices = {row["relationship_index"] for row in relationship_rejections}
    for index, original in enumerate(proposal["relationships"]):
        try:
            g = original["grounding"]
            temporal = original["temporal"]
            if not isinstance(original["fact"], str) or not 1 <= len(original["fact"]) <= 1000:
                raise ValueError("missing-bounded-fact-text")
            if not isinstance(g, dict) or set(g) != {"schema_version", "scope", "polarity", "attributed_to_ref", "evidence"}:
                raise ValueError("invalid-grounding-shape")
            if g["schema_version"] != 1 or g["scope"] not in SCOPES or g["polarity"] not in {"positive", "negative", "unknown"}:
                raise ValueError("invalid-claim-scope")
            if not isinstance(temporal, dict) or set(temporal) != {
                    "schema_version", "character", "occurred_at", "valid_from",
                    "valid_until"}:
                raise ValueError("invalid-semantic-temporal-shape")
            if temporal["schema_version"] != 1 or temporal["character"] not in TEMPORAL_CHARACTERS:
                raise ValueError("invalid-semantic-temporal-classification")
            for key in ("occurred_at", "valid_from", "valid_until"):
                if temporal[key] is not None and (
                        not isinstance(temporal[key], str)
                        or not temporal[key].strip()
                        or len(temporal[key]) > 80):
                    raise ValueError("invalid-semantic-temporal-bound")
            if g["attributed_to_ref"] is not None and g["attributed_to_ref"] not in refs:
                raise ValueError("attribution-is-not-a-supplied-entity")
            if not isinstance(g["evidence"], list) or not 1 <= len(g["evidence"]) <= 8:
                raise ValueError("missing-bounded-source-evidence")
            for evidence in g["evidence"]:
                if set(evidence) != {"source_id", "quote"}:
                    raise ValueError("invalid-quote-reference")
                source = sources.get(evidence["source_id"])
                if (not source or not isinstance(evidence["quote"], str)
                        or not evidence["quote"]
                        or evidence["quote"] not in source["text"]):
                    raise ValueError("quote-does-not-match-original-source")
        except (KeyError, TypeError, ValueError) as error:
            if index not in rejected_indices:
                relationship_rejections.append({
                    "relationship_index": index,
                    "reason": str(error) or "invalid-grounded-relationship",
                })
                rejected_indices.add(index)
    return relationship_rejections


def review_request(episode, packet, proposal, args):
    request = base.evidence_review_request(episode, proposal, args)
    request["messages"][0]["content"] = (
        "Review each proposed entity and scoped relationship against ONLY the original "
        "source_packet. A valid quote establishes source location, not entailment. "
        "DIRECTLY_EVIDENCED requires correct subject, object, predicate, attribution, "
        "speech-act scope, polarity and qualifiers. Questions do not assert their premises; "
        "hypotheses are not observations; failed recall does not establish the missing fact; "
        "jokes and proposals do not establish their embedded propositions as current facts; "
        "quoted third-party claims are not the reporter's personal state. A properly scoped "
        "question, hypothesis or reported claim CAN be directly evidenced as that scoped "
        "claim: DIRECTLY_EVIDENCED then means the source directly expresses the scoped "
        "proposition, not that its embedded proposition is true. Review the graph triple as "
        "well as its fact text. A generic triple is not supported when one of the proposition's "
        "essential participants exists only in prose and is absent from the triple endpoints. "
        "For example, if A says that X might have condition Y, X-has_condition-Y with "
        "hypothesis scope, unknown polarity and attribution to A is directly evidenced as a "
        "hypothesis; it does not claim that X truly has Y. Conversely, A-related_to-Y is not "
        "supported merely because accompanying prose mentions X. "
        "Unknown source referents must not be promoted into concrete facts. "
        "Use REASONABLE_INFERENCE for unstated inference, UNSUPPORTED for absent support, "
        "CONTRADICTED for a conflicting source. No prior graph is supplied. "
        "Judge the supplied proposition, not a corrected version you wish it said. "
        "Return exactly one review per claim_ref via review-context-graph-formation."
    )
    claims = base.evidence_claims(proposal)
    for claim in claims:
        if claim["kind"] == "relationship":
            index = int(claim["claim_ref"].split(":")[1])
            claim["claim"] = proposal["relationships"][index]
    request["messages"][1]["content"] = json.dumps({"source_packet": packet,
        "entities": proposal["entities"], "claims": claims}, ensure_ascii=False)
    return request


def _episode_date(episode):
    value = episode["occurred_at"]
    if isinstance(value, bool):
        raise ValueError("episode occurred_at is not a timestamp")
    if isinstance(value, int) or (
            isinstance(value, str) and re.fullmatch(r"[0-9]+", value)):
        # Sealed production episodes retain Common Lisp universal time: whole
        # seconds since 1900-01-01 UTC.  Preserve that ledger authority instead
        # of asking the model to reinterpret the anchor.
        parsed = datetime(1900, 1, 1, tzinfo=timezone.utc) + timedelta(
            seconds=int(value))
        return parsed.date()
    if not isinstance(value, str):
        raise ValueError("episode occurred_at is not a timestamp")
    parsed = datetime.fromisoformat(value.replace("Z", "+00:00"))
    if parsed.tzinfo is None:
        parsed = parsed.replace(tzinfo=timezone.utc)
    return parsed.date()


def deterministic_temporal(episode, fact):
    """Derive temporal bounds from exact source text, never model authority."""
    supplied = fact["temporal"]
    temporal = {"schema_version": 1, "character": supplied["character"],
                "occurred_at": None, "valid_from": None, "valid_until": None}
    quote = " ".join(row["quote"] for row in fact["grounding"]["evidence"])
    changes = []

    def fill(field, value, rule):
        temporal[field] = value
        changes.append({"field": field, "value": value, "rule": rule})

    anchor = _episode_date(episode)
    relative = re.search(r"\b(yesterday|today|tomorrow)\b", quote, re.IGNORECASE)
    if relative:
        offset = {"yesterday": -1, "today": 0, "tomorrow": 1}[relative.group(1).lower()]
        fill("occurred_at", (anchor + timedelta(days=offset)).isoformat(),
             "relative-day-from-sealed-episode")

    month_pattern = "|".join(name.title() for name in MONTHS)
    for match in re.finditer(
            rf"\b(since|from)\s+({month_pattern})\s+(\d{{4}})\b",
            quote, re.IGNORECASE):
        _cue, month_name, year_text = match.groups()
        year, month = int(year_text), MONTHS[month_name.lower()]
        fill("valid_from", f"{year:04d}-{month:02d}-01", "explicit-month-start")
    for match in re.finditer(
            rf"\b(?:until\s+the\s+end\s+of|through(?:\s+the\s+end\s+of)?)\s+"
            rf"({month_pattern})\s+(\d{{4}})\b", quote, re.IGNORECASE):
        month_name, year_text = match.groups()
        year, month = int(year_text), MONTHS[month_name.lower()]
        last = monthrange(year, month)[1]
        fill("valid_until", f"{year:04d}-{month:02d}-{last:02d}", "explicit-month-end")
    for field in ("occurred_at", "valid_from", "valid_until"):
        value = supplied[field]
        if value is not None and temporal[field] != value:
            changes.append({"field": field, "removed_value": value,
                            "rule": "unsupported-model-time-removed"})
    return temporal, changes


def normalize(episode, packet, proposal, reviews):
    # Keep only directly supported propositions; rejection reasons remain in
    # the review artifacts. The Lisp core rechecks structural source binding.
    accepted = {e["local_ref"] for e in proposal["entities"]
                if reviews["entity:" + e["local_ref"]]["verdict"] == "DIRECTLY_EVIDENCED"}
    normalized = base.normalize_formation(episode, dict(proposal, relationships=[]), episode["episode_id"],
        rejected_entities={e["local_ref"] for e in proposal["entities"]} - accepted, evidence_reviews=reviews)
    classifications = {row["local_ref"]: copy.deepcopy(row["classifications"])
                       for row in proposal["entities"]}
    for row in normalized["proposal"]["entities"]:
        row["classifications"] = classifications[row["local_ref"]]
    normalized["source_packet"] = packet
    temporal_normalizations = []
    for index, fact in enumerate(proposal["relationships"]):
        review = reviews[f"relationship:{index}"]
        attribution = fact["grounding"]["attributed_to_ref"]
        if (review["verdict"] == "DIRECTLY_EVIDENCED" and fact["subject_ref"] in accepted
                and fact["object_ref"] in accepted and (attribution is None or attribution in accepted)):
            temporal, temporal_changes = deterministic_temporal(episode, fact)
            if temporal_changes:
                temporal_normalizations.append({"fact": fact["fact"],
                                                "changes": temporal_changes})
            normalized["proposal"]["facts"].append({
                "subject_ref": fact["subject_ref"], "object_ref": fact["object_ref"],
                "predicate": fact["predicate"], "fact": fact["fact"],
                "supersedes_fact_id": None, "evidence_status": "direct",
                "evidence_note": review["evidence"],
                "grounding": copy.deepcopy(fact["grounding"]),
                "temporal": temporal})
    return normalized, temporal_normalizations


def reuse_received_response(directory, stage, request):
    """Return a response only when its saved request and response receipts still match."""
    rows = json.loads((directory / "calls.json").read_text(encoding="utf-8"))
    request_hash = base.canonical_sha256(request)
    for index, row in enumerate(rows, 1):
        if (row.get("stage") != stage or row.get("status") != "received"
                or row.get("request_sha256") != request_hash):
            continue
        response_path = directory / f"response-{index:02d}-{stage}.json"
        response = json.loads(response_path.read_text(encoding="utf-8"))
        if row.get("response_sha256") != base.canonical_sha256(response):
            raise ValueError("saved extraction response receipt changed")
        return response, row
    return None, None


def formation_residuals(episode, packet, formation, relationship_rejections=(),
                        decisions=(), ontology_sha256=None):
    """Describe incomplete formation without converting absence into a fact."""
    source_evidence = [{"source_id": row["source_id"],
                        "text_sha256": row["text_sha256"]}
                       for row in packet["sources"]]
    rows = []

    def add(kind, detail, reconsider_when, priority):
        identity = {"source_episode_id": episode["episode_id"],
                    "kind": kind, "detail": detail}
        residual_key = "cgr:" + base.canonical_sha256(identity)[:24]
        fingerprint = base.canonical_sha256({
            "identity": identity, "source_evidence": source_evidence,
            "formation_revision": GROUNDING_PROTOCOL_REVISION,
            "ontology_sha256": ontology_sha256})
        rows.append({"schema_version": 1, "residual_key": residual_key,
            "status": "pending", "source_episode_id": episode["episode_id"],
            "kind": kind, "detail": copy.deepcopy(detail),
            "priority": priority,
            "source_evidence": copy.deepcopy(source_evidence),
            "examined_revision": {"formation": GROUNDING_PROTOCOL_REVISION,
                                  "ontology_sha256": ontology_sha256},
            "attempt_fingerprint": fingerprint,
            "reconsider_when": list(reconsider_when)})

    for rejection in relationship_rejections:
        if rejection.get("reason") == "duplicate-relationship":
            continue
        add("relationship-rejected", rejection,
            ["new-source-evidence", "formation-policy-revision",
             "ontology-revision", "operator-correction"], "repair")
    decision_rows = decisions.values() if isinstance(decisions, dict) else decisions
    for decision in decision_rows:
        if decision.get("action") == "UNRESOLVED":
            add("identity-unresolved", {
                "local_ref": decision["local_ref"], "reason": decision["reason"]},
                ["new-graph-candidate", "new-source-evidence",
                 "resolution-policy-revision", "operator-correction"], "repair")

    facts = formation["proposal"]["facts"]
    connected = set()
    missing_event_time = {}
    for fact in facts:
        connected.update((fact["subject_ref"], fact["object_ref"]))
        attributed = fact.get("grounding", {}).get("attributed_to_ref")
        if attributed:
            connected.add(attributed)
        temporal = fact.get("temporal", {})
        if (temporal.get("character") == "event"
                and all(temporal.get(field) is None
                        for field in ("occurred_at", "valid_from", "valid_until"))):
            missing_event_time.setdefault(fact["subject_ref"], []).append({
                "predicate": fact["predicate"], "object_ref": fact["object_ref"],
                "fact": fact["fact"]})
        if fact["predicate"] == "related_to":
            add("predicate-specificity-unresolved", {
                "subject_ref": fact["subject_ref"], "object_ref": fact["object_ref"],
                "fact": fact["fact"]},
                ["ontology-revision", "formation-policy-revision",
                 "new-source-evidence", "operator-correction"], "review")
    for subject_ref, affected_facts in missing_event_time.items():
        add("event-time-unresolved", {
            "anchor_ref": subject_ref, "affected_facts": affected_facts},
            ["new-source-evidence", "temporal-policy-revision",
             "operator-correction"], "repair")
    for entity in formation["proposal"]["entities"]:
        if entity["local_ref"] not in connected:
            add("entity-utility-unresolved", {
                "local_ref": entity["local_ref"], "type": entity["type"],
                "name": entity["name"]},
                ["new-related-fact", "formation-policy-revision",
                 "operator-correction"], "review")
    if len(rows) > 192:
        raise ValueError("formation residual collection exceeds its bound")
    return sorted(rows, key=lambda row: row["residual_key"])


def compare_residuals(previous_by_case, current, compared_case_ids):
    """Compare exact issues only for cases that produced a new formation."""
    compared = sorted(set(compared_case_ids))
    compared_set = set(compared)
    previous = [row for case_id in compared
                for row in previous_by_case.get(case_id, [])]
    current = [row for row in current
               if row["source_episode_id"] in compared_set]

    def counts(before, after):
        before_keys = {row["residual_key"] for row in before}
        after_keys = {row["residual_key"] for row in after}
        return {"previous_count": len(before), "current_count": len(after),
            "persisted_keys": sorted(before_keys & after_keys),
            "cleared_keys": sorted(before_keys - after_keys),
            "new_keys": sorted(after_keys - before_keys)}

    result = {"schema_version": 1,
        "comparison_kind": "exact-residual-identity",
        "compared_case_ids": compared, **counts(previous, current),
        "by_priority": {}}
    for priority in ("repair", "review"):
        result["by_priority"][priority] = counts(
            [row for row in previous if row["priority"] == priority],
            [row for row in current if row["priority"] == priority])
    return result


def load_saved_cases(sources):
    cases = {}
    receipts = []
    for source in sources:
        source = source.resolve()
        outcome_path, formations_path = source / "outcomes.json", source / "formations.json"
        receipts.append({"directory": str(source),
            "outcomes_sha256": base.sha256_file(outcome_path),
            "formations_sha256": base.sha256_file(formations_path)})
        outcomes = json.loads(outcome_path.read_text(encoding="utf-8"))
        formations = {row["episode"]["episode_id"]: row for row in
                      json.loads(formations_path.read_text(encoding="utf-8"))}
        for outcome in outcomes:
            case_id = outcome["case_id"]
            if outcome.get("status") != "applied" or case_id not in formations:
                continue
            decisions_path = source / f"{case_id}-decisions.json"
            cases[case_id] = {"outcome": outcome, "formation": formations[case_id],
                "decisions": (json.loads(decisions_path.read_text(encoding="utf-8"))
                              if decisions_path.is_file() else [])}
    return cases, receipts


def replay_residuals(args, repo, corpus, directory):
    selected = base.load_selected_ontology(repo / "config/context-graph-upper-ontology-v1.2.json")
    ontology_sha = base.canonical_sha256(selected)
    cases, receipts = load_saved_cases(args.residuals_from)
    residuals = []
    selected_ids = {row["case_id"] for row in corpus["cases"]}
    if not selected_ids <= set(cases):
        raise ValueError("residual replay lacks an applied formation for a selected case")
    for case in corpus["cases"]:
        episode, packet = source_episode(case)
        saved = cases[case["case_id"]]
        case_rows = formation_residuals(
            episode, packet, saved["formation"],
            saved["outcome"].get("structural_relationship_rejections", []),
            saved["decisions"], ontology_sha)
        residuals.extend(case_rows)
        import context_graph_resolution_lab as driver
        driver.save(directory / f"{case['case_id']}-residuals.json", case_rows)
    by_kind = {}
    by_priority = {}
    for row in residuals:
        by_kind[row["kind"]] = by_kind.get(row["kind"], 0) + 1
        priority = row["priority"]
        by_priority[priority] = by_priority.get(priority, 0) + 1
    import context_graph_resolution_lab as driver
    driver.save(directory / "seal.json", {"schema_version": 1,
        "mode": "offline-residual-replay", "corpus_sha256": base.canonical_sha256(corpus),
        "source_receipts": receipts, "ontology_sha256": ontology_sha,
        "formation_revision": GROUNDING_PROTOCOL_REVISION})
    driver.save(directory / "residuals.json", residuals)
    report = {"schema_version": 1, "cases": len(corpus["cases"]),
        "residual_count": len(residuals), "by_kind": by_kind,
        "by_priority": by_priority,
        "provider_calls": 0, "live_writes": 0}
    driver.save(directory / "report.json", report)
    print(json.dumps(report), flush=True)
    return 0


def replay_normalized_formations(args, repo, corpus, directory):
    """Reapply deterministic normalization to saved, reviewed proposals."""
    import context_graph_resolution_lab as driver
    selected = base.load_selected_ontology(repo / "config/context-graph-upper-ontology-v1.2.json")
    ontology_sha = base.canonical_sha256(selected)
    saved = {}
    receipts = []
    for source in args.renormalize_from:
        source = source.resolve()
        outcomes_path = source / "outcomes.json"
        outcomes = {row["case_id"]: row for row in
                    json.loads(outcomes_path.read_text(encoding="utf-8"))}
        receipts.append({"directory": str(source),
                         "outcomes_sha256": base.sha256_file(outcomes_path)})
        for case_id, outcome in outcomes.items():
            proposal_path = source / f"{case_id}-proposal.json"
            reviews_path = source / f"{case_id}-reviews.json"
            decisions_path = source / f"{case_id}-decisions.json"
            if outcome.get("status") != "applied" or not (
                    proposal_path.is_file() and reviews_path.is_file()):
                continue
            material = {"outcome": outcome,
                "proposal": json.loads(proposal_path.read_text(encoding="utf-8")),
                "reviews": json.loads(reviews_path.read_text(encoding="utf-8")),
                "decisions": (json.loads(decisions_path.read_text(encoding="utf-8"))
                              if decisions_path.is_file() else {})}
            material["receipts"] = {
                "proposal_sha256": base.sha256_file(proposal_path),
                "reviews_sha256": base.sha256_file(reviews_path),
                "decisions_sha256": (base.sha256_file(decisions_path)
                                     if decisions_path.is_file() else None)}
            saved[case_id] = material
    selected_ids = {row["case_id"] for row in corpus["cases"]}
    if not selected_ids <= set(saved):
        raise ValueError("normalization replay lacks reviewed material for a selected case")

    formations, outcomes, residuals = [], [], []
    for case in corpus["cases"]:
        episode, packet = source_episode(case)
        material = saved[case["case_id"]]
        proposal = material["proposal"]
        saved_reviews = material["reviews"]
        reviews = base.validate_evidence_review({"schema_version": 1,
            "claim_reviews": list(saved_reviews.values())}, proposal)
        formation, changes = normalize(episode, packet, proposal, reviews)
        decisions = material["decisions"]
        if decisions:
            formation = driver.apply_decisions(formation, decisions)
            live_refs = {row["local_ref"] for row in formation["proposal"]["entities"]}
            formation["proposal"]["facts"] = [row for row in formation["proposal"]["facts"]
                if row["grounding"]["attributed_to_ref"] is None
                or row["grounding"]["attributed_to_ref"] in live_refs]
        case_residuals = formation_residuals(
            episode, packet, formation,
            material["outcome"].get("structural_relationship_rejections", []),
            decisions, ontology_sha)
        formations.append(formation)
        residuals.extend(case_residuals)
        outcomes.append({"case_id": case["case_id"], "status": "applied",
            "temporal_normalizations": changes,
            "residual_count": len(case_residuals),
            "source_receipts": material["receipts"]})
    result = driver.run_graph(repo, directory / "final", selected, formations,
                              corpus["queries"])
    driver.save(directory / "formations.json", formations)
    driver.save(directory / "outcomes.json", outcomes)
    driver.save(directory / "residuals.json", residuals)
    driver.save(directory / "seal.json", {"schema_version": 1,
        "mode": "offline-deterministic-renormalization",
        "corpus_sha256": base.canonical_sha256(corpus),
        "source_receipts": receipts, "ontology_sha256": ontology_sha,
        "formation_revision": GROUNDING_PROTOCOL_REVISION})
    report = {"schema_version": 1, "cases": len(formations),
        "entities": result["graph"]["entity_count"],
        "facts": result["graph"]["fact_count"],
        "residual_count": len(residuals), "provider_calls": 0, "live_writes": 0}
    driver.save(directory / "report.json", report)
    print(json.dumps(report), flush=True)
    return 0


def run(args, repo):
    import context_graph_resolution_lab as driver
    from run_lisp_test import find_sbcl
    corpus = json.loads(args.grounding_cases.read_text(encoding="utf-8"))
    validate_corpus(corpus)
    selected_case_ids = getattr(args, "grounding_case_id", None) or []
    if selected_case_ids:
        available = {row["case_id"] for row in corpus["cases"]}
        if len(set(selected_case_ids)) != len(selected_case_ids) or not set(selected_case_ids) <= available:
            raise ValueError("selected grounding case is absent or duplicated")
        corpus = copy.deepcopy(corpus)
        corpus["cases"] = [row for row in corpus["cases"]
                           if row["case_id"] in set(selected_case_ids)]
    directory = args.artifacts.resolve()
    directory.mkdir(parents=True, exist_ok=False)
    if getattr(args, "renormalize_from", None):
        if (args.execute or args.reuse_extractions or args.continue_unattempted_from
                or args.residuals_from or args.residual_guidance_from):
            raise ValueError("normalization replay cannot combine with provider modes")
        return replay_normalized_formations(args, repo, corpus, directory)
    if getattr(args, "residuals_from", None):
        if args.execute or args.reuse_extractions or args.continue_unattempted_from:
            raise ValueError("offline residual replay cannot execute or combine provider modes")
        return replay_residuals(args, repo, corpus, directory)
    native_sbcl = shutil.which("sbcl")
    if native_sbcl:
        os.environ.setdefault("PAI_SBCL", native_sbcl)
        if Path("/opt/quicklisp/setup.lisp").is_file():
            os.environ.setdefault("PAI_QUICKLISP_SETUP", "/opt/quicklisp/setup.lisp")
    else:
        sbcl = find_sbcl(repo)
        os.environ.setdefault("PAI_SBCL", str(sbcl))
        os.environ.setdefault("SBCL_HOME", str(sbcl.parent))
        os.environ.setdefault("PAI_QUICKLISP_SETUP", (repo / ".tools/quicklisp/setup.lisp").as_posix())
    selected = base.load_selected_ontology(repo / "config/context-graph-upper-ontology-v1.2.json")
    args.openrouter_zdr = "require"
    if args.model != "openai/gpt-oss-120b":
        raise ValueError("controlled tranche model is pinned")
    guidance_sources = getattr(args, "residual_guidance_from", None) or []
    guidance_cases, guidance_receipts = ({}, [])
    guidance_residuals = {}
    if guidance_sources:
        guidance_cases, guidance_receipts = load_saved_cases(guidance_sources)
        selected_ids = {row["case_id"] for row in corpus["cases"]}
        if not selected_ids <= set(guidance_cases):
            raise ValueError("residual guidance lacks an applied formation for a selected case")
        ontology_sha = base.canonical_sha256(selected)
        for case in corpus["cases"]:
            episode, packet = source_episode(case)
            saved = guidance_cases[case["case_id"]]
            guidance_residuals[case["case_id"]] = formation_residuals(
                episode, packet, saved["formation"],
                saved["outcome"].get("structural_relationship_rejections", []),
                saved["decisions"], ontology_sha)
    reuse_directory = args.reuse_extractions.resolve() if args.reuse_extractions else None
    if reuse_directory:
        prior_seal = json.loads((reuse_directory / "seal.json").read_text(encoding="utf-8"))
        if (prior_seal.get("model") != args.model
                or prior_seal.get("protocol_revision") != GROUNDING_PROTOCOL_REVISION
                or prior_seal.get("provider") != args.openrouter_provider_only
                or prior_seal.get("zdr") is not True
                or prior_seal.get("data_collection") != "deny"):
            raise ValueError("extraction reuse policy differs from the current sealed policy")
    seal = {"schema_version": 1, "corpus": corpus,
        "corpus_sha256": base.canonical_sha256(corpus), "model": args.model,
        "protocol_revision": GROUNDING_PROTOCOL_REVISION,
        "request_limit": args.request_limit, "cost_ceiling_usd": args.cost_ceiling_usd,
        "max_output_tokens": args.max_output_tokens, "review_output_tokens": args.review_output_tokens,
        "max_prompt_price": args.max_prompt_price, "max_completion_price": args.max_completion_price,
        "provider_timeout_seconds": args.provider_timeout_seconds,
        "provider": args.openrouter_provider_only, "zdr": True, "data_collection": "deny",
        "retries": getattr(args, "transient_retries", 0),
        "residual_guidance": ({"revision": RESIDUAL_GUIDANCE_REVISION,
            "repair_only": True, "source_receipts": guidance_receipts,
            "residuals_sha256": base.canonical_sha256(guidance_residuals)}
            if guidance_sources else None),
        "reused_extractions_from": str(reuse_directory) if reuse_directory else None}
    driver.save(directory / "seal.json", seal)
    if not args.execute:
        print("SEALED controlled corpus; no provider requests", flush=True)
        return 0
    formations, baseline, outcomes, prior_rows = [], [], [], []
    if args.continue_unattempted_from:
        prior = args.continue_unattempted_from.resolve()
        previous_seal = json.loads((prior / "seal.json").read_text(encoding="utf-8"))
        if previous_seal != seal:
            raise ValueError("continuation requires exactly the same sealed corpus and provider/budget policy")
        prior_rows = json.loads((prior / "calls.json").read_text(encoding="utf-8"))
        outcomes = json.loads((prior / "outcomes.json").read_text(encoding="utf-8"))
        formations = json.loads((prior / "formations.json").read_text(encoding="utf-8"))
        attempted = {row["stage"].rsplit("-", 1)[0] for row in prior_rows}
        if attempted != {row["case_id"] for row in outcomes}:
            raise ValueError("continuation contains an unfinished case; do not infer its outcome")
        if {f["episode"]["episode_id"] for f in formations} != {r["case_id"] for r in outcomes if r["status"] == "applied"}:
            raise ValueError("prior applied cases differ from retained formations")
        driver.save(directory / "continued-from.json", {"directory": str(prior), "prior_calls": len(prior_rows)})
        for f in formations:
            ablated = copy.deepcopy(f)
            ablated.pop("source_packet")
            for fact in ablated["proposal"]["facts"]: fact.pop("grounding")
            baseline.append(ablated)
    calls = driver.Calls(directory, args, prior_rows)
    driver.save(directory / "calls.json", calls.rows)
    driver.save(directory / "outcomes.json", outcomes)
    driver.save(directory / "formations.json", formations)
    prior_cases = {row["case_id"] for row in outcomes}
    queries = corpus["queries"]
    for case in corpus["cases"]:
        if case["case_id"] in prior_cases:
            continue
        episode, packet = source_episode(case)
        stage = case["case_id"]
        outcome = {"case_id": stage}
        decisions = []
        try:
            prior = guidance_cases.get(stage)
            repair_guidance = [row for row in guidance_residuals.get(stage, [])
                               if row["priority"] == "repair"]
            extraction_request = formation_request(
                episode, packet, args, selected, repair_guidance,
                prior["formation"] if prior and repair_guidance else None)
            if repair_guidance:
                outcome["guided_residual_keys"] = [row["residual_key"]
                                                     for row in repair_guidance]
            response, reused_row = ((None, None) if reuse_directory is None else
                                    reuse_received_response(reuse_directory, stage + "-extract",
                                                            extraction_request))
            if response is None:
                response = calls.call(stage + "-extract", extraction_request)
            else:
                calls.rows.append({"stage": stage + "-extract", "bound_usd": 0,
                    "charged_usd": 0, "status": "reused", "request_sha256": reused_row["request_sha256"],
                    "response_sha256": reused_row["response_sha256"],
                    "reused_from": str(reuse_directory)})
                driver.save(directory / "calls.json", calls.rows)
            if response["choices"][0].get("finish_reason") == "length":
                raise ValueError("truncated extraction")
            proposal = base.extract_proposal(response)
            rejected_shapes = validate_proposal(proposal, packet, selected)
            outcome["structural_relationship_rejections"] = rejected_shapes
            proposal = base.proposal_without_rejected_relationships(
                proposal, rejected_shapes)
            driver.save(directory / (stage + "-proposal.json"), proposal)
            response = calls.call(stage + "-review", review_request(episode, packet, proposal, args))
            if response["choices"][0].get("finish_reason") == "length":
                raise ValueError("truncated review")
            reviews = base.validate_evidence_review(base.extract_evidence_review(response), proposal)
            driver.save(directory / (stage + "-reviews.json"), reviews)
            formation, temporal_changes = normalize(episode, packet, proposal, reviews)
            outcome["temporal_normalizations"] = temporal_changes
            entities = formation["proposal"]["entities"]
            current = driver.run_graph(repo, directory / (stage + "-candidates"), selected, formations, [], entities)
            if entities:
                response = calls.call(stage + "-resolve", driver.resolution_request(episode, entities, current["candidate_sets"], args))
                decisions = driver.validate_resolution(response, entities, current["candidate_sets"])
                formation = driver.apply_decisions(formation, decisions)
                # An unresolved attributed speaker is also an incident reference.
                live_refs = {e["local_ref"] for e in formation["proposal"]["entities"]}
                formation["proposal"]["facts"] = [f for f in formation["proposal"]["facts"]
                    if f["grounding"]["attributed_to_ref"] is None or f["grounding"]["attributed_to_ref"] in live_refs]
                driver.save(directory / (stage + "-decisions.json"), decisions)
            driver.run_graph(repo, directory / (stage + "-applied"), selected, formations + [formation], queries)
            formations.append(formation)
            ablated = copy.deepcopy(formation)
            ablated.pop("source_packet")
            for fact in ablated["proposal"]["facts"]:
                fact.pop("grounding")
            baseline.append(ablated)
            case_residuals = formation_residuals(
                episode, packet, formation, rejected_shapes, decisions,
                base.canonical_sha256(selected))
            driver.save(directory / (stage + "-residuals.json"), case_residuals)
            outcome["residual_count"] = len(case_residuals)
            outcome.update(status="applied", entities=len(entities), facts=len(formation["proposal"]["facts"]))
        except Exception as error:
            outcome.update(status="deferred" if isinstance(error, driver.ProviderDeferred) else "failed", error=str(error))
        outcomes.append(outcome)
        driver.save(directory / "outcomes.json", outcomes)
        driver.save(directory / "formations.json", formations)
        # Provider error bodies may contain account metadata; keep them local.
        print(stage, outcome["status"], "(see local outcome receipt)" if outcome["status"] == "failed" else "", flush=True)
        # Each untouched case is an independent probe. A failed case stays
        # failed, is never repaired/retried, and retains its attempt reservation.
        if calls.poisoned:
            break
    result = driver.run_graph(repo, directory / "final", selected, formations, queries)
    driver.run_graph(repo, directory / "scope-ablated-baseline", selected, baseline, queries)
    residuals = []
    for outcome in outcomes:
        path = directory / f"{outcome['case_id']}-residuals.json"
        if path.is_file():
            residuals.extend(json.loads(path.read_text(encoding="utf-8")))
    driver.save(directory / "residuals.json", residuals)
    residual_kinds, residual_priorities = {}, {}
    for row in residuals:
        residual_kinds[row["kind"]] = residual_kinds.get(row["kind"], 0) + 1
        residual_priorities[row["priority"]] = residual_priorities.get(row["priority"], 0) + 1
    residual_comparison = None
    if guidance_sources:
        applied_ids = {row["episode"]["episode_id"] for row in formations}
        residual_comparison = compare_residuals(
            guidance_residuals, residuals, applied_ids)
        driver.save(directory / "residual-comparison.json", residual_comparison)
    report = {"cases_selected": len(corpus["cases"]), "cases_applied": len(formations),
        "calls": len(calls.rows),
        "provider_calls": sum(row.get("status") != "reused" for row in calls.rows),
        "reused_responses": sum(row.get("status") == "reused" for row in calls.rows),
        "reserved_usd": calls.reserved,
        "charged_usd": sum(r.get("charged_usd", r["bound_usd"]) for r in calls.rows),
        "residual_count": len(residuals),
        "residuals_by_kind": residual_kinds,
        "residuals_by_priority": residual_priorities,
        "residual_guidance": bool(guidance_sources),
        "exact_residuals_persisted": (len(residual_comparison["persisted_keys"])
                                      if residual_comparison else 0),
        "exact_residuals_cleared": (len(residual_comparison["cleared_keys"])
                                    if residual_comparison else 0),
        "exact_residuals_new": (len(residual_comparison["new_keys"])
                                if residual_comparison else 0),
        "entities": result["graph"]["entity_count"], "facts": result["graph"]["fact_count"],
        "semantic_status": "requires-independent-source-review", "live_writes": 0}
    driver.save(directory / "report.json", report)
    print(json.dumps(report), flush=True)
    return 0 if len(formations) == len(corpus["cases"]) else 1
