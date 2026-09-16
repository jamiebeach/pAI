#!/usr/bin/env python3
"""Start one pAI instance from a strict, credential-free JSON configuration."""

from __future__ import annotations

import argparse
import json
from pathlib import Path
import re
import sys

import pai_cli


TOP_LEVEL_KEYS = {"schema_version", "agent", "storage", "provider", "memory", "runtime", "web"}
AGENT_KEYS = {"id", "persona"}
STORAGE_KEYS = {"state_directory", "initialize_if_empty"}
PROVIDER_KEYS = {"kind", "endpoint", "model"}
MEMORY_KEYS = {
    "embedding_endpoint", "embedding_model", "embedding_revision",
    "retrieval_embedding_model", "retrieval_embedding_revision", "vector_dimension",
}
RUNTIME_KEYS = {
    "provider_profile", "context_profile", "mind_loop", "recursive_tools",
    "curiosity_wake_seconds", "deliberate_curiosity", "curiosity_reach_out",
    "curiosity_briefing", "curiosity_consolidation", "episodic_memory",
    "knowledge_graph_formation", "knowledge_graph_rebuild_only",
    "knowledge_graph_budget_usd", "knowledge_graph_prior_exposure_usd",
    "context_graph_runtime_profile", "context_graph_budget_authorization_id",
    "private_budget_percent", "private_reasoning_effort", "loop_trace",
    "context_trace", "show_rejected", "show_memory_context",
    "affect_baseline_event_id", "verbose_startup",
}
WEB_KEYS = {"enabled", "address", "port", "file_mutation"}
FORBIDDEN_KEY_PARTS = {"password", "secret", "token", "api_key", "credential"}


def _object(document: dict[str, object], key: str, keys: set[str]) -> dict[str, object]:
    value = document.get(key)
    if not isinstance(value, dict) or set(value) != keys:
        raise SystemExit(f"instance config {key!r} must contain exactly {sorted(keys)}")
    return value


def _reject_credentials(value: object, path: str = "config") -> None:
    if isinstance(value, dict):
        for key, child in value.items():
            normalized = str(key).casefold().replace("-", "_")
            if any(part in normalized for part in FORBIDDEN_KEY_PARTS):
                raise SystemExit(f"credentials are forbidden in instance config at {path}.{key}")
            _reject_credentials(child, f"{path}.{key}")
    elif isinstance(value, list):
        for index, child in enumerate(value):
            _reject_credentials(child, f"{path}[{index}]")


def _required_text(section: dict[str, object], key: str, maximum: int = 1000) -> str:
    value = section.get(key)
    if not isinstance(value, str) or not value or len(value) > maximum:
        raise SystemExit(f"instance config {key!r} must be non-empty bounded text")
    if "replace-with-" in value.casefold():
        raise SystemExit(f"replace placeholder value for {key!r} before first run")
    return value


def _required_boolean(section: dict[str, object], key: str) -> bool:
    value = section.get(key)
    if not isinstance(value, bool):
        raise SystemExit(f"instance config {key!r} must be boolean")
    return value


def load_instance_config(path: Path, repo: Path) -> argparse.Namespace:
    try:
        document = json.loads(path.read_text(encoding="utf-8"))
    except (OSError, json.JSONDecodeError) as error:
        raise SystemExit(f"could not read instance config: {error}") from error
    if not isinstance(document, dict) or set(document) != TOP_LEVEL_KEYS:
        raise SystemExit(f"instance config must contain exactly {sorted(TOP_LEVEL_KEYS)}")
    if document.get("schema_version") != 1:
        raise SystemExit("instance config schema_version must be 1")
    _reject_credentials(document)

    agent = _object(document, "agent", AGENT_KEYS)
    storage = _object(document, "storage", STORAGE_KEYS)
    provider = _object(document, "provider", PROVIDER_KEYS)
    memory = _object(document, "memory", MEMORY_KEYS)
    runtime = _object(document, "runtime", RUNTIME_KEYS)
    web = _object(document, "web", WEB_KEYS)

    args = pai_cli.parse_args([])
    agent_id = _required_text(agent, "id", 128)
    if not re.fullmatch(r"[A-Za-z0-9][A-Za-z0-9_.:-]{0,127}", agent_id):
        raise SystemExit("agent id must use letters, digits, '.', '_', ':', or '-'")
    args.agent_id = agent_id
    args.persona = pai_cli.validate_persona_name(_required_text(agent, "persona", 64))

    state_value = _required_text(storage, "state_directory", 1000)
    state_path = Path(state_value)
    if state_path.is_absolute() or ".." in state_path.parts:
        raise SystemExit("state_directory must be a relative path inside the repository")
    args.state_dir = (repo / state_path).resolve()
    try:
        args.state_dir.relative_to(repo.resolve())
    except ValueError as error:
        raise SystemExit("state_directory escapes the repository") from error
    initialize_if_empty = _required_boolean(storage, "initialize_if_empty")
    event_database = args.state_dir / "events.sqlite3"
    derived_database = args.state_dir / "derived.sqlite3"
    if derived_database.exists() and not event_database.exists():
        raise SystemExit("derived state exists without the authoritative event database")
    args.initialize_events = initialize_if_empty and not event_database.exists()

    args.provider = _required_text(provider, "kind", 32)
    if args.provider != "local":
        raise SystemExit("public first-run config currently supports provider kind 'local'")
    args.endpoint = _required_text(provider, "endpoint", 1000)
    args.model = _required_text(provider, "model", 200)

    args.embedding_endpoint = pai_cli.validate_embedding_endpoint(
        _required_text(memory, "embedding_endpoint", 1000)
    )
    args.memory_embedding_model = _required_text(memory, "embedding_model", 256)
    args.memory_embedding_revision = _required_text(memory, "embedding_revision", 256)
    args.memory_retrieval_embedding_model = _required_text(
        memory, "retrieval_embedding_model", 256
    )
    args.memory_retrieval_embedding_revision = _required_text(
        memory, "retrieval_embedding_revision", 256
    )
    dimension = memory.get("vector_dimension")
    if not isinstance(dimension, int) or isinstance(dimension, bool) or dimension <= 0:
        raise SystemExit("memory vector_dimension must be a positive integer")
    args.memory_vector_dimension = dimension

    for key in RUNTIME_KEYS:
        setattr(args, key, runtime[key])
    # The legacy CLI's --provider-profile names a paid-provider policy.  Keep
    # the local instance profile separate so it cannot accidentally select
    # or load that policy surface.
    args.local_provider_profile = _required_text(runtime, "provider_profile", 128)
    if args.local_provider_profile != "local-providerless-v1":
        raise SystemExit("local provider_profile must be 'local-providerless-v1'")
    for key in (
        "recursive_tools", "deliberate_curiosity", "curiosity_reach_out",
        "curiosity_briefing", "curiosity_consolidation", "episodic_memory",
        "knowledge_graph_formation", "knowledge_graph_rebuild_only",
        "show_rejected", "show_memory_context", "verbose_startup",
    ):
        _required_boolean(runtime, key)

    args.web = _required_boolean(web, "enabled")
    configured_web_address = _required_text(web, "address", 64)
    port = web.get("port")
    if not isinstance(port, int) or isinstance(port, bool) or not 1 <= port <= 65535:
        raise SystemExit("web port must be an integer from 1 through 65535")
    args.web_address = configured_web_address if args.web else None
    args.web_port = port if args.web else None
    args.web_file_mutation = _required_boolean(web, "file_mutation")
    return args


def parse_args(argv: list[str] | None = None) -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--config", type=Path, default=Path("config/instance.json"),
        help="credential-free instance configuration (default: config/instance.json)",
    )
    return parser.parse_args(argv)


def main(argv: list[str] | None = None) -> int:
    options = parse_args(argv)
    repo = Path(__file__).resolve().parent.parent
    args = load_instance_config(options.config.resolve(), repo)
    return pai_cli.run(args)


if __name__ == "__main__":
    sys.exit(main())
