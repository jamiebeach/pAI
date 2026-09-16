#!/usr/bin/env python3
"""Validate the declared test profiles; optionally fail while any are blocked."""
from __future__ import annotations

import argparse
import json
from pathlib import Path


def validate(root: Path, contract: dict, require_ready: bool = False) -> dict:
    errors: list[str] = []
    profiles = contract.get("profiles")
    overrides = contract.get("lisp_suite_overrides")
    if contract.get("schema_version") != 1 or not isinstance(profiles, dict) or not isinstance(overrides, dict):
        raise ValueError("qualification contract must use schema version 1 and object maps")
    default = contract.get("default_lisp_profile")
    if default not in profiles:
        errors.append("default-profile-missing")
    suites = {path.name for path in (root / "tests").glob("*-tests.lisp")}
    for suite, profile in overrides.items():
        if suite not in suites:
            errors.append("override-suite-missing")
        if profile not in profiles:
            errors.append("override-profile-missing")
    blocked = sorted(name for name, profile in profiles.items()
                     if profile.get("state") == "blocked")
    for name, profile in profiles.items():
        if profile.get("state") not in {"required", "blocked"}:
            errors.append("invalid-profile-state")
        if profile.get("state") == "blocked" and not profile.get("reason"):
            errors.append("blocked-profile-reason-missing")
    python_profiles = contract.get("python_profiles")
    if not isinstance(python_profiles, dict):
        errors.append("python-profiles-missing")
        python_profiles = {}
    blocked_python = sorted(name for name, profile in python_profiles.items()
                            if profile.get("state") == "blocked")
    if require_ready and (blocked or blocked_python):
        errors.append("blocked-profiles-remain")
    return {
        "schema_version": 1,
        "passed": not errors,
        "lisp_suites_discovered": len(suites),
        "default_profile_suites": len(suites - set(overrides)),
        "overridden_lisp_suites": len(overrides),
        "blocked_profiles": blocked,
        "blocked_python_profiles": blocked_python,
        "errors": sorted(set(errors)),
    }


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--root", type=Path, default=Path.cwd())
    parser.add_argument("--contract", type=Path,
                        default=Path("tests/qualification-contract.json"))
    parser.add_argument("--require-ready", action="store_true")
    parser.add_argument("--output", type=Path)
    args = parser.parse_args()
    try:
        contract = json.loads((args.root / args.contract).read_text(encoding="utf-8-sig"))
        report = validate(args.root, contract, args.require_ready)
    except (OSError, ValueError, json.JSONDecodeError):
        report = {"schema_version": 1, "passed": False,
                  "errors": ["contract-unreadable-or-invalid"]}
    rendered = json.dumps(report, indent=2) + "\n"
    if args.output:
        args.output.write_text(rendered, encoding="utf-8")
    print(rendered, end="")
    return 0 if report["passed"] else 1


if __name__ == "__main__":
    raise SystemExit(main())
