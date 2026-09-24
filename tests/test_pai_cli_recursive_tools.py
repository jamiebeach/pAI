"""Authority and environment gates for recursive primitive tools."""

from __future__ import annotations

import argparse
from pathlib import Path
from unittest.mock import patch
import sys
import tempfile
import unittest


REPO = Path(__file__).resolve().parent.parent
sys.path.insert(0, str(REPO / "scripts"))

from pai_cli import (  # noqa: E402
    affect_inspection_environment,
    curiosity_consolidation_enabled,
    parse_args,
    recursive_tools_environment,
    tool_runtime_environment,
)


class PaiCliRecursiveToolTests(unittest.TestCase):
    def test_trace_modes_are_explicit_cli_contracts(self) -> None:
        configured = parse_args(
            ["--loop-trace", "full", "--context-trace", "metadata"]
        )
        self.assertEqual(configured.loop_trace, "full")
        self.assertEqual(configured.context_trace, "metadata")

    def test_curiosity_interval_is_an_explicit_cli_contract(self) -> None:
        configured = parse_args(
            ["--mind-loop", "recursive", "--curiosity-wake-seconds", "300"]
        )
        self.assertEqual(configured.curiosity_wake_seconds, 300)
        self.assertFalse(configured.deliberate_curiosity)
        self.assertEqual(configured.private_budget_percent, 30)

    def test_affect_inspection_retains_an_explicit_exclusive_baseline(self) -> None:
        configured = parse_args(
            ["--mind-loop", "recursive", "--affect-baseline-event-id", "421"]
        )
        self.assertEqual(
            affect_inspection_environment(configured),
            {"PAI_AFFECT_BASELINE_EVENT_ID": "421"},
        )
        with self.assertRaisesRegex(SystemExit, "non-negative"):
            affect_inspection_environment(
                parse_args(
                    ["--mind-loop", "recursive", "--affect-baseline-event-id", "-1"]
                )
            )
        with self.assertRaisesRegex(SystemExit, "mind-loop recursive"):
            affect_inspection_environment(
                parse_args(["--affect-baseline-event-id", "421"])
            )

    def test_private_cost_budget_is_one_shared_percentage(self) -> None:
        configured = parse_args(["--private-budget-percent", "45"])
        self.assertEqual(configured.private_budget_percent, 45)

    def test_deliberate_curiosity_is_a_separate_opt_in(self) -> None:
        configured = parse_args(
            ["--mind-loop", "recursive", "--deliberate-curiosity"]
        )
        self.assertTrue(configured.deliberate_curiosity)
        self.assertEqual(configured.curiosity_wake_seconds, 0)

    def test_curiosity_reach_out_is_a_separate_opt_in(self) -> None:
        configured = parse_args(
            ["--mind-loop", "recursive", "--curiosity-reach-out"]
        )
        self.assertTrue(configured.curiosity_reach_out)

    def test_curiosity_briefing_is_a_separate_opt_in(self) -> None:
        configured = parse_args(
            ["--mind-loop", "recursive", "--curiosity-briefing"]
        )
        self.assertTrue(configured.curiosity_briefing)

    def test_episodic_memory_is_an_explicit_opt_in(self) -> None:
        configured = parse_args(
            ["--mind-loop", "recursive", "--curiosity-wake-seconds", "300",
             "--episodic-memory"]
        )
        self.assertTrue(configured.episodic_memory)

    def test_curiosity_consolidation_defaults_on_with_explicit_opt_out(self) -> None:
        default = parse_args(["--mind-loop", "recursive"])
        opted_out = parse_args(
            ["--mind-loop", "recursive", "--no-curiosity-consolidation"]
        )
        non_recursive = parse_args([])
        self.assertTrue(curiosity_consolidation_enabled(default))
        self.assertFalse(curiosity_consolidation_enabled(opted_out))
        self.assertFalse(curiosity_consolidation_enabled(non_recursive))

    def test_tools_require_recursive_loop_before_os_boundary(self) -> None:
        with tempfile.TemporaryDirectory(prefix="pai-recursive-tools-") as temporary:
            state = Path(temporary)
            with self.assertRaisesRegex(SystemExit, "mind-loop recursive"):
                recursive_tools_environment(
                    argparse.Namespace(
                        recursive_tools=True, mind_loop="work-state", provider="local"
                    ),
                    REPO,
                    state,
                )

    def test_restricted_environment_admits_selected_api_keys_only(self) -> None:
        with tempfile.TemporaryDirectory(prefix="pai-recursive-home-") as temporary:
            tool_environment = {
                "PAI_RECURSIVE_TOOLS": "host-native-development-v1",
                "PAI_RECURSIVE_WORKSPACE_ROOT": str(REPO),
                "PAI_RECURSIVE_BASH": r"C:\Program Files\Git\bin\bash.exe",
                "PAI_RESTRICTED_RUNTIME_HOME": temporary,
                "PAI_LISP_EVAL_REVIEW_LOG": str(
                    REPO / ".pai-review" / "lisp-evals.jsonl"
                ),
            }
            restricted = tool_runtime_environment(
                {
                    "PATH": "fixture-path",
                    "SystemRoot": r"C:\Windows",
                    "OPENROUTER_API_KEY": "admitted-openrouter-key",
                    "BRAVE_API_KEY": "admitted-brave-key",
                    "UNRELATED_API_KEY": "must-disappear",
                    "PAI_WEB_PASSWORD": "must-also-disappear",
                    "PAI_AGENT_ID": "fixture-agent",
                    "PAI_SOURCE_ROOT": "/workspace/src",
                    "PAI_FILE_SEARCH_ROOT": "/workspace",
                    "PAI_TEMPLATES": "/workspace/templates",
                    "PAI_CONVERSATION_REQUEST_LIMIT": "20",
                    "PAI_CONVERSATION_COST_CEILING_MICROUSD": "30000",
                    "PAI_CONVERSATION_MODEL_OVERRIDE": "openai/gpt-oss-120b",
                    "PAI_CONVERSATION_ZDR_OVERRIDE": "allow-non-zdr",
                    "PAI_CONVERSATION_DATA_COLLECTION_OVERRIDE": "allow",
                    "PAI_CONVERSATION_REASONING_OVERRIDE": "enabled",
                    "PAI_CONVERSATION_REASONING_EFFORT": "high",
                    "PAI_RECURSIVE_LOOP_TRACE": "full",
                    "PAI_CURIOSITY_WAKE_SECONDS": "300",
                    "PAI_DELIBERATE_CURIOSITY": "1",
                    "PAI_CURIOSITY_BRIEFING": "1",
                    "PAI_CURIOSITY_CONSOLIDATION": "0",
                    "PAI_EPISODIC_MEMORY": "1",
                    "PAI_CONTEXT_GRAPH_GENERATION_BUDGET_MICROUSD": "5125001",
                    "PAI_CONTEXT_GRAPH_PRIOR_EXPOSURE_MICROUSD": "2400000",
                    "PAI_CONTEXT_GRAPH_BUDGET_AUTHORIZATION_ID": "fixture-auth-v1",
                    "PAI_CONTEXT_GRAPH_RUNTIME_PROFILE": "reviewed-inference-v7",
                    "PAI_PRIVATE_BUDGET_PERCENT": "30",
                    "PAI_CONTEXT_TRACE": "metadata",
                    "PAI_CONTEXT_TRACE_DIR": "/private/context-traces",
                    "PAI_FLEET_OWN_ADDRESS": "fixture-host.example.ts.net:8443",
                    "PAI_FLEET_OWN_NAME": "FixtureAgent",
                },
                tool_environment,
            )
            self.assertEqual(restricted["PATH"], "fixture-path")
            self.assertEqual(restricted["PAI_AGENT_ID"], "fixture-agent")
            self.assertEqual(restricted["PAI_SOURCE_ROOT"], "/workspace/src")
            self.assertEqual(restricted["PAI_FILE_SEARCH_ROOT"], "/workspace")
            self.assertEqual(restricted["PAI_TEMPLATES"], "/workspace/templates")
            self.assertTrue(
                restricted["PAI_LISP_EVAL_REVIEW_LOG"].endswith(
                    "lisp-evals.jsonl"
                )
            )
            self.assertEqual(restricted["PAI_CONVERSATION_REQUEST_LIMIT"], "20")
            self.assertEqual(
                restricted["PAI_CONVERSATION_COST_CEILING_MICROUSD"], "30000"
            )
            self.assertEqual(
                restricted["PAI_CONVERSATION_MODEL_OVERRIDE"],
                "openai/gpt-oss-120b",
            )
            self.assertEqual(
                restricted["PAI_CONVERSATION_ZDR_OVERRIDE"], "allow-non-zdr"
            )
            self.assertEqual(
                restricted["PAI_CONVERSATION_DATA_COLLECTION_OVERRIDE"], "allow"
            )
            self.assertEqual(
                restricted["PAI_CONVERSATION_REASONING_OVERRIDE"], "enabled"
            )
            self.assertEqual(
                restricted["PAI_CONVERSATION_REASONING_EFFORT"], "high"
            )
            self.assertEqual(restricted["PAI_RECURSIVE_LOOP_TRACE"], "full")
            self.assertEqual(restricted["PAI_CURIOSITY_WAKE_SECONDS"], "300")
            self.assertEqual(restricted["PAI_DELIBERATE_CURIOSITY"], "1")
            self.assertEqual(restricted["PAI_CURIOSITY_BRIEFING"], "1")
            self.assertEqual(restricted["PAI_CURIOSITY_CONSOLIDATION"], "0")
            self.assertEqual(restricted["PAI_EPISODIC_MEMORY"], "1")
            self.assertEqual(
                restricted["PAI_CONTEXT_GRAPH_GENERATION_BUDGET_MICROUSD"],
                "5125001",
            )
            self.assertEqual(
                restricted["PAI_CONTEXT_GRAPH_PRIOR_EXPOSURE_MICROUSD"],
                "2400000",
            )
            self.assertEqual(
                restricted["PAI_CONTEXT_GRAPH_RUNTIME_PROFILE"],
                "reviewed-inference-v7",
            )
            self.assertEqual(
                restricted["PAI_CONTEXT_GRAPH_BUDGET_AUTHORIZATION_ID"],
                "fixture-auth-v1",
            )
            self.assertEqual(restricted["PAI_PRIVATE_BUDGET_PERCENT"], "30")
            self.assertEqual(restricted["PAI_CONTEXT_TRACE"], "metadata")
            self.assertEqual(
                restricted["PAI_CONTEXT_TRACE_DIR"], "/private/context-traces"
            )
            # Confirmed live: without these two, every /fleet-request and
            # /fleet-approve failed on any instance launched with
            # --recursive-tools -- the value reached this process's own
            # environment fine and was silently dropped right here.
            self.assertEqual(
                restricted["PAI_FLEET_OWN_ADDRESS"],
                "fixture-host.example.ts.net:8443",
            )
            self.assertEqual(restricted["PAI_FLEET_OWN_NAME"], "FixtureAgent")
            self.assertEqual(
                restricted["OPENROUTER_API_KEY"], "admitted-openrouter-key"
            )
            self.assertEqual(restricted["BRAVE_API_KEY"], "admitted-brave-key")
            self.assertNotIn("UNRELATED_API_KEY", restricted)
            self.assertNotIn("PAI_WEB_PASSWORD", restricted)
            self.assertEqual(restricted["HOME"], temporary)
            self.assertEqual(
                restricted["CL_SOURCE_REGISTRY"],
                "(:source-registry :ignore-inherited-configuration)",
            )
            if sys.platform == "win32":
                expected = Path(temporary)
                self.assertEqual(restricted["HOMEDRIVE"], expected.drive)
                self.assertEqual(
                    restricted["HOMEPATH"], str(expected)[len(expected.drive):]
                )
            self.assertTrue(Path(restricted["APPDATA"]).is_dir())
            self.assertTrue(Path(restricted["LOCALAPPDATA"]).is_dir())

    def test_selected_web_auth_survives_into_the_shared_lisp_process(self) -> None:
        with tempfile.TemporaryDirectory(prefix="pai-recursive-web-") as temporary:
            tool_environment = {
                "PAI_RECURSIVE_TOOLS": "container-development-v1",
                "PAI_RECURSIVE_WORKSPACE_ROOT": str(REPO),
                "PAI_RECURSIVE_BASH": "/bin/bash",
                "PAI_RESTRICTED_RUNTIME_HOME": temporary,
                "PAI_LISP_EVAL_REVIEW_LOG": str(
                    REPO / ".pai-review" / "lisp-evals.jsonl"
                ),
            }
            restricted = tool_runtime_environment(
                {
                    "PATH": "fixture-path",
                    "PAI_WEB_ENABLED": "1",
                    "PAI_WEB_ADDRESS": "0.0.0.0",
                    "PAI_WEB_PORT": "8080",
                    "PAI_WEB_USERNAME": "pai",
                    "PAI_WEB_PASSWORD": "a-private-password-with-32-characters",
                },
                tool_environment,
            )
            self.assertEqual("pai", restricted["PAI_WEB_USERNAME"])
            self.assertEqual(
                "a-private-password-with-32-characters",
                restricted["PAI_WEB_PASSWORD"],
            )

    def test_container_profile_is_explicitly_selected(self) -> None:
        with tempfile.TemporaryDirectory(prefix="pai-container-tools-") as temporary:
            with patch.dict(
                "os.environ",
                {"PAI_CONTAINER_PROFILE": "workspace-development-v1"},
            ):
                configured = recursive_tools_environment(
                    argparse.Namespace(recursive_tools=True, mind_loop="recursive"),
                    REPO,
                    Path(temporary),
                )
            self.assertEqual(
                configured["PAI_RECURSIVE_TOOLS"], "container-development-v1"
            )


if __name__ == "__main__":
    unittest.main()
