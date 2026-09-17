"""Provider selection contracts for the canonical pAI CLI."""

from __future__ import annotations

from pathlib import Path
import sys
import unittest


REPO = Path(__file__).resolve().parent.parent
sys.path.insert(0, str(REPO / "scripts"))

from pai_cli import (  # noqa: E402
    context_graph_runtime_environment,
    knowledge_graph_rebuild_environment,
    knowledge_graph_budget_environment,
    openrouter_reasoning_override,
    parse_args,
    remote_provider_api_key_env_name,
    validate_openrouter_model_slug,
)


class PaiCliProviderTests(unittest.TestCase):
    def test_open_ended_calls_have_no_global_cli_completion_cap(self) -> None:
        configured = parse_args([])
        self.assertFalse(hasattr(configured, "max_output_tokens"))

    def test_rebuild_only_excludes_every_other_private_runtime_surface(self) -> None:
        configured = parse_args(
            [
                "--mind-loop", "recursive",
                "--curiosity-wake-seconds", "30",
                "--no-curiosity-consolidation",
                "--episodic-memory",
                "--knowledge-graph-formation",
                "--knowledge-graph-rebuild-only",
            ]
        )
        self.assertEqual(
            knowledge_graph_rebuild_environment(configured),
            {
                "PAI_KNOWLEDGE_GRAPH_REBUILD_ONLY": "1",
                "PAI_CONTEXT_GRAPH_PROVIDER_MIN_INTERVAL_SECONDS": "15",
            },
        )
        for extra in ("--recursive-tools", "--web", "--curiosity-reach-out"):
            with self.subTest(extra=extra), self.assertRaises(SystemExit):
                knowledge_graph_rebuild_environment(
                    parse_args(
                        [
                            "--mind-loop", "recursive",
                            "--curiosity-wake-seconds", "30",
                            "--no-curiosity-consolidation",
                            "--episodic-memory",
                            "--knowledge-graph-formation",
                            "--knowledge-graph-rebuild-only",
                            extra,
                        ]
                    )
                )

    def test_rebuild_only_requires_knowledge_graph_formation(self) -> None:
        configured = parse_args(
            [
                "--mind-loop", "recursive",
                "--curiosity-wake-seconds", "30",
                "--no-curiosity-consolidation",
                "--knowledge-graph-rebuild-only",
            ]
        )
        with self.assertRaises(SystemExit):
            knowledge_graph_rebuild_environment(configured)

    def test_knowledge_graph_budget_is_explicit_cumulative_microusd(self) -> None:
        configured = parse_args(
            ["--knowledge-graph-formation", "--knowledge-graph-budget-usd", "5.125001"]
        )
        self.assertEqual(
            knowledge_graph_budget_environment(configured),
            {"PAI_CONTEXT_GRAPH_GENERATION_BUDGET_MICROUSD": "5125001"},
        )

    def test_knowledge_graph_budget_requires_formation_and_exact_precision(self) -> None:
        for arguments in (
            ["--knowledge-graph-budget-usd", "5"],
            ["--knowledge-graph-formation", "--knowledge-graph-budget-usd", "0"],
            ["--knowledge-graph-formation", "--knowledge-graph-budget-usd", "1.0000001"],
        ):
            with self.subTest(arguments=arguments), self.assertRaises(SystemExit):
                knowledge_graph_budget_environment(parse_args(arguments))

    def test_replacement_profile_requires_complete_exposure_accounting(self) -> None:
        configured = parse_args(
            [
                "--knowledge-graph-formation",
                "--knowledge-graph-budget-usd", "4.5",
                "--knowledge-graph-prior-exposure-usd", "2.4",
                "--context-graph-budget-authorization-id", "fixture-auth-v1",
                "--context-graph-runtime-profile", "reviewed-inference-v8",
            ]
        )
        self.assertEqual(
            knowledge_graph_budget_environment(configured),
            {
                "PAI_CONTEXT_GRAPH_GENERATION_BUDGET_MICROUSD": "4500000",
                "PAI_CONTEXT_GRAPH_PRIOR_EXPOSURE_MICROUSD": "2400000",
            },
        )
        self.assertEqual(
            context_graph_runtime_environment(configured),
            {"PAI_CONTEXT_GRAPH_RUNTIME_PROFILE": "reviewed-inference-v8",
             "PAI_CONTEXT_GRAPH_BUDGET_AUTHORIZATION_ID": "fixture-auth-v1"},
        )

        typed = parse_args(
            [
                "--knowledge-graph-formation",
                "--knowledge-graph-budget-usd", "5",
                "--knowledge-graph-prior-exposure-usd", "4.1",
                "--context-graph-budget-authorization-id", "fixture-auth-v2",
                "--context-graph-runtime-profile", "reviewed-inference-v9",
            ]
        )
        self.assertEqual(
            context_graph_runtime_environment(typed),
            {"PAI_CONTEXT_GRAPH_RUNTIME_PROFILE": "reviewed-inference-v9",
             "PAI_CONTEXT_GRAPH_BUDGET_AUTHORIZATION_ID": "fixture-auth-v2"},
        )

    def test_replacement_profile_refuses_implicit_budget_or_prior_exposure(self) -> None:
        for arguments in (
            ["--context-graph-runtime-profile", "reviewed-inference-v7"],
            ["--context-graph-runtime-profile", "reviewed-inference-v8"],
            ["--context-graph-runtime-profile", "reviewed-inference-v9"],
            [
                "--knowledge-graph-formation",
                "--knowledge-graph-budget-usd", "4.5",
                "--context-graph-runtime-profile", "reviewed-inference-v7",
            ],
        ):
            with self.subTest(arguments=arguments), self.assertRaises(SystemExit):
                context_graph_runtime_environment(parse_args(arguments))

    def test_predecessor_exposure_cannot_exceed_cumulative_budget(self) -> None:
        configured = parse_args(
            [
                "--knowledge-graph-formation",
                "--knowledge-graph-budget-usd", "2",
                "--knowledge-graph-prior-exposure-usd", "2.000001",
            ]
        )
        with self.assertRaises(SystemExit):
            knowledge_graph_budget_environment(configured)

    def test_graph_budget_rejects_non_finite_amounts(self) -> None:
        for amount in ("NaN", "Infinity", "-Infinity"):
            configured = parse_args(
                ["--knowledge-graph-formation",
                 f"--knowledge-graph-budget-usd={amount}"]
            )
            with self.subTest(amount=amount), self.assertRaises(SystemExit):
                knowledge_graph_budget_environment(configured)

    def test_openrouter_model_override_is_explicit(self) -> None:
        configured = parse_args(
            [
                "--openrouter-model", "openai/gpt-oss-120b:free",
                "--openrouter-zdr", "allow-non-zdr",
                "--openrouter-data-collection", "allow",
                "--openrouter-reasoning", "enabled",
                "--openrouter-reasoning-effort", "high",
            ]
        )
        self.assertEqual(configured.openrouter_model, "openai/gpt-oss-120b:free")
        self.assertEqual(configured.openrouter_zdr, "allow-non-zdr")
        self.assertEqual(configured.openrouter_data_collection, "allow")
        self.assertEqual(
            openrouter_reasoning_override(configured), ("enabled", "high")
        )

    def test_model_override_defaults_to_native_reasoning(self) -> None:
        configured = parse_args(["--openrouter-model", "openai/gpt-oss-120b"])
        self.assertEqual(
            openrouter_reasoning_override(configured), ("model-default", None)
        )

    def test_bare_enabled_reasoning_defaults_to_medium(self) -> None:
        configured = parse_args(["--openrouter-reasoning", "enabled"])
        self.assertEqual(
            openrouter_reasoning_override(configured), ("enabled", "medium")
        )
        self.assertEqual(configured.private_reasoning_effort, "minimal")

    def test_effort_rejects_a_conflicting_reasoning_mode(self) -> None:
        configured = parse_args(
            [
                "--openrouter-reasoning", "disabled",
                "--openrouter-reasoning-effort", "low",
            ]
        )
        with self.assertRaisesRegex(SystemExit, "requires reasoning mode enabled"):
            openrouter_reasoning_override(configured)

    def test_openrouter_model_slug_accepts_author_and_model(self) -> None:
        self.assertEqual(
            validate_openrouter_model_slug("anthropic/claude-sonnet-4.5"),
            "anthropic/claude-sonnet-4.5",
        )

    def test_openrouter_model_slug_rejects_ambiguous_or_unbounded_values(self) -> None:
        for value in ("no-author", "a/b/c", "a/model with spaces", "a/", "/b"):
            with self.subTest(value=value), self.assertRaises(SystemExit):
                validate_openrouter_model_slug(value)

    def test_openrouter_profile_reads_its_own_key(self) -> None:
        profile = {"provider": "openrouter"}
        self.assertEqual(
            remote_provider_api_key_env_name(
                profile, {"OPENROUTER_API_KEY": "sk-or-fixture"}
            ),
            "OPENROUTER_API_KEY",
        )

    def test_nous_portal_profile_reads_its_own_key_not_openrouters(self) -> None:
        profile = {"provider": "nous-portal"}
        self.assertEqual(
            remote_provider_api_key_env_name(
                profile, {"NOUS_PORTAL_API_KEY": "sk-nous-fixture"}
            ),
            "NOUS_PORTAL_API_KEY",
        )
        # Having only the OTHER provider's key present is not good enough --
        # this was exactly the pre-existing bug the fix replaced.
        with self.assertRaisesRegex(SystemExit, "NOUS_PORTAL_API_KEY"):
            remote_provider_api_key_env_name(
                profile, {"OPENROUTER_API_KEY": "sk-or-fixture"}
            )

    def test_missing_key_for_the_declared_provider_is_refused(self) -> None:
        with self.assertRaisesRegex(SystemExit, "OPENROUTER_API_KEY is missing"):
            remote_provider_api_key_env_name({"provider": "openrouter"}, {})
        with self.assertRaisesRegex(SystemExit, "NOUS_PORTAL_API_KEY is missing"):
            remote_provider_api_key_env_name({"provider": "nous-portal"}, {})

    def test_an_undeclared_or_unknown_provider_is_refused(self) -> None:
        environ = {"OPENROUTER_API_KEY": "sk-or-fixture", "NOUS_PORTAL_API_KEY": "sk-nous-fixture"}
        for profile in ({}, {"provider": "unknown-vendor"}, None, "not-a-dict"):
            with self.subTest(profile=profile), self.assertRaisesRegex(
                SystemExit, "not a declared remote provider profile"
            ):
                remote_provider_api_key_env_name(profile, environ)


if __name__ == "__main__":
    unittest.main()
