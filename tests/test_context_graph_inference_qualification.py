"""Provider-free contracts for the capped private inference qualifier."""

from __future__ import annotations

import json
import os
from pathlib import Path
import sys
import tempfile
import unittest
from unittest import mock


REPO = Path(__file__).resolve().parent.parent
sys.path.insert(0, str(REPO / "scripts"))

import context_graph_inference_qualification as qualifier  # noqa: E402
import context_graph_inference_qualification_resume as resume  # noqa: E402


def row(event_id: int, status: str = "completed") -> dict:
    return {"episode_event_id": event_id, "status": status}


class ContextGraphInferenceQualificationTests(unittest.TestCase):
    def write_json(self, path: Path, value: object) -> None:
        path.write_text(json.dumps(value, sort_keys=True), encoding="utf-8")

    def write_stopped_run(self, path: Path, seed: Path, manifest: dict,
                          selected: list[int], rows: list[dict],
                          provider_calls: int, prior: dict | None = None,
                          status: str = "stopped",
                          maximum_calls: int = 28) -> None:
        path.mkdir()
        seal = {
            "schema_version": 1,
            "protocol": qualifier.PROTOCOL,
            "ontology_revision": qualifier.ONTOLOGY_REVISION,
            "execute": True,
            "database_writes": 0,
            "automatic_retries": 0,
            "provider": "phala",
            "zdr": True,
            "data_collection": "deny",
            "request_ceiling_usd": qualifier.REQUEST_CEILING_USD,
            "episode_event_ids": selected,
            "seed_manifest_sha256": qualifier.base.sha256_file(
                seed / "manifest.json"),
            "event_authority_sha256": manifest["events"]["sha256"],
            "head_event_id": manifest["head_event_id"],
            "maximum_calls": maximum_calls,
            "cost_ceiling_usd": 2.0,
            "prior_run": prior,
        }
        self.write_json(path / "seal.json", seal)
        self.write_json(path / "report.json", {
            "status": status, "database_writes": 0,
            "provider_calls": provider_calls,
        })
        self.write_json(path / "calls.json", rows)

    def test_phase_output_limits_match_production_phase_shapes(self) -> None:
        self.assertEqual(qualifier.phase_output_tokens("review"), 8192)
        for phase in ("new-identity-groups", "new-identities", "facts"):
            self.assertEqual(qualifier.phase_output_tokens(phase), 4096)
        for phase in ("mentions", "pages"):
            self.assertEqual(qualifier.phase_output_tokens(phase), 2048)

    def test_baseline_checkpoint_is_private_and_contract_pinned(self) -> None:
        with tempfile.TemporaryDirectory() as folder:
            root = Path(folder)
            private = root / ".scratch"
            private.mkdir()
            checkpoint = private / "checkpoint.json"
            contract = {
                "agent_id": "agent:test", "persona_id": "persona:test",
                "protocol": qualifier.PROTOCOL,
                "ontology_revision": qualifier.ONTOLOGY_REVISION,
            }
            self.write_json(checkpoint, {
                "status": "target", "digest": "a" * 64,
                "contract": contract, "envelope": {},
            })
            manifest = {"partition": {"agent_id": "agent:test",
                                       "persona_id": "persona:test"}}
            with (mock.patch.object(qualifier, "ROOT", root),
                  mock.patch.dict(os.environ, {"PAI_PRIVATE_SCRATCH": str(private)})):
                descriptor = qualifier.load_baseline_checkpoint(
                    checkpoint, manifest
                )
                self.assertEqual(descriptor["digest"], "a" * 64)
                self.assertEqual(descriptor["contract"], contract)
                self.assertEqual(len(descriptor["sha256"]), 64)
                outside = root / "outside.json"
                self.write_json(outside, json.loads(checkpoint.read_text()))
                with self.assertRaisesRegex(ValueError, "private artifact"):
                    qualifier.load_baseline_checkpoint(outside, manifest)

    def test_replay_prefix_requires_a_completed_native_tool_call(self) -> None:
        completed = {
            "choices": [{
                "finish_reason": "tool_calls",
                "message": {"role": "assistant", "tool_calls": [{}]},
            }],
        }
        self.assertTrue(qualifier.replayable_native_response(completed))
        for changed in (
            {"choices": [{**completed["choices"][0],
                           "finish_reason": "length"}]},
            {"choices": [{"finish_reason": "tool_calls",
                           "message": {"refusal": "blocked",
                                       "tool_calls": [{}]}}]},
            {"choices": [{"finish_reason": "tool_calls",
                           "message": {"tool_calls": []}}]},
        ):
            with self.subTest(changed=changed):
                self.assertFalse(
                    qualifier.replayable_native_response(changed)
                )

    def test_replay_allows_only_a_larger_output_limit(self) -> None:
        sealed = {
            "model": "fixture", "max_tokens": 4096,
            "messages": [{"role": "user", "content": "same"}],
            "provider": {"only": ["phala"], "zdr": True},
        }
        self.assertTrue(
            qualifier.output_limit_extension_compatible(
                sealed, {**sealed, "max_tokens": 8192}
            )
        )
        self.assertFalse(
            qualifier.output_limit_extension_compatible(
                sealed, {**sealed, "max_tokens": 2048}
            )
        )
        self.assertFalse(
            qualifier.output_limit_extension_compatible(
                sealed, {**sealed, "model": "changed", "max_tokens": 8192}
            )
        )

    def test_episode_selection_accepts_ordered_completed_authority_rows(self) -> None:
        catalog = {"episodes": [row(10), row(20, "queued"), row(30, "failed")]}
        self.assertEqual(
            qualifier.validate_episode_selection(catalog, [10, 20]),
            [row(10), row(20, "queued")],
        )

    def test_episode_selection_rejects_ambiguous_or_unusable_inputs(self) -> None:
        catalog = {"episodes": [
            row(10), row(20, "queued"), row(30, "failed"), row(40, "opened")
        ]}
        rejected = ([10, 10], [20, 10], [10, 50], [30], [40])
        for selected in rejected:
            with self.subTest(selected=selected), self.assertRaises(ValueError):
                qualifier.validate_episode_selection(catalog, selected)

    def test_episode_batch_scope_is_ordered_bounded_and_partitioned(self) -> None:
        self.assertEqual(
            qualifier.parse_episode_batches(
                ["10:1", "20:0", "20:2"], [10, 20]
            ),
            [
                {"episode_event_id": 10, "batch_index": 1},
                {"episode_event_id": 20, "batch_index": 0},
                {"episode_event_id": 20, "batch_index": 2},
            ],
        )
        self.assertIsNone(qualifier.parse_episode_batches(None, [10, 20]))
        for invalid in (["20:0", "10:1"], ["10:-1"], ["30:0"],
                        ["10:1", "10:1"], ["invalid"]):
            with self.subTest(invalid=invalid), self.assertRaises(ValueError):
                qualifier.parse_episode_batches(invalid, [10, 20])

    def test_private_output_must_be_new_and_below_scratch(self) -> None:
        permitted = qualifier.private_scratch_root() / "qualifier-test-never-created"
        self.assertEqual(qualifier.require_private_output(permitted),
                         permitted.resolve())
        for rejected in (qualifier.ROOT, qualifier.private_scratch_root()):
            with self.subTest(path=rejected), self.assertRaises(ValueError):
                qualifier.require_private_output(rejected)

    def test_private_scratch_rejects_repository_and_filesystem_roots(self) -> None:
        for rejected in (qualifier.ROOT, Path(qualifier.ROOT.anchor)):
            with (self.subTest(path=rejected),
                  mock.patch.dict(os.environ,
                                  {"PAI_PRIVATE_SCRATCH": str(rejected)}),
                  self.assertRaisesRegex(ValueError, "bounded directory")):
                qualifier.private_scratch_root()

    def test_batch_call_limits_cover_the_exact_ordered_scope(self) -> None:
        batches = [
            {"episode_event_id": 10, "batch_index": 1},
            {"episode_event_id": 20, "batch_index": 0},
        ]
        self.assertEqual(
            qualifier.parse_episode_batch_call_limits(
                ["10:1:8", "20:0:10"], batches
            ),
            [
                {"episode_event_id": 10, "batch_index": 1,
                 "maximum_calls": 8},
                {"episode_event_id": 20, "batch_index": 0,
                 "maximum_calls": 10},
            ],
        )
        self.assertIsNone(
            qualifier.parse_episode_batch_call_limits(None, batches)
        )
        for invalid in (["10:1:8"], ["20:0:8", "10:1:10"],
                        ["10:1:0", "20:0:10"], ["bad"]):
            with self.subTest(invalid=invalid), self.assertRaises(ValueError):
                qualifier.parse_episode_batch_call_limits(invalid, batches)
        with self.assertRaises(ValueError):
            qualifier.parse_episode_batch_call_limits(["10:1:8"], None)

    def test_bounded_scope_can_replay_an_unscoped_first_batch(self) -> None:
        tasks = [
            {"episode_event_id": 10, "batch_index": 0},
            {"episode_event_id": 10, "batch_index": 1},
        ]
        self.assertTrue(
            qualifier.replay_task_prefix_compatible(None, tasks, [10, 20])
        )
        self.assertTrue(
            qualifier.replay_task_prefix_compatible(tasks, tasks, [10, 20])
        )
        batch_one = [{"episode_event_id": 20, "batch_index": 1}]
        self.assertTrue(
            qualifier.replay_task_prefix_compatible(batch_one, batch_one, [10, 20])
        )
        self.assertFalse(
            qualifier.replay_task_prefix_compatible(
                None,
                [{"episode_event_id": 10, "batch_index": 1}],
                [10, 20],
            )
        )

    def test_budget_constants_bound_each_trial_and_request(self) -> None:
        self.assertEqual(qualifier.PROTOCOL, "identity-formation-v14")
        self.assertEqual(
            qualifier.ONTOLOGY_REVISION,
            "personal-context-core-glm53-v1.3",
        )
        self.assertLessEqual(qualifier.REQUEST_CEILING_USD,
                             qualifier.MAX_TRIAL_CAP_USD)
        self.assertEqual(qualifier.MAX_CALLS_PER_BATCH, 14)
        self.assertGreaterEqual(qualifier.MAX_TRIAL_CALLS,
                                qualifier.MAX_CALLS_PER_BATCH)

    def test_verified_transient_http_reject_does_not_consume_call_allowance(self) -> None:
        rows = [
            {"status": "received"},
            {"status": "transient-failed", "http_status": 429},
            {"status": "failed"},
            {"status": "transient-failed"},
        ]
        self.assertEqual(qualifier.qualification_call_count(rows), 3)

    def test_invalid_response_does_not_consume_call_allowance(self) -> None:
        rows = [
            {"status": "received"},
            {"status": "invalid-response"},
            {"status": "failed"},
        ]
        self.assertEqual(qualifier.qualification_call_count(rows), 2)

    def test_worker_supports_ephemeral_read_only_config_without_a_file(self) -> None:
        source = (REPO / "scripts" /
                  "context-graph-inference-qualification.lisp").read_text(
                      encoding="utf-8")
        self.assertIn(
            "PAI_CONTEXT_GRAPH_INFERENCE_QUALIFICATION_CONFIG_JSON",
            source,
        )

    def test_worker_places_applications_after_restored_checkpoint_watermark(self) -> None:
        source = (REPO / "scripts" /
                  "context-graph-inference-qualification.lisp").read_text(
                      encoding="utf-8")
        self.assertIn("(max head", source)
        self.assertIn("context-graph-through-event-id graph", source)
        self.assertIn("(+ application-base 2 (* task-ordinal 2))", source)

    def test_mentions_require_explicit_health_states(self) -> None:
        source = (REPO / "src" / "mind" / "knowledge" / "context-graph" /
                  "identity-formation.lisp").read_text(encoding="utf-8")
        self.assertIn(
            "Every explicitly self-reported or diagnosed health condition, "
            "deficiency, or medical state MUST be selected",
            source,
        )
        self.assertIn(
            "prefer the source-present conventional noun form as the label",
            source,
        )

    def test_source_batch_inventory_derives_complete_cumulative_call_cap(self) -> None:
        ready = {
            "source_batch_counts": [3, 3],
            "source_batch_count": 6,
            "source_batch_call_limits": [14, 14, 14, 14, 14, 14],
            "request_count_upper_bound": 84,
        }
        self.assertEqual(qualifier.workload_request_upper_bound(ready), 84)
        self.assertEqual(
            qualifier.required_cumulative_call_cap(12, ready, 1), 97
        )
        for changed in (
            {**ready, "source_batch_count": 5},
            {**ready, "request_count_upper_bound": 83},
            {**ready, "source_batch_counts": [3, -1]},
            {**ready, "source_batch_counts": [0, 0],
             "source_batch_count": 0,
             "source_batch_call_limits": [],
             "request_count_upper_bound": 0},
            {**ready, "source_batch_call_limits": [14, 14]},
        ):
            with self.subTest(changed=changed), self.assertRaises(ValueError):
                qualifier.workload_request_upper_bound(changed)

    def test_worker_receipt_binds_protocol_ontology_and_zero_writes(self) -> None:
        ready = {
            "schema_version": 1,
            "protocol": qualifier.PROTOCOL,
            "ontology_revision": qualifier.ONTOLOGY_REVISION,
            "database_write_count": 0,
        }
        self.assertIsNone(qualifier.validate_worker_contract(ready))
        for changed in (
            {**ready, "protocol": "identity-formation-v13"},
            {**ready, "ontology_revision": "personal-context-core-glm53-v1.2"},
            {**ready, "database_write_count": 1},
        ):
            with self.subTest(changed=changed), self.assertRaises(ValueError):
                qualifier.validate_worker_contract(changed)

    def test_reviewed_admission_drift_requires_explicit_renormalization(self) -> None:
        adapter = next(iter(
            resume.REVIEWED_ADMISSION_RENORMALIZATION_ALLOWLIST
        ))
        launcher = next(iter(resume.LAUNCHER_DRIFT_ALLOWLIST))
        sealed = {adapter: "old-adapter", launcher: "old-launcher",
                  "src/kernel/event-log.lisp": "stable"}
        current = {adapter: "new-adapter", launcher: "new-launcher",
                   "src/kernel/event-log.lisp": "stable"}

        with self.assertRaisesRegex(ValueError, "formation-semantic"):
            resume.validate_source_drift(sealed, current)
        drift = resume.validate_source_drift(
            sealed, current, renormalize_reviewed_admission=True
        )
        self.assertEqual(
            drift[adapter]["classification"],
            "reviewed-admission-renormalization",
        )
        self.assertEqual(
            drift[launcher]["classification"], "qualification-launcher"
        )

    def test_renormalization_does_not_allow_other_semantic_drift(self) -> None:
        sealed = {"src/kernel/event-log.lisp": "old"}
        current = {"src/kernel/event-log.lisp": "new"}
        with self.assertRaisesRegex(ValueError, "formation-semantic"):
            resume.validate_source_drift(
                sealed, current, renormalize_reviewed_admission=True
            )

    def test_prior_budget_lineage_recursively_carries_conservative_exposure(self) -> None:
        scratch = qualifier.private_scratch_root()
        scratch.mkdir(exist_ok=True)
        with tempfile.TemporaryDirectory(dir=scratch) as directory:
            root = Path(directory)
            seed = root / "seed"
            seed.mkdir()
            self.write_json(seed / "manifest.json", {"fixture": True})
            manifest = {"events": {"sha256": "event-authority-digest"},
                        "head_event_id": 99}
            selected = [10, 20]
            first = root / "first"
            self.write_stopped_run(
                first, seed, manifest, selected,
                [{"stage": "first", "bound_usd": 0.01,
                  "status": "transient-failed"}],
                provider_calls=1,
            )
            first_lineage = qualifier.prior_budget_lineage(
                first, seed, manifest, selected, 2.0, 28)
            second = root / "second"
            self.write_stopped_run(
                second, seed, manifest, selected,
                [{"stage": "second", "bound_usd": 0.02,
                  "status": "transient-failed"}],
                provider_calls=2, prior=first_lineage,
            )

            lineage = qualifier.prior_budget_lineage(
                second, seed, manifest, selected, 2.0, 28)

            self.assertEqual(lineage["call_count"], 2)
            self.assertAlmostEqual(lineage["exposure_usd"], 0.03)

    def test_prior_budget_lineage_rejects_changed_embedded_receipt(self) -> None:
        scratch = qualifier.private_scratch_root()
        scratch.mkdir(exist_ok=True)
        with tempfile.TemporaryDirectory(dir=scratch) as directory:
            root = Path(directory)
            seed = root / "seed"
            seed.mkdir()
            self.write_json(seed / "manifest.json", {"fixture": True})
            manifest = {"events": {"sha256": "event-authority-digest"},
                        "head_event_id": 99}
            selected = [10, 20]
            first = root / "first"
            self.write_stopped_run(
                first, seed, manifest, selected,
                [{"stage": "first", "bound_usd": 0.01,
                  "status": "transient-failed"}],
                provider_calls=1,
            )
            embedded = qualifier.prior_budget_lineage(
                first, seed, manifest, selected, 2.0, 28)
            embedded["exposure_usd"] = 0.001
            second = root / "second"
            self.write_stopped_run(
                second, seed, manifest, selected,
                [{"stage": "second", "bound_usd": 0.02,
                  "status": "transient-failed"}],
                provider_calls=2, prior=embedded,
            )

            with self.assertRaisesRegex(ValueError, "lineage changed"):
                qualifier.prior_budget_lineage(
                    second, seed, manifest, selected, 2.0, 28)

    def test_completed_trial_can_fund_a_corrected_cumulative_lineage(self) -> None:
        scratch = qualifier.private_scratch_root()
        scratch.mkdir(exist_ok=True)
        with tempfile.TemporaryDirectory(dir=scratch) as directory:
            root = Path(directory)
            seed = root / "seed"
            seed.mkdir()
            self.write_json(seed / "manifest.json", {"fixture": True})
            manifest = {"events": {"sha256": "event-authority-digest"},
                        "head_event_id": 99}
            run = root / "completed"
            self.write_stopped_run(
                run, seed, manifest, [10, 20],
                [{"stage": "only", "bound_usd": 0.01,
                  "status": "transient-failed"}],
                provider_calls=1, status="completed-requires-human-review",
            )

            lineage = qualifier.prior_budget_lineage(
                run, seed, manifest, [10, 20], 2.0, 28)

            self.assertEqual(lineage["call_count"], 1)

    def test_resume_chain_accepts_only_existing_private_receipt_roots(self) -> None:
        scratch = qualifier.private_scratch_root()
        scratch.mkdir(exist_ok=True)
        with tempfile.TemporaryDirectory(dir=scratch) as directory:
            root = Path(directory)
            first = root / "first"
            second = root / "second"
            third = root / "third"
            for path in (first, second, third):
                path.mkdir()
            roots = resume.receipt_roots(
                third, {"resumed_from": str(second),
                        "receipt_roots": [str(first)]})
            self.assertEqual(roots, {first.resolve(), second.resolve(),
                                     third.resolve()})
            with self.assertRaisesRegex(ValueError, "outside private"):
                resume.receipt_roots(third, {"resumed_from": str(root / "missing")})

    def test_exhausted_lineage_is_audit_valid_but_not_resumable(self) -> None:
        scratch = qualifier.private_scratch_root()
        scratch.mkdir(exist_ok=True)
        with tempfile.TemporaryDirectory(dir=scratch) as directory:
            root = Path(directory)
            seed = root / "seed"
            seed.mkdir()
            self.write_json(seed / "manifest.json", {"fixture": True})
            manifest = {"events": {"sha256": "event-authority-digest"},
                        "head_event_id": 99}
            run = root / "exhausted"
            self.write_stopped_run(
                run, seed, manifest, [10, 20],
                [{"stage": "only", "bound_usd": 0.01,
                  "status": "transient-failed"}],
                provider_calls=1, maximum_calls=1,
            )

            with self.assertRaisesRegex(ValueError, "exhausts"):
                qualifier.prior_budget_lineage(
                    run, seed, manifest, [10, 20], 2.0, 1)
            lineage = qualifier.prior_budget_lineage(
                run, seed, manifest, [10, 20], 2.0, 1,
                _require_remaining=False)
            self.assertEqual(lineage["call_count"], 1)
            amended = qualifier.prior_budget_lineage(
                run, seed, manifest, [10, 20], 2.0, 2)
            self.assertEqual(amended["call_count"], 1)


if __name__ == "__main__":
    unittest.main()
