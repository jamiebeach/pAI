"""Persistent lab worker shares one durable event-ledger budget authority."""
import json
from pathlib import Path
import shutil
import sys
import tempfile
import unittest
import os

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "scripts"))
from context_graph_episode_lab import LispWorker, WorkerBudgetGateway


TEST_IMAGE = os.environ.get("PAI_TEST_IMAGE")


@unittest.skipUnless(shutil.which("docker") and TEST_IMAGE,
                     "Docker and PAI_TEST_IMAGE are required for the isolated worker")
class BudgetWorkerIntegration(unittest.TestCase):
    def test_reservation_and_settlement_survive_worker_protocol(self):
        with tempfile.TemporaryDirectory(prefix="pai-budget-worker-") as directory:
            root = Path(directory)
            checkpoint = root / "checkpoint.json"
            checkpoint.write_text("{}", encoding="utf-8")
            database = root / "events.sqlite3"
            database.touch()
            policy = {
                "authorization_id": "worker-fixture-auth",
                "agent_id": "worker-fixture-agent",
                "persona_id": "worker-fixture-persona",
                "generation": "worker-fixture-generation",
                "prior_exposure_microusd": 10,
                "ceiling_microusd": 100,
                "per_request_ceiling_microusd": 100,
            }
            manifest = {
                "input_kind": "checkpoint", "storage_mode": "reference",
                "events": {"file": str(checkpoint)},
                "partition": {"agent_id": policy["agent_id"],
                              "persona_id": policy["persona_id"]},
            }
            worker = LispWorker.start(root, manifest, "docker", TEST_IMAGE,
                                      budget_database=database, budget_policy=policy)
            try:
                gateway = WorkerBudgetGateway(worker)
                self.assertEqual(gateway.snapshot()["exposure_microusd"], 10)
                reservation = gateway.reserve("worker-attempt", "worker-digest", "review", 20)
                self.assertEqual(reservation["type"], "context-graph-budget-reserved")
                self.assertEqual(gateway.snapshot()["exposure_microusd"], 30)
                settlement = gateway.settle("worker-attempt", "worker-digest", 5,
                                            "worker-provider-receipt")
                self.assertEqual(settlement["type"], "context-graph-budget-settled")
                self.assertEqual(gateway.snapshot()["exposure_microusd"], 15)
            finally:
                worker.close()


if __name__ == "__main__":
    unittest.main()
