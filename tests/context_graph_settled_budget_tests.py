"""Offline budget settlement regression tests; no provider requests."""
import argparse
import json
import math
from pathlib import Path
import sys
import tempfile
import unittest
from unittest.mock import patch

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / 'scripts'))
import context_graph_resolution_lab as lab


class SettledBudgetTests(unittest.TestCase):
    def fixture(self, directory, cost=.1):
        response = {'usage': {'cost': cost}}
        path = directory / 'receipt.json'
        path.write_text(json.dumps(response), encoding='utf-8')
        charged = cost if isinstance(cost, (float,int)) and not isinstance(cost,bool) and math.isfinite(cost) and cost >= 0 else .9
        row = dict(status='received', bound_usd=.9, charged_usd=charged,
                   response_path=str(path), response_sha256=lab.base.canonical_sha256(response))
        args = argparse.Namespace(max_output_tokens=8, cost_ceiling_usd=1, request_limit=10,
                                  transient_retries=0, provider_timeout_seconds=None)
        return lab.Calls(directory, args, prior_rows=[row])

    def test_settled_cost_replaces_reservation_without_erasing_audit(self):
        with tempfile.TemporaryDirectory() as tmp:
            calls = self.fixture(Path(tmp))
            self.assertEqual(calls.reserved, .9)
            self.assertEqual(calls.budget_used_usd, .1)
            with patch.object(lab.base, 'request_cost_bound', return_value=.2), \
                 patch.object(lab.base, 'openrouter_call', return_value={'usage': {'cost': .05}}), \
                 patch.dict(lab.os.environ, {'OPENROUTER_API_KEY':'synthetic-test'}):
                calls.call('next', {'max_tokens':8})
            self.assertAlmostEqual(calls.reserved, 1.1)
            self.assertAlmostEqual(calls.budget_used_usd, .15)

    def test_pending_and_unknown_costs_keep_full_bounds(self):
        with tempfile.TemporaryDirectory() as tmp:
            calls = self.fixture(Path(tmp))
            for status in ('admitted','failed','http-rejected','transient-failed'):
                calls.rows[0]['status'] = status
                self.assertEqual(calls.budget_used_usd, .9)
            calls.rows[0]['status'] = 'received'
            calls.rows[0].pop('response_path')
            self.assertEqual(calls.budget_used_usd, .9)

    def test_missing_invalid_usage_keeps_bound(self):
        with tempfile.TemporaryDirectory() as tmp:
            for cost in (None, True, -1, float('nan')):
                calls = self.fixture(Path(tmp), cost)
                self.assertEqual(calls.budget_used_usd, .9)

    def test_receipt_drift_fails_closed(self):
        with tempfile.TemporaryDirectory() as tmp:
            calls = self.fixture(Path(tmp))
            Path(calls.rows[0]['response_path']).write_text('{}', encoding='utf-8')
            with self.assertRaises(ValueError):
                _ = calls.budget_used_usd

    def test_invalid_bound_fails_closed(self):
        with tempfile.TemporaryDirectory() as tmp:
            calls = self.fixture(Path(tmp))
            for value in (float('nan'), -1, True):
                calls.rows[0]['bound_usd'] = value
                with self.assertRaises(ValueError):
                    _ = calls.budget_used_usd

    def test_ledger_charge_drift_fails_closed(self):
        with tempfile.TemporaryDirectory() as tmp:
            calls = self.fixture(Path(tmp))
            calls.rows[0]['charged_usd'] = 0
            with self.assertRaises(ValueError):
                _ = calls.budget_used_usd

    def test_real_spend_cap_still_blocks_before_dispatch(self):
        with tempfile.TemporaryDirectory() as tmp:
            calls = self.fixture(Path(tmp), .85)
            with patch.object(lab.base, 'request_cost_bound', return_value=.2), \
                 patch.object(lab.base, 'openrouter_call') as provider:
                with self.assertRaises(lab.BudgetDeferred):
                    calls.call('next', {'max_tokens':8})
                provider.assert_not_called()
            self.assertEqual(len(calls.rows), 1)

if __name__ == '__main__': unittest.main()
