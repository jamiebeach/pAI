import argparse
import copy
from pathlib import Path
import sys
import unittest

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / 'scripts'))
import context_graph_identity_lab as lab


class IdentityTransportTests(unittest.TestCase):
    def test_trial_call_bound_does_not_reset_cumulative_spend(self):
        self.assertEqual((28, 123), lab.request_limits(2, True, 95))
        self.assertEqual((104, 199), lab.request_limits(8, False, 95))
        self.assertEqual((13, 13), lab.request_limits(1, False, 0))
        for count in (0, 9, True, 1.5):
            with self.assertRaises(ValueError): lab.request_limits(count, True, 95)
        with self.assertRaises(ValueError): lab.request_limits(2, True, -1)

    def test_review_preserves_schema_input_and_zdr(self):
        spec = {'adapter_revision': 'identity-formation-review-v2', 'tool_name': 'review',
                'schema': {'type': 'object', 'properties': {}, 'additionalProperties': False},
                'system': 'independent review', 'input': {'identity_comparison': {'bindings': []}}}
        original = copy.deepcopy(spec)
        args = argparse.Namespace(model='meta/muse-glimmer-30b', max_output_tokens=8192,
            openrouter_zdr='require', openrouter_data_collection='deny',
            max_prompt_price=.30, max_completion_price=1.10,
            openrouter_provider_only='phala', reasoning_policy='low')
        request = lab.request_from_spec(spec, args)
        self.assertEqual(spec, original)
        self.assertEqual(request['tools'][0]['function']['parameters'], spec['schema'])
        self.assertIn('identity_comparison', request['messages'][1]['content'])
        self.assertIs(request['provider']['zdr'], True)
        self.assertIs(request['provider']['allow_fallbacks'], False)
        args.openrouter_zdr = 'allow'
        with self.assertRaises(ValueError): lab.request_from_spec(spec, args)
        args.openrouter_zdr = 'require'
        spec['adapter_revision'] = 'unknown'
        with self.assertRaises(ValueError): lab.request_from_spec(spec, args)


if __name__ == '__main__':
    unittest.main()
