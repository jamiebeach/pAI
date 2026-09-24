"""Exercise host-launcher result handling; fake SBCL never runs cognitive code."""
from pathlib import Path
import os
import shutil
import subprocess
import tempfile
import unittest

ROOT = Path(__file__).parents[1]

@unittest.skipUnless(os.name == 'posix' and shutil.which('sh'), 'requires POSIX process execution')
class IsolatedRunnerTests(unittest.TestCase):
    def run_case(self, output, code=0, suite=True):
        with tempfile.TemporaryDirectory() as temporary:
            root=Path(temporary)
            (root/'tests').mkdir()
            if suite: (root/'tests/fixture-tests.lisp').write_text('; synthetic launcher input\n')
            fake=root/'fake-sbcl'
            fake.write_text('#!/usr/bin/env python3\nimport os\nprint(os.environ["FAKE_OUTPUT"])\nraise SystemExit(int(os.environ["FAKE_EXIT"]))\n')
            fake.chmod(0o755)
            result=subprocess.run(['sh',str(ROOT/'tests/run-isolated.sh'),str(fake),str(root)],
                env={**os.environ,'FAKE_OUTPUT':output,'FAKE_EXIT':str(code),
                     'PAI_TEST_STATE':str(root/'scratch'),'PAI_TEST_LOG_DIR':str(root/'logs')},
                text=True,capture_output=True)
            log=(root/'logs/fixture-tests.lisp.log')
            if suite: self.assertEqual(log.read_text(),output+'\n')
            return result.returncode,result.stdout

    def test_passing_tally(self): self.assertEqual(self.run_case('3 passed, 0 failed')[0],0)
    def test_failed_tally(self): self.assertNotEqual(self.run_case('3 passed, 1 failed')[0],0)
    def test_provider_retry_fraction_is_not_a_failed_assertion_count(self):
        self.assertEqual(self.run_case(
            '[conversation] provider attempt 1/3 failed; retrying\n'
            '[conversation] provider attempt 9/12 failed; retrying\n'
            '169 passed, 0 failed')[0], 0)
    def test_provider_retry_cannot_hide_a_real_earlier_failure(self):
        self.assertNotEqual(self.run_case(
            '[conversation] provider attempt 1/3 failed; retrying\n'
            '10 passed, 2 failed\n169 passed, 0 failed')[0], 0)
    def test_failed_tally_with_deliberate_nonzero_exit_is_an_assertion_failure(self):
        code,text=self.run_case('3 passed, 1 failed',1)
        self.assertNotEqual(code,0); self.assertIn('FAIL ',text); self.assertNotIn('ERR ',text)
    def test_harness_abort_after_tally(self):
        code,text=self.run_case('3 passed, 0 failed\nHARNESS-ERR: later failure')
        self.assertNotEqual(code,0); self.assertIn('ERR ',text)
    def test_process_abort_after_tally(self): self.assertNotEqual(self.run_case('3 passed, 0 failed',1)[0],0)
    def test_unrecognized_output(self): self.assertNotEqual(self.run_case('Loaded only')[0],0)
    def test_zero_assertions(self): self.assertNotEqual(self.run_case('0 passed, 0 failed')[0],0)
    def test_assert_by_raising_success(self): self.assertEqual(self.run_case('PASS invariant')[0],0)
    def test_no_matches(self): self.assertNotEqual(self.run_case('',suite=False)[0],0)
    def test_failure_without_tally(self): self.assertNotEqual(self.run_case('PASS first\nFAIL second')[0],0)
    def test_later_tally_cannot_hide_failure(self): self.assertNotEqual(self.run_case('2 passed, 1 failed\n3 passed, 0 failed')[0],0)
    def test_unrelated_prefix_is_not_evidence(self): self.assertNotEqual(self.run_case('PASSENGER loaded')[0],0)
    def test_each_suite_gets_distinct_scratch(self):
        with tempfile.TemporaryDirectory() as temporary:
            root=Path(temporary); (root/'tests').mkdir()
            for name in ['first-tests.lisp','second-tests.lisp']:
                (root/'tests'/name).write_text('; synthetic launcher input\n')
            fake=root/'fake-sbcl'
            fake.write_text('#!/usr/bin/env python3\nimport os\nprint("PASS "+os.environ["PAI_TEST_STATE"])\n')
            fake.chmod(0o755)
            result=subprocess.run(['sh',str(ROOT/'tests/run-isolated.sh'),str(fake),str(root)],
                env={**os.environ,'PAI_TEST_STATE':str(root/'scratch'),
                     'PAI_TEST_LOG_DIR':str(root/'logs')},text=True,capture_output=True)
            self.assertEqual(result.returncode,0,result.stdout+result.stderr)
            first=(root/'logs/first-tests.lisp.log').read_text()
            second=(root/'logs/second-tests.lisp.log').read_text()
            self.assertIn('/first-tests.lisp',first)
            self.assertIn('/second-tests.lisp',second)
            self.assertNotEqual(first,second)

if __name__ == '__main__': unittest.main()
