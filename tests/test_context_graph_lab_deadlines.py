"""Real silent subprocess tests; no Lisp, provider, or database required."""
from pathlib import Path
import subprocess
import sys
import tempfile
import threading
import time
import unittest

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "scripts"))
from context_graph_episode_lab import LispWorker


class Deadlines(unittest.TestCase):
    def worker(self, code):
        config = tempfile.NamedTemporaryFile(delete=False)
        config.close()
        process = subprocess.Popen([sys.executable, "-u", "-c", code],
                                   stdin=subprocess.PIPE, stdout=subprocess.PIPE,
                                   stderr=subprocess.STDOUT, text=True)
        worker = LispWorker(process, threading.Lock(), [], Path(config.name))
        self.addCleanup(worker.close)
        return worker

    def test_silent_startup_is_bounded(self):
        worker = self.worker("import time; time.sleep(60)")
        started = time.monotonic()
        with self.assertRaises(TimeoutError):
            worker._wait_ready(.1)
        self.assertLess(time.monotonic() - started, 3)
        self.assertIsNotNone(worker.process.poll())

    def test_silent_request_is_bounded(self):
        worker = self.worker("import time; print('KG-EPISODE-LAB-READY'); input(); time.sleep(60)")
        worker._wait_ready(3)
        with self.assertRaises(TimeoutError):
            worker.call({"operation": "fixture"}, timeout_seconds=.1)
        self.assertIsNotNone(worker.process.poll())

    def test_result_and_bounded_log(self):
        worker = self.worker("print('KG-EPISODE-LAB-READY'); input(); "
                             "[print('progress') for _ in range(300)]; "
                             "print('KG-EPISODE-LAB-RESULT {\"ok\":true}')")
        worker._wait_ready(3)
        self.assertEqual(worker.call({}, 3), {"ok": True})
        self.assertLessEqual(len(worker.log), 100)

    def test_exit_reports_failure(self):
        worker = self.worker("pass")
        with self.assertRaises(RuntimeError):
            worker._wait_ready(3)

    def test_blocked_input_is_bounded(self):
        worker = self.worker("import time; print('KG-EPISODE-LAB-READY'); time.sleep(60)")
        worker._wait_ready(3)
        with self.assertRaises(TimeoutError):
            worker.call({"payload": "x" * 1000000}, .1)
        self.assertIsNotNone(worker.process.poll())

    def test_busy_worker_does_not_kill_active_request(self):
        worker = self.worker("import time; time.sleep(60)")
        worker.lock.acquire()
        try:
            with self.assertRaisesRegex(TimeoutError, "not submitted"):
                worker.call({}, .1)
            self.assertIsNone(worker.process.poll())
        finally:
            worker.lock.release()

    def test_checkpoint_stream_is_delivered_without_logging_private_payload(self):
        worker = self.worker("print('KG-EPISODE-LAB-READY'); input(); "
                             "print('KG-EPISODE-LAB-CHECKPOINT {\"fixture\":true}'); "
                             "print('KG-EPISODE-LAB-RESULT {\"ok\":true}')")
        worker._wait_ready(3)
        saved = []
        self.assertEqual(worker.call({}, 3, on_checkpoint=saved.append), {"ok": True})
        self.assertEqual(saved, [{"fixture": True}])
        self.assertFalse(any("fixture" in line for line in worker.log))
