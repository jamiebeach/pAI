import json
import os
from pathlib import Path
import sys
import tempfile
import unittest
from unittest import mock


ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(ROOT / "scripts"))

import pai_instance  # noqa: E402


class InstanceConfigTests(unittest.TestCase):
    def test_live_cli_cannot_inherit_small_instance_rebuild_authority(self):
        with tempfile.TemporaryDirectory() as temporary:
            for enabled in (False, True):
                args = pai_instance.pai_cli.parse_args(
                    ["--provider", "local", "--state-dir", temporary]
                )
                if enabled:
                    args.small_instance_rebuild = True
                with mock.patch.dict(os.environ, {"PAI_SMALL_INSTANCE_REBUILD": "1"}), \
                     mock.patch.object(pai_instance.pai_cli, "local_sbcl", return_value=Path("sbcl")), \
                     mock.patch.object(pai_instance.pai_cli, "native_environment", return_value={}), \
                     mock.patch.object(pai_instance.pai_cli, "quicklisp_setup", return_value=Path("setup.lisp")), \
                     mock.patch.object(pai_instance.pai_cli.subprocess, "run", return_value=mock.Mock(returncode=0)) as run:
                    self.assertEqual(0, pai_instance.pai_cli.run(args))
                    self.assertEqual(
                        "1" if enabled else "0",
                        run.call_args.kwargs["env"]["PAI_SMALL_INSTANCE_REBUILD"],
                    )

    def configured_document(self):
        document = json.loads(
            (ROOT / "config" / "instance.example.json").read_text(encoding="utf-8")
        )
        document["agent"]["id"] = "agent:synthetic-first-run"
        document["provider"]["model"] = "fixture-local-model"
        document["memory"]["embedding_revision"] = "fixture-embedding-v1"
        document["memory"]["retrieval_embedding_revision"] = "fixture-retrieval-v1"
        return document

    def write_config(self, root, document):
        path = root / "instance.json"
        path.write_text(json.dumps(document), encoding="utf-8")
        return path

    def test_empty_and_existing_authority_select_distinct_paths(self):
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            config = self.write_config(root, self.configured_document())
            first = pai_instance.load_instance_config(config, root)
            self.assertTrue(first.initialize_events)
            self.assertTrue(first.small_instance_rebuild)
            first.state_dir.mkdir()
            (first.state_dir / "events.sqlite3").write_bytes(b"fixture")
            second = pai_instance.load_instance_config(config, root)
            self.assertFalse(second.initialize_events)
            self.assertTrue(second.small_instance_rebuild)
            self.assertEqual("agent:synthetic-first-run", second.agent_id)
            self.assertEqual(768, second.memory_vector_dimension)
            self.assertEqual("local-providerless-v1", second.local_provider_profile)

    def test_derived_state_without_event_authority_is_rejected(self):
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            config = self.write_config(root, self.configured_document())
            state = root / "state"
            state.mkdir()
            (state / "derived.sqlite3").write_bytes(b"fixture")
            with self.assertRaisesRegex(SystemExit, "without the authoritative"):
                pai_instance.load_instance_config(config, root)

    def test_placeholders_and_credentials_are_rejected(self):
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            document = self.configured_document()
            document["provider"]["model"] = "replace-with-local-model"
            config = self.write_config(root, document)
            with self.assertRaisesRegex(SystemExit, "placeholder"):
                pai_instance.load_instance_config(config, root)

            document = self.configured_document()
            document["web"]["password"] = "not-allowed"
            config = self.write_config(root, document)
            with self.assertRaisesRegex(SystemExit, "credentials are forbidden"):
                pai_instance.load_instance_config(config, root)

    def test_disabled_web_keeps_listener_arguments_absent(self):
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            config = self.write_config(root, self.configured_document())
            args = pai_instance.load_instance_config(config, root)
            self.assertFalse(args.web)
            self.assertIsNone(args.web_address)
            self.assertIsNone(args.web_port)

    def test_local_instance_rejects_a_paid_provider_profile(self):
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            document = self.configured_document()
            document["runtime"]["provider_profile"] = "paid-provider-profile"
            config = self.write_config(root, document)
            with self.assertRaisesRegex(SystemExit, "local provider_profile"):
                pai_instance.load_instance_config(config, root)


if __name__ == "__main__":
    unittest.main()
