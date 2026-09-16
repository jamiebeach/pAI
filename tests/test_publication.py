"""Publication boundary checks use synthetic data, never private fixtures."""
import importlib.util
import json
from pathlib import Path
import tempfile
import unittest
from unittest.mock import patch
import subprocess

SPEC = importlib.util.spec_from_file_location('publication', Path(__file__).parents[1]/'scripts/publication.py')
publication = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(publication)

class PublicationTests(unittest.TestCase):
    def setUp(self):
        self.temporary = tempfile.TemporaryDirectory()
        self.addCleanup(self.temporary.cleanup)
        self.root = Path(self.temporary.name)/'source'
        self.root.mkdir()
        self.terms = ['fixture-person']

    def manifest(self, text='Public source', source='src/file.txt', target=None):
        path = self.root/source
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(text, encoding='utf-8')
        return {'schema_version':1,'files':[{'source':source,'target':target or source,
                                           'sha256':publication.digest(path.read_bytes())}]}

    def rules(self, manifest):
        return {row['rule'] for row in publication.audit(self.root,manifest,self.terms)['findings']}

    def test_clean_export_preserves_bytes_and_omits_unlisted_material(self):
        manifest=self.manifest('Public λ\n')
        (self.root/'private.txt').write_text('private fixture')
        destination=self.root.parent/'export'
        self.assertTrue(publication.export(self.root,manifest,self.terms,destination)['passed'])
        self.assertEqual((destination/'src/file.txt').read_bytes(),(self.root/'src/file.txt').read_bytes())
        self.assertFalse((destination/'private.txt').exists())
        self.assertFalse((destination/'.git').exists())

    def test_identity_blocks_export_without_disclosing_match(self):
        manifest=self.manifest('Private: FIXTURE-PERSON')
        report=publication.audit(self.root,manifest,self.terms)
        self.assertFalse(report['passed'])
        self.assertNotIn('FIXTURE-PERSON',json.dumps(report))
        destination=self.root.parent/'export'
        with self.assertRaises(ValueError): publication.export(self.root,manifest,self.terms,destination)
        self.assertFalse(destination.exists())

    def test_accessibility_is_not_a_private_identity(self):
        manifest=self.manifest('<button aria-label="Close" aria-expanded="true">Close</button>')
        self.assertTrue(publication.audit(self.root,manifest,['ar'+'ia'])['passed'])
        self.assertIn('private-identity',self.rules(self.manifest('fixture-person-label')))

    def test_known_secret_shapes_and_urls_are_rejected(self):
        for text in ['ghp_'+'a'*36, 'sk-or-v1-'+'b'*40, 'https'+ '://user:password@example.invalid']:
            self.assertTrue(self.rules(self.manifest(text)))

    def test_captured_eval_fixture_provenance_is_rejected(self):
        for text in [
                '{"source":"captured-conversation"}',
                '{"fixture_policy":"captured-output where available"}']:
            manifest = self.manifest(text, source='evals/fixtures/anchor.json')
            self.assertIn('captured-fixture-provenance', self.rules(manifest))
        manifest = self.manifest(
            '{"source":"deterministic","note":"no captured data is included"}',
            source='evals/fixtures/synthetic.json')
        self.assertNotIn('captured-fixture-provenance', self.rules(manifest))

    def test_nonportable_operator_approval_is_rejected(self):
        manifest = self.manifest(
            ':approval-scope "operator-' +
            'approved-clone-qualification-2030-01-02"')
        self.assertIn('nonportable-approval-scope', self.rules(manifest))
        manifest = self.manifest(
            ':approval-scope "synthetic-clone-qualification-fixture-v1"')
        self.assertNotIn('nonportable-approval-scope', self.rules(manifest))

    def test_runtime_and_history_paths_are_rejected(self):
        for source in ['state/value.json','.git/config','.pai-promotion-backups/file.txt','secrets/file.txt','events.sqlite3','artifacts/report.txt']:
            self.assertIn('private-path',self.rules(self.manifest(source=source)))

    def test_source_changes_block_publication(self):
        manifest=self.manifest()
        (self.root/'src/file.txt').write_text('Changed after review')
        self.assertIn('source-changed',self.rules(manifest))

    def test_path_escape_and_windows_paths_are_rejected(self):
        for target in ['../escape','/absolute','C:/escape','a\\b','a//b','a/./b']:
            self.assertIn('invalid-entry-or-source',self.rules(self.manifest(target=target)))

    def test_duplicate_and_parent_targets_are_rejected(self):
        first=self.manifest(target='Public.txt')
        first['files'] += self.manifest(source='src/second.txt',target='public.TXT')['files']
        self.assertIn('duplicate-target',self.rules(first))
        for targets in [('folder','folder/file'),('folder/file','folder')]:
            manifest=self.manifest(target=targets[0])
            manifest['files'] += self.manifest(source='second.txt',target=targets[1])['files']
            self.assertIn('target-parent-collision',self.rules(manifest))

    def test_binary_files_require_explicit_review_instead_of_silent_skip(self):
        manifest=self.manifest('text\0binary')
        self.assertIn('binary-or-non-utf8',self.rules(manifest))

    def test_missing_identity_policy_is_not_a_pass(self):
        with self.assertRaises(ValueError): publication.audit(self.root,self.manifest(),[])

    def test_existing_destination_is_preserved(self):
        destination=self.root.parent/'existing'
        destination.mkdir()
        (destination/'keep.txt').write_text('Keep')
        with self.assertRaises(ValueError): publication.export(self.root,self.manifest(),self.terms,destination)
        self.assertEqual((destination/'keep.txt').read_text(),'Keep')

    def test_inventory_includes_uncommitted_source_but_not_ignored_state(self):
        subprocess.run(['git','init','--quiet',str(self.root)],check=True,capture_output=True)
        self.manifest()
        (self.root/'.gitignore').write_text('/state/\n')
        self.manifest(source='state/private.txt')
        names={entry['source'] for entry in publication.inventory(self.root)['files']}
        self.assertIn('src/file.txt',names)
        self.assertNotIn('state/private.txt',names)
        subprocess.run(['git','-C',str(self.root),'add','--force','state/private.txt'],check=True,capture_output=True)
        self.assertIn('private-path',self.rules(publication.inventory(self.root)))

    def test_drift_between_audit_and_copy_leaves_no_partial_export(self):
        manifest=self.manifest()
        report=publication.audit(self.root,manifest,self.terms)
        def changed(*args):
            (self.root/'src/file.txt').write_text('Changed concurrently')
            return report
        destination=self.root.parent/'export'
        with patch.object(publication,'audit',side_effect=changed):
            with self.assertRaises(ValueError): publication.export(self.root,manifest,self.terms,destination)
        self.assertFalse(destination.exists())
        self.assertEqual(list(self.root.parent.glob('.publication-*')),[])

    def test_symlink_escape_is_rejected(self):
        outside=self.root.parent/'outside.txt'
        outside.write_text('Outside')
        manifest=self.manifest()
        (self.root/'src/file.txt').unlink()
        try: (self.root/'src/file.txt').symlink_to(outside)
        except OSError: self.skipTest('Host does not allow symlink creation')
        self.assertIn('invalid-entry-or-source',self.rules(manifest))

if __name__ == '__main__': unittest.main()
