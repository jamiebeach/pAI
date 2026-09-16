#!/usr/bin/env python3
"""Inventory, audit and export a reviewed snapshot; never operates on a remote.

Identity terms belong in an ignored local UTF-8 file, one literal term per line.
Reports contain rule names and locations, never matched text or secret values.
An audit is mechanical evidence, not proof that prose contains no private facts.
"""
from __future__ import annotations

import argparse
import hashlib
import json
from pathlib import Path, PurePosixPath
import re
import subprocess
import tempfile

PRIVATE_PARTS = {'.git', '.clone-state', '.tools', '.cache', '.uv-cache', '.scratch',
                 '.claude', '.pai-review', '.pai-promotion-backups', '.pai-promotion-staging',
                 '.publication-output', '.q5-lifecycle-state', 'state', 'runtime-backups',
                 'private-diagnostics', 'secrets', 'event-log', '__pycache__'}
PRIVATE_SUFFIXES = {'.sqlite', '.sqlite3', '.db', '.jsonl', '.log', '.fasl', '.pyc',
                    '.key', '.pem', '.p12', '.pfx', '.zip', '.tar', '.gz', '.7z'}
SECRET_RULES = {
    'private-key': re.compile(r'-----BEGIN (?:[A-Z0-9]+ )*PRIVATE KEY-----'),
    'provider-token': re.compile(r'\bsk-(?:or-v1-|proj-)?[A-Za-z0-9_-]{24,}\b'),
    'github-token': re.compile(r'\b(?:gh[pousr]_[A-Za-z0-9]{30,}|github_pat_[A-Za-z0-9_]{40,})\b'),
    'slack-token': re.compile(r'\bxox[baprs]-[A-Za-z0-9-]{20,}\b'),
    'aws-access-key': re.compile(r'\b(?:AKIA|ASIA)[A-Z0-9]{16}\b'),
    'credential-url': re.compile(r'(?i)\b(?:https?|postgres(?:ql)?)://[^\s/:@]+:[^\s/@]+@'),
    'personal-host-path': re.compile(r'(?i)(?:[A-Z]:[\\/](?:Users|Documents and Settings)[\\/][^\s/\\]+|/' + r'Users/[^\s/]+)'),
}
CAPTURED_FIXTURE_PROVENANCE = re.compile(
    r'(?i)"(?:fixture_policy|source)"\s*:\s*"[^"]*captured')
NONPORTABLE_APPROVAL = re.compile(
    r'(?i)\boperator-approved-(?:clone-)?qualification-[0-9]{4}-[0-9]{2}-[0-9]{2}\b')
# Mask standard accessibility attributes, not arbitrary uses of their prefix.
ACCESSIBILITY_ATTRIBUTES = re.compile(r'\b' + 'ar' + 'ia-' + r'(?:label|labelledby|describedby|live|atomic|busy|hidden|expanded|'
                  r'controls|current|modal|pressed|selected|checked|disabled|haspopup|'
                  r'valuenow|valuemin|valuemax|valuetext|orientation|relevant|roledescription)\b')

def digest(data: bytes) -> str:
    return hashlib.sha256(data).hexdigest()

def relative(value: str) -> PurePosixPath:
    if not isinstance(value, str) or not value or '\\' in value or ':' in value:
        raise ValueError('path must be a nonempty relative POSIX path')
    path = PurePosixPath(value)
    if path.is_absolute() or any(part in {'', '.', '..'} for part in value.split('/')):
        raise ValueError('absolute, empty and traversal path components are forbidden')
    return path

def private_path(path: PurePosixPath) -> bool:
    parts = [part.casefold() for part in path.parts]
    name = parts[-1]
    return (any(part in PRIVATE_PARTS for part in parts)
            or ('artifacts' in parts)
            or name == '.env' or (name.startswith('.env.') and name != '.env.example')
            or path.suffix.casefold() in PRIVATE_SUFFIXES
            or '.sqlite3-' in name
            or 'heldout' in name or 'deployment-evidence' in name
            or 'source-qualification' in name)

def source_file(root: Path, name: str) -> Path:
    path = relative(name)
    current = root
    for part in path.parts:
        current = current / part
        if current.is_symlink() or getattr(current, 'is_junction', lambda: False)():
            raise ValueError('symlinks are forbidden')
    if not current.is_file() or not current.resolve().is_relative_to(root.resolve()):
        raise ValueError('source must be a regular file inside the source root')
    return current

def inventory(root: Path) -> dict:
    result = subprocess.run(['git', '-C', str(root), 'ls-files', '-z', '--cached',
                             '--others', '--exclude-standard'], check=True, capture_output=True)
    names = sorted(set(result.stdout.decode('utf-8').split('\0')) - {''})
    files = []
    for name in names:
        path = source_file(root, name)
        data = path.read_bytes()
        files.append({'source':name, 'target':name, 'sha256':digest(data), 'bytes':len(data)})
    return {'schema_version':1, 'purpose':'draft inventory; curate before exporting', 'files':files}

def identity_terms(path: Path) -> list[str]:
    terms = [line.strip() for line in path.read_text(encoding='utf-8-sig').splitlines()
             if line.strip() and not line.lstrip().startswith('#')]
    if not terms or any(len(term) < 2 for term in terms):
        raise ValueError('identity deny file must contain literal terms of at least two characters')
    return terms

def audit(root: Path, manifest: dict, terms: list[str]) -> dict:
    if manifest.get('schema_version') != 1 or not isinstance(manifest.get('files'), list) or not manifest['files']:
        raise ValueError('manifest must have schema_version 1 and a nonempty files list')
    if not terms:
        raise ValueError('identity scan cannot be omitted')
    findings, destinations = [], set()
    identities = [re.compile(r'(?<!\w)' + re.escape(term) + r'(?!\w)', re.I) for term in terms]
    def flag(index, rule, line=None):
        findings.append({'entry':index, 'rule':rule, **({'line':line} if line else {})})
    for index, entry in enumerate(manifest['files']):
        try:
            source, target = relative(entry['source']), relative(entry['target'])
            target_key = str(target).casefold()
            if target_key in destinations:
                flag(index, 'duplicate-target')
            destinations.add(target_key)
            if private_path(source) or private_path(target):
                flag(index, 'private-path')
            # Parent/file collisions must not make the export order significant.
            if any(str(parent).casefold() in destinations for parent in target.parents if str(parent) != '.'):
                flag(index, 'target-parent-collision')
            path = source_file(root, str(source))
            data = path.read_bytes()
            if digest(data) != entry.get('sha256'):
                flag(index, 'source-changed')
            if len(data) > 10_000_000:
                flag(index, 'oversized-file')
                continue
            try:
                text = data.decode('utf-8-sig')
                if '\0' in text:
                    raise UnicodeError()
            except UnicodeError:
                flag(index, 'binary-or-non-utf8')
                continue
            for name in (str(source), str(target)):
                if any(pattern.search(ACCESSIBILITY_ATTRIBUTES.sub('', name)) for pattern in identities):
                    flag(index, 'identity-in-path')
            for line_number, line in enumerate(text.splitlines(), 1):
                if any(pattern.search(ACCESSIBILITY_ATTRIBUTES.sub('', line)) for pattern in identities):
                    flag(index, 'private-identity', line_number)
                for rule, pattern in SECRET_RULES.items():
                    if pattern.search(line):
                        flag(index, rule, line_number)
                if (str(target).startswith('evals/fixtures/')
                        and CAPTURED_FIXTURE_PROVENANCE.search(line)):
                    flag(index, 'captured-fixture-provenance', line_number)
                if NONPORTABLE_APPROVAL.search(line):
                    flag(index, 'nonportable-approval-scope', line_number)
        except (KeyError, TypeError, ValueError, OSError):
            flag(index, 'invalid-entry-or-source')
    # Check the reverse order of parent/file collisions too.
    for index, entry in enumerate(manifest['files']):
        try:
            target = str(relative(entry['target'])).casefold()
            if any(other.startswith(target + '/') for other in destinations):
                flag(index, 'target-parent-collision')
        except (KeyError, TypeError, ValueError):
            pass
    return {'schema_version':1, 'passed':not findings, 'files_checked':len(manifest['files']),
            'identity_scan':'performed', 'manifest_sha256':digest(json.dumps(manifest,sort_keys=True).encode()),
            'findings':findings,
            'limits':['No Git history is exported.', 'Pattern scans do not prove absence of private facts or all secret formats.',
                      'Manual content review, provenance and runtime qualification remain required.']}

def export(root: Path, manifest: dict, terms: list[str], destination: Path) -> dict:
    report = audit(root, manifest, terms)
    if not report['passed']:
        raise ValueError('publication audit failed; no export created')
    if destination.exists() or destination.is_symlink():
        raise ValueError('destination must not exist; exports never overwrite')
    destination.parent.mkdir(parents=True, exist_ok=True)
    with tempfile.TemporaryDirectory(prefix='.publication-', dir=destination.parent) as temporary:
        staging = Path(temporary) / 'tree'
        staging.mkdir()
        for entry in manifest['files']:
            # Verify the same bytes we copy, after the audit, to detect source drift.
            original = source_file(root, entry['source'])
            data = original.read_bytes()
            if digest(data) != entry['sha256']:
                raise ValueError('source changed during export')
            target = staging / entry['target']
            target.parent.mkdir(parents=True, exist_ok=True)
            target.write_bytes(data)
            target.chmod(0o755 if target.suffix == '.sh' else 0o644)
        staging.rename(destination)
    return report

def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('command', choices=['inventory','check','export'])
    parser.add_argument('--root', type=Path, default=Path.cwd())
    parser.add_argument('--manifest', type=Path)
    parser.add_argument('--deny-file', type=Path)
    parser.add_argument('--output', type=Path, required=True, help='JSON report, or new inventory manifest')
    parser.add_argument('--destination', type=Path)
    args = parser.parse_args()
    try:
        if args.command == 'inventory':
            result = inventory(args.root)
        else:
            if not args.manifest or not args.deny_file:
                raise ValueError('--manifest and --deny-file are required')
            manifest = json.loads(args.manifest.read_text(encoding='utf-8-sig'))
            terms = identity_terms(args.deny_file)
            if args.command == 'export':
                if not args.destination:
                    raise ValueError('--destination is required')
                result = export(args.root, manifest, terms, args.destination)
            else:
                result = audit(args.root, manifest, terms)
        args.output.parent.mkdir(parents=True, exist_ok=True)
        args.output.write_text(json.dumps(result, indent=2)+'\n', encoding='utf-8')
        print(json.dumps({'command':args.command,'files':result.get('files_checked',len(result.get('files',[]))),
                          'passed':result.get('passed'), 'findings':len(result.get('findings',[]))}))
        return 0 if result.get('passed',True) else 1
    except (ValueError, OSError, subprocess.CalledProcessError) as error:
        # Exception strings can contain paths/content; keep terminal output redacted.
        print(json.dumps({'passed':False,'error_type':type(error).__name__,'error':'Publication operation failed; check manifest, paths and required inputs.'}))
        return 2

if __name__ == '__main__':
    raise SystemExit(main())
