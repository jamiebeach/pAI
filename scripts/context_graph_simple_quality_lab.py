"""Operator-approved bounded comparison; exact prompts originate in Lisp."""
import argparse, copy, json, sys, time, urllib.request
from pathlib import Path
ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(ROOT / 'scripts'))
import context_graph_resolution_lab as driver
import context_graph_authority_lab as authority

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('--execute', action='store_true', help='Explicitly allow paid ZDR execution')
    parser.add_argument('--staged', action='store_true', help='Separate entity extraction from typed fact extraction')
    parser.add_argument('--model-label', choices=('mimo', 'muse'), help='Restrict comparison to one of the predeclared ZDR routes')
    parser.add_argument('--prior', type=Path, required=True)
    parser.add_argument('--corpus', type=Path, required=True)
    parser.add_argument('--experiment', required=True)
    options = parser.parse_args()
    if not options.execute:
        parser.error('This comparison requires --execute; use the authority CLI for a no-call dry run')
    if not options.experiment or any(c not in 'abcdefghijklmnopqrstuvwxyz0123456789-' for c in options.experiment):
        parser.error('experiment must be a lowercase alphanumeric/hyphen identifier')
    prior_path = options.prior.resolve()
    prior = json.loads(prior_path.read_text())
    corpus = json.loads(options.corpus.read_text())
    authority.validate_corpus(corpus)
    selected = driver.base.load_selected_ontology(ROOT / 'config/context-graph-upper-ontology-v1.2.json')
    configs = [('mimo', 'xiaomi/mimo-v2.5', 'deepinfra/fp8', .20, .70),
               ('muse', 'meta/muse-glimmer-30b', 'phala', .30, 1.10)]
    if options.model_label:
        configs = [row for row in configs if row[0] == options.model_label]
    with urllib.request.urlopen('https://openrouter.ai/api/v1/endpoints/zdr', timeout=30) as response:
        metadata = json.loads(response.read())
    endpoints = []
    for label, model, tag, pp, cp in configs:
        matches = [r for r in metadata['data'] if r['model_id'] == model and r['tag'] == tag
                   and r['supports_tool_choice']['required'] and float(r['pricing']['prompt']) <= pp / 1e6
                   and float(r['pricing']['completion']) <= cp / 1e6]
        if not matches: raise ValueError('ZDR price-bounded route unavailable: ' + label)
        endpoints.extend(matches)
    run = ROOT / 'artifacts/context-graph-lab' / f'simple-quality-{time.time_ns()}'
    run.mkdir(exist_ok=False)
    paths = ['scripts/context_graph_resolution_lab.py', 'scripts/context_graph_authority_lab.py',
             'scripts/context_graph_lab.py', 'scripts/context-graph-lab.lisp',
             'scripts/context-graph-authority-session.lisp', 'scripts/conscious_q4_cli.py',
             'pai-context-graph.asd', 'pai-memory-access.asd', 'scripts/context_graph_simple_quality_lab.py']
    for folder in ('context-graph', 'memory-access'):
        paths += [p.relative_to(ROOT).as_posix() for p in (ROOT / 'src/mind/knowledge' / folder).glob('*.lisp')]
    hashes = {p: driver.base.sha256_file(ROOT / p) for p in paths}
    driver.save(run / 'seal.json', {'prior_ledger': str(prior_path), 'prior_rows': prior, 'cap_usd': 1.0,
        'configs': configs, 'zdr_endpoints': endpoints, 'corpus': corpus, 'source_sha256': hashes,
        'max_output_tokens': 8192, 'request_limit': 110, 'no_retries': True, 'staged': options.staged,
        'additional_metric': 'Query scan complete; every hit contains expected label at either endpoint and distinct matching endpoint IDs equal expected_count. Zero expectations require zero hits. Raw object/claim-count metric retained. Each organism must retain its own enduring entity ID across episodes.',
        'experiment': options.experiment})
    reports, calls = [], None
    def save():
        rows = calls.rows if calls else prior
        driver.save(run / 'report.json', {'models': reports, 'provider_calls_cumulative': len(rows),
            'reserved_usd': sum(r['bound_usd'] for r in rows), 'charged_usd': sum(r['charged_usd'] for r in rows), 'live_writes': 0})
    def unchanged():
        if any(driver.base.sha256_file(ROOT / p) != h for p, h in hashes.items()):
            raise ValueError('Sealed source changed')
    def lisp(name, bundle):
        unchanged()
        target = run / name; target.mkdir(); driver.save(target / 'bundle.json', bundle)
        return driver.run_lisp_bundle(ROOT, target)
    print(run, flush=True)
    for label, model, tag, pp, cp in configs:
        args = argparse.Namespace(model=model, max_output_tokens=8192, max_prompt_price=pp,
            max_completion_price=cp, cost_ceiling_usd=1.0, request_limit=110, provider_timeout_seconds=180,
            transient_retries=0, openrouter_zdr='require', openrouter_data_collection='deny',
            openrouter_provider_only=tag, reasoning_policy='low' if label == 'muse' else 'default')
        if calls is None: calls = driver.Calls(run, args, prior_rows=prior)
        else: calls.args = args
        report = {'model': model, 'route': tag, 'episodes': []}; reports.append(report); save()
        bundle = {'schema_version': 2, 'authority_operation': 'staged-model-session' if options.staged else 'simple-model-session',
            'source_kind': corpus['source_kind'], 'ontology': driver.base.runtime_ontology(selected),
            'ontology_revision': selected['ontology_revision'], 'history': [], 'step': None,
            'queries': [q['query'] for q in corpus['queries']]}
        try:
            for number, episode in enumerate(corpus['cases'], 1):
                prefix = f'{options.experiment}-{label}-episode-{number:02d}'
                step = {'episode': episode, 'proposal': None, 'review': None, 'request_digest': None}
                if options.staged:
                    step.update(entity_selection=None, entity_request_digest=None, fact_request_digest=None)
                bundle['step'] = step
                stages = ([('entities', 'entity_selection'), ('facts', 'proposal')] if options.staged else [('extraction', 'proposal')]) + [('review', 'review')]
                for stage, field in stages:
                    built = lisp(prefix + '-' + stage + '-input', bundle)
                    if built['status'] != 'accepted':
                        report['diagnostics'] = built['diagnostics']; raise ValueError(stage + ' preflight rejected')
                    request = authority.request_from_spec(built['value'], args)
                    unchanged(); response = calls.call(prefix + '-' + stage, request)
                    step[field] = authority.response_payload(response, built['value']['tool_name'])
                    if stage == 'entities': step['entity_request_digest'] = built['request_digest']
                    if stage == 'facts': step['fact_request_digest'] = built['request_digest']
                    if stage == 'review': step['request_digest'] = built['request_digest']
                result = lisp(prefix + '-application', bundle)
                report['episodes'].append({'case_id': episode['case_id'], 'status': result['status'],
                    'diagnostics': result['diagnostics'], 'value': result.get('value'), 'queries': result.get('queries', [])})
                save(); print(label, number, result['status'], flush=True)
                # A fully reviewed rejected episode is valid replay history. Keep
                # its outcome visible and test unchanged retrieval on negatives.
                if result['status'] not in {'accepted', 'rejected'}: raise ValueError('Application did not reach a terminal decision')
                bundle['history'].append(copy.deepcopy(step)); driver.save(run / (label + '-history.json'), bundle['history'])
            grouped = []
            for expected, actual in zip(corpus['queries'], result['queries']):
                hits = actual['rows']; target_label = expected['expected_object_label']
                ids = sorted({r[side]['entity_id'] for r in hits for side in ('subject', 'object') if r[side]['label'] == target_label})
                grouped.append({'query': expected['query'], 'entity_ids': ids, 'raw_claim_count': len(hits),
                    'passed': actual['scan_complete'] and len(ids) == expected['expected_count']
                    and all(any(r[side]['label'] == target_label for side in ('subject', 'object')) for r in hits)
                    and (expected['expected_count'] != 0 or not hits)})
            report.update(status='completed', raw_retrieval=authority.evaluate(corpus['queries'], result['queries']),
                          entity_retrieval=grouped)
            save()
        except Exception as error:
            report.update(status='stopped', error=str(error)); save(); print(label, 'stopped', str(error), flush=True)
            if calls.poisoned: break
    save()
if __name__ == '__main__': main()
