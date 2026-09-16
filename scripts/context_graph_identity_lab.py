"""Disposable identity-formation lab. No live state; dry-run by default."""
import argparse
import copy
import json
import math
from pathlib import Path
import time
import urllib.request

import context_graph_authority_lab as authority
import context_graph_resolution_lab as driver

ROOT = Path(__file__).resolve().parents[1]


def request_limits(episode_count, grouped, prior_count):
    "Bound this trial independently; cumulative paid rows still bind spend."
    if type(episode_count) is not int or not 1 <= episode_count <= 8:
        raise ValueError('trial requires one to eight episodes')
    if type(prior_count) is not int or prior_count < 0:
        raise ValueError('invalid prior call count')
    new_limit = episode_count * (14 if grouped else 13)
    return new_limit, prior_count + new_limit


def request_from_spec(spec, args):
    """Explicitly translate the new review revision without changing its ask."""
    value = copy.deepcopy(spec)
    if value.get('adapter_revision') in {'identity-formation-review-v1', 'identity-formation-review-v2'}:
        value['adapter_revision'] = 'kg-authority-lab-model-v1'
    return authority.request_from_spec(value, args)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--corpus', type=Path, required=True)
    parser.add_argument('--output', type=Path, required=True)
    parser.add_argument('--execute', action='store_true')
    parser.add_argument('--cap-usd', type=float)
    parser.add_argument('--typed-review', action='store_true')
    parser.add_argument('--grouped-identities', action='store_true', help='Opt into v3 source-mention grouping with typed review')
    parser.add_argument('--guided-descriptors', action='store_true', help='Opt into v4 grouping with selected ontology kind meanings')
    parser.add_argument('--atomic-mentions', action='store_true', help='Opt into v5 single-referent mention guidance')
    parser.add_argument('--canonical-labels', action='store_true', help='Opt into v6 canonical-label proposal and independent review')
    parser.add_argument('--prior-ledger', type=Path, help='Include settled prior experiments in the total cap')
    options = parser.parse_args()
    if options.canonical_labels:
        options.atomic_mentions = True
    if options.atomic_mentions:
        options.guided_descriptors = True
    if options.guided_descriptors:
        options.grouped_identities = True
    if options.execute and (options.cap_usd is None or not math.isfinite(options.cap_usd) or options.cap_usd <= 0):
        parser.error('--execute requires a positive finite operator-approved --cap-usd')
    if options.corpus.stat().st_size > 200000:
        parser.error('corpus exceeds byte bound')
    corpus = json.loads(options.corpus.read_text(encoding='utf-8'))
    authority.validate_corpus(corpus)
    prior = json.loads(options.prior_ledger.read_text(encoding='utf-8')) if options.prior_ledger else []
    options.output.mkdir(parents=True, exist_ok=False)
    selected = driver.base.load_selected_ontology(ROOT / 'config/context-graph-upper-ontology-v1.2.json')
    paths = list((ROOT / 'scripts').glob('*.py')) + list((ROOT / 'scripts').glob('*.lisp'))
    paths += list((ROOT / 'src').rglob('*.lisp')) + list(ROOT.glob('*.asd'))
    paths += [ROOT / 'config/context-graph-upper-ontology-v1.2.json']
    hashes = {p.relative_to(ROOT).as_posix(): driver.base.sha256_file(p) for p in paths}
    new_limit, cumulative_limit = request_limits(len(corpus['cases']), options.grouped_identities, len(prior))
    args = argparse.Namespace(model='meta/muse-glimmer-30b', max_output_tokens=8192,
        max_prompt_price=.30, max_completion_price=1.10, cost_ceiling_usd=options.cap_usd,
        request_limit=cumulative_limit, provider_timeout_seconds=180, transient_retries=0,
        openrouter_zdr='require', openrouter_data_collection='deny',
        openrouter_provider_only='phala', reasoning_policy='low')
    protocol = ('identity-formation-v6-session' if options.canonical_labels else
                'identity-formation-v5-session' if options.atomic_mentions else
                'identity-formation-v4-session' if options.guided_descriptors else
                'identity-formation-v3-session' if options.grouped_identities else
                'identity-formation-v2-session' if options.typed_review else 'identity-formation-session')
    run_id = str(time.time_ns())
    seal = {'protocol': protocol, 'corpus': corpus, 'source_sha256': hashes,
            'prior_rows': prior, 'run_id': run_id,
            'maximum_new_calls': new_limit, 'maximum_cumulative_calls': cumulative_limit,
            'cap_usd': options.cap_usd, 'model': args.model, 'provider': 'phala',
            'zdr': True, 'execute': options.execute, 'created_at': time.time(), 'no_retries': True}
    driver.save(options.output / 'seal.json', seal)
    bundle = {'schema_version': 2, 'authority_operation': protocol,
              'source_kind': corpus['source_kind'], 'ontology': driver.base.runtime_ontology(selected),
              'ontology_revision': selected['ontology_revision'], 'history': [], 'step': None,
              'queries': [q['query'] for q in corpus['queries']]}
    if options.guided_descriptors:
        bundle['descriptor_guide'] = [{k: row[k] for k in ('name', 'definition', 'inclusion_rule', 'exclusion_rule')}
                                      for row in selected['ontology']['entity_types']]
    calls = driver.Calls(options.output, args, prior_rows=prior)
    # Validate prior settled evidence before any new provider request.
    calls.budget_used_usd
    route_checked = False
    report = {'status': 'preparing', 'episodes': [], 'provider_calls': 0, 'live_writes': 0}

    def unchanged():
        if any(driver.base.sha256_file(ROOT / p) != h for p, h in hashes.items()):
            raise ValueError('sealed source changed')

    def save_report():
        report.update(provider_calls=len(calls.rows) if calls else 0,
                      budget_used_usd=calls.budget_used_usd if calls else 0,
                      charged_usd=sum(r.get('charged_usd', 0) for r in calls.rows) if calls else 0)
        driver.save(options.output / 'report.json', report)

    try:
        for number, episode in enumerate(corpus['cases'], 1):
            step = {'episode': episode, 'calls': []}
            bundle['step'] = step
            for index in range(15 if options.grouped_identities else 14):
                unchanged()
                target = options.output / f'episode-{number:02d}-step-{index:02d}'
                target.mkdir()
                driver.save(target / 'bundle.json', bundle)
                result = driver.run_lisp_bundle(ROOT, target)
                if result.get('terminal') is True:
                    report['episodes'].append({'case_id': episode['case_id'], 'result': result})
                    bundle['history'].append(copy.deepcopy(step))
                    driver.save(options.output / 'history.json', bundle['history'])
                    save_report()
                    break
                if result.get('status') != 'accepted' or 'phase' not in result:
                    report['refusal'] = result
                    raise ValueError('production pipeline refused next request')
                request = request_from_spec(result['value'], args)
                driver.save(target / 'request.json', request)
                if not options.execute:
                    report['status'] = 'sealed-no-calls'
                    save_report()
                    return 0
                if not route_checked:
                    with urllib.request.urlopen('https://openrouter.ai/api/v1/endpoints/zdr', timeout=30) as response:
                        metadata = json.load(response)
                    matches = [r for r in metadata['data'] if r['model_id'] == args.model and r['tag'] == 'phala'
                               and r['supports_tool_choice']['required']
                               and float(r['pricing']['prompt']) <= args.max_prompt_price / 1e6
                               and float(r['pricing']['completion']) <= args.max_completion_price / 1e6]
                    if not matches:
                        raise ValueError('required price-bounded ZDR Muse route unavailable')
                    driver.save(options.output / 'zdr-endpoints.json', matches)
                    route_checked = True
                unchanged()
                response = calls.call(f'{run_id}-episode-{number:02d}-{result["phase"]}', request)
                step['calls'].append({'phase': result['phase'], 'request_digest': result['request_digest'],
                                      'response': authority.response_payload(response, result['value']['tool_name'])})
                driver.save(options.output / 'pending-step.json', step)
                save_report()
            else:
                raise ValueError('formation call-count bound exceeded')
        report['status'] = 'completed-requires-quality-review'
        report['held_out_expectations'] = corpus['queries']
        # Keep production retrieval, not raw graph search, for inspection. A
        # completed episode is not a passing retrieval-quality result.
        save_report()
        return 0
    except Exception as error:
        report.update(status='stopped', error=str(error))
        save_report()
        return 1


if __name__ == '__main__':
    raise SystemExit(main())
