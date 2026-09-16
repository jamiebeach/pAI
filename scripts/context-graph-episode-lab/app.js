(function () {
  'use strict';
  const ns = 'http://www.w3.org/2000/svg';
  const state = { config: null, catalog: null, selected: null, status: 'all', cutoff: null, graph: null, selectedGraphId: null };
  const $ = id => document.getElementById(id);

  function svgElement(tag, attributes) {
    const element = document.createElementNS(ns, tag);
    for (const [key, value] of Object.entries(attributes || {})) element.setAttribute(key, value);
    return element;
  }

  async function api(path, body) {
    const response = await fetch(path, body ? { method: 'POST', headers: {
      'Content-Type': 'application/json', 'X-PAI-Episode-Lab': state.config.csrf_token
    }, body: JSON.stringify(body) } : undefined);
    const value = await response.json();
    if (!response.ok || value.error) throw new Error(value.error || `HTTP ${response.status}`);
    return value;
  }

  function short(value, length) {
    value = String(value || '');
    return value.length > length ? value.slice(0, length - 1) + '…' : value;
  }

  function showDetails(value) {
    $('details').replaceChildren();
    const pre = document.createElement('pre');
    pre.textContent = JSON.stringify(value, null, 2);
    $('details').appendChild(pre);
    state.selectedGraphId = value.node_id || value.edge_id || null;
    document.querySelectorAll('.node').forEach(node => node.classList.toggle('selected', node.dataset.id === state.selectedGraphId));
  }

  function episodeMatches(episode, phrase) {
    if (state.status !== 'all' && episode.status !== state.status) return false;
    if (!phrase) return true;
    return JSON.stringify([episode.episode_id, episode.episode_event_id, episode.synopsis,
      episode.entities, episode.subjects]).toLowerCase().includes(phrase);
  }

  function renderEpisodeList() {
    const phrase = $('episode-filter').value.trim().toLowerCase();
    const rows = state.catalog.episodes.filter(row => episodeMatches(row, phrase));
    $('episode-list').replaceChildren();
    $('episode-count').textContent = `${rows.length} / ${state.catalog.episode_count}`;
    for (const episode of rows) {
      const button = document.createElement('button');
      button.type = 'button'; button.className = 'episode' + (state.selected === episode ? ' selected' : '');
      const top = document.createElement('div'); top.className = 'episode-top';
      const id = document.createElement('span'); id.className = 'episode-id'; id.textContent = `event ${episode.episode_event_id} · ${episode.episode_id || 'unnamed'}`;
      const status = document.createElement('span'); status.className = `episode-status ${episode.status}`; status.textContent = episode.status;
      top.append(id, status);
      const synopsis = document.createElement('div'); synopsis.className = 'episode-synopsis'; synopsis.textContent = episode.synopsis || 'No generated synopsis.';
      const meta = document.createElement('div'); meta.className = 'episode-meta';
      meta.textContent = `${episode.attempts.length} attempt${episode.attempts.length === 1 ? '' : 's'} · ${episode.query_suggestions.length} query cue${episode.query_suggestions.length === 1 ? '' : 's'}`;
      button.append(top, synopsis, meta);
      button.addEventListener('click', () => selectEpisode(episode));
      $('episode-list').appendChild(button);
    }
  }

  function renderStatusFilters() {
    const host = $('status-filters'); host.replaceChildren();
    const statuses = ['all', 'completed', 'failed', 'opened', 'queued'];
    for (const status of statuses) {
      const button = document.createElement('button'); button.type = 'button';
      const count = status === 'all' ? state.catalog.episode_count : (state.catalog.status_counts[status] || 0);
      button.textContent = `${status} ${count}`; button.className = state.status === status ? 'active' : '';
      button.addEventListener('click', () => { state.status = status; renderStatusFilters(); renderEpisodeList(); });
      host.appendChild(button);
    }
  }

  function renderSuggestions(episode) {
    $('suggestions').replaceChildren();
    for (const suggestion of (episode ? episode.query_suggestions : []).slice(0, 10)) {
      const button = document.createElement('button'); button.type = 'button'; button.textContent = suggestion;
      button.addEventListener('click', () => { $('query').value = suggestion; runQuery(); });
      $('suggestions').appendChild(button);
    }
  }

  function latestBatchAttempts(attempts) {
    const latest = new Map();
    for (const attempt of (attempts || [])) {
      const prior = latest.get(attempt.batch_index);
      if (!prior || Number(attempt.terminal_event_id || attempt.opened_event_id || 0) > Number(prior.terminal_event_id || prior.opened_event_id || 0)) {
        latest.set(attempt.batch_index, attempt);
      }
    }
    return Array.from(latest.values()).sort((left, right) => left.batch_index - right.batch_index);
  }

  function appendAuditText(parent, className, label, values) {
    if (!values.length) return;
    const row = document.createElement('p'); row.className = className;
    const strong = document.createElement('strong'); strong.textContent = `${label}: `;
    row.append(strong, document.createTextNode(values.join(' · '))); parent.appendChild(row);
  }

  function renderFormationAudit(attempts, detailed) {
    const host = $('formation-audit'), cards = $('formation-attempts'); cards.replaceChildren();
    const rows = latestBatchAttempts(attempts);
    host.hidden = !rows.length;
    if (!rows.length) return;
    const failures = rows.filter(row => row.status === 'failed' || row.terminal_type === 'context-graph-identity-failed').length;
    const omissionCount = rows.reduce((count, row) => count + (row.omissions || []).length, 0);
    const completed = rows.length - failures;
    $('formation-audit-summary').textContent = `${completed}/${rows.length} batches complete${omissionCount ? ` · ${omissionCount} omitted` : ''}`;
    host.classList.toggle('has-issues', failures > 0 || omissionCount > 0);
    for (const row of rows) {
      const failed = row.status === 'failed' || row.terminal_type === 'context-graph-identity-failed';
      const card = document.createElement('article'); card.className = `formation-attempt${failed || (row.omissions || []).length ? ' issue' : ''}`;
      const heading = document.createElement('div'); heading.className = 'formation-attempt-heading';
      const glyph = document.createElement('span'); glyph.className = 'receipt-glyph'; glyph.textContent = failed ? '!' : ((row.omissions || []).length ? '!' : '✓');
      const title = document.createElement('strong'); title.textContent = `batch ${row.batch_index} · attempt ${row.attempt || 1}`;
      const protocol = document.createElement('span'); protocol.className = 'receipt-protocol'; protocol.textContent = row.protocol || 'recorded protocol';
      heading.append(glyph, title, protocol); card.appendChild(heading);
      if (failed) {
        appendAuditText(card, 'receipt-failure', 'Failed', [row.failure_reason || 'unspecified', row.failure_class || 'unclassified'].filter(Boolean));
      } else if (detailed) {
        appendAuditText(card, 'receipt-proposals', 'Proposed identities', (row.proposal_entities || []).map(item => `${item.label} (${item.identity_action})`));
        appendAuditText(card, 'receipt-proposals', 'Proposed claims', (row.proposal_relationships || []).map(item => item.label));
        appendAuditText(card, 'receipt-omissions', 'Omitted', (row.omissions || []).map(item => `${item.label || item.claim_ref} — ${item.reason}`));
        if (!(row.omissions || []).length) appendAuditText(card, 'receipt-retained', 'Review selection',
          [(row.proposal_entities || []).length || (row.proposal_relationships || []).length ? 'all proposed claims retained' : 'completed empty — no claims proposed']);
      } else {
        appendAuditText(card, 'receipt-proposals', 'Recorded proposal', [`${row.proposal_entity_count || 0} identities`, `${row.proposal_relationship_count || 0} claims`]);
      }
      cards.appendChild(card);
    }
  }

  function selectEpisode(episode) {
    state.run = null;
    state.comparison = null;
    if (state.config.case_mode) {
      $('case-phase').replaceChildren(new Option('Choose a phase', ''));
      for (const phase of episode.phases || []) $('case-phase').appendChild(new Option(`${phase.event_id} · ${phase.phase}`, phase.event_id));
      $('case-response').value = '';
    }
    state.selected = episode;
    state.cutoff = episode.after_event_id;
    $('graph-title').textContent = short(episode.synopsis || episode.episode_id, 90);
    $('replay').disabled = episode.status !== 'completed';
    $('query-cutoff').textContent = `event ${state.cutoff}`;
    $('graph-summary').textContent = episode.status === 'completed'
      ? `Ready to compare event ${episode.before_event_id} → ${episode.after_event_id}. No provider call will run.`
      : `This episode is ${episode.status}; no completed durable receipt exists to replay.`;
    if (state.config.case_mode) $('formation-audit').hidden = true;
    else renderFormationAudit(episode.attempts, false);
    renderSuggestions(episode); renderEpisodeList(); showDetails(episode);
  }

  function positions(nodes, edges, width, height) {
    const result = new Map();
    const radius = Math.max(90, Math.min(width, height) * .34);
    nodes.forEach((node, index) => {
      const angle = -Math.PI / 2 + index * Math.PI * 2 / Math.max(1, nodes.length);
      result.set(node.node_id, { x: width / 2 + Math.cos(angle) * radius, y: height / 2 + Math.sin(angle) * radius, vx: 0, vy: 0 });
    });
    for (let step = 0; step < 110 && nodes.length < 180; step++) {
      for (let i = 0; i < nodes.length; i++) for (let j = i + 1; j < nodes.length; j++) {
        const a = result.get(nodes[i].node_id), b = result.get(nodes[j].node_id);
        let dx = a.x - b.x, dy = a.y - b.y, distance = Math.max(18, Math.hypot(dx, dy));
        const force = 850 / (distance * distance); dx /= distance; dy /= distance;
        a.vx += dx * force; a.vy += dy * force; b.vx -= dx * force; b.vy -= dy * force;
      }
      for (const edge of edges) {
        const a = result.get(edge.from_node_id), b = result.get(edge.to_node_id); if (!a || !b) continue;
        let dx = b.x - a.x, dy = b.y - a.y, distance = Math.max(1, Math.hypot(dx, dy));
        const force = (distance - 125) * .0018; dx /= distance; dy /= distance;
        a.vx += dx * force; a.vy += dy * force; b.vx -= dx * force; b.vy -= dy * force;
      }
      for (const point of result.values()) {
        point.vx += (width / 2 - point.x) * .0007; point.vy += (height / 2 - point.y) * .0007;
        point.vx *= .86; point.vy *= .86; point.x = Math.max(42, Math.min(width - 42, point.x + point.vx)); point.y = Math.max(42, Math.min(height - 42, point.y + point.vy));
      }
    }
    return result;
  }

  function draw(data) {
    state.graph = data; const svg = $('graph'); svg.replaceChildren(); state.selectedGraphId = null;
    const nodes = data.nodes || [], edges = data.edges || []; $('empty-state').style.display = nodes.length ? 'none' : 'grid';
    const hasDelta = nodes.some(row => (row.change || 'unchanged') !== 'unchanged') || edges.some(row => (row.change || 'unchanged') !== 'unchanged');
    $('emphasize-delta').disabled = !hasDelta;
    svg.classList.toggle('delta-emphasis', hasDelta && $('emphasize-delta').checked);
    if (!nodes.length) return;
    const width = Math.max(560, svg.clientWidth || 900), height = Math.max(500, svg.clientHeight || 700);
    svg.setAttribute('viewBox', `0 0 ${width} ${height}`);
    const title = svgElement('title'); title.textContent = hasDelta ? 'Knowledge graph episode delta' : 'Knowledge graph at selected cutoff';
    const description = svgElement('desc'); description.textContent = hasDelta
      ? 'Added nodes are hexagons marked plus. Changed nodes are squares marked tilde. Unchanged nodes are circles marked equals. Removed nodes are crossed circles. Edge labels begin with the same state symbol.'
      : 'Knowledge graph nodes and directed edges at the selected event cutoff.';
    svg.append(title, description);
    const defs = svgElement('defs'); const marker = svgElement('marker', { id: 'arrow', viewBox: '0 0 10 10', refX: 29, refY: 5, markerWidth: 5, markerHeight: 5, orient: 'auto-start-reverse' });
    marker.appendChild(svgElement('path', { d: 'M 0 0 L 10 5 L 0 10 z', fill: '#607b84' })); defs.appendChild(marker); svg.appendChild(defs);
    const at = positions(nodes, edges, width, height);
    for (const edge of edges) {
      const from = at.get(edge.from_node_id), to = at.get(edge.to_node_id); if (!from || !to) continue;
      const eligibility = edge.query_eligible === false ? ' query-ineligible' : '';
      const change = edge.change || 'unchanged';
      const epistemic = edge.evidence_status === 'inference' ? ' inferred' : '';
      const line = svgElement('line', { x1: from.x, y1: from.y, x2: to.x, y2: to.y, class: `edge ${change}${eligibility}${epistemic}`, 'marker-end': 'url(#arrow)' });
      line.addEventListener('click', () => showDetails(edge)); svg.appendChild(line);
      const symbols = { added: '+', changed: '~', unchanged: '=', removed: '−' };
      const label = svgElement('text', { x: (from.x + to.x) / 2, y: (from.y + to.y) / 2 - 5, class: `edge-label ${change}` });
      const eligibilitySymbol = edge.query_eligible === false ? '⊘ ' : '';
      const inferenceSymbol = edge.evidence_status === 'inference' ? '[I] ' : '';
      label.textContent = `[${symbols[change] || '='}] ${inferenceSymbol}${eligibilitySymbol}${edge.predicate || 'related'}`; svg.appendChild(label);
    }
    for (const node of nodes) {
      const point = at.get(node.node_id), change = node.change || 'unchanged';
      const group = svgElement('g', { class: `node ${change}` }); group.dataset.id = node.node_id;
      let shape;
      if (change === 'added') {
        const points = Array.from({ length: 6 }, (_, index) => {
          const angle = Math.PI / 6 + index * Math.PI / 3;
          return `${point.x + 34 * Math.cos(angle)},${point.y + 34 * Math.sin(angle)}`;
        }).join(' ');
        shape = svgElement('polygon', { points, class: 'node-shape' });
      } else if (change === 'changed') {
        shape = svgElement('rect', { x: point.x - 30, y: point.y - 30, width: 60, height: 60, rx: 7, class: 'node-shape' });
      } else {
        shape = svgElement('circle', { cx: point.x, cy: point.y, r: 31, class: 'node-shape' });
      }
      group.appendChild(shape);
      if (change === 'removed') {
        group.appendChild(svgElement('line', { x1: point.x - 20, y1: point.y - 20, x2: point.x + 20, y2: point.y + 20, class: 'removal-cross' }));
        group.appendChild(svgElement('line', { x1: point.x + 20, y1: point.y - 20, x2: point.x - 20, y2: point.y + 20, class: 'removal-cross' }));
      }
      const title = svgElement('title'); title.textContent = `${node.label || '(unnamed)'}\n${node.node_id}`; group.appendChild(title);
      const glyphs = { added: '+', changed: '~', unchanged: '=', removed: '×' };
      const glyph = svgElement('text', { x: point.x, y: point.y - 12, class: 'delta-glyph' }); glyph.textContent = glyphs[change]; group.appendChild(glyph);
      const label = svgElement('text', { x: point.x, y: point.y + 3 }); label.textContent = short(node.label || node.node_kind, 14); group.appendChild(label);
      group.addEventListener('click', () => showDetails(node));
      group.addEventListener('dblclick', () => { $('query').value = node.label || ''; runQuery(node.node_id); }); svg.appendChild(group);
    }
  }

  async function replayDelta() {
    if (!state.selected || state.selected.status !== 'completed') return;
    if (state.config.case_mode) { await replayCase(); return; }
    $('graph-summary').textContent = 'Cold-replaying durable receipts…'; $('replay').disabled = true;
    try {
      const data = await api('/api/delta', { before_event_id: state.selected.before_event_id, after_event_id: state.selected.after_event_id });
      draw(data); const addedNodes = data.nodes.filter(row => row.change === 'added').length, addedEdges = data.edges.filter(row => row.change === 'added').length;
      $('graph-summary').textContent = `event ${data.before.through_event_id}: ${data.before.node_count} entities / ${data.before.edge_count} claims → event ${data.after.through_event_id}: ${data.after.node_count} / ${data.after.edge_count} · +${addedNodes} entities · +${addedEdges} claims · ${data.after.query_eligible_edge_count} query-eligible facts`;
      renderFormationAudit(data.formation_attempts || state.selected.attempts, Boolean(data.formation_attempts));
      showDetails({ episode: state.selected.episode_id, before: data.before, after: data.after, added_nodes: addedNodes, added_edges: addedEdges, formation_attempts: data.formation_attempts || [] });
    } catch (error) { $('graph-summary').textContent = `Replay failed: ${error.message}`; }
    finally { $('replay').disabled = false; }
  }

  async function showHead() {
    state.cutoff = state.config.manifest.head_event_id; $('query-cutoff').textContent = `head ${state.cutoff}`; $('graph-summary').textContent = 'Replaying immutable seed head…'; $('formation-audit').hidden = true;
    try { const data = await api('/api/graph', { through_event_id: state.cutoff }); draw(data); $('graph-summary').textContent = `${data.node_count} entities · ${data.edge_count} admitted claims · ${data.query_eligible_edge_count} query-eligible facts · through event ${data.through_event_id}`; }
    catch (error) { $('graph-summary').textContent = `Head replay failed: ${error.message}`; }
  }

  function queryRequest(startingNodeId) {
    return {starting_node_id: startingNodeId || null,
      query: startingNodeId ? null : ($('query').value.trim() || null),
      exact_queries: $('exact-queries').value.split(/\r?\n/).map(value => value.trim()).filter(Boolean),
      predicates: [], direction: $('direction').value, evidence_policy: $('evidence').value,
      maximum_depth: Number($('depth').value), maximum_paths: Number($('paths').value)};
  }
  async function runQuery(startingNodeId) {
    if (!state.cutoff) state.cutoff = state.config.manifest.head_event_id;
    const query = $('query').value.trim(); const exact = $('exact-queries').value.split(/\r?\n/).map(value => value.trim()).filter(Boolean);
    if (!query && !startingNodeId && !exact.length) { $('query-summary').textContent = 'Enter a natural query, exact audit name, or double-click a node.'; return; }
    $('query-summary').textContent = 'Running the reviewed-context-graph query path…';
    try {
      const activeRun = state.comparison ? state.comparison[$('comparison-version').value] : state.run;
      if (state.config.case_mode && !activeRun?.run_id) throw new Error('Run the selected case first; no automatic replay is performed.');
      const result = await api(state.config.case_mode ? '/api/case-query' : '/api/query', { through_event_id: state.cutoff,
        run_id: activeRun?.run_id, side: $('case-side').value, request: queryRequest(startingNodeId)});
      draw(result); $('query-summary').textContent = `${result.answer_status} · returned ${result.node_count} nodes / ${result.edge_count} edges · graph total ${result.graph_entity_count} / ${result.graph_fact_count} · ${result.database_write_count || 0} writes`;
      showDetails(result);
    } catch (error) { $('query-summary').textContent = `Query failed: ${error.message}`; }
  }

  async function initialize() {
    try {
      state.config = await api('/api/config'); state.catalog = await api('/api/episodes'); state.cutoff = state.config.manifest.head_event_id;
      $('case-controls').hidden = !state.config.case_mode;
      if (state.config.case_mode) { $('show-head').disabled = true; $('show-head').textContent = 'Checkpoint mode'; $('replay').textContent = 'Run checkpoint case'; }
      const freshEnabled = state.config.fresh_phase_execution === 'enabled';
      $('execute-phase').disabled = !freshEnabled;
      $('fresh-execution-status').textContent = freshEnabled
        ? 'Fresh execution enabled: exact digest confirmation and a unique reservation ID are required; one call, no retry.'
        : 'Fresh execution disabled: no provider request can be made by this lab.';
      const hash = state.config.manifest.events_sha256; $('seed-summary').textContent = `${state.config.manifest.episode_count} episodes · event head ${state.cutoff} · seed ${hash.slice(0, 12)}…${hash.slice(-8)}`;
      renderStatusFilters(); renderEpisodeList(); $('query-cutoff').textContent = `head ${state.cutoff}`;
    } catch (error) { $('seed-summary').textContent = `Lab initialization failed: ${error.message}`; }
  }

  $('episode-filter').addEventListener('input', renderEpisodeList);
  async function replayCase() {
    $('replay').disabled = true; $('graph-summary').textContent = 'Replaying saved checkpoint and receipts (30-second deadline)…';
    try {
      const phase = Number($('case-phase').value);
      const request = { case_id: state.selected.case_id, compare: $('compare-recorded').checked,
        expect_no_change: $('expect-no-change').checked };
      if ($('save-query-with-run').checked) request.queries = [queryRequest()];
      if ($('stop-at-phase').checked) { if (!phase) throw new Error('Choose a response phase.'); request.through_event_id = phase; }
      if ($('case-mode').value === 'counterfactual') {
        if (!phase) throw new Error('Choose the phase to replace.');
        request.counterfactual = { event_id: phase, response: JSON.parse($('case-response').value) };
      }
      const result = await api('/api/case-run', request);
      displayCaseResult(result);
    } catch (error) { $('graph-summary').textContent = `Case failed: ${error.message}`; }
    finally { $('replay').disabled = false; }
  }
  function displayCaseResult(result) {
    // Preserve the artifact ID before rendering: presentation failures need no rerun.
    $('saved-run-id').value = result.saved_result || '';
    state.comparison = result.candidate ? result : null; state.run = result.candidate || result;
    const run = state.run;
    if (run.before || result.comparison?.graph_delta) draw(result.comparison?.graph_delta || run.delta || run.before);
    renderFormationAudit(run.formation_attempts || [], true);
    const comparison = result.comparison ? `comparison ${result.comparison.status} · ${result.comparison.changed_count ?? '?'} changes · ` : '';
    const reopened = result.reopened_without_replay ? 'Reopened without replay · cached queries may have expired · ' : '';
    $('graph-summary').textContent = `${reopened}${comparison}${run.mode || 'run'} · ${run.status} · ${Number(run.elapsed_seconds || 0).toFixed(2)} s · ${run.provider_calls} provider calls · ${run.historical_fold_count} historical folds · saved ${result.saved_result}`;
    showDetails(run.failure || { error: run.error, contract: run.contract,
      fingerprinted_files: Object.keys(result.code_fingerprint || {}).length,
      fingerprint_details: 'Use Show complete run and admission trace for the pinned file hashes.',
      comparison: result.comparison, pending: run.pending });
  }
  $('reopen-run').addEventListener('click', async () => {
    try { displayCaseResult(await api('/api/case-result', {run_id: $('saved-run-id').value.trim()})); }
    catch (error) { $('graph-summary').textContent = `Saved result unavailable: ${error.message}`; }
  });
  $('verify-run').addEventListener('click', async () => {
    $('verify-run').disabled = true;
    try {
      if (!state.selected) throw new Error('Choose a case first.');
      const result = await api('/api/case-verify', {
        case_id: state.selected.case_id, run_id: $('saved-run-id').value.trim()});
      $('graph-summary').textContent = `Expectation matrix ${result.status} · ${result.failure_count} failures · no replay · ${result.provider_calls} provider calls`;
      showDetails(result);
    } catch (error) { $('graph-summary').textContent = `Expectation check refused: ${error.message}`; }
    finally { $('verify-run').disabled = false; }
  });
  $('plan-phase').addEventListener('click', async () => {
    $('plan-phase').disabled = true;
    try {
      const phase = Number($('case-phase').value);
      if (!state.selected || !phase) throw new Error('Choose a case and one response phase first.');
      const plan = await api('/api/case-plan', {case_id: state.selected.case_id, event_id: phase});
      $('saved-run-id').value = plan.saved_result;
      $('graph-summary').textContent = `Estimate only · ${plan.phase} · 1 call · upper bound $${plan.maximum_request_cost_usd.toFixed(6)} · not authorized · 0 calls made`;
      showDetails({saved_result: plan.saved_result, phase: plan.phase, request_digest: plan.request_digest,
        request_changed: plan.request_changed, per_request_limit_passed: plan.within_per_request_ceiling,
        cumulative_budget_status: plan.cumulative_budget_status, downstream_status: plan.downstream_status,
        note: 'Reopen this saved result to inspect the full request. No provider execution is enabled.'});
    } catch (error) { $('graph-summary').textContent = `Phase preflight failed: ${error.message}`; }
    finally { $('plan-phase').disabled = false; }
  });
  $('execute-phase').addEventListener('click', async () => {
    $('execute-phase').disabled = true;
    try {
      const phase = Number($('case-phase').value);
      if (!state.selected || !phase) throw new Error('Choose a case and one response phase first.');
      const result = await api('/api/case-execute', {case_id: state.selected.case_id,
        event_id: phase, confirmed_request_digest: $('fresh-confirm-digest').value.trim(),
        reservation_id: $('fresh-reservation-id').value.trim()});
      displayCaseResult(result);
    } catch (error) { $('graph-summary').textContent = `Fresh phase refused or incomplete: ${error.message}`; }
    finally { $('execute-phase').disabled = state.config.fresh_phase_execution !== 'enabled'; }
  });
  $('compare-saved').addEventListener('click', async () => {
    $('compare-saved').disabled = true;
    try {
      const result = await api('/api/case-compare-saved', {
        baseline_run_id: $('baseline-run-id').value.trim(), candidate_run_id: $('saved-run-id').value.trim(),
        expect_no_change: $('expect-no-change').checked});
      draw(result.comparison.graph_delta);
      $('graph-summary').textContent = `Saved-run comparison ${result.comparison.status} · ${result.comparison.changed_count} changes · ${result.comparison.evidence_loss.length} evidence-loss records · no replay · saved ${result.saved_result}`;
      showDetails(result);
    } catch (error) { $('graph-summary').textContent = `Saved comparison refused: ${error.message}`; }
    finally { $('compare-saved').disabled = false; }
  });
  $('case-phase').addEventListener('change', async () => {
    if (!$('case-phase').value) return;
    try { const phase = await api('/api/case-phase', {case_id: state.selected.case_id, event_id: Number($('case-phase').value)});
      $('case-response').value = JSON.stringify(phase.response, null, 2);
    } catch (error) { $('graph-summary').textContent = error.message; }
  });
  $('case-trace').addEventListener('click', () => showDetails(state.comparison || state.run || {message: 'Run a case first.'}));
  $('comparison-version').addEventListener('change', () => {
    if (state.comparison) draw($('comparison-version').value === 'baseline' ? state.comparison.baseline.after :
      (state.comparison.comparison.graph_delta || state.comparison.candidate.after || state.comparison.candidate.before));
  });
  $('replay').addEventListener('click', replayDelta); $('show-head').addEventListener('click', showHead);
  $('emphasize-delta').addEventListener('change', () => {
    $('graph').classList.toggle('delta-emphasis', !$('emphasize-delta').disabled && $('emphasize-delta').checked);
  });
  $('query-form').addEventListener('submit', event => { event.preventDefault(); runQuery(); });
  initialize();
})();
