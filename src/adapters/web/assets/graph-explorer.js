(function () {
  'use strict';
  const ns = 'http://www.w3.org/2000/svg';
  const form = document.getElementById('search-form');
  const query = document.getElementById('query');
  const depth = document.getElementById('depth');
  const evidence = document.getElementById('evidence');
  const svg = document.getElementById('graph');
  const summary = document.getElementById('summary');
  const details = document.getElementById('details');
  const empty = document.getElementById('empty');
  let selectedId = null;

  function element(tag, attrs) {
    const node = document.createElementNS(ns, tag);
    for (const [key, value] of Object.entries(attrs || {})) node.setAttribute(key, value);
    return node;
  }

  function shortLabel(value) {
    value = String(value || '(unnamed)');
    return value.length > 18 ? value.slice(0, 17) + '\u2026' : value;
  }

  function showDetail(value) {
    selectedId = value.node_id || value.edge_id || null;
    details.replaceChildren();
    const descriptor = document.createElement('pre');
    descriptor.textContent = JSON.stringify(value, null, 2);
    details.appendChild(descriptor);
    if (value.node_id) {
      const expand = document.createElement('button');
      expand.type = 'button';
      expand.textContent = 'Expand relationships from this node';
      expand.addEventListener('click', () => search({starting_node_id:value.node_id, query:null}));
      details.appendChild(expand);
    }
    for (const node of svg.querySelectorAll('.node'))
      node.classList.toggle('selected', node.dataset.id === selectedId);
  }

  function positions(nodes, width, height) {
    const centerX = width / 2, centerY = height / 2;
    const radius = Math.max(110, Math.min(width, height) * .35);
    const result = new Map();
    nodes.forEach((node, index) => {
      const angle = nodes.length === 1 ? 0 : (-Math.PI / 2 + index * Math.PI * 2 / nodes.length);
      result.set(node.node_id, {x:centerX + Math.cos(angle) * radius, y:centerY + Math.sin(angle) * radius});
    });
    return result;
  }

  function draw(data) {
    svg.replaceChildren(); selectedId = null;
    const nodes = data.nodes || [], edges = data.edges || [];
    empty.style.display = nodes.length ? 'none' : 'grid';
    const match = data.query_match_kind === 'exact-identity' ? 'Exact identity \u00b7 ' :
      (data.query_match_kind === 'related-suggestions' ? 'No exact identity \u00b7 related suggestions \u00b7 ' : '');
    summary.textContent = match + `${data.node_count || 0} nodes \u00b7 ${data.edge_count || 0} relationships` +
      (data.non_exhaustive ? ' \u00b7 bounded result; more may exist' : '');
    if (!nodes.length) return;
    const width = Math.max(620, svg.clientWidth || 620), height = Math.max(520, svg.clientHeight || 520);
    svg.setAttribute('viewBox', `0 0 ${width} ${height}`);
    const definitions = element('defs');
    const marker = element('marker', {id:'arrow',viewBox:'0 0 10 10',refX:31,refY:5,
      markerWidth:5,markerHeight:5,orient:'auto-start-reverse'});
    marker.appendChild(element('path', {d:'M 0 0 L 10 5 L 0 10 z',fill:'#49647f'}));
    definitions.appendChild(marker); svg.appendChild(definitions);
    const at = positions(nodes, width, height);
    for (const edge of edges) {
      const from = at.get(edge.from_node_id), to = at.get(edge.to_node_id);
      if (!from || !to) continue;
      const line = element('line', {x1:from.x,y1:from.y,x2:to.x,y2:to.y,class:'edge','marker-end':'url(#arrow)'});
      line.addEventListener('click', () => showDetail(edge));
      svg.appendChild(line);
      const label = element('text', {x:(from.x+to.x)/2,y:(from.y+to.y)/2-4,class:'edge-label'});
      label.textContent = edge.predicate || 'related';
      label.addEventListener('click', () => showDetail(edge));
      svg.appendChild(label);
    }
    for (const node of nodes) {
      const point = at.get(node.node_id), group = element('g', {class:'node'});
      group.dataset.id = node.node_id;
      const origins = (node.origin_agent_ids || []).length
        ? `\nImported origin: ${(node.origin_agent_ids || []).join(', ')}`
        : '';
      const title = element('title'); title.textContent = `${node.label || '(unnamed)'}\n${node.node_id}${origins}`;
      group.appendChild(title);
      group.appendChild(element('circle', {cx:point.x,cy:point.y,r:38}));
      const label = element('text', {x:point.x,y:point.y+4});
      label.textContent = shortLabel(node.label); group.appendChild(label);
      group.addEventListener('click', () => showDetail(node));
      group.addEventListener('dblclick', () => search({starting_node_id:node.node_id, query:null}));
      svg.appendChild(group);
    }
  }

  async function search(overrides) {
    summary.textContent = 'Searching\u2026';
    const request = Object.assign({starting_node_id:null,query:query.value.trim() || null,
      predicates:[],direction:'both',evidence_policy:evidence.value,
      maximum_depth:Number(depth.value),maximum_paths:12}, overrides || {});
    try {
      const response = await fetch('/api/v2/graph/search', {method:'POST',credentials:'same-origin',
        headers:{'Content-Type':'application/json','X-PAI-Request':'same-origin'},body:JSON.stringify(request)});
      const data = await response.json();
      if (!response.ok || data.error) throw new Error(data.error || 'Graph request failed');
      draw(data);
    } catch (error) {
      summary.textContent = `Graph search failed: ${error.message}`;
    }
  }

  form.addEventListener('submit', event => { event.preventDefault(); search(); });
})();
