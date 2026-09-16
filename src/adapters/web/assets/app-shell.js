/* Small shared navigation shell. Feature pages register entries; the shell
   knows no terminal, graph, file, or diagnostic implementation details. */
(function () {
  'use strict';
  if (typeof window === 'undefined') return;

  const fontKey = 'pai.text-size';
  function readSize() { try { return localStorage.getItem(fontKey); } catch (_) { return null; } }
  function setTextSize(value, persist = true) {
    const number = Number(value);
    const size = Number.isFinite(number) && number >= 12 ? Math.min(22, Math.round(number)) : 14;
    document.documentElement.style.setProperty('--pai-text-size', size + 'px');
    document.documentElement.style.setProperty('--pai-text-scale', size / 14);
    if (persist) { try { localStorage.setItem(fontKey, String(size)); } catch (_) {} }
    window.dispatchEvent(new CustomEvent('pai-text-size', {detail:size}));
    return size;
  }
  setTextSize(readSize(), false);
  window.addEventListener('storage', event => { if (event.key === fontKey || event.key === null) setTextSize(readSize(), false); });

  const entries = new Map();
  let button = null, drawer = null, list = null;

  function close() {
    if (!drawer) return;
    drawer.classList.remove('open');
    button.setAttribute('aria-expanded', 'false');
  }

  function render() {
    if (!list) return;
    list.replaceChildren();
    for (const entry of entries.values()) {
      const item = document.createElement(entry.href ? 'a' : 'button');
      item.textContent = entry.label;
      item.className = 'pai-menu-item';
      if (entry.href) item.href = entry.href;
      else {
        item.type = 'button';
        item.addEventListener('click', () => { close(); entry.action(); });
      }
      if (entry.current) item.setAttribute('aria-current', 'page');
      list.appendChild(item);
    }
  }

  function register(entry) {
    if (!entry || !entry.id || !entry.label || (!entry.href && !entry.action)) return;
    entries.set(entry.id, entry);
    render();
  }

  function install() {
    const controls = document.querySelector('header .btns');
    if (!controls) return;
    const style = document.createElement('style');
    style.textContent = `
      #pai-menu-button { background:none; border:1px solid var(--border,#223042);
        color:var(--dim,#8b98a5); border-radius:6px; min-width:38px; min-height:32px;
        font:18px/1 ui-monospace,monospace; cursor:pointer; }
      #pai-menu-button:hover { color:var(--text,#e6edf3); border-color:var(--accent,#5fb3ff); }
      #pai-menu-drawer { position:fixed; z-index:90; top:calc(8px + env(safe-area-inset-top));
        right:calc(8px + env(safe-area-inset-right)); width:min(280px,calc(100vw - 16px));
        padding:8px; display:none; flex-direction:column; gap:5px;
        background:var(--panel,#10151c); border:1px solid var(--border,#223042);
        border-radius:9px; box-shadow:0 12px 32px rgba(0,0,0,.45); }
      #pai-menu-drawer.open { display:flex; }
      .pai-menu-item { display:block; width:100%; border:0; border-radius:6px;
        background:none; color:var(--text,#e6edf3); padding:10px 12px; text-align:left;
        text-decoration:none; font:13px ui-monospace,monospace; cursor:pointer; }
      .pai-menu-item:hover,.pai-menu-item[aria-current=page] { background:#1c2b3a; color:var(--accent,#5fb3ff); }
    `;
    document.head.appendChild(style);
    button = document.createElement('button');
    button.id = 'pai-menu-button'; button.type = 'button';
    button.textContent = '\u2630'; button.title = 'Open menu';
    button.setAttribute('aria-label', 'Open navigation menu');
    button.setAttribute('aria-expanded', 'false');
    drawer = document.createElement('nav');
    drawer.id = 'pai-menu-drawer'; drawer.setAttribute('aria-label', 'Application');
    list = document.createElement('div'); drawer.appendChild(list);
    button.addEventListener('click', event => {
      event.stopPropagation();
      const open = !drawer.classList.contains('open');
      drawer.classList.toggle('open', open);
      button.setAttribute('aria-expanded', String(open));
    });
    document.addEventListener('click', event => {
      if (!drawer.contains(event.target) && event.target !== button) close();
    });
    document.addEventListener('keydown', event => { if (event.key === 'Escape') close(); });
    controls.replaceChildren(button); document.body.appendChild(drawer);
    const path = window.location.pathname;
    register({id:'terminal', label:'Terminal', href:'/terminal', current:path === '/' || path === '/terminal'});
    register({id:'graph', label:'Graph Explorer', href:'/graph', current:path === '/graph'});
    register({id:'observability', label:'Observability', href:'/dashboard', current:path === '/dashboard'});
    register({id:'settings', label:'Settings', href:'/settings', current:path === '/settings'});
    window.dispatchEvent(new CustomEvent('pai-shell-ready'));
  }

  window.paiShell = {register, close, setTextSize,
    getTextSize:() => parseInt(document.documentElement.style.getPropertyValue('--pai-text-size'),10) || 14};
  if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', install);
  else install();
})();
