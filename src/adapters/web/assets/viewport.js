/* One viewport policy for terminal, Files and login. No telemetry or storage. */
(function () {
  'use strict';
  const REVISION = 'visual-viewport-v2';
  function keyboardGeometry(innerHeight, viewport, editable) {
    // Zoom also shrinks the visual viewport. Never undo the user's zoom.
    if (!viewport || !editable || Math.abs(viewport.scale - 1) > 0.01 ||
        !(viewport.height > 0 && viewport.height < innerHeight - 80)) return null;
    return {top: Math.max(0, Math.round(viewport.offsetTop || 0)), height: Math.round(viewport.height)};
  }
  function visibleGeometry(viewport) {
    if (!viewport || Math.abs(viewport.scale - 1) > 0.01 || !(viewport.height > 0)) return null;
    return {top: Math.max(0, Math.round(viewport.offsetTop || 0)), height: Math.round(viewport.height)};
  }
  if (typeof module !== 'undefined') module.exports = {keyboardGeometry, visibleGeometry};
  if (typeof window === 'undefined') return;

  const root = document.documentElement;
  const standalone = window.matchMedia('(display-mode: standalone)');
  const style = document.createElement('style');
  style.textContent = `
    :root { --pai-shell-height:100vh; }
    @supports (height:100dvh) { :root { --pai-shell-height:100dvh; } }
    #app, body.pai-login, .modal { bottom:auto; top:var(--pai-visible-top,0px); height:var(--pai-visible-height,var(--pai-shell-height)); }
    html.pai-keyboard #inputbar { padding-bottom:10px; }
    html.pai-keyboard #app header { padding-top:8px; }
    #inputbar textarea { max-height:min(200px,calc(var(--pai-visible-height,var(--pai-shell-height)) * .3)); }
    html.pai-keyboard body.pai-login { padding-top:20px; padding-bottom:20px; align-items:start; }
    html.pai-keyboard .modal { padding-top:8px; padding-bottom:8px; }
    html.pai-keyboard .fm-list { min-height:0; }
    #pai-layout-panel { position:fixed; z-index:100; left:8px; right:8px;
      top:calc(8px + env(safe-area-inset-top)); max-height:65vh; overflow:auto;
      background:#10151c; color:#e6edf3; border:1px solid #5fb3ff; padding:10px; }
    #pai-layout-panel pre { font:11px monospace; white-space:pre-wrap; user-select:text; }
    #pai-layout-panel button { width:auto; min-height:32px; margin:4px; }
  `;
  document.head.appendChild(style);
  if (document.getElementById('login-form')) document.body.classList.add('pai-login');
  let scheduled = false, lastReason = 'load', output = null;
  function rect(selector) {
    const el = document.querySelector(selector);
    if (!el) return null;
    const r = el.getBoundingClientRect(), css = getComputedStyle(el);
    return {top:r.top, bottom:r.bottom, height:r.height, width:r.width,
      paddingTop:css.paddingTop, paddingBottom:css.paddingBottom};
  }
  function snapshot() {
    const vv = window.visualViewport;
    const safeArea = {};
    for (const edge of ['top','bottom','left','right']) {
      const probe = document.createElement('div');
      probe.style.cssText = `position:fixed;visibility:hidden;height:env(safe-area-inset-${edge},0px);width:0;pointer-events:none`;
      document.body.appendChild(probe);
      safeArea[edge] = probe.getBoundingClientRect().height;
      probe.remove();
    }
    return {revision:REVISION, reason:lastReason, standalone:root.classList.contains('pai-standalone'),
      keyboard:root.classList.contains('pai-keyboard'),
      screen:{width:window.screen.width,height:window.screen.height},
      inner:{width:window.innerWidth,height:window.innerHeight},
      document:{width:root.clientWidth,height:root.clientHeight},
      visual:vv ? {width:vv.width,height:vv.height,top:vv.offsetTop,left:vv.offsetLeft,scale:vv.scale} : null,
      safeArea, shell:rect('#app') || rect('body'), header:rect('header'),
      composer:rect('#inputbar'), send:rect('#send'), login:rect('main'), files:rect('.modal.open')};
  }
  function sync() {
    scheduled = false;
    root.classList.toggle('pai-standalone', navigator.standalone === true || standalone.matches);
    const active = document.activeElement;
    const editable = active && (active.tagName === 'TEXTAREA' || active.tagName === 'INPUT' || active.isContentEditable);
    // Use the visible rectangle even when innerHeight has also shrunk or focus
    // has moved to Send during keyboard animation. Never counteract pinch zoom.
    const geometry = visibleGeometry(window.visualViewport);
    const layoutHeight = Math.max(window.innerHeight, root.clientHeight);
    root.classList.toggle('pai-keyboard', !!keyboardGeometry(layoutHeight, window.visualViewport, editable));
    for (const key of ['top','height']) {
      if (geometry) root.style.setProperty('--pai-visible-' + key, geometry[key] + 'px');
      else root.style.removeProperty('--pai-visible-' + key);
    }
    if (output) output.textContent = JSON.stringify(snapshot(), null, 2);
  }
  function schedule(event) {
    lastReason = typeof event === 'string' ? event : event.type;
    if (!scheduled) { scheduled = true; window.requestAnimationFrame(sync); }
  }
  function showDiagnostics() {
    if (output) return;
    const panel = document.createElement('section');
    panel.id = 'pai-layout-panel';
    panel.setAttribute('aria-label', 'Local layout diagnostics');
    const close = document.createElement('button');
    close.type = 'button'; close.textContent = 'Close layout diagnostics';
    close.onclick = () => { panel.remove(); output = null; };
    output = document.createElement('pre');
    panel.append(close, output); document.body.appendChild(panel);
    schedule('diagnostics');
  }
  function registerLayoutMenu() {
    if (window.paiShell) {
      window.paiShell.register({id:'layout',label:'Layout diagnostics',action:showDiagnostics});
      return true;
    }
    return false;
  }
  if (!registerLayoutMenu()) window.addEventListener('pai-shell-ready', registerLayoutMenu);
  window.paiViewport = {snapshot, showDiagnostics};
  for (const name of ['resize','orientationchange','pageshow']) window.addEventListener(name, schedule, {passive:true});
  let focusTimers = [];
  for (const name of ['focusin','focusout']) document.addEventListener(name, event => {
    schedule(event);
    focusTimers.forEach(clearTimeout);
    // WebKit may publish its final keyboard rectangle after the focus event.
    focusTimers = [100, 300, 600].map(delay => setTimeout(() => schedule('focus-settled'), delay));
  });
  document.addEventListener('visibilitychange', () => { if (!document.hidden) schedule('visible-resume'); });
  if (standalone.addEventListener) standalone.addEventListener('change', schedule);
  if (window.visualViewport) for (const name of ['resize','scroll']) window.visualViewport.addEventListener(name, schedule, {passive:true});
  sync();
  if (new URLSearchParams(window.location.search).get('viewport-debug') === '1') showDiagnostics();
})();
