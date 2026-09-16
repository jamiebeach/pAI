/* pAI's service worker intentionally caches no authenticated or private data.
   It establishes the installable app boundary while all requests remain under
   the existing authenticated, network-only web adapter. */
self.addEventListener('install', () => self.skipWaiting());
self.addEventListener('activate', event => event.waitUntil(self.clients.claim()));
