const OFFLINE_CACHE = "workout-notes-v1";
const APP_SHELL = ["./", "./index.html"];

const cacheAppShell = () => caches.open(OFFLINE_CACHE).then((cache) => cache.addAll(APP_SHELL));

const deleteOldCaches = () =>
  caches.keys().then((names) => Promise.all(names.filter((name) => name !== OFFLINE_CACHE).map((name) => caches.delete(name))));

const cachedThenRefreshedFromNetwork = async (request) => {
  const cache = await caches.open(OFFLINE_CACHE);
  const cached = await cache.match(request);
  const refreshed = fetch(request)
    .then((response) => {
      if (response.ok) cache.put(request, response.clone());
      return response;
    })
    .catch(() => cached);
  return cached || refreshed;
};

self.addEventListener("install", (event) => {
  event.waitUntil(cacheAppShell());
  self.skipWaiting();
});

self.addEventListener("activate", (event) => {
  event.waitUntil(deleteOldCaches().then(() => self.clients.claim()));
});

self.addEventListener("fetch", (event) => {
  if (event.request.method !== "GET") return;
  event.respondWith(cachedThenRefreshedFromNetwork(event.request));
});
