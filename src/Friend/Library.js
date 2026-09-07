// The library browser's three calls. As with `Http.js`, the wire shape is
// shaped here so that the PureScript side can hold total records: the server
// omits a field it could not read from a header, and a missing number becomes
// a zero the page knows not to print.

const j = (r) => {
  if (!r.ok) throw new Error(`${r.status} ${r.statusText}`);
  return r.json();
};

const s = (v) => (v == null ? "" : String(v));
const n = (v) => (typeof v === "number" && isFinite(v) ? v : 0);
const i = (v) => (typeof v === "number" && isFinite(v) ? Math.round(v) : 0);
const arr = (v) => (Array.isArray(v) ? v : []);

const scene = (c) => ({ path: s(c.path), name: s(c.name), layers: arr(c.layers).map(s) });

const shelf = (h) => ({
  id: s(h.id),
  lib: s(h.lib),
  libName: s(h.libName),
  group: s(h.group),
  name: s(h.name),
  scenes: arr(h.scenes).map(scene),
});

const layer = (l) => ({
  name: s(l.name),
  secs: n(l.secs),
  rate: i(l.rate),
  bits: i(l.bits),
  channels: i(l.channels),
  bytes: n(l.bytes),
});

export const listLibraryImpl = () => fetch("/api/library").then(j).then((sh) => arr(sh).map(shelf));

export const sceneInfoImpl = (lib) => (p) => () =>
  fetch(`/api/scene?lib=${encodeURIComponent(lib)}&path=${encodeURIComponent(p)}`)
    .then(j)
    .then((d) => ({
      layers: arr(d.layers).map(layer),
      texts: arr(d.texts).map((t) => ({ name: s(t.name), content: s(t.content) })),
    }));

export const audioUrl = (lib) => (p) =>
  `/api/audio?lib=${encodeURIComponent(lib)}&path=${encodeURIComponent(p)}`;
