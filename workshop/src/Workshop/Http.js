// One call. The JS owns the wire shape so the PureScript can hold a record
// with no `Maybe` in it: a failure comes back as `ok: false` with a sentence
// in `output`, which is what the page shows either way.
export const divisions = (req) => () =>
  fetch("/api/onsets", {
    method: "POST",
    headers: { "content-type": "application/json" },
    body: JSON.stringify(req),
  })
    .then((r) => r.json())
    .then((d) => ({
      ok: !!d.ok,
      output: String(d.output ?? ""),
      secs: Number(d.secs ?? 0),
      divides: !!d.divides,
      regions: (d.regions ?? []).map((r) => ({ start: Number(r.start), end: Number(r.end) })),
    }))
    .catch((e) => ({ ok: false, output: String(e.message ?? e), secs: 0, divides: false, regions: [] }));

const j = (r) => r.json();

export const card = () =>
  fetch("/api/card").then(j).then((d) => ({
    rows: (d.card?.banks ?? []).flatMap((b) =>
      (b.kits ?? []).flatMap((k) =>
        Object.entries(k.voices ?? {}).map(([v, val]) => ({
          bank: String(b.name ?? ""),
          kit: String(k.name ?? ""),
          voice: Number(v),
          set: String(val.set ?? ""),
          count: Number(d.sets?.[val.set] ?? 0),
          stereo: !!val.stereo,
          kind: String(val.kind ?? ""),
          slicer: Number(val.slicer ?? 0),
        })))),
    cards: d.cards ?? [],
    plan: String(d.plan ?? ""),
    ok: !!d.ok,
  })).catch((e) => ({ rows: [], cards: [], plan: String(e.message ?? e), ok: false }));

const post = (url, body) =>
  fetch(url, { method: "POST", headers: { "content-type": "application/json" }, body: JSON.stringify(body) })
    .then(j)
    .then((d) => ({ ok: !!d.ok, output: String(d.output ?? "") }))
    .catch((e) => ({ ok: false, output: String(e.message ?? e) }));

export const addToCard = (req) => () => post("/api/card/add", req);
export const writeToCard = (dest) => () => post("/api/card/write", { dest });
