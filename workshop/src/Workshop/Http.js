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
      regions: (d.regions ?? []).map((r) => ({
        start: Number(r.start),
        end: Number(r.end),
        // An older msm answers without these. Zero is honest for a missing
        // measurement — the readout divides by the largest it can see, and a
        // set of zeroes reads as "no measurement", not as "silence".
        peak: Number(r.peak ?? 0),
        rms: Number(r.rms ?? 0),
        zcr: Number(r.zcr ?? 0),
      })),
    }))
    .catch((e) => ({ ok: false, output: String(e.message ?? e), secs: 0, divides: false, regions: [] }));

const j = (r) => r.json();

export const card = () =>
  fetch("/api/card").then(j).then((d) => ({
    rows: (d.card?.banks ?? []).flatMap((b) =>
      (b.kits ?? []).flatMap((k) =>
        Object.entries(k.voices ?? {}).map(([v, val]) => {
          // Read both shapes: a voice used to be one set, and is now an
          // ordered stack of layers.
          const st = Array.isArray(val.layers)
            ? val
            : { layers: [{ set: val.set }], kind: val.kind, stereo: val.stereo,
                sliced: !!val.joined, slots: val.slicer ?? 0 };
          const sets = st.layers.map((l) => String(l.set ?? ""));
          return {
            bank: String(b.name ?? ""),
            kit: String(k.name ?? ""),
            voice: Number(v),
            set: sets[0] ?? "",
            sets,
            count: sets.reduce((n, x) => n + Number(d.sets?.[x] ?? 0), 0),
            stereo: !!st.stereo,
            kind: String(st.kind ?? ""),
            slicer: Number(st.slots ?? 0),
            mode: String(k.layers ?? ""),
          };
        }))),
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
