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
        tilt: Number(r.tilt ?? 0),
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
export const placeSet = (req) => () => post("/api/card/place", req);

// The stored sets. See `writeSet` in server.mjs for what one holds and why it
// lives in the sample directory rather than beside it.
export const storedSets = () =>
  fetch("/api/sets").then(j).then((d) => ({
    ok: !!d.ok,
    sets: (d.sets ?? []).map((s) => ({
      name: String(s.name ?? ""),
      count: Number(s.count ?? 0),
      made: String(s.made ?? ""),
      take: String(s.take ?? ""),
      described: !!s.described,
      runnable: !!s.runnable,
      moved: (s.moved ?? []).map(String),
      extent: (s.extent ?? []).map(Number),
      encoding: String(s.encoding ?? ""),
    })),
  })).catch(() => ({ ok: false, sets: [] }));

// One set's spec, raw. The merge over the current default belongs to
// `Quadrat.Sweep`, which owns the shape — and an FFI module cannot import
// another module's FFI, because spago writes each one to its own directory in
// `output/`. So this fetches and `Sweep.adopt` merges.
export const loadSpec = (name) => () =>
  fetch("/api/sets/" + encodeURIComponent(name))
    .then(j)
    .then((d) => {
      if (!d.ok) return { ok: false, output: String(d.output ?? "no such set"), spec: {} };
      if (!d.set?.spec) {
        return {
          ok: false,
          spec: {},
          output: `${name} was cut from a take that was played, not run — `
            + `there is no spec to run again`,
        };
      }
      return { ok: true, output: "", spec: d.set.spec };
    })
    .catch((e) => ({ ok: false, output: String(e.message ?? e), spec: {} }));

// The rig doctor's calibration tables, through this page's own server (which
// relays :3027). A failed fetch comes back as `ok:false` with the reason rather
// than as an empty list: "no tables" and "deepstar is not running" look the
// same in a dropdown and are different problems.
export const calibrations = () =>
  fetch("/api/calibrations").then(j).then((d) => ({
    ok: !!d.ok,
    tables: (d.tables ?? []).map((t) => ({
      label: String(t.label ?? ""),
      module: String(t.module ?? ""),
      points: Number(t.points ?? 0),
      loHz: Number(t.loHz ?? 0),
      hiHz: Number(t.hiHz ?? 0),
      voltsPerOctave: Number(t.voltsPerOctave ?? 0),
    })),
  })).catch((e) => ({ ok: false, tables: [] }));

export const calibration = (label) => () =>
  fetch("/api/calibrations/" + encodeURIComponent(label)).then(j).then((d) => ({
    ok: !!d.ok,
    label: String(d.label ?? label),
    module: String(d.module ?? ""),
    coarse: String(d.coarse ?? ""),
    measuredAt: String(d.measuredAt ?? ""),
    // Rebuilt point by point rather than passed through: PureScript will read
    // these as Numbers and a NaN here becomes a NaN voltage at the module.
    points: (d.points ?? [])
      .filter((p) => Number.isFinite(Number(p.volts)) && Number.isFinite(Number(p.hz)))
      .map((p) => ({ volts: Number(p.volts), hz: Number(p.hz) })),
    error: String(d.error ?? ""),
  })).catch((e) => ({ ok: false, label, module: "", coarse: "", measuredAt: "", points: [], error: String(e.message ?? e) }));
