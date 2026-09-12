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
      // `ki` is the kit's POSITION in its bank, and that position IS the slot
      // the module will see: `kit build` names each one `{letter}{index}`, so
      // the first kit of bank L is L0 and the second is L1. Carried out here
      // because the slot is the blast radius of a write, and a page that can
      // ask "delete and rewrite" has to be able to say what it would delete.
      (b.kits ?? []).flatMap((k, ki) =>
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
            letter: String(b.letter ?? ""),
            kitIx: ki,
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
export const writeToCard = (dest) => (replace) => () =>
  post("/api/card/write", { dest, replace });
export const placeSet = (req) => () => post("/api/card/place", req);

// A take's envelope, read off the file. Shaped like the daemon's own peaks so
// the page draws a reopened set with exactly the code it draws a live one with.
export const takePeaks = (take) => (buckets) => () =>
  fetch(`/api/take-peaks?take=${encodeURIComponent(take)}&buckets=${buckets}`)
    .then(j)
    .then((d) => ({
      ok: !!d.ok,
      output: String(d.output ?? ""),
      secs: Number(d.secs ?? 0),
      frames: Number(d.frames ?? 0),
      buckets: Number(d.buckets ?? 0),
      lo: (d.lo ?? []).map(Number),
      hi: (d.hi ?? []).map(Number),
    }));

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
// **A stored set, whole**: which take it came from, where its pieces are in
// that take, and when the run fired. Enough to put it back on the bench.
export const loadSet = (name) => () =>
  fetch("/api/sets/" + encodeURIComponent(name))
    .then(j)
    .then((d) => {
      if (!d.ok || !d.set) return { ok: false, output: String(d.output ?? "no such set"), take: "", regions: [], schedule: [], notes: [], struck: [] };
      const s = d.set;
      const regions = (s.samples ?? []).map((x) => ({
        start: Number(x.start ?? 0), end: Number(x.end ?? 0),
        peak: Number(x.peak ?? 0), rms: Number(x.rms ?? 0),
        zcr: Number(x.zcr ?? 0), tilt: Number(x.tilt ?? 0),
      }));
      return {
        ok: regions.length > 0,
        output: regions.length ? "" : `${name} records no regions`,
        take: String(s.take ?? name),
        regions,
        schedule: (s.schedule ?? []).map(Number),
        // What was played into each region, for a set cut from a take somebody
        // performed. Empty per sample on a swept set, and empty throughout on
        // one cut before this was recorded — both of which are the same thing
        // to draw, which is nothing.
        notes: (s.samples ?? []).map((x) => (x.notes ?? []).map(Number)),
        // And the chords each was struck as. A set stored before this existed
        // has none, and the page falls back to reading `notes` as one chord.
        struck: (s.samples ?? []).map((x) =>
          (x.struck ?? []).map((g) => (g ?? []).map(Number))),
      };
    });

// Throw sets away. Plural because the useful gesture is "these four", and a
// loop of single deletes is four chances to stop halfway.
export const deleteSets = (names) => () => post("/api/sets/delete", { names });

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

// **What the owner calls each input.** Keyed by the daemon's own source name,
// which is the wire identity (`--source board=AUDIO4c:1,2`) and stays the
// identity: a stored set records the source it came from by wire name, so
// renaming the label never orphans one. See `sourceLabels` in server.mjs.
export const sourceLabels = () =>
  fetch("/api/sources")
    .then(j)
    .then((d) =>
      Object.entries(d.labels ?? {})
        .map(([wire, label]) => ({ wire: String(wire), label: String(label) }))
        .sort((a, b) => (a.wire < b.wire ? -1 : a.wire > b.wire ? 1 : 0)))
    .catch(() => []);

// One label at a time, merged server-side. An empty label clears it, which is
// how you get back to the wire name without a second control for "forget this".
export const nameSource = (wire) => (label) => () =>
  fetch("/api/sources", {
    method: "PUT",
    headers: { "content-type": "application/json" },
    body: JSON.stringify({ labels: { [wire]: label } }),
  })
    .then(j)
    .then((d) => ({ ok: !!d.ok, output: String(d.output ?? "") }))
    .catch((e) => ({ ok: false, output: String(e.message ?? e) }));
