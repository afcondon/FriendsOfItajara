// One call. The JS owns the wire shape so the PureScript can hold a record
// with no `Maybe` in it: a failure comes back as `ok: false` with a sentence
// in `output`, which is what the page shows either way.
export const divisions = (take) => (as) => (quiet) => () =>
  fetch("/api/onsets", {
    method: "POST",
    headers: { "content-type": "application/json" },
    body: JSON.stringify({ take, as, quiet }),
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
