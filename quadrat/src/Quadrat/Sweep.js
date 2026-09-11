// The plan, across a reload. One key, one object, and the default as the base
// so that a plan written by an older build comes back usable rather than
// half-formed.
//
// Every access is wrapped: localStorage throws outright in a private window and
// in some embedded contexts, and a page that will not start because it could
// not remember a slider position is worse than one that forgets.
const KEY = "workshop.sweep.v1";

export const savePlain = (p) => () => {
  try {
    localStorage.setItem(KEY, JSON.stringify(p));
  } catch (_) {}
};

export const loadPlain = (dflt) => () => {
  let stored = null;
  try {
    const s = localStorage.getItem(KEY);
    stored = s ? JSON.parse(s) : null;
  } catch (_) {}
  return adoptPlain(dflt)(stored);
};

// **A stored plan, over the current default.**
//
// Field-wise, and the same one level down for a parameter, so a field added
// since the plan was written arrives with its default rather than as undefined
// — which PureScript would carry as a Number and render as an empty box nobody
// could explain.
//
// Exported (as `Sweep.adopt`) because a plan reaches this page from two places
// and they must agree: `localStorage`, where it survives a reload, and a
// **stored sample set**, where it survives everything. One merge, so a field
// added later cannot arrive as a default down one path and as undefined down
// the other.
export const adoptPlain = (dflt) => (stored) => {
  if (!stored || typeof stored !== "object") return dflt;
  const params = Array.isArray(stored.params) && stored.params.length
    ? stored.params.map((q) => ({ ...dflt.params[0], ...q,
        values: Array.isArray(q.values) ? q.values.map(Number) : dflt.params[0].values,
        // A calibration table gets the same guard as `values`, and needs it
        // more: PureScript will read these as `{ volts :: Number, hz :: Number }`
        // and a malformed entry becomes a NaN voltage sent to a module. Each
        // point is rebuilt rather than trusted, so a half-written table is
        // dropped entirely — `unflatten` then restores the parameter as an
        // ordinary one, losing the label rather than the tuning.
        pitchTable: Array.isArray(q.pitchTable)
          ? q.pitchTable
              .filter((r) => r && Number.isFinite(Number(r.volts)) && Number.isFinite(Number(r.hz)))
              .map((r) => ({ volts: Number(r.volts), hz: Number(r.hz) }))
          : [] }))
    : dflt.params;
  return { ...dflt, ...stored, params };
};

// **The schedule, across a reload — keyed by the take it belongs to.**
//
// The plan survives a reload and the schedule did not, which quietly downgraded
// a transect to a guess: with no schedule the page falls back to the detector,
// and dividing a take into N EQUAL pieces is only right when the recording ends
// exactly at the last hit. It never does — there is always a tail. Measured on
// 2026-09-11: a 12-cell run at 3000 ms filled 38.28 s, so equal division gave
// 3.190 s bands against a 3.000 s schedule, every band 190 ms too long and the
// twelfth 2.09 s adrift.
//
// A schedule is MEASURED — the trigger times as they actually happened — so it
// cannot be recomputed from the plan, only kept. Keyed by take name because
// that is what it describes: a schedule belonging to some other take is worse
// than none.
const RUN_KEY = "quadrat.run.v1";

export const saveRun = (r) => () => {
  try {
    localStorage.setItem(RUN_KEY, JSON.stringify(r));
  } catch (_) {}
};

export const loadRun = () => {
  try {
    const s = localStorage.getItem(RUN_KEY);
    const r = s ? JSON.parse(s) : null;
    if (r && typeof r.take === "string" && Array.isArray(r.schedule)) {
      return { take: r.take, schedule: r.schedule.filter((n) => Number.isFinite(n)) };
    }
  } catch (_) {}
  return { take: "", schedule: [] };
};
