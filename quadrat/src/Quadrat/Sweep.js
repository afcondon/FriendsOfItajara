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
        values: Array.isArray(q.values) ? q.values.map(Number) : dflt.params[0].values }))
    : dflt.params;
  return { ...dflt, ...stored, params };
};
