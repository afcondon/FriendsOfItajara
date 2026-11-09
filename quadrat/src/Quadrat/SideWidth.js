// The side drawer's width, per page, kept across reloads. Whether it is open
// is not kept: a page loads closed, as Triggerfish's drawers do.
export const loadWidth = (key) => (fallback) => () => {
  try {
    const v = Number(localStorage.getItem("quadrat.side." + key));
    return Number.isFinite(v) && v > 0 ? v : fallback;
  } catch (_) { return fallback; }
};
export const saveWidth = (key) => (w) => () => {
  try { localStorage.setItem("quadrat.side." + key, String(w)); } catch (_) {}
};
