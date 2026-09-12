// Previewing a sub-sample is a RANGE of one file, not a file of its own.
// Cutting them on the server would be dozens of writes to answer a hover, and
// the browser can already start and stop inside a file it has loaded once.
let el = null;
let stopAt = 0;
let loaded = "";

const ensure = (src) => {
  if (!el) {
    el = new Audio();
    el.preload = "auto";
    el.addEventListener("timeupdate", () => {
      // A range ends where it ends. Without this the preview runs on into the
      // next hit, which makes every tile sound like the one after it.
      if (stopAt > 0 && el.currentTime >= stopAt) el.pause();
    });
  }
  if (loaded !== src) {
    el.src = src;
    loaded = src;
  }
  return el;
};

export const playRangeImpl = (src, from, to) => {
  const a = ensure(src);
  stopAt = to;
  try {
    a.currentTime = from;
    // A play() interrupted by the next hover rejects; that is the normal case
    // when someone sweeps across the grid, and it is not an error.
    a.play().catch(() => {});
  } catch (_) {
    // Seeking before the metadata has arrived throws. Wait for it once.
    a.addEventListener("loadedmetadata", () => {
      a.currentTime = from;
      a.play().catch(() => {});
    }, { once: true });
  }
};

// **Several whole files, one after another.** A set is many files where a
// take is one, so auditioning it is a queue rather than a range — and the
// point of auditioning is to answer "which one is this?" before doing
// something irreversible to it, so each piece is capped: three seconds of
// three samples tells you, and thirty seconds of forty-eight does not.
let queue = [];
let cap = 0;

const next = () => {
  if (!queue.length) return;
  const src = queue.shift();
  const a = ensure(src);
  stopAt = 0;
  const onEnd = () => { a.removeEventListener("ended", onEnd); next(); };
  a.addEventListener("ended", onEnd, { once: true });
  try {
    a.currentTime = 0;
    a.play().catch(() => {});
  } catch (_) {
    a.addEventListener("loadedmetadata", () => { a.currentTime = 0; a.play().catch(() => {}); }, { once: true });
  }
  if (cap > 0) {
    setTimeout(() => {
      // Only move on if this is still the one playing; a new audition that
      // arrived in the meantime has already taken over.
      if (loaded === src && !a.paused) { a.pause(); next(); }
    }, cap * 1000);
  }
};

export const playEachImpl = (srcs, maxSecs) => {
  queue = srcs.slice();
  cap = maxSecs;
  if (el) el.pause();
  next();
};

export const stopImpl = () => {
  queue = [];
  if (el) el.pause();
};
