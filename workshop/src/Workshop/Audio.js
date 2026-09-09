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

export const stopImpl = () => {
  if (el) el.pause();
};
