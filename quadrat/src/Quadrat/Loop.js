"use strict";

// The host the page came from, so Helm's hub is reached on the same machine
// whether the page was opened at localhost or at the Mini's name.
export const pageHostImpl = () =>
  typeof window !== "undefined" && window.location && window.location.hostname
    ? window.location.hostname
    : "localhost";

// MMDD-HHMMSS, the looper's own stamp, so a loop sent from here is named as
// one saved from the looper is.
export const stampImpl = () => {
  const d = new Date(), p = (n) => String(n).padStart(2, "0");
  return p(d.getMonth() + 1) + p(d.getDate()) + "-" + p(d.getHours()) + p(d.getMinutes()) + p(d.getSeconds());
};
