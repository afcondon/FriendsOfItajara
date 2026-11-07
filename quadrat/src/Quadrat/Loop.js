"use strict";

// The host the page came from, so Helm's hub is reached on the same machine
// whether the page was opened at localhost or at the Mini's name.
export const pageHostImpl = () =>
  typeof window !== "undefined" && window.location && window.location.hostname
    ? window.location.hostname
    : "localhost";
