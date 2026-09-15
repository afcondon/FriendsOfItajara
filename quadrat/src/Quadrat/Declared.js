"use strict";

// Amphora, the shared artefact store. Follows the page host so this works at
// localhost and at andrews-mac-mini alike; it listens on :3024 with permissive
// CORS, which is why a clip can cross from Triggerfish (:3023) to here (:3029)
// at all — localStorage cannot, being per-origin.
const base = () => {
  const h =
    typeof window !== "undefined" && window.location && window.location.hostname
      ? window.location.hostname
      : "localhost";
  const p =
    typeof window !== "undefined" && window.location && window.location.protocol === "https:"
      ? "https:"
      : "http:";
  return p + "//" + h + ":3024";
};

// Fail fast rather than hang: a store that is down should leave the page saying
// "nothing declared" in a second or two, not pending until the browser gives up.
const TIMEOUT_MS = 2500;

const getJSON = (url) => {
  const c = new AbortController();
  const t = setTimeout(() => c.abort(), TIMEOUT_MS);
  return fetch(url, { signal: c.signal })
    .then((r) => {
      if (!r.ok) throw new Error("GET " + url + " -> " + r.status);
      return r.json();
    })
    .finally(() => clearTimeout(t));
};

const tag = (tags, prefix) => {
  const hit = (tags || []).find((t) => t.indexOf(prefix) === 0);
  return hit ? hit.slice(prefix.length) : "";
};

// **Chords, from the events' own onsets.** Notes sharing an instant are one
// chord — which is the same clustering the page does on overheard MIDI, except
// that these times were DECLARED by whatever played them, so the grouping is
// exact rather than a 50 ms guess about a human hand.
const chordsOf = (events) => {
  const byAt = new Map();
  for (const e of events || []) {
    const k = e.fireUnixMicros;
    if (!byAt.has(k)) byAt.set(k, []);
    byAt.get(k).push(e.pitch);
  }
  return Array.from(byAt.keys())
    .sort((a, b) => a - b)
    .map((at) => ({ at: at / 1000000.0, notes: byAt.get(at).slice().sort((a, b) => a - b) }));
};

export const fetchDeclaredImpl = (onError) => (onSuccess) => () => {
  const b = base();
  getJSON(b + "/favorites?collection=triggerfish-clips")
    .then((favs) => {
      const hashes = (favs || []).map((f) => f.contentHash).filter(Boolean);
      return Promise.all(
        hashes.map((h) =>
          Promise.all([
            getJSON(b + "/content/" + h).then((c) => (c && c.payload) || null),
            getJSON(b + "/labels?hash=" + h).then((ls) => (ls && ls[0]) || null),
          ]).then(([payload, label]) => {
            if (typeof payload !== "string" || !label) return null;
            let clip;
            try {
              clip = JSON.parse(payload);
            } catch (e) {
              return null;
            }
            if (!clip || !Array.isArray(clip.events) || clip.events.length === 0) return null;
            const groups = chordsOf(clip.events);
            return {
              hash: h,
              name: label.name || clip.name || h.slice(0, 8),
              source: tag(label.tags, "source:") || clip.source || "",
              kind: tag(label.tags, "kind:"),
              rebus: tag(label.tags, "rebus:"),
              key: tag(label.tags, "key:") || clip.key || "",
              onsets: groups.map((g) => g.at),
              chords: groups.map((g) => g.notes),
            };
          })
        )
      );
    })
    .then((xs) => onSuccess(xs.filter(Boolean))())
    .catch((e) => onError(e instanceof Error ? e : new Error(String(e)))());
};

// **Has the page just come back to the foreground?** Latches on
// `visibilitychange`, and the caller clears it by asking. A poll that asked the
// store every tick would be a request a second for a list that changes when a
// human presses a button in another window; a poll that never asks makes you
// reload the page to see what you just published. Coming back to the tab is
// exactly the moment the answer might have changed.
let returned = false;
if (typeof document !== "undefined") {
  document.addEventListener("visibilitychange", () => {
    if (!document.hidden) returned = true;
  });
  if (typeof window !== "undefined") window.addEventListener("focus", () => { returned = true; });
}

export const cameBackImpl = () => {
  const r = returned;
  returned = false;
  return r;
};
