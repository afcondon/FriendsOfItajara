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

export const storeBase = () => base();

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
    byAt.get(k).push(e);
  }
  return Array.from(byAt.keys())
    .sort((a, b) => a - b)
    .map((at) => ({
      at: at / 1000000.0,
      notes: byAt.get(at).map((e) => e.pitch).sort((a, b) => a - b),
      // **How long the chord is HELD**, which the declaration knows and a
      // trigger's hold does not. A drum's gate is a 10 ms blip and right for a
      // drum; a chord held for 10 ms on a sustained patch barely speaks.
      gate: Math.max.apply(null, byAt.get(at).map((e) => e.gateMs || 0)) || 0,
      // The loudest voice of the group. A chord machine sends one velocity for
      // all of them; a phrase's groups are mostly single notes, so this is that
      // note's own.
      vel: Math.max.apply(null, byAt.get(at).map((e) => e.vel || 0)) || 0,
    }));
};

// One published clip, from what the store holds for it: the content's hash and
// payload, and its first label's name and tags. Null when the payload is not a
// clip with events. The asking is PureScript, over the store's described
// operations (Amphora.Api); this is only the reading of a clip.
export const declaredOf = (h, payload, labelName, tags) => {
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
    // The publisher's name. A Vetula progression's is its own (given
    // when it was started) and differs from its chord rebus; Odonus
    // names a clip by its rebus.
    name: labelName || clip.name || tag(tags, "rebus:") || h.slice(0, 8),
    source: tag(tags, "source:") || clip.source || "",
    kind: tag(tags, "kind:"),
    rebus: tag(tags, "rebus:"),
    key: tag(tags, "key:") || clip.key || "",
    // The chords (from zero) whose notes leave that key, as Vetula
    // checked them. Absent means none did, or nobody checked.
    outside: tag(tags, "outside:")
      .split(",")
      .filter((x) => x !== "")
      .map((x) => parseInt(x, 10))
      .filter((n) => !isNaN(n)),
    onsets: groups.map((g) => g.at),
    chords: groups.map((g) => g.notes),
    gates: groups.map((g) => g.gate),
    vels: groups.map((g) => g.vel),
  };
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

// **The chosen progression, remembered across a reload.**
//
// `against` is page state and a reload drops it, which silently turns a
// declared run back into a single-note one — and during development there are
// a great many reloads. The hash alone: the clip itself is re-fetched, so this
// is a pointer and never a stale copy.
const PICK = "quadrat.declared.pick";

export const rememberPick = (hash) => () => {
  try {
    if (hash) window.localStorage.setItem(PICK, hash);
    else window.localStorage.removeItem(PICK);
  } catch (e) {}
};

export const recallPick = () => {
  try {
    return window.localStorage.getItem(PICK) || "";
  } catch (e) {
    return "";
  }
};
