// WebMIDI, held open for the life of the page. `requestMIDIAccess` prompts the
// first time and resolves instantly afterwards, but it is still a promise, and
// a sweep cannot afford to discover that mid-run — so it is asked for once,
// when the modal opens, and the ports are shown before anything is sent.
let access = null;
let asked = false;

const names = () =>
  access ? Array.from(access.outputs.values()).map((o) => o.name) : [];

// **This must never block the page.**
//
// `requestMIDIAccess` does not resolve until the permission prompt is answered,
// and the prompt lives in browser chrome where the page cannot see it. Awaited
// directly from a Halogen handler that stalls the whole action queue: the page
// stops polling the daemon and looks dead, and there is nothing on it to say
// why — the dialog asking the question is above the window.
//
// So the request is fired once and never waited on. This resolves with whatever
// is known within a moment; `ports` below is read again on every poll while the
// modal is open, so the list fills in the instant permission is granted.
export const openMidi = () => {
  if (access) return Promise.resolve(names());
  if (!navigator.requestMIDIAccess) return Promise.resolve([]);
  if (!asked) {
    asked = true;
    navigator
      .requestMIDIAccess({ sysex: false })
      .then((a) => {
        access = a;
      })
      .catch(() => {});
  }
  return new Promise((res) => {
    const t0 = Date.now();
    const tick = () =>
      access || Date.now() - t0 > 1200 ? res(names()) : setTimeout(tick, 100);
    tick();
  });
};

// What is known right now, without asking anything. Safe to call every tick.
export const ports = () => names();

// Substring, not equality: port names carry the interface's own spelling and
// the useful thing to type is "IAC" or "ES-9".
const find = (port) => {
  if (!access || !port) return null;
  for (const o of access.outputs.values()) if (o.name.includes(port)) return o;
  return null;
};

export const sendCc = (m) => () => {
  const out = find(m.port);
  if (!out) return;
  out.send([0xb0 | ((m.channel - 1) & 0x0f), m.cc & 0x7f, m.value & 0x7f]);
};

export const sendNote = (m) => () => {
  const out = find(m.port);
  if (!out) return;
  const ch = (m.channel - 1) & 0x0f;
  out.send([0x90 | ch, m.note & 0x7f, m.velocity & 0x7f]);
  // The note-off is scheduled here rather than sent with a timestamp so that
  // closing the page cannot leave a note held down on the rig.
  setTimeout(() => {
    try {
      out.send([0x80 | ch, m.note & 0x7f, 0]);
    } catch (_) {}
  }, Math.max(1, m.ms));
};

const post = (url, body) =>
  fetch(url, {
    method: "POST",
    headers: { "content-type": "application/json" },
    body: JSON.stringify(body),
  })
    .then((r) => r.json())
    .then((d) => ({ ok: !!d.ok, output: String(d.output ?? "") }))
    .catch((e) => ({ ok: false, output: String(e.message ?? e) }));

export const setCv = (req) => () => post("/api/cv", { set: req.set, esx: req.esx ?? [] });
export const pulse = (req) => () =>
  post("/api/cv", { pulse: { bus: req.bus, level: req.level, ms: req.ms } });
// The ES-5's own gates: a bit and a length, and the daemon has no duration
// form for them, so `server.mjs` holds it. See its note.
export const es5pulse = (req) => () =>
  post("/api/cv", { es5pulse: { bit: req.bit, ms: req.ms } });

// **A monotonic clock, for pacing the run.**
//
// `runSweep` used to delay the full spacing AFTER doing its sends, so the
// period was sends PLUS spacing: measured 2026-09-11, a run asking for 3000 ms
// stepped every 3125-3136 ms and a "36.0 s" take took 37.6 s. The schedule is
// measured so nothing was WRONG — but the number you typed was not the number
// you got, and the estimate beside it was a 4% lie.
export const nowMs = () => performance.now();
