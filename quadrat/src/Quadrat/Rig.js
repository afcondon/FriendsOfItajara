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
// **Ask for a gate at a time, not a gate now.**
//
// The daemon applies this one in its audio callback at `current_frame() +
// delayMs` — the clock the capture is written from. `delayMs` is computed by
// the caller against its own absolute grid, so the page's lateness is
// SUBTRACTED rather than recorded: waking 200 ms late costs nothing if it asks
// for 200 ms less. See `runSweep`.
export const pulseAt = (req) => () =>
  post("/api/cv", { pulseAt: { bus: req.bus, level: req.level, ms: req.ms, delayMs: req.delayMs } });

// The ES-5's own gates: a bit and a length, and the daemon has no duration
// form for them, so `server.mjs` holds it. See its note.
export const es5pulse = (req) => () =>
  post("/api/cv", { es5pulse: { bit: req.bit, ms: req.ms } });

// **Instrumentation for one run, collected here and printed ONCE.**
//
// Never logged per step. A `console.log` in an emit path has jittered this
// rig's scheduler badly enough to read as a hardware fault, which is exactly
// the class of mistake this whole measurement was chasing — so the run pushes
// bare numbers and `dumpMarks` formats them after the last gate has gone.
let marks = [];

export const mark = (r) => () => {
  marks.push(r);
};

export const dumpMarks = () => {
  if (!marks.length) return;
  const f = (v) => String(Math.round(v)).padStart(8);
  console.log("quadrat sweep — ms from t0.  want@ is where the gate was ASKED for;");
  console.log("slip is how late the hand-fired paths landed, and does NOT move a bus gate.");
  console.log("    i     in@    cvRT    want@   ahead   hand@    slip");
  for (const m of marks) {
    console.log(
      "  " + String(m.i).padStart(3) + f(m.inAt) + f(m.cv) +
      f(m.want) + f(m.ahead) + f(m.at) + f(m.at - m.want));
  }
  marks = [];
};

// **A monotonic clock, for pacing the run.**
//
// `runSweep` used to delay the full spacing AFTER doing its sends, so the
// period was sends PLUS spacing: measured 2026-09-11, a run asking for 3000 ms
// stepped every 3125-3136 ms and a "36.0 s" take took 37.6 s. The schedule is
// measured so nothing was WRONG — but the number you typed was not the number
// you got, and the estimate beside it was a 4% lie.
export const nowMs = () => performance.now();
