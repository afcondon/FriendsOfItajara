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

// ─────────────────────────────────────────────────────────── what was played
//
// **The notes, for a take nobody swept.**
//
// A swept run knows the pitch it asked for, because it asked. A take played by
// hand knows nothing: the audio arrives on an interface and the chord that
// made it is not in it. So the page listens to MIDI IN for the length of the
// take, and the division afterwards hands each region the notes struck inside
// it — which for a chord set IS the material. Andrew, 2026-09-12: *"i don't
// really care about the names of chords, it's the voicing / pitch-set that
// matters to me because i'm usually looking for very evocative complex and
// interesting chords for somewhat static purposes (pads, ambient)."*
//
// So absolute note numbers, in register, exactly as struck. A pitch-class set
// would throw away the voicing, and the voicing is the thing.
//
// **Note-ons only, and no note-offs.** A pad let ring to silence is defined by
// what was struck together; its length is the decay, which the audio already
// carries and measures better than a key release would. Velocity is dropped
// for the same reason — it is a fact about the performance, not about the
// chord, and the sample records the performance directly.
let heard = [];
const wired = new Set();

// Attached lazily and repeatedly: `access.inputs` is live, so a controller
// switched on after the page loaded appears here on a later pass. Cheap enough
// to call from the poll — it is a set lookup per port.
export const listenMidi = () => {
  if (!access) return;
  for (const i of access.inputs.values()) {
    if (wired.has(i.id)) continue;
    wired.add(i.id);
    const from = i.name;
    i.onmidimessage = (e) => {
      const d = e.data;
      if (!d || d.length < 3) return;
      // Note-on, and a note-on with velocity 0 is a note-off by convention.
      if ((d[0] & 0xf0) !== 0x90 || d[2] === 0) return;
      // **Which port it came from, kept with the note.** Every input is
      // listened to, and exactly one is believed — the page picks. Collecting
      // from all of them is what the first version did, and on a machine whose
      // IAC buses carry the rig's own traffic it swept a sequencer's output
      // into a chord. Kept rather than filtered here so the page can SHOW the
      // ports it is hearing, which is how you notice the wrong one.
      heard.push({ note: d[1], at: e.timeStamp, from, chan: (d[0] & 0x0f) + 1 });
      // A ceiling, because this is held for the life of the page and a page
      // left open all day should not grow without bound.
      if (heard.length > 8192) heard.shift();
    };
  }
};

export const inPorts = () =>
  access ? Array.from(access.inputs.values()).map((i) => i.name) : [];

export const heardNotes = () => heard.slice();

export const forgetHeard = () => {
  heard = [];
};

// **Whether this browser offers Web MIDI here at all**, which is not the same
// question as whether any port is connected.
//
// `requestMIDIAccess` is only defined on a **secure origin**: `https://`, or
// `localhost` / `127.0.0.1`. The Friends server also answers on the machine's
// tailnet name over plain http, and opened that way the whole API is simply
// absent — every port list comes back empty, no prompt appears, and nothing
// anywhere says why. Distinguishing the two is the difference between "plug
// something in" and "open this page by a different name".
export const midiAvailable = () => !!navigator.requestMIDIAccess;

// **Why the port list is empty**, which the list itself cannot say.
//
// "none yet" covers three different situations and the fix differs for each:
// the browser has no Web MIDI at all (not a secure origin), the permission
// prompt has not been answered, or permission is granted and there genuinely
// are no outputs. A dropdown showing the same two words for all three is how
// you end up looking for a text field that was never missing.
export const midiWhy = () => {
  if (!navigator.requestMIDIAccess) return "no Web MIDI on this origin";
  if (!access) return asked ? "waiting for permission…" : "not asked yet";
  return access.outputs.size === 0 ? "no MIDI outputs" : "";
};

// **A phrase on the MIDI subsystem's clock, not the page's.**
//
// `output.send(data, timestamp)` hands the bytes to the browser, which emits
// them at that DOMHighResTimeStamp. The page can then be as late as it likes
// without the phrase drifting — the same move `pulseAt` made for the gate, and
// for the same measured reason: this page is a bad clock.
//
// Note-offs are scheduled the same way rather than held in `setTimeout`, so a
// dense phrase does not accumulate a few hundred pending timers; `hushPort`
// below is what makes that safe.
export const sendPhrase = (m) => () => {
  const out = find(m.port);
  if (!out) return;
  const ch = (m.channel - 1) & 0x0f;
  const t0 = performance.now();
  for (const n of m.notes) {
    const on = t0 + Math.max(0, n.at);
    out.send([0x90 | ch, n.note & 0x7f, n.velocity & 0x7f], on);
    out.send([0x80 | ch, n.note & 0x7f, 0], on + Math.max(1, n.ms));
  }
};

// Drop everything still pending, then end anything already sounding. Both
// halves are needed: `clear()` cannot un-send a note-on that has already gone
// out, and All Notes Off cannot stop one that has not.
export const hushPort = (m) => () => {
  const out = find(m.port);
  if (!out) return;
  const ch = (m.channel - 1) & 0x0f;
  try {
    if (typeof out.clear === "function") out.clear();
  } catch (_) {}
  out.send([0xb0 | ch, 123, 0]);
};
