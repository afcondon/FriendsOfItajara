// The Friend's server: the static page, and the two things a browser cannot
// do for itself — write a take's notes to disk, and run `msm harvest`.
//
// Zero dependencies and one file on purpose. The daemon must not spawn a
// process (audio thread, and shaping is msm's job); the page cannot; so this
// is the seam, and it is deliberately as small as a seam can be. Node 18+.
//
//   node server.mjs            serves ./static on :3029
//   PORT=3030 MSM=/path/msm    override the port and the msm binary
//
//   GET  /api/takes                the takes under ~/.itajara/takes, newest first
//   GET  /api/sources              what the owner calls each of the daemon's inputs
//   PUT  /api/sources              { labels: { wireName: label } }, merged
//   GET  /api/takes/:name/notes    the take's notes.json, or {}
//   PUT  /api/takes/:name/notes    write it
//   GET  /api/sticks               mounted volumes that look like an Arbhar stick
//   GET  /api/library              every library's shelves and scenes, names only
//   GET  /api/scene?lib=&path=     one scene: its audio with headers, its texts
//   GET  /api/audio?lib=&path=     one file, with byte ranges, for auditioning
//   POST /api/harvest              { take, module, stick, bank, scene, card, slot,
//                                    as, overwrite, allLayers, dryRun }
//   GET  /api/card/preview?dest=   what a write to that mounted card would do:
//                                  every slot created, replaced or left alone
//                                  → runs msm harvest,
//                                    answers { ok, output }
//   POST /api/card/place           { set, bank, letter, kit, voice, append,
//                                    layerMode, layers } — `layers` is the
//                                    arrangement: how many alternatives the
//                                    module picks between. Absent means the
//                                    sweep's own extent decides.
//                                  → put a set ALREADY ON DISK onto a voice
//   GET  /api/sets                 every stored sample set, newest first
//   GET  /api/sets/:name           one set whole: spec, schedule, measurements,
//                                  and what each sample meant
//   POST /api/cv                   { set: [{bus, level}], esx: [{slot, level}] }
//                                  | { pulse: {bus, level, ms} }
//                                  | { pulseAt: {bus, level, ms, delayMs} }
//                                  | { es5pulse: {bit, ms} }
//                                  → OSC to es9-daemon. See the note above it:
//                                    this reports what was SENT, never what
//                                    arrived.

import http from "node:http";
import fs from "node:fs";
import path from "node:path";
import os from "node:os";
import dgram from "node:dgram";
import { spawn } from "node:child_process";

const PORT = Number(process.env.PORT || 3029);
const MSM = process.env.MSM || "msm";
// The looper binary, for the pre-flight only — never to start anything.
// Beside us by the same sibling-repo convention the client packages use.
const ITAJARA = process.env.ITAJARA
  || path.resolve(process.cwd(), "../itajara/daemon/target/release/itajara");
const STATIC = path.join(path.dirname(new URL(import.meta.url).pathname), "static");
const TAKES = path.join(os.homedir(), ".itajara", "takes");

const TYPES = { ".html": "text/html; charset=utf-8", ".js": "text/javascript", ".css": "text/css", ".json": "application/json", ".svg": "image/svg+xml" };

// A take name that cannot leave the takes directory: the same rule the daemon
// applies, so the name the page sends is the folder the daemon made.
const safe = (s) => String(s).replace(/[^A-Za-z0-9_-]/g, "_") || "take";

function json(res, status, body) {
  res.writeHead(status, { "content-type": "application/json" });
  res.end(JSON.stringify(body));
}

function readBody(req) {
  return new Promise((resolve, reject) => {
    let data = "";
    req.on("data", (c) => (data += c));
    req.on("end", () => {
      try { resolve(data ? JSON.parse(data) : {}); } catch (e) { reject(e); }
    });
    req.on("error", reject);
  });
}

function takes() {
  if (!fs.existsSync(TAKES)) return [];
  return fs.readdirSync(TAKES)
    .filter((n) => fs.existsSync(path.join(TAKES, n, "export.json")))
    .map((n) => {
      const st = fs.statSync(path.join(TAKES, n, "export.json"));
      let loops = 0, kind = "";
      try {
        const m = JSON.parse(fs.readFileSync(path.join(TAKES, n, "export.json"), "utf8"));
        loops = m.loops.length;
        kind = m.kind;
      } catch {}
      return { name: n, kind, savedAt: st.mtimeMs, loops, harvested: fs.existsSync(path.join(TAKES, n, "datasheet.json")) };
    })
    // Only what a harvest can read: a flat `ex` set has no layers.
    .filter((t) => t.kind === "layers")
    .sort((a, b) => b.savedAt - a.savedAt);
}

function sticks() {
  const vols = "/Volumes";
  if (!fs.existsSync(vols)) return [];
  return fs.readdirSync(vols)
    .map((n) => path.join(vols, n))
    .filter((p) => { try { return fs.statSync(path.join(p, "_arbhar_library")).isDirectory(); } catch { return false; } });
}

// ===========================================================================
// The virtual card.
//
// **A card is compiled, never edited in place.** What Quadrat holds is a
// description — which sample sets are on which voices of which kits — and
// `msm kit build` turns that into a card. So nothing reaches the SD card that
// you could not have read first, and the three rules the module fails silently
// on (twelve layers, byte-sort order, a kit with no voice 1) stay enforced in
// exactly one place.
//
// The description is JSON here because this is Node, and the manifest TOML
// that `kit build` reads is GENERATED from it at write time. The TOML is still
// the thing the compiler consumes and the thing you can keep; it is simply not
// the thing a web form edits.
// **Quadrat's own corner of the daemon's directory.** Named `workshop` until
// 2026-09-10, when the page it serves got a name of its own — a quadrat is the
// square frame a field survey lays down and records everything inside, which
// is what a sweep does to a module, and the two destinations it targets have
// four voices each.
const SHOP = path.join(os.homedir(), ".itajara", "quadrat");
const CARD_JSON = path.join(SHOP, "card.json");
const CARD_TOML = path.join(SHOP, "card.toml");
const SAMPLES = path.join(SHOP, "samples");

const emptyCard = () => ({ name: "Quadrat", banks: [] });

function readCard() {
  try {
    return JSON.parse(fs.readFileSync(CARD_JSON, "utf8"));
  } catch {
    return emptyCard();
  }
}

function writeCard(card) {
  fs.mkdirSync(SHOP, { recursive: true });
  fs.writeFileSync(CARD_JSON, JSON.stringify(card, null, 2) + "\n");
  fs.writeFileSync(CARD_TOML, cardToml(card));
  return card;
}

// TOML by hand, and quoted rather than escaped: these are names, and a name
// with a quote in it is a name to fix.
const q = (v) => `"${String(v).replace(/["\\\n]/g, " ").trim()}"`;

// **A voice is a stack of layers, and each layer may itself be sliced.**
//
// The module has two axes and they are not interchangeable. The layer selector
// picks between alternatives and can be driven by the module itself — by
// velocity, at random, cyclically — while the start point indexes positions
// and only ever does what it is told. A sampled instrument is both at once:
// four velocity layers, each a file of twelve notes.
//
// The first version of this card could hold one set per voice and so could
// reach one axis at a time. `layers` is the second.
function asStack(v) {
  if (!v) return null;
  if (Array.isArray(v.layers)) return v;
  // The one-set shape this replaced, read forward so an existing card still
  // opens. Written back in the new shape the next time it is touched.
  return {
    layers: [{ set: v.set }],
    kind: v.kind || "",
    stereo: !!v.stereo,
    sliced: !!v.joined,
    slots: v.slicer || 0,
    slotSecs: 0,
  };
}

// What a layer's files are, as the manifest wants them.
//
// **A layer is usually a whole set, and on a grid it is a slice of one.** Four
// velocity layers of twelve notes each are ONE recording: the outer axis picks
// the layer, the inner becomes the slices inside it, and both live in the same
// directory because that is what a transect is. So a layer may name its own
// files rather than globbing the set — and carry its own slot length, because
// the four decays of a 4 x 12 are four different lengths and SLICER divides
// each file by proportion. What has to agree across a voice is the slot COUNT,
// which is the global setting, and that is checked where banks are.
function layerSource(st, l) {
  const glob = `samples/${l.set}/*.wav`;
  const srcs = (l.files && l.files.length)
    ? l.files.map((f) => q(`samples/${l.set}/${f}`))
    : [q(glob)];
  if (!st.sliced) {
    // **An unsliced layer used to be a bare path, and a path says nothing.**
    //
    // The meaning fields below were only ever reachable through the `concat`
    // form, so a stack of twelve WHOLE chord samples — the shape a chord set
    // takes, and the one that was written to Q0 — recorded which file sat on
    // which layer and never which chord it was. The index could then say how
    // to SELECT layer 7 and not what layer 7 sounds, which is half an
    // instrument: enough to play the card, not enough to play FROM it.
    //
    // `msm`'s `file` form is the same single file with room to say so. Still a
    // bare path when there is nothing to say, because a manifest is read by
    // people too and `{ file = "x.wav" }` is worse than `"x.wav"`.
    if (srcs.length !== 1 || (!l.notes && l.velocity == null)) {
      return srcs.length === 1 ? srcs[0] : `[${srcs.join(", ")}]`;
    }
    const bits = [`file = ${srcs[0]}`];
    if (l.notes && l.notes.length) bits.push(`notes = [${l.notes.join(", ")}]`);
    if (l.velocity != null) bits.push(`velocity = ${l.velocity}`);
    if (l.name) bits.push(`name = ${q(l.name)}`);
    return `{ ${bits.join(", ")} }`;
  }
  const slot = l.slotSecs != null ? l.slotSecs : st.slotSecs;
  const bits = [`concat = [${srcs.join(", ")}]`, `slot = ${Number(slot).toFixed(4)}`];
  if (st.slots) bits.push(`slots = ${st.slots}`);
  bits.push(`name = ${q(l.name || l.set)}`);
  // What the slots and the layer MEAN. The card records where a sample sits
  // and never what it is, so without these a file of twelve slices is
  // indistinguishable from a field recording by any amount of analysis.
  if (l.velocity != null) bits.push(`velocity = ${l.velocity}`);
  if (l.pitch != null) bits.push(`pitch = ${l.pitch}`);
  // A join whose slots are chords: one array per slot, in slot order. The
  // other way a chord set lands, and not expressible as `pitches`, which is
  // one name per slot and so cannot hold four notes.
  if (l.slotNotes && l.slotNotes.length) {
    bits.push(`slot_notes = [${l.slotNotes.map((ns) => `[${ns.join(", ")}]`).join(", ")}]`);
  }
  return `{ ${bits.join(", ")} }`;
}

function cardToml(card) {
  let t = "# Generated by Quadrat. `msm kit build` reads this.\n";
  t += "# Editing it by hand is fine; the page will overwrite it next time.\n\n";
  t += `name = ${q(card.name || "Quadrat")}\n\n`;
  for (const b of card.banks || []) {
    t += "[[bank]]\n";
    if (b.letter) t += `letter = ${q(b.letter)}\n`;
    t += `name = ${q(b.name)}\n`;
    if (b.kind) t += `kind = ${q(b.kind)}\n`;
    if (b.slicer) t += `slicer = ${b.slicer}\n`;
    t += "\n";
    for (const k of b.kits || []) {
      t += "[[bank.kit]]\n";
      t += `name = ${q(k.name)}\n`;
      if (k.layers) t += `layers = ${q(k.layers)}\n`;
      for (const [v, val] of Object.entries(k.voices || {})) {
        const st = asStack(val);
        const srcs = st.layers.map((l) => layerSource(st, l));
        // One layer is written bare; several become the array form, which is
        // what makes a two-axis instrument expressible at all.
        t += srcs.length === 1
          ? `voice${v} = ${srcs[0]}\n`
          : `voice${v} = [\n${srcs.map((x) => "  " + x).join(",\n")}\n]\n`;
      }
      t += "\n";
    }
  }
  return t;
}

// Mounted Rample cards: the firmware, or at least one kit folder. Same rule
// `msm` uses, so the page and the compiler agree about what a card is.
// **A take's envelope, drawn from the file rather than from the daemon.**
//
// The Bench draws bands over the capture the daemon is holding, which means a
// set can be recorded, measured, saved — and then never looked at again, and
// any stray capture leaves the bands describing one recording and the picture
// another. Reading the envelope off disk makes a stored set re-openable, which
// is the whole of what `set.json` was for.
//
// Min and max per bucket, not rms: a waveform is an outline and the thing you
// are looking for in it is the attack, which an rms would smooth away. The
// shape matches `Socket.Peaks` so the page draws it with the same code.
function takePeaks(name, buckets) {
  const dir = path.join(TAKES, safe(name));
  if (!name || !fs.existsSync(dir)) return { ok: false, output: `no take called ${name || "(none)"}` };
  const wav = firstWav(dir);
  if (!wav) return { ok: false, output: `${name} holds no audio` };

  const buf = fs.readFileSync(wav);
  let off = 12, rate = 0, ch = 0, bits = 0, tag = 0, dOff = 0, dLen = 0;
  while (off + 8 <= buf.length) {
    const id = buf.slice(off, off + 4).toString();
    const size = buf.readUInt32LE(off + 4);
    if (id === "fmt ") {
      tag = buf.readUInt16LE(off + 8); ch = buf.readUInt16LE(off + 10);
      rate = buf.readUInt32LE(off + 12); bits = buf.readUInt16LE(off + 22);
    } else if (id === "data") { dOff = off + 8; dLen = size; break; }
    off += 8 + size + (size & 1);
  }
  if (!rate || !ch || !dLen) return { ok: false, output: `${name}: unreadable header` };
  const wide = bits / 8;
  const frames = Math.floor(dLen / (ch * wide));
  const n = Math.max(1, Math.min(8000, buckets | 0 || 2000));
  const per = Math.max(1, Math.floor(frames / n));
  // The daemon's own peaks are 16-bit ints and the page scales by 32768, so
  // match that rather than teaching the drawing code a second unit.
  const read = (i) => {
    const at = dOff + i * ch * wide;
    if (tag === 3 && bits === 32) return buf.readFloatLE(at);
    if (tag === 1 && bits === 16) return buf.readInt16LE(at) / 32768;
    if (tag === 1 && bits === 24) return ((buf[at] | (buf[at+1] << 8) | (buf[at+2] << 16) << 8 >> 8)) / 8388608;
    if (tag === 1 && bits === 32) return buf.readInt32LE(at) / 2147483648;
    return 0;
  };
  const lo = [], hi = [];
  for (let b = 0; b < n; b++) {
    let mn = 0, mx = 0;
    const from = b * per, to = Math.min(frames, from + per);
    for (let i = from; i < to; i++) {
      const v = read(i);
      if (v < mn) mn = v;
      if (v > mx) mx = v;
    }
    lo.push(Math.round(mn * 32768));
    hi.push(Math.round(mx * 32768));
  }
  return { ok: true, secs: frames / rate, frames, buckets: n, lo, hi };
}


// ---------------------------------------------------------------------------
// The audio pre-flight
// ---------------------------------------------------------------------------

// **What the daemon was actually started with.**
//
// Read off the running process rather than configured here, because the whole
// fault this diagnoses is a map that was right when the daemon started and is
// not right now. A second copy of the arguments could only ever agree with
// what we WISH was running.
function daemonArgs() {
  return new Promise((resolve) => {
    let out = "";
    let child;
    try { child = spawn("ps", ["-ax", "-o", "pid=,command="]); }
    catch { return resolve(null); }
    child.stdout.on("data", (c) => (out += c));
    child.on("error", () => resolve(null));
    child.on("close", () => {
      const line = out.split("\n").find((l) => /itajara\s+loop\b/.test(l));
      if (!line) return resolve(null);
      const pid = Number(line.trim().split(/\s+/)[0]);
      // `--device` takes a name with spaces in it ("ES9 then A4C"), so the
      // arguments cannot be split on whitespace. Walk them instead: a value
      // runs until the next token that begins `--`.
      const toks = line.slice(line.indexOf("itajara")).trim().split(/\s+/).slice(1);
      let device = "", sources = [], i = 0;
      while (i < toks.length) {
        const t = toks[i];
        if (t === "--device" || t === "--source") {
          const val = [];
          i += 1;
          while (i < toks.length && !toks[i].startsWith("--")) val.push(toks[i++]);
          if (t === "--device") device = val.join(" ");
          else if (val.length) sources.push(val.join(" "));
        } else i += 1;
      }
      resolve(device ? { pid, device, sources } : null);
    });
  });
}

// **Where every source lands, right now**, against the device as it currently
// stands — `itajara sources` opens nothing, starts no stream and changes no
// sample rate, which is what makes it safe to run from a web request beside a
// session in progress.
//
// The text is returned as the tool prints it rather than parsed into fields.
// It already reads as English — "ES-9 is not switched on", "NOT THERE" — and a
// parser over somebody else's layout is a second thing to keep in step for no
// gain. The one machine-readable judgement below is a substring test, which is
// honest about being one.
function audioCheck() {
  return new Promise(async (resolve) => {
    const d = await daemonArgs();
    if (!d) {
      return resolve({ ok: false, running: false, output:
        "the looper daemon is not running, so there is nothing to check its "
        + "inputs against — start a session and try again" });
    }
    const args = ["sources", "--device", d.device];
    for (const s of d.sources) args.push("--source", s);
    let out = "", err = "", child;
    try { child = spawn(ITAJARA, args); }
    catch (e) {
      return resolve({ ok: false, running: true, device: d.device,
        output: `could not run the pre-flight (${ITAJARA}): ${e.message}` });
    }
    child.stdout.on("data", (c) => (out += c));
    child.stderr.on("data", (c) => (err += c));
    child.on("error", (e) => resolve({ ok: false, running: true, device: d.device,
      output: `could not run the pre-flight: ${e.message}` }));
    child.on("close", () => {
      const text = (out + err).trim();
      // A member of the aggregate that CoreAudio cannot see today. This is the
      // whole of the automatic judgement — the rest is for a person to read.
      // Kept WHOLE. A member reads as a CoreAudio UID
      // ("AppleUSBAudioEngine:Expert Sleepers Ltd:ES-9:1100000:2,3") and every
      // rule for trimming one to a friendly name is wrong for some device —
      // splitting on the last colon yields "3". The page leads with the
      // per-source sentences below, which are already English; this list only
      // has to answer "is something missing", and it does.
      const gone = text.split("\n")
        .filter((l) => l.includes("NOT THERE"))
        .map((l) => l.replace("NOT THERE", "").trim().replace(/^[—\-\s]+/, "").trim())
        .filter(Boolean);
      // A source the tool says is unreachable. Named so the page can hold them
      // against what the DAEMON believes it is holding: a source the daemon
      // still calls available while this says it is gone is a stale map, and
      // that is a restart rather than a patching problem.
      const unreachable = [];
      for (const line of text.split("\n")) {
        const m = line.trim().match(/^(\w[\w-]*)\s+\((.+)\)$/);
        if (m && !/^in\s/.test(m[2])) unreachable.push({ source: m[1], says: m[2] });
      }
      resolve({ ok: true, running: true, pid: d.pid, device: d.device,
                gone, unreachable, text });
    });
  });
}

// **Throw sets away.** The one destructive thing this server does to work you
// made, so it says exactly what it removed and refuses anything that is not a
// set directory under `samples/`.
//
// The take is left alone. A set is a cut of a take and the take may have others
// cut from it, or be worth cutting again — deleting the derived thing should
// not reach back to the thing it was derived from.
function dropSets(names) {
  const gone = [], kept = [];
  for (const raw of names ?? []) {
    const name = safe(String(raw || ""));
    const dir = path.join(SAMPLES, name);
    if (!name || !fs.existsSync(dir) || !fs.statSync(dir).isDirectory()) {
      kept.push(`${raw}: no such set`);
      continue;
    }
    try {
      fs.rmSync(dir, { recursive: true, force: true });
      gone.push(name);
    } catch (e) {
      kept.push(`${name}: ${e.message}`);
    }
  }
  return {
    ok: kept.length === 0,
    output: (gone.length ? `deleted ${gone.length}: ${gone.join(", ")}` : "nothing deleted")
      + (kept.length ? ` — ${kept.join("; ")}` : ""),
    sets: sets(),
  };
}

function cards() {
  const vols = "/Volumes";
  if (!fs.existsSync(vols)) return [];
  return fs.readdirSync(vols)
    .map((n) => path.join(vols, n))
    .filter((p) => {
      try {
        if (fs.existsSync(path.join(p, "rample.bin"))) return true;
        return fs.readdirSync(p).some((e) => /^[A-Z]([0-9]|[0-9][0-9])$/.test(e));
      } catch { return false; }
    });
}

// **Which letters a mounted card already holds is NOT read here.** Measured
// 2026-09-12, and it cost an hour: a `fs.readdirSync` of `/Volumes/FACTORY`
// **blocks forever** in this process when Bosun spawns it, while the identical
// code answers in 10 ms from a hand-started one — same Node, same PATH, same
// cwd, even with `env -i`. The whole event loop stops, so every later request
// on the socket gets no bytes at all and the page simply dies.
//
// `cards()` above never hit it because it short-circuits on `rample.bin` with
// `existsSync` and so never enumerates the card. Listing the free letters was
// the first thing here ever to read a removable volume's contents, and it
// wedged the server on the first call.
//
// The letters are still worth showing — the letter is the blast radius — but
// they have to come from somewhere that is allowed to look. `msm` reads the
// card happily from a terminal, so a subprocess is the likely route; that is a
// thing to establish before writing it again, not to assume twice.

// What each sample set holds, so the page can say "11 samples" without
// guessing from the name.
// **The key centre a set was played in** — DECLARED, never inferred.
//
// A chord set is a bag of absolute MIDI and knows nothing about what it is
// "in". Two things want that knowledge and neither can recover it:
//
//   * **Transposition.** A set only becomes a transposable bank — the
//     `Key -> chords` shape Vetula's palettes have — once there is a root to
//     measure its intervals from. That needs the ROOT ALONE; major or minor
//     does not enter into shifting a set of intervals.
//   * **Reading how far out a chord is.** That needs the intended tonality,
//     and only as context: against an intended MINOR, a major I is further
//     out than a minor chord of the same complexity. It is a fact about what
//     you meant, so it cannot be measured from what came back.
//
// Why declared. For a capture off a generator like Progressions the key and
// the major/minor switch are SETTINGS SOMEBODY CHOSE. Recovering "D minor"
// from the audio would be handing back an input, badly; being told it is
// exact. And an inferred centre that is wrong is worse than none, because
// everything downstream transposes from it silently.
//
// `root` is a pitch class 0..11 — register is not part of a key centre — and
// -1 means undeclared, which is the honest state for every set recorded
// before this existed. `tonality` is "", "major" or "minor"; "" is a set
// whose root is known and whose intent is not, which is a real position and
// not a half-filled form.
function readCentre(raw) {
  const r = Number(raw?.root);
  const root = Number.isInteger(r) && r >= 0 && r <= 11 ? r : -1;
  const t = String(raw?.tonality ?? "");
  return { root, tonality: t === "major" || t === "minor" ? t : "" };
}

// **Clearing and failing must not look alike.**
//
// `readCentre` is lenient because it also reads sets off disk, where an absent
// or ancient field has to degrade to "not said". A WRITE cannot be lenient in
// the same way: a caller sending root 99 has a bug, and quietly storing "not
// said" for it would erase a declaration that everything downstream transposes
// from — the exact silent failure this field exists to prevent.
//
// So: absent or -1 clears, on purpose. 0..11 sets. Anything else is refused
// with the value in the sentence, because the number that was wrong is the
// only thing worth telling the caller.
function checkCentre(raw) {
  const hasRoot = raw?.root !== undefined && raw?.root !== null;
  const r = Number(raw?.root);
  if (hasRoot && !(Number.isInteger(r) && r >= -1 && r <= 11)) {
    return { bad: `a key centre's root is a pitch class 0..11, or -1 for none — not ${JSON.stringify(raw.root)}` };
  }
  const t = raw?.tonality === undefined || raw?.tonality === null ? "" : String(raw.tonality);
  if (t !== "" && t !== "major" && t !== "minor") {
    return { bad: `a key centre's tonality is "major", "minor", or "" for not said — not ${JSON.stringify(raw.tonality)}` };
  }
  return { centre: { root: hasRoot ? r : -1, tonality: t } };
}

// Write the centre onto a stored set, in place. Only this field is touched:
// a declaration made months after the recording must not rewrite anything the
// recording measured.
// Open a stored set, let `amend` change the fields it owns, write it back.
// Only what `amend` touches is touched: a declaration made months after the
// recording must not rewrite anything the recording measured.
function amendSet(name, amend) {
  const nm = safe(String(name ?? ""));
  const f = path.join(SAMPLES, nm, SET_JSON);
  if (!nm || !fs.existsSync(f)) return { ok: false, output: `no set called ${nm}` };
  try {
    const set = JSON.parse(fs.readFileSync(f, "utf8"));
    const refused = amend(set);
    if (refused) return { ok: false, output: refused };
    fs.writeFileSync(f, JSON.stringify(set, null, 2) + "\n");
    return { ok: true, centre: readCentre(set.centre), arpeggiated: !!set.arpeggiated };
  } catch (e) {
    return { ok: false, output: `could not write to ${nm}: ${e.message}` };
  }
}

function setCentre(body) {
  return amendSet(body?.name, (set) => {
    const checked = checkCentre(body?.centre);
    if (checked.bad) return checked.bad;
    set.centre = checked.centre;
    return null;
  });
}

// **How the chords were played** — as blocks, or arpeggiated. See `voicingsOf`:
// it decides which of the two readings already on disk is the true one.
function setArpeggiated(body) {
  return amendSet(body?.name, (set) => {
    if (typeof body?.arpeggiated !== "boolean") {
      return `how a set was played is true or false, not ${JSON.stringify(body?.arpeggiated)}`;
    }
    set.arpeggiated = body.arpeggiated;
    return null;
  });
}

// **The chords a set was played as**, one entry per sample and in file order.
//
// `struck` keeps two chords played into one region as TWO, which is the whole
// reason it exists: flattened, that pair is an eleven-note voicing, a different
// musical object from the two that were played. A set stored before strikes
// were separated has the flat `notes` list only and is read as one chord.
//
// The rule lives here and only here. It used to live in the client as well,
// and two copies of a rule that decides an IDENTITY is two pictures for one
// chord as soon as they drift.
function voicingsOf(set) {
  // **An arpeggio is one chord, and only the player can say so.**
  //
  // `struck` groups notes that arrive within 50 ms — right for a block chord,
  // where a hand lands its notes 3 to 10 ms apart, and wrong for an arpeggio,
  // where every note is its own strike. Measured on a real take (0913-211849):
  // nine voicings of five and six notes came out as SIXTY-FOUR "chords" of one
  // and two.
  //
  // Both readings are already on disk and neither is lost: `notes` is the
  // region's union, which for an arpeggio IS the voicing. So this is only a
  // question of which to believe, and it stays answerable after the fact.
  //
  // Declared rather than inferred, and the take above is why. "Every strike is
  // a single note" looks like a reliable tell for an arpeggio and would have
  // fired on NONE of those nine samples, because Progressions strikes the bass
  // together with the first arpeggio note — so every region opens with a pair.
  // It would also merge a melodic line, and merge two arpeggiated chords that
  // landed in one region, which is the very case `struck` exists to keep apart.
  const arp = !!set?.arpeggiated;
  return (set?.samples ?? []).map((s) => {
    const flat = Array.isArray(s.notes) ? s.notes : [];
    if (arp) return flat.length ? [flat] : [];
    const struck = (Array.isArray(s.struck) ? s.struck : [])
      .filter((c) => Array.isArray(c) && c.length);
    if (struck.length) return struck;
    return flat.length ? [flat] : [];
  });
}

function sets() {
  if (!fs.existsSync(SAMPLES)) return {};
  const out = {};
  for (const d of fs.readdirSync(SAMPLES, { withFileTypes: true })) {
    if (!d.isDirectory()) continue;
    const files = fs.readdirSync(path.join(SAMPLES, d.name)).filter((f) => f.toLowerCase().endsWith(".wav"));
    out[d.name] = files.length;
  }
  return out;
}

function run(args) {
  return new Promise((resolve) => {
    let out = "", err = "";
    let child;
    try { child = spawn(MSM, args); }
    catch (e) { return resolve({ ok: false, output: `could not start ${MSM}: ${e.message}` }); }
    child.stdout.on("data", (c) => (out += c));
    child.stderr.on("data", (c) => (err += c));
    child.on("error", (e) => resolve({ ok: false, output: e.message }));
    child.on("close", (code) => resolve({ ok: code === 0, output: (out + err).trim() }));
  });
}

// The same, keeping the two streams apart.
//
// `run` joins them because for prose that is what a person wants to read. A
// document cannot be read that way: `msm --json` puts the report on stdout
// and everything else on stderr precisely so that a parser sees one and a
// watching person sees the other, and merging them here would undo that.
//
// A non-zero exit is NOT a failure to parse. `kit build --json` prints the
// report and *then* refuses, because the problems are the thing being asked
// for — so the document is read first and the exit code recorded beside it.
function runJson(args) {
  return new Promise((resolve) => {
    let out = "", err = "";
    let child;
    try { child = spawn(MSM, args); }
    catch (e) { return resolve({ ok: false, report: null, output: `could not start ${MSM}: ${e.message}` }); }
    child.stdout.on("data", (c) => (out += c));
    child.stderr.on("data", (c) => (err += c));
    child.on("error", (e) => resolve({ ok: false, report: null, output: e.message }));
    child.on("close", (code) => {
      let report = null;
      try { report = JSON.parse(out); }
      catch (e) { return resolve({ ok: false, report: null, output: (err + out).trim() || e.message }); }
      resolve({ ok: code === 0, report, output: err.trim() });
    });
  });
}

// ===========================================================================
// **A sample set is a stored object**, not a folder of WAVs.
//
// `Triggerfish.Clips` already holds the pattern one level down: a `MidiClip`
// stores full-fidelity events and bakes in no tempo, key or quantisation, so
// every lossy projection happens at playback. A sample set is that argument
// applied to audio — keep the samples and **the spec that made them**, and
// flatten to whatever the destination can address only at the end.
//
// So `set.json` sits IN the sample directory, not beside it. The set is the
// directory: copy it and you have copied the whole thing, description
// included, and there is no way to end up holding audio whose meaning lived
// somewhere else. `msm cut --overwrite` empties that directory first, which
// is why this is written after the cut and never before.
//
// Four things, and the plan's own list:
//
//   the samples      at capture fidelity, never normalised — `cut` writes them
//   the spec         curves, ranges, routing, schedule; reproducible at a
//                    DIFFERENT resolution, which is the whole point of storing
//                    it rather than the values it happened to produce
//   the measurements peak, rms, zcr, tilt — what says whether the set is any
//                    good, and the only thing an unattended run can be judged by
//   the meanings     what the instrument was DOING, in its own terms
//
// Samples are output: regenerable, discardable, improvable. The spec is what
// you keep.
const SET_JSON = "set.json";

// **How many volts a level of 1.0 is worth**, stated once for the whole set
// rather than multiplied into every sample.
//
// Measured on the ES-9 2026-09-10, closed loop out-to-in: the round trip is
// linear and unity (`measured = 0.9928 x sent - 0.0008`, worst residual
// 0.00001), and the rig documents the panel as +/-10 V. It is a fact about the
// interface, not about the music, so a set recorded through something else
// would carry a different number here and its labels would still read right.
const VOLTS_PER_LEVEL = 10;

function writeSet(dir, meta) {
  // **The files on disk, in the order the module will read them** — read back
  // rather than predicted. `cut` names them `<set>-01.wav` and skips a region
  // that came out empty, so a predicted list can silently be one longer than
  // the real one and every meaning after the gap would describe the wrong
  // audio. Byte order is also exactly how the Rample stacks a voice.
  let files = [];
  try {
    files = fs.readdirSync(dir).filter((f) => f.toLowerCase().endsWith(".wav")).sort();
  } catch { /* the cut failed; there is nothing to describe */ }

  const given = Array.isArray(meta.samples) ? meta.samples : [];
  const samples = files.map((file, i) => ({ file, ...(given[i] || {}) }));

  // What the set said about itself before this write, for the fields a
  // re-write must not silently drop. Absent is the normal case.
  let was = null;
  try { was = JSON.parse(fs.readFileSync(path.join(dir, SET_JSON), "utf8")); }
  catch { /* no description yet, or not readable — nothing to preserve */ }

  const set = {
    version: 1,
    name: meta.name,
    take: meta.take,
    made: new Date().toISOString(),
    kind: meta.kind || "",
    stereo: !!meta.stereo,
    sliced: !!meta.sliced,
    slots: meta.slots || 0,
    slotSecs: meta.slotSecs || 0,
    voltsPerLevel: VOLTS_PER_LEVEL,
    // Null for a take played by hand. The samples and their measurements are
    // still worth keeping; what is missing is the ability to run it again.
    spec: meta.spec ?? null,
    // **What it was listening to, spec or no spec.**
    //
    // Which MIDI port and channel the notes were taken from, and which audio
    // input the sound came back on, are facts about the RECORDING — true of a
    // hand-played take as much as a swept one. They used to live inside the
    // spec, which is null for a played take, so they were discarded for
    // exactly the takes whose notes go wrong. The three chord sets of
    // 2026-09-12 hold regions of 20 to 42 notes spanning 0 to 124 against
    // real voicings of five to seven, and nothing stored with them can say
    // which port or channel let that in.
    listened: {
      notesFrom: meta.listened?.notesFrom ?? "",
      notesChan: Number(meta.listened?.notesChan ?? 0),
      source: meta.listened?.source ?? "",
      // **What PLAYED it**, which no set has ever recorded.
      //
      // `source` is the cable the audio came back on and this is the
      // instrument at the far end of it — the same question only by accident,
      // since "ipad" names a jack and what is worth knowing a year later is
      // what was open on it. Free text, because a list would have to enumerate
      // every synth this rig has never heard of, which is exactly the set of
      // sounds worth recording.
      //
      // It is also what makes a library queryable: "every chord set from that
      // patch, in G minor" is unanswerable today, and no amount of indexing
      // fixes a field that was never captured.
      voice: meta.listened?.voice ?? "",
    },
    // **The key centre, declared with the set rather than onto it afterwards.**
    //
    // See `readCentre`. An undeclared centre leaves whatever is already on
    // disk alone: the Library can declare one long after the cut, and a
    // re-cut arriving with root -1 must not silently discard it. (`msm cut
    // --overwrite` empties the directory first, so there is usually nothing
    // to preserve — this is correct rather than load-bearing, and stays
    // correct if that ever changes.)
    centre: (() => {
      const asked = readCentre(meta.centre);
      if (asked.root >= 0) return asked;
      return readCentre(was?.centre);
    })(),
    schedule: Array.isArray(meta.schedule) ? meta.schedule : [],
    samples,
  };
  try {
    fs.writeFileSync(path.join(dir, SET_JSON), JSON.stringify(set, null, 2) + "\n");
  } catch (e) {
    return `the samples are written but their description is not: ${e.message}`;
  }
  return files.length === given.length || !given.length
    ? null
    : `${files.length} files written and ${given.length} described — the description is `
      + `keyed to the files on disk, so check the set before sending it anywhere`;
}

// Every stored set, newest first, with just enough to choose one by.
function storedSets() {
  if (!fs.existsSync(SAMPLES)) return [];
  const out = [];
  for (const d of fs.readdirSync(SAMPLES, { withFileTypes: true })) {
    if (!d.isDirectory()) continue;
    const dir = path.join(SAMPLES, d.name);
    let set = null;
    try { set = JSON.parse(fs.readFileSync(path.join(dir, SET_JSON), "utf8")); } catch {}
    const files = fs.readdirSync(dir).filter((f) => f.toLowerCase().endsWith(".wav"));
    out.push({
      name: d.name,
      count: files.length,
      // A set written before this existed, or one cut from a hand-played
      // take. Listed either way — what is on disk is what is on disk — and
      // the page says which is which, because they are not the same thing:
      // one has no description, the other has one and no spec inside it.
      described: !!set,
      made: set?.made ?? "",
      take: set?.take ?? "",
      runnable: !!set?.spec,
      // Whether it goes to a card as stereo, which decides how many voices it
      // occupies — a stereo sample plays its right channel on the voice after
      // it, so it needs a pair and cannot start on voice 4. The Library places
      // stored sets, so the Library has to know.
      stereo: !!set?.stereo,
      // The parameters that were moved, by name, so a list of sets reads as a
      // list of experiments rather than a list of folders.
      moved: (set?.spec?.params ?? []).map((q) => q.name),
      extent: set?.spec?.extent ?? [],
      encoding: set?.spec?.encoding ?? "",
      // **The pitch of each sample, in file order**, or -1 where a sample
      // stands for no note. Cheap — it is already in `set.json`, one number
      // per sample — and it is what lets the library DRAW a set rather than
      // describe it: a chromatic run reads as a run, and the same run
      // repeated four times reads as a repeat, which is the whole difference
      // between an arrangement that can reach a pitch and one that cannot.
      notes: (set?.samples ?? []).map((s) =>
        (s.means ?? []).reduce((n, m) => (n >= 0 ? n : Number(m.note)), -1)),
      // **How long each sample is**, in file order. The decay axis of a sweep
      // IS a duration axis — 0.40 / 0.92 / 1.03 / 1.85 down the four rows of
      // the drum grid — and nothing on the page has ever shown it. It is also
      // what decides the slot each layer gets, so it is the same number the
      // manifest writes as `slot`.
      // A set cut before sets were stored has no `samples`, so the lengths
      // come off the files themselves. **Worth the header reads**: those are
      // exactly the sets nobody can tell anything about, and an absent length
      // drawn as a default width is not "unknown", it is a WRONG number that
      // reads as a real one. The two longform sets came out looking like
      // half-second hits when they are 166 and 124 seconds.
      secs: (set?.samples ?? []).length
        ? set.samples.map((s) => Math.max(0, Number(s.end ?? 0) - Number(s.start ?? 0)))
        : fileSecs(dir),
      // What the take WAS, declared when it was recorded. The one reliable
      // statement that a sample is a chord: `samples[].notes` cannot be used
      // for it — a chord-hits set records 36 to 42 "notes" per sample spanning
      // 0 to 123, including 0,1,2,3,4,5,6, and a sibling set recorded the same
      // morning has none at all. Something other than note-ons is reaching it.
      kind: set?.kind ?? "",
      // **The voicings, on the listing** — per sample, the chords struck into
      // it, in playing order.
      //
      // Here rather than only on the set's own page because the CHORD SET'S
      // IDENTITY IS MADE OF THEM: a chord set wears the rebus of its chords,
      // which is how the same progression gets the same picture in Quadrat and
      // in Vetula, and the library list is exactly where that picture has to
      // be drawn. Read from `set.json`, which is already parsed for every row.
      //
      // Only for a set DECLARED a chord take. A sweep's `samples[].notes` are
      // single pitches and would make every sweep look like a progression of
      // one-note chords — and worse, would give it a content identity that
      // means nothing, which is the one thing an identity must not do.
      voicings: (set?.kind ?? "") === "chord-hits" ? voicingsOf(set) : [],
      // The declared key centre, or root -1 for "nobody has said". See
      // `readCentre` — on the listing because it is what makes a set
      // transposable, and that is a property you want to see before opening it.
      centre: readCentre(set?.centre),
      // Declared: an arpeggiated take is one chord per region. See `voicingsOf`.
      arpeggiated: !!set?.arpeggiated,
      // What it was listening to. Empty for every set written before this was
      // recorded, which is an honest "not known" rather than "nothing".
      notesFrom: set?.listened?.notesFrom ?? "",
      notesChan: Number(set?.listened?.notesChan ?? 0),
      // What played it. On the listing because it is the field a library is
      // assembled ON — "every chord set from that patch" is a question about
      // the list, not about one set.
      voice: set?.listened?.voice ?? "",
    });
  }
  return out.sort((a, b) => String(b.made).localeCompare(String(a.made)));
}

function storedSet(name) {
  const dir = path.join(SAMPLES, safe(name));
  try {
    const set = JSON.parse(fs.readFileSync(path.join(dir, SET_JSON), "utf8"));
    // **What the files ARE**, read from the first one's header.
    //
    // Not in `set.json`, because the recorder knew it and never wrote it down
    // — and it is the one fact about a sample that decides whether a module
    // will play it at all. One header read, and only when a set is opened: on
    // the list it would be one per set per refresh, for a number nobody reads
    // while scanning.
    return { ok: true, set, audio: wavShape(dir, set) };
  } catch (e) {
    return { ok: false, output: `${name} has no ${SET_JSON} — it was cut before sets were stored` };
  }
}

// Every WAV in a directory, by length, in the order the module would sort
// them. Capped, because this runs per set on every list refresh and a
// directory of a thousand would cost a thousand opens for a picture that
// cannot show a thousand boxes anyway.
function fileSecs(dir) {
  try {
    return fs.readdirSync(dir)
      .filter((f) => f.toLowerCase().endsWith(".wav") && !f.startsWith("."))
      .sort()
      .slice(0, 256)
      .map((f) => wavSecs(path.join(dir, f)) ?? 0);
  } catch { return []; }
}

function wavShape(dir, set) {
  const first = (set.samples ?? [])[0]?.file
    ?? fs.readdirSync(dir).find((f) => f.toLowerCase().endsWith(".wav"));
  if (!first) return null;
  try {
    const fd = fs.openSync(path.join(dir, first), "r");
    const head = Buffer.alloc(4096);
    const n = fs.readSync(fd, head, 0, 4096, 0);
    fs.closeSync(fd);
    if (head.slice(0, 4).toString() !== "RIFF") return null;
    let off = 12, out = null;
    while (off + 8 <= n) {
      const id = head.slice(off, off + 4).toString();
      const size = head.readUInt32LE(off + 4);
      if (id === "fmt ") {
        out = {
          channels: head.readUInt16LE(off + 10),
          rate: head.readUInt32LE(off + 12),
          bits: head.readUInt16LE(off + 22),
          // 1 is PCM, 0xFFFE is EXTENSIBLE — which the Arbhar refuses
          // silently. `msm` canonicalises on the way out; this says what is
          // on disk. See `hardware-wants-canonical-pcm`.
          tag: head.readUInt16LE(off + 8),
        };
        break;
      }
      off += 8 + size + (size & 1);
    }
    return out;
  } catch { return null; }
}

// ===========================================================================
// **Put a set on a voice.** The card half, and nothing else.
//
// Split out of `addToCard` when SuperDirt arrived. Until then a set was cut
// and placed in one act, because a set had exactly one destination and the
// fusion cost nothing. It costs something now: **the card is a projection of
// the library**, so putting a set on a card has to be possible for a set that
// already exists, without going back to the take it came from. Re-cutting
// would be re-deriving, and a projection that re-derives is not one.
//
// `shape` is what the voice has to agree about — the module's rules, all of
// which it enforces silently on its own.
function placeOnCard({ set, bank: bankIn, letter: letterIn, kit: kitIn, voice: voiceIn,
                       append, layerMode, kind, shape, grid }) {
  // `slots` is reassignable: a set smaller than the bank's division adopts it
  // rather than being refused over silence. See the check below.
  const { sliced, slotSecs } = shape;
  let { slots } = shape;
  const card = readCard();
  const bankName = String(bankIn || "WORKSHOP").toUpperCase().replace(/[^A-Z0-9 ]/g, "").trim() || "WORKSHOP";
  const kitName = String(kitIn || set);
  const voice = Math.min(4, Math.max(1, Number(voiceIn) || 1));
  // **The letter is the blast radius.** `msm kit build --write` deletes each
  // kit slot's whole directory before writing it, so the letter decides what a
  // write destroys. Without one the compiler picks — it chose `A` on
  // 2026-09-12, over a bank of Squarp's own content, while the page believed
  // it had said `L`. It says which, or nothing happens.
  const letter = String(letterIn || "").toUpperCase().replace(/[^A-Z]/g, "").slice(0, 1);
  if (!letter) {
    return { ok: false, output:
      "no bank letter. A write deletes the slot it lands on, so the letter is " +
      "the one thing that cannot be left to a default — say A to Z." };
  }

  let bank = (card.banks ||= []).find((b) => b.letter === letter);
  if (!bank) { bank = { letter, name: bankName, kits: [] }; card.banks.push(bank); }
  else bank.name = bankName;
  let kit = (bank.kits ||= []).find((k) => k.name === kitName);
  if (!kit) { kit = { name: kitName, voices: {} }; bank.kits.push(kit); }
  // Velocity for a stack of hits, manual for anything chosen deliberately.
  const at = String(voice);
  const there = asStack(kit.voices[at]);
  // `shape` came in; `there` is what is already on the voice.

  // **A smaller set adopts the bank's division rather than being refused.**
  //
  // The divisions are a fixed ladder — 8, 12, 16, 24, 32, 48, 64, 128 — and a
  // set takes the smallest rung that holds it. Seven chords want 8 and twelve
  // want 12, which is a clash over nothing: the slots past the last piece are
  // silent by construction, so seven pieces sit perfectly well in twelve slots
  // and the bank keeps one division. Only a set that genuinely does not FIT
  // the bank's division is refused, and then it is refused for a real reason.
  if (slots && bank.slicer && bank.slicer > slots) {
    slots = bank.slicer;
    if (shape) shape.slots = slots;
  }
  if (slots && bank.slicer && bank.slicer !== slots) {
    return { ok: false, output:
      `bank ${bankName} is cut into ${bank.slicer} and this is ${slots}. SLICER is one ` +
      `global setting, so they cannot share a bank — put this in another one.` };
  }

  if (append && there) {
    // **A layer has to be the same shape as the ones beside it.**
    //
    // SLICER divides whatever is playing, so every layer on a voice is cut by
    // the same division — two layers wanting different ones cannot both be
    // right, and the module would not say which was wrong. Same for stereo,
    // which claims the next voice as well.
    const differs =
      there.kind !== shape.kind ||
      !!there.stereo !== shape.stereo ||
      !!there.sliced !== shape.sliced ||
      (there.slots || 0) !== shape.slots;
    if (differs) {
      return { ok: false, output:
        `voice ${voice} holds ${there.kind || "material"} in ${there.slots || 0} slots` +
        `${there.stereo ? ", stereo" : ""} — a layer beside it has to match, because ` +
        `SLICER divides whatever is playing and one voice cannot have two divisions.` };
    }
    if (there.layers.length >= 12 && !there.layers.some((l) => l.set === set)) {
      return { ok: false, output:
        "twelve layers is the module's ceiling, and it drops the rest without saying so." };
    }
    // Re-sending a set already here is a recut, not a thirteenth layer.
    const known = there.layers.findIndex((l) => l.set === set);
    if (known >= 0) there.layers[known] = { ...there.layers[known], set };
    else there.layers.push({ set });
    // The slot has to hold the longest piece of ANY layer.
    there.slotSecs = Math.max(there.slotSecs || 0, slotSecs);
    kit.voices[at] = there;
  } else if (grid) {
    // A grid is the whole voice by construction: every layer of it comes from
    // this one set, and a fifth from somewhere else would have to agree with
    // all four. Appending to one is possible and is not offered until someone
    // wants it.
    kit.voices[at] = { layers: grid.layers, ...shape };
  } else {
    // One layer that is the whole set. When it is SLICED its slots are the
    // set's own samples in order, so the same chords belong on it — by the
    // same rule as a grid's rows, and absent unless every one is known.
    const one = { set };
    if (sliced) {
      // Read here rather than taken from a caller: three callers reach this
      // and only some of them have the set open, and a field that arrives on
      // two paths out of three is a field you cannot trust at the far end.
      const sm = (storedSet(set) || {}).samples || [];
      if (sm.length && sm.every((x) => x.notes && x.notes.length)) {
        one.slotNotes = sm.map((x) => x.notes.slice());
      }
    }
    kit.voices[at] = { layers: [ one ], ...shape };
  }

  // **Softest first, spread across the range.**
  //
  // With layer mode VELOCITY the module picks by how hard you played, and a
  // layer that does not say which dynamic it stands for cannot be picked
  // deliberately. Assigned by position because position is what the recording
  // order already means — the same convention the byte order carries.
  const stack = asStack(kit.voices[at]);
  const n = stack.layers.length;
  if (n > 1) {
    stack.layers.forEach((l, i) => {
      if (l.velocity == null) l.velocity = Math.round(((i + 1) / n) * 127);
    });
  } else {
    delete stack.layers[0].velocity;
  }

  // The layer mode is the kit's, and a stack of alternatives wants the module
  // to choose between them. Hits are velocity by their nature; anything else
  // stays manual until asked.
  if (layerMode) kit.layers = String(layerMode);
  else if (!kit.layers) kit.layers = kind === "drum-hits" ? "velocity" : "manual";

  // **One bank, one division.**
  //
  // SLICER is a single global setting, so every sliced file in a bank is cut
  // by the same number. A bank silently adopting whatever came last is how
  // `WORKSHOP` ended up asking for /12 with sixteen-slice files in it — each
  // file fine, and only their neighbours making them wrong. Refused here
  // rather than left for the compiler, because by then the samples are cut and
  // the card row written.
  if (slots) bank.slicer = slots;
  writeCard(card);

  // The division it SETTLED on, which is not always the one it was asked for:
  // a set smaller than the bank's adopts the bank's. Returned so the caller
  // reports what was written rather than what it proposed.
  return { ok: true, slots };
}

// **A two-axis set, as the module has to hold it.**
//
// The outer axis becomes LAYERS — alternatives the module picks between, by
// velocity or at random — and the inner becomes SLICES inside each layer's
// file, which only the start point can reach. That asymmetry is the whole
// reason a transect has two axes (see `Quadrat.Encoding`), and it is why a
// grid cannot be placed the way a line is: 48 files globbed onto one voice is
// 48 layers, of which the module plays twelve and drops the rest in silence.
// Measured 2026-09-12, on a 4 x 12 that reported exactly that.
//
// Each layer gets its OWN slot length, because the four decays of a decay
// sweep are four different lengths and SLICER divides each file by proportion.
// What must agree is the slot COUNT, which is the one global setting, and
// `placeOnCard` already refuses a bank whose divisions disagree.
//
// **`rows` may be asked for rather than inferred.** The sweep's own extent is
// the natural arrangement and stays the default, but it is not the only legal
// one: the same 48 files are 4 layers of 12, or 2 of 24, or one file of 48,
// and which of those you want is a fact about the MODULE you are playing, not
// about the recording. Until the page could ask, the extent decided — and the
// `as` control that claimed to choose was never consulted for any set with two
// axes, which is every set worth arranging.
//
// A grouping that is not the sweep's own loses the layer NAMES, and says so:
// a layer stands for a value of the outer parameter, and four rows regrouped
// into two stand for nothing anybody can name.

// **`n` samples over `v` voices, as even as it goes.** The remainder is spread
// one each across the first voices rather than piled on the last, and the
// evenness is musical rather than tidy: in RANDOM layer mode each voice picks
// uniformly among its OWN layers, so twelve-and-six would make each of the six
// twice as likely to sound as each of the twelve.
function splitOver(v, n) {
  const base = Math.floor(n / v), extra = n % v;
  return Array.from({ length: v }, (_, i) => base + (i < extra ? 1 : 0));
}

// **Which voices a set can begin on.** A stereo sample plays its right channel
// on the next voice, so it consumes a PAIR — two places to start, not four.
function voicesFrom(first, count, stereo) {
  const step = stereo ? 2 : 1;
  const out = [];
  for (let i = 0; i < count; i++) out.push(first + i * step);
  return out;
}

// **A stack of plain layers over one run of a set's samples**, shaped like a
// grid's layers so `placeOnCard` needs to know nothing new: one layer per
// sample, each naming its own file.
//
// This is what makes a set of more than twelve placeable at all. Eighteen
// stereo chords have no single-voice answer — over the ceiling, and no
// division the module offers divides them — so before this they could not go
// on a card by any route.
function stackOf(set, dir, samples, from, howMany) {
  const run = samples.slice(from, from + howMany);
  const layers = [];
  for (let i = 0; i < run.length; i++) {
    const c = run[i];
    const secs = wavSecs(path.join(dir, c.file));
    if (secs == null) return null;
    layers.push({
      set,
      files: [ c.file ],
      name: `l${from + i + 1}`,
      slotSecs: Math.ceil(secs * 100) / 100,
      velocity: Math.round(((i + 1) / run.length) * 127),
      // **What this layer IS, carried from the set.** The sample already
      // records the chord that made it — declared by whatever played it, not
      // guessed from the audio — and this is the one step where that either
      // travels onto the card or is lost. Absolute MIDI, no name: naming is
      // a theory question and belongs to whoever reads the index.
      notes: (c.notes && c.notes.length) ? c.notes.slice() : undefined,
    });
  }
  return layers.length ? layers : null;
}

function gridLayers(set, d, dir, askRows) {
  const ext = (d.spec && d.spec.extent) || [];
  const sm = d.samples || [];
  const n = sm.length;

  // Asked for: any even grouping of the ordered files, within the module's
  // twelve-layer ceiling. The order is the sweep's, which is the only order
  // these files have.
  // How the sweep itself grouped them, or zero for a set that was not swept
  // on two axes. Two things turn on it and they are NOT the same thing, which
  // is worth separating here because conflating them broke the 1-D case: a
  // grouping that is the sweep's own can be VERIFIED against the recorded
  // cell coordinates and its layers carry parameter values; every other
  // grouping can do neither. A set with no extent has no sweep grouping at
  // all — so it cannot be verified either, and it was never "regrouped",
  // because it was never grouped.
  const swept = ext.length === 2 && ext[0] >= 2 && ext[1] >= 2 && n === ext[0] * ext[1]
    ? ext[0] : 0;

  if (askRows) {
    if (askRows < 1 || askRows > MAX_LAYERS || n === 0 || n % askRows !== 0) return null;
    return gridOf(set, d, dir, askRows, n / askRows, swept);
  }

  if (!swept) return null;
  return gridOf(set, d, dir, swept, n / swept, swept);
}

const MAX_LAYERS = 12;

function gridOf(set, d, dir, rows, cols, swept) {
  const sm = d.samples || [];
  // The sweep's own grouping: verifiable, and its layers stand for values.
  const own = swept > 0 && rows === swept;

  const layers = [];
  for (let r = 0; r < rows; r++) {
    const cells = sm.slice(r * cols, (r + 1) * cols);
    // `Encoding.cells` varies the inner axis fastest, so a row of the file
    // order IS a row of the grid — but say so rather than assume it, because a
    // set written by some later encoding would be silently transposed.
    // `Encoding.cells` varies the inner axis fastest, so a row of the file
    // order IS a row of the grid — checked rather than assumed, because a set
    // written by some later encoding would be silently transposed. Only for
    // the sweep's own grouping: a regrouping is deliberately across the cells
    // and cannot satisfy it.
    if (own && !cells.every((c, i) => Array.isArray(c.cell) && c.cell[0] === r && c.cell[1] === i)) return null;
    let longest = 0;
    for (const c of cells) {
      const secs = wavSecs(path.join(dir, c.file));
      if (secs == null) return null;
      longest = Math.max(longest, secs);
    }
    // What this layer STANDS for: the value of whatever moves along the outer
    // axis. A card records where a sample sits and never what it is, so the
    // name is the only thing carrying the meaning out of here.
    // **One parameter names the layer, not all of them.** Joining every knob
    // that moves along the outer axis gave `decay-33-attack-22-harm-`, cut off
    // mid-word by the length the module can show — three facts, none of them
    // readable. The first parameter is the one you chose the axis for; a `+2`
    // says the others came with it, and the file beside it in `set.json` still
    // holds all of them exactly.
    const mine = own ? (cells[0].means || []).filter((m) => m.note < 0) : [];
    const head = mine[0];
    // Letters, digits and hyphens only: anything else comes back as a space in
    // the filename, and `decay-0 2.wav` reads as a mistake rather than as a
    // note. What else moved along this axis is in `set.json` and in the card's
    // own index, which are the places that can hold it.
    const label = head
      ? `${head.name}-${Math.round(head.at * 100)}`.replace(/[^A-Za-z0-9-]/g, "")
      : `layer-${r + 1}`;
    layers.push({
      set,
      files: cells.map((c) => c.file),
      name: label.slice(0, 20),
      slotSecs: Math.ceil(longest * 100) / 100,
      velocity: Math.round(((r + 1) / rows) * 127),
      // **What each slot of this join sounds.** Only when every cell knows —
      // a half-filled array would be worse than none, since a consumer
      // indexing by slot has no way to tell a missing entry from an empty
      // chord. Absent for a swept set, whose slots are parameter values and
      // whose pitches, if any, are in `means`.
      slotNotes: cells.every((c) => c.notes && c.notes.length)
        ? cells.map((c) => c.notes.slice())
        : undefined,
    });
  }
  // Distinct, because the name is most of the filename and two layers sharing
  // one would be a silent overwrite.
  const seen = new Set();
  for (let i = 0; i < layers.length; i++) {
    let nm = layers[i].name;
    if (seen.has(nm)) nm = `${nm.slice(0, 16)}-${i + 1}`;
    seen.add(nm);
    layers[i].name = nm;
  }
  // The division the module must be set to. `cols` is how many pieces each
  // layer holds; SLICER offers only the eight, so a layer of six sits in eight
  // with two silent — which is how a smaller set adopts a bank's division
  // rather than being refused over silence.
  const slots = SLICE_DIVISIONS.find((x) => x >= cols) || 0;
  // `regrouped` only where there WAS a grouping to depart from.
  return { layers, slots, regrouped: swept > 0 && !own };
}

const SLICE_DIVISIONS = [8, 12, 16, 24, 32, 48, 64, 128];

// Seconds of a WAV, from its header alone. Enough to size a slot, and it does
// not read the audio to do it.
function wavSecs(file) {
  try {
    const fd = fs.openSync(file, "r");
    const head = Buffer.alloc(4096);
    const n = fs.readSync(fd, head, 0, 4096, 0);
    fs.closeSync(fd);
    if (head.slice(0, 4).toString() !== "RIFF") return null;
    let off = 12, rate = 0, ch = 0, bits = 0, bytes = 0;
    while (off + 8 <= n) {
      const id = head.slice(off, off + 4).toString();
      const size = head.readUInt32LE(off + 4);
      if (id === "fmt ") {
        ch = head.readUInt16LE(off + 10);
        rate = head.readUInt32LE(off + 12);
        bits = head.readUInt16LE(off + 22);
      } else if (id === "data") { bytes = size; break; }
      off += 8 + size + (size & 1);
    }
    if (!rate || !ch || !bits || !bytes) return null;
    return bytes / (rate * ch * (bits / 8));
  } catch { return null; }
}

// **A set already on disk, onto a voice.** No take, no cut, no measuring.
//
// The other half of the second encoding. SuperDirt needs no projection at all
// — a set as stored IS a bank — so the Rample projection had to become
// something that acts on a SET rather than on a take, or "the same set reaches
// both" would have meant "the same take was cut twice".
//
// Everything the card needs about the shape is in `set.json`, which is exactly
// what it is for.
function placeStoredSet(body) {
  const set = safe(body.set || "");
  const dir = path.join(SAMPLES, set);
  if (!set || !fs.existsSync(dir)) return { ok: false, output: `no set called ${set || "(none)"}` };

  const got = storedSet(set);
  if (!got.ok) {
    return { ok: false, output:
      `${set} has no description, so nothing here knows whether it is sliced, ` +
      `stereo, or into how many. Cut it again from its take.` };
  }
  const d = got.set;
  const files = fs.readdirSync(dir).filter((f) => f.toLowerCase().endsWith(".wav"));
  if (!files.length) return { ok: false, output: `${set} holds no audio` };

  // **Sliced or stacked is a PLACEMENT decision, not a property of the cut.**
  //
  // The audio on disk is the same either way — twelve files — and what changes
  // is whether the manifest concatenates them into one file the start point
  // addresses, or lists them as twelve layers the layer CV addresses. So a set
  // can be tried both ways without going back to its take, which is the whole
  // reason to ask: twelve stereo layers occupy a voice pair and a whole bank's
  // worth of a kit, and one sliced file occupies the same pair with eleven
  // more kits left over.
  //
  // `set.json`'s own `sliced` stays the default, so nothing that worked before
  // changes; naming it here overrides for this placement only.
  const askSliced = body.sliced == null ? null : !!body.sliced;
  // **The arrangement, asked for.** `layers` is how many alternatives the
  // module picks between; the slices follow from it, since each layer holds
  // what is left. One layer is "one sliced file"; as many layers as there are
  // samples is "plain layers" and has no slices at all.
  const askLayers = Number(body.layers) || 0;
  const SLICES = [8, 12, 16, 24, 32, 48, 64, 128];
  const spans = (d.samples || []).map((x) => Number(x.end) - Number(x.start)).filter((n) => n > 0);
  const ownSlots = SLICES.find((n) => n >= (spans.length || files.length)) || 0;
  const ownSlotSecs = spans.length ? Math.max(...spans) : 0;

  // A grid holds its own shape: layers from the outer axis, slices from the
  // inner. `set.json` says so and nothing else has to be told.
  // Plain layers is the one arrangement that is not a grid: every sample its
  // own layer, and no division at all. Asking for it is asking for no grid.
  const plainLayers = askLayers > 0 && askLayers === (d.samples || files).length;

  // **Across more than one voice.**
  //
  // The module holds twelve layers a voice and four voices — two, for stereo,
  // since a stereo sample claims the next one. A set larger than twelve has no
  // single-voice answer, so it spreads: one kit, one bank, a run of the set on
  // each voice, plain layers throughout.
  //
  // Each voice is written by its own `placeOnCard`, because a voice is what
  // that function places and a kit slot is what it writes. They share the kit,
  // so what comes back is one kit with two stacks in it.
  const askVoices = Math.max(0, Number(body.voices) || 0);
  if (askVoices > 1) {
    const sm = d.samples || [];
    if (!sm.length) {
      return { ok: false, output: `${set} has no described samples to spread` };
    }
    const per = splitOver(askVoices, sm.length);
    const firstVoice = Math.min(4, Math.max(1, Number(body.voice) || 1));
    const seats = voicesFrom(firstVoice, askVoices, !!d.stereo);
    const last = seats[seats.length - 1] + (d.stereo ? 1 : 0);
    if (last > 4) {
      return { ok: false, output:
        `${set} wants ${askVoices}${d.stereo ? " stereo" : ""} voices from voice `
        + `${firstVoice}, which runs past voice 4. Start it at voice 1.` };
    }
    let at = 0, wrote = [];
    for (let i = 0; i < seats.length; i++) {
      const layers = stackOf(set, dir, sm, at, per[i]);
      if (!layers) return { ok: false, output:
        `could not measure ${set}'s samples, so they cannot be laid out` };
      const longest = Math.max(...layers.map((l) => l.slotSecs));
      const one = placeOnCard({
        set, grid: { layers, slots: 0 },
        bank: body.bank, letter: body.letter, kit: body.kit, voice: seats[i],
        append: false, layerMode: body.layerMode, kind: d.kind,
        shape: { kind: d.kind || "", stereo: !!d.stereo, sliced: false,
                 slots: 0, slotSecs: longest },
      });
      // A refusal part-way leaves the earlier voices written. Said plainly:
      // the card is a real thing and half of an arrangement is on it.
      if (!one.ok) {
        return { ok: false, output: one.output
          + (wrote.length ? ` — voice ${wrote.join(" and ")} ${wrote.length > 1 ? "were" : "was"} already written` : "") };
      }
      wrote.push(seats[i]);
      at += per[i];
    }
    const even = per.every((x) => x === per[0]);
    return {
      ok: true,
      output: `${set} (${sm.length} samples) across ${wrote.length}`
        + `${d.stereo ? " stereo" : ""} voices — `
        + (even ? `${per[0]} layers each on voice ${wrote.join(" and ")}`
                : per.map((x, i) => `${x} on voice ${wrote[i]}`).join(", ")),
      card: readCard(),
      sets: sets(),
    };
  }

  const grid = plainLayers ? null : gridLayers(set, d, dir, askLayers || undefined);
  const placed = placeOnCard({
    set, grid,
    bank: body.bank, letter: body.letter, kit: body.kit, voice: body.voice,
    append: body.append, layerMode: body.layerMode, kind: d.kind,
    shape: grid
      ? { kind: d.kind || "", stereo: !!d.stereo, sliced: true,
          slots: grid.slots, slotSecs: Math.max(...grid.layers.map((l) => l.slotSecs)) }
      : plainLayers
        ? { kind: d.kind || "", stereo: !!d.stereo, sliced: false, slots: 0, slotSecs: 0 }
      : askSliced === null
        ? { kind: d.kind || "",
            stereo: !!d.stereo,
            sliced: !!d.sliced,
            slots: d.slots || 0,
            slotSecs: d.slotSecs || 0,
          }
        : { kind: d.kind || "",
            stereo: !!d.stereo,
            sliced: askSliced,
            slots: askSliced ? ownSlots : 0,
            slotSecs: askSliced ? ownSlotSecs : 0,
          },
  });
  if (!placed.ok) return placed;
  return {
    ok: true,
    output: grid
      // **The division it SETTLED on, not the one it proposed.** `placeOnCard`
      // already returns that for exactly this reason — a set smaller than its
      // bank's division adopts the bank's — and this said the proposal
      // anyway: placing a 7-sample set into a bank cut into 12 reported "one
      // file of 8 slices" while writing 12. The reply is the only account
      // anyone gets of what happened, so it has to be an account of what
      // happened.
      ? `${set} on voice ${Math.min(4, Math.max(1, Number(body.voice) || 1))} — `
          + (grid.layers.length === 1
              ? `one file of ${placed.slots || grid.slots} slices`
              : `${grid.layers.length} layers of ${placed.slots || grid.slots} slices`)
          + (grid.regrouped ? ", regrouped — the layers carry positions, not values" : "")
      : askSliced
        ? `${set} on voice ${Math.min(4, Math.max(1, Number(body.voice) || 1))} — `
            + `one file of ${placed.slots || ownSlots} slices, `
            + `${ownSlotSecs.toFixed(2)}s each. SLICER ${placed.slots || ownSlots}.`
        : `${set} (${files.length} samples) on voice ${Math.min(4, Math.max(1, Number(body.voice) || 1))}`,
    card: readCard(),
    sets: sets(),
  };
}

// Cut the kept regions into a set, and put that set on a voice.
async function addToCard(body) {
  const take = safe(body.take || "");
  const dir = path.join(TAKES, take);
  if (!take || !fs.existsSync(dir)) return { ok: false, output: `no take called ${take || "(none)"}` };
  const wav = firstWav(dir);
  if (!wav) return { ok: false, output: `${take} holds no audio` };

  const set = safe(body.set || take);
  // Default true: every caller before SuperDirt meant "and put it on a voice".
  const place = body.place !== false;
  const regions = Array.isArray(body.regions) ? body.regions : [];
  if (!regions.length) return { ok: false, output: "nothing kept, so there is nothing to send" };

  fs.mkdirSync(SHOP, { recursive: true });
  const rjson = path.join(SHOP, ".regions.json");
  fs.writeFileSync(rjson, JSON.stringify(regions));

  // **A phrase is one file with slices, never a stack of layers.**
  //
  // The module plays twelve layers and silently drops the rest, so a bar cut
  // into sixteen cannot be sixteen layers — and layers are picked by the layer
  // selector, which cannot be sequenced per note. Joined, every piece sits
  // under the start point `CC(voice x 10 + 4)` and can be triggered in any
  // order, which is the whole reason to slice a phrase.
  // **Cut writes pieces; the manifest joins them.**
  //
  // It could join here — `cut --join` does — but a pre-joined file is a file,
  // and a file cannot say which velocity it stands for or which note its first
  // slot is. Left as pieces and joined by `kit build`, every layer carries that
  // with it, which is the difference between a card the module can play and an
  // instrument something can play from.
  const sliced = !!body.join;
  const args = ["cut", wav, "--regions", rjson, "--out", path.join(SAMPLES, set),
                "--name", set, "--module", "rample", "--overwrite"];
  // **A bars take is a loop, and only the recording knows it.**
  //
  // It was armed on a bar count and the daemon closed it itself at exactly
  // that length, so the file is one cycle — which means the tail its last
  // slice is missing is the same material as the head it started with. Said
  // here rather than guessed in `cut`, because for every other kind the head
  // is a pre-roll and joining it to the end would splice two unrelated
  // moments together.
  if (sliced && body.kind === "bars") args.push("--cyclic");
  args.push(body.stereo ? "--stereo" : "--mono");
  const cut = await run(args);
  if (!cut.ok) return cut;
  // The division `cut` settled on, straight from its own report rather than
  // recomputed here — SLICER is global on the module and the card cannot carry
  // it, so it has to reach the datasheet as a written instruction.
  // The geometry of a sliced voice, from the regions we just cut: the slot is
  // the longest piece and the division is the next one SLICER offers. Both are
  // needed by the manifest, and the division has to reach the datasheet as an
  // instruction because SLICER is global and no card can carry it.
  const SLICES = [8, 12, 16, 24, 32, 48, 64, 128];
  const slotSecs = sliced
    ? Math.max(...regions.map((r) => Number(r.end) - Number(r.start)))
    : 0;
  const slots = sliced ? SLICES.find((n) => n >= regions.length) || 0 : 0;
  if (sliced && !slots) {
    return { ok: false, output:
      `${regions.length} pieces, and the biggest division SLICER offers is 128. ` +
      `Keep fewer, or put them on more than one voice.` };
  }

  // **The description, written with the audio and into the same directory.**
  //
  // Here rather than in a second call from the page, so that a set cannot
  // exist without it: an interruption between "cut" and "describe" would
  // leave exactly the folder of anonymous WAVs this is meant to abolish.
  const described = writeSet(path.join(SAMPLES, set), {
    name: set,
    take,
    kind: body.kind,
    stereo: !!body.stereo,
    sliced, slots, slotSecs,
    spec: body.spec ?? null,
    listened: body.listened ?? null,
    // Declared in the sentence before the take. `writeSet` keeps whatever is
    // already on disk when this says nothing.
    centre: body.centre ?? null,
    schedule: body.schedule ?? [],
    samples: body.samples ?? [],
  });

  // **Cutting a set and placing it on a card are two acts.**
  //
  // They were one until SuperDirt arrived, because until then every set had
  // exactly one destination and the fusion cost nothing. SuperDirt has no
  // voices, no kits and no banks to be placed in — the set as stored IS the
  // bank — so a set has to be able to exist without a place on a card. Which
  // was always true and had simply never been asked.
  if (!place) {
    return {
      ok: true,
      output: described ? `${cut.output}\n${described}` : cut.output,
      card: readCard(),
      sets: sets(),
    };
  }

  const placed = placeOnCard({
    set,
    bank: body.bank, letter: body.letter, kit: body.kit, voice: body.voice,
    append: body.append, layerMode: body.layerMode, kind: body.kind,
    shape: { kind: body.kind || "", stereo: !!body.stereo, sliced, slots, slotSecs },
  });
  if (!placed.ok) return placed;

  return {
    ok: true,
    output: described ? `${cut.output}\n${described}` : cut.output,
    card,
    sets: sets(),
  };
}

// **Where the detector thinks things begin, over a take the daemon just wrote.**
//
// Quadrat records one take with as many hits in it as you felt like
// playing, and then has to show you what it caught — because a capture you
// cannot see is a capture you have to trust, and the first two attempts at
// this proved how badly that goes. `msm onset --json` proposes the divisions;
// the page draws them over the waveform and a person keeps the ones they meant.
// **A take has had two shapes**, and both are on disk. `exl` writes
// `loop-<n>/layer-<nn>.wav`; the older `w` wrote the layers straight into the
// take. Looking only in the subdirectories found nothing in a flat take and
// said "holds no audio", which is true of the place it looked and false of
// the take. Search both, subdirectories first, and take the first file in
// order — for a Quadrat capture that is the only file.
// ---------------------------------------------------------------------------
// CV, for the sweep. OSC to es9-daemon, which holds the ES-9 open.
//
// The browser cannot open a UDP socket, so this is the seam — and it is the
// only place the page can reach a voltage. Hand-rolled OSC because the whole
// encoding is three rules (pad every string and blob to four bytes, big-endian
// throughout, a comma-led type tag) and a dependency for that would be the
// tail wagging the dog.
//
// **Nothing here can tell you the module moved.** UDP is fire-and-forget, and
// es9-daemon does not answer. `ok` means the datagram left this process. The
// confirmation of a sweep is the audio it produced: twelve tiles that differ.
const ES9 = (process.env.ES9_DAEMON_ADDR || "127.0.0.1:57130").split(":");
const ES9_HOST = ES9[0] || "127.0.0.1";
const ES9_PORT = Number(ES9[1] || 57130);

// N.B. `/cv` is the DIRECT bus path and, unlike `/tidal/cv`, es9-daemon does
// NOT apply its SAFETY_SCALE to it — 1.0 is the ES-9's full output. Clamped to
// ±1 here so a bad number is a quiet limit rather than a loud surprise, but the
// range still belongs to whoever sets it.
const oscStr = (s) => {
  const b = Buffer.from(String(s) + "\0", "ascii");
  return Buffer.concat([b, Buffer.alloc((4 - (b.length % 4)) % 4)]);
};

const oscMsg = (addr, args) => {
  const parts = [oscStr(addr), oscStr("," + args.map((a) => a.t).join(""))];
  for (const a of args) {
    const b = Buffer.alloc(4);
    if (a.t === "i") b.writeInt32BE(a.v | 0, 0);
    else b.writeFloatBE(a.v, 0);
    parts.push(b);
  }
  return Buffer.concat(parts);
};

const bus = (n) => Math.max(0, Math.min(15, Number(n) | 0));
const level = (n) => Math.max(-1, Math.min(1, Number(n) || 0));

let sock = null;
function osc(msgs) {
  if (!sock) {
    sock = dgram.createSocket("udp4");
    sock.on("error", () => {});
    sock.unref();
  }
  for (const m of msgs) sock.send(m, ES9_PORT, ES9_HOST);
}

// **The ES-5's own gates and the ESX-8CV's eight channels.**
//
// Both ride the same Silent Way lane on `ES5_L_BUS` (bus 4) and es9-daemon
// auto-enables the encoding on first use — so the only cost of reaching them
// is that bus 4 stops being raw CV. Buses 8-15, the ES-9's panel jacks, are
// untouched. Eight more CVs and eight more gates for one expander bus.
//
// **The ESX is 12-bit** where the panel jacks are the audio DAC's full
// resolution: `val * 2048`, so about 4.9 mV a step over +/-10 V. Ample for a
// morph; coarse for a V/oct, where it is about a sixteenth of a semitone.
const slot = (n) => Math.max(0, Math.min(7, Number(n) | 0));

// DeepStar's HTTP API. Its port is its own `DefaultAPIPort` constant; override
// with DEEPSTAR_URL if it ever moves. Failure is REPORTED, never silently an
// empty list — "no calibration tables" and "the rig doctor is not running" look
// identical in a dropdown and are completely different problems.
const DEEPSTAR_URL = process.env.DEEPSTAR_URL || "http://127.0.0.1:3027";

async function deepstar(path) {
  try {
    const r = await fetch(DEEPSTAR_URL + path, { signal: AbortSignal.timeout(8000) });
    if (!r.ok) return { ok: false, error: `deepstar ${path}: HTTP ${r.status}` };
    return { ok: true, body: await r.json() };
  } catch (e) {
    return { ok: false, error: `deepstar ${path}: ${e.message} (is \`deepstar serve\` up on ${DEEPSTAR_URL}?)` };
  }
}

function cv(body) {
  const msgs = [];
  const said = [];
  for (const s of body?.set ?? []) {
    msgs.push(oscMsg("/cv", [{ t: "i", v: bus(s.bus) }, { t: "f", v: level(s.level) }]));
    said.push(`${bus(s.bus)}=${level(s.level).toFixed(3)}`);
  }
  for (const e of body?.esx ?? []) {
    msgs.push(oscMsg("/esx", [{ t: "i", v: slot(e.slot) }, { t: "f", v: level(e.level) }]));
    said.push(`esx${slot(e.slot)}=${level(e.level).toFixed(3)}`);
  }
  const p = body?.pulse;
  if (p) {
    const ms = Math.max(1, Math.min(10000, Number(p.ms) || 10));
    msgs.push(oscMsg("/cv/trig", [
      { t: "i", v: bus(p.bus) },
      { t: "f", v: level(p.level) },
      { t: "f", v: ms },
    ]));
    said.push(`trig ${bus(p.bus)}=${level(p.level).toFixed(3)} for ${ms}ms`);
  }
  // **A gate placed on the daemon's clock, not on the page's.**
  //
  // `/cv/trig` fires when the message lands, so every millisecond the browser
  // is late goes straight into the audio. `/cv/trig/at` carries a delay and the
  // daemon applies the gate in its audio callback at `current_frame() + delay`
  // — the same frame counter the capture is written from. So the page says
  // WHEN instead of NOW, and can subtract its own lateness from the delay it
  // asks for: waking 200 ms late costs nothing if it asks for 200 ms less.
  //
  // Measured 2026-09-11. Twelve gates paced from the browser carried a 120 ms
  // step at the sixth, in the same place at 3000 ms and at 5000 ms spacing; the
  // identical sequence paced from node, through these same endpoints and buses,
  // carried none — twice, to 9 and 17 ms over 36 s. The page was the only
  // difference. See docs/kb/research/quadrat-slice-drift.md.
  const pa = body?.pulseAt;
  if (pa) {
    const ms = Math.max(1, Math.min(10000, Number(pa.ms) || 10));
    const delayMs = Math.max(0, Math.min(60000, Number(pa.delayMs) || 0));
    msgs.push(oscMsg("/cv/trig/at", [
      { t: "i", v: bus(pa.bus) },
      { t: "f", v: level(pa.level) },
      { t: "f", v: ms },
      { t: "f", v: delayMs },
    ]));
    said.push(`trig ${bus(pa.bus)}=${level(pa.level).toFixed(3)} for ${ms}ms at +${delayMs.toFixed(0)}ms`);
  }
  // **An ES-5 gate has no duration of its own**, so a pulse is on, wait, off —
  // and the waiting happens HERE rather than in the browser, because two HTTP
  // round trips with the hold between them would put the page's latency inside
  // the gate. It is fire-and-forget either way; see the note above `cv`.
  const g = body?.es5pulse;
  if (g) {
    const bit = slot(g.bit);
    const ms = Math.max(1, Math.min(10000, Number(g.ms) || 10));
    msgs.push(oscMsg("/esx5gate", [{ t: "i", v: bit }, { t: "i", v: 1 }]));
    said.push(`es5 gate ${bit} for ${ms}ms`);
    setTimeout(() => {
      try { osc([oscMsg("/esx5gate", [{ t: "i", v: bit }, { t: "i", v: 0 }])]); } catch {}
    }, ms);
  }
  if (!msgs.length) return { ok: false, output: "nothing to send" };
  try {
    osc(msgs);
  } catch (e) {
    return { ok: false, output: String(e.message ?? e) };
  }
  return { ok: true, output: `sent ${said.join(", ")} to ${ES9_HOST}:${ES9_PORT}` };
}

function firstWav(dir) {
  const inSubdirs = fs.readdirSync(dir, { withFileTypes: true })
    .filter((e) => e.isDirectory() && /^loop-\d+$/.test(e.name))
    .sort((a, b) => a.name.localeCompare(b.name))
    .flatMap((e) =>
      fs.readdirSync(path.join(dir, e.name))
        .filter((f) => f.toLowerCase().endsWith(".wav"))
        .sort()
        .map((f) => path.join(dir, e.name, f)));
  if (inSubdirs.length) return inSubdirs[0];
  const flat = fs.readdirSync(dir)
    .filter((f) => f.toLowerCase().endsWith(".wav"))
    .sort();
  return flat.length ? path.join(dir, flat[0]) : null;
}

function onsets(body) {
  const take = safe(body.take || "");
  const dir = path.join(TAKES, take);
  if (!take || !fs.existsSync(dir)) {
    return Promise.resolve({ ok: false, output: `no take called ${take || "(none)"}` });
  }
  // The take Quadrat writes has one capture in it. Find the
  // first audio file rather than assuming a name: the daemon numbers loops by
  // which one recorded, and the scratch loop is the last one.
  const wav = firstWav(dir);
  if (!wav) return Promise.resolve({ ok: false, output: `${take} holds no audio` });

  // **The schedule divides, where there is one.**
  //
  // A swept run knows every trigger time, so it hands the boundaries over and
  // `msm` measures them instead of looking for them. Same JSON back either
  // way, so the page draws one thing and neither it nor the card path needs
  // to know which happened. Detection stays for takes played by hand, which
  // is every take nothing scheduled.
  const declared = Array.isArray(body.regions) ? body.regions : null;
  if (declared && declared.length) {
    const clean = declared
      .map((r) => ({ start: Number(r.start), end: Number(r.end) }))
      .filter((r) => isFinite(r.start) && isFinite(r.end) && r.end > r.start);
    if (!clean.length) {
      return Promise.resolve({ ok: false, output: "the schedule named no regions with anything in them" });
    }
    return msmJson(["onset", wav, "--json", "--regions", "-"], JSON.stringify(clean));
  }

  const args = ["onset", wav, "--as", String(body.as || "hits").replace(/[^a-z]/g, ""), "--json"];
  // **How close two sounds can be and still be two.**
  //
  // The knob that does not discriminate by loudness, which matters because a
  // velocity stack is played softest first: culling by level takes the quiet
  // end, and the quiet end is the reason the stack exists. Bounded here so a
  // slider cannot ask for something meaningless.
  if (body.minGap != null && isFinite(Number(body.minGap))) {
    args.push("--min-gap", String(Math.min(2000, Math.max(5, Number(body.minGap)))));
  }
  // **Which question to ask of the audio.**
  //
  // Not a refinement of one algorithm — a different one. Attacks finds where
  // the signal changes fastest, which is exactly right for anything struck and
  // meaningless for a pad that swells over two seconds. Gaps finds where the
  // envelope comes back down, which is how six pad chords separate. The page
  // offers both by name because no setting of either becomes the other.
  if (body.by) args.push("--by", String(body.by).replace(/[^a-z0-9:]/g, ""));
  if (body.gapDepth != null && isFinite(Number(body.gapDepth))) {
    args.push("--gap-depth", String(Math.min(40, Math.max(4, Number(body.gapDepth)))));
  }
  return msmJson(args, null);
}

// Run msm and expect one JSON object on stdout. `stdin` is written and the
// pipe closed where it is given — which is how a schedule reaches `--regions -`
// without a temporary file for something that exists for one call.
function msmJson(args, stdin) {
  return new Promise((resolve) => {
    let out = "", err = "";
    let child;
    try {
      child = spawn(MSM, args);
    } catch (e) {
      return resolve({ ok: false, output: `could not start ${MSM}: ${e.message}` });
    }
    child.stdout.on("data", (c) => (out += c));
    child.stderr.on("data", (c) => (err += c));
    child.on("error", (e) => resolve({ ok: false, output: e.message }));
    if (stdin != null) {
      child.stdin.on("error", () => {});
      child.stdin.end(stdin);
    }
    child.on("close", (code) => {
      if (code !== 0) return resolve({ ok: false, output: err || out || `msm exited ${code}` });
      try {
        resolve({ ok: true, ...JSON.parse(out) });
      } catch (e) {
        resolve({ ok: false, output: `msm said something that is not JSON: ${out.slice(0, 200)}` });
      }
    });
  });
}

function harvest(body) {
  const args = ["harvest", safe(body.take), "--module", body.module || "arbhar"];
  if (body.stick) args.push("--stick", String(body.stick));
  // The Rample addresses by KIT, not by bank and scene: a card path and a slot
  // like G0, plus what the layers mean, which is what sets the layer mode.
  if (body.card) args.push("--card", String(body.card));
  if (body.slot) args.push("--slot", String(body.slot).toUpperCase().replace(/[^A-Z0-9]/g, ""));
  if (body.as) args.push("--as", String(body.as).replace(/[^a-z-]/g, ""));
  if (body.bank) args.push("--bank", String(Number(body.bank)));
  if (body.scene) args.push("--scene", String(body.scene).replace(/[^0-9_]/g, ""));
  if (body.overwrite) args.push("--overwrite");
  if (body.allLayers) args.push("--all-layers");
  if (body.dryRun) args.push("--dry-run");
  return new Promise((resolve) => {
    let out = "";
    let child;
    try {
      child = spawn(MSM, args);
    } catch (e) {
      return resolve({ ok: false, output: `could not start ${MSM}: ${e.message}` });
    }
    child.stdout.on("data", (c) => (out += c));
    child.stderr.on("data", (c) => (out += c));
    child.on("error", (e) => resolve({ ok: false, output: `could not run ${MSM}: ${e.message}. Is msm built and on the PATH (cargo install --path SamplesProject/msm)?` }));
    child.on("close", (code) => resolve({ ok: code === 0, output: `$ ${MSM} ${args.join(" ")}\n${out}` }));
  });
}

// ------------------------------------------------------------ source labels
//
// **What the user calls the thing on the other end of the cable.**
//
// The daemon's source names are wire names: `--source board=AUDIO4c:1,2` says
// which jacks, and it has to, because a source that cannot be resolved against
// the interface is a session recorded off the wrong input. But a wire name is
// a poor thing to pick from under pressure — `board` and `hits` sit next to
// each other in a list and read alike, and on 2026-09-11 a whole 4 x 12 was
// recorded from the wrong one of them and came back silent.
//
// A rename in the launch args would fix that pair and no others, because the
// right name is not a fact about this rig: it is "Jupiter 8", or "the Neumann",
// or whatever the person actually has plugged in. So the wire name stays as
// the identity and a *label* sits on top of it, chosen once by whoever owns
// the rig and read everywhere afterwards.
//
// Keyed by the wire name deliberately. The spec of a stored set records the
// source it was recorded from BY WIRE NAME, so a set stays readable when a
// label is changed, and `sourceLost` in the page still fires on the one thing
// that genuinely breaks it — a source that is no longer plugged in at all.
// Labels are cosmetic by construction; nothing routes on them.

const SOURCES = path.join(os.homedir(), ".itajara", "sources.json");

function sourceLabels() {
  try {
    const conf = JSON.parse(fs.readFileSync(SOURCES, "utf8"));
    if (!conf || typeof conf !== "object" || Array.isArray(conf)) return {};
    const out = {};
    for (const [k, v] of Object.entries(conf)) {
      const label = String(v ?? "").trim();
      if (k && label) out[k] = label.slice(0, 40);
    }
    return out;
  } catch {
    return {};
  }
}

// A merge rather than a replace, and an empty label deletes rather than
// storing "". A page that knows about three sources should not be able to
// forget the label on a fourth just by not having seen it.
function setSourceLabels(body) {
  const want = body && typeof body.labels === "object" && !Array.isArray(body.labels)
    ? body.labels : null;
  if (!want) return { ok: false, output: "expected { labels: { wireName: label } }" };
  const now = sourceLabels();
  for (const [k, v] of Object.entries(want)) {
    const label = String(v ?? "").trim().slice(0, 40);
    if (label) now[String(k)] = label;
    else delete now[String(k)];
  }
  try {
    fs.mkdirSync(path.dirname(SOURCES), { recursive: true });
    fs.writeFileSync(SOURCES, JSON.stringify(now, null, 2) + "\n");
  } catch (e) {
    return { ok: false, output: String(e.message || e) };
  }
  return { ok: true, labels: now };
}

// ---------------------------------------------------------------- libraries
//
// A *library* is a directory of scenes; a *scene* is any directory whose
// children include audio. That one rule reads all of them with no
// per-library configuration: the takes directory (`take/loop-3/layer-00.wav`),
// Instruo's v2 stick image (`_arbhar_scenes/1_1_scene/2_Buzzfade.wav`, and the
// single-sample banks, `_arbhar_library_4/3_2_sample/`), and a plain folder of
// samples such as Lubadh's `01 Drums A`.
//
// Note what is deliberately absent: the module's own namespace. A stick
// addresses *positionally* — six banks of thirty-six, and thirty-six scenes —
// while a library addresses *by name*. `msm harvest` is the compiler between
// the two; this browser only ever knows the naming side, and shows the stick's
// directory names raw rather than prettified, because recognising them is half
// of what the browser is for.

const LIBRARIES = path.join(os.homedir(), ".itajara", "libraries.json");

// Written out on first run so there is a file to edit rather than a setting to
// discover. A root that is not mounted stays in the file and simply does not
// list, so unplugging the disk is not a reason to lose the entry.
const DEFAULT_LIBRARIES = [
  { name: "Takes", path: TAKES },
  { name: "Instruo — Arbhar 2.0", path: "/Volumes/Crucial4TB/Books/Manuals/Music/Instruo/Instruo samples/Arbhar 2.0" },
];

const AUDIO = /\.(wav|aif|aiff|flac)$/i;
const AUDIO_TYPES = { ".wav": "audio/wav", ".aif": "audio/aiff", ".aiff": "audio/aiff", ".flac": "audio/flac" };
const slug = (s) => s.toLowerCase().replace(/[^a-z0-9]+/g, "-").replace(/^-|-$/g, "");

// Dotfiles, and with them macOS's `._` AppleDouble sidecars — which are not
// samples but do end in .wav, and which bit the harvest once already.
const visible = (n) => !n.startsWith(".");

// Numbers inside a name sort as numbers, so `2_1_scene` follows `1_6_scene`
// and `layer-10` follows `layer-9`. Lexical order puts 10 before 2, which
// makes a scene list read wrong in exactly the place it matters.
const sortKey = (s) => s.replace(/\d+/g, (d) => d.padStart(12, "0"));
const natural = (a, b) => (sortKey(a) < sortKey(b) ? -1 : sortKey(a) > sortKey(b) ? 1 : 0);

function libraries() {
  let conf;
  try {
    conf = JSON.parse(fs.readFileSync(LIBRARIES, "utf8"));
  } catch {
    conf = DEFAULT_LIBRARIES;
    try {
      fs.mkdirSync(path.dirname(LIBRARIES), { recursive: true });
      fs.writeFileSync(LIBRARIES, JSON.stringify(conf, null, 2) + "\n");
    } catch {}
  }
  return (Array.isArray(conf) ? conf : [])
    .filter((l) => l && l.name && l.path)
    .map((l) => ({ id: slug(l.name), name: l.name, path: path.resolve(String(l.path).replace(/^~/, os.homedir())) }))
    .filter((l) => { try { return fs.statSync(l.path).isDirectory(); } catch { return false; } });
}

// A path inside one library, or null. The whole of this module's safety: a
// relative path that climbs out of its root resolves outside it and is refused.
function resolveIn(libId, rel) {
  const l = libraries().find((x) => x.id === libId);
  if (!l) return null;
  const p = path.resolve(l.path, rel || "");
  return p === l.path || p.startsWith(l.path + path.sep) ? p : null;
}

// Every directory at or under `root` that holds audio, with the audio in it.
// A directory can be both a scene and a shelf — a flat take has wavs of its
// own beside its loop folders — so this pushes and recurses, never either/or.
function scenesUnder(root, rel = "", depth = 4, out = []) {
  let entries;
  try { entries = fs.readdirSync(path.join(root, rel), { withFileTypes: true }); } catch { return out; }
  const here = entries.filter((e) => e.isFile() && visible(e.name) && AUDIO.test(e.name)).map((e) => e.name).sort(natural);
  if (here.length) {
    const dir = rel === "" ? "" : path.dirname(rel);
    out.push({ path: rel, name: rel === "" ? "(root)" : path.basename(rel), group: dir === "." ? "" : dir, layers: here });
  }
  if (depth > 0)
    for (const e of entries.filter((e) => e.isDirectory() && visible(e.name)).sort((a, b) => natural(a.name, b.name)))
      scenesUnder(root, rel ? path.join(rel, e.name) : e.name, depth - 1, out);
  return out;
}

// The whole tree, names only: readdir is cheap and WAV headers are not, so the
// shape of every library arrives in one call and a scene's durations are read
// when a scene is actually opened.
function shelves() {
  return libraries().flatMap((l) => {
    const by = new Map();
    for (const s of scenesUnder(l.path)) {
      if (!by.has(s.group)) by.set(s.group, []);
      by.get(s.group).push({ path: s.path, name: s.name, layers: s.layers });
    }
    return [...by.entries()]
      .sort((a, b) => natural(a[0], b[0]))
      .map(([group, ss]) => ({ id: l.id + " " + group, lib: l.id, libName: l.name, group, name: group || l.name, scenes: ss }));
  });
}

// What a WAV says about itself, from its header alone: walk the chunks for
// `fmt ` and `data`. A `data` size that overruns the file (or the streaming
// 0xffffffff) is not believed — the file's own length is.
function wavInfo(p) {
  let fd;
  try { fd = fs.openSync(p, "r"); } catch { return {}; }
  try {
    const size = fs.fstatSync(fd).size;
    const head = Buffer.alloc(Math.min(size, 8192));
    const n = fs.readSync(fd, head, 0, head.length, 0);
    if (n < 12 || head.toString("ascii", 0, 4) !== "RIFF") return { bytes: size };
    let off = 12, f = null;
    while (off + 8 <= n) {
      const id = head.toString("ascii", off, off + 4);
      const sz = head.readUInt32LE(off + 4);
      if (id === "fmt " && off + 24 <= n) {
        f = { channels: head.readUInt16LE(off + 10), rate: head.readUInt32LE(off + 12), bits: head.readUInt16LE(off + 22) };
      } else if (id === "data") {
        const bytes = sz === 0xffffffff || off + 8 + sz > size ? size - (off + 8) : sz;
        const frame = f ? (f.bits / 8) * f.channels : 0;
        return { bytes: size, ...(f || {}), secs: frame && f.rate ? bytes / frame / f.rate : null };
      }
      off += 8 + sz + (sz & 1);
    }
    return { bytes: size, ...(f || {}) };
  } catch {
    return {};
  } finally {
    try { fs.closeSync(fd); } catch {}
  }
}

// One scene: its audio with headers read, and its text files with their
// contents — an Arbhar scene's preset is a .txt whose *name* is the preset's
// ("arbharClassic.txt"), so both halves are worth showing.
function sceneInfo(dir) {
  let entries;
  try { entries = fs.readdirSync(dir, { withFileTypes: true }); } catch { return { layers: [], texts: [] }; }
  const files = entries.filter((e) => e.isFile() && visible(e.name)).map((e) => e.name);
  return {
    layers: files.filter((n) => AUDIO.test(n)).sort(natural).map((n) => ({ name: n, ...wavInfo(path.join(dir, n)) })),
    texts: files.filter((n) => /\.txt$/i.test(n)).sort(natural).map((n) => {
      let content = "";
      try { content = fs.readFileSync(path.join(dir, n), "utf8").slice(0, 2000); } catch {}
      return { name: n, content };
    }),
  };
}

// Audio, with byte ranges: Safari will not play a media element from a server
// that answers 200 to a Range request, so this is not optional politeness.
function sendAudio(req, res, file) {
  const size = fs.statSync(file).size;
  const type = AUDIO_TYPES[path.extname(file).toLowerCase()] || "application/octet-stream";
  const m = /^bytes=(\d*)-(\d*)$/.exec(req.headers.range || "");
  if (m && (m[1] !== "" || m[2] !== "")) {
    let start, end;
    if (m[1] === "") { start = Math.max(0, size - Number(m[2])); end = size - 1; }
    else { start = Number(m[1]); end = m[2] === "" ? size - 1 : Math.min(Number(m[2]), size - 1); }
    if (start > end || start >= size) {
      res.writeHead(416, { "content-range": `bytes */${size}` });
      return res.end();
    }
    res.writeHead(206, { "content-type": type, "accept-ranges": "bytes", "content-range": `bytes ${start}-${end}/${size}`, "content-length": end - start + 1 });
    return fs.createReadStream(file, { start, end }).pipe(res);
  }
  res.writeHead(200, { "content-type": type, "accept-ranges": "bytes", "content-length": size });
  fs.createReadStream(file).pipe(res);
}

const server = http.createServer(async (req, res) => {
  const url = new URL(req.url, "http://x");
  const m = url.pathname.match(/^\/api\/takes\/([^/]+)\/notes$/);
  try {
    if (url.pathname === "/api/takes" && req.method === "GET") return json(res, 200, takes());
    if (url.pathname === "/api/sticks" && req.method === "GET") return json(res, 200, sticks());
    if (m && req.method === "GET") {
      const p = path.join(TAKES, safe(m[1]), "notes.json");
      return json(res, 200, fs.existsSync(p) ? JSON.parse(fs.readFileSync(p, "utf8")) : {});
    }
    if (m && req.method === "PUT") {
      const dir = path.join(TAKES, safe(m[1]));
      fs.mkdirSync(dir, { recursive: true });
      const body = await readBody(req);
      fs.writeFileSync(path.join(dir, "notes.json"), JSON.stringify(body, null, 2) + "\n");
      return json(res, 200, { ok: true, path: path.join(dir, "notes.json") });
    }
    if (url.pathname === "/api/card" && req.method === "GET") {
      // The plan as well as the description: `kit build` without --write says
      // everything wrong with it, and saying that here means the page shows
      // the compiler's own objections rather than a second opinion.
      const card = readCard();
      let plan = { ok: true, output: "" };
      if ((card.banks || []).some((b) => (b.kits || []).length)) {
        writeCard(card);
        plan = await run(["kit", "build", CARD_TOML, path.join(SHOP, "preview")]);
      }
      return json(res, 200, { card, sets: sets(), cards: cards(), plan: plan.output, ok: plan.ok });
    }
    // **What a write to THIS card would do**, before doing it.
    //
    // `/api/card` dry-runs into a scratch directory, which answers "is the
    // manifest buildable" and nothing about the card in the slot. The question
    // in front of the Write button is the other one — *what happens to what is
    // already there* — and until this existed the only thing that answered it
    // was the write, by refusing, atomically, in one line at the end of a log.
    //
    // Occupancy is read by `msm` rather than here **on purpose**. Enumerating
    // a mounted Rample volume from this process wedges the event loop dead —
    // measured 2026-09-12, every later request gets zero bytes — while the
    // same walk in a subprocess is fine. See the note above `cards()`.
    if (url.pathname === "/api/card/preview" && req.method === "GET") {
      const dest = String(url.searchParams.get("dest") || "");
      if (!cards().includes(dest)) return json(res, 200, { ok: false, report: null, output: `${dest} is not a mounted card` });
      writeCard(readCard());
      const r = await runJson(["kit", "build", CARD_TOML, dest, "--json"]);
      return json(res, 200, r);
    }
    if (url.pathname === "/api/card/add" && req.method === "POST") {
      return json(res, 200, await addToCard(await readBody(req)));
    }
    if (url.pathname === "/api/card/write" && req.method === "POST") {
      const body = await readBody(req);
      const dest = String(body.dest || "");
      if (!cards().includes(dest)) return json(res, 200, { ok: false, output: `${dest} is not a mounted card` });
      writeCard(readCard());
      // **Never `--overwrite` a card without being asked to.**
      //
      // `execute` removes each kit slot's whole directory before writing it,
      // so an unconditional --overwrite turns "write my nine kits" into
      // "delete whatever nine kits happen to share those letters". On the
      // FACTORY card, whose banks A, B and C hold Squarp's artist content,
      // that would have been nine of theirs. The flag that exists to stop this
      // was being passed every time, so the guard never fired once.
      const args = ["kit", "build", CARD_TOML, dest, "--write"];
      if (body.replace) args.push("--overwrite");
      const r = await run(args);
      return json(res, 200, r);
    }
    if (url.pathname === "/api/card/clear" && req.method === "POST") {
      return json(res, 200, { ok: true, card: writeCard(emptyCard()), sets: sets(), output: "card cleared" });
    }
    if (url.pathname === "/api/onsets" && req.method === "POST") {
      const body = await readBody(req);
      return json(res, 200, await onsets(body));
    }
    if (url.pathname === "/api/cv" && req.method === "POST") {
      const body = await readBody(req);
      return json(res, 200, cv(body));
    }
    if (url.pathname === "/api/harvest" && req.method === "POST") {
      const body = await readBody(req);
      return json(res, 200, await harvest(body));
    }
    // The stored sets: the list, and one whole. See `writeSet` — the object
    // is what makes a set re-runnable at a resolution nobody chose at the time.
    if (url.pathname === "/api/sets/delete" && req.method === "POST") {
      const body = await readBody(req);
      return json(res, 200, dropSets(body?.names));
    }
    if (url.pathname === "/api/card/place" && req.method === "POST") {
      const body = await readBody(req);
      return json(res, 200, placeStoredSet(body));
    }
    // What the owner calls each input. See `sourceLabels`.
    if (url.pathname === "/api/sources" && req.method === "GET") {
      return json(res, 200, { ok: true, labels: sourceLabels() });
    }
    if (url.pathname === "/api/sources" && req.method === "PUT") {
      return json(res, 200, setSourceLabels(await readBody(req)));
    }
    // Declare (or clear) a set's key centre. See `setCentre`.
    if (url.pathname === "/api/sets/centre" && req.method === "POST") {
      return json(res, 200, setCentre(await readBody(req)));
    }
    // The audio pre-flight. See `audioCheck` — reads, opens nothing.
    if (url.pathname === "/api/audio/check" && req.method === "GET") {
      return json(res, 200, await audioCheck());
    }
    if (url.pathname === "/api/sets/arpeggiated" && req.method === "POST") {
      return json(res, 200, setArpeggiated(await readBody(req)));
    }
    if (url.pathname === "/api/sets" && req.method === "GET") {
      return json(res, 200, { ok: true, sets: storedSets() });
    }
    if (url.pathname.startsWith("/api/sets/") && req.method === "GET") {
      return json(res, 200, storedSet(decodeURIComponent(url.pathname.slice("/api/sets/".length))));
    }
    // Calibration tables, relayed from `deepstar serve` (:3027).
    //
    // Relayed rather than fetched by the page, and not only because of CORS: it
    // keeps the ONE address of the rig doctor in this file beside the others,
    // so a Quadrat served from anywhere reaches whatever this machine calls
    // DeepStar. The page gets a table once, carries it in the plan, and does
    // its own inversion (Quadrat.Pitch) — verified to agree with /realise to
    // 2e-15 V — so a 192-hit transect makes ONE call here, not 192.
    if (url.pathname === "/api/calibrations" && req.method === "GET") {
      const r = await deepstar("/calibrations");
      if (!r.ok) return json(res, 502, { error: r.error });
      // Summaries only: the list is for choosing, and the tables behind it are
      // ~21 points each — no reason to ship all 28 to fill a dropdown.
      return json(res, 200, { ok: true, tables: (r.body ?? []).map((t) => ({
        label: t.label, module: t.module ?? "", points: t.points ?? 0,
        loHz: t.lo_hz ?? 0, hiHz: t.hi_hz ?? 0, voltsPerOctave: t.volts_per_octave ?? 0,
      })) });
    }
    if (url.pathname.startsWith("/api/calibrations/") && req.method === "GET") {
      const label = decodeURIComponent(url.pathname.slice("/api/calibrations/".length));
      const r = await deepstar("/calibrations/" + encodeURIComponent(label));
      if (!r.ok) return json(res, 502, { error: r.error });
      const t = r.body?.table ?? r.body ?? {};
      const points = (t.points ?? [])
        .filter((q) => Number.isFinite(Number(q.volts)) && Number.isFinite(Number(q.hz)))
        .map((q) => ({ volts: Number(q.volts), hz: Number(q.hz) }));
      // An empty table is reported as an error rather than returned. A pitch
      // parameter holding no points realises every note as 0 V — one flat
      // transect, and nothing on the page to say why.
      if (!points.length) return json(res, 502, { error: `calibration "${label}" has no usable points` });
      return json(res, 200, { ok: true, label, module: t.module ?? "",
        coarse: t.coarse_setting ?? "", measuredAt: t.measured_at ?? "", points });
    }
    if (url.pathname === "/api/library" && req.method === "GET") return json(res, 200, shelves());
    if (url.pathname === "/api/scene" && req.method === "GET") {
      const dir = resolveIn(url.searchParams.get("lib"), url.searchParams.get("path"));
      if (!dir) return json(res, 404, { error: "no such library" });
      return json(res, 200, sceneInfo(dir));
    }
    // The take Quadrat just recorded, as audio the page can scrub.
    // Previewing a sub-sample is a range of ONE file rather than a file each:
    // cutting them on the server would mean writing dozens of wavs to answer a
    // hover, and the browser can already start and stop inside a file.
    if (url.pathname === "/api/take-peaks" && req.method === "GET") {
      return json(res, 200, takePeaks(url.searchParams.get("take") || "",
                                      Number(url.searchParams.get("buckets")) || 2000));
    }
    if (url.pathname === "/api/take-audio" && req.method === "GET") {
      const take = safe(url.searchParams.get("take") || "");
      const dir = path.join(TAKES, take);
      if (!take || !fs.existsSync(dir)) return json(res, 404, { error: "no such take" });
      const wav = firstWav(dir);
      if (!wav) return json(res, 404, { error: "no audio in that take" });
      return sendAudio(req, res, wav);
    }
    // **One sample of a stored set, to hear.** Named by set and index rather
    // than by path, because the caller knows a set and a position and has no
    // business assembling filenames — and because a route that takes a path is
    // a route to get the path rule wrong in a second place.
    if (url.pathname === "/api/set-audio" && req.method === "GET") {
      const set = safe(url.searchParams.get("set") || "");
      const dir = path.join(SAMPLES, set);
      if (!set || !fs.existsSync(dir)) return json(res, 404, { error: "no such set" });
      const wavs = fs.readdirSync(dir).filter((f) => f.toLowerCase().endsWith(".wav")).sort(natural);
      const i = Math.max(0, Math.min(wavs.length - 1, Number(url.searchParams.get("i")) || 0));
      if (!wavs.length) return json(res, 404, { error: "that set holds no audio" });
      return sendAudio(req, res, path.join(dir, wavs[i]));
    }
    if (url.pathname === "/api/audio" && req.method === "GET") {
      const file = resolveIn(url.searchParams.get("lib"), url.searchParams.get("path"));
      if (!file || !fs.existsSync(file) || fs.statSync(file).isDirectory()) return json(res, 404, { error: "no such file" });
      return sendAudio(req, res, file);
    }
    if (url.pathname.startsWith("/api/")) return json(res, 404, { error: "no such route" });

    // Static, rooted, no traversal.
    let file = path.normalize(path.join(STATIC, url.pathname === "/" ? "index.html" : url.pathname));
    if (!file.startsWith(STATIC)) return json(res, 403, { error: "outside static" });
    if (!fs.existsSync(file) || fs.statSync(file).isDirectory()) file = path.join(STATIC, "index.html");
    res.writeHead(200, { "content-type": TYPES[path.extname(file)] || "application/octet-stream", "cache-control": "no-cache" });
    fs.createReadStream(file).pipe(res);
  } catch (e) {
    json(res, 500, { error: e.message });
  }
});

server.listen(PORT, "127.0.0.1", () => {
  console.log(`the Friends on http://localhost:${PORT}/  (takes in ${TAKES}, msm = ${MSM})`);
});

// Drain on a signal: stop accepting, let a harvest in flight finish its
// reply, then go. A supervisor that sends TERM and waits gets a clean exit
// rather than a socket dropped mid-answer.
for (const sig of ["SIGTERM", "SIGINT"]) {
  process.on(sig, () => {
    server.close(() => process.exit(0));
    setTimeout(() => process.exit(0), 5000).unref();
  });
}
