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
//   GET  /api/takes/:name/notes    the take's notes.json, or {}
//   PUT  /api/takes/:name/notes    write it
//   GET  /api/sticks               mounted volumes that look like an Arbhar stick
//   GET  /api/library              every library's shelves and scenes, names only
//   GET  /api/scene?lib=&path=     one scene: its audio with headers, its texts
//   GET  /api/audio?lib=&path=     one file, with byte ranges, for auditioning
//   POST /api/harvest              { take, module, stick, bank, scene, card, slot,
//                                    as, overwrite, allLayers, dryRun }
//                                  → runs msm harvest,
//                                    answers { ok, output }

import http from "node:http";
import fs from "node:fs";
import path from "node:path";
import os from "node:os";
import { spawn } from "node:child_process";

const PORT = Number(process.env.PORT || 3029);
const MSM = process.env.MSM || "msm";
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
// **A card is compiled, never edited in place.** What the Workshop holds is a
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
const SHOP = path.join(os.homedir(), ".itajara", "workshop");
const CARD_JSON = path.join(SHOP, "card.json");
const CARD_TOML = path.join(SHOP, "card.toml");
const SAMPLES = path.join(SHOP, "samples");

const emptyCard = () => ({ name: "Workshop", banks: [] });

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
function layerSource(st, l) {
  const glob = `samples/${l.set}/*.wav`;
  if (!st.sliced) return q(glob);
  const bits = [`concat = [${q(glob)}]`, `slot = ${st.slotSecs.toFixed(4)}`];
  if (st.slots) bits.push(`slots = ${st.slots}`);
  bits.push(`name = ${q(l.set)}`);
  // What the slots and the layer MEAN. The card records where a sample sits
  // and never what it is, so without these a file of twelve slices is
  // indistinguishable from a field recording by any amount of analysis.
  if (l.velocity != null) bits.push(`velocity = ${l.velocity}`);
  if (l.pitch != null) bits.push(`pitch = ${l.pitch}`);
  return `{ ${bits.join(", ")} }`;
}

function cardToml(card) {
  let t = "# Generated by the Workshop. `msm kit build` reads this.\n";
  t += "# Editing it by hand is fine; the page will overwrite it next time.\n\n";
  t += `name = ${q(card.name || "Workshop")}\n\n`;
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

// What each sample set holds, so the page can say "11 samples" without
// guessing from the name.
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

// Cut the kept regions into a set, and put that set on a voice.
async function addToCard(body) {
  const take = safe(body.take || "");
  const dir = path.join(TAKES, take);
  if (!take || !fs.existsSync(dir)) return { ok: false, output: `no take called ${take || "(none)"}` };
  const wav = firstWav(dir);
  if (!wav) return { ok: false, output: `${take} holds no audio` };

  const set = safe(body.set || take);
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

  const card = readCard();
  const bankName = String(body.bank || "WORKSHOP").toUpperCase().replace(/[^A-Z0-9 ]/g, "").trim() || "WORKSHOP";
  const kitName = String(body.kit || set);
  const voice = Math.min(4, Math.max(1, Number(body.voice) || 1));

  let bank = (card.banks ||= []).find((b) => b.name === bankName);
  if (!bank) { bank = { name: bankName, kits: [] }; card.banks.push(bank); }
  let kit = (bank.kits ||= []).find((k) => k.name === kitName);
  if (!kit) { kit = { name: kitName, voices: {} }; bank.kits.push(kit); }
  // Velocity for a stack of hits, manual for anything chosen deliberately.
  const at = String(voice);
  const there = asStack(kit.voices[at]);
  const shape = { kind: body.kind || "", stereo: !!body.stereo, sliced, slots, slotSecs };

  if (body.append && there) {
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
  } else {
    kit.voices[at] = { layers: [{ set }], ...shape };
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
      l.velocity = Math.round(((i + 1) / n) * 127);
    });
  } else {
    delete stack.layers[0].velocity;
  }

  // The layer mode is the kit's, and a stack of alternatives wants the module
  // to choose between them. Hits are velocity by their nature; anything else
  // stays manual until asked.
  if (body.layerMode) kit.layers = String(body.layerMode);
  else if (!kit.layers) kit.layers = body.kind === "drum-hits" ? "velocity" : "manual";

  // **One bank, one division.**
  //
  // SLICER is a single global setting, so every sliced file in a bank is cut
  // by the same number. A bank silently adopting whatever came last is how
  // `WORKSHOP` ended up asking for /12 with sixteen-slice files in it — each
  // file fine, and only their neighbours making them wrong. Refused here
  // rather than left for the compiler, because by then the samples are cut and
  // the card row written.
  if (slots && bank.slicer && bank.slicer !== slots) {
    return { ok: false, output:
      `bank ${bankName} is cut into ${bank.slicer} and this is ${slots}. SLICER is one ` +
      `global setting, so they cannot share a bank — put this in another one.` };
  }
  if (slots) bank.slicer = slots;
  writeCard(card);

  return { ok: true, output: cut.output, card, sets: sets() };
}

// **Where the detector thinks things begin, over a take the daemon just wrote.**
//
// The Workshop records one take with as many hits in it as you felt like
// playing, and then has to show you what it caught — because a capture you
// cannot see is a capture you have to trust, and the first two attempts at
// this proved how badly that goes. `msm onset --json` proposes the divisions;
// the page draws them over the waveform and a person keeps the ones they meant.
// **A take has had two shapes**, and both are on disk. `exl` writes
// `loop-<n>/layer-<nn>.wav`; the older `w` wrote the layers straight into the
// take. Looking only in the subdirectories found nothing in a flat take and
// said "holds no audio", which is true of the place it looked and false of
// the take. Search both, subdirectories first, and take the first file in
// order — for the Workshop that is the only file.
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
  // The take the Workshop writes has one loop with one layer in it. Find the
  // first audio file rather than assuming a name: the daemon numbers loops by
  // which one recorded, and the scratch loop is the last one.
  const wav = firstWav(dir);
  if (!wav) return Promise.resolve({ ok: false, output: `${take} holds no audio` });

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
    if (url.pathname === "/api/harvest" && req.method === "POST") {
      const body = await readBody(req);
      return json(res, 200, await harvest(body));
    }
    if (url.pathname === "/api/library" && req.method === "GET") return json(res, 200, shelves());
    if (url.pathname === "/api/scene" && req.method === "GET") {
      const dir = resolveIn(url.searchParams.get("lib"), url.searchParams.get("path"));
      if (!dir) return json(res, 404, { error: "no such library" });
      return json(res, 200, sceneInfo(dir));
    }
    // The take the Workshop just recorded, as audio the page can scrub.
    // Previewing a sub-sample is a range of ONE file rather than a file each:
    // cutting them on the server would mean writing dozens of wavs to answer a
    // hover, and the browser can already start and stop inside a file.
    if (url.pathname === "/api/take-audio" && req.method === "GET") {
      const take = safe(url.searchParams.get("take") || "");
      const dir = path.join(TAKES, take);
      if (!take || !fs.existsSync(dir)) return json(res, 404, { error: "no such take" });
      const wav = firstWav(dir);
      if (!wav) return json(res, 404, { error: "no audio in that take" });
      return sendAudio(req, res, wav);
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
