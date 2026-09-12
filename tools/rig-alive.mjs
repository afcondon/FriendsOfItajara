#!/usr/bin/env node
// **Is the rig actually there?** Two questions, asked separately, because
// answering them together is how a working ES-9 gets called dead.
//
//   1. IS THE INPUT STREAM LIVE?  Every ES-9 input is DC-coupled, so input 1
//      sits at about -0.0234 with nothing patched and nothing playing. That
//      offset is a property of the stream: present means the ES-9 is being
//      read, exactly 0.000000 means it is not. No modular is involved, so this
//      answer holds whatever the rack is doing.
//
//   2. IS THE SIGNAL PATH LIVE?  Fire a gate, listen for a sound. This one
//      needs a module, and the module has to be one that is powered whenever
//      the ES-9 is — i.e. in the ES-9's own rack. Andrew, 2026-09-12: the rig
//      has many supplies, so a probe module on a different one makes "the rig
//      is up" and "the thing under test is up" two facts pretending to be one.
//      Wire the Rample or the QuadDrum for this and the pretence goes away.
//
// Measured 2026-09-12, and the reason this exists: a BIA on another supply
// gave no rise, the daemon reported `audioAlive: true`, and the ES-9 was
// declared dead. Its DC was -0.022918 throughout.
//
//   node rig-alive.mjs [--src N] [--bus N] [--level L] [--hits N]

import fs from "node:fs";

const arg = (k, d) => {
  const i = process.argv.indexOf(k);
  return i >= 0 && process.argv[i + 1] ? process.argv[i + 1] : d;
};
const SRC = Number(arg("--src", 4));        // itajara source index, 1-based
const BUS = Number(arg("--bus", 15));       // es9-daemon bus; 8..15 are the panel
const LEVEL = Number(arg("--level", 0.5));
const HITS = Number(arg("--hits", 5));
const NAME = "rig-alive-probe";

const WS = "ws://127.0.0.1:23028";
const CV = "http://127.0.0.1:3029/api/cv";
const TAKES = `${process.env.HOME}/.itajara/takes/${NAME}`;

const sleep = (ms) => new Promise((r) => setTimeout(r, ms));
const jack = (b) => (b >= 8 && b <= 15 ? `ES-9 jack ${b - 7}` : `CV bus ${b}`);

async function capture(ws, secs, fire) {
  ws.send("cdrop");
  await sleep(200);
  ws.send("carm0");            // no head trim: we want the silence too
  ws.send("cstop0");           // runs until told
  ws.send("cap" + SRC);
  await sleep(300);
  if (fire) await fire();
  await sleep(secs * 1000);
  ws.send("cend");
  await sleep(400);
  ws.send("cw" + NAME);
  await sleep(1200);
}

// Read the take's left channel. float32 stereo is what the daemon writes.
function readTake() {
  const f = fs.readdirSync(TAKES).find((n) => n.endsWith(".wav"));
  const buf = fs.readFileSync(`${TAKES}/${f}`);
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
  if (tag !== 3 || bits !== 32) throw new Error(`unexpected format tag ${tag}/${bits}`);
  const n = Math.floor(dLen / (ch * 4));
  const xs = new Float32Array(n);
  for (let i = 0; i < n; i++) xs[i] = buf.readFloatLE(dOff + i * ch * 4);
  return { rate, xs };
}

function stats(xs) {
  let sum = 0, peak = 0;
  for (const v of xs) { sum += v; if (Math.abs(v) > peak) peak = Math.abs(v); }
  const dc = sum / xs.length;
  let sq = 0;
  for (const v of xs) sq += (v - dc) * (v - dc);
  return { dc, peak, rms: Math.sqrt(sq / xs.length) };
}
const dB = (v) => (v > 0 ? (20 * Math.log10(v)).toFixed(1) : "-inf");

const ws = new WebSocket(WS);
ws.onerror = (e) => { console.error("cannot reach the looper daemon:", e.message ?? e); process.exit(1); };
ws.onopen = async () => {
  // ── 1. the input stream, with nothing asked of the modular ──────────────
  await capture(ws, 1.0, null);
  const quiet = stats(readTake().xs);
  const live = Math.abs(quiet.dc) > 0.005;
  console.log(`\n1. THE INPUT STREAM   source ${SRC}`);
  console.log(`   DC      ${quiet.dc >= 0 ? "+" : ""}${quiet.dc.toFixed(6)}  (${dB(Math.abs(quiet.dc))} dBFS)`);
  console.log(`   AC rms  ${quiet.rms.toFixed(6)}  (${dB(quiet.rms)} dBFS)`);
  console.log(live
    ? "   LIVE — the DC-coupled offset is there, so the ES-9 is being read."
    : "   DEAD — no DC offset at all. The ES-9 input is not being read, whatever\n" +
      "   every status field says. Restart es9-daemon; if that does not do it,\n" +
      "   check the ES-9 is in Hosted mode.");

  // ── 2. the signal path, which needs a module on THIS rack's power ───────
  const fire = async () => {
    for (let i = 0; i < HITS; i++) {
      await fetch(CV, { method: "POST", headers: { "content-type": "application/json" },
                        body: JSON.stringify({ pulse: { bus: BUS, level: LEVEL, ms: 10 } }) });
      await sleep(500);
    }
  };
  await capture(ws, HITS * 0.5 + 0.8, fire);
  const hit = stats(readTake().xs);
  const rose = hit.rms > Math.max(quiet.rms * 8, 0.002);
  console.log(`\n2. THE SIGNAL PATH    gate on ${jack(BUS)} at ${LEVEL}, ${HITS} hits`);
  console.log(`   peak    ${hit.peak.toFixed(6)}  (${dB(hit.peak)} dBFS)`);
  console.log(`   AC rms  ${hit.rms.toFixed(6)}  (${dB(hit.rms)} dBFS)   vs ${dB(quiet.rms)} quiet`);
  console.log(rose
    ? "   LIVE — the gate goes out and the sound comes back."
    : live
      ? "   NOTHING CAME BACK, and the input stream is fine — so this is the\n" +
        "   modular: the probe module's power, the gate cable, or the audio\n" +
        "   cable. Use a module in the ES-9's OWN rack, or its supply is a\n" +
        "   second fact this test cannot see."
      : "   Untestable while the input stream is dead — fix 1 first.");
  console.log("");
  process.exit(live && rose ? 0 : 1);
};
setTimeout(() => { console.error("timed out"); process.exit(2); }, 40000);
