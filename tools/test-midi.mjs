#!/usr/bin/env node
// **Test MIDI for a card the Workshop built.**
//
// The card knows what each kit is — how many velocity layers, how many slices,
// whether it is stereo — so the files that exercise it can be generated rather
// than typed. Which matters because the interesting questions are all of the
// form "does the module do what the card says", and a hand-made test file
// encodes a second opinion about that rather than the card's own.
//
//   node tools/test-midi.mjs [outdir]
//
// Everything is on MIDI channel 1, which is `SETTINGS > CHANNEL 1`. The
// trigger notes are SP1..SP4 = 60..63, which is the module's default and is
// also a setting — if the module is set otherwise these files address nothing.

import fs from "node:fs";
import os from "node:os";
import path from "node:path";

const CARD = path.join(os.homedir(), ".itajara", "workshop", "card.json");
const SAMPLES = path.join(os.homedir(), ".itajara", "workshop", "samples");
const OUT = process.argv[2] || path.join(os.homedir(), "Desktop", "rample-tests");

const PPQ = 480;
const CH = 0;                       // channel 1
const SP = [60, 61, 62, 63];        // SP1..SP4
// The start point has to land before the note or the module plays the slice it
// was already on. 40 ms measured; a beat at any sane tempo is far longer.
const SETTLE = 0.040;

// ---------------------------------------------------------------- SMF writing

const vlq = (n) => {
  const out = [n & 0x7f];
  n >>= 7;
  while (n > 0) { out.unshift((n & 0x7f) | 0x80); n >>= 7; }
  return out;
};

class Track {
  constructor(bpm) {
    this.bpm = bpm;
    this.ev = [];                   // { tick, bytes }
  }
  at(sec, bytes) {
    this.ev.push({ tick: Math.round(sec * (this.bpm / 60) * PPQ), bytes });
  }
  cc(sec, num, val) { this.at(sec, [0xb0 | CH, num & 0x7f, val & 0x7f]); }
  pc(sec, prog) { this.at(sec, [0xc0 | CH, prog & 0x7f]); }
  note(sec, pitch, vel, len) {
    this.at(sec, [0x90 | CH, pitch, Math.max(1, Math.min(127, vel))]);
    this.at(sec + len, [0x80 | CH, pitch, 0]);
  }
  bytes() {
    // A tempo meta first, so the file plays at the tempo it was written for.
    const usPerQuarter = Math.round(60000000 / this.bpm);
    const head = [0xff, 0x51, 0x03,
      (usPerQuarter >> 16) & 0xff, (usPerQuarter >> 8) & 0xff, usPerQuarter & 0xff];
    // Stable sort: note-offs before note-ons at the same tick would steal a
    // retrigger, so ties keep insertion order and insertion order is musical.
    const evs = this.ev.map((e, i) => ({ ...e, i }))
      .sort((a, b) => a.tick - b.tick || a.i - b.i);
    const out = [...vlq(0), ...head];
    let last = 0;
    for (const e of evs) {
      out.push(...vlq(e.tick - last), ...e.bytes);
      last = e.tick;
    }
    out.push(...vlq(0), 0xff, 0x2f, 0x00);
    const len = out.length;
    return Buffer.from([
      0x4d, 0x54, 0x68, 0x64, 0, 0, 0, 6, 0, 0, 0, 1, (PPQ >> 8) & 0xff, PPQ & 0xff,
      0x4d, 0x54, 0x72, 0x6b, (len >> 24) & 0xff, (len >> 16) & 0xff,
      (len >> 8) & 0xff, len & 0xff,
      ...out,
    ]);
  }
}

// Select a bank and kit: CC00 is the bank letter, Program Change is the kit.
const select = (t, letter, kit) => {
  t.cc(0, 0, letter.charCodeAt(0) - 65);
  t.pc(0.01, kit);
};

// The CC value that lands in the MIDDLE of slice k of n, so a rounding error
// cannot put it in the neighbour.
const slicePoint = (k, n) => Math.min(127, Math.round(((k + 0.5) * 128) / n));

// The start point for a voice: CC(voice * 10 + 4).
const startCC = (voice) => voice * 10 + 4;

// ------------------------------------------------------------------ the tests

const write = (name, t, why) => {
  fs.mkdirSync(OUT, { recursive: true });
  fs.writeFileSync(path.join(OUT, name), t.bytes());
  console.log(`  ${name.padEnd(34)} ${why}`);
};

const card = JSON.parse(fs.readFileSync(CARD, "utf8"));
const letters = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
console.log(`writing to ${OUT}\n`);

card.banks.forEach((bank, bi) => {
  const letter = bank.letter || letters[bi];
  bank.kits.forEach((kit, ki) => {
    const slot = `${letter}${ki}`;
    const v1 = kit.voices["1"];
    if (!v1) return;
    const stack = Array.isArray(v1.layers) ? v1 : { layers: [{ set: v1.set }], ...v1 };
    // **How many layers the MODULE will see**, which is not how many entries
    // the card model holds. A sliced voice is one joined file per entry; an
    // unsliced one globs its set, so ten samples in a set are ten layers and
    // the model still records a single entry. Counting entries missed the
    // ten-kick velocity stack entirely — the one test everything else is
    // gated on.
    const wavs = (set) => {
      try {
        return fs.readdirSync(path.join(SAMPLES, set))
          .filter((f) => f.toLowerCase().endsWith(".wav") && !f.startsWith(".")).length;
      } catch { return 0; }
    };
    const nLayers = stack.sliced
      ? stack.layers.length
      : stack.layers.reduce((n, l) => n + wavs(l.set), 0);
    const slots = stack.slots || 0;
    const mode = (kit.layers || "manual").toLowerCase();

    // --- velocity across the layers -------------------------------------
    // One note per layer plus a couple beyond each end, so a stack that is
    // not being picked by velocity is obvious rather than plausible.
    if (mode === "velocity" && nLayers > 1) {
      const t = new Track(100);
      select(t, letter, ki);
      const n = nLayers + 2;
      for (let i = 0; i < n; i++) {
        const vel = Math.max(1, Math.round(((i + 0.5) / n) * 127));
        t.note(0.5 + i * 0.9, SP[0], vel, 0.4);
      }
      write(`${slot}-velocity.mid`, t,
        `${n} notes, velocity 1..127, across ${nLayers} layers on SP1`);
    }

    // --- the slices, in order and then not -------------------------------
    if (slots > 1) {
      const inOrder = new Track(88.7);
      select(inOrder, letter, ki);
      const step = 60 / 88.7 / 4;             // a sixteenth
      for (let k = 0; k < slots; k++) {
        const at = 1.0 + k * step;
        inOrder.cc(at - SETTLE, startCC(1), slicePoint(k, slots));
        inOrder.note(at, SP[0], 100, step * 0.9);
      }
      write(`${slot}-slices-in-order.mid`, inOrder,
        `${slots} slices at 88.7bpm sixteenths — do they land on the beat?`);

      // Reversed, which is the case the grid phase was fixed for: in order a
      // slice's head can carry its neighbour's tail unnoticed, and out of
      // order it cannot.
      const shuffled = new Track(88.7);
      select(shuffled, letter, ki);
      for (let i = 0; i < slots; i++) {
        const k = slots - 1 - i;
        const at = 1.0 + i * step;
        shuffled.cc(at - SETTLE, startCC(1), slicePoint(k, slots));
        shuffled.note(at, SP[0], 100, step * 0.9);
      }
      write(`${slot}-slices-reversed.mid`, shuffled,
        `the same ${slots} backwards — any flam is the grid phase`);
    }

    // --- both axes at once ------------------------------------------------
    if (slots > 1 && nLayers > 1 && mode === "velocity") {
      const t = new Track(100);
      select(t, letter, ki);
      let at = 0.5;
      for (let L = 0; L < nLayers; L++) {
        const vel = Math.max(1, Math.round(((L + 0.5) / nLayers) * 127));
        for (let k = 0; k < slots; k++) {
          t.cc(at - SETTLE, startCC(1), slicePoint(k, slots));
          t.note(at, SP[0], vel, 0.28);
          at += 0.32;
        }
        at += 0.5;                            // a breath between layers
      }
      write(`${slot}-two-axes.mid`, t,
        `${nLayers} velocities x ${slots} slices — layer and start point together`);
    }

    // --- which trigger notes answer --------------------------------------
    // A stereo voice claims the voice after it, so a stereo kit should answer
    // on SP1 and SP3 and be silent on SP2 and SP4. That is a thing to hear,
    // not to infer.
    {
      const t = new Track(100);
      select(t, letter, ki);
      SP.forEach((n, i) => t.note(0.5 + i * 1.2, n, 100, 0.9));
      write(`${slot}-triggers.mid`, t,
        stack.stereo
          ? "SP1..SP4 in turn — stereo should answer on 1 and 3 only"
          : "SP1..SP4 in turn — which voices have anything on them");
    }
  });
});
