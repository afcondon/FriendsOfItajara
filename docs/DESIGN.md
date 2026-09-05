# Friends of Itajara — one looper, a face per module

**Status:** built to first light 2026-09-04, harvesting the same night, and
in its own repository from then. Picks up
`producing-with-your-feet/docs/DESIGN-HARVEST.md` §6 (three strata) and §7
step 10, which is where the idea came from; this is the design of the app
itself.

---

## 1. What it is

A looper for people with a sample-playing eurorack module, a sound source
and a Mac: the Itajara daemon with a page on it. **One app, and a face per
module.** When we say "Arbhar's Friend" we mean the Friend wearing its
Arbhar face; Morphagene's, Rample's and QD's Friends are the same bundle
with a different row of one table. The differences between them — how many
layers the module holds, how long one is, what a loop and a layer *become*
on the stick, how the daemon should be started — are data, so they are
`Friend.Face` and nothing else knows them.

**And the face is not the skin.** A face is a configuration; how the page
looks is a stylesheet. The two vary independently: `friend.css` is the plain
skin (light, sans-serif, a grid of cards), and a skin is a second stylesheet
in `static/skins/`, chosen by `?skin=<name>`, that replaces it and leaves the
class names alone.

The first skin is **`river`**, for the Arbhar face, and it is deliberately
*not* a copy of Instruo's panel — no black, no gold, no attempt at their
lettering, which would only ever be an imperfect clone. It takes one thing
from the module, the thing at the heart of its design: the **Granular
Stream**, the river of blue LEDs that runs across the face — here, the
water.

Its first cut (2026-09-04) was a sketchbook: grid paper, hand lettering,
hand-drawn boxes. Andrew's verdict the next morning: what he asked for, but
too busy — too much text, too many labels. The brief became **an
architectural drawing**: still hand-lettered, but with the precision and
sparseness of pre-CAD drafting, and the boxes and the river reimagined as
**a cousin of Fallingwater**. So the skin is now an elevation. Each loop is
a *slab* — a wide, thin terrace with a heavy shadow line beneath it,
with its own start and its own length, every one crossing a hatched stone
core that rises a quarter of the way across the sheet — staggered, so the
eight step across the page the way the house does rather than lining up on
a margin. The water runs along the foot of the
sheet as a shelf and spills over an edge into a widening fall. Loop numbers
are the circled room numbers of a plan; lengths are dimension lines with
ticks; the layers are the ribbon windows, lettered α–ζ because that is what
they become; every button is a bare glyph whose word appears beneath it on hover, in a
line that is always there in transparent ink so nothing shifts;
and the daemon's shape, the face's capacity and any warning go into a
**title block** bottom-right, where a drawing keeps its data. One drafting
hand throughout (Architects Daughter), capitals for labels, and almost
nothing written. The water is one SVG, base64 in the stylesheet, an absolute
layer at the foot of the page; the root element is left unpainted so the
body's paper propagates to the canvas and the water's negative z-index sits
between paper and page. Its filters have large absolute regions on purpose —
the default region clips a blurred stroke to a box, which is how a fall
turns into a stripe.

## 2. Where the pieces live

```
itajara/                 (github.com/afcondon/itajara)
  daemon/    the engine (Rust)
  client/    A + B: socket, snapshot, verbs, recipes,
             Data.Looper.Duty (the vocabulary), Data.Looper.Machine (meaning)
  surface/   Halogen views over the client's types:
             Itajara.Surface.Wave (a layer's envelope as the loop plays it)
             Itajara.Surface.Edit (the Edit panel), and looper.css —
             one rendering of the class names they draw with
  tools/     check-verbs, check-snapshot
friends-of-itajara/      (github.com/afcondon/FriendsOfItajara — this repo)
  src/       Friend.Face (the table), Friend.App (the page), Friend.Http, Main
  server.mjs the seam: notes and the harvest
  static/    the page and the plain skin
```

Two repositories, checked out side by side: the Friends reach `client` and
`surface` by path (`../itajara/…`). The daemon and its client are the thing
every surface needs; the Friends are one surface, and a separate repo keeps
that so — a pedalboard app is another, and neither owns the other.

`producing-with-your-feet` also consumes `client` and `surface` by path and
was the first consumer of both — its Looper page's Edit panel *is*
`Itajara.Surface.Edit` and its slots draw with `Itajara.Surface.Wave`.
That is what makes the seam real rather than claimed: one source, two apps,
the same picture. What the pedalboard keeps is everything with feet on it —
`Data.Looper.Banks` (MC6), `Data.Looper.Twister`, `Switchboard`, the slot
grid in the pedal's order — and one number, `Data.Looper.Surface.nLoops = 8`,
which is what *that* surface is laid out for. The machine no longer has a
loop count: "all loops" is the length of the daemon's array.

The three strata of HARVEST §6 are now three packages, and the leak it
named (`Duty` defined in the MC6 module) is closed by construction: the
client package cannot import a bank.

---

## 3. The page

One Halogen component, `Friend.App`. From the top:

- **Header** — the face's name, what it writes for whom, and the socket's
  truth in one line: looking, connected (with the URL), connected-but-silent
  (the age, so a dead engine behind a live socket is not "connected"), lost,
  or absent. Absent shows the face's daemon command, with `<device>` left for
  the reader.
- **Shape** — what the daemon reports (`nLoops × maxLayers × maxSecs` at the
  rate) beside what the face needs, and a warning when the daemon was started
  with fewer layers than a unit holds: *"start it with --layers 6 to fill
  one"*. The check is the face's, `Face.shapeNote`.
- **Loop cards**, one per loop the daemon has, laid out by the snapshot. Each
  says its state, its length (bars too when it is on the grid), and where it
  goes — *→ scene 3* — because that is the thing the harvest will do and the
  reason a loop's layers are grouped as they are. Each layer is a row: a
  checkbox (in or out of the mix, `ly<n>1|0`) and its envelope as the loop
  now plays it. Six buttons: Record (which says what the next press *does*,
  because `r` is one verb that opens, closes, overdubs or cancels), Overdub,
  Play/Stop, Undo, Clear, Edit.
- **Controls** — click, stop all, clear all; a take name; **Save for
  \<module\>**.
- **Log** — the daemon's acks by sequence and the app's own notes, newest
  first.
- **Edit** — the shared panel in a modal over the loop in focus, asking for
  its peaks only when the picture would differ (loop, layer count, newest
  birth).

Every button goes through `Machine.perform` against `rigOf` — the same
function a footswitch goes through in the pedalboard — with an empty grab
list, since this page can reach every loop. The one exception is the named
save: no switch can carry a name, so the vocabulary has no slot for one, and
`SaveAll` sends `<n>w<take>-<n>` per loop with material through the same
`runAction` (logged, sent, refused-if-no-daemon) but not through `perform`.

**No Twister, no MC6, no MIDI at all** in this first cut. HARVEST §6 sized a
Twister-for-those-who-have-one as `Data.Looper.Twister` plus a WebMIDI port;
that module is 1,650 lines and still in the pedalboard, and moving it is C's
problem (§7 step 11), not the Friend's.

---

## 4. Saving, harvesting, and the datasheet

Three buttons, in the order they are used.

**Save take** sends one verb, `exl<take>`, and the daemon writes
`~/.itajara/takes/<take>/loop-<n>/` for every loop that holds something —
the layers raw, a version-1 `take.json` in each folder so it reloads as a
plain take, and one `export.json` at version 2 for the set carrying the
window, rotation, bars, tempo, source and per-layer gain and birth. The
edit is recorded beside the material rather than applied to it. A "saved ✓"
appears when the server sees the folder.

**Notes** is what only the player knows: title, key, BPM, timbre, intended
use, tags, free text, and a row per loop. Saved as `notes.json` in the take.
The daemon's facts — length, bars, tempo, source — are not asked for twice;
the datasheet joins the two.

**Harvest to Arbhar** runs `msm harvest`. The mapping, from the firmware 2.0
manual: **a loop is a library bank** (`_arbhar_library/<bank>_<layer>_sample/`,
six single-layer slots the panel loads one at a time, so scanning the Layer
knob within a bank walks the takes that were played against each other) **and
a scene** (`_arbhar_scenes/<bank>_<scene>_scene/`, up to six files loaded into
the six layers in one action). Each file is ten seconds of the loop's pass
followed by the three seconds that follow it — the wrap, which is the audio
that does come next — at 24-bit, 48 kHz, stereo, named `<k>_<take>_loop<n>`
so the leading digit keeps the layer order the module loads by. No
configuration file is written: audio with no file *is* "Load Layers" by the
manual's own definition. Layers switched off in the take are left out unless
asked for; a slot already holding audio is kept unless `--overwrite`. The
form takes the stick (auto-detected as the mounted volume with an
`_arbhar_library`), the first bank and first scene, and offers a dry run.

**The datasheet** is written twice: `datasheet.json` and `DATASHEET.md` in
the take, and `_harvest/<take>.json` and `.md` on the stick, so the stick can
be read back by something that was not there when it was written. It
carries the notes, the daemon's facts per loop, every placement (loop,
layer, slot, file, seconds, gain) and the remarks (what was left out, what
was full). That file is the index a to-be-written **Arbhar archive manager**
would read: one record per harvest, with enough in it to find "the D minor
bowed thing in bank 3" without listening.

**The seam.** The browser cannot spawn a process and the daemon must not, so
`server.mjs` — Node, zero dependencies, one file — serves the page
and holds the two things the page cannot do: write `notes.json`, and run
`msm harvest` (`MSM=` overrides the binary; `cargo install --path
SamplesProject/msm` puts it on the path). Served statically instead, the
page still loops and saves; Notes and Harvest simply cannot reach anything.

Not built, in the order they are likely wanted:

1. Morphagene, Rample and QD faces made real: one mapping each in
   `msm::harvest`, on the Arbhar's pattern.
2. Per-loop level and source on the card. The daemon has both; the page
   shows neither yet.
3. Keyboard: number keys select a loop, space is Record. Recording with a
   mouse is a compromise, and the first thing a player will ask for.
4. The Instruo skin, out of tree.
5. The Twister, for those who have one.
6. The archive manager: reads `_harvest/*.json` off any stick and the
   `datasheet.json` in every take, and answers "what is on this stick" and
   "where did that take go".

## 5. Running it

```
git clone https://github.com/afcondon/itajara
git clone https://github.com/afcondon/FriendsOfItajara friends-of-itajara
cd itajara/daemon && cargo build --release
./target/release/itajara loop --device <device> --layers 6 --yes
cd ../../friends-of-itajara && make serve   # bundles, copies looper.css, node server.mjs on :3029
open http://localhost:3029/?face=arbhar
```

The page connects to `ws://127.0.0.1:3028` (the client's `defaultUrl`) and
reconnects by itself; start the daemon in either order. Harvesting needs
`msm` on the path (`cargo install --path SamplesProject/msm`).
