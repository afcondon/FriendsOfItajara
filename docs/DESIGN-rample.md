# Rample's Friend

> **Superseded in part, 2026-09-08.** The plan is now
> `afc-work/docs/kb/plans/rample-as-an-instrument.md`, which carries what the
> module *measurably* does — this document was written before any of it was
> tested on the hardware, and the module disagreed with the manual in four
> places. What stands here: the two modes of use, the kinds, and the two-card
> reasoning. What has moved on: layer ordering (a byte sort, not natural), the
> twelve-layer ceiling (hard, measured), `.rpl` (CBOR we can write), SLICER, and
> the scope — the Rample is now an instrument to play, not only a card to
> curate.

> **See also, 2026-09-08:** `afc-work/docs/kb/plans/friend-as-sampler.md` — the
> Friend as a sampling instrument rather than a card writer: four record
> buttons by scale, conducted hit sessions, one classified library, and the
> card as an export. That is the plan the *page* follows; this document is
> about the card.

Design notes, 2026-09-07. Not built yet; this is the shape agreed in the
session that surveyed the module, the manual and what is already on disk.

## Which firmware this describes — read this first

**The module runs rampleOS 3.00** (February 2026; confirmed by Andrew in
SETTINGS > INFO, 2026-09-07). **Everything below comes from the older "wave
system" quickstart manual**, which predates it. Treat the file-layout rules as
probable, not certain, until the 3.0 documentation is checked.

Reassuringly, the 3.0 binary's own strings show the model this design rests on
is intact: the layer modes are still `MANUAL`, `RANDOM`, `CYCLIC`, `R CYCLIC`,
`VELOCITY`, and the per-voice parameters are still `SP1`–`SP4` × `PITCH`,
`BIT`, `FILTER`, `FREEZE`, `LEVEL`, with `ALL …` variants. So voices, layers
and the CC map survive into 3.0.

What is visibly *new* in 3.0 and absent from the old manual is a **`VOICE FX`
section** with named processors — `AGGRESSIVE FET`, `SLOW OPTO`, `PUNCH VCA`,
`LEVELING AMP`, `PARALLEL COMP`, `PEAK LIMITER`, `DRUM TRANSIENT`, `COMPRESS`,
`GLUE`, `TAPE`, `BLAST`, `CLASSIC`, `FLIP` — plus `CHROMATIC`, `VU-METER`,
`AUTOSAVE`, `PROG CHG` and `INFO` in the menus. Those are additive and do not
touch the filesystem.

**Still to confirm against 3.0 documentation**, because each would change what
a writer must do: the 12-layer ceiling, the accepted sample formats (the old
manual says 44.1 kHz, 16- or 8-bit, mono, with stereo "see the online manual"),
and whether layer ordering is still a plain filename sort.

Firmware lives as `rample.bin` at the card root and **only flashes when the
encoder is held at power-on** — so a `.bin` sitting on a card is inert and
cannot downgrade anything by accident.

## The module, as the manual states it

Squarp Rample, 4 voices, one MIDI channel per unit.

| | |
|---|---|
| Kits | 2600, folders named `?X` — `?` a letter `A`–`Z`, `X` a number `0`–`99` — **flat at the card root** |
| Voices | 4. **The first character of a filename is the voice**, `1`–`4`. The rest is free text |
| Layers | up to **12 per voice**, ordered by the **alphabetical sort of the filename** |
| Format | `.wav`, **16-bit or 8-bit, 44100 Hz** |
| Length | **minimum 50 ms**; no maximum at all — hours-long samples are offered |
| Stereo | allowed, but **"a stereo sample will fill 2 mono voices"** — a stereo file in voice 1 consumes voices 1 *and* 2, so a kit of stereo samples holds **two**, not four |
| To open at all | **"A kit folder must include at least the voice 1 sample (a valid .wav file starting with `1`). Otherwise, you will not be allowed to open this kit."** |

Those last three come from the **online** manual at
<https://squarp.net/rample/manual/>, not the quickstart PDF, and the last is the
sharpest silent failure on the module: a kit with voices 2, 3 and 4 and no
voice 1 looks entirely normal in a file listing and **cannot be opened**. There
is nothing to hear and nothing to see. `msm kit read` reports it as
`UNOPENABLE`, ranked above every other fault.

Also from the online manual, and directly serving the chromatic use: each voice
has a **MIDI note range** (Settings → SP1…SP4), which gives keyboard splits
across two-octave ranges *and* **layer selection by note**. Twelve layers and
twelve semitones are not a coincidence you have to exploit by hand.

Everything is reachable over MIDI, which matters later:

- kit select — **`PC` = bank `0`–`25` (`A`–`Z`), `CC00` = kit `0`–`99`**, `CC100`/`CC101` = prev/next. Measured on the module 2026-09-09: this document had it the other way round, which is the conventional way and not what the Rample does. `CC0=7, PC=3` lands on **D7**, not H3.
- voice parameters — `CC(voice × 10 + p)`, so `CC10` is SP1 pitch and `CC38` is SP3 level:

| p | 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 |
|---|---|---|---|---|---|---|---|---|---|---|
| | pitch | bits | filter | freeze | start | length | env | run mode | level | **layer** |

- `STORE` writes the current kit's parameters and assignments back to the card
- `LAYERS` (a setting) chooses how layers are picked: `MANUAL`, `RANDOM`,
  `CYCLIC`, `Reverse CYCLIC`, or by `VELOCITY MIDI IN`

## The card itself, read 2026-09-07

The physical card was mounted and inventoried, read-only. 15 GB, 9% used,
**246 kit folders**.

**A warning about what this evidence is.** An earlier draft of this document
said the module "demonstrably plays" these kits. It does not say that any more,
because nothing here was ever played. Presence on the card is evidence of *what
Squarp and the artists shipped* — a source of hypotheses — and nothing at all
about what the module accepts. The card refutes the inference by itself: five of
its kits are damaged (below). **No naming rule in this document should be relied
on until a test kit has been played on the module.**

Kits per bank:

| A | B | C | D | E | F | J | K | M | N | O | P | R | S | U | W | X | Z |
|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|
| 9 | 8 | 9 | 8 | 8 | 10 | 6 | 5 | 15 | 10 | 12 | 7 | 8 | **68** | 10 | **34** | 8 | 11 |

**Free letters: `G` `H` `I` `L` `Q` `T` `V` `Y`** — eight of them, 800 slots.
*This dissolves the collision question below: our kinds need not overwrite
anybody.* (`S` is not spare — its 68 kits are all `WS-…`, Waveshaper overflow
from bank `W`.)

At the card root, beside the kit folders:

- **15 `.rtf` legend files**, one per bank letter, each naming an artist
  (`D - RICHARD DEVINE.rtf`, `K - KANGDING RAY.rtf`). **A sidecar index is
  therefore safe by direct observation, not by inference** — and one letter =
  one kind is the module's own idiom, not our imposition.
- **`rample.bin`**, 275 KB — the firmware. Do not touch.
- **`_save/`** — the parameter store. `STORE` writes **`<kit>.rpl`** here, named
  by slot (`O1.rpl`, 277 bytes), beside `settings.rpl` (240 B) and
  `global_assign.rpl` (61 B), plus a zero-length `autosave_C1.rpl`. **Kit
  parameters live outside the kit folder and are addressed by slot.** So
  writing audio never disturbs settings, and settings could be authored
  independently of audio. Small enough to be worth decoding.
- OS litter: `System Volume Information`, `.Spotlight-V100`, `.fseventsd`.

### The card has filesystem damage

FAT32, and five kits are damaged. Found by a full read-only scan; nothing was
written or repaired.

| Kit | Bad | Of | What |
|---|---|---|---|
| `C4` | 30 | ? | directory entries unreadable |
| `E2` | 6 | 48 | names read, `stat` fails — bad cluster chain |
| `F5` | 11 | 13 | **mangled 8.3 short names** (`7rwb\t~1.WAV`) — long-name entries gone |
| `M13` | 3 | ? | directory entries unreadable |
| `M14` | 7 | 7 | the whole kit |

Two things follow.

**Image the card before writing anything to it.** This is also a better
candidate explanation for past unexplained failures than either audio format or
filename convention, and it is the kind of fault that spreads.

**`F5` raises a hypothesis worth testing.** Those `~1.WAV` names are FAT32
*short* names surfacing because the long-name entries were damaged. If the
module's FAT driver ever reads short names rather than long ones, then the
alphabetical sort — and therefore the layer order — is computed on mangled
eight-character names, not the ones we chose. Untested, and nasty if true.

### A file that is silently ignored, on the card, right now

`X5/PAR 12.wav`. An ordinary filename, a valid wav, in a real kit folder. **The
module ignores it, because the first character is not `1`–`4`.** No error and no
sound: exactly the failure mode where one goes looking at audio formats. This
single file is the argument for the whole design. (`Z9/Neuer Ordner` is just an
empty stray folder, and harmless.)

**875 AppleDouble `._` files.** The card is full of them, including
`._rample.bin` and one per kit folder. This is exactly the class of file that
broke the Arbhar harvest mid-bank; msm has since learned to filter them, and
anything reading or writing this card must do the same. Worth cleaning off.

### Four naming conventions, all on one card, all valid

Only the first character means anything to the module. Everything after it is
human convention, and the card proves nobody agrees:

| Kit | A file | Convention |
|---|---|---|
| `A0` | `1 KICK LOW 01.wav` | voice, space, NAME, index |
| `D0` | `1. Kick.wav` | voice, period, Name — Squarp's factory style |
| `K0` | `1_KR_inC_JP_LV_A_Cm.wav` | voice, underscore, tokens |
| `Z0` | `1 baffsample.wav` | voice, space, name |
| `RamKits/D0` | `1 a BD A 808 Decay C 04_16bit.wav` | **voice, space, layer letter, name** |

The last is ours, and `a`–`l` covers exactly the twelve layers allowed, sorts
correctly by construction, and stays readable.

**But do not derive the rules from this table.** Be conservative: take the
narrowest envelope Squarp themselves ship — `1. Kick.wav`, a digit, plain
printable ASCII, short, lowercase `.wav` — and place our layer letter inside it.
The wider conventions on the card are evidence that somebody once wrote them,
not that the module read them.

**And `K0` is direct evidence for the general-ordinal decision:** its four
layers are `A_Cm`, `B_Csus2`, `C_Csus4`, `D_Cm7` — the ordinal is *harmony*,
not dynamics. Kangding Ray got there first.

## What is already on disk

Surveyed under `/Volumes/Crucial4TB/Samples/` — the sample archive, and the
root that should become a library. **None of this starts from zero, and the
existing conventions are good ones: conform, do not reinvent.**

- **`RamKits/`** — 39 built kits, banks `D`–`M`, 15 to 48 files each (48 = 4
  voices × 12 layers, i.e. full). **Note the collision:** these letters are all
  spoken for on the card. Rebank them into the free eight.
- **`RamKitsData.json`** — 171 KB, March 2024. The manifest that built them:
  `{ id, name: "D0", slots: [ { files: [<url>, …] } × 4 ] }`. A kit is a slot
  address plus four ordered lists of source files. **This is already the right
  model**; what it lacks is a kind per bank, a human name, and what the layer
  ordinal *means*.
- **`Samples from Mars collection`, `SamplesFromMarsWAV-16bit`** — the purchased
  library, bought for the Rample and never used. The `_16bit` tree is already
  converted, and `RamKitsData.json` sources from it.
- **`Rample card template/`, `Rample currently loaded/`** — abandoned. Both are
  empty `A/A0` scaffolding with **zero wav files**, and both use a *nested*
  layout the card does not want. Ignore them; do not resurrect the nesting.

**A constraint the archive imposes, now measured:** `/Volumes/Crucial4TB/Samples`
is **191 GB, 7,815 directories within depth 4, and 441,588 wav files**.

The Library panel's `/api/library` walks every root and returns the whole tree,
names included, on **every call**. That is right for the takes and the Arbhar
stick image — eleven shelves, ~1,300 names, 400 ms cold. It is hopeless here:
nearly eight thousand `readdir`s over USB and a JSON response of a third of a
million filenames.

**So this root cannot simply be added.** The fix is not caching the walk, it is
not walking: **msm already has the answer** — a SQLite library with `scan`,
`search`, `tag` and `stats` over exactly this kind of archive. For a root of
this size the Library panel should query that index, and keep the live walk for
roots small enough to be authoritative in the moment (takes, a mounted stick, a
card). Two strategies, chosen per root, is the honest shape; one live walk for
everything is not.

## Two cards

Decided 2026-09-07: **one card stays factory, one card is ours.** The factory
content is how you get playing on day one, and it goes generic in your own music
soon after; mixing the two on one card is what makes the addressing a lottery.

The diff between the card and the archive at
`Samples/Rample/Rample Factory Samples/` makes the split cheap:

| | |
|---|---|
| archive | 210 kits, **all 210 also on the card**, none missing |
| card | 246 kits — the same 210, plus **34 Waveshaper** (`W0`–`W33`, with its own `W - WAVESHAPER.rtf`) and **two of ours** (`A7` MamanKuesters, `A8` WestCoastKit) |
| identical | 205 of the shared kits, filename for filename |
| differ | `C4`, `F4`, `F5`, `M13`, `M14` — i.e. **the damaged ones** |

Two facts follow, and they point opposite ways.

**The archive heals the card.** All five damaged kits are present and readable in
it, and a full scan of the archive — 2,801 files — finds *no* corruption at all.
So the factory card can be rebuilt clean rather than repaired.

**But the archive is not a superset.** Thirty-six kits exist **only** on a card
that is visibly degrading: the entire Waveshaper pack and the two of ours.
Restoring from the archive alone would silently drop them. **Copy those off
before anything else happens to that card.**

### What the card-only content actually is

Checked, because the guess was that it came from Samples From Mars. It does not.

**`W0`–`W33` are Waveshaper**, and the card's own credit file says so verbatim:
*"ALL SAMPLES RECORDED AND EDITED BY WAVESHAPER – payhip.com/WaveShaperDrumSamples"*,
followed by a list of `WS 606 Fat Kit1`, `WS 707 kit1`, `WS 808 slowK7`,
`WS Dusty kit`, `WS MicroKit`. A separate purchase. The confusion is a fair one:
Waveshaper's kits are named `WS606`, `WS707`, `WS808`, and Samples From Mars
sells *606 From Mars*, *707 From Mars* and *808 From Mars*. The same drum
machines, a different vendor.

**And Waveshaper is not only in `W`.** Bank `S` — 68 kits — is `WS-DR110`,
`WS-NoisyMemory`, `WS-Anatronix TeK2`, `WS Synthax BD`: sixty-seven of its
sixty-eight are Waveshaper too (`S24` is a lone `GranuSFX-sfx render`). Its
legend file still reads `S - SINK.rtf`. **SINK's factory content is gone — from
the card and from the archive alike** — and the legend has been misdescribing
that bank for at least as long as the archive has existed. Waveshaper spans
`W` + `S` = 101 kits, one more than a bank holds, which is presumably why it
spilled in the first place.

**So the "factory" card is not factory, and cannot be restored to factory from
what we have.** Either re-fetch SINK's bank from Squarp, or accept that `S` is
Waveshaper now and make the legend say so. Either way this is the argument for
the split, in miniature: third-party content crept into an artist's bank and the
label went stale, and nobody could have noticed from the module.

**`A7` is Endorphin.es' Maman Kuesters**, already reshaped for Rample — 4 voices
× 8 layers, `1 MamanKuesters 01`…`4 MamanKuesters 08` — from a source pack laid
out in folders `1`–`8`. A precedent for exactly the conversion this document
proposes. **`A8` (WestCoastKit)** appears nowhere in the archive; origin unknown.

**Correction, made when a second card was read.** An earlier draft said "not
one byte of Samples From Mars is on the card". That was true of the 16 GB card
and false of the setup: **all 39 `RamKits` are on a second, 64 GB card labelled
`RAMPLE`**, filename for filename identical to the archive. They were built in
March 2024 and they *were* loaded. The 191 GB library is less unused than it
looked — though what reached the module is 39 kits of it.

### The `RAMPLE` card: card two already exists

Read 2026-09-07. 64 GB, FAT32, **2% used**. 42 kits in banks `A`–`M`: the 39
`RamKits` plus `A0` (the Alois Plays kit), `B0` (Maman Kuesters — `A7` on the
other card) and `C0` (WestCoastKit — `A8` there). `_save/autosave_E1.rpl` says
it has been in the module.

**It is clean.** 1,189 playable files and **not one unreadable byte** — against
106 on the 16 GB card. Whatever ails that card has not touched this one.

**And it validates the naming convention on real data.** Every file is
`<voice> <letter> <name>` — `1 a 1 KICK LOW 01.wav` — and across all 1,189 the
reader finds **zero ORDER faults**, because lettering the layers makes byte
order and human order agree by construction. That is the empirical case for the
convention, on Andrew's own material rather than on a factory card's.

So the two-card plan is half-built already: **this is card two.** It wants
three things and no redesign — the AIFF kit fixed, the litter swept, and kinds
assigned across the alphabet it is only using half of.

One real fault, and it is the same one twice: **`C0` is twenty AIFF files
wearing `.wav` names**, the identical content to `A8` on the other card. That
kit has never played on either card. Also 1,253 AppleDouble `._` files.

A cosmetic note: `A0`–`C0` carry a doubled prefix, `1 a 1 KICK LOW 01.wav` —
the layer letter was inserted before a name that already began with a voice
digit. Harmless, since only the first character means anything, but a writer
should strip the old prefix rather than nest it.

### The consequence for kinds

The earlier section concluded that our kinds must fit into the eight letters the
factory leaves free — `G H I L Q T V Y`. **A second card retires that
constraint: on our own card the whole alphabet is ours**, 26 kinds of 100 kits.
The cramped allocation was an artefact of sharing, and it is worth not designing
around it.

It also means `RamKits`' existing `D`–`M` banking no longer collides with
anything, and may be able to stay as it is.

### Recipe

- **Factory card** — archive's 210 + `W0`–`W33` + the fifteen `.rtf` legends,
  written to a freshly formatted card. `A7`/`A8` do not belong here.
- **Our card** — `A7`, `A8`, `RamKits`, Samples From Mars, and takes. Empty of
  factory content, and kinds assigned across the full alphabet.

Neither card should carry the 875 AppleDouble `._` files, and neither should be
built by copying the damaged one.

## The spine: two namespaces, and a compiler between them

Third time this shape has appeared, after the Arbhar's library and its stick.

| | you say | the card holds |
|---|---|---|
| Arbhar | library / scene / layer | 6 banks × 36, 36 scenes |
| **Rample** | **kind / kit / voice / layer** | `A0`–`Z99`, voice `1`–`4`, sort order |

The named side is unlimited and meaningful; the positional side is fixed and
mute. `msm` is the compiler. The Friend holds the map. Nothing in the browser
should speak in slots, and nothing on the card should need the app to be legible.

## Decisions taken

**One bank letter = one kind.** `D` = drum kits, `B` = breaks, `A` = ambient,
`V` = vocals — 100 kits each, 26 kinds. Chosen because **bank select is a live
control**: `CC00` steps banks, so stepping banks steps categories on the module
itself. The taxonomy earns its keep in performance, not only in the app. You
say "new drum kit"; it says `D25`. You never type `A0`–`Z99` again.

**The index lives on the card**, in a sidecar at the root — the card is then
self-describing and its names travel with it. Same move as the library-root
notes for the Arbhar. The factory card proves the module tolerates it.

**The layer ordinal is a general ordinal.** This corrects a first draft that
assumed velocity. Layer *N* can mean:

- a **velocity ladder** — softest to hardest (`LAYERS = VELOCITY MIDI IN`);
- a **chromatic set** — 12 layers is exactly 12 semitones, and 12 is exactly the
  cap;
- a **pool** — round-robin or random variation, where order does not matter.

The writer's only guarantee is *layer N in the folder is layer N you asked for*.
What N **means** belongs to the kit, is recorded in the sidecar, and decides
both the `LAYERS` setting and which editor the Friend shows.

**It lives in Rample's Friend** (`?face=rample`), not in a new app.

## Why the Friend, and what already exists there

- The **Library panel** (built 2026-09-07) already browses and auditions any
  directory of samples — which is exactly the front half of this. Samples From
  Mars is a root in `~/.itajara/libraries.json` and needs no new code to browse.
- **`Friend.Face.rample`** already says four loops make a kit and a layer is a
  sample in a voice's stack. The recording model is done.
- **The BIA case needs no new recorder.** "Eight velocity-controlled kicks off
  the Basimilus" is: record eight passes into loop 1, then harvest. It is the
  *harvest* that does not exist.

Two small corrections to make on the way:

- `Face.rample` has `alternates: false`. On Rample one layer sounds per
  trigger, always — that is what `alternates` means. It should be `true`.
- `msm` says 2600 kits / `A0`–`Z99` (right); `msm-web`'s Rample sidebar says 260
  / `A0`–`Z9` and mono-only (wrong on both). One source of truth.

## Two modes of use, one card

Not two formats — the module has no duration limit, so this is entirely a
question of organisation and of which editor is useful.

**Hits.** Four voices, each a stack of short samples. The ordinal carries
meaning (dynamics, or pitch). The editor is a ladder: drag to order, audition in
place, and the writer names `a`, `b`, `c`… to match.

**Long-form.** Breaks, ambient, vocals. Four long samples; layers are alternates
and order is arbitrary. What matters is start point, length and run mode per
voice — `CCx4`, `CCx5`, `CCx7` — which is very nearly the Arbhar's Edit panel,
already built and shared in `itajara/surface`.

Kinds make this legible on the card: a bank is one mode as well as one category.

## Before anything is built: the test kit

One kit written into a free bank — `G0` — with four voices whose layer order is
unmistakable by ear. Played on the module, it settles in a single sitting what
no amount of reading the card can:

1. whether the naming envelope works at all;
2. whether the sort order is the one we intended, or the FAT short-name order;
3. whether a 13th layer is read or dropped — **the card has an `S62` with 18
   layers on one voice**, against a documented limit of 12, and it is not known
   which of those is wrong;
4. whether a file the module ignores is silent or breaks the kit.

Nothing below should be trusted until this has been done.

## What to build, in order

1. **`msm kit`** — the compiler, in msm, and provable without any UI.
   - `read <card>` — inventory a card into the model (names, voices, layers).
   - `build <spec>` — convert to 44.1/16, name by the existing convention, write
     flat kit folders, write the sidecar.
   - Import `RamKitsData.json` so it starts with 39 real kits rather than empty.
2. **`msm harvest --module rample`** — a take's four loops become a kit's four
   voices; each loop's layers become that voice's stack, in the order the page
   set. This is the BIA path.
3. **The kit grid in the Friend** — 4 × 12, sources dragged in from the Library
   panel, the ordinal editable, the kind and name written to the sidecar.
4. *Later, and a different axis:* a control surface over the 40 voice CCs plus
   bank/kit select. Same shape PWYF already draws for a Chase Bliss pedal, and
   it would make `STORE` the end of a workflow rather than a panel dance.

## Resolved: the letter collision

Asked before the card was read, and the card answered it. `G` `H` `I` `L` `Q`
`T` `V` `Y` are unused — 800 slots. Our kinds go there; Richard Devine, The
Flashbulb and Kangding Ray keep their letters, and the factory content becomes
seed material for kinds rather than something to displace. `RamKits`' existing
`D`–`M` banking is the thing that must move, and it moves cheaply because
`RamKitsData.json` addresses kits by name.
