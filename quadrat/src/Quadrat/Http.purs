-- | Quadrat's own server, as one call.
-- |
-- | The page can see the daemon's waveform live, but only `msm` knows where a
-- | sound begins — so the divisions come back over HTTP while the picture
-- | comes down the socket, and the page puts one on top of the other.
module Quadrat.Http
  ( Region
  , Divisions
  , divisions
  , CardRow
  , CardView
  , Wrote
  , card
  , Fate(..)
  , fateOf
  , PreviewSlot
  , Preview
  , previewCard
  , Meant
  , addToCard
  , SetRow
  , Centre
  , setCentre
  , setArpeggiated
  , AudioCheck
  , audioCheck
  , storedSets
  , SourceName
  , sourceLabels
  , nameSource
  , CalibRow
  , calibrations
  , calibration
  , placeSet
  , Arrangement
  , arrangementsOf
  , voiceRoom
  , splitOver
  , slicerOffers
  , takePeaks
  , TakePeaks
  , loadSpec
  , loadSet
  , deleteSets
  , StoredSet
  , writeToCard
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..), fromMaybe)

import Control.Promise (Promise)
import Data.Nullable (Nullable)
import Effect (Effect)

-- | Seconds from the start of the take, both ends — and what `msm` measured
-- | inside them.
-- |
-- | The measurements ride along with the boundaries because they are one pass
-- | over audio the detector had already read, and because the question they
-- | answer is asked of the same objects: *is this division the sound I meant,
-- | and are these twelve actually different from each other?*
-- |
-- | Three witnesses, because each is blind to something. `peak` misses a sweep
-- | that changes timbre at constant loudness. `zcr` reads the *fundamental*, so
-- | it misses a morph from sine to square — measured on a Basimilus, it moved
-- | 17% while the sound changed completely. `tilt` is harmonic richness, and on
-- | that same morph it moved 3.03x and resolved the parameter's whole shape.
type Region =
  { start :: Number
  , end :: Number
  , peak :: Number
  , rms :: Number
  , zcr :: Number
  , tilt :: Number
  }

type Divisions =
  { ok :: Boolean
  , output :: String
  , secs :: Number
  , divides :: Boolean
  , regions :: Array Region
  }

-- | `POST /api/onsets` — a take's name, what it is, and how close two sounds
-- | may be and still be two; back with where the detector thinks things begin.
-- | A record rather than four positional arguments, because `take` and `as`
-- | and `by` are all strings and nothing would have caught them being swapped.
-- |
-- | **`regions` turns the detector off.** A take the rig ran already has its
-- | divisions — the page issued every trigger — so it sends them and `msm`
-- | measures those instead of looking. Empty is the ordinary case and means
-- | "go and find them". The answer has the same shape either way, so nothing
-- | downstream of here knows or needs to which it was. See `Quadrat.Schedule`.
foreign import divisions
  :: { take :: String, as :: String, by :: String, minGap :: Number
     , regions :: Array { start :: Number, end :: Number } }
  -> Effect (Promise Divisions)

-- | **The virtual card, flattened for showing.**
-- |
-- | The card is banks holding kits holding voices; a row here is one voice,
-- | which is the unit a person points at. The nesting is the manifest's
-- | business and the JS bridge does the flattening, so this side holds what a
-- | table holds.
type CardRow =
  { bank :: String
  -- | The bank's LETTER, and the kit's position in it. Together they are the
  -- | slot the module sees — `kit build` names each kit `{letter}{index}` —
  -- | and the slot is what a write deletes.
  , letter :: String
  , kitIx :: Int
  , kit :: String
  , voice :: Int
  , set :: String
  , count :: Int
  , stereo :: Boolean
  , kind :: String
  -- | Non-zero when this voice is ONE file holding that many equal slices,
  -- | rather than a stack of layers. `SETTINGS > SLICER` has to be set to it
  -- | by hand: the module keeps it globally and a card cannot carry it.
  , slicer :: Int
  -- | Every set on this voice, in the order they sound. One is the ordinary
  -- | case; several make the voice an axis of its own, picked by the layer
  -- | selector while the start point indexes inside each of them.
  , sets :: Array String
  -- | How the layer selector moves: manual, velocity, random, cyclic.
  , mode :: String
  }

type CardView =
  { rows :: Array CardRow
  -- | Mounted Rample cards, by mount point. Empty is the ordinary case.
  , cards :: Array String
  -- | **What `kit build` says about it**, run without writing anything. The
  -- | compiler's own objections rather than a second opinion formed here.
  , plan :: String
  , ok :: Boolean
  }

type Wrote = { ok :: Boolean, output :: String }

foreign import card :: Effect (Promise CardView)

-- | **What becomes of one slot** when the manifest is written to a card.
-- |
-- | Closed, and read off a string the compiler chose, which is why `Unknown`
-- | is here: `msm` and this page are different repositories on different
-- | release cycles, and a fate this build has never heard of must not render
-- | as the mildest one. A row that says it does not know is recoverable; a
-- | row that says "creates" over something it would delete is not.
data Fate = Create | Replace | Keep | Unknown String

derive instance Eq Fate

fateOf :: String -> Fate
fateOf = case _ of
  "create" -> Create
  "replace" -> Replace
  "keep" -> Keep
  other -> Unknown other

-- | One slot, both sides of the transform: what the manifest puts there, and
-- | what is there now. Flat, because the JS owns the wire shape and this side
-- | holds what a table holds — a planned-but-absent field is an empty string
-- | or a zero, never a `Maybe`.
-- |
-- | **`there*` is the whole slot, not the part we would overwrite.** A write
-- | removes the kit's entire directory before rebuilding it, so what is at
-- | risk is everything in `thereNames` — which is why they are all listed.
type PreviewSlot =
  { slot :: String
  , letter :: String
  -- | `create`, `replace` or `keep`. Through `fateOf` before it decides
  -- | anything.
  , fate :: String
  -- | The kit the manifest puts here. Empty when this slot is only on the
  -- | card — those rows exist so that a view of the card is a view of the
  -- | card, and not only of our own corner of it.
  , name :: String
  , kind :: String
  , settings :: String
  , files :: Int
  , secs :: Number
  -- | Non-zero when what lands is one file of that many equal slices, and so
  -- | needs `SETTINGS > SLICER` set to it by hand. Per slot rather than per
  -- | card: the manifest puts the division on the bank.
  , slots :: Int
  , thereFiles :: Int
  , therePlayable :: Int
  , thereBytes :: Number
  , thereNames :: Array String
  }

-- | **What a write to a particular card would do**, asked before doing it.
-- |
-- | Distinct from `CardView`'s `plan`, which dry-runs into a scratch directory
-- | and so answers only whether the manifest is buildable. The question at the
-- | Write button is the other one, and it has a different answer for every
-- | card you might mount.
type Preview =
  { ok :: Boolean
  -- | A sentence when the call or the compiler had something to say that is
  -- | not a per-slot fact. Empty is the ordinary case.
  , output :: String
  , dest :: String
  -- | Whether a write would go through **as asked**: the manifest is good and
  -- | either nothing collides or replacing was chosen.
  , wouldWrite :: Boolean
  , mounted :: Boolean
  -- | Why the card could not be read, when it could not. A card that is
  -- | mounted and unreadable must never be shown as an empty one: an empty
  -- | one says every letter is free.
  , unreadable :: String
  , problems :: Array String
  , notes :: Array String
  -- | The slots that are occupied and wanted. **Non-empty means a write
  -- | without replacing refuses ALL of it** — not those slots only.
  , collisions :: Array String
  -- | The bank letters nothing is using, as one string.
  , free :: String
  , slots :: Array PreviewSlot
  }

-- | Ask what writing to this mounted card would do. Nothing is written.
foreign import previewCard :: String -> Effect (Promise Preview)

-- | **What one sample is and what it meant**, written beside the audio.
-- |
-- | The boundaries and the measurements say what came out; `cell` and `means`
-- | say what the instrument was doing to produce it. A sample's meaning is not
-- | its index — `morph 2.5 V` survives a year and `position 7 of 12` does not.
-- | See `Quadrat.Sweep.Meaning`, whose shape this is; the two are structural
-- | so nothing converts between them.
type Meant =
  { cell :: Array Int
  , start :: Number
  , end :: Number
  , peak :: Number
  , rms :: Number
  , zcr :: Number
  , tilt :: Number
  -- | **How long this cell took to go quiet**, in seconds from the region's
  -- | start, against `floor` — the take's noise floor as a fraction of its peak.
  -- | Two numbers and no verdict, because "long enough" depends on what the
  -- | sample is for.
  -- |
  -- | Stored because it is the input to the NEXT run: a sweep measured once can
  -- | be re-run with a per-cell spacing fitted to what each cell actually
  -- | needed. That is the only way to size a transect whose own Decay or
  -- | velocity is one of the swept parameters — a uniform worst-case spacing
  -- | pays the longest decay on every cell, and two probes at the extremes
  -- | cannot see the middle.
  , decay :: Number
  , floor :: Number
  -- `note` is the MIDI note a pitch parameter landed on, `-1` otherwise. It
  -- rides with the measurements rather than being derivable from `level`,
  -- because deriving it needs the calibration table and a stored set has to be
  -- readable without one.
  , means :: Array { name :: String, at :: Number, level :: Number, cc :: Int, note :: Int }
  -- | **The notes struck inside this region**, absolute and in register.
  -- |
  -- | Beside `means` rather than inside it because they answer different
  -- | questions: `means` is what the run ASKED the instrument for, and this is
  -- | what somebody PLAYED. A swept set has the first and not the second; a
  -- | chord set played by hand has the second and not the first.
  -- |
  -- | In register, and note numbers rather than pitch classes, because the
  -- | voicing is the point — these sets exist to keep complex chords found by
  -- | hand, and a pitch-class set throws away the spacing that made one worth
  -- | keeping. Naming them is a separate question with a separate answer
  -- | (`Harmonia.Recognise`), and it is not this field's job.
  , notes :: Array Int
  -- | **The same notes as the chords they were struck as**, in order.
  -- |
  -- | Usually one entry holding what `notes` holds. Not always: two chords
  -- | deliberately stacked into one sample are two entries, and flattened they
  -- | would read as a single voicing of eleven notes — a different musical
  -- | object from the two that were played, and the difference is the whole
  -- | content of "these two go together". A resend of a chord still ringing is
  -- | not a second entry; see `strikesIn`.
  , struck :: Array (Array Int)
  }

-- | Cut the kept regions into a named set and put that set on a voice — and
-- | write the set's own description into the same directory.
-- |
-- | `spec` is polymorphic on purpose: this module is the wire and has no
-- | business knowing the shape of a sweep plan. It is `Quadrat.Sweep.Plain`
-- | at the one call site, and JSON on the far side.
foreign import addToCard
  :: forall spec
   . { take :: String, set :: String, bank :: String, letter :: String
     , kit :: String
     , voice :: Int, kind :: String, stereo :: Boolean, join :: Boolean
     , append :: Boolean, layerMode :: String, regions :: Array Region
     -- | **Cut it, or cut it and put it on a voice.** Two acts, and they were
     -- | one until SuperDirt arrived with no voices to be placed in.
     , place :: Boolean
     -- | The spec that produced these samples, so the set can be run again at
     -- | a resolution nobody chose at the time. **Null for a take played by
     -- | hand**, which is what makes such a set worth keeping but not worth
     -- | re-running — and the difference has to be recorded, because a set
     -- | that claims a spec it never ran by is worse than one with none.
     , spec :: Nullable spec
     -- | **What this take was listening to**, and NOT part of the spec.
     -- |
     -- | The spec is null for a hand-played take, and rightly: a set that
     -- | claims a sweep it never ran by is worse than one with none. But
     -- | which MIDI port and channel the notes were taken from, and which
     -- | audio input the sound came back on, are facts about the RECORDING
     -- | rather than about a sweep — and they are true of a played take too.
     -- |
     -- | Keeping them inside the spec threw them away for exactly the takes
     -- | most likely to need them. The three chord sets of 2026-09-12 carry
     -- | regions holding 20 to 42 notes spanning 0 to 124, against real
     -- | voicings of five to seven, and nothing stored with them can say
     -- | which port or channel let that in.
     -- | `voice` is what PLAYED it, free text — the counterpart to `source`,
     -- | which is only the cable it came back on. Nothing has ever recorded
     -- | it, and it is the field a queryable library turns on: you cannot
     -- | select on what was never captured.
     , listened ::
         { notesFrom :: String, notesChan :: Int, source :: String
         , voice :: String
         }
     -- | **The key, declared before the take.** An undeclared centre (root -1)
     -- | leaves any centre already on disk alone — re-cutting an old take must
     -- | not discard what the Library was told about it afterwards.
     , centre :: Centre
     , schedule :: Array Number
     , samples :: Array Meant
     }
  -> Effect (Promise Wrote)

-- | One row of the stored-set list: enough to choose by, never the whole thing.
-- | A declared key centre. `tonality` is `""`, `"major"` or `"minor"` — the
-- | empty case being a set whose root is known and whose intent is not, which
-- | is a real position and not a half-filled form.
type Centre = { root :: Int, tonality :: String }

type SetRow =
  { name :: String
  , count :: Int
  , made :: String
  , take :: String
  -- | It has a `set.json` at all. False for a set cut before sets were stored,
  -- | which is a different thing from one cut from a take played by hand: the
  -- | first has no description, the second has one and no spec inside it.
  , described :: Boolean
  -- | It has a spec, so it can be run again.
  , runnable :: Boolean
  -- | Goes to a card as stereo, and so occupies a PAIR of voices. See
  -- | `Kind.foldsTo` — it is a property of the material, decided when the set
  -- | was cut and recorded in `set.json`.
  , stereo :: Boolean
  -- | The parameters that moved, by name — so a list of sets reads as a list
  -- | of experiments rather than a list of folders.
  , moved :: Array String
  , extent :: Array Int
  , encoding :: String
  -- | **The pitch of each sample, in file order**; `-1` where a sample stands
  -- | for no note. What lets the library draw a set rather than describe it:
  -- | a chromatic run reads as a run, and the same run repeated four times
  -- | reads as a repeat — which is exactly the difference between an
  -- | arrangement that can reach a pitch and one that cannot.
  , notes :: Array Int
  -- | **How long each sample is**, in file order. The decay axis of a sweep
  -- | is a duration axis, and nothing on the page has ever shown it; it is
  -- | also the number that decides the slot a layer gets.
  , secs :: Array Number
  -- | What the take was, declared when it was recorded: `drum-hits`,
  -- | `chord-hits`, `bars`, `chromatic`, `longform`.
  -- |
  -- | **The only reliable statement that a sample is a chord.** Not
  -- | `samples[].notes` — a chord-hits set records 36 to 42 of those per
  -- | sample spanning 0 to 123, including 0 through 6, and a sibling set
  -- | recorded the same morning has none at all. Something other than
  -- | note-ons is reaching that field, so nothing may be concluded from it.
  , kind :: String
  -- | **The chords this set was played as**, one entry per sample and in file
  -- | order, each entry the chords struck into that region. Empty for anything
  -- | not declared a chord take.
  -- |
  -- | On the LISTING, not only on a set's page, because a chord set's identity
  -- | is made of them: it wears the rebus of its own chords, which is what
  -- | makes the same progression draw the same picture here and in Vetula.
  -- | The list is where that picture earns its keep.
  , voicings :: Array (Array (Array Int))
  -- | **The key centre this set was played in**, as declared by whoever played
  -- | it. `root` is a pitch class, or `-1` for "nobody has said".
  -- |
  -- | Declared and never inferred, because for a capture off a generator the
  -- | key and the major/minor switch are SETTINGS SOMEBODY CHOSE — recovering
  -- | them from the audio would hand back an input, badly. And a wrong centre
  -- | is worse than none: everything downstream transposes from it silently.
  -- |
  -- | The root alone is what makes the set transposable; `tonality` is context
  -- | for reading how far out a chord is, since against an intended minor a
  -- | major I is further out than a minor chord of the same complexity.
  , centre :: Centre
  -- | **Whether the chords were arpeggiated**, declared by whoever played
  -- | them. An arpeggiated region is ONE chord: its notes arrive one at a
  -- | time, so the 50 ms strike grouping reads each as its own chord — nine
  -- | voicings came back as sixty-four on a real take. The flat union is the
  -- | voicing in that case, and it is already on disk either way.
  , arpeggiated :: Boolean
  -- | **Which MIDI port and channel the notes were taken from.** Empty and
  -- | zero for every set written before this was recorded, which is an honest
  -- | "not known" and not "nothing" — and zero as a CHANNEL means "all of
  -- | them on that port", which is the setting that let a sequencer's traffic
  -- | into a chord.
  , notesFrom :: String
  , notesChan :: Int
  -- | **What played it** — the instrument or patch, free text, empty for not
  -- | said. The counterpart to the audio input: that is the cable, this is
  -- | what was at the far end of it.
  -- |
  -- | On the listing because assembling a pack freely is a question about the
  -- | LIST — "every chord set from that patch, in G minor" — and no index can
  -- | answer it for sets that never recorded the answer.
  , voice :: String
  }

-- | **A name the owner chose, against the wire name the daemon uses.**
-- |
-- | `wire` is what `--source` called it — the identity, and the thing a stored
-- | set records. `label` is what the person with the cable in their hand calls
-- | the thing on the other end of it, which is "Jupiter 8" far more usefully
-- | than it is "pedalboard stereo". Nothing routes on the label.
type SourceName = { wire :: String, label :: String }

foreign import sourceLabels :: Effect (Promise (Array SourceName))

-- | One label. Empty clears it, and the source goes back to showing its wire
-- | name, so there is no second control for forgetting.
foreign import nameSource :: String -> String -> Effect (Promise Wrote)

-- | Declare (or clear) a set's key centre. Only that field is written: a
-- | declaration made months later must not rewrite what the recording
-- | measured.
foreign import setCentre :: String -> Centre -> Effect (Promise Wrote)

-- | Say whether a set's chords were arpeggiated. See `SetRow.arpeggiated` —
-- | it changes which of the two readings already on disk is believed, and so
-- | changes what the set IS: its voicings, its export, and its glyph.
foreign import setArpeggiated :: String -> Boolean -> Effect (Promise Wrote)

-- | **Where every source lands, against the device as it stands right now.**
-- |
-- | `itajara sources` resolved against the daemon's OWN arguments, read off
-- | the running process — because the fault this exists to catch is a channel
-- | map that was right when the daemon started and is not right now.
-- |
-- | `gone` names members of an aggregate that CoreAudio cannot see today, whole
-- | and untrimmed (they are UIDs, and every rule for shortening one is wrong
-- | for some device). `unreachable` is the per-source verdict in English.
-- | `text` is the tool's own report, shown as it prints it.
type AudioCheck =
  { ok :: Boolean
  , running :: Boolean
  , device :: String
  , gone :: Array String
  , unreachable :: Array { source :: String, says :: String }
  , text :: String
  }

foreign import audioCheck :: Effect (Promise AudioCheck)

foreign import storedSets :: Effect (Promise { ok :: Boolean, sets :: Array SetRow })

-- | One row of the calibration list, enough to choose by. `loHz`/`hiHz` are the
-- | span the table actually measured, which is the fact that decides whether a
-- | note is reachable at all — outside it the realiser clamps, and a whole
-- | transect can come out on one pitch that way.
type CalibRow =
  { label :: String
  , module :: String
  , points :: Int
  , loHz :: Number
  , hiHz :: Number
  , voltsPerOctave :: Number
  }

foreign import calibrations :: Effect (Promise { ok :: Boolean, tables :: Array CalibRow })

-- | One table, whole. `coarse` is the module's front-panel state as recorded at
-- | sweep time and it is not decoration: the BIA's pitch knob is a pure octave
-- | OFFSET, so a table is only true for the knob position it was measured at.
foreign import calibration
  :: String
  -> Effect (Promise { ok :: Boolean, label :: String, module :: String
                     , coarse :: String, measuredAt :: String
                     , points :: Array { volts :: Number, hz :: Number }
                     , error :: String })

-- | **Put a set already on disk onto a voice** — no take, no cut, no measuring.
-- |
-- | The Rample projection of a stored set. It has to act on the SET rather
-- | than on the take, or "the same set reaches both destinations" would only
-- | mean "the same take was cut twice". Everything the card needs about the
-- | shape is in `set.json`, which is what it is for.
-- | **A take's envelope, from the file.**
-- |
-- | The Bench draws bands over whatever the daemon is holding, so a set could
-- | be recorded, saved, and then never looked at again — and any stray capture
-- | left the bands describing one recording and the picture another. Shaped
-- | like `Socket.Peaks` so the drawing code does not learn a second source.
type TakePeaks =
  { ok :: Boolean, output :: String
  , secs :: Number, frames :: Int, buckets :: Int
  , lo :: Array Int, hi :: Array Int }

foreign import takePeaks :: String -> Int -> Effect (Promise TakePeaks)

-- | **A stored set, whole.** Which take it came from, where its pieces are in
-- | that take, and when the run fired — enough to put it back on the bench.
-- | `loadSpec` beside it answers a different question, which is what to RUN
-- | again rather than what was run.
type StoredSet =
  { ok :: Boolean, output :: String
  , take :: String
  , regions :: Array Region
  , schedule :: Array Number
  -- | The notes struck inside each region, one array per sample and in the
  -- | same order. See `Meant.notes` — this is the way back in, so a chord set
  -- | reopened next month still shows its voicings.
  , notes :: Array (Array Int)
  -- | And the chords each was struck as. See `Meant.struck`.
  , struck :: Array (Array (Array Int))
  -- | **What the files are**, from the first one's header — the one fact
  -- | about a sample that decides whether a module will play it at all, and
  -- | the one `set.json` never recorded because the recorder knew it. A zero
  -- | in any field means the header did not say, and the page prints nothing
  -- | rather than a lie.
  , audio :: { rate :: Int, bits :: Int, channels :: Int, tag :: Int } }

foreign import loadSet :: String -> Effect (Promise StoredSet)

-- | **Throw sets away.** Plural because the useful gesture is "these four",
-- | and a loop of single deletes is four chances to stop halfway.
foreign import deleteSets :: Array String -> Effect (Promise Wrote)

-- | Put a set already on disk onto a voice. No take, no cut, no measuring —
-- | the shape the card needs is in the set's own description.
-- |
-- | **`layers` is the arrangement**, and it is the one thing the description
-- | does NOT settle, because it is not a fact about the audio. The same
-- | forty-eight files are four alternatives of twelve positions, or two of
-- | twenty-four, or one file of forty-eight — and which you want is a fact
-- | about the module you are playing. Zero leaves it to the sweep's own
-- | extent, which is the natural arrangement and the old behaviour.
-- |
-- | `sliced` survives for sets with no extent at all, where there is no grid
-- | to regroup and the only question is whether the files are joined.
foreign import placeSet
  :: { set :: String, bank :: String, letter :: String, kit :: String, voice :: Int
     , append :: Boolean, sliced :: Boolean, layerMode :: String, layers :: Int
     , voices :: Int }
  -> Effect (Promise Wrote)

-- | **How a set of `n` samples can be arranged for the module**, and nothing
-- | else about it. Pure, and here beside the wire it travels on.
-- |
-- | Two axes, and they are not interchangeable — this is the asymmetry the
-- | whole transect rests on:
-- |
-- | - **layers are alternatives the MODULE picks between**, by the layer CV,
-- |   by velocity, at random or cyclically. Twelve is the ceiling and the
-- |   thirteenth is dropped in silence, measured.
-- | - **slices are positions only YOU can reach**, by the start point, one CC
-- |   per voice. The division is a single global setting on the module, so
-- |   every sliced file in a bank must share it.
-- |
-- | Only the arrangements that waste nothing are offered. A layer of six in a
-- | division of eight works — the last two are silent by construction — but
-- | it is a thing to reach for deliberately, not one to be offered six of.
-- | `voices` is how many of the module's four this occupies; `layers` is the
-- | largest number of alternatives on any one of them, and `slices` the
-- | division inside each (0 for none). The exact per-voice split is
-- | `splitOver` — as even as it goes, and the evenness is musical rather than
-- | tidy: in RANDOM layer mode each voice picks uniformly among its OWN
-- | layers, so twelve-and-six would make each of the six twice as likely to
-- | sound as each of the twelve.
type Arrangement = { voices :: Int, layers :: Int, slices :: Int }

-- | **How many voices there are to spread over.**
-- |
-- | A stereo sample plays its right channel on the next voice, so it consumes
-- | a PAIR — which leaves two places to begin, not four. See `voicesFor` in
-- | the page: the same fact, from the other side.
voiceRoom :: Boolean -> Int
voiceRoom stereo = if stereo then 2 else 4

-- | `n` samples over `v` voices, as even as it goes: the remainder is spread
-- | one each across the first voices rather than piled on the last.
splitOver :: Int -> Int -> Array Int
splitOver v n
  | v <= 0 = []
  | otherwise =
      let base = n / v
          extra = n `mod` v
      in Array.mapWithIndex (\i b -> if i < extra then b + 1 else b) (Array.replicate v base)

-- | The divisions `SETTINGS > SLICER` offers. Measured, not documented.
slicerOffers :: Array Int
slicerOffers = [ 8, 12, 16, 24, 32, 48, 64, 128 ]

-- | **Every arrangement of `n` samples that wastes nothing**, across as many
-- | voices as the material allows.
-- |
-- | The one-voice answers are as they always were: grids whose rows land on a
-- | division the module offers, plus plain layers when the whole set fits
-- | under the twelve-layer ceiling.
-- |
-- | What is new is the voice axis, and it is what makes a set of more than
-- | twelve placeable at all. Eighteen stereo chords have NO one-voice answer —
-- | eighteen is over the ceiling, and 18/1, /2, /3, /6 and /9 are none of them
-- | divisions the module offers — so before this the set could not go on a
-- | card by any route. Across two voices it is nine and nine.
-- |
-- | Multi-voice arrangements are plain layers only. A grid already fits one
-- | voice by construction, and spreading one over several would mean two
-- | pictures of the same set that nobody could hold in mind at once.
arrangementsOf :: { count :: Int, stereo :: Boolean } -> Array Arrangement
arrangementsOf o
  | o.count <= 0 = []
  | otherwise =
      let
        n = o.count
        grids = Array.mapMaybe
          (\l ->
            if n `mod` l /= 0 then Nothing
            else
              let per = n / l
              in if Array.elem per slicerOffers then Just { voices: 1, layers: l, slices: per }
                 else Nothing)
          (Array.range 1 12)
        -- Every sample its own alternative, over as few voices as will hold
        -- it — one first, so the familiar answer stays the first answer.
        stacks = Array.mapMaybe
          (\v ->
            let per = splitOver v n
                widest = fromMaybe 0 (Array.head per)
            in if widest > 12 || Array.any (_ < 1) per then Nothing
               else Just { voices: v, layers: widest, slices: 0 })
          (Array.range 1 (voiceRoom o.stereo))
      in
        grids <> stacks

-- | One stored set's spec, raw. Polymorphic for the same reason `addToCard`
-- | is: this module is the wire. `Quadrat.Sweep.adopt` is what makes a plan
-- | of it, and it owns the merge over the current default — a set written by
-- | an older build has to open rather than half-open.
-- |
-- | `ok` is false, with a sentence, for a set cut from a take that was played
-- | rather than run: there is nothing to run again, which is a fact about the
-- | set and not a failure of this call.
foreign import loadSpec
  :: forall spec. String
  -> Effect (Promise { ok :: Boolean, output :: String, spec :: spec })

-- | Compile the manifest onto a mounted card.
-- | Compile the manifest onto a card. The `Boolean` is `--overwrite`, which
-- | **deletes each kit slot's whole directory before writing it** — so it is
-- | never a default and the page asks before sending it true.
foreign import writeToCard :: String -> Boolean -> Effect (Promise Wrote)
