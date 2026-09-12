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
  , Meant
  , addToCard
  , SetRow
  , storedSets
  , SourceName
  , sourceLabels
  , nameSource
  , CalibRow
  , calibrations
  , calibration
  , placeSet
  , takePeaks
  , TakePeaks
  , loadSpec
  , loadSet
  , deleteSets
  , StoredSet
  , writeToCard
  ) where

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
     , schedule :: Array Number
     , samples :: Array Meant
     }
  -> Effect (Promise Wrote)

-- | One row of the stored-set list: enough to choose by, never the whole thing.
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
  , struck :: Array (Array (Array Int)) }

foreign import loadSet :: String -> Effect (Promise StoredSet)

-- | **Throw sets away.** Plural because the useful gesture is "these four",
-- | and a loop of single deletes is four chances to stop halfway.
foreign import deleteSets :: Array String -> Effect (Promise Wrote)

-- | Put a set already on disk onto a voice. No take, no cut, no measuring —
-- | the shape the card needs is in the set's own description.
-- |
-- | `sliced` is the one thing the description does NOT settle, because it is
-- | not a fact about the audio: the same files are a stack of layers or one
-- | concatenated file with boundaries, and which of those you want depends on
-- | what else is going on the voice.
foreign import placeSet
  :: { set :: String, bank :: String, letter :: String, kit :: String, voice :: Int
     , append :: Boolean, sliced :: Boolean, layerMode :: String }
  -> Effect (Promise Wrote)

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
