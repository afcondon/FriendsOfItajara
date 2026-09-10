-- | The Workshop's own server, as one call.
-- |
-- | The page can see the daemon's waveform live, but only `msm` knows where a
-- | sound begins — so the divisions come back over HTTP while the picture
-- | comes down the socket, and the page puts one on top of the other.
module Workshop.Http
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
  , loadSpec
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
-- | downstream of here knows or needs to which it was. See `Workshop.Schedule`.
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
-- | See `Workshop.Sweep.Meaning`, whose shape this is; the two are structural
-- | so nothing converts between them.
type Meant =
  { cell :: Array Int
  , start :: Number
  , end :: Number
  , peak :: Number
  , rms :: Number
  , zcr :: Number
  , tilt :: Number
  , means :: Array { name :: String, at :: Number, level :: Number, cc :: Int }
  }

-- | Cut the kept regions into a named set and put that set on a voice — and
-- | write the set's own description into the same directory.
-- |
-- | `spec` is polymorphic on purpose: this module is the wire and has no
-- | business knowing the shape of a sweep plan. It is `Workshop.Sweep.Plain`
-- | at the one call site, and JSON on the far side.
foreign import addToCard
  :: forall spec
   . { take :: String, set :: String, bank :: String, kit :: String
     , voice :: Int, kind :: String, stereo :: Boolean, join :: Boolean
     , append :: Boolean, layerMode :: String, regions :: Array Region
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
  -- | The parameters that moved, by name — so a list of sets reads as a list
  -- | of experiments rather than a list of folders.
  , moved :: Array String
  , extent :: Array Int
  , encoding :: String
  }

foreign import storedSets :: Effect (Promise { ok :: Boolean, sets :: Array SetRow })

-- | One stored set's spec, raw. Polymorphic for the same reason `addToCard`
-- | is: this module is the wire. `Workshop.Sweep.adopt` is what makes a plan
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
foreign import writeToCard :: String -> Effect (Promise Wrote)
