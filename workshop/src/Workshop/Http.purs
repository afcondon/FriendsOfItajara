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
  , addToCard
  , writeToCard
  ) where

import Control.Promise (Promise)
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
foreign import divisions
  :: { take :: String, as :: String, by :: String, minGap :: Number }
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

-- | Cut the kept regions into a named set and put that set on a voice.
foreign import addToCard
  :: { take :: String, set :: String, bank :: String, kit :: String
     , voice :: Int, kind :: String, stereo :: Boolean, join :: Boolean
     , append :: Boolean, layerMode :: String, regions :: Array Region }
  -> Effect (Promise Wrote)

-- | Compile the manifest onto a mounted card.
foreign import writeToCard :: String -> Effect (Promise Wrote)
