-- | The Workshop's own server, as one call.
-- |
-- | The page can see the daemon's waveform live, but only `msm` knows where a
-- | sound begins — so the divisions come back over HTTP while the picture
-- | comes down the socket, and the page puts one on top of the other.
module Workshop.Http
  ( Region
  , Divisions
  , divisions
  ) where

import Control.Promise (Promise)
import Effect (Effect)

-- | Seconds from the start of the take, both ends.
type Region = { start :: Number, end :: Number }

type Divisions =
  { ok :: Boolean
  , output :: String
  , secs :: Number
  , divides :: Boolean
  , regions :: Array Region
  }

-- | `POST /api/onsets` — a take's name and what it is, back with where the
-- | detector thinks things begin.
foreign import divisions :: String -> String -> Effect (Promise Divisions)
