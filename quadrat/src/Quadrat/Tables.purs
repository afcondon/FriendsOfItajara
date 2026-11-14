-- | The calibration tables, read from where they are kept.
-- |
-- | A table is content in Amphora, curated into `vco-calibrations`, in the
-- | shape `Reef.Calibration.Stored` describes. This asks the store for them
-- | through its own described operations (`Amphora.Api`), as Odonus does.
-- | Until 2026-11-14 they came by way of `deepstar serve` on :3027, relayed
-- | by this page's server; that was a detour, and DeepStar is being retired.
-- |
-- | A failure is a sentence, never an empty list: "nothing has been measured
-- | yet" and "the store is not running" are the same empty dropdown and
-- | different jobs for you.
module Quadrat.Tables
  ( list
  , one
  , Whole
  , storeBase
  ) where

import Prelude

import Amphora.Api as Api
import Control.Parallel (parTraverse)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Hawser.Fetch (troubleText)
import Hawser.Fetch as Fetch
import Quadrat.Http (CalibRow)
import Reef.Calibration.Stored (Stored)
import Reef.Calibration.Stored as Stored

-- | Where the store is: the page's own host, on :3024.
foreign import storeBase :: Effect String

-- | Every curated table that reads as one. A curated content that does not
-- | read as a calibration table is left out.
curated :: Aff (Either String (Array Stored))
curated = do
  base <- liftEffect storeBase
  let store = { base, timeoutMs: 2500 }
  Fetch.ask store Api.listFavorites { collection: Just Stored.collection } >>= case _ of
    Left trouble -> pure (Left ("the store (:3024) " <> troubleText trouble))
    Right favourites -> do
      found <- favourites # parTraverse \{ contentHash } ->
        Fetch.ask store Api.getContent { hash: contentHash } <#> case _ of
          Right content -> Stored.read content.payload
          Left _ -> Nothing
      pure (Right (Array.catMaybes found))

-- | One row for each table, enough to choose by, in the order of their
-- | labels.
list :: Aff (Either String (Array CalibRow))
list = map (Array.sortWith _.label <<< map row) <$> curated
  where
  row kept =
    let
      byPitch = Array.sortWith _.hz kept.points
    in
      { label: kept.label
      , module: fromMaybe "" kept.module
      , points: Array.length kept.points
      , loHz: fromMaybe 0.0 (Array.head byPitch <#> _.hz)
      , hiHz: fromMaybe 0.0 (Array.last byPitch <#> _.hz)
      , voltsPerOctave: Stored.voltsPerOctave kept
      }

-- | One table, whole. `coarse` is the module's front-panel state as recorded
-- | at sweep time and it is not decoration: the BIA's pitch knob is a pure
-- | octave OFFSET, so a table is only true for the knob position it was
-- | measured at.
type Whole =
  { label :: String
  , module :: String
  , coarse :: String
  , measuredAt :: String
  , points :: Array { volts :: Number, hz :: Number }
  }

-- | The table with this label, its points in order of voltage. If the label
-- | was measured more than once, the latest.
one :: String -> Aff (Either String Whole)
one label = curated <#> case _ of
  Left problem -> Left problem
  Right tables -> case Array.last (Array.sortWith (_.measuredAt >>> fromMaybe "") (Array.filter (\kept -> kept.label == label) tables)) of
    Nothing -> Left ("no calibration table is labelled " <> label)
    Just kept -> Right
      { label: kept.label
      , module: fromMaybe "" kept.module
      , coarse: fromMaybe "" kept.coarseSetting
      , measuredAt: fromMaybe "" kept.measuredAt
      , points: Array.sortWith _.volts (kept.points <#> \measured -> { volts: measured.volts, hz: measured.hz })
      }
