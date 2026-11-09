-- | **What a link asked Quadrat to do on arrival.**
-- |
-- | Pinakes (the sets kept, and placing them) is its own app since
-- | 2026-11-09, so its "Open on the Bench" and "Sweep again" are links:
-- | `quadrat.html?open=<set>` and `quadrat.html?again=<set>`. Read once at
-- | start and then taken off the address, so a reload does not reopen the set
-- | over whatever has been done since.
module Quadrat.Arrival
  ( Arrival(..)
  , arrival
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)

data Arrival = OpenSet String | SweepAgain String

foreign import takeParam :: String -> Effect String

arrival :: Effect (Maybe Arrival)
arrival = do
  open <- takeParam "open"
  again <- takeParam "again"
  pure
    if open /= "" then Just (OpenSet open)
    else if again /= "" then Just (SweepAgain again)
    else Nothing
