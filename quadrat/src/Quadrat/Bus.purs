-- | Quadrat on the Atlantis tab bus, so the dashboard can see the page and
-- | whether a capture is armed. Announces only; see `Bus.js`.
module Quadrat.Bus
  ( start
  , setArmed
  ) where

import Prelude

import Effect (Effect)

foreign import start :: Effect Unit

foreign import setArmed :: Boolean -> Effect Unit
