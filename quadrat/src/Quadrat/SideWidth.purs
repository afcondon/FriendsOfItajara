-- | The side drawer's width per page, remembered in this browser. A viewing
-- | preference, so localStorage and not the sample store.
module Quadrat.SideWidth (loadWidth, saveWidth) where

import Prelude
import Effect (Effect)

foreign import loadWidth :: String -> Number -> Effect Number
foreign import saveWidth :: String -> Number -> Effect Unit
