-- | **Copy a string to the system clipboard.**
-- |
-- | Best-effort by design, and silent when it fails: the browser withholds
-- | clipboard access outside a user gesture and in some contexts refuses it
-- | outright, and neither of those is something a page can do anything about.
-- | The text is on screen in a selectable code block either way, so a failed
-- | copy costs a drag of the mouse and not the work.
module Quadrat.Clip (copyText) where

import Data.Unit (Unit)
import Effect (Effect)

foreign import copyText :: String -> Effect Unit
