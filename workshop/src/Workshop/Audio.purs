-- | Auditioning a sub-sample, without cutting one.
-- |
-- | One `<audio>` element holds the whole take and every preview is a range
-- | inside it. That keeps the server out of the loop entirely — a hover costs
-- | a seek, not a request and a file — and it means the thing you hear is
-- | literally the take, at the offsets the detector proposed, rather than a
-- | copy that might have been made differently.
module Workshop.Audio (playRange, stop) where

import Prelude

import Effect (Effect)
import Effect.Uncurried (EffectFn3, runEffectFn3)

foreign import playRangeImpl :: EffectFn3 String Number Number Unit
foreign import stopImpl :: Effect Unit

playRange :: String -> Number -> Number -> Effect Unit
playRange = runEffectFn3 playRangeImpl

stop :: Effect Unit
stop = stopImpl
