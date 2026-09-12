-- | Auditioning a sub-sample, without cutting one.
-- |
-- | One `<audio>` element holds the whole take and every preview is a range
-- | inside it. That keeps the server out of the loop entirely — a hover costs
-- | a seek, not a request and a file — and it means the thing you hear is
-- | literally the take, at the offsets the detector proposed, rather than a
-- | copy that might have been made differently.
module Quadrat.Audio (playRange, playEach, stop) where

import Prelude

import Effect (Effect)
import Effect.Uncurried (EffectFn2, EffectFn3, runEffectFn2, runEffectFn3)

foreign import playRangeImpl :: EffectFn3 String Number Number Unit
foreign import playEachImpl :: EffectFn2 (Array String) Number Unit
foreign import stopImpl :: Effect Unit

playRange :: String -> Number -> Number -> Effect Unit
playRange = runEffectFn3 playRangeImpl

-- | **Several whole files, one after another, each capped.** A set is many
-- | files where a take is one, and the point of auditioning it is to answer
-- | "which one is this?" before doing something irreversible — which three
-- | seconds of three samples answers and thirty seconds of forty-eight does
-- | not.
playEach :: Array String -> Number -> Effect Unit
playEach = runEffectFn2 playEachImpl

stop :: Effect Unit
stop = stopImpl
