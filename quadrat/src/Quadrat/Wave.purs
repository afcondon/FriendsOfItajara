-- | **Drawing what was caught.**
-- |
-- | Two pictures from one set of numbers. The daemon sends a bucketed envelope
-- | for the whole take — `lo` and `hi` per bucket — and every sub-sample is a
-- | *slice of that array*. So a grid of forty small waveforms costs one
-- | snapshot and no extra request: the tile knows which buckets are its own.
-- |
-- | The alternative was cutting the audio on the server and drawing each piece
-- | from its own file, which is dozens of writes to answer a hover.
module Quadrat.Wave
  ( bucketsFor
  , svg
  , klass
  ) where

import Prelude

import Data.Array as Array
import Data.Int as Int
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen (AttrName(..), ElemName(..), Namespace(..))

-- | Which buckets of a whole-take envelope belong to a stretch of seconds.
-- |
-- | Half-open, and clamped: a region that ends at the last frame must not ask
-- | for a bucket past the end, and a region shorter than one bucket still gets
-- | one — a very short hit is still a hit and a tile with nothing in it reads
-- | as a bug.
bucketsFor :: Int -> Number -> Number -> Number -> { from :: Int, to :: Int }
bucketsFor buckets totalSecs start end =
  let
    n = max 1 buckets
    at t = clamp 0 n (Int.round (t / max 0.0001 totalSecs * Int.toNumber n))
    a = at start
    b = at end
  in
    { from: a, to: max (a + 1) b }

-- | An envelope as a filled shape, centred, in a 0..1 box on both axes.
-- |
-- | `preserveAspectRatio="none"` on purpose: these are read as *shapes over
-- | time*, and a tile that letterboxed itself to keep a waveform's proportions
-- | would waste the width that carries the meaning.
svg :: forall r w i. Array Int -> Array Int -> Array (HH.IProp r i) -> HH.HTML w i
svg lo hi props =
  let
    n = max 1 (Array.length hi)
    x i = Int.toNumber i / Int.toNumber n
    -- The daemon's buckets are 0..1000 either side of zero.
    y v = 0.5 - Int.toNumber v / 2200.0
    top = Array.mapWithIndex (\i v -> pt (x i) (y v)) hi
    bot = Array.reverse (Array.mapWithIndex (\i v -> pt (x i) (y v)) lo)
    pts = Array.intercalate " " (top <> bot)
  in
    el "svg"
      ( [ attr "viewBox" "0 0 1 1"
        , attr "preserveAspectRatio" "none"
        ] <> props )
      [ el "polygon" [ attr "points" pts, attr "vector-effect" "non-scaling-stroke" ] [] ]
  where
  pt a b = show a <> "," <> show b

-- | **An SVG element, in the SVG namespace.**
-- |
-- | `HH.element` makes an HTML element called `svg`, which the browser renders
-- | as nothing at all: a box the right size with no shape in it. That is not a
-- | missing-data symptom and it reads exactly like one.
el :: forall r w i. String -> Array (HH.IProp r i) -> Array (HH.HTML w i) -> HH.HTML w i
el name = HH.elementNS (Namespace "http://www.w3.org/2000/svg") (ElemName name)

-- | A class on an SVG node. See `attr` for why `HP.class_` will not do.
klass :: forall r i. String -> HH.IProp r i
klass = attr "class"

-- | And an attribute, not a property.
-- |
-- | `HP.class_` sets the DOM *property*, and on an SVG element `className` is a
-- | read-only `SVGAnimatedString` — so the assignment does nothing, quietly,
-- | and every rule keyed on that class never applies.
attr :: forall r i. String -> String -> HH.IProp r i
attr k v = HP.attr (AttrName k) v
