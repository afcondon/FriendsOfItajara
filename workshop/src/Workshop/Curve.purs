-- | **A curve is a thing, not a setting.**
-- |
-- | Andrew's sketch, 2026-09-10: down the left of the page, one small curve per
-- | parameter you want to move. You add a curve *because* you want to control
-- | something, so the list of curves and the list of controlled parameters are
-- | the same list. Click a curve and it steps through the named shapes;
-- | double-click and it opens for editing point by point — at which moment the
-- | little icon stops drawing a smooth line and starts drawing the values,
-- | because that is now what it is.
-- |
-- | That last detail is the whole reconciliation with what yesterday taught us.
-- | A table beats a curve for the edit you make *after listening*, which is
-- | almost always "position 8 is wrong". A curve beats a table for filling
-- | seven parameters in seven clicks. Holding both is only dangerous if you
-- | keep two copies of the answer — so **there is one field**, and its
-- | constructor says which kind of thing it currently is. The thumbnail is a
-- | picture of the constructor.
module Workshop.Curve
  ( Shape(..)
  , shapes
  , shapeName
  , shapeOf
  , at
  , Curve(..)
  , label
  , isDrawn
  , valuesOf
  , setAt
  , flipped
  , nextShape
  , asDrawn
  , resample
  ) where

import Prelude

import Data.Array as Array
import Data.Int as Int
import Data.Maybe (Maybe, fromMaybe)
import Data.Number as Number

-- | **Only the monotone families.**
-- |
-- | `purescript-hylograph-selection` has twenty-five, and they are the same
-- | arithmetic — but they are ANIMATION easings, and Back, Elastic and Bounce
-- | all overshoot outside 0…1 on purpose, because a menu that springs past its
-- | resting place looks alive. A sweep's value is a voltage into a module, so an
-- | overshoot leaves the range you declared and reaches a place you never looked
-- | at. `at` clamps as well, but the honest fix is not to offer them.
-- |
-- | Names match hylograph's `EasingType` constructors exactly, so if this ever
-- | wants the full set it is an import rather than a rewrite. (`applyEasing`
-- | lives in `Hylograph.Internal.Transition.Manager`, not `Hylograph.Attribute`
-- | — the latter has the type but not the function, and Internal is not an API.)
data Shape
  = Linear
  | QuadIn | QuadOut | QuadInOut
  | CubicIn | CubicOut | CubicInOut
  | SinIn | SinOut | SinInOut
  | ExpIn | ExpOut | ExpInOut
  | CircleIn | CircleOut | CircleInOut

derive instance Eq Shape

shapes :: Array Shape
shapes =
  [ Linear
  , QuadIn, QuadOut, QuadInOut
  , CubicIn, CubicOut, CubicInOut
  , SinIn, SinOut, SinInOut
  , ExpIn, ExpOut, ExpInOut
  , CircleIn, CircleOut, CircleInOut
  ]

shapeName :: Shape -> String
shapeName = case _ of
  Linear -> "Linear"
  QuadIn -> "QuadIn"
  QuadOut -> "QuadOut"
  QuadInOut -> "QuadInOut"
  CubicIn -> "CubicIn"
  CubicOut -> "CubicOut"
  CubicInOut -> "CubicInOut"
  SinIn -> "SinIn"
  SinOut -> "SinOut"
  SinInOut -> "SinInOut"
  ExpIn -> "ExpIn"
  ExpOut -> "ExpOut"
  ExpInOut -> "ExpInOut"
  CircleIn -> "CircleIn"
  CircleOut -> "CircleOut"
  CircleInOut -> "CircleInOut"

shapeOf :: String -> Maybe Shape
shapeOf s = Array.find (\x -> shapeName x == s) shapes

-- | The shape at `t`, clamped into the unit interval. Same arithmetic as
-- | hylograph's `applyEasing` for every constructor named here.
at :: Shape -> Number -> Number
at sh t = clamp 0.0 1.0 (raw sh (clamp 0.0 1.0 t))
  where
  raw = case _ of
    Linear -> identity
    QuadIn -> \u -> u * u
    QuadOut -> \u -> u * (2.0 - u)
    QuadInOut -> \u -> if u < 0.5 then 2.0 * u * u else -1.0 + (4.0 - 2.0 * u) * u
    CubicIn -> \u -> u * u * u
    CubicOut -> \u -> let v = u - 1.0 in v * v * v + 1.0
    CubicInOut -> \u ->
      if u < 0.5 then 4.0 * u * u * u
      else (u - 1.0) * (2.0 * u - 2.0) * (2.0 * u - 2.0) + 1.0
    SinIn -> \u -> 1.0 - Number.cos (u * Number.pi / 2.0)
    SinOut -> \u -> Number.sin (u * Number.pi / 2.0)
    SinInOut -> \u -> -(Number.cos (Number.pi * u) - 1.0) / 2.0
    ExpIn -> \u -> if u == 0.0 then 0.0 else Number.pow 2.0 (10.0 * (u - 1.0))
    ExpOut -> \u -> if u == 1.0 then 1.0 else 1.0 - Number.pow 2.0 (-10.0 * u)
    ExpInOut -> \u ->
      if u == 0.0 then 0.0
      else if u == 1.0 then 1.0
      else if u < 0.5 then Number.pow 2.0 (20.0 * u - 10.0) / 2.0
      else (2.0 - Number.pow 2.0 (-20.0 * u + 10.0)) / 2.0
    CircleIn -> \u -> 1.0 - Number.sqrt (1.0 - u * u)
    CircleOut -> \u -> Number.sqrt (1.0 - Number.pow (u - 1.0) 2.0)
    CircleInOut -> \u ->
      if u < 0.5 then (1.0 - Number.sqrt (1.0 - Number.pow (2.0 * u) 2.0)) / 2.0
      else (Number.sqrt (1.0 - Number.pow (-2.0 * u + 2.0) 2.0) + 1.0) / 2.0

-- | **One field, and its constructor says what kind of answer it is.**
-- |
-- | `Named` is still a curve: it can be sampled at any number of points, so
-- | changing a sweep from twelve positions to sixteen gives an exact sixteen
-- | rather than an interpolation of twelve. `Drawn` is no longer a curve — the
-- | values *are* the answer, and asking it for a different number of them is a
-- | resampling with the loss that implies.
-- |
-- | `flipped` keeps a reversed easing NAMED rather than collapsing it to points.
-- | Andrew's Basimilus set was made by driving Morph and Attack in opposite
-- | directions from the SUM and INV of Maths, so "the same shape, the other way
-- | round" is an idiom worth being able to say — and worth being able to *see*
-- | said, in a label, rather than inferring it from two rows of numbers.
data Curve
  = Named Shape Boolean
  | Drawn (Array Number)

derive instance Eq Curve

label :: Curve -> String
label = case _ of
  Named sh false -> shapeName sh
  Named sh true -> shapeName sh <> " ↔"
  Drawn _ -> "drawn"

isDrawn :: Curve -> Boolean
isDrawn = case _ of
  Drawn _ -> true
  _ -> false

-- | The curve as `n` values in the unit interval.
valuesOf :: Int -> Curve -> Array Number
valuesOf n = case _ of
  Named sh rev ->
    let vs = sampled sh n
    in if rev then Array.reverse vs else vs
  Drawn vs -> resample n vs

sampled :: Shape -> Int -> Array Number
sampled sh n
  | n <= 1 = [ at sh 1.0 ]
  | otherwise = map
      (\i -> at sh (Int.toNumber i / Int.toNumber (n - 1)))
      (Array.range 0 (n - 1))

-- | **Editing one point turns a curve into a drawing.**
-- |
-- | It has to. A named easing with one point moved is not that easing any
-- | more, and a label that went on claiming otherwise would be the two-copies
-- | bug wearing a different hat — the thumbnail changing from a line to a set
-- | of bars is the page telling you that this is now yours rather than the
-- | library's.
setAt :: Int -> Int -> Number -> Curve -> Curve
setAt n i v c =
  Drawn (fromMaybe vs (Array.updateAt i (clamp 0.0 1.0 v) vs))
  where
  vs = valuesOf n c

-- | Reverse it, keeping a named shape named.
flipped :: Int -> Curve -> Curve
flipped n = case _ of
  Named sh rev -> Named sh (not rev)
  Drawn vs -> Drawn (Array.reverse (resample n vs))

-- | **Click steps through the shapes — but never over a drawing.**
-- |
-- | A single click that silently discarded hand-placed values would be the
-- | worst button on the page, so this leaves `Drawn` alone and the view offers
-- | going back to a curve as its own, deliberate act.
nextShape :: Curve -> Curve
nextShape = case _ of
  Drawn vs -> Drawn vs
  Named sh rev ->
    let i = fromMaybe 0 (Array.findIndex (_ == sh) shapes)
        next = fromMaybe Linear (Array.index shapes ((i + 1) `mod` Array.length shapes))
    in Named next rev

-- | Freeze whatever it is into points, which is what opening the editor does.
asDrawn :: Int -> Curve -> Curve
asDrawn n c = Drawn (valuesOf n c)

-- | **Changing the number of points keeps the shape you drew.**
-- |
-- | Truncating would silently drop the top of a sweep and padding would repeat
-- | its end; interpolating keeps what a hand edit meant, which is the only
-- | thing in here that could not be recreated by pressing a button.
resample :: Int -> Array Number -> Array Number
resample n xs
  | n <= 0 = []
  | Array.length xs == n = xs
  | otherwise = case Array.length xs of
      0 -> Array.replicate n 0.0
      1 -> Array.replicate n (fromMaybe 0.0 (Array.head xs))
      m
        | n == 1 -> [ fromMaybe 0.0 (Array.last xs) ]
        | otherwise -> map
            (\i -> lerpAt xs
                     (Int.toNumber i / Int.toNumber (n - 1) * Int.toNumber (m - 1)))
            (Array.range 0 (n - 1))

lerpAt :: Array Number -> Number -> Number
lerpAt xs u =
  let
    i = Int.floor u
    f = u - Int.toNumber i
    a = fromMaybe 0.0 (Array.index xs i)
    b = fromMaybe a (Array.index xs (i + 1))
  in
    a + (b - a) * f
