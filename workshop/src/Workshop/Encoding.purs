-- | **What the destination can address, which is what decides the shape.**
-- |
-- | Andrew, 2026-09-10: *"perhaps the choice of linear vs grid is subordinate to
-- | the encoding systems which we can enumerate, such as layers and offsets for
-- | Rample?"* Yes — and it is the same discipline as everywhere else here.
-- | Whether a swept set is a line or a grid is not a preference to be offered
-- | alongside the real choices; it is a **consequence** of how the thing playing
-- | it back can be addressed. Offering it freely would let you record a shape
-- | the destination cannot express, which is a mistake you would find out about
-- | on the module.
-- |
-- | So you choose an encoding, and the number of axes, their names, their legal
-- | sizes and the order the cells are recorded in all follow.
-- |
-- | ## The one that bites
-- |
-- | **SLICER offers 8, 12, 16, 24, 32, 48, 64 and 128 — and nothing else.** A
-- | four-by-four grid is not addressable on a Rample, because four is not a
-- | division the module has. `msm kit build` already refuses it at compile time;
-- | the point of putting it here is to refuse it *before* sixteen hits are
-- | recorded rather than after.
module Workshop.Encoding
  ( Encoding(..)
  , all
  , name
  , label
  , blurb
  , Axis
  , axes
  , slicerDivisions
  , defaultExtent
  , total
  , voicesPer
  , objections
  , Cell
  , cells
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..), fromMaybe)

data Encoding
  -- | One voice holding a stack. The MODULE picks between them — velocity,
  -- | random or cyclic — so this is the axis you cannot sequence and the one
  -- | that makes a swept set feel like an instrument rather than a list.
  = RampleLayers
  -- | One voice holding one joined file. YOU pick, by sending the start point
  -- | `CC(voice x 10 + 4)` about 40 ms ahead of the trigger.
  | RampleSlices
  -- | Both at once: a stack of joined files. Two knobs, two axes, and the only
  -- | two-dimensional thing the module can actually address.
  | RampleGrid

derive instance Eq Encoding

all :: Array Encoding
all = [ RampleLayers, RampleSlices, RampleGrid ]

name :: Encoding -> String
name = case _ of
  RampleLayers -> "rample-layers"
  RampleSlices -> "rample-slices"
  RampleGrid -> "rample-grid"

label :: Encoding -> String
label = case _ of
  RampleLayers -> "Rample · layers"
  RampleSlices -> "Rample · slices"
  RampleGrid -> "Rample · layers × slices"

blurb :: Encoding -> String
blurb = case _ of
  RampleLayers ->
    "A stack on one voice. The module chooses between them by velocity, at \
    \random, or in turn — so the sweep becomes something you play rather than \
    \something you address. Twelve is the ceiling and the thirteenth is \
    \silently dropped."
  RampleSlices ->
    "One file, cut into equal pieces, any of which the start point can reach. \
    \Sequenceable, and the only way to put a swept set under a melody — but \
    \nothing picks between them for you."
  RampleGrid ->
    "A stack of sliced files: the layer selector moves along one axis while the \
    \start point moves along the other. Both knobs at once, which is the whole \
    \of what this module can address in two dimensions."

-- | The sizes SLICER actually offers. Everything else is refused by the
-- | compiler, so it is refused here first.
slicerDivisions :: Array Int
slicerDivisions = [ 8, 12, 16, 24, 32, 48, 64, 128 ]

-- | An axis of the sample set: what it is called, how the module moves along
-- | it, and which sizes are available.
type Axis =
  { name :: String
  , picked :: String
  , sizes :: Array Int
  }

layerAxis :: Axis
layerAxis =
  { name: "layer"
  , picked: "the module: velocity, random or cyclic"
  , sizes: Array.range 2 12
  }

sliceAxis :: Axis
sliceAxis =
  { name: "slice"
  , picked: "you: the start point, sent ahead of the note"
  , sizes: slicerDivisions
  }

axes :: Encoding -> Array Axis
axes = case _ of
  RampleLayers -> [ layerAxis ]
  RampleSlices -> [ sliceAxis ]
  RampleGrid -> [ layerAxis, sliceAxis ]

-- | Somewhere sensible to start, per encoding. Twelve layers because that is
-- | the ceiling and a stack wants to use it; eight slices because it is the
-- | smallest division there is and a grid gets long fast.
defaultExtent :: Encoding -> Array Int
defaultExtent = case _ of
  RampleLayers -> [ 12 ]
  RampleSlices -> [ 16 ]
  RampleGrid -> [ 4, 8 ]

-- | How many samples the run will produce — which is how many hits it will
-- | play, and therefore how long it will take. Worth stating before Run and
-- | not after: a twelve-by-sixteen grid is **192 hits**.
total :: Array Int -> Int
total = Array.foldl (*) 1

-- | How many of the module's four voices one such set occupies, before stereo
-- | doubles it. Always one: every encoding here lives on a single voice, which
-- | is exactly why the two axes have to be layers and slices rather than voices.
voicesPer :: Encoding -> Int
voicesPer _ = 1

-- | **Everything the module will not do with this, said now.**
-- |
-- | Empty means it is expressible. The compiler enforces the same rules at
-- | write time; this is the same objection moved to before the recording, which
-- | is the only place it can save anything.
objections :: Encoding -> Array Int -> Array String
objections enc ext = Array.catMaybes (Array.mapWithIndex check (axes enc)) <> long
  where
  check i ax =
    let n = fromMaybe 0 (Array.index ext i)
    in
      if Array.elem n ax.sizes then Nothing
      else Just
        (ax.name <> " " <> show n <> " is not available — "
           <> (if ax.name == "slice"
                 then "SLICER offers only " <> joinInts ax.sizes
                 else "the module plays twelve layers and silently drops the rest"))

  long =
    let n = total ext
    in
      if n <= 128 then []
      else
        [ show n <> " samples is a long run: every one has to be played, \
          \recorded and divided inside a single take" ]

joinInts :: Array Int -> String
joinInts = Array.intercalate ", " <<< map show

-- | **One cell of the set: a position along each axis, in axis order.**
-- |
-- | An array rather than a record with named fields, because the number of axes
-- | is the encoding's business and a third one — a Rample drum machine adding
-- | voices to layers and slices — should be a row in a table rather than a new
-- | field everywhere.
type Cell = Array Int

-- | Every cell, **in the order it will be RECORDED**, and that order is not
-- | arbitrary.
-- |
-- | A grid is a stack of joined files: each layer is one file holding its
-- | slices end to end. So the take has to arrive in the order the compiler will
-- | cut it — every slice of layer 1, then every slice of layer 2 — because the
-- | module reads a voice's stack in byte order and reads a joined file left to
-- | right, and neither of those is negotiable. **Last axis varies fastest.**
-- |
-- | Getting this backwards would not fail. It would produce a card that plays,
-- | with the two axes transposed, and the only symptom would be that the
-- | instrument felt wrong.
cells :: Encoding -> Array Int -> Array Cell
cells enc ext = Array.foldl step [ [] ] (Array.mapWithIndex size (axes enc))
  where
  size i _ = max 1 (fromMaybe 1 (Array.index ext i))
  step acc n = do
    prefix <- acc
    j <- Array.range 0 (n - 1)
    pure (Array.snoc prefix j)
