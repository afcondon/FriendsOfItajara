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
module Quadrat.Encoding
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
  , onCard
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
  -- | **A folder of files, indexed by `n`.** No count limit, no division, no
  -- | layer ceiling, no format to convert to — the destination with no
  -- | constraints, which is exactly what makes it the test of this model.
  | DirtBank
  -- | The same folder, recorded as a grid. SuperDirt still addresses **one**
  -- | index, so the grid is flattened on the way in and the arithmetic to get
  -- | back out of it is `n = outer * inners + inner`.
  | DirtGrid

derive instance Eq Encoding

all :: Array Encoding
all = [ RampleLayers, RampleSlices, RampleGrid, DirtBank, DirtGrid ]

name :: Encoding -> String
name = case _ of
  RampleLayers -> "rample-layers"
  RampleSlices -> "rample-slices"
  RampleGrid -> "rample-grid"
  DirtBank -> "dirt-bank"
  DirtGrid -> "dirt-grid"

label :: Encoding -> String
label = case _ of
  RampleLayers -> "Rample · layers"
  RampleSlices -> "Rample · slices"
  RampleGrid -> "Rample · layers × slices"
  DirtBank -> "SuperDirt · a bank"
  DirtGrid -> "SuperDirt · a grid, flattened"

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
  DirtBank ->
    "A folder of files, played as s \"name\" # n 3. Nothing is refused: any \
    \count, any length, any format, any order \x2014 the set as it is stored \
    \IS the bank, so there is nothing to compile and nothing to convert."
  DirtGrid ->
    "The same folder, recorded as a grid. SuperDirt addresses ONE index, so \
    \the two axes are flattened into it on the way in: n = outer * inners + \
    \inner. Nothing is lost \x2014 what each n means is written down beside \
    \the audio, which is the only place two dimensions could survive."

-- | The sizes SLICER actually offers. Everything else is refused by the
-- | compiler, so it is refused here first.
slicerDivisions :: Array Int
slicerDivisions = [ 8, 12, 16, 24, 32, 48, 64, 128 ]

-- | An axis of the sample set: what it is called, how the module moves along
-- | it, and which sizes are available.
type Axis =
  { name :: String
  , picked :: String
  -- | The sizes the destination offers. **Enumerated where the destination
  -- | enumerates them** — SLICER's eight divisions are the whole of what a
  -- | slice axis can be, and a fifth entry would be a lie about the module.
  , sizes :: Array Int
  -- | **`sizes` is a range rather than a list**, so any number between its
  -- | ends is legal and a chooser would be the wrong control.
  -- |
  -- | This is the one place SuperDirt bent the model, and it bent it exactly
  -- | once: a destination that refuses nothing still has to say so somehow,
  -- | and the honest way is a flag rather than a five-hundred-entry dropdown
  -- | pretending to be a set of choices. `objections` needs no change — the
  -- | ends of `sizes` still bound it.
  , free :: Boolean
  }

layerAxis :: Axis
layerAxis =
  { name: "layer"
  , picked: "the module: velocity, random or cyclic"
  , sizes: Array.range 2 12
  , free: false
  }

sliceAxis :: Axis
sliceAxis =
  { name: "slice"
  , picked: "you: the start point, sent ahead of the note"
  , sizes: slicerDivisions
  , free: false
  }

-- | **The axis with nothing wrong with it.**
-- |
-- | `n` is an index into a folder and SuperDirt neither counts nor converts,
-- | so the only bound is how long you are prepared to wait. 512 is the ceiling
-- | because a run has to fit in one take, not because anything downstream
-- | objects — see the long-run note in `objections`.
nAxis :: String -> String -> Axis
nAxis nm how = { name: nm, picked: how, sizes: Array.range 1 512, free: true }

axes :: Encoding -> Array Axis
axes = case _ of
  RampleLayers -> [ layerAxis ]
  RampleSlices -> [ sliceAxis ]
  RampleGrid -> [ layerAxis, sliceAxis ]
  DirtBank -> [ nAxis "n" "you: `n` in the pattern" ]
  -- Outer and inner rather than two names of their own, because that is what
  -- the flattening arithmetic calls them and a person reading `n = outer *
  -- inners + inner` should find the same two words on the page.
  DirtGrid ->
    [ nAxis "outer" "you: the slower half of `n`"
    , nAxis "inner" "you: the faster half of `n`"
    ]

-- | Somewhere sensible to start, per encoding. Twelve layers because that is
-- | the ceiling and a stack wants to use it; eight slices because it is the
-- | smallest division there is and a grid gets long fast.
defaultExtent :: Encoding -> Array Int
defaultExtent = case _ of
  RampleLayers -> [ 12 ]
  RampleSlices -> [ 16 ]
  RampleGrid -> [ 4, 8 ]
  DirtBank -> [ 16 ]
  DirtGrid -> [ 4, 8 ]

-- | How many samples the run will produce — which is how many hits it will
-- | play, and therefore how long it will take. Worth stating before Run and
-- | not after: a twelve-by-sixteen grid is **192 hits**.
total :: Array Int -> Int
total = Array.foldl (*) 1

-- | **Can a set on this encoding be given an address?**
-- |
-- | A Rample set goes to a bank, a kit and a voice, and is nowhere until it
-- | does. A SuperDirt set *is* the bank as stored — there is no voice to put
-- | it in and no second act to perform. So placing is offered for one family
-- | and meaningless for the other, and a page that shows bank/kit/voice beside
-- | a SuperDirt set is asking a question with no answer.
onCard :: Encoding -> Boolean
onCard = case _ of
  RampleLayers -> true
  RampleSlices -> true
  RampleGrid -> true
  DirtBank -> false
  DirtGrid -> false

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
      -- A free axis is bounded and not enumerated, so the objection is about
      -- the ends and nothing in between.
      else if ax.free then Just
        (ax.name <> " " <> show n <> " is outside 1\x2026"
           <> show (fromMaybe 0 (Array.last ax.sizes)))
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
