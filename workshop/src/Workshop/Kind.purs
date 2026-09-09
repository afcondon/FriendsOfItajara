-- | **What is being recorded, and therefore what happens to it.**
-- |
-- | The looper's controls are about *time* — record, overdub, undo, a loop and
-- | its layers. The Workshop's are about *material*, because everything that
-- | matters afterwards follows from what the thing IS: how it is armed, when it
-- | closes, whether it is divided, and what the divisions mean.
-- |
-- | Five kinds, and the same taxonomy `msm`'s classifier reads a card with.
module Workshop.Kind
  ( Kind(..)
  , all
  , name
  , label
  , blurb
  , Close(..)
  , closes
  , divides
  , material
  , prompt
  , Fold(..)
  , foldsTo
  , joins
  , voicesOn
  ) where

import Prelude

import Data.Maybe (Maybe(..))

data Kind
  -- | Many hits of one drum, played into one recording. The order is the
  -- | dynamic order, because the module reads a stack in byte order and
  -- | velocity picks along it.
  = DrumHits
  -- | Discrete chords, one per strike, each allowed to ring.
  | ChordHits
  -- | A bar, or a count of them, at tempo.
  | Bars Int
  -- | Twelve of something, pitched. **Not yet designed** — see `Workshop.Card`
  -- | for the question it turns on.
  | Chromatic
  -- | One long take. A pad, a field recording, a drone.
  | Longform

derive instance Eq Kind

all :: Array Kind
all = [ DrumHits, ChordHits, Bars 1, Chromatic, Longform ]

name :: Kind -> String
name = case _ of
  DrumHits -> "drum-hits"
  ChordHits -> "chord-hits"
  Bars _ -> "bars"
  Chromatic -> "chromatic"
  Longform -> "longform"

label :: Kind -> String
label = case _ of
  DrumHits -> "Drum hits"
  ChordHits -> "Chord hits"
  Bars n -> if n == 1 then "One bar" else show n <> " bars"
  Chromatic -> "Chromatic"
  Longform -> "Longform"

blurb :: Kind -> String
blurb = case _ of
  DrumHits ->
    "Play them all into one take, softest first. Divided at the transients \
    \afterwards, and the order you played them in is the order velocity picks \
    \them in."
  ChordHits ->
    "One strike each, let them ring. Divided the same way, with a decay window \
    \long enough for a piano rather than a kick."
  Bars _ ->
    "Starts on your first note and closes itself at the count. Kept whole — \
    \a groove cut into hits stops being a groove."
  Chromatic ->
    "Twelve of something, pitched. How the pitches get their names is not \
    \settled yet."
  Longform ->
    "Starts on your first sound and runs until you stop it. Nothing is divided."

-- | **When a take ends.** Every kind starts the same way — armed, on the first
-- | sound — because a countdown you have to play to is a worse instrument than
-- | one that waits. They differ in how they stop.
data Close
  -- | You press Stop. The take is as long as you played.
  = ByHand
  -- | The daemon closes it, at a length known before a note was played.
  | AtCount Int

derive instance Eq Close

closes :: Kind -> Close
closes = case _ of
  DrumHits -> ByHand
  ChordHits -> ByHand
  Bars n -> AtCount n
  -- Twelve notes is a count of *events*, not of bars, and the daemon counts
  -- bars. So it is by hand until something counts onsets live.
  Chromatic -> ByHand
  Longform -> ByHand

-- | How the take is divided once it is closed, as `msm onset` names it.
-- |
-- | `Nothing` is not "we have not got round to it" — a bar and a longform take
-- | are whole things, and dividing them would remove what they are.
divides :: Kind -> Maybe String
divides = case _ of
  DrumHits -> Just "hits"
  ChordHits -> Just "chords"
  Bars _ -> Nothing
  Chromatic -> Just "hits"
  Longform -> Nothing

-- | What `msm` should be told the material is.
material :: Kind -> String
material = case _ of
  DrumHits -> "hits"
  ChordHits -> "chords"
  Bars _ -> "break"
  Chromatic -> "hits"
  Longform -> "ambient"

-- | **How many channels this material wants on the card.**
-- |
-- | Not how it is captured: a take is recorded in whatever the input offers,
-- | because folding at capture throws the right channel away for ever and
-- | folding at the cut loses nothing. This is the decision made *later*, and
-- | it is a property of the material — Andrew's rule, and it matches the
-- | module: drum hits and most bass are mono, pads and chords and progressions
-- | and ambient takes lean stereo.
data Fold = ToMono | ToStereo

derive instance Eq Fold

foldsTo :: Kind -> Fold
foldsTo = case _ of
  DrumHits -> ToMono
  -- A chord is a voicing in a room, and the room is half of it.
  ChordHits -> ToStereo
  Bars _ -> ToMono
  Chromatic -> ToMono
  Longform -> ToStereo

-- | **How many of a Rample's four voices one sample occupies.**
-- |
-- | Measured, and from Squarp: *a stereo sample will fill 2 mono voices*. So a
-- | kit is four mono voices, or **two stereo ones**, or a mix — and a stereo
-- | kit answers on SP1 and SP3 rather than on all four trigger notes, which is
-- | a fact anything playing it has to know.
voicesOn :: Kind -> Int
voicesOn k = case foldsTo k of
  ToMono -> 1
  ToStereo -> 2

-- | **Does this material become ONE file with slices, or a stack of layers?**
-- |
-- | A phrase joins. The module plays twelve layers and silently drops the
-- | rest, so a bar cut into sixteen cannot be sixteen layers — and layers are
-- | chosen by the layer selector, which cannot be sequenced per note. Joined,
-- | every piece sits under the start point and can be triggered in any order,
-- | which is the whole reason to slice a phrase rather than keep it whole.
-- |
-- | Hits and chords stay as layers: their pieces are alternatives to each
-- | other, not a sequence, and the layer selector is exactly the right way to
-- | choose between them.
joins :: Kind -> Boolean
joins = case _ of
  DrumHits -> false
  ChordHits -> false
  Bars _ -> true
  Chromatic -> false
  -- One take, kept whole — which the joiner would also do, but by a longer
  -- road and with a division nobody asked for.
  Longform -> false

-- | What to say while it is listening.
prompt :: Kind -> String
prompt = case _ of
  DrumHits -> "listening — play them softest first, then stop"
  ChordHits -> "listening — one strike each, let them ring"
  Bars n -> "listening — it starts on your first note and closes after "
              <> show n <> (if n == 1 then " bar" else " bars")
  Chromatic -> "listening — twelve, in order"
  Longform -> "listening — starts on your first sound, stop when you are done"
