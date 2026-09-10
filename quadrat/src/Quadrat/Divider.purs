-- | **The question put to the audio — chosen, not inferred.**
-- |
-- | Materially different sounds do not disagree about the *settings* of one
-- | algorithm; they disagree about which algorithm is asking the right thing.
-- | A drum hit is an attack, and spectral flux finds an attack exactly. A pad
-- | has no attack: it swells over a second or two, so flux peaks somewhere up
-- | the slope and reports a moment that means nothing musically. Measured on a
-- | take of six pad chords, `Attacks` found four onsets, of which one was
-- | right, one was 1.7 s inside the first chord, and three chords produced
-- | none at all — and no setting of the gap slider changed any of that, which
-- | is the tell. `Gaps` found all six.
-- |
-- | So the kind supplies a default and the buttons let you overrule it. The
-- | pictures underneath make it obvious within a second which was right, which
-- | is a better interface than a correct guess would be.
module Quadrat.Divider
  ( Divider(..)
  , all
  , name
  , label
  , blurb
  , defaultFor
  , needsCount
  ) where

import Prelude

import Quadrat.Kind (Kind(Bars, Chromatic, ChordHits, DrumHits, Longform))

data Divider
  -- | Spectral flux with a peak follower: where the signal changes fastest.
  = Attacks
  -- | Minima in the envelope the sound stands clear of on both sides: where
  -- | it comes back down.
  | Gaps
  -- | A fixed number of equal pieces, blind to the audio. When the grid is
  -- | known and even, a missed onset shifts everything after it and this
  -- | cannot miss.
  | Equal Int
  -- | One region, the whole take.
  | Whole

derive instance Eq Divider

all :: Int -> Array Divider
all n = [ Attacks, Gaps, Equal n, Whole ]

-- | What `msm onset --by` is given.
name :: Divider -> String
name = case _ of
  Attacks -> "attacks"
  Gaps -> "gaps"
  Equal n -> "equal:" <> show n
  Whole -> "whole"

-- | What the button says. Named for what it looks *for*, not for how it works
-- | — the mechanism is in the blurb and the answer is in the pictures.
label :: Divider -> String
label = case _ of
  Attacks -> "by attack"
  Gaps -> "by silence"
  Equal _ -> "equally"
  Whole -> "not at all"

blurb :: Divider -> String
blurb = case _ of
  Attacks -> "Where the sound changes fastest. Right for anything struck or \
             \plucked; finds nothing useful in a pad."
  Gaps -> "Where the sound comes back down. Right for anything held or bowed, \
          \and for hits with air between them."
  Equal _ -> "Blind to the audio: every piece the same length. Right when the \
             \grid is known and one missed onset would shift all the rest."
  Whole -> "Keep it in one piece."

needsCount :: Divider -> Boolean
needsCount = case _ of
  Equal _ -> true
  _ -> false

-- | The kind's usual answer, which is only a default.
defaultFor :: Kind -> Divider
defaultFor = case _ of
  DrumHits -> Attacks
  -- A chord is held, not struck. Even on a piano the useful boundary is the
  -- release; on a pad there is no attack to find at all.
  ChordHits -> Gaps
  Bars _ -> Attacks
  Chromatic -> Attacks
  Longform -> Whole
