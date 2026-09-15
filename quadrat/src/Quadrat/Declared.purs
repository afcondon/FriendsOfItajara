-- | `Quadrat.Declared` — **takes whose schedule arrives before the sound does.**
-- |
-- | The page has two ways of learning what was struck, and they are not two
-- | settings of one thing.
-- |
-- | A take played by hand, or by a black box like Progressions on an iPad, has
-- | to be *listened to*: the MIDI wire is watched and notes within 50 ms are
-- | called one chord. That is inference, and it is the best available when the
-- | thing making the music will not say what it did.
-- |
-- | Triggerfish will say. It knows the chords, their order and their times
-- | before a note sounds, and publishes them to Amphora as a clip. So the
-- | chords here are not detected at all — they are read off the declaration,
-- | and the grouping that is a guess about a human hand is exact about a
-- | machine's.
-- |
-- | **Declared beats inferred**, the rule this page was built on, arriving from
-- | a second direction: the sweep declares its own triggers, and now a
-- | progression declares its own chords.
module Quadrat.Declared
  ( Clip
  , fetchDeclared
  , cameBack
  ) where

import Prelude

import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (Aff, makeAff, nonCanceler)
import Effect.Exception (Error)

-- | One progression waiting to be sampled. `onsets` are seconds from the first
-- | chord — which is what `Main.schedule` wants, because a take is armed on its
-- | first sound and so starts where the first chord does.
-- |
-- | `rebus` is the identity Triggerfish minted. Both apps mint with
-- | `Rebus.chordsOf`/`rebusOf`, so a set stored against this rebus and the token
-- | on Triggerfish's shelf carry the same glyph without anyone arranging it.
type Clip =
  { hash :: String
  , name :: String
  , source :: String      -- "vetula" | "odonus" | …
  , kind :: String        -- what the publisher thinks it is: "chord-hits", …
  , rebus :: String
  , key :: String         -- e.g. "C Ionian"; "" when unknown
  , onsets :: Array Number
  , chords :: Array (Array Int)
  }

foreign import fetchDeclaredImpl
  :: (Error -> Effect Unit)
  -> (Array Clip -> Effect Unit)
  -> Effect Unit

-- | Everything published for sampling. An unreachable store is an empty list
-- | and a note, never fatal: the page samples perfectly well without one.
fetchDeclared :: Aff (Array Clip)
fetchDeclared = makeAff \cb -> do
  fetchDeclaredImpl (cb <<< Left) (cb <<< Right)
  pure nonCanceler

-- | **Has the page come back to the foreground since this was last asked?**
-- |
-- | The publish happens in another window, so the list changes when a human
-- | presses a button somewhere Quadrat cannot see. Asking the store on every
-- | poll would be a request a second for an answer that rarely moves; asking
-- | only at start-up means reloading to see what you just sent. Returning to
-- | the tab is the one moment it is worth re-reading, and it is the moment you
-- | are about to look.
-- |
-- | Reading CLEARS the latch, so one return is one fetch.
foreign import cameBackImpl :: Effect Boolean

cameBack :: Effect Boolean
cameBack = cameBackImpl
