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
  , rememberPick
  , recallPick
  ) where

import Prelude

import Amphora.Api as Api
import Control.Parallel (parTraverse)
import Data.Array (catMaybes, head)
import Data.Either (Either(..))
import Data.Function.Uncurried (Fn4, runFn4)
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, toMaybe)
import Effect (Effect)
import Effect.Aff (Aff, throwError)
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Hawser.Fetch (troubleText)
import Hawser.Fetch as Fetch
import Hawser.Http (Endpoint)

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
  -- | What the publisher says this IS, and therefore how it is played and
  -- | divided: `"chord-hits"` are alternatives, one sample each, struck on the
  -- | page's own even grid; `"phrase"` is a piece of music, played once at its
  -- | own times and kept whole.
  , kind :: String
  , rebus :: String
  , key :: String         -- e.g. "C Ionian"; "" when unknown
  -- | **The chords that leave that key**, by index from zero, as the
  -- | publisher checked them (Harmonia's `ProgressionKey`). The key is the one
  -- | the progression was written in, not a promise every chord keeps to it.
  , outside :: Array Int
  , onsets :: Array Number
  , chords :: Array (Array Int)
  -- | How long each chord is held, in ms, as the publisher declared it. A
  -- | trigger's `ms` is a gate for a drum — ten milliseconds, and right for one
  -- | — where a chord wants holding. Carried per chord rather than once,
  -- | because nothing says a progression holds evenly.
  , gates :: Array Number
  -- | How hard each was struck. A captured phrase's dynamics are part of the
  -- | material for the same reason its rhythm is: play it all at one velocity
  -- | and you have recorded a different piece. A minted progression's are
  -- | uniform anyway, so nothing is lost by carrying it for both.
  , vels :: Array Int
  }

-- | Where the store is: the page's own host, on :3024.
foreign import storeBase :: Effect String

-- | One clip from a content's hash and payload and its first label's name and
-- | tags; null when the payload is not a clip with events.
foreign import declaredOf :: Fn4 String String String (Array String) (Nullable Clip)

-- | Everything published for sampling: each favourite of `triggerfish-clips`,
-- | with its content and its first label. An unreachable store is an empty
-- | list and a note, never fatal: the page samples perfectly well without one.
-- |
-- | Each request is an operation of `Amphora.Api`, the store's own description
-- | of what it answers; nothing here writes a path. A store that is down
-- | should leave the page saying "nothing declared" in a second or two, so
-- | each gives up after 2.5 s.
fetchDeclared :: Aff (Array Clip)
fetchDeclared = do
  base <- liftEffect storeBase
  let
    ask :: forall request reply. Endpoint request reply -> request -> Aff reply
    ask described request = Fetch.ask { base, timeoutMs: 2500 } described request >>= case _ of
      Right reply -> pure reply
      Left trouble -> throwError (error ("Amphora: " <> troubleText trouble))
  favourites <- ask Api.listFavorites { collection: Just "triggerfish-clips" }
  map catMaybes $ favourites # parTraverse \{ contentHash } -> do
    content <- ask Api.getContent { hash: contentHash }
    labels <- ask Api.listLabels { hash: Just contentHash }
    pure (head labels >>= \label -> toMaybe (runFn4 declaredOf contentHash content.payload label.name label.tags))

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

-- | **Remember which progression is picked**, by hash, across a reload.
-- |
-- | The hash only: the clip is re-fetched from the store, so this is a pointer
-- | and can never be a stale copy of chords that have since changed. An empty
-- | string forgets.
foreign import rememberPick :: String -> Effect Unit

foreign import recallPick :: Effect String
