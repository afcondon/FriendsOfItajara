-- | **Where a set can go, and what that place imposes.**
-- |
-- | The destination pane used to be a heading that said "Squarp Rample" and a
-- | table that could only have meant a Rample. That was honest while there was
-- | one destination and dishonest the moment there were four, because **the
-- | shape of the destination is what decides the controls beneath it, and that
-- | shape is not a skin**: a Rample is banks of kits of four voices of twelve
-- | layers with one global SLICER; a QuadDrum is a folder per set at the root,
-- | 128 to a voice; a Morphagene reel is one long file with splice markers
-- | inside it; an Arbhar stick is six banks of thirty-six addressed by
-- | position. None of the four is another with different words — a layer is
-- | not a splice is not a position — so none of them can share a table, and
-- | the thing that must NOT be shared is *where a second set goes*.
-- |
-- | Hence tabs rather than a dropdown: a dropdown says "the same panel, filtered",
-- | and what is wanted is "a different panel".
-- |
-- | `built` is the one field that is about this page rather than about the
-- | module. Three of the four have no placement machinery yet, and a tab that
-- | quietly offered Rample's controls under a Morphagene heading would be the
-- | failure this project keeps meeting: **the act succeeded and the report of
-- | it was wrong**. So an unbuilt tab says what the module imposes — which is
-- | measured and useful today — and says plainly that nothing here writes it.
module Quadrat.Dest
  ( Dest(..)
  , all
  , Facts
  , factsOf
  , slug
  ) where

import Prelude

import Data.Maybe (Maybe(..))

data Dest = Rample | QuadDrum | Morphagene | Arbhar

derive instance Eq Dest
derive instance Ord Dest

-- | In the order they are likely to be reached for, not alphabetically: the
-- | Rample is the one with machinery, and the other three are the ones being
-- | tested next.
all :: Array Dest
all = [ Rample, QuadDrum, Morphagene, Arbhar ]

type Facts =
  { name :: String
  , maker :: String
  -- | How it is ADDRESSED — the one sentence that decides the controls.
  , shape :: String
  -- | Sample rate, bit depth, channels, as `msm`'s own profile has them.
  , format :: String
  -- | What the module imposes, one fact per line. Limits discovered by having
  -- | a build refused, not read off a datasheet.
  , imposes :: Array String
  -- | What a set most naturally becomes here, which is different for each and
  -- | is the reason the tabs are not one panel.
  , suits :: String
  -- | `Nothing` once this page can place onto it. `Just why` while it cannot.
  , unbuilt :: Maybe String
  }

factsOf :: Dest -> Facts
factsOf = case _ of
  Rample ->
    { name: "Rample"
    , maker: "Squarp"
    , shape: "banks of kits, 4 voices, 12 layers each, one SLICER for the whole card"
    , format: "44.1 kHz \x00b7 16-bit \x00b7 mono or stereo"
    , imposes:
        [ "kit folders named ?X in the root \x2014 A0 to Z99, 2600 of them"
        , "12 layers per voice, hard: a thirteenth never plays"
        , "a stereo sample pairs UPWARD and consumes the voice after it"
        , "SLICER is one setting for the whole card, so it cannot be carried per kit"
        , "the layer selector is CC (voice \x00d7 10 + 9), and it is absolute"
        ]
    , suits: "alternatives \x2014 one hit per layer, the module choosing between them"
    , unbuilt: Nothing
    }
  QuadDrum ->
    { name: "QuadDrum"
    , maker: "vpme.de"
    , shape: "a folder per sample set at the root, 4 voices, up to 128 samples each"
    , format: "44.1 kHz \x00b7 16-bit \x00b7 mono preferred"
    , imposes:
        [ "128 samples per voice, 1024 on the card"
        , "copy the whole card in one operation after erasing it \x2014 fragmentation costs playback"
        , "no layer ceiling to work around: the constraint here is the opposite one"
        ]
    , suits: "runs \x2014 a long chromatic or a whole sweep, where the Rample's twelve run out"
    , unbuilt: Just "what a QuadDrum is BEST used for is still an open question \x2014 \
                    \128 a voice is a different instrument from 12, and the placement \
                    \here should follow the answer rather than guess it"
    }
  Morphagene ->
    { name: "Morphagene"
    , maker: "Make Noise"
    , shape: "one reel per file at the root, divided by splice markers inside the audio"
    , format: "48 kHz \x00b7 32-bit float \x00b7 stereo"
    , imposes:
        [ "splices are WAV cue points, up to 300 in a reel"
        , "a reel is at most about 174 seconds"
        , "files go in the root of the card, no folders"
        ]
    , suits: "progressions \x2014 a whole chord sequence per splice, not one hit per splice"
    , unbuilt: Just "Quadrat can already record a take of this shape; what is missing is \
                    \writing the cue points, which is `msm splice` and not a card layout"
    }
  Arbhar ->
    { name: "Arbhar"
    , maker: "Instruo"
    , shape: "a stick of six banks of thirty-six, addressed by POSITION rather than by name"
    , format: "48 kHz \x00b7 24-bit \x00b7 stereo"
    , imposes:
        [ "the first 13 seconds of each sample is what loads"
        , "positional addressing: the slot number is the identity, the filename is not"
        , "_toConvert converts on boot; _userfiles expects the format already right"
        ]
    , suits: "grain material \x2014 washes and sustained tones rather than hits"
    , unbuilt: Just "the Friend already writes these, through `msm harvest`, which is the \
                    \compiler between a name and a position \x2014 Quadrat should reach that \
                    \rather than grow a second one"
    }

slug :: Dest -> String
slug = case _ of
  Rample -> "rample"
  QuadDrum -> "quaddrum"
  Morphagene -> "morphagene"
  Arbhar -> "arbhar"
