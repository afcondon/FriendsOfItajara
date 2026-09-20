-- | **The card, divided into named blocks — ordinal, because the module
-- | renumbers.**
-- |
-- | A Morphagene card cannot describe itself: a reel is named by position,
-- | `mg1.wav` through `mgw.wav`, and `options.txt` carries configuration and
-- | the selected reel and nothing about contents. So at the panel you have a
-- | number and no other help, which makes a *grouping* the only navigational
-- | affordance there is — "1 to 8 are voices, 9 onward are chords" is
-- | something a player holds in their head mid-patch; thirty-two arbitrary
-- | positions is not.
-- |
-- | ## Why the ranges are computed and not written down
-- |
-- | The first version of this declared fixed ranges — voices 1-8, chords 9-20
-- | — and that cannot work, measured 2026-09-19: **the Morphagene compacts the
-- | card on load.** A reel written to slot 9 with five reels present came back
-- | as `mg6`, rewritten rather than renamed (the module stamps 2013, having no
-- | clock) with its audio byte-identical. The module keeps one contiguous run
-- | `mg1..mgN` and closes any gap.
-- |
-- | So a slot cannot be **reserved**. What survives is **order**: the reel went
-- | to the next free position and kept its place relative to the others. Blocks
-- | therefore work as *runs in sequence* whose boundaries move as the card
-- | fills, not as addresses.
-- |
-- | Which makes the card a **build artefact rather than an editable store**:
-- | you compile it in one pass, in section order, from a database that knows
-- | what everything is. Dropping a reel into a "reserved" address and expecting
-- | it to stay is the thing the module will not allow.
-- |
-- | Each section therefore declares only how many reels it **wants**, and
-- | `ranges` lays them end to end. Change one size and the rest reflow, which
-- | is the ordinal property made structural: there is nowhere to write a
-- | number that could disagree with the layout.
module Quadrat.Sections
  ( Section
  , sections
  , Range
  , ranges
  , sectionAt
  , landing
  , slots
  , covers
  ) where

import Prelude

import Data.Array as Array
import Data.Foldable (foldl)
import Data.Maybe (Maybe)

-- | How many reels a Morphagene card holds. `mg1`..`mg9`, then `mga`..`mgw`.
slots :: Int
slots = 32

type Section =
  { name :: String
  -- | How many reels this block wants. Positions are derived from the order
  -- | and these counts; nothing here names a slot.
  , wants :: Int
  -- | What belongs here, in the player's words, so the rule is legible at the
  -- | moment of placing rather than in a document.
  , holds :: String
  }

-- | The blocks, in the order they occupy the card.
-- |
-- | The sizes are a judgement about what there is and what there will be, not
-- | an even division: chords are the biggest because twelve reels of them
-- | already exist, and beats are reserved because none do yet.
sections :: Array Section
sections =
  [ { name: "voices"
    , wants: 8
    , holds: "spoken word \x2014 readings, poetry, recordings of talk"
    }
  , { name: "chords"
    , wants: 12
    , holds: "chord sets and progressions, the same material the Rample carries"
    }
  , { name: "beats"
    , wants: 8
    , holds: "harvested beats \x2014 whole bars, split on bar lines"
    }
  , { name: "wild"
    , wants: 4
    , holds: "longform takes, drum hits, and whatever is being tried this week"
    }
  ]

type Range = { name :: String, holds :: String, from :: Int, to :: Int }

-- | The sections laid end to end, one after another from slot 1.
ranges :: Array Range
ranges = (foldl step { at: 1, acc: [] } sections).acc
  where
  step s sec =
    { at: s.at + sec.wants
    , acc: Array.snoc s.acc
        { name: sec.name, holds: sec.holds, from: s.at, to: s.at + sec.wants - 1 }
    }

sectionAt :: Int -> Maybe Range
sectionAt n = Array.find (\r -> n >= r.from && n <= r.to) ranges

-- | **Where a reel written to slot `n` will actually end up.**
-- |
-- | The module closes gaps, so a reel lands after however many reels already
-- | sort before it — regardless of the number it was written under. Given the
-- | slots a card currently holds, this is that arithmetic, and it is exact:
-- | writing slot 9 to a card holding 1-5 predicts 6, which is what the module
-- | did.
-- |
-- | Worth showing rather than hiding. A write that silently lands somewhere
-- | else is the failure this project keeps meeting — the act succeeded and the
-- | report of it was wrong.
landing :: Array Int -> Int -> Int
landing taken n = 1 + Array.length (Array.filter (_ < n) taken)

-- | Do the sections account for exactly the card? A total under `slots` leaves
-- | positions no block names; over it promises room that does not exist.
covers :: Boolean
covers = foldl (\a s -> a + s.wants) 0 sections == slots
