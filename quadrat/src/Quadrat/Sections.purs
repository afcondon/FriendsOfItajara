-- | **The card, divided into named blocks — our convention, not the module's.**
-- |
-- | A Morphagene card cannot describe itself. A reel is named by position,
-- | `mg1.wav` through `mgw.wav`, and the module reads nothing else: the set's
-- | name does not travel onto the card, and `options.txt` turned out to carry
-- | configuration and the selected reel and nothing about contents at all
-- | (measured 2026-09-19, by cutting a 27-reel card to two and reading it
-- | back). So at the panel you have a number and no other help.
-- |
-- | Which makes a *grouping* the only navigational affordance there is.
-- | "Reels 1 to 8 are voices, 9 to 20 are chords, 21 to 28 are beats" is
-- | something a player can hold in their head mid-patch; thirty-two arbitrary
-- | positions is not. So the blocks are drawn on the slot strip, and placing a
-- | set means dropping it in a *section* and letting the number follow —
-- | rather than picking a number and hoping to remember what lives there.
-- |
-- | This is emphatically **not** a module fact and must not migrate into
-- | `Dest.Facts`, which is for what the hardware imposes. It is a habit made
-- | visible, and a habit only works if it is written down in one place. The
-- | module will happily put a drum loop in slot 3; this table is what stops
-- | *us* doing it by accident.
-- |
-- | Ranges are inclusive, one-based, and must tile 1..32 without gaps or
-- | overlap — `covers` is the property that says so.
module Quadrat.Sections
  ( Section
  , sections
  , sectionAt
  , covers
  , slots
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe)

-- | How many reels a Morphagene card holds. `mg1`..`mg9`, then `mga`..`mgw`.
slots :: Int
slots = 32

type Section =
  { name :: String
  -- | First and last slot, inclusive, one-based.
  , from :: Int
  , to :: Int
  -- | What belongs here, in the player's words — shown as the block's title so
  -- | the rule is legible at the moment of placing rather than in a document.
  , holds :: String
  }

-- | The blocks, in slot order.
-- |
-- | The sizes are a judgement about what there is and what there will be, not
-- | an even division: chords are the biggest block because twelve reels of
-- | them already exist, and beats are reserved because none do yet. An empty
-- | slot is silent and harmless — the old card proved that six times over —
-- | so reserving space costs nothing, while a full card forces a deletion
-- | decision every time something new is worth trying.
sections :: Array Section
sections =
  [ { name: "voices"
    , from: 1, to: 8
    , holds: "spoken word \x2014 readings, poetry, recordings of talk"
    }
  , { name: "chords"
    , from: 9, to: 20
    , holds: "chord sets and progressions, the same material the Rample carries"
    }
  , { name: "beats"
    , from: 21, to: 28
    , holds: "harvested beats \x2014 whole bars, split on bar lines"
    }
  , { name: "wild"
    , from: 29, to: 32
    , holds: "longform takes, drum hits, and whatever is being tried this week"
    }
  ]

-- | Which section a slot falls in.
sectionAt :: Int -> Maybe Section
sectionAt n = Array.find (\s -> n >= s.from && n <= s.to) sections

-- | Do the sections tile every slot exactly once? A gap would leave a slot the
-- | strip could draw and the table could not name; an overlap would let one
-- | slot answer to two blocks. Neither is a thing to discover on the card.
covers :: Boolean
covers =
  Array.length (Array.nub owners) == slots
    && Array.length owners == slots
  where
  owners = Array.concatMap (\s -> map (\n -> n) (Array.range s.from s.to)) sections
