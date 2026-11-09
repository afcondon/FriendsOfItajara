-- | **A voicing, on a grand staff.**
-- |
-- | A chord set records what was struck inside each region, absolute and in
-- | register, and until now the only thing ever done with that was to count
-- | it. A count is the one reading of a chord that carries no music: five
-- | notes could be a cluster or an open voicing two octaves wide, and which
-- | of those it is decides whether the sample is usable for anything.
-- |
-- | So it is drawn where a musician can read it. Not a pitch ribbon and not a
-- | list of note names — a stave, because the spacing between noteheads IS
-- | the voicing, and that is the fact being looked for.
-- |
-- | ## One axis, both staves
-- |
-- | The two staves of a grand staff are one continuous scale of DIATONIC
-- | steps with a gap in the middle, so everything here works in step numbers
-- | rather than semitones: `d = octave * 7 + step-within-octave`. Middle C
-- | lands on 35, one step below the treble's bottom line and one above the
-- | bass's top, which is exactly the ledger line it is written on. Nothing
-- | needs to know which staff a note belongs to; the y position decides, and
-- | ledger lines follow from being outside a staff's own five.
-- |
-- | ## Sharps, never flats
-- |
-- | The notes arrive as MIDI numbers and a MIDI number does not know how it
-- | was spelled: 63 is D# or Eb depending on music nobody recorded. One
-- | consistent spelling beats a correct enharmonic guessed at, so every
-- | accidental is a sharp — which is also what makes the diatonic step of a
-- | pitch class a function rather than a judgement.
module Quadrat.Stave
  ( grand
  , grandIn
  , spanOf
  , diatonic
  ) where

import Prelude

import Data.Array as Array
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe)
import Halogen.HTML as HH
import Quadrat.Wave (attr, el)

-- | Half a line-space, in pixels. Everything vertical is a multiple of it:
-- | a line-space is two, a staff is eight.
step :: Number
step = 4.0

-- | The diatonic step of each pitch class, and whether it carries a sharp.
-- | Sharps throughout — see the note above.
degreeOf :: Int -> { deg :: Int, sharp :: Boolean }
degreeOf pc = fromMaybe { deg: 0, sharp: false } (Array.index table pc)
  where
  table =
    [ { deg: 0, sharp: false }, { deg: 0, sharp: true }
    , { deg: 1, sharp: false }, { deg: 1, sharp: true }
    , { deg: 2, sharp: false }
    , { deg: 3, sharp: false }, { deg: 3, sharp: true }
    , { deg: 4, sharp: false }, { deg: 4, sharp: true }
    , { deg: 5, sharp: false }, { deg: 5, sharp: true }
    , { deg: 6, sharp: false }
    ]

-- | A MIDI note as a position on the one continuous diatonic axis, with the
-- | accidental that spelling it needed. Middle C (60) is 35.
diatonic :: Int -> { d :: Int, sharp :: Boolean }
diatonic n =
  let g = degreeOf (n `mod` 12)
  in { d: (n / 12) * 7 + g.deg, sharp: g.sharp }

-- | The five lines of each staff, as diatonic steps.
-- |
-- | Treble E4 G4 B4 D5 F5 and bass G2 B2 D3 F3 A3, worked out rather than
-- | written down so that the arithmetic above is the only thing to be wrong.
trebleLines :: Array Int
trebleLines = map (_.d <<< diatonic) [ 64, 67, 71, 74, 77 ]

bassLines :: Array Int
bassLines = map (_.d <<< diatonic) [ 43, 47, 50, 53, 57 ]

-- | The whole system, top and bottom, in diatonic steps — before any ledger
-- | lines a particular voicing needs.
topD :: Int
topD = fromMaybe 45 (Array.last trebleLines)

botD :: Int
botD = fromMaybe 25 (Array.head bassLines)

-- | **The vertical extent a set of voicings needs**, so that a row of them
-- | can share one.
-- |
-- | Drawn per-voicing, each stave sizes itself to its own notes — and then a
-- | row of them does not line up, so a note that LOOKS higher than its
-- | neighbour need not be. For a progression that is the whole comparison
-- | being offered, so the range is computed once over everything and handed
-- | to each.
spanOf :: Array Int -> { lo :: Int, hi :: Int }
spanOf notes =
  let ds = Array.sort (map (_.d <<< diatonic) notes)
  in { lo: min botD (fromMaybe botD (Array.head ds))
     , hi: max topD (fromMaybe topD (Array.last ds))
     }

-- | **A voicing**, on its own extent. Empty draws nothing at all rather than
-- | an empty staff: a sample with no notes recorded is not a sample with no
-- | notes.
grand :: forall w i. Array Int -> HH.HTML w i
grand notes =
  let s = spanOf notes
  in grandIn { lo: s.lo, hi: s.hi, clefs: true } notes

-- | A voicing drawn on an extent chosen by the caller, so several can share
-- | one and be compared.
-- |
-- | `clefs` because a system carries them once at its start and not on every
-- | bar of it: a row of seven voicings is one system, and seven copies of a
-- | G clef is six copies of a thing you already read.
grandIn :: forall w i. { lo :: Int, hi :: Int, clefs :: Boolean } -> Array Int -> HH.HTML w i
grandIn ext notes
  | Array.null notes = HH.text ""
  | otherwise =
      el "svg"
        [ attr "viewBox" ("0 0 " <> num width <> " " <> num height)
        , attr "width" (num width)
        , attr "height" (num height)
        , attr "class" "q-stave"
        ]
        (staffLines <> clefGlyphs <> ledgers <> heads)
  where
  ds = map diatonic (Array.sort notes)
  -- The extent is given rather than derived, so a row of these lines up. It
  -- is still widened here if a voicing somehow exceeds it: a chord that runs
  -- off the top of the page is the one you most want to see the top of.
  hiD = max ext.hi (fromMaybe ext.hi (Array.last (Array.sort (map _.d ds))))
  loD = min ext.lo (fromMaybe ext.lo (Array.head (Array.sort (map _.d ds))))
  pad = 5.0
  height = Int.toNumber (hiD - loD) * step + pad * 2.0
  -- Room for an accidental, a notehead, and the second-offset a cluster
  -- needs — plus the clefs, when this stave carries them.
  clefRoom = if ext.clefs then 20.0 else 0.0
  width = 34.0 + clefRoom
  x0 = 14.0 + clefRoom
  y d = pad + Int.toNumber (hiD - d) * step

  line d extra =
    el "line"
      ([ attr "x1" (num (clefRoom + 6.0)), attr "y1" (num (y d))
       , attr "x2" (num (width - 2.0)), attr "y2" (num (y d))
       ] <> extra)
      []

  -- | **The clefs, as glyphs.**
  -- |
  -- | Each is anchored on the line it names, which is what a clef IS: the G
  -- | clef curls around G4 and the F clef's two dots straddle F3. Positioned
  -- | from those lines rather than from the staff, so the arithmetic above
  -- | stays the only thing that can be wrong.
  -- |
  -- | The glyphs live in Apple Symbols and, on this machine, in nothing else
  -- | — so a fallback stack cannot save them off a Mac. Quadrat runs on the
  -- | rig, which is one; if it ever leaves, these want drawing as paths.
  clefGlyphs
    | not ext.clefs = []
    | otherwise =
        [ clefAt "\x1d11e" (_.d (diatonic 67)) 34.0 "q-clef is-g"
        , clefAt "\x1d122" (_.d (diatonic 53)) 22.0 "q-clef is-f"
        ]

  clefAt glyph onLine size klass =
    el "text"
      [ attr "x" (num 4.0)
      , attr "y" (num (y onLine))
      , attr "font-size" (num size)
      , attr "dominant-baseline" "central"
      , attr "class" klass
      ]
      [ HH.text glyph ]

  staffLines = map (\d -> line d [ attr "class" "q-staveline" ])
    (trebleLines <> bassLines)

  -- | **Ledger lines, per note and only where they are needed.**
  -- |
  -- | Every line-step between a note and the staff it is outside — which is
  -- | what makes a note three ledgers up readable as three ledgers up rather
  -- | than as a notehead floating in white.
  ledgers = Array.nub (Array.concatMap ledgersFor ds) <#> \d ->
    el "line"
      [ attr "x1" (num (x0 - 5.0)), attr "y1" (num (y d))
      , attr "x2" (num (x0 + 5.0)), attr "y2" (num (y d))
      , attr "class" "q-staveledger"
      ]
      []

  ledgersFor { d }
    | d > topD = Array.filter (\k -> (k - topD) `mod` 2 == 0) (Array.range (topD + 1) d)
    | d < botD = Array.filter (\k -> (botD - k) `mod` 2 == 0) (Array.range d (botD - 1))
    -- Middle C and its neighbours sit in the gap between the staves, which is
    -- the one place a ledger is needed without being outside the system.
    | otherwise =
        let gapLo = fromMaybe 33 (Array.last bassLines)
            gapHi = fromMaybe 37 (Array.head trebleLines)
        in if d > gapLo && d < gapHi && (d - gapLo) `mod` 2 == 0 then [ d ] else []

  -- | **Seconds are offset, as they are written.** Two noteheads a diatonic
  -- | step apart cannot occupy the same column — they would overlap into one
  -- | blob and a voicing whose whole character is a second would read as a
  -- | single note.
  heads = Array.mapWithIndex headAt (Array.sortWith _.d ds)

  headAt i g =
    let
      prev = Array.index (Array.sortWith _.d ds) (i - 1)
      shift = case prev of
        Just p | g.d - p.d == 1 -> 4.5
        _ -> 0.0
      cx = x0 + shift
    in
      el "g" []
        ( [ el "ellipse"
              [ attr "cx" (num cx), attr "cy" (num (y g.d))
              , attr "rx" (num 3.0), attr "ry" (num 2.2)
              , attr "class" "q-stavehead"
              ]
              []
          ]
            <> (if not g.sharp then [] else
                  [ el "text"
                      [ attr "x" (num (cx - 6.0)), attr "y" (num (y g.d + 2.6))
                      , attr "class" "q-stavesharp"
                      ]
                      [ HH.text "\x266f" ]
                  ])
        )

-- | SVG takes a plain decimal, and `show` on a Number gives one.
num :: Number -> String
num = show
