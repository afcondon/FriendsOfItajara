-- | **Turning a note into a voltage, using what the module actually does.**
-- |
-- | Every other parameter in a transect is interpolated: `lerp cvLo cvHi t`
-- | gives a level, the module does something with it, and whether that
-- | something is linear is nobody's business. Pitch is the exception, because
-- | pitch has a RIGHT ANSWER — a C is a C — and the module's opinion of how
-- | volts become frequency is a measurable fact rather than a convention.
-- |
-- | Measured on the BIA, 2026-09-10: its pitch CV tracks 1.007 V/oct up to
-- | about 2.25 V and then compresses to 1.16, reaching **471 cents flat** of
-- | ideal at 5 V. A naive lerp at 0.1 V per semitone is not slightly out at the
-- | top of that range, it is four and a half semitones out. Three other modules
-- | on the identical signal path are flat to within 2%, so this is the module,
-- | not the interface — which is exactly why the answer has to come from a
-- | measurement of THIS path rather than from arithmetic.
-- |
-- | The table comes from `deepstar tune` (published in Amphora, served by
-- | `deepstar serve` on :3027). It stores ABSOLUTE `(volts, hz)` pairs, so an
-- | offset is absorbed exactly and nothing here needs to know the base note.
-- | This module is the same inversion DeepStar's `/realise` performs, done
-- | locally so a plan can hold its table and stay pure — see `Quadrat.Sweep`.
module Quadrat.Pitch
  ( Point
  , Table
  , noteHz
  , hzNote
  , voltsForHz
  , voltsForNote
  , levelForNote
  , noteAt
  , noteName
  , Realised
  ) where

import Prelude

import Data.Array as Array
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number (log, pow)
import Data.Tuple (Tuple(..))

-- | One measured point: the frequency the module produced when this voltage
-- | was applied. Absolute, not relative to any base.
type Point = { volts :: Number, hz :: Number }

-- | A calibration table as `deepstar serve` hands it over. `label` is the
-- | provenance — it names a SIGNAL PATH, not a module (the ES-9's own jacks
-- | differ by up to 14.6 cents), so it belongs in `set.json` beside the samples.
type Table = { label :: String, points :: Array Point }

-- | What came back, and whether the table could actually reach it. `clamped`
-- | is not a detail: asking a table for a note outside its measured span
-- | silently returns the nearest end, and a whole transect can come out on one
-- | pitch that way.
type Realised = { volts :: Number, hz :: Number, clamped :: Boolean }

-- | MIDI note number to frequency. A4 = 69 = 440 Hz.
noteHz :: Int -> Number
noteHz n = 440.0 * pow 2.0 ((Int.toNumber n - 69.0) / 12.0)

-- | Frequency to the nearest MIDI note number. The inverse of `noteHz`, used
-- | to report what a measured sample actually came out as.
hzNote :: Number -> Int
hzNote hz = Int.round (69.0 + 12.0 * log2 (hz / 440.0))

-- | **The inversion.** What voltage does this module need to produce this
-- | frequency?
-- |
-- | Interpolated in (log-frequency, volts) space rather than (frequency,
-- | volts): a V/oct input is linear in the LOG of frequency by construction, so
-- | that is the space where a straight line between two measured points is a
-- | good guess. Interpolating in linear frequency would put every value between
-- | two points sharp, worst in the middle — an error that no amount of extra
-- | measurement removes, because it is in the reader rather than the data.
voltsForHz :: Table -> Number -> Realised
voltsForHz t hz = case Array.head pts, Array.last pts of
  Just lo, Just hi
    | hz <= lo.hz -> { volts: lo.volts, hz: lo.hz, clamped: true }
    | hz >= hi.hz -> { volts: hi.volts, hz: hi.hz, clamped: true }
    | otherwise -> fromMaybe
        { volts: lo.volts, hz: lo.hz, clamped: true }
        (Array.findMap bracket (Array.zip pts (Array.drop 1 pts)))
  _, _ -> { volts: 0.0, hz: 0.0, clamped: true }
  where
  -- Sorted by voltage, because a table is a sweep and a sweep is ordered by
  -- what it swept. Not assumed: a re-published or hand-edited table has no
  -- obligation to arrive in order, and an unsorted table read as sorted finds
  -- no bracket and clamps everything to one end.
  pts = Array.sortWith _.volts t.points

  bracket (Tuple a b)
    | hz >= a.hz && hz <= b.hz && b.hz > a.hz =
        let f = (log2 hz - log2 a.hz) / (log2 b.hz - log2 a.hz)
        in Just { volts: a.volts + f * (b.volts - a.volts), hz, clamped: false }
    | otherwise = Nothing

-- | The voltage for a MIDI note.
voltsForNote :: Table -> Int -> Realised
voltsForNote t = voltsForHz t <<< noteHz

-- | The voltage as **es9-daemon's own -1…1 level**, which is what a `Step`
-- | carries and `/cv` takes.
-- |
-- | Level 1.0 is 10 V, measured — the one constant here that is a property of
-- | the interface rather than of the module, and the reason it lives beside the
-- | table rather than in it.
levelForNote :: Table -> Int -> Realised
levelForNote t n = let r = voltsForNote t n in r { volts = r.volts / 10.0 }

-- | **Which note a curve position lands on**, over a range given in semitones.
-- |
-- | Rounded to a whole semitone, and that is the entire point. A curve spans
-- | lo→hi *inclusive*, so N positions give N−1 intervals: eleven steps from C3
-- | to C4 are 12/11 of a semitone apart, and every one of them except the ends
-- | is out of tune by an amount nobody chose. Rounding lands every step on a
-- | real note whatever N is — ask for fewer positions than semitones and you
-- | get a subset of the notes, still in tune, rather than a chromatic scale
-- | with a limp.
noteAt :: Int -> Int -> Number -> Int
noteAt lo hi t = Int.round (Int.toNumber lo + (Int.toNumber hi - Int.toNumber lo) * t)

-- | A note number as a name, for the readout. Sharps rather than flats, with no
-- | attempt at a key — this is a label on a measurement, not a score.
noteName :: Int -> String
noteName n =
  fromMaybe "?" (Array.index names (mod n 12)) <> show (n / 12 - 1)
  where
  names = [ "C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B" ]

log2 :: Number -> Number
log2 x = log x / log 2.0
