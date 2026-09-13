-- | **A chord set, as text somebody else can read.**
-- |
-- | Quadrat records what was struck inside each region, absolute and in
-- | register, and draws it on a staff. That is the reading for a human. This is
-- | the reading for a machine — specifically for **Vetula**, Triggerfish's
-- | chord workbench, where a progression caught here can be revoiced,
-- | transposed, sequenced and played by the rig.
-- |
-- | ## Why a string, and why this string
-- |
-- | Vetula's own persistence rule is *save the rendering, not bespoke
-- | structure*: its library entries and presets are stored as TidalCycles
-- | source, because that is the form that already round-trips through
-- | `Vetula.Tidal.parseProgression` and through the clipboard. So the seam
-- | between the two apps is a **format neither of them owns** rather than an
-- | API one of them has to publish, and Quadrat takes on no dependency —
-- | not on Vetula, not on Reef, not on Harmonia.
-- |
-- | The consequence worth stating: this module must emit what
-- | `Vetula.Tidal.progressionSource` emits, note for note, or the shared format
-- | is not shared. The note-name table below is therefore copied from it
-- | VERBATIM, mixed spellings and all (`cs` but `ef`, `fs` but `af`) — that is
-- | Tidal's convention, not a choice being made here, and a tidier table would
-- | mean the same chord exported from the two apps no longer compared equal.
-- |
-- | ## What is lost, and what that means
-- |
-- | Pitches, in order, and nothing else. A sample's duration, its measurements,
-- | which point of a transect it came from, the audio itself — none of that
-- | survives, because none of it is a chord. That is honest for this direction:
-- | Vetula wants the harmony, and the harmony is exactly what is being handed
-- | over. It is the opposite of the card write, which keeps the audio and
-- | discards the harmony.
module Quadrat.Tidal
  ( noteName
  , chordBracket
  , progression
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe (fromMaybe, maybe)
import Data.String.Common (joinWith)

-- | A MIDI note as a Tidal-safe name with octave: 60 → `"c5"`, 61 → `"cs5"`.
-- | Tidal's octave is `midi / 12`, so middle C is c5 and not c4 — a surprise
-- | worth leaving in a comment rather than rediscovering.
noteName :: Int -> String
noteName midi = name <> show (midi / 12)
  where
  names = [ "c", "cs", "d", "ef", "e", "f", "fs", "g", "af", "a", "bf", "b" ]
  name = fromMaybe "?" (Array.index names (mod midi 12))

-- | One chord as Tidal mini-notation for simultaneous notes: ascending,
-- | comma-separated, in brackets. Sorted because a bracket is a set and the
-- | order notes happened to arrive in is a fact about MIDI, not about the
-- | chord.
chordBracket :: Array Int -> String
chordBracket ns = "[" <> joinWith "," (map noteName (Array.sort ns)) <> "]"

-- | **The whole set as one Tidal block.**
-- |
-- | Chord per cycle in the live line (`<…>`), which is the natural reading of a
-- | progression, with the all-in-one-cycle form offered commented out. The
-- | header comments are free text — the parser drops every line starting `--`
-- | — so they are used to say where this came from, which is the one thing a
-- | note list cannot say about itself once it is in another app.
-- |
-- | The second comment is the **feet**, in order. Quadrat cannot name a chord:
-- | it has pitches and no harmonic reading, and a guessed `Cmaj7` in a comment
-- | would be believed. The lowest sounding note of each voicing is not a guess,
-- | and read across the row it is the bass line.
progression :: { name :: String, alias :: String } -> Array (Array Int) -> String
progression who chords
  | Array.null chords = ""
  | otherwise =
      joinWith "\n"
        [ "-- quadrat \xb7 " <> who.name
            <> (if who.alias == "" then "" else " \xb7 " <> who.alias)
            <> " \xb7 " <> show (Array.length chords) <> " chords"
        , "-- feet: " <> joinWith "   " (Array.mapWithIndex foot chords)
        , "note \"<" <> joinWith " " brackets <> ">\""
        , "-- all in one cycle:"
        , "-- note \"" <> joinWith " " brackets <> "\""
        ]
  where
  brackets = map chordBracket chords
  foot i ns = show (i + 1) <> " " <> maybe "?" noteName (Array.head (Array.sort ns))
