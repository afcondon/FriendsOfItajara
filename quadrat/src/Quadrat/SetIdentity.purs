-- | **What a sample set is called when its name is a timestamp.**
-- |
-- | The library's own comments have been complaining about this for a while:
-- | *"every name is a timestamp and two runs of the same morning differ by
-- | four characters in the middle"* — which is why deleting a set makes you
-- | read the names and hear the audio before it will act. A rebus answers it
-- | directly: `chord-hits-0912-164548` and `chord-hits-0912-164852` become two
-- | visibly different pairs of icons, and each pair has words, so it can be
-- | said aloud and typed.
-- |
-- | ## Two identities, and a set gets exactly one of them
-- |
-- | Rebus serves two purposes and they are opposites. A CONTENT identity must
-- | change when the content changes — that is how an edit announces itself,
-- | and it is how two programs holding the same content agree without talking.
-- | A DURABLE identity must never change, so that the picture you learned for
-- | a thing is still its picture after it has been renamed, re-cut or
-- | improved.
-- |
-- | Which a set wants depends on whether it HAS content anyone else can hold.
-- |
-- | **A chord set does.** Its chords are the same object Vetula trades in, so
-- | it takes a content identity minted from them — `Rebus.chordsOf`, the
-- | shared normal form, so the order the notes happened to arrive in cannot
-- | change the picture. A progression caught here and a progression saved
-- | there then wear the same icons, and that agreement is the whole point:
-- | you can see that the thing on the card and the thing in the sequencer are
-- | the same music without either app knowing the other exists.
-- |
-- | **Nothing else does.** A drum grid, a sweep, an ambient take — nobody else
-- | holds their content, so a content identity would agree with nothing. They
-- | take the durable one, keyed on `made`: the moment the set was written,
-- | which is in `set.json` and never altered afterwards. Not the name, which
-- | is the thing being escaped from; not the samples, which are meant to be
-- | improvable.
-- |
-- | The two are told apart by COLOUR, which is Rebus's own convention: a
-- | content identity is coloured, a container's is monochrome. So a grey pair
-- | of icons says "this set, whatever becomes of it" and a coloured pair says
-- | "these chords, wherever they are" — and a match between two coloured
-- | glyphs means something, which is the only reason to draw them at all.
-- |
-- | A set cut before sets were described has no `made` and falls back to its
-- | name — imperfect, and honest about which sets it is imperfect for: those
-- | are exactly the sets that record nothing else about themselves.
-- |
-- | ## Width two
-- |
-- | Four thousand distinct pairs against a library of dozens. Width is chosen
-- | from how many things must be told apart, and three would be right at a few
-- | hundred: measured over Marginalia's 289 projects, pairs collide twenty
-- | times and triples not at all.
module Quadrat.SetIdentity
  ( SetKey(..)
  , setKey
  , setGlyph
  , chordGlyph
  , Mark
  , markOf
  ) where

import Prelude

import Data.Array as Array
import Rebus (Glyph, chordsOf, rebusOf)
import Rebus.Canonical (class Canonical)

-- | The two fields of a set that decide its identity, and no others — so that
-- | what the identity depends on is legible here rather than at the call site.
newtype SetKey = SetKey { name :: String, made :: String }

derive instance Eq SetKey

-- | **The canonical form is the contract**, and it is a string on purpose:
-- | two programs agree about an identity only if the thing they hash is
-- | something a person can print and compare.
-- |
-- | Prefixed, so that a set and some other kind of object that happened to be
-- | made at the same instant are different identities. The prefix is part of
-- | the contract and changing it renumbers every set.
instance Canonical SetKey where
  canonical (SetKey s)
    | s.made /= "" = "quadrat/set@" <> s.made
    | otherwise = "quadrat/set:" <> s.name

setKey :: forall r. { name :: String, made :: String | r } -> SetKey
setKey r = SetKey { name: r.name, made: r.made }

setGlyph :: forall r. { name :: String, made :: String | r } -> Glyph
setGlyph = rebusOf <<< setKey

-- | **The rebus of a progression**, minted the way anybody else would mint it.
-- |
-- | `chordsOf` rather than the bare `Chords` constructor: a chord is a set of
-- | sounding pitches and the order they are listed in is an accident of how
-- | they were read — finger order from a MIDI capture here, bass-first from a
-- | voicing engine in Vetula. Sorting each chord is what makes the two agree;
-- | the progression's own order is left alone, because played backwards it is
-- | a different piece.
-- |
-- | Empty chords are dropped rather than hashed as empty, so a set with a
-- | silent region identifies as the chords it actually has.
chordGlyph :: Array (Array Int) -> Glyph
chordGlyph = rebusOf <<< chordsOf <<< Array.filter (not <<< Array.null)

-- | What a set's picture MEANS, so that the thing drawing it cannot get the
-- | colour wrong. A `Mark` is a glyph that already knows whether it is content
-- | or container; nothing downstream has to remember the rule.
type Mark = { glyph :: Glyph, mono :: Boolean, says :: String }

-- | **The one place a set's picture is chosen.** Chords if it has them,
-- | otherwise the durable key — and the `mono` flag travels with the glyph so
-- | a caller cannot draw a container in colour and quietly claim a content
-- | identity for it.
markOf
  :: forall r
   . { name :: String, made :: String, voicings :: Array (Array (Array Int)) | r }
  -> Mark
markOf r =
  case Array.filter (not <<< Array.null) (join r.voicings) of
    [] -> { glyph: setGlyph r, mono: true, says: "this set" }
    chords -> { glyph: chordGlyph chords, mono: false, says: "these chords" }
