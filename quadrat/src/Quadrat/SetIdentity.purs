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
-- | ## Durable, not content
-- |
-- | Rebus serves two purposes and they are opposites. Vetula wants a CONTENT
-- | identity, which must change when the content changes — that is how an edit
-- | announces itself. A library wants a DURABLE one, pinned to something that
-- | cannot change, so that the picture you learned for a set is still that
-- | set's picture after it has been renamed, re-cut, placed on a card, or had
-- | its samples improved.
-- |
-- | So the key is `made`, the moment the set was written, which is in
-- | `set.json` and never altered afterwards. Not the name, which is the thing
-- | being escaped from; not the samples, which are meant to be improvable.
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
  ) where

import Prelude

import Rebus (Glyph, rebusOf)
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
