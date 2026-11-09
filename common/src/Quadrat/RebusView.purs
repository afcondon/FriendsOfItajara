-- | **Drawing a rebus.**
-- |
-- | Rebus itself is renderer-free on purpose — it yields icon names and
-- | colours and knows nothing about how they are shown, so that Triggerfish
-- | can draw them with a webfont and Quadrat with vendored paths and both get
-- | the same identity. This is Quadrat's half of that bargain.
-- |
-- | The shapes come from `Quadrat.RebusIcons`, which is generated from the
-- | same Font Awesome release Triggerfish loads from its CDN. They have to be
-- | the SAME shapes: the whole value of an identity is that one content wears
-- | one picture wherever it is shown, and a different icon set would leave the
-- | hash agreeing while the pictures disagree — worse than not sharing it,
-- | because you would trust a match that is not one.
-- |
-- | Deliberately takes an array of `{ icon, color }` rather than a `Glyph`, so
-- | it does not depend on Rebus at all. A drawing module that imported the
-- | identity library would make the identity library's shape its problem, and
-- | this one only needs to know what a coloured icon is.
module Quadrat.RebusView
  ( chip
  , Painted
  ) where

import Prelude

import Data.Array as Array
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number as Number
import Data.String as String
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Quadrat.RebusIcons (iconOf)
import Quadrat.Wave (attr, el)

-- | One icon of a rebus, as Rebus hands it over.
type Painted = { icon :: String, color :: String }

-- | **A rebus as a row of icons.**
-- |
-- | `height` is the only size given, because an icon's width is its own:
-- | Font Awesome's shapes are not all the same aspect — a truck is wider than
-- | a key, five different widths across this deck — and forcing them to a
-- | common width would squash the wide ones. So each is drawn at the shared
-- | height and takes whatever width that implies.
-- |
-- | `mono` drops the colours. Not a style preference: a *container's* alias
-- | is rendered monochrome so it cannot be mistaken for a *content* identity,
-- | which is coloured. Two glyph rows of the same width that meant different
-- | kinds of thing would be exactly the confusion identity exists to prevent.
chip
  :: forall w i
   . { height :: Number, mono :: Boolean, title :: String }
  -> Array Painted
  -> HH.HTML w i
chip o painted =
  HH.span
    [ HP.class_ (HH.ClassName ("q-rebus" <> if o.mono then " is-mono" else ""))
    , HP.title o.title
    ]
    (Array.mapMaybe one painted)
  where
  one p = case iconOf p.icon of
    -- | **A name with no shape is drawn as nothing at all.**
    -- |
    -- | Not a placeholder box: the deck and the vendored paths are generated
    -- | from each other, so a miss means they have drifted apart — and a
    -- | placeholder would make two identities that differ only in the missing
    -- | icon look identical, which is the one failure this must not have.
    Nothing -> Nothing
    Just ic ->
      Just
        (el "svg"
          [ attr "viewBox" ic.box
          , attr "height" (show (Int.round o.height))
          , attr "width" (show (Int.round (o.height * aspect ic.box)))
          , attr "class" "q-rebusicon"
          , attr "role" "img"
          , attr "aria-label" p.icon
          ]
          [ el "path"
              [ attr "d" ic.path
              , attr "fill" (if o.mono then "currentColor" else p.color)
              ]
              []
          ])

-- | The width of a viewBox against its height, so an icon keeps its own
-- | proportions. `"0 0 640 512"` is 1.25 wide. A box that cannot be read
-- | falls back to square, which is wrong by at most a quarter and never
-- | collapses the icon to nothing.
aspect :: String -> Number
aspect box =
  case Array.drop 2 (String.split (String.Pattern " ") box) of
    [ w, h ] -> fromMaybe 1.0 do
      wn <- Number.fromString w
      hn <- Number.fromString h
      if hn <= 0.0 then Nothing else Just (wn / hn)
    _ -> 1.0
