-- | **Every recording page, one shape.** Andrew's drawing, 2026-11-08: under
-- | the head, a main column whose head is the page's statement, and a side
-- | column for the secondary controls.
-- |
-- | **The side column is a drawer** since 2026-11-09 (AC: "further clean up
-- | the initial views and allow us to focus on the primary function of each
-- | view"), the same `Halogen.Widgets.Drawer` the Triggerfish pages use: on
-- | the right edge, closed when the page loads, opened by the arrow on its
-- | rail and resizable by its inner edge. What it holds is what you set and
-- | then stop looking at: the Bench's parameter sweeps, Score's published
-- | shelf, Jam's note on the length, the looper's controllers and log.
-- |
-- | The page still scrolls as a document; the drawer's dock is pinned to the
-- | viewport as it does (`quadrat.css`), as the side column was.
module Quadrat.Layout
  ( Side
  , layout
  ) where

import Prelude

import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Widgets.Drawer as Drawer

-- | The drawer's input and its slotted rail, both from the page's owner: the
-- | owner holds `open` and `width`, and only it can slot a component.
type Side w i = { input :: Drawer.Input, handle :: HH.HTML w i }

layout
  :: forall w i
   . { statement :: HH.HTML w i
     , main :: Array (HH.HTML w i)
     , side :: Array (HH.HTML w i)
     , drawer :: Side w i
     }
  -> HH.HTML w i
layout r =
  HH.div [ cls "q-layout" ]
    [ Drawer.frame r.drawer.input
        { handle: r.drawer.handle
        , body: [ HH.div [ cls "q-layout-side" ] r.side ]
        , main: [ HH.div [ cls "q-layout-main" ] ([ r.statement ] <> r.main) ]
        }
    ]

cls :: forall r i. String -> HP.IProp (class :: String | r) i
cls = HP.class_ <<< HH.ClassName
