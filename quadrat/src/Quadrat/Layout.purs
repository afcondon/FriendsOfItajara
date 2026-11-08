-- | **Every recording page, one shape.** Andrew's drawing, 2026-11-08: under
-- | the title, a main column two thirds wide whose head is the page's
-- | statement with the input meter beside it, and a side column a third wide
-- | for the secondary controls, which scrolls on its own.
-- |
-- | The statement and meter are in the same place on Bench, Jam and Loop, so
-- | the question "what is this page about to do, and is anything coming in"
-- | is answered where the eye already is. The main column holds the meat of
-- | the page; the side column what you set and then stop looking at (the
-- | Bench's parameter sweeps, the looper's Twister card and log).
module Quadrat.Layout (layout) where

import Prelude

import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

layout
  :: forall w i
   . { statement :: HH.HTML w i
     , main :: Array (HH.HTML w i)
     , side :: Array (HH.HTML w i)
     }
  -> HH.HTML w i
layout r =
  HH.div [ cls "q-layout" ]
    [ HH.div [ cls "q-layout-main" ] ([ r.statement ] <> r.main)
    , HH.aside [ cls "q-layout-side" ] r.side
    ]

cls :: forall r i. String -> HP.IProp (class :: String | r) i
cls = HP.class_ <<< HH.ClassName
