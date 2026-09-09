-- | **The sweep grid**: parameters down, positions across.
-- |
-- | One row per parameter, one column per position, and a slider in every cell.
-- | Read across a row and it is that parameter's shape over the sweep; read
-- | down a column and it is the complete state of the instrument for one hit.
-- | Both readings matter and the grid gives them for the same price, which is
-- | the argument for sliders over a curve: a curve can only be read the first
-- | way.
-- |
-- | The columns are also the SAME columns as the tiles in `What was caught`.
-- | Position 7's sliders sit above position 7's waveform, so "the interesting
-- | change is all bunched at the top" is something you see rather than measure.
module Workshop.SweepView
  ( body
  ) where

import Prelude

import Data.Array as Array
import Data.Int as Int
import Data.Maybe (maybe)
import Halogen (AttrName(..))
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Workshop.Sweep (Msg(..), Param, Plan, shapeName, shapes)

body :: forall w. Array String -> Plan -> HH.HTML w Msg
body ports plan =
  HH.div [ cls "ws-sweep" ]
    [ settings
    , HH.div [ cls "ws-swgrid", style ("grid-template-columns: 232px repeat("
                <> show plan.positions <> ", minmax(0, 1fr))") ]
        (heading <> Array.concat (Array.mapWithIndex row plan.params))
    , HH.div [ cls "ws-swadd" ]
        [ HH.button
            [ cls "ws-plain"
            , HP.disabled (Array.length plan.params >= 7)
            , HP.title "seven is what the FH-2 has spare, and about what fits"
            , HE.onClick \_ -> AddParam
            ]
            [ HH.text "+ parameter" ]
        , HH.span [ cls "ws-muted" ] [ HH.text note ]
        ]
    , trigger
    ]
  where
  ns = Array.range 0 (plan.positions - 1)

  note =
    "Each column is one hit. The values are 0…1 here and are scaled on the way \
    \out — to a level on a bus, to a controller between its two ends, or to \
    \both at once."

  heading =
    [ HH.div [ cls "ws-swcorner" ] [ HH.text "parameter" ] ]
      <> map (\i -> HH.div [ cls "ws-swnum" ] [ HH.text (show (i + 1)) ]) ns

  -- | The row header carries the whole of where this parameter goes, because
  -- | that is the thing you set once and then stop thinking about, while the
  -- | cells to its right are what you come back to after every listen.
  row i p =
    [ HH.div [ cls "ws-swhead" ]
        [ HH.input
            [ cls "ws-swname", HP.type_ HP.InputText, HP.value p.name
            , HP.title "what this parameter is called on the instrument"
            , HE.onValueInput (SetName i) ]
        , HH.div [ cls "ws-swwhere" ]
            [ tiny "cv" 3 (maybe "" show p.cv) (SetCv i) "es9-daemon bus, 0-15; blank for none"
            , tiny "" 4 (num p.cvLo) (SetCvLo i) "what 0 means on that bus, -1 to 1 (1.0 is FULL output: this path is not halved)"
            , arrow
            , tiny "" 4 (num p.cvHi) (SetCvHi i) "what 1 means on that bus"
            ]
        , HH.div [ cls "ws-swwhere" ]
            [ tiny "cc" 3 (maybe "" show p.cc) (SetCc i) "controller number, 0-127; blank for none"
            , tiny "" 3 (show p.ccLo) (SetCcLo i) "controller value at 0"
            , arrow
            , tiny "" 3 (show p.ccHi) (SetCcHi i) "controller value at 1"
            , tiny "ch" 2 (show p.channel) (SetChannel i) "MIDI channel"
            ]
        , HH.div [ cls "ws-swwhere" ]
            [ HH.select
                [ cls "ws-swseed", HE.onValueChange (SetSeed i)
                , HP.title "fill the row with this shape — a starting point, not a description" ]
                (map (opt p) shapes)
            , HH.button [ cls "ws-swmini", HE.onClick \_ -> ApplySeed i
                        , HP.title "overwrite this row with the shape" ]
                [ HH.text "fill" ]
            , HH.button [ cls "ws-swmini", HE.onClick \_ -> FlipRow i
                        , HP.title "reverse this row — the falling half of a pair" ]
                [ HH.text "flip" ]
            , HH.button [ cls "ws-swmini is-drop", HE.onClick \_ -> DropParam i
                        , HP.title "remove this parameter" ]
                [ HH.text "×" ]
            ]
        ]
    ]
      <> map (cell i p) ns

  opt p sh = HH.option
    [ HP.value (shapeName sh), HP.selected (sh == p.seed) ]
    [ HH.text (shapeName sh) ]

  cell i p j =
    let v = pct p j
    in HH.div [ cls "ws-swcell" ]
         [ HH.input
             [ cls "ws-swslider"
             , HP.type_ HP.InputRange
             , HP.min 0.0, HP.max 100.0, HP.step (HP.Step 1.0)
             , HP.value (show v)
             , HP.title (p.name <> " at " <> show (j + 1) <> ": " <> show v <> "%")
             , HE.onValueInput (SetValue i j)
             ]
         , HH.span [ cls "ws-swval" ] [ HH.text (show v) ]
         ]

  settings =
    HH.div [ cls "ws-swrow" ]
      [ field "positions" (show plan.positions) SetPositions 4
          "how many hits the sweep makes — twelve is the module's layer ceiling"
      , field "settle ms" (show plan.settleMs) SetSettle 5
          "how long after setting the parameters before the trigger; too short and a hit is struck on its way to the value"
      , field "spacing ms" (show plan.spacingMs) SetSpacing 5
          "trigger to trigger; long enough for the sound to finish AND for the divider to see silence between"
      , HH.label [ cls "ws-field is-tight" ]
          [ HH.span_ [ HH.text "midi out" ]
          , HH.select [ HE.onValueChange SetPort ]
              (Array.cons
                 (HH.option [ HP.value "", HP.selected (plan.port == "") ]
                    [ HH.text (if Array.null ports
                              then "none yet \x2014 allow MIDI when Chrome asks"
                              else "none") ])
                 (map
                   (\o -> HH.option [ HP.value o, HP.selected (o == plan.port) ] [ HH.text o ])
                   ports))
          ]
      ]

  trigger =
    HH.div [ cls "ws-swrow is-trig" ]
      [ HH.span [ cls "ws-arm-label" ] [ HH.text "Trigger" ]
      , field "gate bus" (maybe "" show plan.trigger.gate) SetGate 3
          "an es9-daemon bus pulsed to fire the sound; blank for none"
      , field "level" (num plan.trigger.gateLevel) SetGateLevel 4
          "how high that pulse goes, -1 to 1"
      , field "note" (maybe "" show plan.trigger.note) SetNote 4
          "a MIDI note to play instead of, or as well as, the gate"
      , field "ch" (show plan.trigger.channel) SetTrigChannel 3 "MIDI channel for the note"
      , field "vel" (show plan.trigger.velocity) SetVelocity 4 "how hard"
      , field "hold ms" (show plan.trigger.ms) SetHold 4 "how long the gate stays up and the note stays down"
      , HH.span [ cls "ws-muted" ]
          [ HH.text (show plan.positions <> " hits, about "
              <> show (Int.round (Int.toNumber (plan.positions * (plan.settleMs + plan.spacingMs)) / 1000.0))
              <> " s") ]
      ]

  field lbl v act w title =
    HH.label [ cls "ws-field is-tight", HP.title title ]
      [ HH.span_ [ HH.text lbl ]
      , HH.input
          [ HP.type_ HP.InputText, HP.value v
          , style ("width: " <> show (w * 11 + 18) <> "px")
          , HE.onValueInput act ]
      ]

  arrow = HH.span [ cls "ws-swarrow" ] [ HH.text "\x2192" ]

  tiny lbl w v act title =
    HH.label [ cls "ws-swtiny", HP.title title ]
      [ if lbl == "" then HH.text "" else HH.span_ [ HH.text lbl ]
      , HH.input
          [ HP.type_ HP.InputText, HP.value v
          , style ("width: " <> show (w * 9 + 14) <> "px")
          , HE.onValueInput act ]
      ]

pct :: Param -> Int -> Int
pct p j = Int.round (100.0 * maybe 0.0 identity (Array.index p.values j))

num :: Number -> String
num n = show (Int.toNumber (Int.round (n * 1000.0)) / 1000.0)

cls :: forall r i. String -> HH.IProp (class :: String | r) i
cls = HP.class_ <<< HH.ClassName

style :: forall r i. String -> HH.IProp r i
style = HP.attr (AttrName "style")
