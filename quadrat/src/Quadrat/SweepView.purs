-- | **The sweep, as a bench rather than a dialog.**
-- |
-- | Andrew's sketch, 2026-09-10: curves down the left, the parameters they move
-- | beside them, the set they produce to the right. One curve per parameter you
-- | want to control, so adding a curve *is* adding a parameter.
-- |
-- | The thumbnail is a picture of the constructor — a line while it is still a
-- | named shape, bars once it has been edited point by point. That is what lets
-- | one field hold both representations without anyone having to remember which
-- | is live.
-- |
-- | Editing opens **in place**, full width, rather than in a modal. Eight
-- | parameters at twelve or sixteen positions is a mixing desk, and a desk in a
-- | dialog is a desk you cannot work.
module Quadrat.SweepView
  ( Handlers
  , body
  , settings
  ) where

import Prelude

import Data.Array as Array
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Halogen (AttrName(..), ElemName(..), Namespace(..))
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Quadrat.Curve (isDrawn)
import Quadrat.Curve as Curve
import Quadrat.Encoding as Encoding
import Quadrat.Sweep (Msg(..), Plan, valuesFor)

type Handlers act =
  { ports :: Array String
  -- | Which parameter is open for point-by-point editing, if any.
  , open :: Maybe Int
  , plan :: Plan
  , msg :: Msg -> act
  , openParam :: Maybe Int -> act
  }

-- | **What the take is** — the destination, the shape of the set, and the two
-- | intervals that decide how long it runs. The left page's, because they
-- | describe the take rather than the instrument: change one and you are
-- | asking for a different set, not moving a different knob.
settings :: forall w act. Handlers act -> HH.HTML w act
settings h =
  HH.div [ cls "q-sweep" ]
    [ encodingRow
    , objections
    ]
  where
  p = h.plan
  axs = Encoding.axes p.encoding

  encodingRow =
    HH.div [ cls "q-swrow" ]
      ( [ HH.label [ cls "q-stack" ]
            [ HH.span_ [ HH.text "encoding" ]
            , HH.select [ HE.onValueChange (h.msg <<< PickEncoding) ]
                (map
                  (\e -> HH.option
                    [ HP.value (Encoding.name e), HP.selected (e == p.encoding) ]
                    [ HH.text (Encoding.label e) ])
                  Encoding.all)
            ]
        ]
          <> Array.mapWithIndex extentField axs
          <> [ field "settle ms" (show p.settleMs) SetSettle 4
                 "after setting the parameters, before the trigger — too short and a \
                 \cell is a blend of itself and its neighbour"
             , field "spacing ms" (show p.spacingMs) SetSpacing 5
                 "trigger to trigger; long enough for the sound to finish AND for a \
                 \gap to be visible after it"
             ]
      )

  extentField i ax =
    HH.label [ cls "q-stack", HP.title (ax.name <> " — picked by " <> ax.picked) ]
      [ HH.span_ [ HH.text (ax.name <> "s") ]
      , if ax.free
          then HH.input
                 [ HP.type_ HP.InputNumber
                 , HP.value (show (fromMaybe 1 (Array.index p.extent i)))
                 , HP.min (Int.toNumber (fromMaybe 1 (Array.head ax.sizes)))
                 , HP.max (Int.toNumber (fromMaybe 1 (Array.last ax.sizes)))
                 , HE.onValueInput (h.msg <<< SetExtent i)
                 ]
          else HH.select [ HE.onValueChange (h.msg <<< SetExtent i) ]
                 (map (\n -> HH.option
                         [ HP.value (show n)
                         , HP.selected (Array.index p.extent i == Just n) ]
                         [ HH.text (show n) ])
                     ax.sizes)
      ]

  objections =
    case Encoding.objections p.encoding p.extent of
      [] ->
        HH.div [ cls "q-swnote" ]
          [ HH.text ("about "
              <> show (Int.round (Int.toNumber
                   (Encoding.total p.extent * (p.settleMs + p.spacingMs)) / 1000.0))
              <> " s to record") ]
      objs ->
        HH.div [ cls "q-swnote is-bad" ]
          (map (\o -> HH.div_ [ HH.text o ]) objs)

  field lbl v act w title =
    HH.label [ cls "q-stack", HP.title title ]
      [ HH.span_ [ HH.text lbl ]
      , HH.input
          [ HP.type_ HP.InputText, HP.value v
          , style ("width: " <> show (w * 11 + 18) <> "px")
          , HE.onValueInput (h.msg <<< act) ]
      ]

body :: forall w act. Handlers act -> HH.HTML w act
body h =
  HH.div [ cls "q-sweep" ]
    -- **The trigger first.** It is rig setup — which jack fires the sound —
    -- set once when the cable went in and then left alone, where the curves
    -- below it are the thing being worked on. It was at the bottom because it
    -- was added last, which is not a reason.
    [ trigger
    , HH.div [ cls "q-curves" ] (Array.concat (Array.mapWithIndex row p.params))
    , HH.div [ cls "q-swadd" ]
        [ HH.button
            [ cls "q-plain"
            , HP.disabled (Array.length p.params >= 8)
            , HP.title "one curve for every parameter you want to move"
            , HE.onClick \_ -> h.msg AddParam
            ]
            [ HH.text "+ curve" ]
        , HH.span [ cls "q-muted" ]
            [ HH.text "A parameter with no curve is held. Click a curve to step \
                      \through the shapes; open it to place every value by hand." ]
        ]
    ]
  where
  p = h.plan
  axs = Encoding.axes p.encoding

  -- | **The encoding first, because everything else follows from it.**

  -- | **A chooser where the destination enumerates, a box where it does not.**
  -- |
  -- | SLICER's eight divisions are the whole of what a slice axis can be, and
  -- | a dropdown says so at a glance. SuperDirt's `n` has no such list, and a
  -- | five-hundred-entry dropdown would be a set of choices pretending to be a
  -- | constraint. Same `Axis`, one flag, two controls.

  -- | **What the destination will refuse, said before anything is recorded.**
  -- |
  -- | `msm kit build` makes the same objections at write time, which is after
  -- | the hits exist. This is the only place saying it can save anything.

  -- | A curve, and the parameter it moves. The sketch's left two columns, kept
  -- | on one row so the arrow between them needs no drawing.
  row i q =
    [ HH.div [ cls "q-curvecard" ]
        [ HH.button
            [ cls "q-curveface"
            , HP.title (if isDrawn q.curve
                          then "drawn by hand — open it to change the values"
                          else "click for the next shape")
            , HE.onClick \_ -> h.msg (NextShape i)
            ]
            [ thumb (valuesFor p q) (isDrawn q.curve) ]
        , HH.div [ cls "q-curvefoot" ]
            [ HH.span [ cls "q-curvelabel" ] [ HH.text (Curve.label q.curve) ]
            , mini "↔" "reverse it — the falling half of a pair" (h.msg (FlipCurve i))
            , mini (if h.open == Just i then "close" else "open")
                "place every value by hand"
                (h.openParam (if h.open == Just i then Nothing else Just i))
            , if isDrawn q.curve
                then mini "curve" "back to a named shape — this discards the values"
                       (h.msg (ToCurve i))
                else HH.text ""
            ]
        ]
    , HH.div [ cls "q-curveparam" ]
        -- **Remove belongs where the parameter is**, not inside the drawer.
        -- Buried in `open` it could only be reached by expanding the thing you
        -- wanted gone, and a row of eight curves had no way to lose one
        -- without opening it first.
        [ HH.div [ cls "q-swtop" ]
            [ HH.input
                [ cls "q-swname", HP.type_ HP.InputText, HP.value q.name
                , HP.title "what this parameter is called on the instrument"
                , HE.onValueInput (h.msg <<< SetName i) ]
            , HH.button
                [ cls "q-swmini is-drop", HP.title "remove this parameter"
                , HE.onClick \_ -> h.msg (DropParam i) ]
                [ HH.text "×" ]
            ]
        -- | **Where it goes, said in a phrase rather than in nine boxes.**
        -- |
        -- | The routing was always on screen and almost never touched: a bus
        -- | and a range, set once when the cable went in. Nine small fields
        -- | for that crowded out the two things you DO read — the shape and
        -- | the name — and made every extra parameter cost a line of numbers.
        -- | So the card states it, and `open` is where you change it, beside
        -- | the sliders that are the other reason to open a parameter.
        , HH.div [ cls "q-swsays" ]
            [ HH.text (routing q)
            , HH.span [ cls "q-swaxis" ]
                [ HH.text (" · " <> maybe "" _.name (Array.index axs q.axis)) ]
            ]
        ]
    ]
      <> (if h.open == Just i then [ sliders i q ] else [])

  -- | Which axis it moves along. Only a question when there is more than one —
  -- | and when there is, it is the question that makes the grid legible.
  axisPick i q
    | Array.length axs < 2 = HH.text ""
    | otherwise =
        HH.label [ cls "q-swtiny", HP.title "which axis this parameter moves along" ]
          [ HH.span_ [ HH.text "along" ]
          , HH.select
              [ HE.onValueChange (\v -> h.msg (SetAxis i (maybe 0 identity (Int.fromString v)))) ]
              (Array.mapWithIndex
                (\a ax -> HH.option
                   [ HP.value (show a), HP.selected (a == q.axis) ] [ HH.text ax.name ])
                axs)
          ]

  -- | **The desk.** Full width, one slider per position on this parameter's
  -- | axis, and touching any of them turns the curve into a drawing.
  sliders i q =
    let vs = valuesFor p q
    in
      HH.div [ cls "q-desk" ]
        [ HH.div [ cls "q-deskhead" ]
            [ HH.text (q.name <> " — "
                <> show (Array.length vs) <> " values along "
                <> maybe "the axis" _.name (Array.index axs q.axis)) ]
        , where_ i q
        , HH.div [ cls "q-deskgrid" ]
            (Array.mapWithIndex (cell i) vs)
        ]

  -- | **A parameter's destination as one line.** Blank on both is worth
  -- | saying out loud: a curve routed nowhere moves nothing, and it looks
  -- | exactly like one that does until the take comes back flat.
  routing q =
    case q.cv, q.cc of
      Nothing, Nothing -> "not routed — this parameter moves nothing"
      Just b, Nothing -> "cv " <> show b <> "  " <> num q.cvLo <> " → " <> num q.cvHi
      Nothing, Just c ->
        "cc " <> show c <> " ch " <> show q.channel
          <> "  " <> show q.ccLo <> " → " <> show q.ccHi
      Just b, Just c ->
        "cv " <> show b <> "  " <> num q.cvLo <> " → " <> num q.cvHi
          <> "   ·   cc " <> show c <> " ch " <> show q.channel
          <> "  " <> show q.ccLo <> " → " <> show q.ccHi

  -- | **The routing, where you opened the parameter to change it.**
  -- |
  -- | Beside the sliders because they are the two reasons to open one, and
  -- | because for most of a session the defaults are right — which is an
  -- | argument for having them out of the way, not for having them absent.
  where_ i q =
    HH.div [ cls "q-swwhere" ]
      [ HH.span [ cls "q-swgroup" ]
          [ tiny "cv bus" 3 (maybe "" show q.cv) (SetCv i)
              "es9-daemon bus — 8 is ES-9 panel jack 1, 15 is jack 8; blank for none"
          , tiny "at 0" 4 (num q.cvLo) (SetCvLo i)
              "what 0 means on that bus, -1 to 1 (1.0 is FULL output: this path is not halved)"
          , tiny "at 1" 4 (num q.cvHi) (SetCvHi i) "what 1 means on that bus"
          ]
      , HH.span [ cls "q-swgroup" ]
          [ tiny "cc" 3 (maybe "" show q.cc) (SetCc i) "controller number; blank for none"
          , tiny "at 0" 3 (show q.ccLo) (SetCcLo i) "controller value at 0"
          , tiny "at 1" 3 (show q.ccHi) (SetCcHi i) "controller value at 1"
          , tiny "ch" 2 (show q.channel) (SetChannel i) "MIDI channel"
          ]
      , axisPick i q
      ]

  cell i j v =
    let pc = Int.round (v * 100.0)
    in
      HH.div [ cls "q-swcell" ]
        [ HH.input
            [ cls "q-swslider"
            , HP.type_ HP.InputRange
            , HP.min 0.0, HP.max 100.0, HP.step (HP.Step 1.0)
            , HP.value (show pc)
            , HP.title (show (j + 1) <> ": " <> show pc <> "%")
            , HE.onValueInput (h.msg <<< SetValue i j)
            ]
        , HH.span [ cls "q-swval" ] [ HH.text (show pc) ]
        , HH.span [ cls "q-swidx" ] [ HH.text (show (j + 1)) ]
        ]

  trigger =
    HH.div [ cls "q-swrow is-trig" ]
      [ HH.span [ cls "q-arm-label" ] [ HH.text "Trigger" ]
      , field "gate bus" (maybe "" show p.trigger.gate) SetGate 3
          "an es9-daemon bus pulsed to fire the sound; 15 is ES-9 panel jack 8"
      , field "level" (num p.trigger.gateLevel) SetGateLevel 4 "how high that pulse goes, -1 to 1"
      , field "note" (maybe "" show p.trigger.note) SetNote 4
          "a MIDI note to play instead of, or as well as, the gate"
      , field "ch" (show p.trigger.channel) SetTrigChannel 3 "MIDI channel for the note"
      , field "vel" (show p.trigger.velocity) SetVelocity 4 "how hard"
      , field "hold ms" (show p.trigger.ms) SetHold 4 "how long it is held"
      -- Where the note goes, beside the note. It was over with the encoding,
      -- which is about the shape of the set and has nothing to say about MIDI.
      , HH.label [ cls "q-stack" ]
          [ HH.span_ [ HH.text "midi out" ]
          , HH.select [ HE.onValueChange (h.msg <<< SetPort) ]
              (Array.cons
                (HH.option [ HP.value "", HP.selected (p.port == "") ]
                  [ HH.text (if Array.null h.ports then "none yet" else "none") ])
                (map (\o -> HH.option
                        [ HP.value o, HP.selected (o == p.port) ] [ HH.text o ])
                   h.ports))
          ]
      ]

  -- | **Label above value, not beside it.**
  -- |
  -- | Beside, a row of seven fields is fourteen items wide and wraps onto a
  -- | second line at the first excuse — which it did. Stacked, each field is
  -- | one narrow column, the row holds twice as many, and the labels line up
  -- | across it so the row reads as a table of one row rather than as a
  -- | sentence of alternating words.
  field lbl v act w title =
    HH.label [ cls "q-stack", HP.title title ]
      [ HH.span_ [ HH.text lbl ]
      , HH.input
          [ HP.type_ HP.InputText, HP.value v
          , style ("width: " <> show (w * 11 + 18) <> "px")
          , HE.onValueInput (h.msg <<< act) ]
      ]

  tiny lbl w v act title =
    HH.label [ cls "q-stack is-tiny", HP.title title ]
      [ HH.span_ [ HH.text lbl ]
      , HH.input
          [ HP.type_ HP.InputText, HP.value v
          , style ("width: " <> show (w * 10 + 16) <> "px")
          , HE.onValueInput (h.msg <<< act) ]
      ]

  mini txt title act =
    HH.button [ cls "q-swmini", HP.title title, HE.onClick \_ -> act ] [ HH.text txt ]

-- | **A picture of the constructor.**
-- |
-- | A line while it is a named shape; bars once the values have been placed by
-- | hand. Nothing else on the page says which of the two it is, and nothing
-- | else needs to.
thumb :: forall w i. Array Number -> Boolean -> HH.HTML w i
thumb vs drawn =
  el "svg"
    [ attr "viewBox" "0 0 1 1", attr "preserveAspectRatio" "none"
    , attr "class" ("q-thumb" <> if drawn then " is-drawn" else "")
    ]
    (if drawn then map bar (Array.mapWithIndex (\i v -> { i, v }) vs)
     else [ el "polyline" [ attr "points" points, attr "class" "q-thumbline" ] [] ])
  where
  n = max 1 (Array.length vs)
  w = 1.0 / Int.toNumber n
  x i = (Int.toNumber i + 0.5) * w
  y v = 1.0 - v
  points = Array.intercalate " "
    (Array.mapWithIndex (\i v -> show (x i) <> "," <> show (y v)) vs)
  bar { i, v } =
    el "rect"
      [ attr "x" (show (Int.toNumber i * w + w * 0.15))
      , attr "y" (show (y v))
      , attr "width" (show (w * 0.7))
      , attr "height" (show (max 0.01 v))
      ] []

el :: forall r w i. String -> Array (HH.IProp r i) -> Array (HH.HTML w i) -> HH.HTML w i
el nm = HH.elementNS (Namespace "http://www.w3.org/2000/svg") (ElemName nm)

attr :: forall r i. String -> String -> HH.IProp r i
attr k v = HP.attr (AttrName k) v

num :: Number -> String
num n = show (Int.toNumber (Int.round (n * 1000.0)) / 1000.0)

cls :: forall r i. String -> HH.IProp (class :: String | r) i
cls = HP.class_ <<< HH.ClassName

style :: forall r i. String -> HH.IProp r i
style = HP.attr (AttrName "style")
