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
  , triggerView
  , pitchView
  , curves
  , settings
  ) where

import Prelude

import Data.Array as Array
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe, isJust, isNothing, maybe)
import Halogen (AttrName(..), ElemName(..), Namespace(..))
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Quadrat.Http as Http
import Quadrat.Pitch as Pitch
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
  -- | The calibration tables the rig doctor knows about, for the pitch picker.
  -- | Empty means either none measured or `deepstar serve` is down — the page
  -- | says which, because in a dropdown they look identical.
  , tables :: Array Http.CalibRow
  , tablesErr :: String
  -- | Choosing one has to FETCH it, so it is an action rather than a `Msg`.
  , pickPitch :: Int -> String -> act
  -- | **Make a pitch sweep out of nothing.** Choosing an instrument in an
  -- | empty Pitch section has to CREATE the parameter, because otherwise the
  -- | only way in is to add a curve in the section below, turn it into a
  -- | pitch, and then choose an instrument — three steps in two sections to
  -- | reach the thing the first section is named after.
  , addPitch :: String -> act
  -- | **Who fires the sound** — `true` when the rig does, which is what makes
  -- | a take a transect, and `false` when you play it.
  -- |
  -- | One choice, asked once. It used to be asked twice: as a Transect/By-hand
  -- | tab at the top of the page, AND as a trigger spec that was simply
  -- | ignored in the second case. Two controls for one fact is two chances to
  -- | disagree, and the page had no way to say which had won.
  , rigFires :: Boolean
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

  -- | **The encoding and a single extent are said in the statement**, which
  -- | is the one place the whole specification is legible at once. What stays
  -- | here is what the statement cannot say without becoming a paragraph: the
  -- | shape of a MULTI-axis run, and the two intervals.
  encodingRow =
    HH.div [ cls "q-swrow" ]
      ( (if Array.length axs > 1 then Array.mapWithIndex extentField axs else [])
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

-- | **Three surfaces from one renderer.**
-- |
-- | Trigger, pitch and curves each answer a different question at a different
-- | moment, and two of the three now live behind their own door. Split by a
-- | tag rather than by moving code: every helper they need lives in one
-- | where-clause, and duplicating those to gain three entry points would be
-- | three renderers to keep in step.
triggerView :: forall w act. Handlers act -> HH.HTML w act
triggerView h = part h 0

pitchView :: forall w act. Handlers act -> HH.HTML w act
pitchView h = part h 1

curves :: forall w act. Handlers act -> HH.HTML w act
curves h = part h 2

part :: forall w act. Handlers act -> Int -> HH.HTML w act
-- | **Three optional sections, ruled apart: what strikes it, what pitch it is
-- | struck at, and what else moves.**
-- |
-- | It was one undifferentiated column — trigger fields, then eight curve rows
-- | with the pitch picker buried inside whichever row happened to be a pitch.
-- | That read as one long form with no structure, when in fact each of the
-- | three is independently optional and they are answered at different times:
-- | the trigger when the cable went in, the pitch when you chose the
-- | instrument, the curves every single run.
part h which =
  HH.div [ cls "q-sweep" ]
    ( case which of
        0 -> [ triggerSection ]
        1 -> [ pitchSection ]
        _ -> [ paramsSection ] )
  where
  p = h.plan

  -- | Pitch parameters and ordinary ones, each keeping its ORIGINAL index —
  -- | every message is addressed by position in `p.params`, so filtering
  -- | without carrying the index would send every edit to the wrong parameter.
  rowsWhere wantPitch =
    Array.concat
      (Array.mapWithIndex
        (\i q -> if isJust q.pitch == wantPitch then row i q else [])
        p.params)

  sectionHead label hint =
    HH.div [ cls "q-swsec" ]
      [ HH.span [ cls "q-arm-label" ] [ HH.text label ]
      , HH.span [ cls "q-muted" ] [ HH.text hint ]
      ]

  -- | **The trigger IS the transect.**
  -- |
  -- | Whether the rig fires the sound or you do is the same question as
  -- | whether this take is a transect, and it was being asked in two places
  -- | that could disagree. Asked here, once, as the thing it actually decides
  -- | — and the fields below are the rig's instructions, so they grey out
  -- | when the rig is not the one playing.
  triggerSection =
    HH.div_
      [ sectionHead "Trigger"
          "who strikes the instrument — and so whether the take divides by its \
          \own schedule or by finding the sounds afterwards"
      -- **Absent, not greyed.** These are the rig's instructions for striking
      -- the instrument; when you are the one striking it there are no
      -- instructions, and a greyed row of eight fields is eight things to read
      -- before discovering they do not apply. WHO strikes it is said in the
      -- statement, which is the only place it is asked.
      , if h.rigFires then trigger
        else
          HH.span [ cls "q-muted" ]
            [ HH.text "You are striking it, so the rig sends nothing. The \
                      \onsets are found in the take afterwards." ]
      ]

  pitchSection =
    HH.div [ cls (if h.rigFires then "" else "is-moot") ]
      [ sectionHead "Pitch sweep"
          "optional — a measured table turns notes into the volts this \
          \instrument needs for them"
      , if Array.any (\q -> isJust q.pitch) p.params
          then HH.div [ cls "q-curves" ] (rowsWhere true)
          else newPitch
      ]

  -- | **The way in, in the section it belongs to.**
  -- |
  -- | One control: name the instrument, and the parameter that plays it comes
  -- | into being routed and ready. Everything else about a pitch axis has a
  -- | sensible answer already — the note range is the table's own span, the
  -- | curve is a line, and a jack can be changed once it exists.
  newPitch =
    HH.div [ cls "q-swrow" ]
      [ HH.label [ cls "q-stack" ]
          [ HH.span_ [ HH.text "instrument" ]
          , HH.select
              [ cls "q-swsel"
              , HP.title "a calibration table from `deepstar tune` — it names a \
                         \SIGNAL PATH, not a module, so it is only true for the \
                         \jack it was swept from"
              , HE.onValueChange h.addPitch
              ]
              ( Array.cons
                  (HH.option [ HP.value "", HP.selected true ]
                    [ HH.text "— no pitch sweep —" ])
                  (map
                    (\t -> HH.option [ HP.value t.label ]
                      [ HH.text (t.label <> "  " <> hzSpan t) ])
                    h.tables)
              )
          ]
      , HH.span [ cls "q-muted" ]
          [ HH.text
              (if h.tablesErr /= "" then h.tablesErr
               else if Array.null h.tables
                 then "No instrument has been measured yet. `deepstar tune` \
                      \builds a table by sweeping a module and listening to it."
               else "Choosing one adds the parameter that plays it, on ES-9 \
                    \jack 1, across the table's own range. Change any of that \
                    \once it is there.") ]
      ]

  paramsSection =
    HH.div [ cls (if h.rigFires then "" else "is-moot") ]
      [ sectionHead "Parameter sweep"
          "optional — one curve for every knob you want moved across the take"
      , HH.div [ cls "q-curves" ] (rowsWhere false)
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

  -- | **A pitch is not a curve over a voltage range, so it is not drawn like
  -- | one.**
  -- |
  -- | Physically the two are the same wire; as questions they are unrelated. A
  -- | modulation parameter asks for a fraction of the way between two voltages;
  -- | a pitch asks for F#3, and the volts are whatever the measured table says
  -- | that costs. Sharing a row made them look like variants of one control,
  -- | and `cvLo`/`cvHi` sat there on a pitch row doing nothing at all —
  -- | `levelOf` ignores them outright.
  row i q = case q.pitch of
    Just _ -> pitchRow i q
    Nothing -> plainRow i q

  -- | Instrument, range and where it goes — the whole of a pitch axis, inline.
  -- | The per-cell values stay behind `open`, which is where a tuning that is
  -- | not a run of semitones gets placed by hand.
  pitchRow i q =
    [ HH.div [ cls "q-curveparam is-pitch" ]
        [ HH.div [ cls "q-swtop" ]
            [ HH.input
                [ cls "q-swname", HP.type_ HP.InputText, HP.value q.name
                , HP.title "what this parameter is called on the instrument"
                , HE.onValueInput (h.msg <<< SetName i) ]
            , mini (if h.open == Just i then "close" else "open")
                "place every note by hand"
                (h.openParam (if h.open == Just i then Nothing else Just i))
            , HH.button
                [ cls "q-swmini is-drop", HP.title "remove this parameter"
                , HE.onClick \_ -> h.msg (DropParam i) ]
                [ HH.text "×" ]
            ]
        , if isJust q.pitch then HH.text "" else pitchPick i q
        , HH.div [ cls "q-swsays" ]
            [ HH.text (routing q)
            , HH.span [ cls "q-swaxis" ]
                [ HH.text (" · " <> maybe "" _.name (Array.index axs q.axis)) ]
            ]
        -- **Say when the notes are not evenly spaced.** The curve still shapes
        -- a pitch run — `noteAt` reads its value — so a shape left over from
        -- some other use would bend the scale silently now that the thumbnail
        -- is gone.
        , if isDrawn q.curve || Curve.label q.curve /= "Linear"
            then
              HH.div [ cls "q-swwarn" ]
                [ HH.text ("shaped by " <> Curve.label q.curve
                             <> " — the notes are not evenly spaced")
                , HH.button
                    [ cls "q-swfix"
                    , HP.title "back to one degree per step"
                    , HE.onClick \_ -> h.msg (NextShape i)
                    ]
                    [ HH.text "next shape" ]
                ]
            else HH.text ""
        ]
    ]
      <> (if h.open == Just i then [ sliders i q ] else [])

  -- | A curve, and the parameter it moves. The sketch's left two columns, kept
  -- | on one row so the arrow between them needs no drawing.
  -- | **A card: the shape above, what it means below.**
  -- |
  -- | Side by side, a curve and its description were two columns that had to
  -- | stay aligned across every row, which fixed the row height to the taller
  -- | of them and let four parameters fill a page. Stacked, a card is one
  -- | object of its own width, and eight of them are a line you read across —
  -- | which is what a transect's parameters are.
  plainRow i q =
    [ HH.div [ cls "q-pcard" ]
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
    let
      -- A pitch parameter's range is NOT cvLo/cvHi, and printing them would be
      -- a lie in the one place you look to check what a curve does.
      volts = case q.pitch of
        Just ps -> "  " <> Pitch.noteName ps.noteLo <> " → " <> Pitch.noteName ps.noteHi
                     <> " (" <> ps.label <> ")"
        Nothing -> "  " <> num q.cvLo <> " → " <> num q.cvHi
      -- | **A bus number names nothing you can see; the jack does.**
      -- |
      -- | Bus 8 is ES-9 panel jack 1 and bus 15 is jack 8 — the lower eight
      -- | reach expanders. On 2026-09-11 a parameter called `morph` sat on
      -- | cv 8 while the BIA's V/oct was in panel jack 1, so a plain linear
      -- | ramp swept the module's pitch and the run sounded exactly like the
      -- | pitch sweep it was not. The page said `cv 8`, which was true and
      -- | told nobody anything.
      jackOf b
        | b >= 8 && b <= 15 = " (ES-9 jack " <> show (b - 7) <> ")"
        | otherwise = ""
      parts =
        Array.catMaybes
          [ map (\b -> "cv " <> show b <> jackOf b <> volts) q.cv
          , map (\k -> "esx " <> show k <> volts) q.esx
          , map (\c -> "cc " <> show c <> " ch " <> show q.channel
                   <> "  " <> show q.ccLo <> " → " <> show q.ccHi) q.cc
          ]
    in
      if Array.null parts
        then "not routed — this parameter moves nothing"
        else Array.intercalate "   ·   " parts

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
          , tiny "esx" 2 (maybe "" show q.esx) (SetEsx i)
              "ESX-8CV channel 0-7, through Silent Way on one expander bus. \
              \12-bit, so coarser than a panel jack; blank for none"
          , tiny "at 0" 4 (num q.cvLo) (SetCvLo i)
              "what 0 means on either, -1 to 1 (1.0 is FULL output: this path is not halved)"
          , tiny "at 1" 4 (num q.cvHi) (SetCvHi i) "what 1 means on either"
          ]
      , HH.span [ cls "q-swgroup" ]
          [ tiny "cc" 3 (maybe "" show q.cc) (SetCc i) "controller number; blank for none"
          , tiny "at 0" 3 (show q.ccLo) (SetCcLo i) "controller value at 0"
          , tiny "at 1" 3 (show q.ccHi) (SetCcHi i) "controller value at 1"
          , tiny "ch" 2 (show q.channel) (SetChannel i) "MIDI channel"
          ]
      , pitchPick i q
      , axisPick i q
      ]

  -- | **Pitch, when this parameter is one.**
  -- |
  -- | A dropdown of measured tables and two note fields, and choosing a table
  -- | is what turns an ordinary parameter into a pitch: the range then reads in
  -- | semitones and `cvLo`/`cvHi` stop being consulted at all. That is why the
  -- | picker sits here rather than beside them — it does not refine the
  -- | voltage range, it replaces the question.
  pitchPick i q =
    HH.span [ cls "q-swgroup q-swpitch" ]
      [ HH.label [ cls "q-swtiny" ]
          [ HH.span_ [ HH.text "pitch" ]
          , HH.select
              [ cls "q-swsel"
              , HP.title "a calibration table from `deepstar tune` — it names a SIGNAL PATH, \
                         \not a module, so it is only true for the jack it was swept from"
              , HE.onValueChange (h.pickPitch i)
              ]
              ( Array.cons
                  (HH.option [ HP.value "", HP.selected (isNothing q.pitch) ] [ HH.text "— not pitch —" ])
                  ( map
                    (\t -> HH.option
                      [ HP.value t.label
                      , HP.selected (maybe false (\ps -> ps.label == t.label) q.pitch)
                      ]
                      [ HH.text (t.label <> "  " <> hzSpan t) ])
                    h.tables
                  )
              )
          ]
      , case q.pitch of
          Nothing ->
            -- The empty case is not blank: an empty list has two causes and
            -- they need different actions from you.
            HH.span [ cls "q-swhint" ]
              [ HH.text
                  (if h.tablesErr /= "" then h.tablesErr
                   else if Array.null h.tables then "no calibration tables — run `deepstar tune`"
                   else "") ]
          Just ps ->
            HH.span_
              -- The FIELDS carry numbers and the hint carries the names. A
              -- field that displayed "C2" while expecting `36` typed back is a
              -- box you cannot retype its own contents into.
              -- **A base and a count, not a range.** The top follows from how
              -- many samples the run makes — see `Sweep.fixPitch` — so there
              -- is one number to set and no way for two to disagree.
              [ tiny "from" 4 (show ps.noteLo) (SetPitchLo i)
                  "the lowest note, as a MIDI number — 60 is C4, 12 to an octave. \
                  \The top follows from the number of samples."
              , HH.span [ cls "q-swhint" ]
                  [ HH.text (Pitch.noteName ps.noteLo <> " – "
                              <> Pitch.noteName ps.noteHi <> " chromatic · "
                              <> reach ps) ]
              ]
      ]

  -- What a table can actually reach, because outside its span the realiser
  -- clamps and a transect comes out on one pitch.
  hzSpan t = show (Int.round t.loHz) <> "–" <> show (Int.round t.hiHz) <> " Hz"

  reach ps = case Array.head ps.table, Array.last ps.table of
    Just lo, Just hi ->
      "table reaches " <> Pitch.noteName (Pitch.hzNote lo.hz)
        <> "–" <> Pitch.noteName (Pitch.hzNote hi.hz)
    _, _ -> "table is empty"

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
      [ field "gate bus" (maybe "" show p.trigger.gate) SetGate 3
          "an es9-daemon bus pulsed to fire the sound; 15 is ES-9 panel jack 8"
      , field "level" (num p.trigger.gateLevel) SetGateLevel 4 "how high that pulse goes, -1 to 1"
      , field "es5 gate" (maybe "" show p.trigger.es5) SetEs5 3
          "one of the ES-5's own eight gates, 0-7. No level — they have none. \
          \Fires alongside the bus gate if both are set"
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
