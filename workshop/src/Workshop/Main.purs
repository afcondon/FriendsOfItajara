-- | **The Workshop** — a page for building a card.
-- |
-- | Deliberately not the Friend's looper page. That was tried first and fought
-- | back: its source bar points every loop at one input, its Record button is
-- | the obvious thing on the slab, and its takes are loops with layers. Two
-- | capture sessions were lost to pressing the obvious thing.
-- |
-- | The insight that makes this page small: **a hit session is not ten
-- | recordings, it is one recording with ten hits in it.** Arm once, play, stop
-- | by hand, and divide afterwards with the onset detector in `msm`. Everything
-- | that went wrong before — fixed pass lengths, alternates summing, layer
-- | stacks — came from using loops for something that does not loop.
-- |
-- | So the daemon is used as little as possible: one scratch loop, one open
-- | recording, level-armed. All the judgement is downstream.
module Workshop.Main where

import Prelude

import Control.Monad.Rec.Class (forever)
import Data.Array as Array
import Data.Foldable (for_)
import Data.Int as Int
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Number as Number
-- `Bars` names a thing in both vocabularies — a length in the daemon's verbs
-- and a kind of material here — so the verbs come in by name and the kinds
-- through `Kind.`.
import Data.Looper.Verb (Verb(Alternates, AskPeaks, Clear, ExportLayers, LevelArm, Mono, OnGrid, Record, Sounding, Source))
import Data.Looper.Verb as Verb
import Effect (Effect)
import Effect.Aff (Milliseconds(..), attempt, delay)
import Effect.Aff as Aff
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Foreign.LooperSocket (LooperState, LoopState)
import Foreign.LooperSocket as Socket
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Subscription as HS
import Halogen.VDom.Driver (runUI)
import Data.Set (Set)
import Data.Set as Set
import Control.Promise (toAffE)
import Workshop.Audio as Audio
import Workshop.Http as Http
import Workshop.Wave as Wave
import Workshop.Kind (Close(..), Kind)
import Workshop.Kind as Kind

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI component unit body

-- | **The scratch loop.** The Workshop records into the last loop and nothing
-- | else, so a pedalboard session on loops 1–6 is untouched by a capture and a
-- | capture is untouched by it. It is a tape head, not a loop: cleared before
-- | every take, and its contents are a take on their way to `msm`.
scratch :: Int
scratch = 7

type State =
  { looper :: Maybe LooperState
  , kind :: Kind
  , bars :: Int
  , armed :: Boolean
  , name :: String
  , log :: Array String
  -- | The picture, from the daemon: it holds the audio, so it draws it.
  , peaks :: Maybe Socket.Peaks
  -- | Where `msm` thinks things begin, over the take just written. Empty until
  -- | a take has been closed and analysed.
  , regions :: Array Http.Region
  -- | **Which of them you actually want.** The detector proposes and is often
  -- | right and sometimes not: a stick tapped by accident is a division too.
  -- | Indices into `regions`; everything starts kept.
  , keep :: Set Int
  , busy :: Boolean
  -- | Sweeping across the grid to hear it, rather than clicking each one.
  -- | Off by default: it is the right gesture for comparing forty hits and the
  -- | wrong one for a page you are only reading.
  , hoverPlays :: Boolean
  , playing :: Maybe Int
  -- | The take the grid is of. Held rather than read from `name`, so that
  -- | typing a new name does not silently repoint the audio at a take that
  -- | has not been recorded yet.
  , showing :: String
  -- | A take has been asked to close and its layer has not landed yet.
  -- |
  -- | Closing is not instant: the daemon commits the layer on its own thread,
  -- | measured at about 70 ms after the press. Analysing straight away found
  -- | `layers == 0`, took the "nothing to do" branch and said nothing at all,
  -- | which is why two recordings in a row appeared to do nothing.
  , waiting :: Boolean
  -- | **How close two sounds can be and still be two**, in milliseconds.
  -- |
  -- | The knob that does not discriminate by loudness — which matters, because
  -- | a velocity stack is played softest first and a threshold on level culls
  -- | exactly the quiet end the stack exists for. Measured on a real take: at
  -- | 600 ms it kept all eleven hits including the softest, where the level
  -- | knob had dropped that one and merged its neighbour into the tile before.
  , minGap :: Number
  }

data Action
  = Init
  | Poll
  | PickKind Kind
  | SetBars String
  | PickSource Int
  | SetMono Boolean
  | SetName String
  | Arm
  | Close
  | Discard
  | ToggleKeep Int
  | KeepAll Boolean
  | Analyse
  | Divide
  | SetGap String
  | Play Int
  | HoverPlay Int
  | SetHoverPlays Boolean

component :: forall q i o m. MonadAff m => H.Component q i o m
component = H.mkComponent
  { initialState: \_ ->
      { looper: Nothing, kind: Kind.DrumHits, bars: 1
      , armed: false, name: "kick", log: []
      , peaks: Nothing, regions: [], keep: Set.empty, busy: false
      , hoverPlays: false, playing: Nothing, showing: "", waiting: false
      , minGap: 300.0 }
  , render
  , eval: H.mkEval H.defaultEval { handleAction = handleAction, initialize = Just Init }
  }

send :: forall o m. MonadAff m => Verb -> H.HalogenM State Action () o m Unit
send v = do
  let c = Verb.at scratch v
  ok <- liftEffect (Socket.send (c <> "@0"))
  unless ok $ H.modify_ (note ("no daemon — " <> c <> " went nowhere"))

-- | A verb for the rig rather than for a loop. `exl` is one: it writes every
-- | loop that has anything in it, so addressing it to one would be a lie.
sendBare :: forall o m. MonadAff m => Verb -> H.HalogenM State Action () o m Unit
sendBare v = do
  ok <- liftEffect (Socket.send (Verb.render v <> "@0"))
  unless ok $ H.modify_ (note "no daemon — the take was not written")

note :: String -> State -> State
note m s = s { log = Array.takeEnd 10 (Array.snoc s.log m) }

loop :: State -> Maybe LoopState
loop st = st.looper >>= \top -> Array.index top.loops scratch

handleAction :: forall o m. MonadAff m => Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  Init -> do
    liftEffect $ Socket.connect Socket.defaultUrl
    void $ H.subscribe $ HS.makeEmitter \emit -> do
      fiber <- Aff.launchAff $ forever do
        delay (Milliseconds 100.0)
        liftEffect (emit Poll)
      pure (Aff.launchAff_ (Aff.killFiber (Aff.error "stopped") fiber))
  Poll -> do
    before <- H.get
    snap <- liftEffect Socket.latest
    pk <- liftEffect Socket.latestPeaks
    H.modify_ _ { looper = snap, peaks = pk }
    -- The daemon draws; ask it to, the first time a take comes into view. That
    -- covers a reload as well as a recording — the engine did not forget the
    -- loop just because the page did.
    now <- H.get
    let had = maybe false (\l -> l.layers > 0) (loop before)
        has = maybe false (\l -> l.layers > 0) (loop now)
    when (has && not had) (send (AskPeaks 900))
    -- A take that closed itself — a bar count — leaves `armed` set here, so
    -- the page would go on saying "listening" over a finished recording.
    st <- H.get
    for_ (loop st) \lp ->
      when (st.armed && not lp.armed && not Socket.isWriting lp && lp.layers > 0) $
        H.modify_ (note ("closed itself: " <> fmt lp.loopSecs <> " s")
                     <<< _ { armed = false, waiting = true })

    -- The one place a take becomes something to look at, whichever way it
    -- ended — by hand, or at its own count.
    st2 <- H.get
    for_ (loop st2) \lp ->
      when (st2.waiting && lp.layers > 0 && not Socket.isWriting lp && not lp.armed) do
        H.modify_ _ { waiting = false }
        handleAction Analyse
  PickKind k -> H.modify_ _ { kind = k }
  SetBars v -> H.modify_ \s ->
    let n = clamp 1 64 (fromMaybe s.bars (Int.fromString v))
    in s { bars = n, kind = case s.kind of
                             Kind.Bars _ -> Kind.Bars n
                             other -> other }
  -- **The loop's source is the loop's, not the page's.**
  --
  -- Held here once, and asserted again at Arm, it silently overrode whatever
  -- had been chosen before a reload — the page came back thinking "board",
  -- said nothing, and armed on an input with no drums on it. So: sent when
  -- you click it, read back from the snapshot, and never re-asserted. The
  -- daemon is the one that knows.
  PickSource n -> send (Source n)
  SetMono b -> send (Mono b)
  SetName v -> H.modify_ _ { name = v }
  Analyse -> analyse true
  Divide -> analyse false
  SetGap v -> do
    H.modify_ \s -> s { minGap = fromMaybe s.minGap (Number.fromString v) }
    st <- H.get
    when (st.showing /= "") (analyse false)
  SetHoverPlays b -> do
    unless b (liftEffect Audio.stop)
    H.modify_ _ { hoverPlays = b }
  HoverPlay i -> do
    st <- H.get
    when st.hoverPlays (handleAction (Play i))
  Play i -> do
    st <- H.get
    for_ (Array.index st.regions i) \r -> do
      liftEffect (Audio.playRange ("/api/take-audio?take=" <> st.showing) r.start r.end)
      H.modify_ _ { playing = Just i }
  ToggleKeep i -> H.modify_ \s ->
    s { keep = if Set.member i s.keep then Set.delete i s.keep else Set.insert i s.keep }
  KeepAll on -> H.modify_ \s ->
    s { keep = if on then Set.fromFoldable (Array.range 0 (Array.length s.regions - 1)) else Set.empty }
  Discard -> do
    send Clear
    H.modify_ (note "cleared" <<< _ { regions = [], keep = Set.empty, peaks = Nothing })
  Arm -> do
    st <- H.get
    -- Everything the take needs, set before it starts and nowhere else. The
    -- scratch loop is emptied first: it holds one take at a time, and a take
    -- that landed on top of another is the bug this page exists to avoid.
    send Clear
    -- Not the source and not mono: those are the loop's own, set when you
    -- chose them and shown from the snapshot. Asserting them here is how the
    -- page came to overrule a choice it had forgotten making.
    -- Never alternates. Alternates sums a further pass into the layer that
    -- sounds, which is right for takes of one scene and wrong for everything
    -- here — and it is what put ten kicks in one layer on the looper page.
    send (Alternates false)
    -- Silent while it fills. You are playing into it, not along to it.
    send (Sounding false)
    send (OnGrid false)
    case Kind.closes st.kind of
      AtCount n -> send (Verb.Bars n)
      ByHand -> pure unit
    -- The whole of the arming: `r` now waits for a sound instead of starting
    -- on the press, and the daemon reaches back past the crossing so the
    -- attack that triggered it is inside the take.
    send (LevelArm true)
    send Record
    H.modify_ (note (Kind.prompt st.kind) <<< _ { armed = true })
  Close -> do
    st <- H.get
    for_ (loop st) \lp ->
      if Socket.isWriting lp || lp.armed
        then do
          send Record
          H.modify_ (note "closed")
        else H.modify_ (note "nothing is recording")
    send (LevelArm false)
    send (Sounding true)
    -- Not `Analyse` here: the layer is not there yet. Ask for it, and let the
    -- poll that sees it arrive do the work.
    H.modify_ \s -> s { armed = false, waiting = true }

-- | Write the take down and ask where the sounds are.
-- |
-- | Two hops, because they are two different kinds of knowledge: the daemon
-- | holds the audio and writes it, `msm` reads the file and says where things
-- | begin. Neither could do the other's half.
-- | `write` is false when the take is already on disk and only the dividing
-- | is being asked again — which is what moving the slider does, and it should
-- | not cost a re-export every time.
analyse :: forall o m. MonadAff m => Boolean -> H.HalogenM State Action () o m Unit
analyse write = do
  st <- H.get
  case loop st of
    Just lp | lp.layers > 0 -> divide st
    _ -> H.modify_ (note "nothing to divide — the scratch loop is empty")
  where
  divide st = do
      H.modify_ _ { busy = true, regions = [], keep = Set.empty }
      when write do
        send (AskPeaks 900)
        sendBare (ExportLayers st.name)
        -- The daemon writes on its own thread and the ack lands in a snapshot;
        -- the folder is there a moment later.
        H.liftAff (delay (Milliseconds 900.0))
      let takeName = if write then st.name else st.showing
      r <- H.liftAff (attempt (toAffE (Http.divisions takeName (Kind.material st.kind) st.minGap)))
      case r of
        Left e -> H.modify_ (note ("could not analyse: " <> Aff.message e) <<< _ { busy = false })
        Right d
          | not d.ok -> H.modify_ (note d.output <<< _ { busy = false })
          | otherwise -> do
              let n = Array.length d.regions
              H.modify_ _
                { regions = d.regions
                , keep = Set.fromFoldable (Array.range 0 (n - 1))
                , busy = false
                , showing = takeName
                }
              H.modify_ (note
                (show n <> (if n == 1 then " division" else " divisions")
                  <> " over " <> fmt d.secs <> " s"
                  <> (if d.divides then "" else " (this kind is kept whole)")))

fmt :: Number -> String
fmt n = show (Int.round (n * 100.0) # \k -> Int.toNumber k / 100.0)

render :: forall m. State -> H.ComponentHTML Action () m
render st =
  HH.div [ HP.class_ (HH.ClassName "ws") ]
    [ HH.header [ HP.class_ (HH.ClassName "ws-head") ]
        [ HH.h1_ [ HH.text "Workshop" ]
        , HH.span [ HP.class_ (HH.ClassName "ws-sub") ]
            [ HH.text "record material, divide it, put it on a card" ]
        , connection
        ]
    , sourceBar
    , recordBox
    , caught
    , HH.section [ HP.class_ (HH.ClassName "ws-card") ]
        [ HH.h2_ [ HH.text "The card" ]
        , HH.p [ HP.class_ (HH.ClassName "ws-muted") ]
            [ HH.text
                "A virtual card lives here — banks, kits, voices — and stays a \
                \manifest until you ask for it. Writing it to a real card is a \
                \compile, never an edit in place, so what is on the card is \
                \always something you could read first. Next." ]
        ]
    , HH.section [ HP.class_ (HH.ClassName "ws-log") ]
        (map (\l -> HH.div_ [ HH.text l ]) st.log)
    ]
  where
  lp = loop st
  -- What the daemon says this loop is doing, never a second copy of it.
  srcNow = maybe 0 _.src lp
  isMono = maybe true _.mono lp
  srcName = maybe "?" _.name
    (st.looper >>= \top -> Array.index top.sources (srcNow - 1))
  hasTake = maybe false (\l -> l.layers > 0) lp
  writing = maybe false Socket.isWriting lp
  listening = maybe false _.armed lp
  -- How long this take has been running, from the daemon's own frame count
  -- rather than from a clock here: a page that keeps its own time drifts from
  -- the recording it is describing.
  elapsed =
    let sr = maybe 48000 _.sampleRate st.looper
    in maybe "0" (\l -> fmt (Int.toNumber l.recFrames / Int.toNumber sr)) lp

  connection = case st.looper of
    Nothing -> HH.span [ HP.class_ (HH.ClassName "ws-warn") ] [ HH.text "no daemon" ]
    Just _ -> HH.span [ HP.class_ (HH.ClassName "ws-ok") ] [ HH.text "daemon" ]

  sourceBar =
    HH.section [ HP.class_ (HH.ClassName "ws-source") ]
      [ HH.h2_ [ HH.text "Input" ]
      , HH.div [ HP.class_ (HH.ClassName "ws-chips") ]
          (maybe [ HH.text "—" ]
            (\top -> Array.mapWithIndex chip top.sources)
            st.looper)
      , HH.div [ HP.class_ (HH.ClassName "ws-toggle") ]
          [ tog "mono" isMono (SetMono true)
          , tog "stereo" (not isMono) (SetMono false)
          ]
      ]

  chip n s =
    HH.button
      [ HP.class_ (HH.ClassName ("ws-chip"
          <> (if srcNow == n + 1 then " on" else "")
          <> (if s.available then "" else " off")))
      , HP.disabled (not s.available || st.armed || writing)
      , HP.title (if s.available
                    then s.name <> " — " <> fmt s.db <> " dBFS"
                    else s.name <> " is on an interface that is not switched on")
      , HE.onClick \_ -> PickSource (n + 1)
      ]
      [ HH.span [ HP.class_ (HH.ClassName "ws-chip-name") ] [ HH.text s.name ]
      , HH.span [ HP.class_ (HH.ClassName "ws-chip-db") ] [ HH.text (fmt s.db) ]
      ]

  tog lbl on act =
    HH.button
      [ HP.class_ (HH.ClassName ("ws-tog" <> if on then " on" else ""))
      , HP.disabled (st.armed || writing)
      , HE.onClick \_ -> act
      ]
      [ HH.text lbl ]

  recordBox =
    HH.section [ HP.class_ (HH.ClassName "ws-rec") ]
      [ HH.h2_ [ HH.text "Record" ]
      , HH.div [ HP.class_ (HH.ClassName "ws-kinds") ]
          (map kindBtn Kind.all)
      , case st.kind of
          Kind.Bars _ ->
            HH.label [ HP.class_ (HH.ClassName "ws-field") ]
              [ HH.span_ [ HH.text "How many bars" ]
              , HH.input
                  [ HP.type_ HP.InputText, HP.value (show st.bars)
                  , HP.disabled (st.armed || writing)
                  , HE.onValueInput SetBars ]
              ]
          _ -> HH.text ""
      , HH.label [ HP.class_ (HH.ClassName "ws-field") ]
          [ HH.span_ [ HH.text "Call it" ]
          , HH.input
              [ HP.type_ HP.InputText, HP.value st.name
              , HP.disabled (st.armed || writing)
              , HE.onValueInput SetName ]
          ]
      , HH.p [ HP.class_ (HH.ClassName "ws-blurb") ]
          [ HH.text (Kind.blurb st.kind)
          , HH.text (" Recording from " <> srcName <> (if isMono then ", mono." else ", stereo."))
          ]
      , HH.div [ HP.class_ (HH.ClassName "ws-actions") ]
          [ if st.armed || writing || listening
              then HH.button
                     [ HP.class_ (HH.ClassName "ws-big is-stop"), HE.onClick \_ -> Close ]
                     [ HH.text (if writing then "Stop" else "Cancel") ]
              else HH.button
                     [ HP.class_ (HH.ClassName "ws-big")
                     , HP.disabled (st.looper == Nothing)
                     , HE.onClick \_ -> Arm ]
                     [ HH.text "Arm" ]
          , HH.span [ HP.class_ (HH.ClassName "ws-state") ]
              [ HH.text
                  (if writing then "recording — " <> elapsed <> " s"
                   else if listening then Kind.prompt st.kind
                   else maybe "" (\l -> if l.layers > 0
                                          then "captured " <> fmt l.loopSecs <> " s"
                                          else "ready") lp) ]
          , case lp of
              Just l | l.layers > 0 && not writing && not listening ->
                HH.button [ HP.class_ (HH.ClassName "ws-plain"), HE.onClick \_ -> Discard ]
                  [ HH.text "Discard" ]
              _ -> HH.text ""
          ]
      ]

  -- | **What was caught, and what it was divided into.**
  -- |
  -- | The whole take across the top, and then every sub-sample as its own tile
  -- | underneath. Small multiples because the question is comparative — which
  -- | of these forty is the one I meant, and are they the same sound? — and a
  -- | list of numbers cannot be read that way while a grid of shapes can.
  -- Shown whenever there is anything to show — which includes a take the
  -- daemon is still holding from before the page was reloaded. A page that
  -- forgot a recording the engine had not forgotten was the difference between
  -- "nothing changed" and "everything is one press away".
  caught
    | Array.null st.regions && not st.busy && not hasTake = HH.text ""
    | otherwise =
        HH.section [ HP.class_ (HH.ClassName "ws-caught") ]
          [ HH.h2_ [ HH.text "What was caught" ]
          , case st.peaks of
              Just pk | Array.length pk.hi > 0 ->
                HH.div [ HP.class_ (HH.ClassName "ws-whole") ]
                  [ Wave.svg pk.lo pk.hi [ Wave.klass "ws-whole-svg" ] ]
              _ -> HH.text ""
          , HH.div [ HP.class_ (HH.ClassName "ws-gridhead") ]
              [ HH.span_
                  [ HH.text (if st.busy then "dividing…"
                             else if Array.null st.regions
                               then maybe "" (\l -> fmt l.loopSecs <> " s recorded, not divided yet") (loop st)
                               else show (Set.size st.keep) <> " of "
                                    <> show (Array.length st.regions) <> " kept") ]
              , if Array.null st.regions && hasTake && not st.busy
                  then HH.button [ HP.class_ (HH.ClassName "ws-plain"), HE.onClick \_ -> Analyse ]
                         [ HH.text "Divide it" ]
                  else HH.text ""
              , HH.button [ HP.class_ (HH.ClassName "ws-plain"), HE.onClick \_ -> KeepAll true ]
                  [ HH.text "Keep all" ]
              , HH.button [ HP.class_ (HH.ClassName "ws-plain"), HE.onClick \_ -> KeepAll false ]
                  [ HH.text "Keep none" ]
              , HH.label [ HP.class_ (HH.ClassName "ws-quiet") ]
                  -- A wider gap means fewer divisions, so FEWER is the
                  -- right-hand end — the same direction the old level knob
                  -- ran, and the one people expect.
                  [ HH.span_ [ HH.text "more" ]
                  , HH.input
                      [ HP.type_ HP.InputRange
                      , HP.min 25.0, HP.max 1200.0, HP.step (HP.Step 25.0)
                      , HP.value (show st.minGap)
                      , HE.onValueChange SetGap
                      , HP.title "how close two sounds can be and still be two, in milliseconds"
                      ]
                  , HH.span_ [ HH.text "fewer" ]
                  , HH.span [ HP.class_ (HH.ClassName "ws-gapval") ]
                      [ HH.text (show (Int.round st.minGap) <> " ms") ]
                  ]
              , HH.label [ HP.class_ (HH.ClassName "ws-hover") ]
                  [ HH.input
                      [ HP.type_ HP.InputCheckbox, HP.checked st.hoverPlays
                      , HE.onChecked SetHoverPlays ]
                  , HH.span_ [ HH.text "hover plays" ]
                  ]
              ]
          , HH.div [ HP.class_ (HH.ClassName "ws-grid") ]
              (Array.mapWithIndex tile st.regions)
          ]

  -- One sub-sample. Its picture is a SLICE of the take's own envelope, so
  -- forty tiles cost one snapshot rather than forty requests.
  tile i r =
    let
      total = maybe 1.0 (\l -> l.loopSecs) (loop st)
      n = maybe 0 (Array.length <<< _.hi) st.peaks
      b = Wave.bucketsFor n total r.start r.end
      cut xs = Array.slice b.from b.to xs
      kept = Set.member i st.keep
    in
      HH.div
        [ HP.class_ (HH.ClassName ("ws-tile"
            <> (if kept then "" else " is-dropped")
            <> (if st.playing == Just i then " is-playing" else "")))
        , HE.onMouseEnter \_ -> HoverPlay i
        ]
        [ HH.button
            [ HP.class_ (HH.ClassName "ws-tile-face")
            , HP.title (fmt (r.end - r.start) <> " s at " <> fmt r.start <> " s")
            , HE.onClick \_ -> Play i
            ]
            [ Wave.svg (maybe [] (cut <<< _.lo) st.peaks)
                       (maybe [] (cut <<< _.hi) st.peaks)
                       [ Wave.klass "ws-tile-svg" ]
            ]
        , HH.div [ HP.class_ (HH.ClassName "ws-tile-foot") ]
            [ HH.span [ HP.class_ (HH.ClassName "ws-tile-n") ] [ HH.text (show (i + 1)) ]
            , HH.span [ HP.class_ (HH.ClassName "ws-tile-len") ]
                [ HH.text (fmt (r.end - r.start)) ]
            , HH.button
                [ HP.class_ (HH.ClassName "ws-tile-keep")
                , HP.title (if kept then "drop this one" else "keep this one")
                , HE.onClick \_ -> ToggleKeep i
                ]
                [ HH.text (if kept then "✓" else "·") ]
            ]
        ]

  kindBtn k =
    HH.button
      [ HP.class_ (HH.ClassName ("ws-kind" <> if Kind.name k == Kind.name st.kind then " on" else ""))
      , HP.disabled (st.armed || writing)
      , HE.onClick \_ -> PickKind (case k of
                                     Kind.Bars _ -> Kind.Bars st.bars
                                     other -> other)
      ]
      [ HH.text (Kind.label k) ]
