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
import Data.Maybe (Maybe(..), fromMaybe, maybe)
-- `Bars` names a thing in both vocabularies — a length in the daemon's verbs
-- and a kind of material here — so the verbs come in by name and the kinds
-- through `Kind.`.
import Data.Looper.Verb (Verb(Alternates, Clear, LevelArm, Mono, OnGrid, Record, Sounding, Source))
import Data.Looper.Verb as Verb
import Effect (Effect)
import Effect.Aff (Milliseconds(..), delay)
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
  -- | Which input, one-based into the daemon's `sources`.
  , src :: Int
  , mono :: Boolean
  , armed :: Boolean
  , name :: String
  , log :: Array String
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

component :: forall q i o m. MonadAff m => H.Component q i o m
component = H.mkComponent
  { initialState: \_ ->
      { looper: Nothing, kind: Kind.DrumHits, bars: 1, src: 1, mono: true
      , armed: false, name: "kick", log: [] }
  , render
  , eval: H.mkEval H.defaultEval { handleAction = handleAction, initialize = Just Init }
  }

send :: forall o m. MonadAff m => Verb -> H.HalogenM State Action () o m Unit
send v = do
  let c = Verb.at scratch v
  ok <- liftEffect (Socket.send (c <> "@0"))
  unless ok $ H.modify_ (note ("no daemon — " <> c <> " went nowhere"))

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
    snap <- liftEffect Socket.latest
    H.modify_ _ { looper = snap }
    -- A take that closed itself — a bar count — leaves `armed` set here, so
    -- the page would go on saying "listening" over a finished recording.
    st <- H.get
    for_ (loop st) \lp ->
      when (st.armed && not lp.armed && not Socket.isWriting lp && lp.layers > 0) $
        H.modify_ (note ("closed itself: " <> fmt lp.loopSecs <> " s")
                     <<< _ { armed = false })
  PickKind k -> H.modify_ _ { kind = k }
  SetBars v -> H.modify_ \s ->
    let n = clamp 1 64 (fromMaybe s.bars (Int.fromString v))
    in s { bars = n, kind = case s.kind of
                             Kind.Bars _ -> Kind.Bars n
                             other -> other }
  PickSource n -> H.modify_ _ { src = n }
  SetMono b -> H.modify_ _ { mono = b }
  SetName v -> H.modify_ _ { name = v }
  Discard -> do
    send Clear
    H.modify_ (note "cleared")
  Arm -> do
    st <- H.get
    -- Everything the take needs, set before it starts and nowhere else. The
    -- scratch loop is emptied first: it holds one take at a time, and a take
    -- that landed on top of another is the bug this page exists to avoid.
    send Clear
    send (Source st.src)
    send (Mono st.mono)
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
    H.modify_ \s -> s { armed = false }

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
          [ tog "mono" st.mono (SetMono true)
          , tog "stereo" (not st.mono) (SetMono false)
          ]
      ]

  chip n s =
    HH.button
      [ HP.class_ (HH.ClassName ("ws-chip"
          <> (if st.src == n + 1 then " on" else "")
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
      , HH.p [ HP.class_ (HH.ClassName "ws-blurb") ] [ HH.text (Kind.blurb st.kind) ]
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

  kindBtn k =
    HH.button
      [ HP.class_ (HH.ClassName ("ws-kind" <> if Kind.name k == Kind.name st.kind then " on" else ""))
      , HP.disabled (st.armed || writing)
      , HE.onClick \_ -> PickKind (case k of
                                     Kind.Bars _ -> Kind.Bars st.bars
                                     other -> other)
      ]
      [ HH.text (Kind.label k) ]
