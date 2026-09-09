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
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.Number as Number
import Data.String as String
-- `Bars` names a thing in both vocabularies — a length in the daemon's verbs
-- and a kind of material here — so the verbs come in by name and the kinds
-- through `Kind.`.
import Data.Looper.Verb (Verb(Alternates, AskPeaks, Clear, ExportLayers, LevelArm, OnGrid, Record, Sounding, Source))
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
import Workshop.Kind (Close(..), Fold(..), Kind)
import Workshop.Kind as Kind
import Workshop.Slug (slugFor)
import Workshop.Divider (Divider)
import Workshop.Divider as Divider

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
  , divider :: Divider
  , mine :: Boolean
  , kitMine :: Boolean
  , equalN :: Int
  -- | The virtual card, as the server flattens it, plus what `kit build` says
  -- | about it. Refreshed after anything that could change it.
  , cardView :: Maybe Http.CardView
  -- | Where a kept set is going: the bank, the kit, and which voice.
  , bank :: String
  , kit :: String
  , voice :: Int
  , cardBusy :: Boolean
  }

data Action
  = Init
  | Poll
  | PickKind Kind
  | SetBars String
  | SetName String
  | ArmOn Int
  | Close
  | ToggleKeep Int
  | KeepAll Boolean
  | Analyse
  | Divide
  | SetGap String
  | PickDivider Divider
  | SetEqualN String
  | RefreshCard
  | SetBank String
  | SetKit String
  | SetVoice String
  | SendToCard
  | WriteCard String
  | Play Int
  | HoverPlay Int
  | SetHoverPlays Boolean

component :: forall q i o m. MonadAff m => H.Component q i o m
component = H.mkComponent
  { initialState: \_ ->
      { looper: Nothing, kind: Kind.DrumHits, bars: 1
      , armed: false, name: "", log: []
      , peaks: Nothing, regions: [], keep: Set.empty, busy: false
      , hoverPlays: false, playing: Nothing, showing: "", waiting: false
      , minGap: 300.0, divider: Divider.Attacks, equalN: 16, mine: false, kitMine: false
      , cardView: Nothing, bank: "WORKSHOP", kit: "", voice: 1, cardBusy: false }
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
    n <- liftEffect (slugFor Kind.DrumHits)
    H.modify_ _ { name = n }
    handleAction RefreshCard
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
  PickKind k -> do
    H.modify_ _ { kind = k, divider = Divider.defaultFor k }
    st <- H.get
    -- The name says what the take holds, so changing what you are about to
    -- record renames it — unless you have typed one of your own, which the
    -- generated shape lets us recognise.
    when (wantsAName st) do
      n <- liftEffect (slugFor k)
      H.modify_ _ { name = n }
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
  SetName v -> H.modify_ _ { name = v, mine = v /= "" }
  RefreshCard -> do
    r <- H.liftAff (attempt (toAffE Http.card))
    case r of
      Left e -> H.modify_ (note ("could not read the card: " <> Aff.message e))
      Right v -> H.modify_ _ { cardView = Just v }
  SetBank v -> H.modify_ _ { bank = v }
  SetKit v -> H.modify_ _ { kit = v, kitMine = v /= "" }
  SetVoice v -> H.modify_ \s -> s { voice = clamp 1 4 (fromMaybe s.voice (Int.fromString v)) }
  WriteCard dest -> do
    H.modify_ _ { cardBusy = true }
    r <- H.liftAff (attempt (toAffE (Http.writeToCard dest)))
    case r of
      Left e -> H.modify_ (note (Aff.message e) <<< _ { cardBusy = false })
      Right w -> H.modify_ (note (lastLine w.output) <<< _ { cardBusy = false })
    handleAction RefreshCard
  SendToCard -> do
    st <- H.get
    -- Only what you kept, in the order they were played. `msm cut` numbers
    -- them zero-padded from that order, and the module reads a voice's stack
    -- in byte order — so this is the step where "softest first" stops being a
    -- thing you remember and becomes a fact about filenames.
    let keptRegions = Array.catMaybes
          (Array.mapWithIndex
            (\i r -> if Set.member i st.keep then Just r else Nothing)
            st.regions)
    if Array.null keptRegions
      then H.modify_ (note "nothing kept, so there is nothing to send")
      else do
        H.modify_ _ { cardBusy = true }
        let setName = if st.name == "" then "set" else st.name
        r <- H.liftAff (attempt (toAffE (Http.addToCard
              { take: st.showing, set: setName
              , bank: st.bank
              , kit: if st.kit == "" then setName else st.kit
              , voice: st.voice
              , kind: Kind.name st.kind
              , stereo: Kind.foldsTo st.kind /= ToMono
              -- **Equal slices are always one file, whatever the material.**
              --
              -- The kind says what a take usually becomes, but the divider
              -- says what its pieces actually are — and pieces of the same
              -- length exist to be indexed by the start point, which only
              -- works inside one file. Without this a four-minute drone cut
              -- into 32 would have tried to be 32 layers, and the module
              -- plays twelve.
              , join: Kind.joins st.kind || isEqual st.divider
              , regions: keptRegions })))
        case r of
          Left e -> H.modify_ (note (Aff.message e) <<< _ { cardBusy = false })
          Right w -> H.modify_ (note (lastLine w.output) <<< _ { cardBusy = false })
        handleAction RefreshCard
  Analyse -> analyse true
  Divide -> analyse false
  SetGap v -> do
    H.modify_ \s -> s { minGap = fromMaybe s.minGap (Number.fromString v) }
    st <- H.get
    when (st.showing /= "") (analyse false)
  -- Choosing an algorithm re-divides at once. It costs one pass over a take
  -- already on disk, and the whole value of naming them is being able to see
  -- both answers next to each other.
  PickDivider dv -> do
    H.modify_ _ { divider = dv }
    st <- H.get
    when (st.showing /= "") (analyse false)
  SetEqualN v -> do
    H.modify_ \s ->
      let n = clamp 2 128 (fromMaybe s.equalN (Int.fromString v))
      in s { equalN = n, divider = case s.divider of
                                     Divider.Equal _ -> Divider.Equal n
                                     other -> other }
    st <- H.get
    when (st.showing /= "" && Divider.needsCount st.divider) (analyse false)
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
  ArmOn src -> do
    -- **Choosing the input is the act of arming.** Kept apart, the page had
    -- its own idea of which input to use and asserted it at Arm — so a reload
    -- silently armed on the wrong one. Together, the thing you press says what
    -- it is going to listen to, and there is no second copy to disagree.
    send (Source src)
    st <- H.get
    -- Everything the take needs, set before it starts and nowhere else. The
    -- scratch loop is emptied first: it holds one take at a time, and a take
    -- that landed on top of another is the bug this page exists to avoid.
    --
    -- This is also why there is no Discard button. Arming clears, so discarding
    -- was only ever a way of doing early what the next take does anyway — and a
    -- second button that says "throw it away" next to one that says "keep it"
    -- invites the reading that the kept set is somehow at stake. It is not:
    -- what has been sent to a kit is on disk and nothing here can reach it.
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
    -- A fresh name unless you gave it one that has not been used yet. This is
    -- the whole of the overwrite fix: the export is `exl <name>`, so a name
    -- that already belongs to a take on disk is a name that destroys it.
    when (wantsAName st) do
      n <- liftEffect (slugFor st.kind)
      H.modify_ _ { name = n, kit = if st.kitMine then st.kit else "" }
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
      r <- H.liftAff (attempt (toAffE (Http.divisions
            { take: takeName
            , as: Kind.material st.kind
            , by: Divider.name st.divider
            , minGap: st.minGap })))
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

-- | Uniform slots, which is the case the start point was made for.
isEqual :: Divider -> Boolean
isEqual = case _ of
  Divider.Equal _ -> true
  _ -> false

-- | **Should this take be given a name of its own?**
-- |
-- | The first version of this asked whether the *current* name had been used
-- | yet, which sounds equivalent and is not: on a fresh page nothing has been
-- | used, so choosing a kind never renamed anything and four takes of chords
-- | all came out called `drum-hits-…`. They were distinct files and none was
-- | lost, but a name that lies about what it holds is barely better than a
-- | name that collides.
-- |
-- | So ask the honest question instead — did you type this yourself? A
-- | generated name is ours to replace whenever the kind or the take changes; a
-- | name you typed is yours, and is only replaced once a take has actually
-- | claimed it, because at that point keeping it would overwrite.
wantsAName :: State -> Boolean
wantsAName st = not st.mine || st.name == "" || st.name == st.showing

-- | The last thing a command said, which is its summary. The rest is a list of
-- | files and belongs in the log it already went to.
lastLine :: String -> String
lastLine s = fromMaybe s (Array.last (Array.filter (_ /= "") (String.split (String.Pattern "\n") s)))

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
    , recordBox
    , caught
    , cardView
    , HH.section [ HP.class_ (HH.ClassName "ws-log") ]
        (map (\l -> HH.div_ [ HH.text l ]) st.log)
    ]
  where
  lp = loop st
  -- What the daemon says this loop is doing, never a second copy of it.
  srcNow = maybe 0 _.src lp
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

  -- | **The inputs, and pressing one is what arms.**
  -- |
  -- | Andrew's simplification, and it removes a whole class of mistake: the
  -- | page cannot arm on an input you did not just choose, because choosing is
  -- | the gesture. Each chip shows what the daemon says that input is doing
  -- | right now, so a dead one is visible before you play into it.
  armRow =
    HH.div [ HP.class_ (HH.ClassName "ws-arm") ]
      [ HH.span [ HP.class_ (HH.ClassName "ws-arm-label") ] [ HH.text "Arm on" ]
      , HH.div [ HP.class_ (HH.ClassName "ws-chips") ]
          (maybe [ HH.text "no daemon" ]
            (\top -> Array.mapWithIndex chip top.sources)
            st.looper)
      ]

  chip n s =
    HH.button
      [ HP.class_ (HH.ClassName ("ws-chip is-arm"
          <> (if srcNow == n + 1 then " on" else "")
          <> (if s.available then "" else " off")))
      , HP.disabled (not s.available)
      , HP.title (if s.available
                    then "arm on " <> s.name <> " — it reads " <> fmt s.db <> " dBFS right now"
                    else s.name <> " is on an interface that is not switched on")
      , HE.onClick \_ -> ArmOn (n + 1)
      ]
      [ HH.span [ HP.class_ (HH.ClassName "ws-chip-name") ] [ HH.text s.name ]
      , HH.span [ HP.class_ (HH.ClassName "ws-chip-db") ] [ HH.text (fmt s.db) ]
      ]

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
          , HH.text (" Captured from " <> srcName <> " as it comes"
              <> ", and folded to "
              <> (if Kind.foldsTo st.kind == ToMono then "mono" else "stereo")
              <> " on the way to a card"
              <> (if Kind.voicesOn st.kind == 2
                    then " — where it takes two of the four voices."
                    else "."))
          ]
      , HH.div [ HP.class_ (HH.ClassName "ws-actions") ]
          [ if st.armed || writing || listening
              then HH.button
                     [ HP.class_ (HH.ClassName "ws-big is-stop"), HE.onClick \_ -> Close ]
                     [ HH.text (if writing then "Stop" else "Cancel") ]
              else armRow
          , HH.span [ HP.class_ (HH.ClassName "ws-state") ]
              [ HH.text
                  (if writing then "recording — " <> elapsed <> " s"
                   else if listening then Kind.prompt st.kind
                   else maybe "" (\l -> if l.layers > 0
                                          then "captured " <> fmt l.loopSecs <> " s"
                                          else "ready") lp) ]
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
          , dividerRow
          , HH.div [ HP.class_ (HH.ClassName "ws-grid") ]
              (Array.mapWithIndex tile st.regions)
          , sendRow
          ]

  -- | **The algorithms, by name.**
  -- |
  -- | Not settings of one detector — different questions, and one of them is
  -- | right for material the others cannot see at all. Naming them is the
  -- | whole interface: press one, and the tiles underneath say within a second
  -- | whether it was the right question.
  dividerRow
    | not hasTake = HH.text ""
    | otherwise =
        HH.div [ HP.class_ (HH.ClassName "ws-dividers") ]
          [ HH.span [ HP.class_ (HH.ClassName "ws-arm-label") ] [ HH.text "Divide" ]
          , HH.div [ HP.class_ (HH.ClassName "ws-chips") ]
              (map dividerBtn (Divider.all st.equalN))
          , if Divider.needsCount st.divider
              then HH.label [ HP.class_ (HH.ClassName "ws-field is-tight") ]
                     [ HH.span_ [ HH.text "pieces" ]
                     , HH.input
                         [ HP.type_ HP.InputNumber, HP.value (show st.equalN)
                         , HP.min 2.0, HP.max 128.0
                         , HE.onValueInput SetEqualN ]
                     ]
              else HH.text ""
          , HH.span [ HP.class_ (HH.ClassName "ws-muted") ]
              [ HH.text (Divider.blurb st.divider) ]
          ]

  dividerBtn dv =
    HH.button
      [ HP.class_ (HH.ClassName ("ws-chip" <> if dv == st.divider then " on" else ""))
      , HP.disabled st.busy
      , HP.title (Divider.blurb dv)
      , HE.onClick \_ -> PickDivider dv
      ]
      [ HH.text (Divider.label dv) ]

  -- | Where the kept tiles go. Beside them, because it acts on them.
  sendRow
    | Array.null st.regions = HH.text ""
    | otherwise =
        HH.div [ HP.class_ (HH.ClassName "ws-send") ]
          [ HH.span [ HP.class_ (HH.ClassName "ws-arm-label") ] [ HH.text "Send to" ]
          , small "bank" st.bank SetBank
          , small "kit" (if st.kit == "" then st.name else st.kit) SetKit
          , HH.label [ HP.class_ (HH.ClassName "ws-field is-tight") ]
              [ HH.span_ [ HH.text "voice" ]
              , HH.select [ HE.onValueChange SetVoice ]
                  (map (\n -> HH.option
                          [ HP.value (show n), HP.selected (n == st.voice) ]
                          [ HH.text (show n
                              <> (if Kind.foldsTo st.kind /= ToMono
                                    then " + " <> show (n + 1) else "")) ])
                      [ 1, 2, 3, 4 ])
              ]
          , HH.button
              [ HP.class_ (HH.ClassName ("ws-plain is-go"
                  <> if isJust occupant then " is-replacing" else ""))
              , HP.disabled (st.cardBusy || Set.isEmpty st.keep)
              , HE.onClick \_ -> SendToCard
              ]
              [ HH.text (case occupant of
                  Nothing -> show (Set.size st.keep) <> " to the kit"
                  Just _ -> "replace with " <> show (Set.size st.keep)) ]
          -- **Say what is about to be destroyed, before it is.**
          --
          -- A kit's voice is an address, and sending to one that is taken
          -- replaces what is there. That happened four times in a row without
          -- a word being said, because the kit name stuck to the first take
          -- and every later send addressed the same slot. The samples all
          -- reached the disk; only the card's record of them collided. Naming
          -- the occupant costs one line and makes the whole class of mistake
          -- visible at the moment it can still be avoided.
          , case occupant of
              Nothing -> HH.text ""
              Just r ->
                HH.span [ HP.class_ (HH.ClassName "ws-warn") ]
                  [ HH.text ("voice " <> show st.voice <> " of this kit already \
                             \holds " <> r.set <> " — sending puts this in its place") ]
          ]

  -- | What already sits at the address this send would write to — and only
  -- | when it is something else, since re-sending the same set is a refresh
  -- | rather than a loss.
  occupant =
    let kitName = if st.kit == "" then st.name else st.kit
        setName = if st.name == "" then "set" else st.name
    in do
      v <- st.cardView
      Array.find
        (\r -> r.bank == st.bank && r.kit == kitName
                 && r.voice == st.voice && r.set /= setName)
        v.rows

  small lbl v act =
    HH.label [ HP.class_ (HH.ClassName "ws-field is-tight") ]
      [ HH.span_ [ HH.text lbl ]
      , HH.input [ HP.type_ HP.InputText, HP.value v, HE.onValueInput act ]
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

  -- | **The card, which is a description until you ask for it.**
  -- |
  -- | Writing it is a compile and never an edit in place, so what lands on the
  -- | SD card is always something you could have read first — and the three
  -- | rules the module fails silently on stay enforced in one place, by the
  -- | compiler, whose objections are shown here rather than restated.
  cardView =
    HH.section [ HP.class_ (HH.ClassName "ws-card") ]
      [ HH.h2_ [ HH.text "The card, so far" ]
      , case st.cardView of
          Nothing -> HH.p [ HP.class_ (HH.ClassName "ws-muted") ] [ HH.text "…" ]
          Just v
            | Array.null v.rows ->
                HH.p [ HP.class_ (HH.ClassName "ws-muted") ]
                  [ HH.text "Nothing on it yet. Record something, keep the ones you \
                            \meant, and send them to a voice. It is kept on disk as \
                            \you build it; no card need be mounted until you write." ]
            | otherwise ->
                HH.div_
                  [ HH.table [ HP.class_ (HH.ClassName "ws-table") ]
                      [ HH.thead_ [ HH.tr_ (map (\h -> HH.th_ [ HH.text h ])
                          [ "bank", "kit", "voice", "holds" ]) ]
                      , HH.tbody_ (map row v.rows)
                      ]
                  , writeRow v
                  , if v.plan == "" then HH.text ""
                    else HH.pre [ HP.class_ (HH.ClassName ("ws-plan" <> if v.ok then "" else " is-bad")) ]
                           [ HH.text v.plan ]
                  ]
      ]

  row r =
    HH.tr_
      [ HH.td_ [ HH.text r.bank ]
      , HH.td_ [ HH.text r.kit ]
      , HH.td_ [ HH.text (show r.voice <> (if r.stereo then " + " <> show (r.voice + 1) else "")) ]
      , HH.td_
          [ HH.text (r.set <> " — "
              <> (if r.slicer > 0
                    then "one file, SLICER " <> show r.slicer
                    else show r.count
                           <> (if r.count == 1 then " sample" else " samples"))
              <> (if r.stereo then ", stereo" else "")) ]
      ]

  writeRow v =
    HH.div [ HP.class_ (HH.ClassName "ws-send") ]
      [ HH.span [ HP.class_ (HH.ClassName "ws-arm-label") ] [ HH.text "Write to" ]
      , if Array.null v.cards
          then HH.span [ HP.class_ (HH.ClassName "ws-muted") ]
                 [ HH.text "no Rample card is mounted — everything above is safe on \
                           \disk; mount one when you want it written" ]
          else HH.div [ HP.class_ (HH.ClassName "ws-chips") ]
                 (map (\c -> HH.button
                         [ HP.class_ (HH.ClassName "ws-chip is-arm")
                         , HP.disabled (st.cardBusy || not v.ok)
                         , HP.title ("compile the manifest onto " <> c)
                         , HE.onClick \_ -> WriteCard c
                         ]
                         [ HH.text c ]) v.cards)
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
