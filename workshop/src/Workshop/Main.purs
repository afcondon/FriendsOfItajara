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
import Data.Maybe as Maybe
import Data.Number as Number
import Data.String as String
import Data.String.Common (joinWith)
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
import Workshop.Rig as Rig
import Workshop.Encoding as Encoding
import Workshop.Schedule as Schedule
import Workshop.Sweep as Sweep
import Workshop.SweepView as SweepView

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
  , layerMode :: String
  , equalN :: Int
  -- | The virtual card, as the server flattens it, plus what `kit build` says
  -- | about it. Refreshed after anything that could change it.
  , cardView :: Maybe Http.CardView
  -- | Where a kept set is going: the bank, the kit, and which voice.
  , bank :: String
  , kit :: String
  , voice :: Int
  , cardBusy :: Boolean
  -- | **The sweep**: what to set, at how many points, and how to make a sound
  -- | at each of them. See `Workshop.Sweep` for why it is a table of values
  -- | rather than a set of curves.
  , sweep :: Sweep.Plan
  , sweepOpen :: Boolean
  -- | Which position the run is on, while it is running.
  , sweepAt :: Maybe Int
  -- | Held so the run can be stopped. A forked action rather than a blocking
  -- | one, because a loop that owns the handler for half a minute cannot be
  -- | interrupted by pressing anything.
  , sweepFork :: Maybe H.ForkId
  , midiPorts :: Array String
  -- | Which parameter's values are open for editing point by point. UI state,
  -- | so it is here and not in the plan: a plan that remembered which drawer
  -- | was open would put that in the file it is saved to.
  , sweepEdit :: Maybe Int
  -- | The take on show was made by a sweep, so the measurements are worth
  -- | reading as a set rather than one at a time.
  , swept :: Boolean
  -- | **When each trigger of the last run landed**, in seconds into the take.
  -- |
  -- | Read from the daemon's own `recFrames` at the instant of each trigger,
  -- | so it is take time and not page time. Empty for a take that was played
  -- | rather than run, and that emptiness is the switch: with a schedule the
  -- | take divides at its own boundaries, without one `msm` goes looking. See
  -- | `Workshop.Schedule` for why that difference matters more at 192 hits
  -- | than at twelve.
  , schedule :: Array Number
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
  | SendToCard Boolean
  | SetLayerMode String
  | WriteCard String
  | Play Int
  | HoverPlay Int
  | SetHoverPlays Boolean
  | OpenSweep Boolean
  | SweepMsg Sweep.Msg
  | OpenParam (Maybe Int)
  | RunSweep Int
  | StopSweep
  | SetLead String

component :: forall q i o m. MonadAff m => H.Component q i o m
component = H.mkComponent
  { initialState: \_ ->
      { looper: Nothing, kind: Kind.DrumHits, bars: 1
      , armed: false, name: "", log: []
      , peaks: Nothing, regions: [], keep: Set.empty, busy: false
      , hoverPlays: false, playing: Nothing, showing: "", waiting: false
      , minGap: 300.0, divider: Divider.Attacks, equalN: 16, mine: false, kitMine: false, layerMode: ""
      , cardView: Nothing, bank: "WORKSHOP", kit: "", voice: 1, cardBusy: false
      , sweep: Sweep.emptyPlan, sweepOpen: false, sweepAt: Nothing
      , sweepFork: Nothing, midiPorts: [], swept: false, sweepEdit: Nothing
      , schedule: [] }
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
    -- The sweep plan as it was left. See `Workshop.Sweep.restore` — a run,
    -- listen, bend, run again loop cannot survive a page that forgets between
    -- runs, and reloading to pick up a fix is exactly when it forgets.
    pl <- liftEffect (Sweep.restore Sweep.emptyPlan)
    H.modify_ _ { name = n, sweep = pl }
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
    -- The MIDI ports, while the sweep is open. Read rather than asked for: the
    -- asking is a permission prompt that may sit unanswered for as long as it
    -- likes, and this is how its answer arrives without anything waiting on it.
    st3 <- H.get
    when st3.sweepOpen do
      ps <- liftEffect Rig.ports
      when (ps /= st3.midiPorts) do
        H.modify_ _ { midiPorts = ps }
        unless (Array.null ps) $ H.modify_
          (note (show (Array.length ps) <> " MIDI ports"))
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
  SetLayerMode m -> H.modify_ _ { layerMode = m }
  -- | `append` is the whole of the two-axis change at this end: the same set,
  -- | the same voice, and a choice about whether it stands beside what is
  -- | there or in its place.
  SendToCard append -> do
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
              , append
              , layerMode: st.layerMode
              , regions: keptRegions })))
        case r of
          Left e -> H.modify_ (note (Aff.message e) <<< _ { cardBusy = false })
          -- **A send establishes the kit, and later takes join it.**
          --
          -- The kit name used to follow the take name, so every take proposed
          -- a fresh kit and four takes meant for one voice became four kits on
          -- four voices. Adopting the name that was just used makes the kit a
          -- place you are working, which is what a kit is; the free-voice
          -- search and the add-as-layer verb are what keep that from
          -- destroying anything, and they did not exist when the name was
          -- first made to follow.
          Right w -> H.modify_ \s ->
            (note (lastLine w.output) s)
              { cardBusy = false
              , kit = if s.kit == "" then setName else s.kit
              , kitMine = true
              }
        handleAction RefreshCard
  Analyse -> analyse true
  Divide -> analyse false
  SetGap v -> do
    H.modify_ \s -> s { minGap = fromMaybe s.minGap (Number.fromString v) }
    st <- H.get
    when (st.showing /= "") (analyse false)
  -- | The lead, and re-divide at once. It costs one pass over a take already
  -- | on disk, and the whole reason it is a knob rather than a constant is
  -- | that you set it by looking at what comes back.
  SetLead v -> do
    H.modify_ \s -> s { sweep = Sweep.update (Sweep.SetLead v) s.sweep }
    st <- H.get
    liftEffect (Sweep.remember st.sweep)
    when (st.showing /= "" && not (Array.null st.schedule)) (analyse false)
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
  ArmOn src -> armOn true src
  -- | **The sweep modal**, and asking the browser for MIDI when it opens.
  -- |
  -- | Asked once, on opening, rather than at Run: `requestMIDIAccess` prompts
  -- | the first time, and a permission dialog appearing in the middle of a run
  -- | would cost the take.
  OpenSweep b -> do
    H.modify_ _ { sweepOpen = b }
    when b do
      r <- H.liftAff (attempt (toAffE Rig.openMidi))
      case r of
        Left e -> H.modify_ (note ("no MIDI: " <> Aff.message e))
        Right ports -> do
          H.modify_ _ { midiPorts = ports }
          -- Empty here is the ORDINARY first answer, not a failure: the
          -- permission prompt is still up. The poll fills the list when it is
          -- answered, so this says "not yet" rather than "not at all".
          when (Array.null ports) $ H.modify_
            (note "no MIDI ports yet — allow MIDI if Chrome asks; CV is unaffected")
  OpenParam i -> H.modify_ _ { sweepEdit = i }
  SweepMsg m -> do
    H.modify_ \s -> s { sweep = Sweep.update m s.sweep }
    st <- H.get
    liftEffect (Sweep.remember st.sweep)
  -- | **Arming and running are one gesture**, for the same reason arming and
  -- | choosing the input are: the run has to land inside a take, and a Run
  -- | button that assumed something was already recording would fail silently
  -- | by producing sound nobody caught.
  RunSweep src -> do
    st <- H.get
    case st.sweepFork of
      Just _ -> H.modify_ (note "a sweep is already running")
      Nothing -> do
        -- Open, not level-armed: see `armOn`. The schedule knows when the
        -- first hit happens, so nothing needs to detect it.
        armOn false src
        H.modify_ _ { swept = false, sweepAt = Nothing, schedule = [] }
        fid <- H.fork runSweep
        H.modify_ _ { sweepFork = Just fid }
  StopSweep -> do
    st <- H.get
    for_ st.sweepFork H.kill
    restCv
    H.modify_ (note "sweep stopped" <<< _ { sweepFork = Nothing, sweepAt = Nothing })
    handleAction Close
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

-- | **Arm the scratch loop on an input, and start recording into it.**
-- |
-- | `levelArmed` is the whole of the difference between a take you play and a
-- | take the rig plays.
-- |
-- | Playing it yourself, `r` has to wait for a sound: you cannot press Record
-- | and pick up a stick in the same instant, and the daemon reaches back past
-- | the crossing so the attack that triggered it is inside the take.
-- |
-- | **A sweep must not wait for a sound**, and this cost a run to learn. The
-- | first swept take held five hits out of twelve: position 1 is the bottom of
-- | the range, the BIA at Morph 0 is barely audible, and the level arm did not
-- | trip until the sweep had climbed loud enough — around position eight. The
-- | measurements say so plainly, peak 0.16 rising to 0.70 and brightness 19 to
-- | 108 across what survived.
-- |
-- | And the fix is the rule this whole project keeps rediscovering: **declared
-- | beats inferred.** The take's start is not something to detect, because the
-- | page is the thing about to make the sound and already knows when. Inferring
-- | it from level throws away information we hold, and throws it away in the
-- | one direction that is silent — a sweep whose quiet end is the interesting
-- | end loses exactly the part it was made for.
armOn :: forall o m. MonadAff m => Boolean -> Int -> H.HalogenM State Action () o m Unit
armOn levelArmed src = do
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
  send (LevelArm levelArmed)
  send Record
  -- A fresh name unless you gave it one that has not been used yet. This is
  -- the whole of the overwrite fix: the export is `exl <name>`, so a name
  -- that already belongs to a take on disk is a name that destroys it.
  when (wantsAName st) do
    n <- liftEffect (slugFor st.kind)
    H.modify_ _ { name = n, kit = if st.kitMine then st.kit else "" }
  -- Not swept until something sweeps it. Left set, the declared-against-found
  -- check would go on comparing every later take by hand against a position
  -- count that has nothing to do with it.
  -- The schedule belongs to a run, and this take is not that run until one
  -- starts. Left standing, a take played by hand would be divided at the
  -- boundaries of the sweep before it — silently, and at plausible-looking
  -- times.
  H.modify_ (note (if levelArmed then Kind.prompt st.kind else "recording — the run starts in a moment")
    <<< _ { armed = true, swept = false, schedule = [] })

-- | **The run.**
-- |
-- | Set, settle, strike, wait — twelve times, inside one take. Nothing here is
-- | clever and nothing needs to be: the divider finds the real onsets
-- | afterwards, so a few milliseconds of jitter between the schedule and the
-- | audio costs nothing at all. The only interval that has to be right is
-- | `settleMs`, and that is because a parameter still on its way when the
-- | trigger lands makes position 7 a blend of 6 and 7 — an error the pictures
-- | cannot show, since a blend looks exactly like a value.
-- |
-- | Forked, so Stop is a button rather than a wish.
runSweep :: forall o m. MonadAff m => H.HalogenM State Action () o m Unit
runSweep = do
  st <- H.get
  let p = st.sweep
  -- The recording is already open, so this is real silence at the head of the
  -- take rather than a wait for something to start it — which is what `by
  -- attack` wants in front of the first onset anyway.
  H.liftAff (delay (Milliseconds 400.0))
  for_ (Sweep.steps p) \s -> do
    H.modify_ _ { sweepAt = Just s.index }
    unless (Array.null s.cv) $ void $
      H.liftAff (attempt (toAffE (Rig.setCv { set: s.cv })))
    when (p.port /= "") $ liftEffect $ for_ s.cc \c ->
      Rig.sendCc { port: p.port, channel: c.channel, cc: c.cc, value: c.value }
    H.liftAff (delay (Milliseconds (Int.toNumber p.settleMs)))
    -- **Where in the take this hit is about to be**, asked immediately before
    -- the trigger rather than after it: everything between here and the pulse
    -- is a few microseconds of arithmetic, where everything after it is a
    -- round trip of unknown length. See `Workshop.Schedule`.
    mk <- liftEffect (Schedule.at scratch)
    for_ mk \t -> H.modify_ \s0 -> s0 { schedule = Array.snoc s0.schedule t }
    for_ p.trigger.gate \b -> void $
      H.liftAff (attempt (toAffE (Rig.pulse
        { bus: b, level: p.trigger.gateLevel, ms: p.trigger.ms })))
    when (p.port /= "") $ for_ p.trigger.note \n -> liftEffect $
      Rig.sendNote { port: p.port, channel: p.trigger.channel, note: n
                   , velocity: p.trigger.velocity, ms: p.trigger.ms }
    H.liftAff (delay (Milliseconds (Int.toNumber p.spacingMs)))
  -- The last hit gets the same gap as the others and then a little more, so
  -- that closing the take is never the thing that ends its decay. A final
  -- sample that is short because the recording stopped is indistinguishable
  -- from one that is short because the sound was.
  H.liftAff (delay (Milliseconds 300.0))
  restCv
  H.modify_ (note ("swept " <> show (Encoding.total p.extent) <> " samples")
    <<< _ { sweepAt = Nothing, sweepFork = Nothing, sweepOpen = false, swept = true })
  handleAction Close

-- | **Put every bus this sweep touched back to nothing.**
-- |
-- | A voltage is not a message; it stays where it was left. Ending a run at
-- | position twelve and walking away leaves the instrument held at the top of
-- | the sweep, which is a thing you then hear in every unrelated patch and
-- | blame on something else.
restCv :: forall o m. MonadAff m => H.HalogenM State Action () o m Unit
restCv = do
  st <- H.get
  let buses = Array.nub
        (Array.mapMaybe _.cv st.sweep.params
           <> Array.catMaybes [ st.sweep.trigger.gate ])
  unless (Array.null buses) $ void $ H.liftAff
    (attempt (toAffE (Rig.setCv { set: map (\b -> { bus: b, level: 0.0 }) buses })))

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
          -- **The run divides its own take.** Empty for anything played by
          -- hand, which is when the detector is the only thing that could
          -- know. See `Workshop.Schedule` for the lead, which is the one
          -- number the schedule cannot supply itself.
          declared = Schedule.slots
                       (Int.toNumber st.sweep.leadMs / 1000.0)
                       st.schedule
      r <- H.liftAff (attempt (toAffE (Http.divisions
            { take: takeName
            , as: Kind.material st.kind
            , by: Divider.name st.divider
            , minGap: st.minGap
            , regions: declared })))
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
              -- **Propose somewhere free to put it.**
              --
              -- A kit name you typed used to stick to voice 1 for ever, so
              -- every later take aimed at the slot holding the last one. The
              -- warning made that visible, which is not the same as making it
              -- right: the default should be a place the take can go, and only
              -- then a warning for when you deliberately aim elsewhere.
              -- **Stay where this take could be a layer; move where it could not.**
              --
              -- Two different intentions wear the same gesture. Filling a kit
              -- wants the next free voice; stacking layers wants to stay put
              -- so `add as layer` is on offer. The difference is whether what
              -- is already there is the same *shape* — same material, same
              -- channels, same division — because that is exactly the
              -- condition under which it could be a layer at all.
              H.modify_ \s ->
                if couldLayer s then s
                else case freeVoice s of
                  Just v -> s { voice = v }
                  -- All four spoken for, so the kit is full rather than the
                  -- voice taken, and the answer is a new kit.
                  Nothing -> s { kit = "", kitMine = false }
              H.modify_ (note
                (show n <> (if n == 1 then " division" else " divisions")
                  <> " over " <> fmt d.secs <> " s"
                  <> (if d.divides then "" else " (this kind is kept whole)")))

-- | **Could this take stand beside what is on the chosen voice?**
-- |
-- | Only if it is the same shape. SLICER divides whatever is playing, so every
-- | layer on a voice is cut by the same division and two layers wanting
-- | different ones cannot both be right; and stereo claims the voice after it,
-- | so it cannot sit beside mono. Where all that matches, staying put is
-- | almost certainly what was meant — nobody records a second twelve-note
-- | chromatic run to put it somewhere else.
couldLayer :: State -> Boolean
couldLayer st = case occupantOf st of
  Nothing -> false
  Just r ->
    r.kind == Kind.name st.kind
      && r.stereo == (Kind.foldsTo st.kind /= ToMono)
      && (r.slicer > 0) == (Kind.joins st.kind || isEqual st.divider)
      && Array.length r.sets < 12

-- | What sits on the voice this send is aimed at, whatever it is.
occupantOf :: State -> Maybe Http.CardRow
occupantOf st =
  let kitName = if st.kit == "" then st.name else st.kit
  in do
    v <- st.cardView
    Array.find
      (\r -> r.bank == st.bank && r.kit == kitName && r.voice == st.voice)
      v.rows

-- | **The lowest voice this take could go on without displacing anything.**
-- |
-- | A stereo sample occupies the voice after it too, so it needs a pair — and
-- | a stereo take on voice 4 has nowhere to put its right channel, which is
-- | why the search stops at 3 for those. Nothing means the kit is full.
freeVoice :: State -> Maybe Int
freeVoice st =
  let kitName = if st.kit == "" then st.name else st.kit
      wide = Kind.foldsTo st.kind /= ToMono
      taken v = case st.cardView of
        Nothing -> false
        Just cv -> Array.any
          (\r -> r.bank == st.bank && r.kit == kitName
                   && (r.voice == v || (r.stereo && r.voice + 1 == v)))
          cv.rows
      fits v = not (taken v) && (not wide || not (taken (v + 1)))
  in Array.find fits (if wide then [ 1, 3 ] else [ 1, 2, 3, 4 ])

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
  -- The bench wants width the rest of the page does not, so the container
  -- widens for it rather than the bench breaking out of the container.
  HH.div [ HP.class_ (HH.ClassName ("ws" <> if st.sweepOpen then " is-wide" else "")) ]
    [ HH.header [ HP.class_ (HH.ClassName "ws-head") ]
        [ HH.h1_ [ HH.text "Workshop" ]
        , HH.span [ HP.class_ (HH.ClassName "ws-sub") ]
            [ HH.text "record material, divide it, put it on a card" ]
        , connection
        ]
    , if st.sweepOpen then sweepBench else HH.text ""
    , if st.sweepOpen then HH.text "" else recordBox
    , if st.sweepOpen then HH.text "" else caught
    , if st.sweepOpen then HH.text "" else cardView
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

  running = Maybe.isJust st.sweepFork

  runChip n src =
    HH.button
      [ HP.class_ (HH.ClassName ("ws-chip is-arm" <> if src.available then "" else " off"))
      , HP.disabled (not src.available)
      , HP.title ("arm on " <> src.name <> " and start the run")
      , HE.onClick \_ -> RunSweep (n + 1)
      ]
      [ HH.span [ HP.class_ (HH.ClassName "ws-chip-name") ] [ HH.text src.name ]
      , HH.span [ HP.class_ (HH.ClassName "ws-chip-db") ] [ HH.text (fmt src.db) ]
      ]

  -- | **The sweep, as a view rather than a dialog.**
  -- |
  -- | It was a modal first, and eight parameters at sixteen positions is a
  -- | mixing desk — a desk in a dialog is a desk you cannot work. So the page
  -- | swaps: while you are describing a run you are not looking at a recording,
  -- | and the two want different verbs on screen anyway.
  -- |
  -- | Not a second HTML page, which was the other option considered. The point
  -- | was room, and a second page would have meant a second Halogen app and a
  -- | second socket to the daemon to get it.
  sweepBench =
    HH.section [ HP.class_ (HH.ClassName "ws-bench") ]
      [ HH.header [ HP.class_ (HH.ClassName "ws-modhead") ]
          [ HH.h2_ [ HH.text "Sweep" ]
          , HH.span [ HP.class_ (HH.ClassName "ws-sub") ]
              [ HH.text "a curve for every parameter you want to move, and the \
                        \destination decides the shape of the set" ]
          , HH.button
              [ HP.class_ (HH.ClassName "ws-plain")
              , HP.disabled running
              , HE.onClick \_ -> OpenSweep false
              ]
              [ HH.text "back" ]
          ]
      , SweepView.body
          { ports: st.midiPorts
          , open: st.sweepEdit
          , plan: st.sweep
          , msg: SweepMsg
          , openParam: OpenParam
          }
      , HH.div [ HP.class_ (HH.ClassName "ws-send") ]
          ( [ HH.span [ HP.class_ (HH.ClassName "ws-arm-label") ]
                [ HH.text (if running then "Running" else "Run on") ] ]
              <> runOrStop
          )
      ]

  runOrStop
    | running =
        [ HH.span [ HP.class_ (HH.ClassName "ws-state") ]
            [ HH.text ("position " <> show (maybe 0 (_ + 1) st.sweepAt)
                <> " of " <> show (Encoding.total st.sweep.extent)) ]
        , HH.button
            [ HP.class_ (HH.ClassName "ws-plain is-replacing")
            , HE.onClick \_ -> StopSweep
            ]
            [ HH.text "stop" ]
        ]
    | otherwise =
        [ HH.div [ HP.class_ (HH.ClassName "ws-chips") ]
            (maybe [ HH.text "no daemon" ]
              (\top -> Array.mapWithIndex runChip top.sources)
              st.looper)
        , HH.span [ HP.class_ (HH.ClassName "ws-muted") ]
            [ HH.text "pressing an input arms the take and starts the run, the \
                      \same gesture as recording by hand. The take closes itself \
                      \when the last position has sounded." ]
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
          -- | **Playing it yourself is one way to fill a take.** This is the
          -- | other: hand the schedule to the rig, and get a set that is even
          -- | where a hand cannot be even.
          , HH.button
              [ HP.class_ (HH.ClassName "ws-plain")
              , HP.disabled (st.armed || writing)
              , HP.title "drive the instrument through a set of positions and \
                         \record the result as one take"
              , HE.onClick \_ -> OpenSweep true
              ]
              [ HH.text "Sweep\x2026" ]
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
              , spread
              , declaredVsFound
              , if Array.null st.regions && hasTake && not st.busy
                  then HH.button [ HP.class_ (HH.ClassName "ws-plain"), HE.onClick \_ -> Analyse ]
                         [ HH.text "Divide it" ]
                  else HH.text ""
              , HH.button [ HP.class_ (HH.ClassName "ws-plain"), HE.onClick \_ -> KeepAll true ]
                  [ HH.text "Keep all" ]
              , HH.button [ HP.class_ (HH.ClassName "ws-plain"), HE.onClick \_ -> KeepAll false ]
                  [ HH.text "Keep none" ]
              , if not (Array.null st.schedule) then HH.text "" else
                HH.label [ HP.class_ (HH.ClassName "ws-quiet") ]
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
    -- The run's own boundaries, and no chooser: see `scheduled`.
    | not (Array.null st.schedule) = scheduled
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

  -- | **A run divides its own take**, so there is nothing here to choose.
  -- |
  -- | Shown in place of the dividers, because offering both would be offering
  -- | a way to throw the schedule away by accident. What is left is the one
  -- | number the schedule cannot supply: how long the sound takes to come
  -- | back. Set it by looking — the tiles show a clipped attack as a low peak
  -- | and a lead too long as silence at the head.
  scheduled =
    HH.div [ HP.class_ (HH.ClassName "ws-dividers") ]
      [ HH.span [ HP.class_ (HH.ClassName "ws-arm-label") ] [ HH.text "Divide" ]
      , HH.span [ HP.class_ (HH.ClassName "ws-chips") ]
          [ HH.span [ HP.class_ (HH.ClassName "ws-chip on") ] [ HH.text "by the schedule" ] ]
      , HH.label [ HP.class_ (HH.ClassName "ws-field is-tight") ]
          [ HH.span_ [ HH.text "lead ms" ]
          , HH.input
              [ HP.type_ HP.InputNumber, HP.value (show st.sweep.leadMs)
              , HP.min (-500.0), HP.max 2000.0
              , HP.title "how long after a trigger its sound is in the take"
              , HE.onValueChange SetLead ]
          ]
      , HH.span [ HP.class_ (HH.ClassName "ws-muted") ]
          [ HH.text (show (Array.length st.schedule)
              <> " triggers, timed by the daemon as it recorded them — nothing was \
                 \detected, so nothing could be missed") ]
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
          -- **Two verbs where there was one.**
          --
          -- A voice used to hold one set, so sending could only mean "put it
          -- here". It can now hold a stack, and the two things you might mean
          -- are opposites: stand beside what is there, or take its place. The
          -- additive one is offered first and plainly; the destructive one has
          -- to be aimed at.
          , case occupant of
              Nothing ->
                HH.button
                  [ HP.class_ (HH.ClassName "ws-plain is-go")
                  , HP.disabled (st.cardBusy || Set.isEmpty st.keep)
                  , HE.onClick \_ -> SendToCard false
                  ]
                  [ HH.text (show (Set.size st.keep) <> " to the kit") ]
              Just r ->
                HH.div [ HP.class_ (HH.ClassName "ws-twoverbs") ]
                  [ HH.button
                      [ HP.class_ (HH.ClassName "ws-plain is-go")
                      , HP.disabled (st.cardBusy || Set.isEmpty st.keep)
                      , HP.title "the layer selector picks between them"
                      , HE.onClick \_ -> SendToCard true
                      ]
                      [ HH.text ("add as layer "
                          <> show (Array.length r.sets + 1)) ]
                  , HH.button
                      [ HP.class_ (HH.ClassName "ws-plain is-replacing")
                      , HP.disabled (st.cardBusy || Set.isEmpty st.keep)
                      , HE.onClick \_ -> SendToCard false
                      ]
                      [ HH.text "replace" ]
                  ]
          -- **Say what is about to be destroyed, before it is.**
          --
          -- A kit's voice is an address, and sending to one that is taken
          -- replaces what is there. That happened four times in a row without
          -- a word being said, because the kit name stuck to the first take
          -- and every later send addressed the same slot. The samples all
          -- reached the disk; only the card's record of them collided. Naming
          -- the occupant costs one line and makes the whole class of mistake
          -- visible at the moment it can still be avoided.
          -- **How the layer selector moves.** Only worth asking once a voice
          -- holds more than one thing to choose between — and it is the whole
          -- reason to use layers rather than slices, since these are the modes
          -- where the module decides for itself.
          , case occupant of
              Just r | Array.length r.sets >= 1 ->
                HH.label [ HP.class_ (HH.ClassName "ws-field is-tight") ]
                  [ HH.span_ [ HH.text "picked by" ]
                  , HH.select [ HE.onValueChange SetLayerMode ]
                      (map (\m -> HH.option
                              [ HP.value m
                              , HP.selected (m == (if st.layerMode == "" then r.mode else st.layerMode)) ]
                              [ HH.text m ])
                          [ "manual", "velocity", "random", "cyclic" ])
                  ]
              _ -> HH.text ""
          , case occupant of
              Nothing -> HH.text ""
              Just r ->
                HH.span [ HP.class_ (HH.ClassName "ws-warn") ]
                  [ HH.text ("voice " <> show st.voice <> " holds "
                      <> joinWith ", " r.sets
                      <> (if r.slicer > 0 then " in " <> show r.slicer <> " slots" else "")
                      <> " — add stands beside them, replace puts this in their place") ]
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

  -- | **Did anything actually change across these?**
  -- |
  -- | Two ratios, largest over smallest, on level and on **spectral tilt** —
  -- | harmonic richness, the RMS surviving a high-pass over the RMS of the
  -- | whole.
  -- |
  -- | Not zero-crossing rate, which was here first and was the wrong witness.
  -- | `zcr` reads the fundamental: a Basimilus morph blends sine to square and
  -- | every one of those crosses zero twice a cycle, so measured across the
  -- | whole of that parameter `zcr` moved **17%** while the sound changed
  -- | completely. `tilt` moved **3.03x** over the same sweep and showed the
  -- | parameter's actual shape — a wrap at 3.3 V and a hard ceiling at 5 V.
  -- |
  -- | Ratios rather than a verdict, except at the one end where a verdict is
  -- | safe: if BOTH are flat then nothing moved, and that is worth saying
  -- | loudly, because twelve identical tiles are what a run that never left
  -- | this page looks like.
  spread
    | Array.length st.regions < 3 = HH.text ""
    | otherwise =
        let
          rng f =
            let
              xs = Array.sort (map f st.regions)
              lo = fromMaybe 0.0 (Array.head xs)
              hi = fromMaybe 0.0 (Array.last xs)
            in
              if lo <= 0.0 then 0.0 else hi / lo
          rp = rng _.peak
          rz = rng _.tilt
          flat = rp > 0.0 && rz > 0.0 && rp < 1.1 && rz < 1.1
        in
          HH.span [ HP.class_ (HH.ClassName (if flat then "ws-warn" else "ws-muted")) ]
            [ HH.text
                (if flat
                   then "these " <> show (Array.length st.regions)
                          <> " are within a few percent of each other on level AND \
                             \brightness — whatever was meant to change did not reach \
                             \the instrument"
                   else "level ×" <> fmt rp <> ", brightness ×" <> fmt rz) ]

  -- | You said how many positions; the detector found this many. Worth saying
  -- | now, while the divider and the gap are one press away, rather than on the
  -- | module.
  declaredVsFound
    | not st.swept = HH.text ""
    | Array.null st.regions = HH.text ""
    -- Under a schedule the two counts are the same object, so a mismatch here
    -- would mean the take is shorter than the run — which `msm` reports by
    -- dropping the regions that fall past its end.
    | not (Array.null st.schedule) =
        if Array.length st.regions == Array.length st.schedule then HH.text ""
        else HH.span [ HP.class_ (HH.ClassName "ws-warn") ]
               [ HH.text ("the run made " <> show (Array.length st.schedule)
                   <> " hits and the take holds " <> show (Array.length st.regions)
                   <> " — the recording ended before the run did") ]
    | Array.length st.regions == Encoding.total st.sweep.extent = HH.text ""
    | otherwise =
        HH.span [ HP.class_ (HH.ClassName "ws-warn") ]
          [ HH.text ("you swept " <> show (Encoding.total st.sweep.extent)
              <> " samples and this divided into " <> show (Array.length st.regions)
              <> " — try another divider, or a wider gap, before sending it") ]

  -- The loudest and the brightest in this take, so a tile is read against its
  -- own neighbours rather than against an absolute nobody carries in their head.
  loudest = fromMaybe 0.0 (Array.last (Array.sort (map _.peak st.regions)))
  brightest = fromMaybe 0.0 (Array.last (Array.sort (map _.tilt st.regions)))

  meter r =
    HH.div [ HP.class_ (HH.ClassName "ws-meter") ]
      [ bar "level" (r.peak / max 1.0e-9 loudest)
          (fmt (r.peak * 100.0) <> "% of the loudest here")
      , bar "bright" (r.tilt / max 1.0e-9 brightest)
          (fmt (r.tilt * 100.0) <> "% of its energy above the high-pass; "
             <> show (Int.round r.zcr) <> " zero crossings a second")
      ]

  bar k v title =
    HH.div [ HP.class_ (HH.ClassName ("ws-bar is-" <> k)), HP.title (k <> " — " <> title) ]
      [ HH.div
          [ HP.class_ (HH.ClassName "ws-bar-fill")
          , HP.attr (HH.AttrName "style")
              ("width: " <> show (Int.round (100.0 * clamp 0.0 1.0 v)) <> "%")
          ]
          []
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
        , meter r
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
