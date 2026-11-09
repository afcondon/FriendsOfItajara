-- | **The Loop page: play the looper from Quadrat.** The loops, their layers,
-- | the Edit panel — the same page PWYF draws, from the same module
-- | (`Itajara.Surface.Looper.Page`), driving the daemon through the same
-- | machine the footswitches use.
-- |
-- | ## One meaning, reached three ways
-- |
-- | `Data.Looper.Machine.perform` is the only place that decides what a press
-- | means. PWYF reaches it from the page, the MC6 and the Twister; the looper's
-- | own process (`Itajara.Controllers.Main`) from the MC6 and the Twister; and
-- | this from the page. Every handler below ends in `perform` (or, for the
-- | board simulator, `Switchboard.act`, which is `perform` behind the MC6's
-- | layout) and every resulting `Machine.Action` goes through `runAction`, the
-- | only writer to the socket in this module. A button that wrote a verb
-- | itself would be a second meaning table; that has been built and removed
-- | twice in PWYF.
-- |
-- | ## The Twister is followed, never held
-- |
-- | The looper's own process holds the Twister on its banks and PWYF holds it
-- | otherwise; a third holder would be the two-owners problem Helm's hub
-- | exists to end. So this page only *watches* what the holder sends the
-- | Twister (`watchPort`, `onSent`), reads the page back from the pager's
-- | colour (`Data.Looper.Twister.pageShown`), and, when a tab is clicked, asks
-- | the holder to turn (`askHolder`). It opens no Web MIDI of its own, claims
-- | nothing, and sends nothing to the Twister or the MC6.
-- |
-- | ## Polymorphic in the state, so Main stays the owner
-- |
-- | `Fields` is a row: Quadrat's `State` carries it, and the handlers here are
-- | written over any record that does, so they cannot reach into the rest of
-- | Quadrat's state even by accident.
module Quadrat.Loop
  ( Fields
  , Follow
  , Msg(..)
  , initial
  , start
  , poll
  , handle
  , view
  ) where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (for_, traverse_)
import Data.Int as Int
import Data.Looper.Banks as LoopBanks
import Data.Looper.Layout as Layout
import Data.Looper.Machine as Machine
import Data.Looper.PedalCC as PedalCC
import Data.Looper.Press as Press
import Data.Looper.Switchboard as Switchboard
import Data.Looper.Twister as LoopTwister
import Data.Looper.TwisterHolder as TwisterHolder
import Data.Looper.Verb as Verb
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), isJust, maybe)
import Data.Number as Number
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Foreign.LooperSocket (LooperState, Peaks, SocketStatus)
import Foreign.LooperSocket as Socket
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Subscription as HS
import Helm.Twister as HelmTwister
import Helm.WebMIDI (MIDIAccess)
import Helm.WebMIDI as MIDI
import Itajara.Surface.Looper.Page as Page
import Quadrat.Layout as PageLayout
import Itajara.Surface.Looper.Statement as LooperStatement
import Halogen.Widgets.Statement as Statement
import Itajara.Surface.Looper.TwisterCard as TwisterCard
import Itajara.Surface.Looper.TwisterMap as TwisterMap
import Itajara.Surface.Looper.Types (LogLine, LooperPanel(..), pushLog)

-- | Where this page stands with the Twister.
-- |
-- | An ADT rather than a pair of maybes because "not asked yet", "asked and
-- | waiting", "the hub said no" and "the hub has no Twister" want four
-- | different sentences on the card, and a page that said nothing in two of
-- | them would be one more card believed when it should not be.
data Follow
  -- | The Loop page has not been opened, so the hub has not been asked.
  = NotAsked
  -- | Asked; the hub has not answered.
  | Asking
  -- | Through the hub. `port` is the Twister output being watched, `Nothing`
  -- | while the hub has no Twister (it is looked for again when ports change).
  | Following { access :: MIDIAccess, port :: Maybe String }
  -- | The hub could not be reached, and why.
  | NotFollowed String

-- | Everything the Loop page reads and writes, as a row Quadrat's `State`
-- | carries. Named as PWYF names them, so `Page.State` is satisfied as is.
type Fields r =
  ( looper :: Maybe LooperState
  , looperStatus :: SocketStatus
  , looperSnapshotAge :: Number
  -- | The loop in hand: here at once on a press, and adopted from the
  -- | daemon's `selected` when that changes (another surface chose).
  , looperFocus :: Int
  , looperSelectedSeen :: Maybe Int
  , looperPanel :: Maybe LooperPanel
  -- | A loop's picture for the Edit panel: only peaks answers about a loop
  -- | (`loop >= 0`) land here; a capture's (`loop == -1`) are Quadrat's own.
  , looperPeaks :: Maybe Peaks
  -- | What the panel last asked a picture of, so it asks once per change.
  , looperPeaksKey :: String
  -- | A slider a hand is on, which owns its value until released.
  , looperEditLocal :: Map String Int
  , looperBankShown :: Maybe LoopBanks.BankSlot
  , looperLog :: Array LogLine
  , looperAckSeq :: Int
  , twisterPage :: Int
  -- | Which block the device last spoke from. Always `Nothing` here: this
  -- | page hears what is sent *to* the Twister, never what it sends.
  , twisterHeardBank :: Maybe Int
  , twisterFollow :: Follow
  , looperSaveName :: String
  | r
  )

-- | The Loop page's fields at load, as PWYF's `Engine.initialState` has them
-- | (less the MC6 and the pedal engine, which Quadrat does not have).
initial
  :: { looperStatus :: SocketStatus
     , looperSnapshotAge :: Number
     , looperFocus :: Int
     , looperSelectedSeen :: Maybe Int
     , looperPanel :: Maybe LooperPanel
     , looperPeaks :: Maybe Peaks
     , looperPeaksKey :: String
     , looperEditLocal :: Map String Int
     , looperBankShown :: Maybe LoopBanks.BankSlot
     , looperLog :: Array LogLine
     , looperAckSeq :: Int
     , twisterPage :: Int
     , twisterHeardBank :: Maybe Int
     , twisterFollow :: Follow
     , looperSaveName :: String
     }
initial =
  { looperStatus: { connected: false, everConnected: false, lastError: "", url: "" }
  , looperSnapshotAge: 0.0
  , looperFocus: 0
  , looperSelectedSeen: Nothing
  , looperPanel: Nothing
  , looperPeaks: Nothing
  , looperPeaksKey: ""
  , looperEditLocal: Map.empty
  , looperBankShown: Just LoopBanks.LoopBank
  , looperLog: []
  , looperAckSeq: 0
  , twisterPage: 0
  , twisterHeardBank: Nothing
  , twisterFollow: NotAsked
  , looperSaveName: ""
  }

-- | Every action the page can cause, plus the hub's two answers.
data Msg
  -- | A loop box was clicked: the first click takes the loop in hand, a click
  -- | on the loop already in hand opens its Edit panel.
  = PressLoop Int
  -- | The statement's choices: launch (beats, rig-wide), the input (one-based),
  -- | the grid, and the length in bars, each as the sentence gives it.
  | SetLaunch String
  | SetSource String
  | SetGrid Boolean
  | SetBars String
  | Simulate LoopBanks.BankSlot Int LoopBanks.Gesture
  | ShowTwisterPage Int
  | OpenPanel (Maybe LooperPanel)
  -- | A CC on the Itajara pedal's channel, pressed (value 127).
  | Gesture Int
  | SetClick Boolean
  | SetArm String
  | SetFeedback String
  | SetTone String
  | SetLayer Int Int Boolean
  | WindowIn Int Int
  | WindowOut Int Int
  | WindowTo Int Int Int
  | LayerWindowTo Int Int Int Int
  | ClearLayerWindow Int Int
  | ClearWindow Int
  | ShiftStart Int Int
  | AskPeaks Int
  | EditDone String
  | SetSaveName String
  -- | Save the loop in hand under this name (empty: the looper names it).
  | Save String
  | HubAnswered (Either String MIDIAccess)
  | TwisterSent String (Array Int)
  | PortsChanged

type Loop r act slots o m = H.HalogenM (Record (Fields r)) act slots o m Unit

foreign import pageHostImpl :: Effect String

-- | Helm's hub, as registered with Bosun, on the host the page came from.
hubPort :: Int
hubPort = 3039

-- | **Ask the hub, once, when the Loop page is first opened.** Not at load: a
-- | session spent on the Bench has no use for the Twister, and a page that
-- | connected to the hub regardless would appear in its `/state` as one more
-- | client for no reason.
start :: forall r act slots o m. MonadAff m => (Msg -> act) -> Loop r act slots o m
start wrap = do
  st <- H.get
  case st.twisterFollow of
    NotAsked -> do
      H.modify_ _ { twisterFollow = Asking }
      host <- liftEffect pageHostImpl
      let url = "ws://" <> host <> ":" <> show hubPort
      void $ H.subscribe $ HS.makeEmitter \emit -> do
        MIDI.requestHubAccess { url, client: "quadrat", waitMs: 3000.0 }
          (emit <<< wrap <<< HubAnswered)
        pure (pure unit)
    _ -> pure unit

-- | The page's share of Quadrat's poll: the socket's state, a loop's picture,
-- | the daemon's selection and its acks. `previous` is the snapshot before
-- | this tick's, which Quadrat has already replaced.
-- |
-- | Every write is guarded by a change, as PWYF's is, so a quiet daemon costs
-- | nothing here.
poll :: forall r act slots o m. MonadAff m => Maybe LooperState -> Maybe Peaks -> Loop r act slots o m
poll previous peaks = do
  status <- liftEffect Socket.status
  age <- liftEffect Socket.snapshotAge
  cur <- H.get
  -- Rounded, or a number that changes every tick re-renders for nothing.
  let age' = Number.floor (age / 500.0) * 500.0
  when (cur.looperStatus /= status || cur.looperSnapshotAge /= age') $
    H.modify_ _ { looperStatus = status, looperSnapshotAge = age' }
  for_ peaks \pk -> when (pk.loop >= 0 && cur.looperPeaks /= Just pk) $
    H.modify_ _ { looperPeaks = Just pk }
  for_ cur.looper \snap -> do
    -- The selected loop is the daemon's, shared by every surface on it.
    -- Adopted when it changes, not whenever it differs, so a press here is
    -- not overwritten by a snapshot that predates it.
    when (cur.looperSelectedSeen /= Just snap.selected) $
      H.modify_ _ { looperFocus = snap.selected, looperSelectedSeen = Just snap.selected }
    -- What the daemon said back. By sequence, not text: two identical
    -- refusals are two refusals.
    when (snap.ackSeq /= cur.looperAckSeq && snap.ack /= "") $
      H.modify_ \s -> s
        { looperLog = pushLog (snap.ack <> ackGap cur.looperAckSeq snap.ackSeq) s.looperLog
        , looperAckSeq = snap.ackSeq
        }
    when (snap.nLoops /= Layout.nLoops && maybe true (\p -> p.nLoops /= snap.nLoops) previous) $
      note ( "this surface is laid out for " <> show Layout.nLoops
               <> " loops; the daemon has " <> show snap.nLoops
               <> ". Loops past that cannot be reached from here." )
  -- **The Edit panel asks for its picture here**, and only when the picture
  -- would differ: the loop in focus, its layer count and its newest layer's
  -- birth. A loop with nothing in it has no picture and is not asked about.
  now <- H.get
  when (now.looperPanel == Just PanelEdit) $
    for_ (now.looper >>= \s -> Array.index s.loops now.looperFocus) \lp -> do
      let key = if lp.layers == 0 then ""
                else show now.looperFocus <> ":" <> show lp.layers <> ":"
                  <> show (maybe 0 _.born (Array.last lp.shapes))
      when (key /= "" && key /= now.looperPeaksKey) do
        H.modify_ _ { looperPeaksKey = key }
        onLoop now.looperFocus (LoopBanks.AskPeaks 600)

-- | Acks the 10 Hz poll stepped over, said rather than lost.
ackGap :: Int -> Int -> String
ackGap seen now
  | seen <= 0 = ""
  | now - seen <= 1 = ""
  | otherwise = " (and " <> show (now - seen - 1) <> " more the poll did not catch)"

handle :: forall r act slots o m. MonadAff m => (Msg -> act) -> Msg -> Loop r act slots o m
handle wrap = case _ of
  -- **Select, then edit.** A first click takes the loop in hand the way a
  -- press does (here at once, and told to the daemon, which holds the
  -- selection every surface shares); a click on the loop already in hand opens
  -- its Edit panel. Opening the panel on every click made choosing a loop and
  -- editing one the same gesture, and selecting is the commoner of the two.
  PressLoop loop -> do
    st <- H.get
    if st.looperFocus == loop
      then H.modify_ _ { looperPanel = Just PanelEdit, looperPeaksKey = "" }
      else do
        H.modify_ _ { looperFocus = loop }
        void $ liftEffect $ Socket.send (show loop)
  SetLaunch raw -> for_ (Int.fromString raw) \q -> focused (LoopBanks.Launch q)
  SetSource raw -> for_ (Int.fromString raw) \n -> focused (LoopBanks.SetSource n)
  SetGrid on -> focused (LoopBanks.OnGrid on)
  SetBars raw -> for_ (Int.fromString raw) \n -> when (n > 0) (focused (LoopBanks.SetBars n))
  -- **The MC6's pipeline, minus the wire.** The bytes the device would have
  -- sent are built and decoded exactly as a real press is, then read against
  -- the board's layout by `Switchboard.act`. Acted on here even when the
  -- looper's process holds the switches: this press never reached a wire, so
  -- nothing else will answer it.
  Simulate slot switch gesture ->
    -- On the default channel at both ends: this press never touches a wire,
    -- so there is no setting for it to disagree with.
    case LoopBanks.decodeSwitch LoopBanks.defaultSwitchChannel
           (LoopBanks.switchChannelNumber LoopBanks.defaultSwitchChannel)
           (LoopBanks.switchCC slot switch) (LoopBanks.gestureValue gesture) of
      Nothing -> note ("the board has no " <> LoopBanks.gestureName gesture
                         <> " on that switch")
      Just press -> do
        followBoard press
        st <- H.get
        traverse_ (runAction (Press.deferralOf Press.defaultDeferral press))
          (Switchboard.act (rigOf st) press)
  -- **Asked of the holder, while there is a Twister to ask about.** The card
  -- then follows what the holder sends. With nothing followed, the card is a
  -- reference and simply turns.
  ShowTwisterPage page -> do
    st <- H.get
    case st.twisterFollow of
      Following { access, port: Just port } ->
        liftEffect (MIDI.askHolder access port (TwisterHolder.pageRequest page))
      _ -> H.modify_ _ { twisterPage = page }
  -- Opening a panel forgets the last picture's key, so the Edit panel asks
  -- afresh even for the loop it showed last time.
  OpenPanel which -> H.modify_ _ { looperPanel = which, looperPeaksKey = "" }
  Gesture cc -> pedal cc 127
  SetSaveName name -> H.modify_ _ { looperSaveName = name }
  -- Through `runAction` like every other command; the daemon's answer (the
  -- folder, or "there is already a take called ...") comes back in the log.
  -- The field empties so the next save is not the same name by accident.
  Save name -> do
    st <- H.get
    runAction 0.0 (Machine.Command (Verb.at st.looperFocus (Verb.SaveTake name)))
    H.modify_ _ { looperSaveName = "" }
  -- Set from the wanted state, never flipped: the daemon's own flip is how
  -- two surfaces came to disagree about the click.
  SetClick wanted -> pedal 81 (if wanted then 127 else 0)
  SetArm raw -> for_ (Number.fromString raw) \db -> focused (LoopBanks.ArmLevel db)
  SetFeedback raw -> for_ (Number.fromString raw) \db -> focused (LoopBanks.Feedback db)
  SetTone raw -> for_ (Number.fromString raw) \hz -> focused (LoopBanks.Tone hz)
  SetLayer loop layer on -> onLoop loop (LoopBanks.LayerOn layer on)
  WindowIn loop frames -> do
    hold "in" frames
    onLoop loop (LoopBanks.WindowIn frames)
  WindowOut loop frames -> do
    hold "out" frames
    onLoop loop (LoopBanks.WindowOut frames)
  WindowTo loop inFrame outFrame -> do
    hold "in" inFrame
    onLoop loop (LoopBanks.WindowIn inFrame)
    onLoop loop (LoopBanks.WindowOut outFrame)
  LayerWindowTo loop layer inFrame outFrame -> do
    hold "in" inFrame
    onLoop loop (LoopBanks.LayerWindow layer inFrame outFrame)
  ClearLayerWindow loop layer -> onLoop loop (LoopBanks.ClearLayerWindow layer)
  ClearWindow loop -> onLoop loop LoopBanks.ClearWindow
  ShiftStart loop by -> do
    st <- H.get
    let rotNow = maybe 0 _.rot (st.looper >>= \s -> Array.index s.loops loop)
    hold "rot" (rotNow + by)
    onLoop loop (LoopBanks.ShiftStart by)
  AskPeaks loop -> onLoop loop (LoopBanks.AskPeaks 600)
  EditDone key -> H.modify_ \s -> s { looperEditLocal = Map.delete key s.looperEditLocal }
  HubAnswered (Left why) -> H.modify_ _ { twisterFollow = NotFollowed why }
  HubAnswered (Right access) -> do
    H.modify_ _ { twisterFollow = Following { access, port: Nothing } }
    void $ H.subscribe $ HS.makeEmitter \emit ->
      MIDI.onSent access \port bytes -> emit (wrap (TwisterSent port bytes))
    void $ H.subscribe $ HS.makeEmitter \emit ->
      MIDI.onStateChange access \_ -> emit (wrap PortsChanged)
    findTwister
  PortsChanged -> findTwister
  -- What the holder sent the Twister. The pager's colour says which page it
  -- is showing; anything else sent (a ring, a light) is not a page and says
  -- nothing.
  TwisterSent port bytes -> do
    st <- H.get
    case st.twisterFollow of
      Following { port: Just watched } | watched == port ->
        for_ (LoopTwister.pageShown bytes) \page ->
          when (page /= st.twisterPage) $ H.modify_ _ { twisterPage = page }
      _ -> pure unit
  where
  hold key value = H.modify_ \s -> s { looperEditLocal = Map.insert key value s.looperEditLocal }

-- | Look for the Twister among the hub's outputs and watch it, once found.
-- | Watches outlive a hub reconnect (Helm re-sends them), so this runs only
-- | until there is a port.
findTwister :: forall r act slots o m. MonadAff m => Loop r act slots o m
findTwister = do
  st <- H.get
  case st.twisterFollow of
    Following { access, port: Nothing } -> do
      found <- liftEffect (MIDI.findOutput access HelmTwister.port)
      for_ found \output -> do
        let id = (MIDI.outputPort output).id
        liftEffect (MIDI.watchPort access id)
        H.modify_ _ { twisterFollow = Following { access, port: Just id } }
    _ -> pure unit

-- | The Itajara pedal's CC, read by the shared table and then performed.
-- | No lateness: a click is unambiguous the moment it happens.
pedal :: forall r act slots o m. MonadAff m => Int -> Int -> Loop r act slots o m
pedal cc value = case PedalCC.dispatch cc value of
  PedalCC.Ignore -> pure unit
  PedalCC.NotYetImplemented what -> note (what <> " is not in the engine yet")
  PedalCC.Do subject duty -> do
    st <- H.get
    traverse_ (runAction 0.0) (Machine.perform (rigOf st) subject duty)

focused :: forall r act slots o m. MonadAff m => LoopBanks.Duty -> Loop r act slots o m
focused duty = do
  st <- H.get
  traverse_ (runAction 0.0) (Machine.perform (rigOf st) LoopBanks.Focused duty)

-- | One editing duty on one loop: the Edit panel's way in, and the layer
-- | checkboxes'.
onLoop :: forall r act slots o m. MonadAff m => Int -> LoopBanks.Duty -> Loop r act slots o m
onLoop loop duty = do
  st <- H.get
  traverse_ (runAction 0.0) (Machine.perform (rigOf st) (LoopBanks.OnLoop loop) duty)

rigOf :: forall r. Record (Fields r) -> Machine.Rig
rigOf st = Press.rigFrom st.looper st.looperFocus

-- | Which bank the simulated board is left on, read from the jump table it
-- | was programmed with (PWYF's `followBoard`).
followBoard :: forall r act slots o m. MonadAff m => LoopBanks.SwitchGesture -> Loop r act slots o m
followBoard press =
  H.modify_ _ { looperBankShown = case LoopBanks.sendsTo press.slot press.switch press.gesture of
      Just (LoopBanks.ToSlot to) -> Just to
      Just LoopBanks.ToBoard -> Nothing
      Nothing -> Just press.slot
    }

-- | **Carrying out what the machine decided; the only writer to the socket
-- | here.** The same cases as PWYF's `Control.runAction` and the looper
-- | process's `run`, and `ShowBank` answered as the process answers it: the
-- | MC6 belongs to PWYF, so this page says it cannot.
runAction :: forall r act slots o m. MonadAff m => Number -> Machine.Action -> Loop r act slots o m
runAction late action = do
  liftEffect $ Console.log $ "looper: " <> Machine.describe action
    <> (if late >= 1.0 then " (" <> show (Int.round late) <> " ms late)" else "")
  case action of
    Machine.Command command -> do
      ok <- liftEffect $ Socket.send (command <> "@" <> show (Int.round late))
      note (if ok then Machine.describe action else "no daemon — " <> command <> " went nowhere")
    -- Here at once, so the next press acts on it, and told to the daemon,
    -- which holds the selection every surface shares.
    Machine.Focus loop -> do
      H.modify_ _ { looperFocus = loop }
      void $ liftEffect $ Socket.send (show loop)
    Machine.ShowBank _ ->
      note ("not available here: " <> Machine.describe action <> " (the MC6 is PWYF's)")
    Machine.Unavailable why -> note why
    Machine.Handled what -> note what

note :: forall r act slots o m. MonadAff m => String -> Loop r act slots o m
note text = H.modify_ \s -> s { looperLog = pushLog text s.looperLog }

-- | The page, drawn by itajara's surface with Quadrat's state.
-- |
-- | No MC6 banks and no print sheet: programming the MC6 stays in PWYF. The
-- | Twister card is the looper's pages only, with a line under the tabs
-- | whenever it is not following a device, so a card showing page 1 because
-- | nothing told it otherwise is never mistaken for one that knows.
view :: forall r w act. (Msg -> act) -> PageLayout.Side w act -> Record (Fields r) -> HH.HTML w act
view wrap drawer st =
  -- **The page's shape, not the looper's** (`Quadrat.Layout`): the statement
  -- heads the main column as on every recording page (the meter is in the
  -- page's header since 2026-11-09), the panel
  -- buttons and the loops follow, and the Twister card and the log are in
  -- the side drawer, whose rail the owner slots (`drawer`).
  PageLayout.layout
    { statement
    , main: parts.notices <> parts.main <> [ parts.overlay ]
    , side: parts.side
    , drawer
    }
  where
  parts = Page.parts handlers ports extras st

  handlers =
    { pressLoop: wrap <<< PressLoop
    , simulate: \slot switch gesture -> wrap (Simulate slot switch gesture)
    , showTwisterPage: wrap <<< ShowTwisterPage
    , openPanel: wrap <<< OpenPanel
    , gesture: wrap <<< Gesture
    , setClick: wrap <<< SetClick
    , setArm: wrap <<< SetArm
    , setFeedback: wrap <<< SetFeedback
    , setTone: wrap <<< SetTone
    , setLayer: \loop layer on -> wrap (SetLayer loop layer on)
    , windowIn: \loop frames -> wrap (WindowIn loop frames)
    , windowOut: \loop frames -> wrap (WindowOut loop frames)
    , clearWindow: wrap <<< ClearWindow
    , shiftStart: \loop by -> wrap (ShiftStart loop by)
    , askPeaks: wrap <<< AskPeaks
    , windowTo: \loop i o -> wrap (WindowTo loop i o)
    , layerWindowTo: \loop layer i o -> wrap (LayerWindowTo loop layer i o)
    , clearLayerWindow: \loop layer -> wrap (ClearLayerWindow loop layer)
    , editDone: wrap <<< EditDone
    , waveDrag: Nothing
    , setSaveName: wrap <<< SetSaveName
    , save: wrap <<< Save
    }

  following = case st.twisterFollow of
    Following { port } -> isJust port
    _ -> false

  ports = { twister: following }

  extras =
    { twisterCard:
        [ TwisterMap.tabs st.twisterPage (wrap <<< ShowTwisterPage)
        , followLine
        , TwisterCard.render st.twisterPage
        ]
    , banks: Nothing
    , printSheet: Nothing
    }

  -- | **The strip's readings, said as the page's statement**, like the
  -- | Bench's and Conspicillum's. The Twister's page is left out: the card on
  -- | the right says it, and it is about the hand rather than the rig.
  statement =
    Statement.statement Statement.defaultStatement
      { class_ = "q-say is-loop"
      , sentences = map (map part) (LooperStatement.sentences settings st.looper st.looperFocus)
      , warnings = map (\w -> HH.p [ HP.class_ (HH.ClassName "q-clash is-soft") ] [ HH.text w ])
          (LooperStatement.warnings st.looper)
      }

  settings =
    { setClick: wrap <<< SetClick
    , setArm: wrap <<< SetArm
    , setFeedback: wrap <<< SetFeedback
    , setTone: wrap <<< SetTone
    , setLaunch: wrap <<< SetLaunch
    , setSource: wrap <<< SetSource
    , setGrid: wrap <<< SetGrid
    , setBars: wrap <<< SetBars
    }

  part = case _ of
    LooperStatement.Said words -> HH.text words
    LooperStatement.Read r -> Statement.reading r
    -- The Bench's slot classes, so a setting in this sentence looks and
    -- behaves like one in the Bench's.
    LooperStatement.Toggle t ->
      HH.button
        [ HP.class_ (HH.ClassName "q-slot is-toggle")
        , HP.title t.label
        , HE.onClick \_ -> t.intent
        ]
        [ HH.text t.value ]
    -- On change, not on input: typing "-40" passes through "-" and "-4", and
    -- each would be sent.
    LooperStatement.Field f ->
      HH.span_
        [ HH.input
            [ HP.class_ (HH.ClassName "q-slot is-num")
            , HP.type_ HP.InputNumber
            , HP.value f.value
            , HP.min f.min
            , HP.max f.max
            , HP.step (HP.Step f.step)
            , HP.title f.label
            , HE.onValueChange f.intent
            ]
        , HH.text f.unit
        ]
    LooperStatement.Choice c ->
      HH.select
        [ HP.class_ (HH.ClassName "q-slot")
        , HP.title c.label
        , HE.onValueChange c.intent
        ]
        ( map
            ( \o -> HH.option
                [ HP.value o.value
                , HP.selected (o.value == c.value)
                , HP.disabled o.off
                ]
                [ HH.text o.text ]
            )
            c.options
        )

  followLine = case st.twisterFollow of
    Following { port: Just _ } -> HH.text ""
    Following { port: Nothing } ->
      said "Helm's hub has no Twister, so this card is not following one."
    NotFollowed why ->
      said ("The Twister is not followed (" <> why <> "); the tabs only turn this card.")
    Asking -> said "Asking Helm's hub for the Twister\x2026"
    NotAsked -> HH.text ""

  said text = HH.p [ HP.class_ (HH.ClassName "twister-map-status") ] [ HH.text text ]
