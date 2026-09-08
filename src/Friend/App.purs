-- | The page. One component, because the Friend is one page: the loops as
-- | cards, each layer drawn, and every control a button that goes through the
-- | same machine a footswitch would — `Data.Looper.Machine.perform` against the
-- | daemon's own snapshot, so a button here and a switch on a pedalboard
-- | cannot come to mean different things by the same name.
-- |
-- | Two things are copied from producing-with-your-feet deliberately rather
-- | than shared, because each is a fact about how a Halogen app has to live
-- | beside this daemon and is worth reading in place: the poll is a
-- | subscription that only emits (a forked loop that *calls* the handler dies
-- | with the first throw and freezes the picture for ever), and the daemon's
-- | acks are read by sequence, not text (two identical refusals are two).
module Friend.App (component) where

import Prelude

import Control.Monad.Rec.Class (forever)
import Control.Promise (toAffE)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (for_, traverse_)
import Data.Int as Int
import Data.Looper.Duty (Duty, Subject(..))
import Data.Looper.Duty as Duty
import Data.Looper.Machine as Machine
import Data.Looper.Verb as Verb
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.Number as Number
import Data.String as String
import Effect.Aff (Milliseconds(..), attempt, delay)
import Effect.Exception (message)
import Effect.Aff as Aff
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Foreign.LooperSocket (LoopState, LooperState, Peaks, SocketStatus)
import Foreign.LooperSocket as Socket
import Friend.Face (Face)
import Friend.Face as Face
import Friend.Http (Notes, LoopNote)
import Friend.Http as Http
import Friend.Library as Library
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Subscription as HS
import Itajara.Surface.Edit as Edit
import Itajara.Surface.Wave (viewOf, wave)
import Web.DOM.Element as Element
import Web.Event.Event (currentTarget, preventDefault)
import Web.HTML.Event.DragEvent (DragEvent)
import Web.HTML.Event.DragEvent as DragEvent
import Web.UIEvent.MouseEvent (MouseEvent)
import Web.UIEvent.MouseEvent as ME

-- | **A conducted capture: many takes of one sound, counted.**
-- |
-- | *"Give me ten kicks"* is not a thing the looper can be asked for. It can
-- | record a pass, and it can wait for a sound before starting one, and the
-- | player can do the rest by pressing record ten times. This does the
-- | pressing.
-- |
-- | **Nothing here detects a hit.** The daemon already does: `lev` makes `r`
-- | arm rather than start, and the take begins on the sound that follows. That
-- | mode *survives every take* (`engine/next_take.rs`), so a session is one
-- | `lev1` and then a bare `r` per hit — the conductor watches the layer count
-- | and presses again when the last one has landed.
-- |
-- | **And nothing here trims.** The passes are the loop's own length, so ten
-- | hits are ten mostly-silent layers, and `msm` cuts them on the way to a
-- | card. That is *store everything, flatten late* — the same reason the take
-- | keeps full-fidelity layers rather than what a module happens to want.
type Session =
  { -- | The loop the session records into. One voice's stack.
    loop :: Int
  -- | What is being played, for the prompt: "kick 3 of 10".
  , label :: String
  , want :: Int
  -- | Each pass is this long. Only the first pass sets it; after that the
  -- | loop has a length and every layer matches it, which is what a stack
  -- | needs and what `RecordFixed` already does on a loop with material.
  , secs :: Number
  -- | Layers the loop already had when Start was pressed. `done` is measured
  -- | against this rather than counted here, so an undo mid-session is
  -- | simply a smaller number and not a desynchronised tally.
  , base :: Int
  , running :: Boolean
  -- | An `r` has gone out and the daemon has not yet shown its effect. Held
  -- | so a tick two hundred milliseconds later does not press again.
  , pending :: Boolean
  }

type State =
  { face :: Face
  , looper :: Maybe LooperState
  , status :: Maybe SocketStatus
  , age :: Number
  -- | The loop in hand: what the Edit panel edits, what a bare duty acts on.
  , focus :: Int
  , peaks :: Maybe Peaks
  , peaksKey :: String
  -- | A hand on the Edit picture: where it went down, the window's start
  -- | then, and how many frames one pixel is.
  , waveDrag :: Maybe { x0 :: Int, fpp :: Number, d :: Edit.Drag }
  -- | The slider a hand is on; see `Itajara.Surface.Edit.View`.
  , local :: Map String Int
  , panel :: Panel
  , ackSeq :: Int
  , log :: Array String
  -- | The name the next save goes under.
  , take :: String
  -- | Takes the server can see on disk: a name in here has been saved.
  , saved :: Array String
  -- | The player's notes for the take in hand, and which take they were
  -- | loaded for, so switching takes does not carry one's notes to another.
  , notes :: Notes
  , notesFor :: String
  , notesStatus :: String
  -- | The harvest form.
  , sticks :: Array String
  , stick :: String
  , bank :: String
  , scene :: String
  -- Rample: the kit to write, and what its layers mean.
  , slot :: String
  , kindAs :: String
  -- | The conducted capture; see `Session`.
  , session :: Session
  , overwrite :: Boolean
  , allLayers :: Boolean
  , harvestOut :: String
  , harvestBusy :: Boolean
  -- | **The library browser.** Every shelf the server can see, the shelf and
  -- | scene in hand, that scene's audio once its headers have been read, and
  -- | what is sounding. Deliberately independent of the daemon: browsing and
  -- | auditioning samples is worth doing with no looper running at all.
  , shelves :: Array Library.Shelf
  , shelfId :: Maybe String
  , openScene :: Maybe { lib :: String, path :: String, name :: String }
  , sceneAt :: Library.SceneInfo
  , hearing :: Maybe { url :: String, name :: String }
  , libStatus :: String
  -- | A drag in progress: which loop it came from and, for one layer, which
  -- | (from one). And the empty loop the pointer is over, if any.
  , drag :: Maybe { loop :: Int, layer :: Maybe Int }
  , dropOn :: Maybe Int
  }

-- | One modal at a time: the loop in hand's edit, the take's notes, or the
-- | harvest. The Edit panel is the shared one; the other two are this page's.
data Panel = NoPanel | EditPanel | NotesPanel | HarvestPanel | LibraryPanel | SessionPanel

derive instance Eq Panel

-- | A field of the take's notes, so one action sets any of them.
data NoteField = NTitle | NKey | NBpm | NTimbre | NUses | NNotes | NTags

-- | A field of one loop's notes.
data LoopField = LTitle | LKey | LTimbre | LUses | LNotes

data Action
  = Initialize
  | Poll
  | Do Subject Duty
  | Focus Int
  | ToggleEdit Int
  | SetLayer Int Int Boolean
  | WindowIn Int Int
  | WindowOut Int Int
  | ClearWindow Int
  | ShiftStart Int Int
  | AskPeaks Int
  | EditDone String
  | SetTake String
  | SaveAll
  | OpenPanel Panel
  | RefreshTakes
  | SetNote NoteField String
  | SetLoopNote Int LoopField String
  | SaveNotes
  | RefreshSticks
  | SetStick String
  | SetBank String
  | SetScene String
  | SetOverwrite Boolean
  | SetAllLayers Boolean
  | SetSlot String
  | SetKindAs String
  -- | The conductor; see `Session` and `sessionModal`.
  | OpenSession Int
  | SetSessionLabel String
  | SetSessionWant String
  | SetSessionSecs String
  | StartSession
  | StopSession
  | RunHarvest Boolean
  -- | The library browser; see `libraryModal`.
  | OpenLibrary
  | PickShelf String
  | PickScene String String String
  | Audition String String
  | SetWindow Int Int Int
  | SetLayerWindow Int Int Int Int
  | ClearLayerWindow Int Int
  | Hear Int Int
  -- | One source for every loop; see `sourceBar`.
  | SetSourceAll Int
  -- | One source for ONE loop — what a capture session needs, where the bar
  -- | above is what a performance needs.
  | SetSourceOne Int Int
  | NotesFor Int
  | StartDrag Int (Maybe Int)
  | WaveDown Edit.Drag MouseEvent
  | WaveMove MouseEvent
  | WaveUp
  | DragOver Int DragEvent
  | DragLeave Int
  | DropOn Int DragEvent
  | EndDrag

component :: forall q o m. MonadAff m => H.Component q Face o m
component =
  H.mkComponent
    { initialState: \face ->
        { face, looper: Nothing, status: Nothing, age: 0.0, focus: 0, peaks: Nothing
        , peaksKey: "", waveDrag: Nothing, local: Map.empty, panel: NoPanel, ackSeq: 0, log: [], take: "take"
        , saved: [], notes: Http.emptyNotes, notesFor: "", notesStatus: ""
        , sticks: [], stick: "", bank: "1", scene: "1_1", overwrite: false, allLayers: false
        -- No default slot: which kit to overwrite is not a thing to guess at,
        -- and msm refuses an empty one rather than picking.
        , slot: "", kindAs: "drum-kit"
        , session: { loop: 0, label: "kick", want: 10, secs: 2.0, base: 0, running: false, pending: false }
        , harvestOut: "", harvestBusy: false, drag: Nothing, dropOn: Nothing
        , shelves: [], shelfId: Nothing, openScene: Nothing, sceneAt: Library.emptyScene
        , hearing: Nothing, libStatus: "" }
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction, initialize = Just Initialize }
    }

-- | Everything the machine is allowed to know, from the newest snapshot.
-- | No grab loops and no grab source: those are facts about a pedalboard's
-- | reach, and this page reaches everything.
rigOf :: State -> Machine.Rig
rigOf st =
  { loops: maybe [] _.loops st.looper
  , focus: st.focus
  -- The daemon's own default until it says otherwise; see `Machine.Rig`.
  , maxLayers: maybe 4 _.maxLayers st.looper
  , click: maybe false _.click st.looper
  , monitor: maybe false _.monitor st.looper
  , armDb: maybe (-36.0) _.armDb st.looper
  , launchQ: maybe (-1) _.launchQ st.looper
  , sources: maybe [] (map _.name <<< _.sources) st.looper
  , grab: []
  , grabSource: ""
  }

handleAction :: forall o m. MonadAff m => Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  Initialize -> do
    liftEffect $ Socket.connect Socket.defaultUrl
    void $ H.subscribe $ HS.makeEmitter \emit -> do
      fiber <- Aff.launchAff $ forever do
        delay (Milliseconds 100.0)
        liftEffect (emit Poll)
      pure (Aff.launchAff_ (Aff.killFiber (Aff.error "poll stopped") fiber))
    handleAction RefreshTakes
    handleAction RefreshSticks

  Poll -> do
    status <- liftEffect Socket.status
    age <- liftEffect Socket.snapshotAge
    snap <- liftEffect Socket.latest
    pk <- liftEffect Socket.latestPeaks
    cur <- H.get
    when (cur.peaks /= pk) $ H.modify_ _ { peaks = pk }
    -- Rounded, or a number that changes every tick redraws the page ten
    -- times a second for ever.
    let age' = Number.floor (age / 500.0) * 500.0
    when (cur.looper /= snap || cur.status /= Just status || cur.age /= age') do
      H.modify_ _ { looper = snap, status = Just status, age = age' }
      -- The Edit panel asks for its picture only when the picture would
      -- differ: the loop in focus, its layer count, its newest layer's birth.
      when (cur.panel == EditPanel) $
        for_ snap \s -> for_ (Array.index s.loops cur.focus) \lp -> do
          let key = if lp.layers == 0 then ""
                    else show cur.focus <> ":" <> show lp.layers <> ":"
                      <> show (maybe 0 _.born (Array.last lp.shapes))
          when (key /= "" && key /= cur.peaksKey) do
            H.modify_ _ { peaksKey = key }
            duty cur.focus (Duty.AskPeaks 600)
      -- **No solo rule here any more.** The newest layer sounding alone, the
      -- silence while the next one goes down, the repair after an undo: all
      -- of it is the loop's `alt` property, kept by the daemon since
      -- 2026-09-07, and this page's whole part is `ensureAlternates` before
      -- a take. The snapshot diff that used to do it from here silenced the
      -- pedalboard's layers from a background tab, which is the reason.
      -- What the daemon had to say. By sequence, so two identical refusals
      -- in a row are two lines.
      for_ snap \s ->
        when (s.ackSeq /= cur.ackSeq && s.ack /= "") do
          -- An ack is also the answer to a press: if the daemon refused the
          -- one just sent, the session must not wait for ever for a take that
          -- will never open.
          H.modify_ (note s.ack <<< _ { ackSeq = s.ackSeq })
          H.modify_ \x -> x { session = x.session { pending = false } }

      -- **The conductor's whole body.**
      --
      -- Press, wait for the pass to land, press again, stop at N. It knows a
      -- hit landed because the layer count went up, which is the daemon's
      -- own account of it rather than a second opinion formed here — and it
      -- counts from `base` rather than tallying its own presses, so an undo
      -- mid-session is a smaller number instead of a desynchronised session.
      for_ snap \s -> do
        now <- H.get
        when now.session.running $
          for_ (Array.index s.loops now.session.loop) \lp -> do
            let done = lp.layers - now.session.base
                -- Listening or writing: the last press is being answered, so
                -- there is nothing to do but let it finish.
                busy = Socket.isWriting lp || lp.armed
            when busy $
              H.modify_ \x -> x { session = x.session { pending = false } }
            if done >= now.session.want then do
              setListening now.session.loop false
              H.modify_ \x -> x { session = x.session { running = false, pending = false } }
              H.modify_ (note
                (show now.session.want <> " × " <> now.session.label
                  <> " on loop " <> show (now.session.loop + 1)
                  <> " — they are layers of the loop's own length, so they are mostly "
                  <> "silence until msm trims them"))
            else
              when (not busy && not now.session.pending) do
                H.modify_ \x -> x { session = x.session { pending = true } }
                -- Only an empty loop needs a length; after that every layer
                -- takes the loop's, which is what a stack requires.
                duty now.session.loop
                  (if lp.layers == 0 then Duty.RecordFixed now.session.secs else Duty.RecordLoop)

  Do subject d -> do
    -- **Every take this page starts declares the loop's layers alternates
    -- first**, on a face that wants them so; the daemon then silences the
    -- loop while the layer goes down, solos it when it lands, and sums a
    -- held overdub into the one that sounds. The pedalboard never sends it,
    -- and a loop it cleared has forgotten it, so ask before each take rather
    -- than once.
    case subject, d of
      OnLoop i, Duty.RecordFixed _ -> ensureAlternates i
      OnLoop i, Duty.OverdubLoop -> ensureAlternates i
      -- Open means open. Undo keeps a loop's length so the pedalboard's next
      -- take lands on the same grid; on this face a loop with no layers and
      -- a length would close an "open" take at thirteen seconds, so the
      -- length is let go first.
      OnLoop i, Duty.RecordLoop -> do
        ensureAlternates i
        st <- H.get
        for_ (st.looper >>= \top -> Array.index top.loops i) \lp ->
          when (lp.sized && not (Socket.isWriting lp) && not lp.armed) $
            duty i Duty.ForgetLength
      _, _ -> pure unit
    st <- H.get
    traverse_ runAction (Machine.perform (rigOf st) subject d)
  Focus i -> H.modify_ _ { focus = i }
  ToggleEdit i -> do
    st <- H.get
    let opening = not (st.panel == EditPanel && st.focus == i)
    H.modify_ _ { focus = i, panel = if opening then EditPanel else NoPanel, peaksKey = "" }
  SetLayer loop layer on -> duty loop (Duty.LayerOn layer on)
  WindowIn loop f -> do
    H.modify_ \s -> s { local = Map.insert "in" f s.local }
    duty loop (Duty.WindowIn f)
  WindowOut loop f -> do
    H.modify_ \s -> s { local = Map.insert "out" f s.local }
    duty loop (Duty.WindowOut f)
  ClearWindow loop -> duty loop Duty.ClearWindow
  -- Both ends together: the fixed window's slider. Two verbs on one
  -- connection, held by the daemon to the same settle, so they land as one.
  SetWindow loop i o -> do
    H.modify_ \s -> s { local = Map.insert "in" i s.local }
    duty loop (Duty.WindowIn i)
    duty loop (Duty.WindowOut o)
  SetLayerWindow loop k i o -> do
    H.modify_ \s -> s { local = Map.insert "in" i s.local }
    duty loop (Duty.LayerWindow k i o)
  ClearLayerWindow loop k -> duty loop (Duty.ClearLayerWindow k)
  -- The Layer knob: on an alternate loop the daemon reads "layer k on" as
  -- "this one, and only this one", so nothing here works out the rest.
  Hear loop k -> do
    H.modify_ _ { focus = loop, peaksKey = "" }
    duty loop (Duty.LayerOn k true)
  -- **Every loop, not the focused one.** The verb is per loop because the
  -- daemon's model is; the decision is per session because this page's is.
  -- Sent to all of them so the readout can be a single word rather than a
  -- word plus a footnote about which loops it did not reach.
  SetSourceOne loop n -> duty loop (Duty.SetSource n)
  SetSourceAll n -> do
    st <- H.get
    let count = maybe 0 (Array.length <<< _.loops) st.looper
    traverse_ (\i -> duty i (Duty.SetSource n)) (Array.range 0 (count - 1))
  NotesFor i -> do
    H.modify_ _ { focus = i }
    handleAction (OpenPanel NotesPanel)
  -- **Drag and drop is a copy onto an empty loop.** The machine decides
  -- whether the drop means anything (empty target, a source with layers) and
  -- the daemon decides again; here we only say which loop is under the
  -- pointer and let the default drop be prevented, which is what makes a
  -- browser allow one at all.
  StartDrag i k -> H.modify_ _ { drag = Just { loop: i, layer: k } }
  -- **The window under the hand.** Frames per pixel from the picture's
  -- width, read once on the way down; every move is then the distance
  -- travelled in frames, snapped to the step and held to the bounds, sent
  -- as the same window the slider sends.
  WaveDown d ev -> do
    liftEffect $ preventDefault (ME.toEvent ev)
    width <- liftEffect $ case currentTarget (ME.toEvent ev) >>= Element.fromEventTarget of
      Just el -> _.width <$> Element.getBoundingClientRect el
      Nothing -> pure 0.0
    when (width > 0.0) $
      H.modify_ _ { waveDrag = Just { x0: ME.clientX ev, fpp: Int.toNumber d.span / width, d } }
  WaveMove ev -> do
    st <- H.get
    for_ st.waveDrag \w -> do
      let
        dx = Int.toNumber (ME.clientX ev - w.x0) * w.fpp
        steps = Int.round (dx / Int.toNumber (max 1 w.d.step))
        s = clamp w.d.lo w.d.hi (w.d.win0 + steps * w.d.step)
        n = st.face.windowSecs
        sr = maybe 48000 _.sampleRate st.looper
        o = s + Int.round (n * Int.toNumber sr)
      when (Map.lookup "in" st.local /= Just s) $ case w.d.layer of
        Just k -> handleAction (SetLayerWindow w.d.loop k s o)
        Nothing -> handleAction (SetWindow w.d.loop s o)
  WaveUp -> do
    st <- H.get
    when (isJust st.waveDrag) do
      H.modify_ _ { waveDrag = Nothing }
      handleAction (EditDone "in")
  DragOver i ev -> do
    st <- H.get
    when (canDrop st i) do
      liftEffect (preventDefault (DragEvent.toEvent ev))
      when (st.dropOn /= Just i) $ H.modify_ _ { dropOn = Just i }
  DragLeave i -> H.modify_ \s -> s { dropOn = if s.dropOn == Just i then Nothing else s.dropOn }
  DropOn i ev -> do
    liftEffect (preventDefault (DragEvent.toEvent ev))
    -- A drop grows the loop as a take does, so it declares the same thing.
    ensureAlternates i
    st <- H.get
    for_ st.drag \d ->
      duty i (case d.layer of
        Just k | d.loop == i -> Duty.DupLayer k
        Just k -> Duty.CopyLayer d.loop k
        Nothing -> Duty.CopyLoop d.loop)
    H.modify_ _ { drag = Nothing, dropOn = Nothing }
  EndDrag -> H.modify_ _ { drag = Nothing, dropOn = Nothing }
  ShiftStart loop k -> do
    st <- H.get
    let rotNow = maybe 0 _.rot (st.looper >>= \s -> Array.index s.loops loop)
    H.modify_ \s -> s { local = Map.insert "rot" (rotNow + k) s.local }
    duty loop (Duty.ShiftStart k)
  AskPeaks loop -> duty loop (Duty.AskPeaks 600)
  EditDone key -> H.modify_ \s -> s { local = Map.delete key s.local }
  SetTake t -> H.modify_ _ { take = t }
  -- **One verb, one ack.** `exl<name>` writes every loop that holds
  -- something as a take of its own — `<name>/loop-<n>/`, the layers raw —
  -- and one manifest for the set, which is exactly the material a scene is
  -- made of. The shaping into the module's own folder is the harvest step,
  -- which the face says whether it has yet. The one thing here that does
  -- not go through `perform`: no switch can carry a name, so the vocabulary
  -- has no slot for one.
  SaveAll -> do
    st <- H.get
    let loops = maybe [] _.loops st.looper
    if Array.all (\lp -> lp.layers == 0) loops then H.modify_ (note "nothing to save: no loop has a layer")
    else do
      when (st.notesFor == safeName st.take) $ handleAction SaveNotes
      runAction (Machine.Command (Verb.render (Verb.ExportLayers (safeName st.take))))
      -- The daemon writes on its own thread and the ack lands in the
      -- snapshot; the folder is there a moment later. Ask the server then.
      void $ H.fork do
        H.liftAff (delay (Milliseconds 1500.0))
        handleAction RefreshTakes

  OpenPanel p -> do
    -- Leaving the notes panel saves it: a button nobody pressed was how a
    -- take went to the stick with an empty datasheet.
    before <- H.get
    when (before.panel == NotesPanel && p /= NotesPanel && before.notesFor == safeName before.take) $
      handleAction SaveNotes
    H.modify_ _ { panel = p }
    st <- H.get
    when (p == NotesPanel && st.notesFor /= safeName st.take) do
      r <- H.liftAff (attempt (toAffE (Http.loadNotes (safeName st.take))))
      case r of
        Right n -> H.modify_ _ { notes = n, notesFor = safeName st.take, notesStatus = "" }
        Left e -> H.modify_ _ { notes = Http.emptyNotes, notesFor = safeName st.take, notesStatus = "could not load notes: " <> message e }
    when (p == HarvestPanel) $ handleAction RefreshSticks
  OpenLibrary -> do
    handleAction (OpenPanel LibraryPanel)
    H.modify_ _ { libStatus = "reading…" }
    r <- H.liftAff (attempt (toAffE Library.listLibrary))
    case r of
      Right sh -> H.modify_ _
        { shelves = sh
        , libStatus = if Array.null sh then "No libraries. Add one to ~/.itajara/libraries.json and open this again." else ""
        }
      Left e -> H.modify_ _ { libStatus = "could not read the libraries: " <> message e }
  PickShelf k -> H.modify_ _ { shelfId = Just k, openScene = Nothing, sceneAt = Library.emptyScene }
  PickScene lib pth name -> do
    -- The names are already in hand from the tree; only the headers are not,
    -- so the column fills at once and the durations arrive behind them.
    H.modify_ _ { openScene = Just { lib, path: pth, name }, sceneAt = Library.emptyScene }
    r <- H.liftAff (attempt (toAffE (Library.sceneInfo lib pth)))
    case r of
      Right d -> H.modify_ _ { sceneAt = d }
      Left e -> H.modify_ _ { libStatus = "could not read the scene: " <> message e }
  Audition url name -> H.modify_ _ { hearing = Just { url, name } }
  RefreshTakes -> do
    r <- H.liftAff (attempt (toAffE Http.listTakes))
    case r of
      Right ts -> H.modify_ _ { saved = ts }
      -- No server (the page is being served statically): nothing to list,
      -- and nothing to say every second about it.
      Left _ -> pure unit
  SetNote f v -> H.modify_ \s -> s { notes = setNote f v s.notes }
  SetLoopNote i f v -> H.modify_ \s -> s { notes = s.notes { loops = setLoopNote i f v s.notes.loops } }
  SaveNotes -> do
    st <- H.get
    r <- H.liftAff (attempt (toAffE (Http.saveNotes (safeName st.take) st.notes)))
    H.modify_ _ { notesStatus = case r of
      Right _ -> "saved to ~/.itajara/takes/" <> safeName st.take <> "/notes.json"
      Left e -> "could not save: " <> message e }
  RefreshSticks -> do
    r <- H.liftAff (attempt (toAffE Http.listSticks))
    case r of
      Right ss -> H.modify_ \s -> s { sticks = ss, stick = if s.stick == "" then fromMaybe "" (Array.head ss) else s.stick }
      Left _ -> pure unit
  SetStick v -> H.modify_ _ { stick = v }
  SetBank v -> H.modify_ _ { bank = v }
  SetScene v -> H.modify_ _ { scene = v }
  SetSlot v -> H.modify_ _ { slot = v }
  OpenSession i -> do
    st <- H.get
    H.modify_ _ { focus = i, panel = SessionPanel
                , session = st.session { loop = i } }
  SetSessionLabel v -> H.modify_ \s -> s { session = s.session { label = v } }
  SetSessionWant v ->
    H.modify_ \s -> s { session = s.session { want = clamp 1 128 (fromMaybe s.session.want (Int.fromString v)) } }
  SetSessionSecs v ->
    H.modify_ \s -> s { session = s.session { secs = fromMaybe s.session.secs (Number.fromString v) } }
  StopSession -> do
    st <- H.get
    -- Take the mode back off with the session. A loop left listening holds
    -- the input for a recording that is no longer coming, and the next thing
    -- the player does would start a take they did not ask for.
    setListening st.session.loop false
    H.modify_ \s -> s { session = s.session { running = false, pending = false } }
    H.modify_ (note "session stopped")
  StartSession -> do
    st <- H.get
    let i = st.session.loop
    case st.looper >>= \top -> Array.index top.loops i of
      Nothing -> H.modify_ (note ("loop " <> show (i + 1) <> " is not in the snapshot"))
      Just lp
        | Socket.isWriting lp ->
            H.modify_ (note ("loop " <> show (i + 1) <> " is recording — close it first"))
        | otherwise -> do
            ensureAlternates i
            -- **The grid has to be off.** Armed and quantised, the daemon
            -- finds the crossing and then waits for the bar — so the hit that
            -- started the take is behind the recording by up to a bar, and
            -- what lands is silence followed by the next hit. A hit session
            -- is not tempo-locked by definition; say so rather than let that
            -- happen quietly.
            when lp.quant do
              runAction (Machine.Command (Verb.at i (Verb.OnGrid false)))
              H.modify_ (note ("loop " <> show (i + 1) <> "'s grid is off for the session: "
                <> "an armed take on the grid waits for the bar, and the hit that armed it "
                <> "would be behind the recording"))
            -- The one mode the session needs, and the whole of its hit
            -- detection: `r` now waits for a sound instead of starting on
            -- the press. The daemon reaches back past the threshold crossing,
            -- so the attack is not clipped by the thing that detected it.
            setListening i true
            H.modify_ \s -> s
              { session = s.session { base = lp.layers, running = true, pending = false } }
            H.modify_ (note
              ("listening for " <> show st.session.want <> " × " <> st.session.label
                <> " on loop " <> show (i + 1)
                <> " — softest first, and do not stop between them"))
  SetKindAs v -> H.modify_ _ { kindAs = v }
  SetOverwrite v -> H.modify_ _ { overwrite = v }
  SetAllLayers v -> H.modify_ _ { allLayers = v }
  RunHarvest dryRun -> do
    st <- H.get
    H.modify_ _ { harvestBusy = true, harvestOut = if dryRun then "dry run…" else "harvesting…" }
    r <- H.liftAff (attempt (toAffE (Http.harvest
      { take: safeName st.take, module: st.face.id, stick: st.stick, bank: st.bank, scene: st.scene
      , card: "", slot: st.slot, as: st.kindAs
      , overwrite: st.overwrite, allLayers: st.allLayers, dryRun })))
    H.modify_ _ { harvestBusy = false, harvestOut = case r of
      Right res -> res.output <> (if res.ok then "" else "\n(msm reported a failure)")
      Left e -> "could not reach the server: " <> message e }
    handleAction RefreshTakes
  where
  duty loop d = do
    st <- H.get
    traverse_ runAction (Machine.perform (rigOf st) (OnLoop loop) d)
  -- | Set the loop's level-arm, rather than flipping it.
  -- |
  -- | `Duty.LevelArm` is the toggle a footswitch wants; a session has to know
  -- | the mode is on at the start and off at the end, and a flip that started
  -- | from the wrong place would leave the loop listening after Stop — holding
  -- | the input for a take nobody asked for.
  setListening loop on =
    runAction (Machine.Command (Verb.at loop (Verb.LevelArm on)))
  -- **The one thing this page adds to a take.** On a face whose layers are
  -- alternates, a loop that is not yet declared so is declared before the
  -- take, the copy or the duplicate that grows it — every path here that
  -- grows a loop goes through this. Not while the loop is writing or
  -- listening: the press is then a close or a cancel, not a take.
  ensureAlternates loop = do
    st <- H.get
    when st.face.alternates $
      for_ (st.looper >>= \top -> Array.index top.loops loop) \lp ->
        unless (lp.alt || Socket.isWriting lp || lp.armed) $
          duty loop (Duty.Alternates true)

runAction :: forall o m. MonadAff m => Machine.Action -> H.HalogenM State Action () o m Unit
runAction a = do
  liftEffect $ Console.log ("looper: " <> Machine.describe a)
  case a of
    Machine.Command c -> do
      ok <- liftEffect $ Socket.send (c <> "@0")
      H.modify_ (note (if ok then Machine.describe a else "no daemon — " <> c <> " went nowhere"))
    Machine.Focus i -> H.modify_ _ { focus = i }
    -- No pedalboard to show a bank on. Not an error: the machine asks as a
    -- courtesy, and here there is nobody to ask.
    Machine.ShowBank _ -> pure unit
    Machine.Unavailable why -> H.modify_ (note why)
    Machine.Handled what -> H.modify_ (note what)

note :: String -> State -> State
note msg s = s { log = Array.takeEnd 12 (Array.snoc s.log msg) }

-- | A take name the filesystem and the module both accept: letters, digits,
-- | dash and underscore; anything else becomes an underscore.
safeName :: String -> String
safeName s =
  let
    ok c = c == "-" || c == "_" || (c >= "a" && c <= "z") || (c >= "A" && c <= "Z") || (c >= "0" && c <= "9")
    cleaned = String.joinWith "" (map (\c -> if ok c then c else "_") (String.split (String.Pattern "") s))
  in
    if cleaned == "" then "take" else cleaned

-- | Whether a drop here would mean something: a drag in hand, from another
-- | loop onto one that holds nothing — or a layer onto its own loop, which
-- | duplicates it, while there is room. The same rules the machine applies.
canDrop :: State -> Int -> Boolean
canDrop st i = case st.drag of
  Nothing -> false
  Just d -> case st.looper of
    Nothing -> false
    Just top -> case Array.index top.loops i of
      Nothing -> false
      Just lp
        | d.loop == i -> d.layer /= Nothing && lp.layers < top.maxLayers
        | otherwise -> lp.layers == 0

-- | The layer sounding alone, from one, on a face whose layers are
-- | alternates: the first one the daemon reports on. The Edit panel edits
-- | its window.
activeLayer :: State -> Int -> Maybe Int
activeLayer st i
  | not st.face.alternates = Nothing
  | otherwise = do
      top <- st.looper
      lp <- Array.index top.loops i
      k <- Array.findIndex _.on lp.shapes
      pure (k + 1)

setNote :: NoteField -> String -> Notes -> Notes
setNote f v n = case f of
  NTitle -> n { title = v }
  NKey -> n { key = v }
  NBpm -> n { bpm = v }
  NTimbre -> n { timbre = v }
  NUses -> n { uses = v }
  NNotes -> n { notes = v }
  NTags -> n { tags = v }

-- | The loop's row, made if it has none yet. Loops are numbered from one
-- | here, as the datasheet and the folders are.
setLoopNote :: Int -> LoopField -> String -> Array LoopNote -> Array LoopNote
setLoopNote i f v rows =
  case Array.findIndex (\r -> r.loop == i) rows of
    Just k -> fromMaybe rows (Array.modifyAt k (set) rows)
    Nothing -> Array.snoc rows (set { loop: i, title: "", key: "", timbre: "", uses: "", notes: "" })
  where
  set r = case f of
    LTitle -> r { title = v }
    LKey -> r { key = v }
    LTimbre -> r { timbre = v }
    LUses -> r { uses = v }
    LNotes -> r { notes = v }

loopNote :: Int -> Array LoopNote -> LoopNote
loopNote i rows = fromMaybe { loop: i, title: "", key: "", timbre: "", uses: "", notes: "" } (Array.find (\r -> r.loop == i) rows)

render :: forall m. State -> H.ComponentHTML Action () m
render st =
  HH.div [ HP.class_ (HH.ClassName "friend") ]
    ( [ header ]
        <> (case st.looper of
              Nothing -> [ noDaemon ]
              Just top -> [ shape top, loops top, controls top ])
        <> [ logView ]
        <> (case st.panel of
              NoPanel -> []
              EditPanel -> [ editModal ]
              NotesPanel -> [ notesModal ]
              HarvestPanel -> [ harvestModal ]
              LibraryPanel -> [ libraryModal ]
              SessionPanel -> [ sessionModal ])
    )
  where
  f = st.face

  header =
    HH.header [ HP.class_ (HH.ClassName "friend-head") ]
      [ HH.h1_ [ HH.text f.name ]
      , HH.p [ HP.class_ (HH.ClassName "friend-sub") ]
          [ HH.text ("A looper that writes " <> f.unit <> "s for the " <> f.maker <> " " <> f.module_ <> ". ") ]
      , HH.p [ HP.class_ (HH.ClassName ("friend-conn " <> connClass)) ] [ HH.text connWord ]
      , maybe (HH.text "") sourceBar st.looper
      -- In the header, not the controls: the controls only exist when a
      -- daemon does, and a sample browser has no business needing one.
      , HH.button
          [ HP.class_ (HH.ClassName ("friend-libbtn" <> if st.panel == LibraryPanel then " is-on" else ""))
          , HE.onClick \_ -> OpenLibrary
          ]
          [ HH.text "Library" ]
      ]

  connClass = case st.status of
    Just s | s.connected -> "is-on"
    _ -> "is-off"
  connWord = case st.status of
    Nothing -> "Looking for the daemon…"
    Just s
      | s.connected && st.age > 2000.0 -> "Connected, but the daemon has said nothing for " <> secs (st.age / 1000.0) <> " s."
      | s.connected -> "Daemon at " <> s.url
      | s.everConnected -> "Lost the daemon at " <> s.url <> " — it was there and is not now."
      | otherwise -> "No daemon at " <> s.url <> ". Start one:"

  noDaemon =
    HH.section [ HP.class_ (HH.ClassName "friend-start") ]
      [ HH.pre_ [ HH.code_ [ HH.text f.daemon ] ]
      , HH.p_ [ HH.text ("<device> is your audio interface's name; itajara devices lists them. This page connects by itself once the daemon is up.") ]
      , HH.ul_ (map (\n -> HH.li_ [ HH.text n ]) f.notes)
      ]

  shape top =
    HH.section [ HP.class_ (HH.ClassName "friend-shape") ]
      ( [ HH.span_
            [ HH.text (show top.nLoops <> " loops × " <> show top.maxLayers <> " layers × "
                <> secs top.maxSecs <> " s at " <> show top.sampleRate <> " Hz") ]
        , HH.span_ [ HH.text ("A " <> f.unit <> " holds " <> show f.layers <> " " <> f.layerWord <> "s of " <> secs f.layerSecs <> " s; " <> f.holds <> ".") ]
        ]
          <> map (\n -> HH.span [ HP.class_ (HH.ClassName "friend-warn") ] [ HH.text n ]) (Face.shapeNotes f top)
      )

  loops top =
    HH.section [ HP.class_ (HH.ClassName "friend-loops") ]
      (Array.mapWithIndex (card top) top.loops)

  card top i lp =
    HH.article
      [ HP.class_ (HH.ClassName ("friend-loop " <> phaseClass lp
          <> (if i == st.focus then " is-focus" else "")
          <> (if st.dropOn == Just i then " is-drop" else "")
          <> (if canDrop st i then " can-drop" else "")))
      , HE.onClick \_ -> Focus i
      , HE.onDragOver (DragOver i)
      , HE.onDragLeave \_ -> DragLeave i
      , HE.onDrop (DropOn i)
      ]
      [ HH.div [ HP.class_ (HH.ClassName "friend-loop-head") ]
          -- The name is the handle for the whole loop: drag it onto an empty
          -- loop and every layer goes.
          [ HH.span
              [ HP.class_ (HH.ClassName "friend-loop-name")
              , HP.draggable (lp.layers > 0)
              , HP.title (if lp.layers > 0 then "drag onto an empty loop to copy every layer" else "")
              , HE.onDragStart \_ -> StartDrag i Nothing
              , HE.onDragEnd \_ -> EndDrag
              ]
              [ HH.text ("Loop " <> show (i + 1)) ]
          , HH.span [ HP.class_ (HH.ClassName "friend-loop-state") ] [ HH.text (stateWord lp) ]
          , if lp.alt
              then HH.span [ HP.class_ (HH.ClassName "friend-loop-alt"), HP.title "the layers are alternates: one sounds" ] [ HH.text "alt" ]
              else HH.text ""
          , HH.span [ HP.class_ (HH.ClassName "friend-loop-len") ] [ HH.text (lengthWord top lp) ]
          , HH.span [ HP.class_ (HH.ClassName "friend-loop-dest") ] [ HH.text ("→ " <> f.unit <> " " <> show (i + 1)) ]
          ]
      , HH.div [ HP.class_ (HH.ClassName "friend-layers") ]
          ( (if Array.null lp.shapes && not (Socket.isWriting lp)
              then [ HH.div [ HP.class_ (HH.ClassName "friend-layer is-empty") ] [ HH.text "empty" ] ]
              else Array.mapWithIndex (layerRow i lp) lp.shapes)
            <> (if Socket.isWriting lp then [ recordingRow top lp ] else []) )
      , HH.div [ HP.class_ (HH.ClassName "friend-loop-buttons") ]
          -- No Overdub beside Record: on this page Record already reads the
          -- loop's state and says what the next press does, and the one thing
          -- Overdub adds — refusing a first take — is a footswitch's need, not
          -- a button's. The duty stays in the vocabulary for the pedalboard.
          --
          -- Two ways to record where the module wants one length. The left
          -- button is the module's own gesture: on an empty loop a take of
          -- the face's seconds, on a loop with material another layer of the
          -- loop's length — both closed by the daemon. The right one records
          -- open-ended and is only for an empty loop; with material in the
          -- loop every layer is the loop's length, so it is drawn disabled
          -- rather than removed — the row must not shift.
          --
          -- And a third, on a face whose layers are alternates: **Sum** is
          -- sound-on-sound, a held `r` on an alternate loop with material,
          -- which the daemon sums into the layer that sounds rather than
          -- opening a new one — "loop N sums into layer K", and on the close
          -- "layer K has another pass". Close while it sums; nothing while
          -- the loop is empty, listening, or writing something else. Add
          -- layer waits while a sum is going down: two takes in one loop at
          -- once is not a thing.
          ( (if f.windowSecs > 0.0
              then [ slabBtn "fix" (fixWord lp)
                       (Do (OnLoop i) (Duty.RecordFixed f.windowSecs)) (Socket.isWriting lp) (lp.armed || summing lp) ]
              else [])
          <> [ slabBtn "rec" (openWord lp) (Do (OnLoop i) Duty.RecordLoop)
                 (Socket.isWriting lp && lp.layers == 0) (f.windowSecs > 0.0 && lp.layers > 0) ]
          <> (if f.alternates
              then [ slabBtn "sum" (if overdubbing lp then "Close" else "Sum") (Do (OnLoop i) Duty.OverdubLoop)
                       (overdubbing lp)
                       (lp.layers == 0 || lp.armed || (Socket.isWriting lp && not (overdubbing lp))) ]
              else [])
          <> [ slabBtn "play" (if lp.state == "playing" && not lp.muted then "Stop" else "Play") (Do (OnLoop i) Duty.Transport) false false
             , slabBtn "undo" "Undo" (Do (OnLoop i) Duty.Undo) false false
             , slabBtn "clear" "Clear" (Do (OnLoop i) Duty.ClearLoop) false false
             , slabBtn "edit" "Edit" (ToggleEdit i) (st.panel == EditPanel && st.focus == i) false
             , slabBtn "notes" "Notes" (NotesFor i) (st.panel == NotesPanel && st.focus == i) false
             ] )
      ]

  layerRow i lp k sh =
    -- A layer is a handle too: drag one onto an empty loop and it goes alone.
    HH.div
      [ HP.class_ (HH.ClassName ("friend-layer" <> (if sh.on then "" else " is-off") <> (if f.alternates then " is-solo" else "")))
      , HP.draggable true
      , HP.title "drag onto an empty loop to copy this layer"
      , HE.onDragStart \_ -> StartDrag i (Just (k + 1))
      , HE.onDragEnd \_ -> EndDrag
      ]
      ( (if f.alternates then []
          else
            [ HH.input
                [ HP.type_ HP.InputCheckbox
                , HP.class_ (HH.ClassName "loop-layer-on")
                , HP.checked sh.on
                , HP.title (f.layerWord <> " " <> show (k + 1) <> (if sh.on then ", in the mix" else ", out of the mix"))
                , HE.onChecked (SetLayer i (k + 1))
                ]
            ])
        -- On a face whose layers are alternates, the letter and the envelope
        -- are the Layer knob: a click makes this the one that sounds.
        <> [ HH.span
               ( [ HP.class_ (HH.ClassName "friend-layer-n") ]
                   <> (if f.alternates then [ HE.onClick \_ -> Hear i (k + 1), HP.title soloWord ] else []) )
               [ HH.text (show (k + 1)) ]
           , HH.div
               ( [ HP.class_ (HH.ClassName "friend-layer-wave") ]
                   <> (if f.alternates then [ HE.onClick \_ -> Hear i (k + 1), HP.title soloWord ] else []) )
               (wave (viewOf lp sh))
           ]
      )
    where
    soloWord = "hear " <> f.layerWord <> " " <> show (k + 1) <> " alone"

  -- **The layer being written, as a bar filling.** A fixed first take has
  -- its length before it starts and its bar fills towards it; an open one
  -- has none and just counts; an overdub's bar is the play head across the
  -- loop. The row sits where the layer will, so it takes that
  -- layer's letter and colour.
  recordingRow top lp =
    let
      sr = Int.toNumber top.sampleRate
      linear = case Socket.phaseOf lp of
        Socket.Overdubbing -> lp.recFrames > 0
        _ -> true
      -- A one-pass layer reports how far its pass has come; an open overdub
      -- reports zero and the bar is the play head across the loop.
      elapsed = if linear || lp.recFrames > 0 then lp.recFrames else lp.pos
      -- A fixed take's loop already has its length while it records, so the
      -- bar fills towards it; an open take's is zero, and the bar just counts.
      ref = lp.loopFrames
      pct = if ref > 0 then min 100.0 (Int.toNumber elapsed / Int.toNumber ref * 100.0) else 100.0
      word = secs (Int.toNumber elapsed / sr) <> " s" <> (if ref > 0 then " of " <> secs (Int.toNumber ref / sr) else "")
    in
      HH.div [ HP.class_ (HH.ClassName "friend-layer is-writing") ]
        [ HH.span [ HP.class_ (HH.ClassName "friend-layer-n") ] [ HH.text (show (lp.layers + 1)) ]
        , HH.div [ HP.class_ (HH.ClassName "friend-layer-bar") ]
            [ HH.div [ HP.class_ (HH.ClassName ("friend-layer-bar-fill" <> (if ref > 0 then "" else " is-open"))), HP.style ("width:" <> show pct <> "%") ] []
            , HH.span [ HP.class_ (HH.ClassName "friend-layer-bar-word") ] [ HH.text word ]
            ]
        ]

  btn label act on =
    HH.button
      [ HP.class_ (HH.ClassName ("friend-btn" <> (if on then " on" else "")))
      , HE.onClick \_ -> act
      ]
      [ HH.text label ]

  -- A slab button carries a name in its class, so the skin can give each
  -- its glyph without counting positions — a count that moves whenever a
  -- face adds or drops a button.
  slabBtn key label act on off =
    HH.button
      [ HP.class_ (HH.ClassName ("friend-btn friend-btn-" <> key <> (if on then " on" else "")))
      , HP.disabled off
      , HE.onClick \_ -> act
      ]
      [ HH.text label ]

  -- | **What the whole page is listening to.**
  -- |
  -- | The daemon's own model is per loop — `<n>src<i>`, and it has to be,
  -- | because `ClaimPast` reaches back into a *source's* pre-roll ring and
  -- | "the last eight seconds" is ambiguous the moment two loops want
  -- | different pasts. The pedalboard uses that: drums off the iPad on two
  -- | loops, a bass on another, the stereo board on the rest, each loop
  -- | remembering its own.
  -- |
  -- | This page is not that. Here you are harvesting *one thing* — the
  -- | modular today, the guitar tomorrow — and eight per-loop choices would
  -- | be eight copies of a decision made once a session. So it is one
  -- | control, and it broadcasts: every loop gets the `src` verb.
  -- |
  -- | It does not lie about the underlying model. If something else moved a
  -- | single loop — the pedalboard on the same daemon — the control says
  -- | *mixed* rather than showing one source and meaning another, and the
  -- | next click puts them back together.
  -- |
  -- | Not disabled while recording. Changing the input mid-take is a strange
  -- | thing to want and a legitimate one to reach for, and the daemon takes
  -- | the verb whenever it is sent; foreclosing it here would be this page
  -- | inventing a rule the engine does not have.
  sourceBar top
    | Array.length top.sources <= 1 = HH.text ""
    | otherwise =
        let here = Array.nub (map _.src top.loops)
            one = case here of
              [ n ] -> Just n
              _ -> Nothing
        in HH.div [ HP.class_ (HH.ClassName "friend-source") ]
             ( [ HH.span [ HP.class_ (HH.ClassName "friend-source-label") ]
                   [ HH.text (if one == Nothing then "hearing (mixed)" else "hearing") ]
               ]
               <> Array.mapWithIndex (srcChip one) top.sources )

  srcChip one n src =
    HH.button
      [ HP.class_ (HH.ClassName ("friend-src" <> (if one == Just (n + 1) then " on" else "")))
      , HP.title ("every loop hears " <> src.name
                    <> (if src.mono then " (mono)" else " (stereo)"))
      , HE.onClick \_ -> SetSourceAll (n + 1)
      ]
      [ HH.text src.name ]

  -- The open record's word on a face with a fixed length says it is the open
  -- one; everywhere else it is the record word as it was.
  openWord lp = case recordWord lp of
    "Record" | f.windowSecs > 0.0 -> "Record open"
    w -> w

  -- The fixed button says what the daemon will do: a take of the face's
  -- seconds, another layer, or — while writing — the close.
  fixWord lp
    | Socket.isWriting lp = recordWord lp
    | lp.layers > 0 = "Add layer"
    | otherwise = "Record " <> show (Int.round f.windowSecs) <> "s"

  overdubbing lp = Socket.phaseOf lp == Socket.Overdubbing

  -- An open overdub — the sum — reports no pass length; a one-pass layer
  -- (Add layer) reports how far its pass has come. Same test the recording
  -- row draws its bar by.
  summing lp = overdubbing lp && lp.recFrames == 0

  controls top =
    HH.section [ HP.class_ (HH.ClassName "friend-controls") ]
      [ btn (if top.click then "Click on" else "Click off") (Do Focused Duty.ClickToggle) top.click
      , btn "Stop all" (Do Focused Duty.StopAll) false
      , btn "Clear all" (Do Focused Duty.ClearAll) false
      , HH.span [ HP.class_ (HH.ClassName "friend-gap") ] []
      , HH.label_ [ HH.text "Take " ]
      , HH.input
          [ HP.type_ HP.InputText
          , HP.class_ (HH.ClassName "friend-take")
          , HP.value st.take
          , HE.onValueInput SetTake
          ]
      , btn "Save take" SaveAll false
      , HH.span [ HP.class_ (HH.ClassName "friend-saved") ]
          [ HH.text (if Array.elem (safeName st.take) st.saved then "saved ✓" else "") ]
      , btn "Notes" (OpenPanel NotesPanel) (st.panel == NotesPanel)
      , btn (if st.session.running then "Session ●" else "Session")
            (OpenSession st.focus) (st.panel == SessionPanel)
      , if f.harvest
          then btn ("Harvest to " <> f.module_) (OpenPanel HarvestPanel) (st.panel == HarvestPanel)
          else HH.text ""
      , HH.span [ HP.class_ (HH.ClassName "friend-note") ]
          [ HH.text
              ("Save writes every loop's layers to ~/.itajara/takes/<take>/loop-<n>/, raw, with one manifest. "
                <> (if f.harvest
                      then "Harvest shapes that onto the stick: a loop is a library bank and a scene, and a datasheet goes with it."
                      else "The " <> f.module_ <> " layout is the next step."))
          ]
      ]

  logView =
    HH.section [ HP.class_ (HH.ClassName "friend-log") ]
      (map (\l -> HH.div_ [ HH.text l ]) (Array.reverse st.log))

  editModal =
    HH.div [ HP.class_ (HH.ClassName "looper-modal-overlay") ]
      [ HH.div [ HP.class_ (HH.ClassName "looper-modal-backdrop"), HE.onClick \_ -> ToggleEdit st.focus ] []
      , HH.div [ HP.class_ (HH.ClassName ("looper-modal is-edit" <> maybe "" (\k -> " is-layer-" <> show k) (activeLayer st st.focus))), HP.attr (HH.AttrName "role") "dialog" ]
          [ HH.button [ HP.class_ (HH.ClassName "looper-modal-close"), HE.onClick \_ -> ToggleEdit st.focus ] [ HH.text "×" ]
          , HH.div [ HP.class_ (HH.ClassName "looper-modal-body") ]
              [ HH.h2_ [ HH.text ("Edit — loop " <> show (st.focus + 1) <> maybe "" (\k -> ", " <> f.layerWord <> " " <> show k) (activeLayer st st.focus)) ]
              , Edit.editPanel editHandlers
                  { focus: st.focus, peaks: st.peaks, local: st.local
                  , fixedFrames: if f.windowSecs > 0.0
                      then map (\top -> Int.round (f.windowSecs * Int.toNumber top.sampleRate)) st.looper
                      else Nothing
                  , layer: activeLayer st st.focus
                  }
                  st.looper
              ]
          ]
      ]

  modal klass title body =
    HH.div [ HP.class_ (HH.ClassName "looper-modal-overlay") ]
      [ HH.div [ HP.class_ (HH.ClassName "looper-modal-backdrop"), HE.onClick \_ -> OpenPanel NoPanel ] []
      , HH.div [ HP.class_ (HH.ClassName ("looper-modal " <> klass)), HP.attr (HH.AttrName "role") "dialog" ]
          [ HH.button [ HP.class_ (HH.ClassName "looper-modal-close"), HE.onClick \_ -> OpenPanel NoPanel ] [ HH.text "×" ]
          , HH.div [ HP.class_ (HH.ClassName "looper-modal-body") ] ([ HH.h2_ [ HH.text title ] ] <> body)
          ]
      ]

  field label value onV =
    HH.label [ HP.class_ (HH.ClassName "friend-field") ]
      [ HH.span_ [ HH.text label ]
      , HH.input [ HP.type_ HP.InputText, HP.value value, HE.onValueInput onV ]
      ]

  -- **Three columns, and what is sounding.** Shelf, scene, layer — the shape
  -- the server discovers rather than one it imposes, so the same columns read
  -- our own takes (`take/loop-3/`) and Instruo's stick image
  -- (`_arbhar_scenes/1_2_scene/`) without either being a special case.
  --
  -- The names are the directories' own. `_arbhar_library_4` is not tidied into
  -- "bank 4" on purpose: what the module shows you is these names, and the
  -- browser is partly here to teach them.
  libraryModal =
    modal "is-library" "Library"
      [ HH.p [ HP.class_ (HH.ClassName "friend-note") ]
          [ HH.text ("Scenes by name, wherever they live. What the "
              <> f.module_ <> " reads is slots — "
              <> f.holds
              <> " — and Harvest is what puts a scene into one. Roots come from ~/.itajara/libraries.json.")
          ]
      , if st.libStatus == "" then HH.text ""
        else HH.p [ HP.class_ (HH.ClassName "friend-lib-status") ] [ HH.text st.libStatus ]
      -- Above the columns, not below them: a scene with a preset pushes the
      -- third column down the page, and what is sounding is the one thing that
      -- must never need scrolling to.
      , player
      , HH.div [ HP.class_ (HH.ClassName "friend-lib") ]
          [ HH.div [ HP.class_ (HH.ClassName "friend-lib-col") ]
              ([ HH.h3_ [ HH.text "Library" ] ] <> shelfRows)
          , HH.div [ HP.class_ (HH.ClassName "friend-lib-col") ]
              ([ HH.h3_ [ HH.text (capital f.unit) ] ]
                <> case openShelf of
                     Nothing -> [ HH.p [ HP.class_ (HH.ClassName "friend-lib-empty") ] [ HH.text "Pick a library." ] ]
                     Just h -> map (sceneRow h) h.scenes)
          , HH.div [ HP.class_ (HH.ClassName "friend-lib-col is-layers") ]
              ([ HH.h3_ [ HH.text (capital f.layerWord) ] ] <> layerRows <> textRows)
          ]
      ]

  openShelf = st.shelfId >>= \k -> Array.find (\h -> h.id == k) st.shelves

  -- The library's name once per run, not once per row. Seven shelves that all
  -- begin "Instruo — Arbhar 2.0" spend the whole width on the part they share
  -- and truncate the part that tells them apart, which is the wrong way round.
  shelfRows = Array.concat (Array.mapWithIndex shelfGroup st.shelves)

  shelfGroup i h =
    (if map _.lib (Array.index st.shelves (i - 1)) == Just h.lib then []
     else [ HH.div [ HP.class_ (HH.ClassName "friend-lib-head") ] [ HH.text h.libName ] ])
      <> [ shelfRow h ]

  shelfRow h =
    HH.button
      [ HP.class_ (HH.ClassName ("friend-lib-row" <> if Just h.id == st.shelfId then " is-on" else ""))
      , HE.onClick \_ -> PickShelf h.id
      ]
      [ HH.span [ HP.class_ (HH.ClassName "friend-lib-name") ] [ HH.text h.name ]
      , HH.span [ HP.class_ (HH.ClassName "friend-lib-meta") ] [ HH.text (show (Array.length h.scenes)) ]
      ]

  sceneRow h sc =
    HH.button
      [ HP.class_ (HH.ClassName ("friend-lib-row" <> if Just sc.path == map _.path st.openScene then " is-on" else ""))
      , HE.onClick \_ -> PickScene h.lib sc.path sc.name
      ]
      [ HH.span [ HP.class_ (HH.ClassName "friend-lib-name") ] [ HH.text sc.name ]
      , HH.span [ HP.class_ (HH.ClassName "friend-lib-meta") ] [ HH.text (show (Array.length sc.layers)) ]
      ]

  layerRows = case st.openScene of
    Nothing -> [ HH.p [ HP.class_ (HH.ClassName "friend-lib-empty") ] [ HH.text ("Pick a " <> f.unit <> ".") ] ]
    Just sc
      | Array.null st.sceneAt.layers ->
          [ HH.p [ HP.class_ (HH.ClassName "friend-lib-empty") ] [ HH.text "reading…" ] ]
      | otherwise -> map (libLayerRow sc) st.sceneAt.layers

  libLayerRow sc l =
    let url = Library.audioUrl sc.lib (if sc.path == "" then l.name else sc.path <> "/" <> l.name)
    in HH.button
         [ HP.class_ (HH.ClassName ("friend-lib-row" <> if Just url == map _.url st.hearing then " is-on" else ""))
         , HE.onClick \_ -> Audition url (sc.name <> " / " <> l.name)
         ]
         [ HH.span [ HP.class_ (HH.ClassName "friend-lib-play") ] [ HH.text "\x25b6" ]
         , HH.span [ HP.class_ (HH.ClassName "friend-lib-name") ] [ HH.text l.name ]
         , HH.span [ HP.class_ (HH.ClassName "friend-lib-meta") ] [ HH.text (layerMeta l) ]
         ]

  -- A zero means the header did not say, and the page prints nothing rather
  -- than a plausible lie.
  layerMeta l = String.joinWith " · " (Array.catMaybes
    [ if l.secs > 0.0 then Just (secs l.secs <> " s") else Nothing
    , if l.rate > 0 then Just (show (l.rate / 1000) <> "k") else Nothing
    , if l.bits > 0 then Just (show l.bits <> " bit") else Nothing
    , if l.channels > 0 then Just (if l.channels == 1 then "mono" else "stereo") else Nothing
    ])

  -- An Arbhar scene's preset is a text file whose *name* is the preset's, so
  -- the name is shown as loudly as the contents.
  textRows = case st.openScene of
    Nothing -> []
    Just _ -> map
      (\t -> HH.div [ HP.class_ (HH.ClassName "friend-lib-text") ]
        [ HH.strong_ [ HH.text t.name ]
        , HH.pre_ [ HH.text t.content ]
        ])
      st.sceneAt.texts

  player = case st.hearing of
    Nothing -> HH.text ""
    Just h ->
      HH.div [ HP.class_ (HH.ClassName "friend-lib-player") ]
        [ HH.span [ HP.class_ (HH.ClassName "friend-lib-now") ] [ HH.text h.name ]
        , HH.audio [ HP.src h.url, HP.controls true, HP.autoplay true ] []
        ]

  -- **What only the player knows.** The daemon's facts — length, bars,
  -- tempo, source — go on the datasheet by themselves; these are the rest.
  -- | **The conductor.** Many takes of one sound, counted, on one loop.
  -- |
  -- | Deliberately small: three fields and two buttons, because the work it
  -- | does is pressing record and the interesting decisions were all made
  -- | elsewhere — by the daemon, which waits for the sound, and by `msm`,
  -- | which trims what lands. What is left here is the count and the prompt.
  sessionModal =
    let ses = st.session
        lp = st.looper >>= \top -> Array.index top.loops ses.loop
        done = maybe 0 (\l -> l.layers - ses.base) lp
        left = ses.want - done
        listening = maybe false _.armed lp
        writing = maybe false Socket.isWriting lp
    in modal "is-session" ("Session — loop " <> show (ses.loop + 1))
      [ HH.div [ HP.class_ (HH.ClassName "friend-fields") ]
          [ field "Playing" ses.label SetSessionLabel
          , field "How many" (show ses.want) SetSessionWant
          , field "Seconds each" (show ses.secs) SetSessionSecs
          -- **Which input, for this loop alone.** The bar at the top of the
          -- page points every loop at one source, which is what a performance
          -- wants; a capture session is one voice off one jack while the rest
          -- of the rig keeps hearing what it was hearing.
          , case st.looper of
              Just top | Array.length top.sources > 1 ->
                HH.label [ HP.class_ (HH.ClassName "friend-field") ]
                  [ HH.span_ [ HH.text "Recording from" ]
                  , HH.select [ HE.onValueChange (\v -> SetSourceOne ses.loop (fromMaybe 1 (Int.fromString v))) ]
                      (Array.mapWithIndex
                        (\n src -> HH.option
                          [ HP.value (show (n + 1))
                          , HP.selected (maybe false (\l -> l.src == n + 1) lp)
                          ]
                          [ HH.text (src.name <> (if src.mono then " (mono)" else " (stereo)")) ])
                        top.sources)
                  ]
              _ -> HH.text ""
          ]
      , HH.p [ HP.class_ (HH.ClassName "friend-note") ]
          [ HH.text
              ("Press Start and play. The loop waits for a sound, records "
                <> show ses.secs <> "s from it, closes itself, and waits again — "
                <> "so the takes are yours to time and the counting is not.")
          ]
      , HH.p [ HP.class_ (HH.ClassName "friend-note") ]
          [ HH.strong_ [ HH.text "Softest first." ]
          , HH.text
              (" The module reads a stack in order and velocity picks along it, "
                <> "so the order you play them in is the order they answer to. "
                <> "Nothing downstream reorders them and nothing levels them.")
          ]
      , HH.p [ HP.class_ (HH.ClassName "friend-session-count") ]
          [ HH.text
              (if ses.running
                 then (if writing then "recording " else if listening then "listening for " else "next: ")
                        <> ses.label <> " " <> show (min ses.want (done + 1))
                        <> " of " <> show ses.want
                 else if done > 0 then show done <> " × " <> ses.label <> " on this loop"
                 else "not started")
          ]
      , HH.div [ HP.class_ (HH.ClassName "friend-session-pips") ]
          (map (\n -> HH.span
                  [ HP.class_ (HH.ClassName
                      ("friend-pip" <> if n <= done then " is-done" else "")) ]
                  [ HH.text "" ])
              (Array.range 1 (max 1 ses.want)))
      , HH.div [ HP.class_ (HH.ClassName "looper-edit-actions") ]
          [ if ses.running
              then btn "Stop" StopSession false
              else btn "Start" StartSession false
          , HH.span [ HP.class_ (HH.ClassName "looper-edit-note") ]
              [ HH.text
                  (if ses.running
                     then show left <> " to go — Stop takes the loop off listening"
                     else "records into loop " <> show (ses.loop + 1)
                            <> ", which is voice " <> show (ses.loop + 1)
                            <> " of the kit")
              ]
          ]
      ]

  notesModal =
    modal "is-notes" ("Notes — " <> safeName st.take)
      [ HH.div [ HP.class_ (HH.ClassName "friend-fields") ]
          [ field "Title" st.notes.title (SetNote NTitle)
          , field "Key" st.notes.key (SetNote NKey)
          , field "BPM" st.notes.bpm (SetNote NBpm)
          , field "Timbre" st.notes.timbre (SetNote NTimbre)
          , field "Intended use" st.notes.uses (SetNote NUses)
          , field "Tags" st.notes.tags (SetNote NTags)
          ]
      , HH.label [ HP.class_ (HH.ClassName "friend-field is-wide") ]
          [ HH.span_ [ HH.text "Notes" ]
          , HH.textarea [ HP.value st.notes.notes, HP.rows 3, HE.onValueInput (SetNote NNotes) ]
          ]
      , HH.table [ HP.class_ (HH.ClassName "friend-loop-notes") ]
          [ HH.thead_ [ HH.tr_ [ HH.th_ [ HH.text "loop" ], HH.th_ [ HH.text "title" ], HH.th_ [ HH.text "key" ], HH.th_ [ HH.text "timbre" ], HH.th_ [ HH.text "use / notes" ] ] ]
          , HH.tbody_ (map loopNoteRow (Array.range 1 (maybe 0 (Array.length <<< _.loops) st.looper)))
          ]
      , HH.div [ HP.class_ (HH.ClassName "looper-edit-actions") ]
          [ btn "Save notes" SaveNotes false
          , HH.span [ HP.class_ (HH.ClassName "looper-edit-note") ] [ HH.text st.notesStatus ]
          ]
      ]

  loopNoteRow i =
    let r = loopNote i st.notes.loops
        cell fld v = HH.td_ [ HH.input [ HP.type_ HP.InputText, HP.value v, HE.onValueInput (SetLoopNote i fld) ] ]
    in HH.tr [ HP.class_ (HH.ClassName (if i == st.focus + 1 then "is-focus" else "")) ]
      [ HH.td_ [ HH.text (show i <> (if hasMaterial i then "" else " (empty)")) ]
      , cell LTitle r.title
      , cell LKey r.key
      , cell LTimbre r.timbre
      , cell LNotes r.notes
      ]

  hasMaterial i = maybe false (\top -> maybe false (\lp -> lp.layers > 0) (Array.index top.loops (i - 1))) st.looper

  -- **The stick, and where on it.** A loop is a library bank and a scene;
  -- the form says which bank and scene the first loop takes and the rest
  -- follow. Dry run first is cheap and says exactly what would land where.
  -- Where a take LANDS differs by module, and by more than a label: the Arbhar
  -- addresses positionally (a library bank, a scene) while the Rample addresses
  -- by kit. The card is not asked for — msm finds the mounted one, and refuses
  -- if there are two — but the KIT is, because which kit to overwrite is not a
  -- thing to guess at.
  whereItGoes
    | f.id == "rample" =
        [ field "Kit (A0 … Z99)" st.slot SetSlot
        , HH.label [ HP.class_ (HH.ClassName "friend-field") ]
            [ HH.span_ [ HH.text "The material is" ]
            , HH.select [ HE.onValueChange SetKindAs ]
                (map (\o -> HH.option [ HP.value o.v, HP.selected (o.v == st.kindAs) ] [ HH.text o.label ])
                  -- The first two become LAYERS, picked by the layer selector
                  -- and capped at twelve; the last two become SLICES, joined
                  -- into equal slots and addressed by the start point. Which
                  -- axis a kind wants is a fact about the music, so it is the
                  -- kind that is chosen here and never the axis.
                  [ { v: "drum-kit", label: "hits — dynamics of one drum (layers, velocity picks)" }
                  , { v: "progressions", label: "progressions — long takes, kept whole (layers)" }
                  , { v: "chords", label: "chords — one per capture, cut to slices" }
                  , { v: "break", label: "a break — one bar, cut into its hits (slices)" }
                  ])
            ]
        ]
    | otherwise =
        [ HH.label [ HP.class_ (HH.ClassName "friend-field") ]
            [ HH.span_ [ HH.text "Stick" ]
            , if Array.null st.sticks
                then HH.span [ HP.class_ (HH.ClassName "friend-warn") ] [ HH.text "no mounted volume has an _arbhar_library folder" ]
                else HH.select [ HE.onValueChange SetStick ]
                  (map (\p -> HH.option [ HP.value p, HP.selected (p == st.stick) ] [ HH.text p ]) st.sticks)
            ]
        , field "First library bank (1–6)" st.bank SetBank
        , field "First scene (1_1 … 6_6)" st.scene SetScene
        ]

  harvestModal =
    modal "is-harvest" ("Harvest " <> safeName st.take <> " to the " <> f.module_)
      ( (if Array.elem (safeName st.take) st.saved then []
          else [ HH.p [ HP.class_ (HH.ClassName "friend-warn") ] [ HH.text "This take has not been saved yet — Save take first." ] ])
      <> [ HH.div [ HP.class_ (HH.ClassName "friend-fields") ]
            ( whereItGoes <>
              [ HH.label [ HP.class_ (HH.ClassName "friend-field is-check") ]
                  [ HH.input [ HP.type_ HP.InputCheckbox, HP.checked st.overwrite, HE.onChecked SetOverwrite ], HH.span_ [ HH.text "Overwrite slots already holding audio" ] ]
              , HH.label [ HP.class_ (HH.ClassName "friend-field is-check") ]
                  [ HH.input [ HP.type_ HP.InputCheckbox, HP.checked st.allLayers, HE.onChecked SetAllLayers ], HH.span_ [ HH.text "Include layers switched off" ] ]
              ]
            )
        , HH.div [ HP.class_ (HH.ClassName "looper-edit-actions") ]
            [ btn "Refresh sticks" RefreshSticks false
            , btn "Dry run" (RunHarvest true) false
            , btn ("Harvest to " <> f.module_) (RunHarvest false) st.harvestBusy
            , HH.span [ HP.class_ (HH.ClassName "looper-edit-note") ]
                [ HH.text
                    ( if f.id == "rample" then
                        "Loop 1 becomes voice 1 — a kit with no voice 1 cannot be opened at all. "
                          <> "Twelve layers per voice is a hard ceiling; a thirteenth is dropped in silence, "
                          <> "so the rest are refused here instead. Mono, 16-bit at 44.1 kHz, because a "
                          <> "stereo sample fills two voices. The layer mode is written to the card's .rpl, "
                          <> "which is what stops it having to be set by hand after every power-off."
                      else
                        "Each loop takes one bank and one scene, ten seconds plus the three that follow, "
                          <> "24-bit at 48 kHz. The datasheet lands in the take and in _harvest/ on the stick."
                    ) ]
            ]
        , HH.pre [ HP.class_ (HH.ClassName "friend-harvest-out") ] [ HH.text st.harvestOut ]
        ]
      )

  editHandlers =
    { windowIn: WindowIn
    , windowOut: WindowOut
    , clearWindow: ClearWindow
    , shiftStart: ShiftStart
    , askPeaks: AskPeaks
    , windowTo: SetWindow
    , layerWindowTo: SetLayerWindow
    , clearLayerWindow: ClearLayerWindow
    , editDone: EditDone
    , waveDrag: Just { down: WaveDown, move: WaveMove, up: WaveUp }
    }

-- | The record button says what the next press does, because `r` is one
-- | verb that opens, closes, overdubs or cancels depending on the loop.
recordWord :: LoopState -> String
recordWord lp = case Socket.phaseOf lp of
  Socket.Armed -> "Cancel arm"
  Socket.RecordingFirst -> "Close"
  Socket.Overdubbing -> "End overdub"
  Socket.Multiplying -> "End multiply"
  Socket.Playing -> "Overdub"
  Socket.Idle -> if lp.layers > 0 then "Overdub" else "Record"

stateWord :: LoopState -> String
stateWord lp = case Socket.phaseOf lp of
  Socket.Armed -> "armed"
  Socket.RecordingFirst -> "recording"
  Socket.Overdubbing -> "overdubbing"
  Socket.Multiplying -> "multiplying"
  -- Undo of the last layer leaves the loop turning with a length and
  -- nothing in it: sized, not playing — the daemon's word for it.
  Socket.Playing
    | lp.sized -> "empty"
    | lp.muted -> "muted"
    | otherwise -> "playing"
  Socket.Idle -> if lp.layers > 0 then "stopped" else "empty"

phaseClass :: LoopState -> String
phaseClass lp = case Socket.phaseOf lp of
  Socket.Armed -> "is-armed"
  Socket.RecordingFirst -> "is-recording"
  Socket.Overdubbing -> "is-recording"
  Socket.Multiplying -> "is-recording"
  Socket.Playing -> if lp.muted then "is-muted" else "is-playing"
  Socket.Idle -> if lp.layers > 0 then "is-stopped" else "is-empty"

lengthWord :: LooperState -> LoopState -> String
lengthWord top lp
  | lp.loopFrames <= 0 = ""
  | top.barFrames > 0 && lp.quant =
      let bars = Int.toNumber lp.loopFrames / Int.toNumber top.barFrames
      in secs lp.loopSecs <> " s · " <> secs bars <> " bars"
  | otherwise = secs lp.loopSecs <> " s"

secs :: Number -> String
secs n = show (Int.toNumber (Int.round (n * 10.0)) / 10.0)

-- | A face's words are lower case because they are used in sentences; a
-- | column heading wants one capital and no other change.
capital :: String -> String
capital s = String.toUpper (String.take 1 s) <> String.drop 1 s
