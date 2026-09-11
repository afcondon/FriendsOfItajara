-- | **Quadrat** — sample an instrument at chosen points, and keep what comes
-- | back.
-- |
-- | A quadrat is the square frame a field survey lays down before recording
-- | everything inside it: you cannot walk the whole meadow, so you choose
-- | where to look and you look there thoroughly. That is the whole method
-- | here. A module has more parameter space than anyone can play through, so
-- | you cut a **transect** across it — N curves through N dimensions — and
-- | sample it at the points the destination can address.
-- |
-- | **Two tasks, and they are opposites.** Finding the transect is slow, one
-- | judgement at a time, with you listening, and it fails by not converging.
-- | Harvesting it is as fast as the instrument allows, with nobody present,
-- | and it fails *silently* — you come back to 192 samples and one is wrong.
-- | The first task's output is small, complete, and the second task's only
-- | input, which is what lets a harvest run unattended, repeatedly, on
-- | another machine, at a resolution nobody chose at the time.
-- |
-- | That is also why the measurements are not a nicety: an unattended run has
-- | to be able to say "these twelve are identical" without you there.
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
-- | So the daemon is used as little as possible — and since 2026-09-10 it is
-- | not used as a *looper* at all. It records through `cap`, a capture beside
-- | the engine that has a start and an end and no other state. Before that
-- | this page drove loop 7 and spent four verbs turning the looper off to do
-- | it: `Alternates false`, `Sounding false`, `OnGrid false`, and a `Clear`
-- | before every take. That mismatch cost a take — see `captureOn` below —
-- | and those four verbs are exactly what capture removed.
module Quadrat.Main where

import Prelude

import Control.Monad.Rec.Class (forever)
import Data.Array as Array
import Data.Foldable (for_)
import Data.Int as Int
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Maybe as Maybe
import Data.Nullable as Nullable
import Data.Number as Number
import Data.String as String
import Data.String.Common (joinWith)
-- `Bars` names a thing in both vocabularies — a length in the daemon's verbs
-- and a kind of material here — so the verbs come in by name and the kinds
-- through `Kind.`.
import Data.Looper.Verb (Verb(Capture, CaptureArm, CapturePeaks, CaptureStop, EndCapture, WriteCapture))
import Data.Looper.Verb as Verb
import Effect (Effect)
import Effect.Aff (Milliseconds(..), attempt, delay)
import Effect.Aff as Aff
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Foreign.LooperSocket (Capture, LooperState)
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
import Quadrat.Audio as Audio
import Quadrat.Http as Http
import Quadrat.Wave as Wave
import Quadrat.Kind (Close(..), Fold(..), Kind)
import Quadrat.Kind as Kind
import Quadrat.Slug (slugFor)
import Quadrat.Divider (Divider)
import Quadrat.Divider as Divider
import Quadrat.Rig as Rig
import Quadrat.Encoding as Encoding
import Quadrat.Schedule as Schedule
import Quadrat.Pitch as Pitch
import Quadrat.Sweep as Sweep
import Quadrat.SweepView as SweepView

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI component unit body


-- | **The two halves of the tool, and they are opposites.**
-- |
-- | Finding a transect is slow, one judgement at a time, with you listening,
-- | and it fails by not converging. Harvesting it is as fast as the instrument
-- | allows, with nobody present, and it fails *silently*. They want different
-- | things on screen and they are never done at the same moment, so they are
-- | two pages and not two panels.
data Page = Bench | Library

derive instance Eq Page

-- | **Two ways to fill a take**, sharing everything downstream of the take.
-- |
-- | A transect is played by the rig from a schedule; by hand is played by you.
-- | What comes back is the same object either way, which is why this chooses
-- | only the left half of the bench and nothing else on the page moves.
data Fill = Swept | Played

derive instance Eq Fill

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
  -- | at each of them. See `Quadrat.Sweep` for why it is a table of values
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
  -- | `Quadrat.Schedule` for why that difference matters more at 192 hits
  -- | than at twelve.
  , schedule :: Array Number
  -- | **The sets already on disk**, newest first. Read from the server rather
  -- | than remembered here: a set outlives the page by a long way, which is
  -- | the entire point of storing it.
  , sets :: Array Http.SetRow
  -- | The calibration tables `deepstar serve` knows about, and why the list is
  -- | empty when it is. Fetched once at init: they change only when someone
  -- | runs `deepstar tune`, and re-asking on every render would be 28 rows of
  -- | nothing new.
  , tables :: Array Http.CalibRow
  , tablesErr :: String
  -- | The capture buffer filled and we have said so once. A latch, not a
  -- | reading: the daemon goes on reporting `full` until the next capture.
  , overran :: Boolean
  , page :: Page
  , fill :: Fill
  -- | **Which cell is open as a preset**, if any.
  -- |
  -- | The transpose of `sweepEdit`. That opens one PARAMETER and shows its
  -- | value at every position — a row of the table. This opens one POSITION
  -- | and shows every parameter's value there — a column. Same table, same
  -- | edits, and the second one is how you fix the one hit that came out
  -- | wrong without hunting through eight drawers to do it.
  , pivot :: Maybe Int
  -- | **Which input the next take listens to**, 1-based, or zero for the
  -- | first available.
  -- |
  -- | Held here because a capture has no source until it starts, so there is
  -- | nothing to read it back from. That was the shape of an old bug — the
  -- | page kept its own idea of the input and asserted it at Arm, so a reload
  -- | silently armed on the wrong one. The answer is not to hide the choice
  -- | but to put it where it cannot be missed: in the masthead, with the
  -- | level it is reading right now, and named on the button that uses it.
  , source :: Int
  -- | **Has this take been written as a set yet?**
  -- |
  -- | A run always leaves a take on disk, and until 2026-09-11 nothing on the
  -- | page distinguished that from having kept it — so a take that looked
  -- | finished was one more button away from existing, and the difference was
  -- | invisible. False from the moment a run starts; true only after a set is
  -- | actually written.
  -- | **Which modal is open, if any.** The two fiddly jobs — how the take was
  -- | divided, and where a set goes on a card — are each a handful of controls
  -- | consulted rarely and read never. On the page they crowded the two things
  -- | you look at constantly: the waveform and the sentence.
  -- | **The last few seconds of input level**, for the sparkline beside the
  -- | source. A single bar says how loud it is NOW, which is nothing at all
  -- | between two hits; a short history says whether anything has arrived,
  -- | which is the question actually being asked.
  , levels :: Array Number
  , modal :: Maybe Modal
  , kept :: Boolean
  -- | **The overwrite question, held open.** A set whose name is already taken
  -- | is replaced wholesale (`msm cut --overwrite` deletes the directory
  -- | first), so the one destructive act on this page asks before it acts.
  , confirmKeep :: Boolean
  }

-- | The two panels that became modals.
data Modal = DivisionModal | TriggerModal | PitchModal | ExportModal

derive instance Eq Modal

data Action
  -- | **Make this parameter a pitch**, or (with an empty label) stop it being
  -- | one. An Action rather than a `Sweep.Msg` because it has to FETCH the
  -- | table: the plan carries the measurement, not a pointer to it.
  = PickPitch Int String
  | Init
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
  | SendToCard { place :: Boolean, append :: Boolean }
  -- | **Keep, with the overwrite question asked first.** Falls straight
  -- | through to `SendToCard` when the name is free; otherwise arms the
  -- | confirmation and waits.
  -- | **Choosing an instrument with no pitch parameter yet.** Creates one,
  -- | routed, then hands over to `PickPitch` — so the Pitch section can be
  -- | entered from the Pitch section.
  | AddPitch String
  -- | **The two slots in the statement that are not already one action.**
  -- | `SetPitched` turns the pitch axis on or off; `SetTriggerBy` says which
  -- | interface strikes the instrument, or that you do.
  | OpenModal (Maybe Modal)
  | SetPitched String
  | SetTriggerBy String
  | AskKeep
  | CancelKeep
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
  | RefreshSets
  | GoTo Page
  | FillBy Fill
  | PickSource String
  | OpenPivot (Maybe Int)
  -- | Set every bus for one cell and strike it, so the change you just made
  -- | is audible on the instrument before anything is recorded.
  | Hear Int
  | PlayWhole
  | StopAudio
  | RunAgain String
  | PlaceSet String

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
      , schedule: [], sets: [], tables: [], tablesErr: "", overran: false
      , page: Bench, fill: Swept, source: 0, pivot: Nothing
      , levels: [], modal: Nothing, kept: false, confirmKeep: false }
  , render
  , eval: H.mkEval H.defaultEval { handleAction = handleAction, initialize = Just Init }
  }

-- | **One sender, and no loop to address.**
-- |
-- | Every verb this page sends is rig-wide now: a capture is not a loop, so
-- | there is no number in front of it. Which also means a pedalboard session
-- | on loops 1–6 is untouched by Quadrat and Quadrat by it — the
-- | separation the scratch loop was standing in for, without a loop.
send :: forall o m. MonadAff m => Verb -> H.HalogenM State Action () o m Unit
send v = do
  let c = Verb.render v
  ok <- liftEffect (Socket.send (c <> "@0"))
  unless ok $ H.modify_ (note ("no daemon — " <> c <> " went nowhere"))

note :: String -> State -> State
note m s = s { log = Array.takeEnd 10 (Array.snoc s.log m) }

-- | **What the daemon says the capture is doing** — never a second copy of it.
-- |
-- | The whole of this page's view of the rig, where it used to be a loop with
-- | thirty-odd fields of which four meant anything here.
cap :: State -> Maybe Capture
cap st = map _.capture st.looper

handleAction :: forall o m. MonadAff m => Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  -- | Fetch the table and hang it on the parameter — or, with an empty label,
  -- | take it off. The table is stored IN THE PLAN rather than fetched per
  -- | step, so this is the only moment it is asked for, and the only moment
  -- | that can fail.
  PickPitch i label
    | label == "" -> handleAction (SweepMsg (Sweep.ClearPitch i))
    | otherwise -> do
        r <- H.liftAff (attempt (toAffE (Http.calibration label)))
        case r of
          Left e -> H.modify_ (note ("calibration " <> label <> ": " <> Aff.message e))
          Right t
            -- An empty table would realise every note as 0 V — one flat
            -- transect and nothing on the page to say why — so it is refused
            -- here rather than stored and discovered later.
            | not t.ok || Array.null t.points ->
                H.modify_ (note ("calibration " <> label <> " has no usable points"
                                  <> (if t.error == "" then "" else ": " <> t.error)))
            | otherwise -> do
                st0 <- H.get
                let tlo = Pitch.hzNote (fromMaybe 0.0 (map _.hz (Array.head t.points)))
                    thi = Pitch.hzNote (fromMaybe 0.0 (map _.hz (Array.last t.points)))
                    -- **One cell per semitone, anchored at the bottom of the
                    -- table.**
                    --
                    -- The default used to be the table's whole span, which is
                    -- the right bound and the wrong range: `bia-bass` measures
                    -- G1–D6, 56 degrees, and twelve cells across 56 degrees is
                    -- a step of five semitones — a rising stack of PERFECT
                    -- FOURTHS, played on 2026-09-11 and sounding like a bug in
                    -- the tuning when it was arithmetic doing as it was told.
                    --
                    -- A pitch axis has a natural granularity, so the only
                    -- default that cannot surprise is one degree per cell. The
                    -- span still bounds it: outside the table the realiser
                    -- clamps, and pointing at notes the sweep never measured is
                    -- the commonest mistake with a fresh calibration.
                    cells = max 1 (Encoding.total st0.sweep.extent)
                    lo = tlo
                    hi = min thi (tlo + cells - 1)
                H.modify_ \st -> st
                  { sweep = st.sweep
                      { params = fromMaybe st.sweep.params
                          (Array.modifyAt i
                            (_ { pitch = Just
                                  { label: t.label
                                  , noteLo: lo
                                  , noteHi: hi
                                  , table: t.points } })
                            st.sweep.params) } }
                -- The default range is the table's OWN span, because a range
                -- outside it clamps silently and the commonest mistake with a
                -- newly-picked calibration is to be pointing at notes the sweep
                -- never measured.
                H.modify_ (note (label <> ": " <> Pitch.noteName lo <> "–" <> Pitch.noteName hi
                                  <> " (measured " <> Pitch.noteName tlo <> "–" <> Pitch.noteName thi
                                  <> ", " <> show (Array.length t.points) <> " points)"))
  Init -> do
    n <- liftEffect (slugFor Kind.DrumHits)
    -- The sweep plan as it was left. See `Quadrat.Sweep.restore` — a run,
    -- listen, bend, run again loop cannot survive a page that forgets between
    -- runs, and reloading to pick up a fix is exactly when it forgets.
    pl <- liftEffect (Sweep.restore Sweep.emptyPlan)
    -- **And the schedule of the last run.** The plan survived a reload and the
    -- schedule did not, so a transect reopened after a reload was divided into
    -- equal pieces instead — which is right only if the recording stops at the
    -- last hit, and it never does. Measured 2026-09-11: a 12-cell run at
    -- 3000 ms filled 38.28 s, giving 3.190 s bands against a 3.000 s schedule,
    -- with the twelfth 2.09 s adrift and straddling its neighbour.
    --
    -- Unconditional, because the daemon still holds the audio of that same run
    -- — which is precisely why the take is still divisible after a reload. Any
    -- new capture clears it; see `captureOn`.
    rn <- liftEffect Sweep.loadRun
    -- **An equal division should start from the plan the page is holding.**
    -- 16 is `slicerDivisions`' second entry and was never an answer about THIS
    -- take; a page holding a 12-cell plan offering to cut a take into 16 is
    -- asserting something it has no reason to believe.
    H.modify_ _
      { name = n, sweep = pl, schedule = rn.schedule
      , equalN = clamp 2 128 (Encoding.total pl.extent)
      }
    -- The calibration list, once. Failure is carried as a SENTENCE rather than
    -- as an empty array: "nothing has been measured yet" and "the rig doctor is
    -- not running" are the same empty dropdown and different jobs for you.
    cal <- H.liftAff (attempt (toAffE Http.calibrations))
    H.modify_ case cal of
      Left e -> _ { tables = [], tablesErr = "calibrations unavailable: " <> Aff.message e }
      Right r
        | r.ok -> _ { tables = r.tables, tablesErr = "" }
        | otherwise -> _ { tables = [], tablesErr = "calibrations unavailable — is `deepstar serve` up on :3027?" }
    handleAction RefreshCard
    liftEffect $ Socket.connect Socket.defaultUrl
    void $ H.subscribe $ HS.makeEmitter \emit -> do
      fiber <- Aff.launchAff $ forever do
        delay (Milliseconds 100.0)
        liftEffect (emit Poll)
      pure (Aff.launchAff_ (Aff.killFiber (Aff.error "stopped") fiber))
  Poll -> do
    before <- H.get
    -- The sparkline's history. Capped here rather than at render, so the
    -- array cannot grow without bound over a long session.
    H.modify_ \s0 ->
      s0 { levels = Array.takeEnd 40 (Array.snoc s0.levels (levelNow s0)) }
    snap <- liftEffect Socket.latest
    pk <- liftEffect Socket.latestPeaks
    H.modify_ _ { looper = snap, peaks = pk }
    -- The daemon draws; ask it to, the first time a capture comes into view.
    -- That covers a reload as well as a recording — the daemon did not forget
    -- what it is holding just because the page did.
    now <- H.get
    let had = maybe false _.holds (cap before)
        has = maybe false _.holds (cap now)
    when (has && not had) (send (CapturePeaks 900))
    -- **A capture that closed itself at its count.** One field, where the
    -- looper needed four read together — a loop that stopped recording could
    -- be armed, writing, sized or empty and only the combination said which.
    st <- H.get
    for_ (cap st) \c ->
      when (st.armed && not c.on) $
        H.modify_ (note ("closed itself: " <> fmt c.secs <> " s")
                     <<< _ { armed = false, waiting = true })

    -- The one place a take becomes something to look at, whichever way it
    -- ended — by hand, or at its own count.
    st2 <- H.get
    for_ (cap st2) \c ->
      when (st2.waiting && c.holds) do
        H.modify_ _ { waiting = false }
        handleAction Analyse
    -- **The buffer filled and the rest was dropped.** Loud, because a run
    -- that quietly recorded the first six minutes of a nine-minute grid
    -- produces a set that looks complete.
    filled <- H.get
    for_ (cap filled) \c ->
      when (c.full && not filled.overran) $
        H.modify_ (note ("the capture filled at " <> fmt c.capSecs
                     <> " s and stopped — raise --capture-secs on the daemon")
                     <<< _ { overran = true })
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
    handleAction RefreshSets
  RefreshSets -> do
    r <- H.liftAff (attempt (toAffE Http.storedSets))
    case r of
      Left _ -> pure unit
      Right v -> H.modify_ _ { sets = v.sets }
  -- | **The Rample projection of a stored set.**
  -- |
  -- | Beside "sweep again" because they are the two things you do with a set
  -- | that already exists: make more of it, or send it somewhere. Nothing is
  -- | cut and nothing is measured — the shape the card needs is in the set's
  -- | own description, and re-deriving it from the take would make this a
  -- | second recording rather than a projection.
  PlaceSet nm -> do
    st <- H.get
    H.modify_ _ { cardBusy = true }
    r <- H.liftAff (attempt (toAffE (Http.placeSet
          { set: nm
          , bank: st.bank
          , kit: nm
          , voice: st.voice
          , append: false
          , layerMode: st.layerMode })))
    case r of
      Left e -> H.modify_ (note (Aff.message e) <<< _ { cardBusy = false })
      Right w -> H.modify_ (note (lastLine w.output) <<< _ { cardBusy = false })
    handleAction RefreshCard
  -- | **Run this set again**, at whatever resolution you like.
  -- |
  -- | The whole of what makes a set a stored object rather than a folder: the
  -- | spec comes back as a plan like any other, so changing the extent and
  -- | pressing Run records the same transect at a resolution nobody chose at
  -- | the time. Nothing here remembers how the set was made — it is read off
  -- | the disk, which is the test.
  RunAgain nm -> do
    r <- H.liftAff (attempt (toAffE (Http.loadSpec nm)))
    case r of
      Left e -> H.modify_ (note ("could not read " <> nm <> ": " <> Aff.message e))
      Right v
        | not v.ok -> H.modify_ (note v.output)
        | otherwise -> do
            H.modify_ \st -> st { sweep = Sweep.adopt Sweep.emptyPlan v.spec }
            st <- H.get
            liftEffect (Sweep.remember st.sweep)
            H.modify_ (note ("loaded the spec from " <> nm
                        <> " — change the extent and run it again"))
            handleAction (OpenSweep true)
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
  SendToCard { place, append } -> do
    st <- H.get
    -- Only what you kept, in the order they were played. `msm cut` numbers
    -- them zero-padded from that order, and the module reads a voice's stack
    -- in byte order — so this is the step where "softest first" stops being a
    -- thing you remember and becomes a fact about filenames.
    let keptRegions = Array.catMaybes
          (Array.mapWithIndex
            (\i r -> if Set.member i st.keep then Just r else Nothing)
            st.regions)
        -- **What each kept sample is, and what it meant.**
        --
        -- Zipped by index against the run's own steps, before the keep filter
        -- rather than after: the run's cell 7 is the take's seventh hit
        -- whether or not you kept the first six, and pairing them up after
        -- the filter would relabel every sample past a discard.
        --
        -- Empty for a take played by hand. The measurements are still worth
        -- storing; what is missing is a description of an instrument that was
        -- never driven.
        runSteps = if Array.null st.schedule then [] else Sweep.steps st.sweep
        kept = Array.catMaybes
          (Array.mapWithIndex
            (\i r ->
              if not (Set.member i st.keep) then Nothing
              else Just
                { cell: maybe [] _.cell (Array.index runSteps i)
                , start: r.start, end: r.end
                , peak: r.peak, rms: r.rms, zcr: r.zcr, tilt: r.tilt
                , means: maybe [] _.means (Array.index runSteps i)
                })
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
              , place
              , layerMode: st.layerMode
              , regions: keptRegions
              -- The spec, so the set can be run again; `Plain` because that is
              -- already the form a plan survives in, and a second codec for
              -- the same object is a second thing to keep in step.
              --
              -- Null unless this take was actually run. The plan is sitting
              -- right there and it would be easy to send regardless — and then
              -- every hand-played set would carry a description of a sweep
              -- that never touched it.
              , spec: if Array.null st.schedule
                        then Nullable.null
                        else Nullable.notNull (Sweep.flatten st.sweep)
              , schedule: st.schedule
              , samples: kept })))
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
              , kept = true
              , confirmKeep = false
              }
        handleAction RefreshCard
        handleAction RefreshSets

  -- | **Ask before the one act on this page that destroys something.**
  -- |
  -- | `msm cut --overwrite` deletes the set's whole directory before writing,
  -- | so keeping under a name that is taken is not a merge. The question is
  -- | asked from `sets`, which is already fetched — no round trip, and no
  -- | dialog: the button becomes the question and cancel is beside it.
  -- Empty means "not a pitch run", so it clears rather than doing nothing:
  -- the statement's calibration slot uses the same action for both, and a
  -- dropdown you cannot get back out of is a trap.
  AddPitch label
    | label == "" -> do
        st <- H.get
        case Array.findIndex (\q -> Maybe.isJust q.pitch) st.sweep.params of
          Just i -> handleAction (SweepMsg (Sweep.ClearPitch i))
          Nothing -> pure unit
    | otherwise -> do
        st <- H.get
        case Array.findIndex (\q -> Maybe.isJust q.pitch) st.sweep.params of
          -- Retarget the pitch there already is, rather than growing a second
          -- one: two pitch axes on one transect is a thing to mean deliberately
          -- and never a thing to arrive at by using a dropdown.
          Just i -> handleAction (PickPitch i label)
          Nothing -> do
            handleAction (SweepMsg Sweep.AddParam)
            st2 <- H.get
            let i = Array.length st2.sweep.params - 1
            handleAction (SweepMsg (Sweep.SetName i "pitch"))
            -- **Routed on arrival.** Bus 8 is ES-9 panel jack 1. A parameter
            -- that moves nothing is the failure this section exists to avoid,
            -- and an unrouted pitch axis looks identical to a working one
            -- until the take comes back silent.
            handleAction (SweepMsg (Sweep.SetCv i "8"))
            handleAction (PickPitch i label)

  OpenModal m -> H.modify_ _ { modal = m }

  SetPitched v -> do
    st <- H.get
    case v, Array.findIndex (\q -> Maybe.isJust q.pitch) st.sweep.params of
      "unpitched", Just i -> handleAction (SweepMsg (Sweep.ClearPitch i))
      "pitched", Nothing ->
        case Array.head st.tables of
          Just t -> handleAction (AddPitch t.label)
          Nothing -> H.modify_ (note "no calibration tables — run `deepstar tune` first")
      _, _ -> pure unit

  -- | **Which interface strikes the instrument.**
  -- |
  -- | Three real paths exist — an es9-daemon bus, one of the ES-5's own gates,
  -- | and a MIDI note. The FH-2 is not a fourth: it is a MIDI module, reached
  -- | on its own port, so it is offered as a named preset of the MIDI path
  -- | rather than as a device the rig does not separately have.
  SetTriggerBy v -> case v of
    "hand" -> handleAction (FillBy Played)
    "es9" -> do
      handleAction (FillBy Swept)
      st <- H.get
      when (Maybe.isNothing st.sweep.trigger.gate)
        (handleAction (SweepMsg (Sweep.SetGate "15")))
      handleAction (SweepMsg (Sweep.SetEs5 ""))
      handleAction (SweepMsg (Sweep.SetNote ""))
    "es5" -> do
      handleAction (FillBy Swept)
      st <- H.get
      when (Maybe.isNothing st.sweep.trigger.es5)
        (handleAction (SweepMsg (Sweep.SetEs5 "0")))
      handleAction (SweepMsg (Sweep.SetGate ""))
      handleAction (SweepMsg (Sweep.SetNote ""))
    _ -> do
      handleAction (FillBy Swept)
      st <- H.get
      when (Maybe.isNothing st.sweep.trigger.note)
        (handleAction (SweepMsg (Sweep.SetNote "60")))
      handleAction (SweepMsg (Sweep.SetGate ""))
      handleAction (SweepMsg (Sweep.SetEs5 ""))
      when (v == "fh2") $
        case Array.find (String.contains (String.Pattern "FH-2")) st.midiPorts of
          Just o -> handleAction (SweepMsg (Sweep.SetPort o))
          Nothing -> H.modify_ (note "no MIDI port named FH-2 — is the module on?")

  AskKeep -> do
    st <- H.get
    let setName = if st.name == "" then "set" else st.name
    if Array.any (\r -> r.name == setName) st.sets
      then H.modify_ _ { confirmKeep = true }
      else handleAction (SendToCard { place: false, append: false })

  CancelKeep -> H.modify_ _ { confirmKeep = false }

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
  -- | **The transect end to end**, which is the one listen the tiles cannot
  -- | give you: twelve samples heard in order, with the gaps, is how you hear
  -- | a sweep as a sweep rather than as twelve sounds.
  PlayWhole -> do
    st <- H.get
    when (st.showing /= "") $ liftEffect
      (Audio.playRange ("/api/take-audio?take=" <> st.showing) 0.0
        (maybe 1.0e6 _.secs (cap st)))
    H.modify_ _ { playing = Nothing }
  OpenPivot j -> do
    H.modify_ _ { pivot = j }
    -- Opening a preset leaves the instrument standing at it, so the first
    -- thing you hear after a tweak is that tweak and not the last position of
    -- the last run. Nothing is struck until you ask.
    for_ j \k -> setCellCv k
  -- | **Hear one cell**, exactly as the run would play it: set the buses,
  -- | wait the settle the plan specifies, strike with the plan's own trigger.
  -- |
  -- | The same three lines `runSweep` uses, which is the point — a preview
  -- | that differed from the run in any particular would be a preview of
  -- | something else.
  Hear j -> do
    setCellCv j
    st <- H.get
    H.liftAff (delay (Milliseconds (Int.toNumber st.sweep.settleMs)))
    for_ st.sweep.trigger.gate \b -> void $ H.liftAff (attempt (toAffE (Rig.pulse
      { bus: b, level: st.sweep.trigger.gateLevel, ms: st.sweep.trigger.ms })))
    for_ st.sweep.trigger.es5 \b -> void $
      H.liftAff (attempt (toAffE (Rig.es5pulse { bit: b, ms: st.sweep.trigger.ms })))
    when (st.sweep.port /= "") $ for_ st.sweep.trigger.note \n -> liftEffect $
      Rig.sendNote { port: st.sweep.port, channel: st.sweep.trigger.channel, note: n
                   , velocity: st.sweep.trigger.velocity, ms: st.sweep.trigger.ms }
  StopAudio -> do
    liftEffect Audio.stop
    H.modify_ _ { playing = Nothing }
  Play i -> do
    st <- H.get
    for_ (Array.index st.regions i) \r -> do
      liftEffect (Audio.playRange ("/api/take-audio?take=" <> st.showing) r.start r.end)
      H.modify_ _ { playing = Just i }
  ToggleKeep i -> H.modify_ \s ->
    s { keep = if Set.member i s.keep then Set.delete i s.keep else Set.insert i s.keep }
  KeepAll on -> H.modify_ \s ->
    s { keep = if on then Set.fromFoldable (Array.range 0 (Array.length s.regions - 1)) else Set.empty }
  ArmOn src -> captureOn true src
  -- | **The sweep modal**, and asking the browser for MIDI when it opens.
  -- |
  -- | Asked once, on opening, rather than at Run: `requestMIDIAccess` prompts
  -- | the first time, and a permission dialog appearing in the middle of a run
  -- | would cost the take.
  GoTo pg -> H.modify_ _ { page = pg }
  PickSource v -> H.modify_ \s0 -> s0 { source = fromMaybe s0.source (Int.fromString v) }
  -- | **Choosing how to fill the take**, and asking the browser for MIDI the
  -- | first time a transect is chosen.
  -- |
  -- | Asked on choosing rather than at Run: `requestMIDIAccess` prompts the
  -- | first time, and a permission dialog appearing in the middle of a run
  -- | would cost the take.
  FillBy f -> do
    H.modify_ _ { fill = f }
    handleAction (OpenSweep (f == Swept))
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
        -- No head trim: see `captureOn`. The schedule declares when the
        -- first hit happens, so nothing needs to find it.
        captureOn false src
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
    for_ (cap st) \c ->
      if c.on
        then do
          send EndCapture
          H.modify_ (note "closed")
        else H.modify_ (note "nothing is recording")
    -- Nothing to put back. The looper needed `LevelArm false` and `Sounding
    -- true` here because arming had changed the rig's state and leaving it
    -- changed would have surprised the next thing to use it; a capture
    -- changes nothing outside itself, so there is nothing to undo.
    H.modify_ \s -> s { armed = false, waiting = true }

-- | **Start a capture on an input.**
-- |
-- | Where this was nine verbs it is now three, and the difference is the whole
-- | of plan item 6. It used to send `Source`, `Clear`, `Alternates false`,
-- | `Sounding false`, `OnGrid false`, `Bars`, `LevelArm` and `Record` — four
-- | of those existing only to turn the looper OFF, because the page was
-- | driving a looper to do something that is not looping.
-- |
-- | ## What `trimHead` is, and what it is not
-- |
-- | It is **not** a level arm, and that distinction cost a take to learn. The
-- | first swept run caught five hits of twelve: position 1 is the bottom of the
-- | range, the BIA at Morph 0 is barely audible, and the looper's level arm did
-- | not trip until the sweep had climbed loud enough — around position eight.
-- | The measurements said so plainly, peak 0.16 rising to 0.70.
-- |
-- | So the capture always begins the moment you ask. `trimHead` only decides
-- | **where the file starts**, resolved at write time over audio already in
-- | hand. A quiet first hit can no longer be missed, because nothing waits for
-- | it; at worst the trim lands in the wrong place and the audio it would have
-- | cut is still there.
-- |
-- | Playing by hand you want it — you cannot press Record and pick up a stick
-- | in the same instant. A run does not: the schedule declares its own start.
-- | **Declared beats inferred**, which is the rule this project keeps
-- | rediscovering.
captureOn :: forall o m. MonadAff m => Boolean -> Int -> H.HalogenM State Action () o m Unit
captureOn trimHead src = do
  st <- H.get
  -- **Choosing the input is the act of arming.** Kept apart, the page had its
  -- own idea of which input to use and asserted it at Arm — so a reload
  -- silently armed on the wrong one. Together, the thing you press says what
  -- it is going to listen to, and there is no second copy to disagree.
  --
  -- Nothing is cleared first, because starting a capture IS the clearing: a
  -- capture holds one take and there is nothing a second could be layered
  -- onto. That is also why there is no Discard button — starting the next one
  -- does early what this does anyway.
  send (CaptureArm trimHead)
  -- **A count, in frames**, because the page is the one holding the tempo and
  -- the daemon is the one holding the frame. Zero runs until stopped, which is
  -- every kind but `Bars`.
  send (CaptureStop (closeAfter st))
  send (Capture src)
  -- **A new take is scratch until it is kept.** Cleared here rather than in
  -- either caller, because this is the one place both ways of filling a take
  -- go through, and a stale "kept" badge on a fresh recording is exactly the
  -- confusion the flag exists to remove.
  H.modify_ _ { kept = false, confirmKeep = false }
  -- **A new capture invalidates the kept schedule.** Restored on reload, a
  -- schedule belonging to some earlier run would divide THIS take into bands
  -- that look deliberate and describe nothing. Cleared here and written again
  -- only when a sweep actually finishes.
  liftEffect (Sweep.saveRun { take: "", schedule: [] })
  -- A fresh name unless you gave it one that has not been used yet. This is
  -- the whole of the overwrite fix: the write is `cw <name>`, so a name that
  -- already belongs to a take on disk is a name that destroys it.
  when (wantsAName st) do
    n <- liftEffect (slugFor st.kind)
    H.modify_ _ { name = n, kit = if st.kitMine then st.kit else "" }
  -- Not swept until something sweeps it. Left set, the declared-against-found
  -- check would go on comparing every later take by hand against a position
  -- count that has nothing to do with it.
  --
  -- The schedule belongs to a run, and this take is not that run until one
  -- starts. Left standing, a take played by hand would be divided at the
  -- boundaries of the sweep before it — silently, and at plausible-looking
  -- times.
  H.modify_ (note (if trimHead then Kind.prompt st.kind else "recording — the run starts in a moment")
    <<< _ { armed = true, swept = false, schedule = [], overran = false })

-- | **How many frames a take of this kind runs for**, or zero for by hand.
-- |
-- | The bar comes from the daemon's own `barFrames`, which is Link's where
-- | there is a clock and the anchor loop's cycle where there is not — the
-- | field the daemon's own comment says the app should read. No clock and no
-- | anchor means no bar, and a count of nothing is by hand.
closeAfter :: State -> Int
closeAfter st = case Kind.closes st.kind of
  ByHand -> 0
  AtCount n -> n * maybe 0 _.barFrames st.looper

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
    unless (Array.null s.cv && Array.null s.esx) $ void $
      H.liftAff (attempt (toAffE (Rig.setCv { set: s.cv, esx: s.esx })))
    when (p.port /= "") $ liftEffect $ for_ s.cc \c ->
      Rig.sendCc { port: p.port, channel: c.channel, cc: c.cc, value: c.value }
    H.liftAff (delay (Milliseconds (Int.toNumber p.settleMs)))
    -- **Where in the take this hit is about to be**, asked immediately before
    -- the trigger rather than after it: everything between here and the pulse
    -- is a few microseconds of arithmetic, where everything after it is a
    -- round trip of unknown length. See `Quadrat.Schedule`.
    mk <- liftEffect Schedule.at
    for_ mk \t -> H.modify_ \s0 -> s0 { schedule = Array.snoc s0.schedule t }
    for_ p.trigger.gate \b -> void $
      H.liftAff (attempt (toAffE (Rig.pulse
        { bus: b, level: p.trigger.gateLevel, ms: p.trigger.ms })))
    for_ p.trigger.es5 \b -> void $
      H.liftAff (attempt (toAffE (Rig.es5pulse { bit: b, ms: p.trigger.ms })))
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
  -- **Keep the schedule, because it cannot be recomputed.** It is measured —
  -- the trigger times as they actually landed — and a reload that loses it
  -- downgrades a transect to a guess without saying so. See `Sweep.saveRun`.
  st1 <- H.get
  liftEffect (Sweep.saveRun { take: st1.name, schedule: st1.schedule })
  handleAction Close

-- | **Stand the instrument at one cell of the transect**, without striking it.
-- |
-- | Read from `Sweep.steps` rather than recomputed, so a preset previewed and
-- | the same preset recorded cannot disagree about what they mean.
setCellCv :: forall o m. MonadAff m => Int -> H.HalogenM State Action () o m Unit
setCellCv j = do
  st <- H.get
  for_ (Array.index (Sweep.steps st.sweep) j) \step -> do
    unless (Array.null step.cv && Array.null step.esx) $ void $
      H.liftAff (attempt (toAffE (Rig.setCv { set: step.cv, esx: step.esx })))
    when (st.sweep.port /= "") $ liftEffect $ for_ step.cc \c ->
      Rig.sendCc { port: st.sweep.port, channel: c.channel, cc: c.cc, value: c.value }

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
      -- The ESX holds its last value exactly as a bus does. A voltage left
      -- standing at the top of a sweep is a thing you hear in an unrelated
      -- patch an hour later and blame on something else.
      slots = Array.nub (Array.mapMaybe _.esx st.sweep.params)
  unless (Array.null buses && Array.null slots) $ void $ H.liftAff
    (attempt (toAffE (Rig.setCv
      { set: map (\b -> { bus: b, level: 0.0 }) buses
      , esx: map (\k -> { slot: k, level: 0.0 }) slots })))

-- | Write the take down and ask where the sounds are.
-- |
-- | Two hops, because they are two different kinds of knowledge: the daemon
-- | holds the audio and writes it, `msm` reads the file and says where things
-- | begin. Neither could do the other's half.
-- | `write` is false when the take is already on disk and only the dividing
-- | is being asked again — which is what moving the slider does, and it should
-- | not cost a re-export every time.
-- | The chosen source's level as 0…1, from the daemon's own dB. -60 is the
-- | floor: below it nothing is playing, and the sparkline says so by lying flat.
levelNow :: State -> Number
levelNow s =
  let
    ix = fromMaybe 1 (if s.source > 0 then Just s.source
                      else map (_ + 1) (s.looper >>= \t -> Array.findIndex _.available t.sources))
    db = maybe (-120.0) _.db (s.looper >>= \t -> Array.index t.sources (ix - 1))
  in
    max 0.0 (min 1.0 ((db + 60.0) / 60.0))

analyse :: forall o m. MonadAff m => Boolean -> H.HalogenM State Action () o m Unit
analyse write = do
  st <- H.get
  case cap st of
    Just c | c.holds -> divide st
    _ -> H.modify_ (note "nothing to divide — nothing has been captured")
  where
  divide st = do
      H.modify_ _ { busy = true, regions = [], keep = Set.empty }
      when write do
        send (CapturePeaks 900)
        send (WriteCapture st.name)
        -- The daemon writes on its own thread and the ack lands in a snapshot;
        -- the folder is there a moment later.
        H.liftAff (delay (Milliseconds 900.0))
      let takeName = if write then st.name else st.showing
          -- **The run divides its own take.** Empty for anything played by
          -- hand, which is when the detector is the only thing that could
          -- know. See `Quadrat.Schedule` for the lead, which is the one
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

-- | `clamp` for numbers, named apart because `clamp` is already in scope for
-- | the Int uses on this page.
clampN :: Number -> Number -> Number -> Number
clampN lo hi v = max lo (min hi v)

fmt :: Number -> String
fmt n = show (Int.round (n * 100.0) # \k -> Int.toNumber k / 100.0)

render :: forall m. State -> H.ComponentHTML Action () m
render st =
  HH.div [ HP.class_ (HH.ClassName "q") ]
    [ HH.header [ HP.class_ (HH.ClassName "q-head") ]
        [ HH.h1_ [ HH.text "Quadrat" ]
        , HH.nav [ HP.class_ (HH.ClassName "q-nav") ]
            [ pageTab Bench "Bench" "cut a transect and look at what came back"
            , pageTab Library "Library" "every set kept, re-runnable, and where it can go"
            ]
        , HH.span [ HP.class_ (HH.ClassName "q-sub") ] [ HH.text tagline ]
        , connection
        ]
    , if st.page == Bench then statement else HH.text ""
    , case st.page of
        -- | **A notebook spread: the method on the left, the results on the
        -- | right.**
        -- |
        -- | These were two views and you had to flip between them, which broke
        -- | the only loop that matters here — run, listen, bend, run again.
        -- | You cannot judge a curve against a sound you have to leave the
        -- | page to hear. Side by side they are one gesture.
        -- | **Left is the take; right is the instrument.**
        -- |
        -- | Andrew's arrangement, and the samples argued for it: a
        -- | one-dimensional transect is a vertical stack of traces, which
        -- | wants a narrow column and no more. So what the take IS and what
        -- | came OUT of it share the left page — they are the same subject —
        -- | and the right page is given to the thing that needs width and is
        -- | actually being worked: the trigger, and a curve per parameter.
        -- | **Left is what came back; right is what was asked for.**
        -- |
        -- | They were the other way round, with the specification split across
        -- | both pages: the name and the encoding on the left, the trigger and
        -- | the curves on the right, and a Transect/By-hand tab above the lot
        -- | asking a question the trigger asked again. So no page was about
        -- | one thing, and the take's own description had no single home.
        -- |
        -- | The specification needs width — an encoding row, a trigger row,
        -- | eight curves — and it is all one subject, so it takes the wide
        -- | page whole. What came back is a vertical stack of traces and a run
        -- | button, which wants a narrow column and no more.
        -- | **Specification, result, adjustments — in that order, down the
        -- | page.**
        -- |
        -- | Andrew's arrangement. The sentence says what this run is; the
        -- | take, full width, is what came of it; the columns beneath are the
        -- | things you adjust and then stop looking at. Side-by-side pages put
        -- | the specification and the result in competition for the same
        -- | glance, and neither won.
        Bench ->
          HH.div_
            [ HH.section [ HP.class_ (HH.ClassName "q-hero") ]
                [ HH.div [ HP.class_ (HH.ClassName "q-actbar") ] [ goRow, doors ]
                , if not (Array.null st.regions) || st.busy || hasTake
                    then caught
                    else case st.fill of
                      Swept -> expected
                      Played -> waiting
                ]
            -- | **Four doors, and the curves.**
            -- |
            -- | Everything between the take and the curves is consulted, set,
            -- | and then not read again: how the take was divided, what
            -- | strikes it, what pitches it plays, where it goes afterwards.
            -- | Each was a panel competing with the waveform for the page.
            -- | Behind a door they cost one line, and the line says what is
            -- | inside it rather than showing you.
            , sendRow
            , HH.section [ HP.class_ (HH.ClassName "q-curverow") ]
                [ SweepView.curves sweepHandlers
                , case st.pivot of
                    Just j | st.fill == Swept -> pivotPanel j
                    _ -> HH.text ""
                ]
            , case st.modal of
                Just DivisionModal -> modalBox "Division details" divisionPanel
                Just TriggerModal -> modalBox "Trigger"
                  (HH.div_
                    [ case st.fill of
                        Swept -> SweepView.settings sweepHandlers
                        Played -> handPanel
                    , SweepView.triggerView sweepHandlers
                    ])
                -- Opened with the pitch parameter ALREADY expanded: the reason
                -- to come in here is the values, and a door that opens onto a
                -- second door is a door too many.
                Just PitchModal -> modalBox "Pitch"
                  (SweepView.pitchView (sweepHandlers { open = pitchIx }))
                Just ExportModal -> modalBox "Export to card" placeBlock
                Nothing -> HH.text ""
            ]
        Library ->
          HH.div_ [ setsView, cardView ]
    , HH.section [ HP.class_ (HH.ClassName "q-log") ]
        (map (\l -> HH.div_ [ HH.text l ]) st.log)
    ]
  where
  cp = cap st
  -- What the daemon says the capture is doing, never a second copy of it.
  --
  -- Four booleans where the loop needed `layers`, `armed`, `isWriting` and
  -- `sized` read together, and where the combination — not any one of them —
  -- said which of six states a loop was in.
  srcName = maybe "?" _.name
    (st.looper >>= \top -> Array.index top.sources (srcNow - 1))
  hasTake = maybe false _.holds cp
  writing = maybe false _.on cp
  -- How long this take has been running, from the daemon's own frame count
  -- rather than from a clock here: a page that keeps its own time drifts from
  -- the recording it is describing.
  elapsed = maybe "0" (\c -> fmt c.secs) cp

  -- | **Peers of Record.** They are all things you do to this take, and a
  -- | separate row for four of them implied a separation that is not there.
  doors =
    HH.div [ HP.class_ (HH.ClassName "q-doors") ]
      [ door DivisionModal "Division"
          (if Array.null st.regions then "nothing divided yet"
           else show (Set.size st.keep) <> " of "
                  <> show (Array.length st.regions) <> " kept")
          (not (Array.null st.regions) || hasTake)
      , door TriggerModal "Trigger" triggerSays true
      , door PitchModal "Pitch" pitchSays2 true
      , door ExportModal "Export to card"
          (if placeable then "bank " <> st.bank <> " · voice " <> show st.voice
           else "SuperDirt — already a bank")
          (placeable && not (Set.isEmpty st.keep))
      ]

  -- | A door: what is behind it, and what it currently says. The summary is
  -- | the point — a button that only says "Trigger" makes you open it to
  -- | learn anything, which is the panel it replaced with extra steps.
  door m label says live =
    HH.button
      [ HP.class_ (HH.ClassName ("q-door" <> if live then "" else " is-moot"))
      , HE.onClick \_ -> OpenModal (Just m)
      ]
      [ HH.span [ HP.class_ (HH.ClassName "q-doorname") ] [ HH.text label ]
      , HH.span [ HP.class_ (HH.ClassName "q-doorsays") ] [ HH.text says ]
      ]

  triggerSays = case triggerBy of
    "hand" -> "you play it"
    "es5" -> "ES-5 gate " <> maybe "?" show st.sweep.trigger.es5
    "es9" -> "bus " <> maybe "?" show st.sweep.trigger.gate
               <> maybe "" (\b -> " · ES-9 jack " <> show (b - 7)) st.sweep.trigger.gate
    _ -> "note " <> maybe "?" show st.sweep.trigger.note
           <> (if st.sweep.port == "" then " · no MIDI port" else " · " <> st.sweep.port)

  pitchSays2 = case Array.findMap _.pitch st.sweep.params of
    Nothing -> "unpitched"
    Just ps -> ps.label <> " · " <> Pitch.noteName ps.noteLo
                 <> "–" <> Pitch.noteName ps.noteHi

  pitchIx = Array.findIndex (\q -> Maybe.isJust q.pitch) st.sweep.params

  sweepHandlers =
    { ports: st.midiPorts, open: st.sweepEdit, plan: st.sweep
    , msg: SweepMsg, openParam: OpenParam
    , tables: st.tables, tablesErr: st.tablesErr, pickPitch: PickPitch
    , rigFires: st.fill == Swept, addPitch: AddPitch
    }

  -- | **A modal, for the jobs that are consulted rarely and read never.**
  -- |
  -- | Dividing settings and card addresses are each a handful of controls that
  -- | matter intensely for about ten seconds and then never again. On the page
  -- | they competed for attention with the two things you look at constantly.
  modalBox title inner =
    HH.div
      [ HP.class_ (HH.ClassName "q-scrim")
      , HE.onClick \_ -> OpenModal Nothing
      ]
      [ HH.div
          [ HP.class_ (HH.ClassName "q-modal")
          -- Clicks inside must not reach the scrim, or every control in the
          -- modal would also close it.
          , HE.onClick \_ -> OpenModal (st.modal)
          ]
          [ HH.div [ HP.class_ (HH.ClassName "q-modalhead") ]
              [ HH.h2_ [ HH.text title ]
              , HH.button
                  [ HP.class_ (HH.ClassName "q-plain")
                  , HE.onClick \_ -> OpenModal Nothing ]
                  [ HH.text "done" ]
              ]
          , inner
          ]
      ]

  -- | **The whole specification, as one sentence.**
  -- |
  -- | Andrew, 2026-09-11: *"it's very easy to get used to a bad interface and
  -- | then just not see it for what it is"*. The salient choices were spread
  -- | over two pages, four panels and a masthead — every one of them legible,
  -- | none of them legible TOGETHER, so what the next Run would actually do
  -- | could only be assembled by reading the whole page.
  -- |
  -- | Written as a sentence, it also stops being possible to leave a slot
  -- | meaningless: "samples from hits" is not English, which is the same fault
  -- | the spec records about the daemon's source names, arriving here as a
  -- | thing you cannot help reading.
  -- |
  -- | Each slot is the ONLY control for what it says. Where a panel below used
  -- | to ask the same question it no longer does.
  statement =
    HH.section [ HP.class_ (HH.ClassName "q-say") ]
      [ HH.p [ HP.class_ (HH.ClassName "q-sayline") ]
          [ HH.text "Making ", slotExtent
          , HH.text " ", slotPitched
          , HH.text " samples from ", slotSource, listening
          , HH.text ", triggered by ", slotTrigger
          , HH.text ", kept as ", slotName
          , HH.text " for ", slotEncoding
          , HH.text "."
          ]
      , HH.p [ HP.class_ (HH.ClassName "q-sayfine") ]
          ( [ HH.text "Calibration scheme: ", slotCalib ]
              <> pitchSays
              <> sweptSays
              <> [ HH.text " · about ", HH.text runSecs, HH.text " to record" ] )
      ]

  -- | **The level, beside the input it measures.**
  -- |
  -- | It was a row of its own under the sentence, naming the source a second
  -- | time and drawing two rules across the page to hold three words. The
  -- | sentence already says WHICH input; what it could not say is whether
  -- | anything is arriving on it, and twenty polls of history says that in
  -- | the width of a word.
  -- |
  -- | The long warning goes with the row. It existed because the button named
  -- | the input and naming was not enough — but a sparkline lying flat in the
  -- | middle of the sentence is the same news, delivered where the decision is
  -- | made rather than as a paragraph beside the act.
  listening =
    HH.span
      [ HP.class_ (HH.ClassName ("q-spark" <> if quiet then " is-quiet" else ""))
      , HP.title (fmt srcDb <> " dB — the last two seconds of input level"
                    <> (if quiet then ". Nothing is playing into it." else ""))
      ]
      [ HH.text sparkline ]

  -- | One axis is a number you can say; two are a shape, and the shape belongs
  -- | with the axes that make it rather than in the middle of a sentence.
  slotExtent = case st.sweep.extent of
    [ n ] ->
      HH.input
        [ HP.class_ (HH.ClassName "q-slot is-num"), HP.type_ HP.InputNumber
        , HP.value (show n), HP.min 1.0, HP.max 512.0
        , HP.title "how many samples this run makes"
        , HE.onValueInput \v -> SweepMsg (Sweep.SetExtent 0 v)
        ]
    ns -> HH.span [ HP.class_ (HH.ClassName "q-slot is-fixed") ]
            [ HH.text (joinWith " × " (map show ns)) ]

  slotPitched =
    sel "q-slot" (if pitchOn then "pitched" else "unpitched") SetPitched
      [ { v: "unpitched", t: "unpitched" }, { v: "pitched", t: "pitched" } ]

  pitchOn = Array.any (\q -> Maybe.isJust q.pitch) st.sweep.params

  slotSource = case st.looper of
    Nothing -> HH.span [ HP.class_ (HH.ClassName "q-slot is-fixed") ] [ HH.text "…" ]
    Just top ->
      sel "q-slot" (show srcNow) PickSource
        (Array.mapWithIndex
          (\i src -> { v: show (i + 1)
                     , t: src.name <> (if src.available then "" else " — off") })
          top.sources)

  slotTrigger =
    sel "q-slot" triggerBy SetTriggerBy
      [ { v: "es9", t: "the ES-9" }, { v: "es5", t: "the ES-5" }
      , { v: "fh2", t: "the FH-2" }, { v: "midi", t: "MIDI" }
      , { v: "hand", t: "my own hands" } ]

  triggerBy
    | st.fill == Played = "hand"
    | Maybe.isJust st.sweep.trigger.note =
        if String.contains (String.Pattern "FH-2") st.sweep.port then "fh2" else "midi"
    | Maybe.isJust st.sweep.trigger.es5 = "es5"
    | otherwise = "es9"

  slotName =
    HH.input
      [ HP.class_ (HH.ClassName "q-slot is-name"), HP.type_ HP.InputText
      , HP.value st.name, HP.disabled (st.armed || writing)
      , HP.title "names the take, the set, and the kit it proposes"
      , HE.onValueInput SetName
      ]

  slotEncoding =
    sel "q-slot" (Encoding.name st.sweep.encoding) (SweepMsg <<< Sweep.PickEncoding)
      (map (\e -> { v: Encoding.name e, t: Encoding.label e }) Encoding.all)

  -- | The measured table, in small letters: which scheme turns notes into
  -- | volts. `— none —` is not a gap, it is the unpitched case said plainly.
  slotCalib =
    sel "q-slot is-fine" (fromMaybe "" pitchLabel) AddPitch
      ( Array.cons { v: "", t: "— none, unpitched —" }
          (map (\t -> { v: t.label, t: t.label <> " · " <> t.module }) st.tables) )

  pitchLabel = map _.label (Array.findMap _.pitch st.sweep.params)

  -- | **What the notes actually are**, said where the specification is read.
  -- |
  -- | A range and a cell count are two numbers, and the interval between the
  -- | notes is the thing you hear — but it is neither of them, it is their
  -- | quotient. `bia-bass` measures G1-D6, so twelve cells over its span step
  -- | five semitones and the run climbs in FOURTHS. That is a legitimate thing
  -- | to ask for (twelve samples across five octaves is how you fill a sparse
  -- | sampler) and a terrible thing to get by accident, which is exactly the
  -- | case for saying it rather than preventing it.
  -- |
  -- | Not auto-corrected for the same reason. A pitch axis picked fresh
  -- | defaults to one degree per cell; one restored from an older plan keeps
  -- | the range it was given, and gets told.
  pitchSays = case Array.findIndex (\q -> Maybe.isJust q.pitch) st.sweep.params of
    Nothing -> []
    Just i -> case Array.index st.sweep.params i >>= _.pitch of
      Nothing -> []
      Just ps ->
        let
          n = max 1 (Encoding.total st.sweep.extent)
          span = ps.noteHi - ps.noteLo
          step = Int.toNumber span / Int.toNumber (max 1 (n - 1))
        in
          [ HH.text (" · " <> Pitch.noteName ps.noteLo <> "–"
                       <> Pitch.noteName ps.noteHi <> " ") ]
            <> if span == n - 1
                 then [ HH.text "chromatic" ]
                 else
                   [ HH.span [ HP.class_ (HH.ClassName "q-sayodd") ]
                       [ HH.text ("in steps of " <> fmt step <> " semitones") ]
                   , HH.button
                       [ HP.class_ (HH.ClassName "q-sayfix")
                       , HP.title ("one semitone per sample — "
                             <> Pitch.noteName ps.noteLo <> "–"
                             <> Pitch.noteName (ps.noteLo + n - 1))
                       , HE.onClick \_ ->
                           SweepMsg (Sweep.SetPitchHi i (show (ps.noteLo + n - 1)))
                       ]
                       [ HH.text "make it chromatic" ]
                   ]

  -- | What is being moved, by name only. The shapes and the ranges are in the
  -- | panel; this says how many knobs are in play, which is the part you want
  -- | at a glance and the part a list of curves does not tell you.
  sweptSays =
    let ns = map _.name (Array.filter (\q -> Maybe.isNothing q.pitch) st.sweep.params)
    in if Array.null ns then []
       else [ HH.text (" · sweeping " <> joinWith ", " ns) ]

  runSecs =
    let n = Encoding.total st.sweep.extent
    in fmt (Int.toNumber (n * st.sweep.spacingMs) / 1000.0) <> " s"

  -- | **A dropdown that is exactly as wide as the word it is showing.**
  -- |
  -- | A native `select` sizes itself to its WIDEST option, so "Rample ·
  -- | layers" was underlined to the width of "SuperDirt · a grid, flattened"
  -- | and the sentence trailed blank underscores after half its slots. The
  -- | visible word is therefore a span, which sizes to its content, with the
  -- | select laid transparently over it — native menu, native keyboard
  -- | handling, correct width.
  sel k cur act opts =
    HH.span [ HP.class_ (HH.ClassName ("q-slotwrap " <> k)) ]
      [ HH.span [ HP.class_ (HH.ClassName "q-slottext") ]
          [ HH.text (fromMaybe cur (map _.t (Array.find (\o -> o.v == cur) opts))) ]
      , HH.select
          [ HP.class_ (HH.ClassName "q-slotsel"), HE.onValueChange act ]
          (map (\o -> HH.option [ HP.value o.v, HP.selected (o.v == cur) ] [ HH.text o.t ]) opts)
      ]

  -- | **The page's own line**, which changes with the page because the two
  -- | halves are not doing the same thing and should not claim to be.
  tagline = case st.page of
    Bench -> "sample an instrument at chosen points"
    Library -> "what has been kept, and where it can go"

  pageTab pg label why =
    HH.button
      [ HP.class_ (HH.ClassName ("q-tab" <> if st.page == pg then " on" else ""))
      , HP.title why
      , HE.onClick \_ -> GoTo pg
      ]
      [ HH.text label ]

  -- | **The one button, whatever is about to fill the take.**
  -- |
  -- | Arming and going are one gesture for the same reason choosing the input
  -- | is: the sound has to land inside a take, and a Go button that assumed
  -- | something was already recording would fail silently by producing sound
  -- | nobody caught.
  goRow =
    HH.div [ HP.class_ (HH.ClassName "q-go") ]
      ( if st.armed || writing
          then
            [ HH.button
                [ HP.class_ (HH.ClassName "q-big is-stop"), HE.onClick \_ -> Close ]
                [ HH.text (if writing then "Stop" else "Cancel") ]
            , HH.span [ HP.class_ (HH.ClassName "q-state") ]
                [ HH.text ("recording — " <> elapsed <> " s") ]
            ]
          else if running
            then
              [ HH.span [ HP.class_ (HH.ClassName "q-state") ]
                  [ HH.text ("position " <> show (maybe 0 (_ + 1) st.sweepAt)
                      <> " of " <> show (Encoding.total st.sweep.extent)) ]
              , HH.button
                  [ HP.class_ (HH.ClassName "q-big is-stop"), HE.onClick \_ -> StopSweep ]
                  [ HH.text "Stop" ]
              ]
            else
              -- **One button, and it names what it will listen to.** Five
              -- chips became a chooser in the masthead; the gesture stayed
              -- one press, and the press still says out loud which input it
              -- is about to arm — which is the property the chips were there
              -- for and the only one worth keeping.
              -- **A verb, not a sentence.** The button used to name the input
              -- — "Run on board" — because nothing else did. The statement
              -- names it now, and better, so what is left here is the act.
              [ HH.button
                  [ HP.class_ (HH.ClassName "q-big is-rec")
                  , HP.disabled (st.looper == Nothing)
                  , HP.title (if st.fill == Swept
                                then "play the schedule and record the lot as one take"
                                else "arm, and record what you play")
                  , HE.onClick \_ ->
                      if st.fill == Swept then RunSweep srcNow else ArmOn srcNow
                  ]
                  [ HH.span [ HP.class_ (HH.ClassName "q-recdot") ] []
                  , HH.text "Record"
                  ]
              -- | **The silent-input warning moved into the sentence.**
              -- |
              -- | A whole transect once went to an input with nothing patched
              -- | to it — twenty-six seconds of correct schedule, correct
              -- | division, correct measurement, all of digital silence — so
              -- | the fact has to be carried somewhere. It is carried by the
              -- | sparkline lying flat next to the input's own name, which is
              -- | where the decision is made. Repeating it here as a sentence
              -- | of red capitals said the same thing twice and was the
              -- | loudest thing on the page for a condition that is normal
              -- | between takes.
              , HH.span [ HP.class_ (HH.ClassName "q-state") ]
                  [ HH.text (case st.fill of
                      Swept -> "the take closes itself when the last position has sounded"
                      Played -> maybe "" (\c -> if c.holds
                                                  then "captured " <> fmt c.secs <> " s"
                                                  else Kind.prompt st.kind) cp) ]
              ]
      )

  -- | **One position, every parameter — the table read down instead of across.**
  -- |
  -- | Andrew, 2026-09-10: *"if you identify one hit in the transect that isn't
  -- | what you want, you're going to want to tweak perhaps many parameters of
  -- | that one hit"*. The curve drawer answers "what does morph do across the
  -- | run"; this answers "what is happening at hit 7", which is the question
  -- | you have when hit 7 is wrong.
  -- |
  -- | **It needed no new model.** A curve becomes `Drawn` the moment any one
  -- | of its points is edited, so a transect authored entirely as presets is
  -- | one where every curve is drawn — which `Sweep` has always supported and
  -- | `set.json` has always stored. Which makes this a second way to author
  -- | the same object rather than a feature beside it: **define twelve
  -- | presets and record them**, with the curves as a way to seed them.
  -- |
  -- | Every move is heard on release, not on drag: a preview per pixel would
  -- | be a machine gun, and `input` fires per pixel where `change` fires when
  -- | you let go.
  pivotPanel j =
    HH.div [ HP.class_ (HH.ClassName "q-pivot") ]
      [ HH.div [ HP.class_ (HH.ClassName "q-deskhead") ]
          [ HH.text ("position " <> show (j + 1) <> " of "
              <> show (Encoding.total st.sweep.extent)
              <> " — every parameter at this one hit") ]
      , HH.div [ HP.class_ (HH.ClassName "q-pivotrows") ]
          (Array.mapWithIndex (pivotRow j) st.sweep.params)
      , HH.div [ HP.class_ (HH.ClassName "q-pivotfoot") ]
          [ HH.button
              [ HP.class_ (HH.ClassName "q-plain is-go")
              , HP.title "set every bus for this cell and strike it"
              , HE.onClick \_ -> Hear j
              ]
              [ HH.text "\x25b6 hear it" ]
          , HH.button
              [ HP.class_ (HH.ClassName "q-plain")
              , HP.disabled (j <= 0)
              , HE.onClick \_ -> OpenPivot (Just (j - 1)) ]
              [ HH.text "\x2190 previous" ]
          , HH.button
              [ HP.class_ (HH.ClassName "q-plain")
              , HP.disabled (j + 1 >= Encoding.total st.sweep.extent)
              , HE.onClick \_ -> OpenPivot (Just (j + 1)) ]
              [ HH.text "next \x2192" ]
          , HH.button
              [ HP.class_ (HH.ClassName "q-plain"), HE.onClick \_ -> OpenPivot Nothing ]
              [ HH.text "close" ]
          ]
      ]

  -- | One parameter at this position: what it is called, where it is going in
  -- | the instrument's own terms, and a slider that says so as you move it.
  pivotRow j i q =
    let vs = Sweep.valuesFor st.sweep q
        onAxis = fromMaybe 0 (Array.index (cellAt j) q.axis)
        v = fromMaybe 0.0 (Array.index vs onAxis)
        pc = Int.round (v * 100.0)
        level = q.cvLo + (q.cvHi - q.cvLo) * v
    in
      HH.div [ HP.class_ (HH.ClassName "q-pivotrow") ]
        [ HH.span [ HP.class_ (HH.ClassName "q-pivotname") ] [ HH.text q.name ]
        , HH.input
            [ HP.class_ (HH.ClassName "q-pivotslider")
            , HP.type_ HP.InputRange
            , HP.min 0.0, HP.max 100.0, HP.step (HP.Step 1.0)
            , HP.value (show pc)
            , HE.onValueInput (SweepMsg <<< Sweep.SetValue i onAxis)
            -- On release, not on drag. See `pivotPanel`.
            , HE.onValueChange \_ -> Hear j
            ]
        , HH.span [ HP.class_ (HH.ClassName "q-pivotsays") ]
            -- Volts, because that is the instrument's own term and the one
            -- you can look up in a manual a year later. Measured: level 1.0
            -- is 10 V on the ES-9, and the ESX is the same scale.
            [ HH.text (case q.cv, q.esx, q.cc of
                Just b, _, _ -> "cv " <> show b <> "  " <> fmt (level * 10.0) <> " V"
                _, Just k, _ -> "esx " <> show k <> "  " <> fmt (level * 10.0) <> " V"
                _, _, Just c -> "cc " <> show c <> "  "
                  <> show (Int.round (Int.toNumber q.ccLo
                       + (Int.toNumber q.ccHi - Int.toNumber q.ccLo) * v))
                _, _, _ -> "not routed") ]
        ]

  -- The cell at this index, in the encoding's own recording order.
  cellAt j = fromMaybe []
    (Array.index (Encoding.cells st.sweep.encoding st.sweep.extent) j)

  -- | Eight block characters, oldest to newest. Empty history draws the
  -- | floor rather than nothing, so the line does not appear and disappear.
  sparkline =
    let
      blocks = [ "\x2581", "\x2582", "\x2583", "\x2584", "\x2585", "\x2586", "\x2587", "\x2588" ]
      pick v = fromMaybe "\x2581"
        (Array.index blocks (clamp 0 7 (Int.round (clampN 0.0 1.0 v * 7.0))))
      recent = Array.takeEnd 20 st.levels
      padded = Array.replicate (20 - Array.length recent) 0.0 <> recent
    in
      Array.fold (map pick padded)

  -- Whichever is chosen, or the first the daemon says is available.
  srcNow =
    if st.source > 0 then st.source
    else 1 + fromMaybe 0
      (st.looper >>= \top -> Array.findIndex _.available top.sources)
  srcDb = maybe (-120.0) _.db
    (st.looper >>= \top -> Array.index top.sources (srcNow - 1))
  -- | **Below this nobody is playing into it.**
  -- |
  -- | Was -90, which -80 does not trip — and -80 is silence. A whole transect
  -- | went to an input with nothing patched to it while the page showed the
  -- | reading and called it fine. -60 is quiet in any room; anything a module
  -- | is actually driving sits far above it.
  quiet = srcDb < -60.0

  connection = case st.looper of
    Nothing -> HH.span [ HP.class_ (HH.ClassName "q-warn") ] [ HH.text "no daemon" ]
    Just _ -> HH.span [ HP.class_ (HH.ClassName "q-ok") ] [ HH.text "daemon" ]

  -- | **The inputs, and pressing one is what arms.**
  -- |
  -- | Andrew's simplification, and it removes a whole class of mistake: the
  -- | page cannot arm on an input you did not just choose, because choosing is
  -- | the gesture. Each chip shows what the daemon says that input is doing
  -- | right now, so a dead one is visible before you play into it.


  running = Maybe.isJust st.sweepFork


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
  -- | **The sets on disk**, which are the artefact this page exists to make.
  -- |
  -- | A card is a projection: it says which set is on which voice, in the
  -- | terms one module can address. The set is the thing itself — the samples
  -- | at capture fidelity, the measurements, what each one meant, and the spec
  -- | that produced them. Sets outlive cards, and a set can go somewhere the
  -- | Rample cannot reach.
  -- |
  -- | So the useful verb here is not "load" but **"run it again"**: the spec
  -- | comes back as a plan, and the resolution is chosen now rather than then.
  -- | Twelve positions today, sixteen tomorrow, from the same description.
  setsView
    | Array.null st.sets =
        HH.section [ HP.class_ (HH.ClassName "q-sets") ]
          [ HH.h2_ [ HH.text "Sets" ]
          , HH.p [ HP.class_ (HH.ClassName "q-muted") ]
              [ HH.text "Nothing kept yet. Cut a take into a set on the bench \
                        \and it will be here — with the spec that made it, so \
                        \it can be run again at a resolution you have not \
                        \chosen yet." ]
          ]
    | otherwise =
        HH.section [ HP.class_ (HH.ClassName "q-sets") ]
          ( [ HH.div [ HP.class_ (HH.ClassName "q-sechead") ]
                [ HH.h2_ [ HH.text "Sets" ]
                , HH.span [ HP.class_ (HH.ClassName "q-muted") ]
                    [ HH.text (show (Array.length st.sets) <> " kept, "
                        <> show (Array.length (Array.filter _.runnable st.sets))
                        <> " re-runnable") ]
                ]
            ]
              <> map setRow st.sets
          )

  -- | How to play it in a pattern. `n` counts from zero and the files from
  -- | one, which is worth saying once here rather than being discovered.
  dirt r
    | r.count == 0 = ""
    | otherwise = case r.extent of
        [ outer, inner ] ->
          "s \"" <> r.name <> "\" # n (o*" <> show inner <> "+i)"
            <> "   -- o 0.." <> show (outer - 1) <> ", i 0.." <> show (inner - 1)
        _ -> "s \"" <> r.name <> "\" # n \"0.." <> show (r.count - 1) <> "\""

  -- | **One set as a record entry**, not a table row.
  -- |
  -- | The fields are heterogeneous — a name and a date, a count, a sentence
  -- | about what moved, a line of code to type — and forcing them into
  -- | columns of one width made every one of them cramped. A ruled entry with
  -- | its own internal alignment reads the way a notebook page does.
  setRow r =
    HH.article [ HP.class_ (HH.ClassName "q-set") ]
      [ HH.div [ HP.class_ (HH.ClassName "q-set-id") ]
          [ HH.div [ HP.class_ (HH.ClassName "q-set-name") ] [ HH.text r.name ]
          , HH.div [ HP.class_ (HH.ClassName "q-set-when") ]
              [ HH.text (String.take 10 r.made
                  <> (if r.take == "" then "" else " · from " <> r.take)) ]
          ]
      , HH.div [ HP.class_ (HH.ClassName "q-set-n") ]
          [ HH.text (show r.count), HH.span_ [ HH.text " samples" ] ]
      , HH.div [ HP.class_ (HH.ClassName "q-set-what") ]
          -- Said in three words, not thirteen. A library of legacy sets
          -- repeated the whole sentence down the page and it became the
          -- loudest thing on it; the reason belongs in a tooltip.
          [ HH.div
              [ HP.title (if not r.described
                            then "cut before sets were stored, so nothing here \
                                 \knows how it was made"
                            else "") ]
              [ HH.text
                  (if not r.described then "no description"
                   else if Array.null r.moved then "played by hand"
                   else joinWith ", " r.moved
                        <> " over " <> joinWith " × " (map show r.extent)
                        <> (if r.encoding == "" then "" else " · " <> r.encoding)) ]
          -- Every set is already a SuperDirt bank, whatever it was recorded
          -- for — a folder of numbered files is a named, indexed set at both
          -- ends. Said for all of them, because that is the finding.
          , HH.code [ HP.class_ (HH.ClassName "q-set-dirt") ] [ HH.text (dirt r) ]
          ]
      , HH.div [ HP.class_ (HH.ClassName "q-set-do") ]
          [ if r.runnable
              then HH.button
                     [ HP.class_ (HH.ClassName "q-plain")
                     , HP.title "load the spec that made this set, so it can be \
                                \recorded again at a different resolution"
                     , HE.onClick \_ -> RunAgain r.name
                     ]
                     [ HH.text "Sweep again\x2026" ]
              else HH.text ""
          , if r.described && r.count > 0
              then HH.button
                     [ HP.class_ (HH.ClassName "q-plain")
                     , HP.disabled st.cardBusy
                     , HP.title "put this set on the card, at the bank and voice \
                                \chosen on the card below — nothing is cut or \
                                \measured again"
                     , HE.onClick \_ -> PlaceSet r.name
                     ]
                     [ HH.text "Onto the card" ]
              else HH.text ""
          ]
      ]

  -- | **Playing it yourself is the other way to fill a take.**
  -- |
  -- | The kinds are about *material*, not about loops: what the take holds
  -- | decides how it closes, whether it is divided, and what the divisions
  -- | mean. Nothing here says Record — the column's Go row does that, the
  -- | same one a transect uses.
  handPanel =
    HH.div [ HP.class_ (HH.ClassName "q-hand") ]
      [ HH.div [ HP.class_ (HH.ClassName "q-kinds") ]
          (map kindBtn Kind.all)
      , case st.kind of
          Kind.Bars _ ->
            HH.label [ HP.class_ (HH.ClassName "q-field is-tight") ]
              [ HH.span_ [ HH.text "bars" ]
              , HH.input
                  [ HP.type_ HP.InputText, HP.value (show st.bars)
                  , HP.disabled (st.armed || writing)
                  , HE.onValueInput SetBars ]
              ]
          _ -> HH.text ""
      , HH.p [ HP.class_ (HH.ClassName "q-blurb") ]
          [ HH.text (Kind.blurb st.kind)
          , HH.text (" Captured from " <> srcName <> " as it comes"
              <> ", and folded to "
              <> (if Kind.foldsTo st.kind == ToMono then "mono" else "stereo")
              <> " on the way to a card"
              <> (if Kind.voicesOn st.kind == 2
                    then " — where it takes two of the four voices."
                    else "."))
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
  -- | **The set you are about to record, before there is one.**
  -- |
  -- | An empty page here is a wasted half of the spread, and the extent is
  -- | the one setting whose consequence is hard to picture from a number:
  -- | `12` and `4 × 8` and `12 × 16` are a column, a block and a wall. Drawn
  -- | rather than counted, and it fills in as the run passes each cell — so a
  -- | run in progress is visible from the same place its result will be.
  expected =
    let cells = Encoding.cells st.sweep.encoding st.sweep.extent
        at = fromMaybe (-1) st.sweepAt
    in
      HH.div_
        [ HH.div [ HP.class_ (HH.ClassName "q-gridhead") ]
            [ HH.span_
                [ HH.text (show (Array.length cells) <> " samples"
                    -- The extent only says something a count does not when
                    -- there is more than one axis: "12 samples, 12" is noise,
                    -- "32 samples, 4 × 8" is the shape.
                    <> (if Array.length st.sweep.extent > 1
                          then ", " <> joinWith " × " (map show st.sweep.extent)
                          else "")
                    <> (if running then " — position " <> show (at + 1)
                        else " to record")) ]
            ]
        -- **The same strip, before there is a take to draw it on.**
        --
        -- These were numbered boxes in a grid — the only thing on the page
        -- standing for a sample that does not exist yet, which made them the
        -- only way to author a transect preset by preset. That job is real and
        -- survives; the boxes do not. An empty strip divided into the bands
        -- the plan implies is the same picture the recording will fill in, so
        -- a position is reached the same way before and after, and the page
        -- stops having two grammars for one thing.
        , HH.div_ (map plannedRow (rowsOf (Array.length cells)))
        , HH.p [ HP.class_ (HH.ClassName "q-blurb") ]
            [ HH.text (if running
                then "Each one fills as it sounds. The take closes itself when \
                     \the last position has."
                else "Every one of these becomes a sample. When the run has \
                     \finished they are replaced by what it caught — the \
                     \waveform of each, and what it measured — so a set that \
                     \came out flat is visible here rather than on the module.") ]
        ]

  -- | The right page before a take played by hand. Says what will appear and
  -- | where, rather than leaving half the spread blank.
  waiting =
    HH.p [ HP.class_ (HH.ClassName "q-blurb") ]
      [ HH.text ("Nothing recorded yet. Arm on an input: "
          <> Kind.prompt st.kind
          <> ". What you caught appears here as one waveform and a tile per \
             \sound, with what each measured, so you can keep the ones you \
             \meant before any of it reaches a card.") ]

  caught
    | otherwise =
        HH.div_
          [ case st.peaks of
              Just pk | Array.length pk.hi > 0 ->
                HH.div [ HP.class_ (HH.ClassName "q-whole") ]
                  [ HH.div_ (map (strip pk) (rowsOf (Array.length st.regions)))
                  , HH.div [ HP.class_ (HH.ClassName "q-wholebar") ]
                      [ HH.button
                          [ HP.class_ (HH.ClassName "q-plain")
                          , HP.title "the whole take, end to end, gaps and all"
                          , HE.onClick \_ -> PlayWhole
                          ]
                          [ HH.text "\x25b6 all of it" ]
                      , HH.button
                          [ HP.class_ (HH.ClassName "q-plain")
                          , HE.onClick \_ -> StopAudio ]
                          [ HH.text "stop" ]
                      , HH.span [ HP.class_ (HH.ClassName "q-muted") ]
                          [ HH.text (maybe "" (\c -> fmt c.secs <> " s") (cap st)) ]
                      ]
                  ]
              _ -> HH.text ""
          , if Array.null st.regions && hasTake && not st.busy
              then
                HH.div [ HP.class_ (HH.ClassName "q-wholebar") ]
                  [ HH.button
                      [ HP.class_ (HH.ClassName "q-plain is-go"), HE.onClick \_ -> Analyse ]
                      [ HH.text "Divide it" ]
                  , HH.span [ HP.class_ (HH.ClassName "q-muted") ]
                      [ HH.text (if st.busy then "dividing…" else "") ]
                  ]
              else HH.text ""
          ]

  -- | **How the take was divided** — the controls, where the numbers are not.
  -- |
  -- | A divider, a gap and a lead are decided once and then read never; the
  -- | counts they produce are read constantly. The column carries the counts
  -- | and this carries the controls, which is the split the page was missing.
  divisionPanel =
    HH.div [ HP.class_ (HH.ClassName "q-divpanel") ]
      [ HH.div [ HP.class_ (HH.ClassName "q-gridhead") ]
              [ HH.span_
                  [ HH.text (if st.busy then "dividing…"
                             else if Array.null st.regions
                               then maybe "" (\c -> fmt c.secs <> " s recorded, not divided yet") (cap st)
                               else show (Set.size st.keep) <> " of "
                                    <> show (Array.length st.regions) <> " kept") ]
              , spread
              , declaredVsFound
              , if Array.null st.regions && hasTake && not st.busy
                  then HH.button [ HP.class_ (HH.ClassName "q-plain"), HE.onClick \_ -> Analyse ]
                         [ HH.text "Divide it" ]
                  else HH.text ""
              , HH.button [ HP.class_ (HH.ClassName "q-plain"), HE.onClick \_ -> KeepAll true ]
                  [ HH.text "Keep all" ]
              , HH.button [ HP.class_ (HH.ClassName "q-plain"), HE.onClick \_ -> KeepAll false ]
                  [ HH.text "Keep none" ]
              , if not (Array.null st.schedule) then HH.text "" else
                HH.label [ HP.class_ (HH.ClassName "q-quiet") ]
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
                  , HH.span [ HP.class_ (HH.ClassName "q-gapval") ]
                      [ HH.text (show (Int.round st.minGap) <> " ms") ]
                  ]
              , HH.label [ HP.class_ (HH.ClassName "q-hover") ]
                  [ HH.input
                      [ HP.type_ HP.InputCheckbox, HP.checked st.hoverPlays
                      , HE.onChecked SetHoverPlays ]
                  , HH.span_ [ HH.text "hover plays" ]
                  ]
              ]
      , dividerRow
      ]

  -- | **The rows of a transect.**
  -- |
  -- | A one-dimensional run is one row. A two-dimensional one is `outer` rows
  -- | of `inner` pieces, because the inner axis varies fastest — see
  -- | `Encoding.cells` — so consecutive pieces of the take are consecutive
  -- | cells of one row. Drawing them as rows is therefore not an arrangement
  -- | imposed on the recording; it is the recording's own shape.
  innerN = case st.sweep.extent of
    [ _, i ] | i > 1 -> i
    _ -> 0

  rowsOf n
    | n <= 0 = []
    | innerN <= 0 = [ { lo: 0, hi: n } ]
    | otherwise =
        map (\k -> { lo: k * innerN, hi: min n ((k + 1) * innerN) })
            (Array.range 0 ((n - 1) / innerN))

  -- | **One row, drawn on the stretch of the take that made it.**
  -- |
  -- | Its picture is a SLICE of the whole take's envelope — `Wave.bucketsFor`
  -- | again, which is what made a grid of tiles cost one snapshot rather than
  -- | forty requests, and now makes a stack of rows cost the same.
  -- |
  -- | Two overlays rather than nested controls: the bands open a position, the
  -- | bar along the bottom keeps or drops it. Nested, the keep button would
  -- | bubble its click into the band behind it and every drop would also open
  -- | a panel.
  strip pk rng =
    let
      total = max 0.001 (maybe 1.0 _.secs (cap st))
      rs = Array.slice rng.lo rng.hi st.regions
      t0 = maybe 0.0 _.start (Array.head rs)
      t1 = maybe total _.end (Array.last rs)
      span = max 0.001 (t1 - t0)
      b = Wave.bucketsFor (Array.length pk.hi) total t0 t1
      cut xs = Array.slice b.from b.to xs
      left v = show (100.0 * (v - t0) / span) <> "%"
      wide v = show (100.0 * v / span) <> "%"
    in
      HH.div [ HP.class_ (HH.ClassName "q-strip") ]
        [ Wave.svg (cut pk.lo) (cut pk.hi) [ Wave.klass "q-whole-svg" ]
        , HH.div [ HP.class_ (HH.ClassName "q-segs") ]
            (Array.mapWithIndex (\j r -> segment (rng.lo + j) r left wide) rs)
        , HH.div [ HP.class_ (HH.ClassName "q-keeps") ]
            (Array.mapWithIndex (\j r -> keepBar (rng.lo + j) r left wide) rs)
        ]

  -- | **One piece of the take, drawn where it happened.**
  -- |
  -- | A band over the envelope rather than a tile beside it. The division and
  -- | the sound that produced it are then the same picture, so a piece that
  -- | starts late, runs into its neighbour, or caught nothing at all is
  -- | visible without auditioning anything — and the gaps between bands are
  -- | the silence, drawn by not being covered.
  -- |
  -- | Hover plays it; clicking opens every parameter at that position. The
  -- | numbered boxes that used to be the only way to reach a position are
  -- | gone: the piece itself is a better handle on the sample than a box
  -- | standing in for it.
  segment i r left wide =
    let
      kept = Set.member i st.keep
      w = witness i
    in
      HH.div
        [ HP.class_ (HH.ClassName ("q-seg"
            <> (if kept then "" else " is-dropped")
            <> (if st.playing == Just i then " is-playing" else "")
            <> (if st.pivot == Just i then " is-open" else "")))
        , style ("left:" <> left r.start
                   <> ";width:" <> wide (max 0.0 (r.end - r.start))
                   <> (if kept then ";background:" <> w.tint else ""))
        , HP.title (w.label <> " — " <> fmt (r.end - r.start) <> " s at "
                      <> fmt r.start <> " s. Click for every parameter here.")
        , HE.onMouseEnter \_ -> HoverPlay i
        , HE.onClick \_ -> OpenPivot (if st.pivot == Just i then Nothing else Just i)
        ]
        [ HH.span [ HP.class_ (HH.ClassName "q-seg-n") ] [ HH.text w.label ] ]

  -- | A row of the transect as PLANNED: equal bands, because a schedule is
  -- | regular by construction until the rig plays it and reports otherwise.
  plannedRow rng =
    let
      k = rng.hi - rng.lo
      w = 100.0 / Int.toNumber (max 1 k)
      at = fromMaybe (-1) st.sweepAt
      steps = Sweep.steps st.sweep
    in
      HH.div [ HP.class_ (HH.ClassName "q-strip is-planned") ]
        [ HH.div [ HP.class_ (HH.ClassName "q-segs") ]
            (map
              (\j ->
                let i = rng.lo + j in
                HH.div
                  [ HP.class_ (HH.ClassName ("q-seg is-plan"
                      <> (if running && i <= at then " is-done" else "")
                      <> (if running && i == at then " is-now" else "")
                      <> (if st.pivot == Just i then " is-open" else "")))
                  , style ("left:" <> show (Int.toNumber j * w) <> "%;width:" <> show w <> "%")
                  , HP.title ("position " <> show (i + 1)
                        <> " — every parameter at this hit")
                  , HE.onClick \_ ->
                      if running then OpenPivot st.pivot
                      else OpenPivot (if st.pivot == Just i then Nothing else Just i)
                  ]
                  [ HH.span [ HP.class_ (HH.ClassName "q-seg-n") ]
                      [ HH.text (labelOf steps i) ] ])
              (if k <= 0 then [] else Array.range 0 (k - 1)))
        ]

  -- | **A position, named by what it asks for.** The note when the run has a
  -- | pitch axis, the position number otherwise — `Meaning.note` is -1 on a
  -- | parameter that is not a pitch, so finding one IS the test.
  labelOf steps i =
    case Array.index steps i >>= \sp -> Array.find (\x -> x.note >= 0) sp.means of
      Just m -> Pitch.noteName m.note
      Nothing -> show (i + 1)

  keepBar i r left wide =
    let kept = Set.member i st.keep
    in
      HH.button
        [ HP.class_ (HH.ClassName ("q-keepbar" <> if kept then " on" else ""))
        , style ("left:" <> left r.start <> ";width:" <> wide (max 0.0 (r.end - r.start)))
        , HP.title (if kept then "kept — click to drop it"
                    else "dropped — click to keep it")
        , HE.onClick \_ -> ToggleKeep i
        ]
        []

  -- | Steps of the run that made this take, for the meanings. Empty for a take
  -- | played by hand, which has no schedule and so no declared meaning.
  runStepsC = if Array.null st.schedule then [] else Sweep.steps st.sweep

  -- | **A parameter declares its witness** (spec §5): a pitch axis is
  -- | witnessed by the note it asked for, anything else by how much came out.
  -- | `note` is -1 on a parameter that is not a pitch, so finding one is the
  -- | whole test — no second flag to fall out of step.
  witness i =
    let
      noted = do
        step <- Array.index runStepsC i
        m <- Array.find (\x -> x.note >= 0) step.means
        pure m.note
      lo = 36
      hi = 96
    in
      case noted of
        Just n ->
          { label: Pitch.noteName n
          , tint: hsl (Int.toNumber (n - lo) / Int.toNumber (hi - lo))
          }
        Nothing ->
          { label: show (i + 1)
          , tint: hsl (maybe 0.0 (\r -> min 1.0 r.rms * 2.0) (Array.index st.regions i))
          }

  -- | A cool-to-warm band, at an alpha low enough that the waveform still
  -- | reads through it. The colour is the measurement, not decoration.
  hsl t =
    "hsla(" <> show (Int.round (210.0 - 190.0 * clamp01 t)) <> ",70%,48%,0.22)"

  clamp01 v = max 0.0 (min 1.0 v)

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
        HH.div [ HP.class_ (HH.ClassName "q-dividers") ]
          [ HH.span [ HP.class_ (HH.ClassName "q-arm-label") ] [ HH.text "Divide" ]
          , HH.div [ HP.class_ (HH.ClassName "q-chips") ]
              (map dividerBtn (Divider.all st.equalN))
          , if Divider.needsCount st.divider
              then HH.label [ HP.class_ (HH.ClassName "q-field is-tight") ]
                     [ HH.span_ [ HH.text "pieces" ]
                     , HH.input
                         [ HP.type_ HP.InputNumber, HP.value (show st.equalN)
                         , HP.min 2.0, HP.max 128.0
                         , HE.onValueInput SetEqualN ]
                     ]
              else HH.text ""
          , HH.span [ HP.class_ (HH.ClassName "q-muted") ]
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
    HH.div [ HP.class_ (HH.ClassName "q-dividers") ]
      [ HH.span [ HP.class_ (HH.ClassName "q-arm-label") ] [ HH.text "Divide" ]
      , HH.span [ HP.class_ (HH.ClassName "q-chips") ]
          [ HH.span [ HP.class_ (HH.ClassName "q-chip on") ] [ HH.text "by the schedule" ] ]
      , HH.label [ HP.class_ (HH.ClassName "q-field is-tight") ]
          [ HH.span_ [ HH.text "lead ms" ]
          , HH.input
              [ HP.type_ HP.InputNumber, HP.value (show st.sweep.leadMs)
              , HP.min (-500.0), HP.max 2000.0
              , HP.title "how long after a trigger its sound is in the take"
              , HE.onValueChange SetLead ]
          ]
      , HH.span [ HP.class_ (HH.ClassName "q-muted") ]
          [ HH.text (show (Array.length st.schedule)
              <> " triggers, timed by the daemon as it recorded them — nothing was \
                 \detected, so nothing could be missed") ]
      ]

  dividerBtn dv =
    HH.button
      [ HP.class_ (HH.ClassName ("q-chip" <> if dv == st.divider then " on" else ""))
      , HP.disabled st.busy
      , HP.title (Divider.blurb dv)
      , HE.onClick \_ -> PickDivider dv
      ]
      [ HH.text (Divider.label dv) ]

  -- | Where the kept tiles go. Beside them, because it acts on them.
  -- | **Three verbs, three moments.**
  -- |
  -- | Run leaves a scratch take; KEEP writes the set; PLACE gives that set an
  -- | address on a card. These were one row holding two name fields and two
  -- | write buttons, and on 2026-09-11 a name typed into KIT never reached the
  -- | set — which was written under the take's name, silently. The fault was
  -- | not the labels: it was that three different objects with three different
  -- | lifetimes were sharing one row of controls.
  -- |
  -- | So: one name (the take's, set before the run), one button that writes,
  -- | and the card kept separate — because a set exists whether or not it has
  -- | an address, and `server.mjs` has said so all along.
  sendRow
    | Array.null st.regions = HH.text ""
    | otherwise =
        HH.div [ HP.class_ (HH.ClassName "q-acts") ]
          [ keepBlock
          -- **Exporting is a later, separate act.** A set exists whether or
          -- not it has an address; putting one on a card is a thing you do
          -- afterwards, often to a set made an hour ago. So it is a door
          -- rather than a panel — and for SuperDirt there is no door, because
          -- the set as stored is already the bank.
          , if not placeable then noPlace
            else
              HH.button
                [ HP.class_ (HH.ClassName "q-plain")
                , HP.disabled (Set.isEmpty st.keep)
                , HE.onClick \_ -> OpenModal (Just ExportModal)
                ]
                [ HH.text "Export to card…" ]
          ]

  setName = if st.name == "" then "set" else st.name

  -- | **Placing is a Rample idea.** A SuperDirt set IS the bank as stored, so
  -- | bank/kit/voice beside one is a question with no answer. Only asked of a
  -- | swept take: a take played by hand has no encoding to consult.
  placeable = st.fill /= Swept || Encoding.onCard st.sweep.encoding

  keepBlock =
    HH.div [ HP.class_ (HH.ClassName "q-keep") ]
      [ HH.span [ HP.class_ (HH.ClassName "q-acthead") ] [ HH.text "Keep" ]
      , if st.confirmKeep
          then
            HH.span [ HP.class_ (HH.ClassName "q-twoverbs") ]
              [ HH.button
                  [ HP.class_ (HH.ClassName "q-plain is-replacing")
                  , HP.disabled st.cardBusy
                  , HP.title "the whole directory is deleted first — this is a \
                             \replacement, not a merge"
                  , HE.onClick \_ -> SendToCard { place: false, append: false }
                  ]
                  [ HH.text ("overwrite " <> setName) ]
              , HH.button
                  [ HP.class_ (HH.ClassName "q-plain")
                  , HE.onClick \_ -> CancelKeep
                  ]
                  [ HH.text "cancel" ]
              ]
          else
            HH.button
              [ HP.class_ (HH.ClassName "q-plain is-go")
              , HP.disabled (st.cardBusy || Set.isEmpty st.keep)
              , HP.title "cut the kept pieces into a set, measure them, and write \
                         \the description beside them"
              , HE.onClick \_ -> AskKeep
              ]
              [ HH.text ("Keep " <> show (Set.size st.keep) <> " samples") ]
      -- **Say where it lands, at the moment of landing it.** The one place a
      -- path is shown, so the name in `The take` and the directory on disk
      -- cannot drift apart again.
      , HH.span [ HP.class_ (HH.ClassName "q-dest") ]
          [ HH.text ("→ samples/" <> setName) ]
      , if st.kept
          then
            HH.span [ HP.class_ (HH.ClassName "q-scratch is-kept") ]
              [ HH.text "kept" ]
          else
            HH.span [ HP.class_ (HH.ClassName "q-scratch") ]
              [ HH.text "scratch — the next run replaces this take" ]
      ]

  noPlace =
    HH.div [ HP.class_ (HH.ClassName "q-place is-moot") ]
      [ HH.span [ HP.class_ (HH.ClassName "q-acthead") ] [ HH.text "Export" ]
      , HH.span [ HP.class_ (HH.ClassName "q-scratch") ]
          [ HH.text "SuperDirt — the set as stored is already the bank, so there \
                    \is nothing to place it in" ]
      ]

  placeBlock =
    HH.div [ HP.class_ (HH.ClassName "q-place") ]
      [ HH.span [ HP.class_ (HH.ClassName "q-acthead") ] [ HH.text "Export to card" ]
      , small "bank" st.bank SetBank
      , small "kit" (if st.kit == "" then setName else st.kit) SetKit
      , HH.label [ HP.class_ (HH.ClassName "q-field is-tight") ]
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
              [ HP.class_ (HH.ClassName "q-plain is-go")
              , HP.disabled (st.cardBusy || Set.isEmpty st.keep)
              , HE.onClick \_ -> SendToCard { place: true, append: false }
              ]
              [ HH.text (show (Set.size st.keep) <> " to the kit") ]
          Just r ->
            HH.div [ HP.class_ (HH.ClassName "q-twoverbs") ]
              [ HH.button
                  [ HP.class_ (HH.ClassName "q-plain is-go")
                  , HP.disabled (st.cardBusy || Set.isEmpty st.keep)
                  , HP.title "the layer selector picks between them"
                  , HE.onClick \_ -> SendToCard { place: true, append: true }
                  ]
                  [ HH.text ("add as layer " <> show (Array.length r.sets + 1)) ]
              , HH.button
                  [ HP.class_ (HH.ClassName "q-plain is-replacing")
                  , HP.disabled (st.cardBusy || Set.isEmpty st.keep)
                  , HE.onClick \_ -> SendToCard { place: true, append: false }
                  ]
                  [ HH.text "replace" ]
              ]
      -- **How the layer selector moves.** Only worth asking once a voice holds
      -- more than one thing to choose between — and it is the whole reason to
      -- use layers rather than slices, since these are the modes where the
      -- module decides for itself.
      , case occupant of
          Just r | Array.length r.sets >= 1 ->
            HH.label [ HP.class_ (HH.ClassName "q-field is-tight") ]
              [ HH.span_ [ HH.text "picked by" ]
              , HH.select [ HE.onValueChange SetLayerMode ]
                  (map (\m -> HH.option
                          [ HP.value m
                          , HP.selected (m == (if st.layerMode == "" then r.mode else st.layerMode)) ]
                          [ HH.text m ])
                      [ "manual", "velocity", "random", "cyclic" ])
              ]
          _ -> HH.text ""
      -- **Say what is about to be destroyed, before it is.**
      --
      -- A kit's voice is an address, and sending to one that is taken replaces
      -- what is there. That happened four times in a row without a word being
      -- said, because the kit name stuck to the first take and every later send
      -- addressed the same slot. Naming the occupant costs one line and makes
      -- the whole class of mistake visible while it can still be avoided.
      , case occupant of
          Nothing -> HH.text ""
          Just r ->
            HH.span [ HP.class_ (HH.ClassName "q-warn") ]
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
    in do
      v <- st.cardView
      Array.find
        (\r -> r.bank == st.bank && r.kit == kitName
                 && r.voice == st.voice && r.set /= setName)
        v.rows

  style :: forall r. String -> HH.IProp r Action
  style = HP.attr (HH.AttrName "style")

  small lbl v act =
    HH.label [ HP.class_ (HH.ClassName "q-field is-tight") ]
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
    -- | **Silent is not flat, and the flat test cannot catch it.**
    -- |
    -- | Twelve regions of noise floor have ratios like level x1.35 — the
    -- | noise is not constant, so they are not "within a few percent" and the
    -- | flat warning stays quiet while the whole take is nothing. Measured on
    -- | the take that proved it: peak 0.00014 throughout, which is -77 dBFS.
    -- | A separate question, asked separately.
    | loudest < 0.003 =
        HH.span [ HP.class_ (HH.ClassName "q-warn") ]
          [ HH.text ("nothing was recorded — the loudest sample in this take \
                     \peaks at " <> show (Int.round (loudest * 100000.0))
              <> "/100000, which is silence. The input had nothing playing \
                 \into it.") ]
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
          HH.span [ HP.class_ (HH.ClassName (if flat then "q-warn" else "q-muted")) ]
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
        else HH.span [ HP.class_ (HH.ClassName "q-warn") ]
               [ HH.text ("the run made " <> show (Array.length st.schedule)
                   <> " hits and the take holds " <> show (Array.length st.regions)
                   <> " — the recording ended before the run did") ]
    | Array.length st.regions == Encoding.total st.sweep.extent = HH.text ""
    | otherwise =
        HH.span [ HP.class_ (HH.ClassName "q-warn") ]
          [ HH.text ("you swept " <> show (Encoding.total st.sweep.extent)
              <> " samples and this divided into " <> show (Array.length st.regions)
              <> " — try another divider, or a wider gap, before sending it") ]

  -- The loudest in this take, which is what decides whether anything was
  -- recorded at all.
  loudest = fromMaybe 0.0 (Array.last (Array.sort (map _.peak st.regions)))

  -- | **The card, which is a description until you ask for it.**
  -- |
  -- | Writing it is a compile and never an edit in place, so what lands on the
  -- | SD card is always something you could have read first — and the three
  -- | rules the module fails silently on stay enforced in one place, by the
  -- | compiler, whose objections are shown here rather than restated.
  cardView =
    HH.section [ HP.class_ (HH.ClassName "q-card") ]
      [ HH.h2_ [ HH.text "The card, so far" ]
      , case st.cardView of
          Nothing -> HH.p [ HP.class_ (HH.ClassName "q-muted") ] [ HH.text "…" ]
          Just v
            | Array.null v.rows ->
                HH.p [ HP.class_ (HH.ClassName "q-muted") ]
                  [ HH.text "Nothing on it yet. Record something, keep the ones you \
                            \meant, and send them to a voice. It is kept on disk as \
                            \you build it; no card need be mounted until you write." ]
            | otherwise ->
                HH.div_
                  [ HH.table [ HP.class_ (HH.ClassName "q-table") ]
                      [ HH.thead_ [ HH.tr_ (map (\h -> HH.th_ [ HH.text h ])
                          [ "bank", "kit", "voice", "holds" ]) ]
                      , HH.tbody_ (map row v.rows)
                      ]
                  , writeRow v
                  , if v.plan == "" then HH.text ""
                    else HH.pre [ HP.class_ (HH.ClassName ("q-plan" <> if v.ok then "" else " is-bad")) ]
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
    HH.div [ HP.class_ (HH.ClassName "q-send") ]
      [ HH.span [ HP.class_ (HH.ClassName "q-arm-label") ] [ HH.text "Write to" ]
      , if Array.null v.cards
          then HH.span [ HP.class_ (HH.ClassName "q-muted") ]
                 [ HH.text "no Rample card is mounted — everything above is safe on \
                           \disk; mount one when you want it written" ]
          else HH.div [ HP.class_ (HH.ClassName "q-chips") ]
                 (map (\c -> HH.button
                         [ HP.class_ (HH.ClassName "q-chip is-arm")
                         , HP.disabled (st.cardBusy || not v.ok)
                         , HP.title ("compile the manifest onto " <> c)
                         , HE.onClick \_ -> WriteCard c
                         ]
                         [ HH.text c ]) v.cards)
      ]

  kindBtn k =
    HH.button
      [ HP.class_ (HH.ClassName ("q-kind" <> if Kind.name k == Kind.name st.kind then " on" else ""))
      , HP.disabled (st.armed || writing)
      , HE.onClick \_ -> PickKind (case k of
                                     Kind.Bars _ -> Kind.Bars st.bars
                                     other -> other)
      ]
      [ HH.text (Kind.label k) ]
