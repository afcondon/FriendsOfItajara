-- | **The Calibration page: measure how an oscillator really tracks, and keep
-- | the table that corrects it.**
-- |
-- | An analogue oscillator does not follow a volt per octave, and no two
-- | outputs of the interface are quite alike. So a signal path (this output,
-- | this oscillator, this front-panel setting) is swept: a staircase of
-- | voltages on one ES-9 jack, recorded as one take, each step's pitch read
-- | from the recording. The result is a calibration table, kept in Amphora
-- | where every realiser on the rig looks for it.
-- |
-- | ## A calibration sweep is a transect
-- |
-- | Set a voltage, keep what comes back, measure it, repeat: that is what
-- | Quadrat does. So this page records through the daemon's capture as every
-- | Quadrat run does, marks each step on the capture's own clock
-- | (`Quadrat.Schedule.at`), and has `msm` read each region. Nothing else
-- | opens the audio input, and the recording is kept, so a point that looks
-- | wrong can be listened to.
-- |
-- | ## Before and after
-- |
-- | The sweep is the **before**: ideal voltages, and how far each step's
-- | pitch fell from a perfect volt per octave. The **after** plays the same
-- | pitches through the new table and measures again: what is left is the
-- | error the table did not remove. Both takes can be played, the after with
-- | a reference tone at each step's target, to check by ear.
-- |
-- | ## Where the rules come from
-- |
-- | The judgement (a median of several readings, a point believed when its
-- | median is corroborated, a table refused when it is flat or folded, the
-- | ear deciding the octave) is `Reef.Calibration.Judge`. Every rule was
-- | DeepStar's, learned on this rig; the reasons are in
-- | docs/kb/reference/vco-calibration-lessons.md.
-- |
-- | Polymorphic in the state, as `Quadrat.Loop` is: `Fields` is a row that
-- | Quadrat's `State` carries, and nothing here can reach the rest of it.
module Quadrat.Calibrate
  ( Fields
  , Own
  , Msg(..)
  , Run
  , Step
  , initial
  , handle
  , view
  ) where

import Prelude

import Amphora.Api as Api
import Control.Promise (Promise, toAffE)
import Data.Array as Array
import Data.Either (Either(..), either)
import Data.Foldable (for_, maximum)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.Nullable (Nullable, toMaybe)
import Data.Number as Number
import Data.Number.Format (fixed, toStringWith)
import Data.String as String
import Data.Traversable (traverse)
import Data.Looper.Verb (Verb)
import Data.Looper.Verb as Verb
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..), attempt, delay)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Foreign.LooperSocket (LooperState)
import Foreign.LooperSocket as Socket
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Widgets.Statement as Statement
import Hawser.Fetch (Trouble, troubleText)
import Hawser.Fetch as Fetch
import Hawser.Http (Endpoint)
import Quadrat.Inventory as Inventory
import Quadrat.Layout as PageLayout
import Quadrat.Rig as Rig
import Quadrat.Schedule as Schedule
import Quadrat.Slug as Slug
import Quadrat.Tables as Tables
import Reef.Calibration (covers, realise)
import Reef.Calibration.Judge as Judge
import Reef.Calibration.Notes as Notes
import Reef.Calibration.Stored (Stored, StoredPoint)
import Reef.Calibration.Stored as Stored

-- | One sweep as it was recorded and read: the take, and for each step the
-- | voltage sent, the pitch aimed at (the pitch a perfect volt per octave
-- | would give from the first step), when the step began on the take's own
-- | clock, and what was read.
type Run =
  { take :: String
  , steps :: Array Step
  }

type Step =
  { volts :: Number
  , target :: Number
  , at :: Number
  , hz :: Number
  , reads :: Int
  , spreadCents :: Number
  , agreementCents :: Number
  -- | Whether the table could reach the target at all. A pitch above or
  -- | below everything the sweep measured cannot be aimed at: the realiser
  -- | holds the last voltage it measured and does not guess beyond it. Such a
  -- | step is not an error of the table, and is not counted as one.
  , reached :: Boolean
  }

-- | Everything the Calibration page reads and writes, as a row Quadrat's
-- | `State` carries. `looper` is the daemon's snapshot, shared with the Loop
-- | page: the capture's state and the sources that can be recorded.
type Fields r = (looper :: Maybe LooperState | Own r)

-- | The page's own fields: `Fields` less the snapshot, which Quadrat's
-- | `State` already carries for the Loop page.
type Own r =
  -- | The signal path. Realisers look the table up by this, and a table is
  -- | true only for its path.
  ( calLabel :: String
  -- | Which physical module, where it is, and anything worth saying.
  , calModule :: String
  , calWhere :: String
  , calComment :: String
  -- | The module's front-panel state. A table without it cannot be
  -- | reproduced.
  , calCoarse :: String
  -- | The ES-9 panel jack, 1 to 8, and the input the oscillator is heard on.
  , calJack :: Int
  , calSource :: String
  , calFrom :: Number
  , calTo :: Number
  , calStep :: Number
  -- | What is happening, said as a sentence; empty when nothing is.
  , calBusy :: String
  -- | The last thing worth telling: a verdict, a refusal, a result.
  , calSaid :: String
  , calBefore :: Maybe Run
  , calAfter :: Maybe Run
  -- | Octaves the before's pitches are moved by, to agree with the ear.
  , calHeard :: Int
  -- | The hash the table was last kept under, once it has been.
  , calKept :: String
  -- | Tuning: the starting voltage is held and the pitch read over and
  -- | over, so the oscillator can be put where the sweep should begin.
  , calTuning :: Boolean
  -- | The note the starting voltage was declared to be ("this is C2:
  -- | calibrate from here"), as a MIDI number. The targets are then anchored
  -- | on that note exactly, so that the few cents nobody can dial out of a
  -- | panel knob are part of what the table corrects.
  , calBase :: Maybe Int
  -- | Whether tuning switched the daemon's monitoring on, and so should
  -- | switch it off again.
  , calMonitored :: Boolean
  -- | The signal paths already calibrated, the latest table of each: what
  -- | the path menu offers, so that calibrating one again is one choice.
  , calPaths :: Array Stored
  -- | The modules Marquetry says exist, for choosing which one this is.
  , calModules :: Array Inventory.Module
  | r
  )

initial
  :: { calLabel :: String, calModule :: String, calWhere :: String, calComment :: String, calCoarse :: String
     , calJack :: Int, calSource :: String, calFrom :: Number, calTo :: Number, calStep :: Number
     , calBusy :: String, calSaid :: String, calBefore :: Maybe Run, calAfter :: Maybe Run, calHeard :: Int, calKept :: String
     , calTuning :: Boolean, calBase :: Maybe Int, calMonitored :: Boolean
     , calPaths :: Array Stored, calModules :: Array Inventory.Module
     }
initial =
  { calLabel: "", calModule: "", calWhere: "", calComment: "", calCoarse: ""
  , calJack: 1, calSource: "", calFrom: 0.0, calTo: 5.0, calStep: 0.25
  , calBusy: "", calSaid: "", calBefore: Nothing, calAfter: Nothing, calHeard: 0, calKept: ""
  , calTuning: false, calBase: Nothing, calMonitored: false
  , calPaths: [], calModules: []
  }

data Msg
  -- | The page was opened: read which paths have been calibrated.
  = Load
  -- | A path chosen from the menu, by its label; empty for a new one.
  | Pick String
  | SetLabel String
  | SetModule String
  | SetWhere String
  | SetComment String
  | SetCoarse String
  | SetJack Int
  | SetSource String
  | SetFrom String
  | SetTo String
  | SetStep String
  -- | Hold the starting voltage and keep reading the pitch, until stopped.
  | Tune
  | StopTuning
  -- | "This is the note here: calibrate from here."
  | Declare
  -- | Work out which input the jack's oscillator is heard on.
  | FindInput
  -- | Two voltages, two pitches: is the patch what it should be?
  | CheckPatch
  -- | The sweep: the before.
  | Sweep
  -- | The same pitches through the new table: the after.
  | Verify
  -- | "I hear it an octave up" (1) or down (-1).
  | Heard Int
  | Keep
  -- | Play a take: the before (false) or the after (true), the second flag
  -- | adding the reference tone.
  | Play Boolean Boolean
  | StopPlaying

type Handler r act slots o m = H.HalogenM (Record (Fields r)) act slots o m Unit

-- ── Timing ─────────────────────────────────────────────────────────────────

-- | How long each voltage is held, and how much of that is read. The first
-- | 300 ms are left for the output and the oscillator to settle; the last
-- | 100 ms are left alone in case the next step arrives early. What is
-- | between is read three times, half a second each, which is longer than
-- | the detector's window.
holdMs :: Number
holdMs = 1900.0

settleSecs :: Number
settleSecs = 0.3

guardSecs :: Number
guardSecs = 0.1

readings :: Int
readings = 3

-- | A reading rests on at least this many periods of its pitch, or it is
-- | not used. At 7 Hz in a 0.3 s window the detector once read a steady
-- | 35 Hz, three steps running.
fewestPeriods :: Number
fewestPeriods = 8.0

-- ── The outside ────────────────────────────────────────────────────────────

type Measured =
  { ok :: Boolean
  , output :: String
  , regions :: Array { rms :: Number, pitch :: Array (Nullable { hz :: Number, clarity :: Number, periods :: Number }) }
  }

foreign import measureImpl :: String -> Array { start :: Number, end :: Number } -> Int -> Effect (Promise Measured)
foreign import nowIso :: Effect String
foreign import playImpl :: String -> Array { at :: Number, hz :: Number, secs :: Number } -> Boolean -> Effect Unit
foreign import stopImpl :: Effect Unit

verb :: Verb -> Effect Boolean
verb sent = Socket.send (Verb.render sent <> "@0")

ask :: forall request reply. Endpoint request reply -> request -> Aff (Either Trouble reply)
ask described request = do
  base <- liftEffect Tables.storeBase
  Fetch.ask { base, timeoutMs: 10000 } described request

-- ── Playing a staircase into a take and reading it ─────────────────────────

-- | How a sweep's steps are held and read.
sweeping :: { holdMs :: Number, readings :: Int, rest :: Boolean }
sweeping = { holdMs, readings, rest: true }

-- | The ES-9 bus behind a panel jack: jack 1 is bus 8.
busOf :: Int -> Int
busOf jack = jack + 7

-- | Play these voltages on the jack into one take, and read each step's
-- | pitch. The `Left` is why not, as a sentence. The jack is put back to 0 V
-- | however it ends.
record
  :: forall r act slots o m
   . MonadAff m
  => String
  -> { holdMs :: Number, readings :: Int, rest :: Boolean }
  -> Array { volts :: Number, target :: Number, reached :: Boolean }
  -> H.HalogenM (Record (Fields r)) act slots o m (Either String Run)
record prefix how wanted = do
  st <- H.get
  let
    source = st.looper >>= \rig ->
      map (_ + 1) (Array.findIndex (\candidate -> candidate.name == st.calSource && candidate.available) rig.sources)
    bus = busOf st.calJack
    set volts = void $ liftAff $ attempt $ toAffE $ Rig.setCv { set: [ { bus, level: volts / 10.0 } ], esx: [] }
  case source of
    Nothing -> pure (Left "choose the input the oscillator is heard on; the one chosen is not there")
    Just index -> do
      take <- liftEffect (Slug.stamped prefix)
      -- A capture with a start and an end and nothing else: no bars, no
      -- quantising, no trimming of its head. The steps say where they are.
      liftEffect $ for_ [ Verb.CaptureArm false, Verb.CaptureQuantise 0, Verb.CaptureBars 0, Verb.CaptureStop 0, Verb.Capture index ] verb
      opened <- liftAff (awaitCapture 50)
      if not opened then pure (Left "the daemon did not start recording; is Itajara running?")
      else do
        marks <- wanted # traverse \step -> do
          set step.volts
          mark <- liftEffect Schedule.at
          H.modify_ _ { calBusy = "holding " <> show2 step.volts <> " V" }
          liftAff (delay (Milliseconds how.holdMs))
          pure (map (\at -> { volts: step.volts, target: step.target, reached: step.reached, at }) mark)
        when how.rest (set 0.0)
        _ <- liftEffect (verb Verb.EndCapture)
        liftAff (delay (Milliseconds 200.0))
        _ <- liftEffect (verb (Verb.WriteCapture take))
        liftAff (delay (Milliseconds 1000.0))
        case Array.catMaybes marks of
          marked | Array.length marked /= Array.length wanted -> pure (Left "the recording stopped before the sweep ended")
          marked -> do
            H.modify_ _ { calBusy = "reading the pitches" }
            let regions = marked <#> \step -> { start: step.at + settleSecs, end: step.at + how.holdMs / 1000.0 - guardSecs }
            measured <- liftAff (toAffE (measureImpl take regions how.readings))
            pure
              if not measured.ok then Left ("the take could not be read: " <> measured.output)
              else if Array.length measured.regions /= Array.length marked then Left "the take is shorter than the sweep: some steps fell outside it"
              else Right
                { take
                , steps: Array.zipWith
                    ( \step region ->
                        let
                          usable = Array.mapMaybe (\reading -> toMaybe reading >>= \found -> if found.periods >= fewestPeriods then Just found.hz else Nothing) region.pitch
                          combined = Judge.combine usable
                        in
                          { volts: step.volts, target: step.target, at: step.at, hz: combined.median, reads: Array.length usable
                          , spreadCents: combined.spreadCents, agreementCents: combined.agreementCents, reached: step.reached
                          }
                    )
                    marked
                    measured.regions
                }
  where
  awaitCapture :: Int -> Aff Boolean
  awaitCapture tries = do
    snapshot <- liftEffect Socket.latest
    if maybe false _.capture.on snapshot then pure true
    else if tries <= 0 then pure false
    else delay (Milliseconds 100.0) *> awaitCapture (tries - 1)

-- | The voltages of the grid, each to a microvolt so that drift does not
-- | show as 4.999999.
grid :: Number -> Number -> Number -> Array Number
grid from to step =
  if step <= 0.0 || to <= from then []
  else Array.range 0 (Int.floor ((to - from) / step + 1.0e-9)) <#> \index ->
    Number.round ((from + Int.toNumber index * step) * 1.0e6) / 1.0e6

-- ── From a run to a table ──────────────────────────────────────────────────

-- | Where the targets are anchored: a voltage and the pitch that voltage
-- | is meant to be. The declared note at the starting voltage, if one was
-- | declared; otherwise whatever the oscillator gave at the first voltage
-- | asked for.
anchorOf :: forall r. Record (Fields r) -> Run -> { volts :: Number, hz :: Number }
anchorOf st run = case st.calBase of
  Just midi -> { volts: st.calFrom, hz: 440.0 * Number.pow 2.0 ((Int.toNumber midi - 69.0) / 12.0) }
  Nothing -> case Array.find (\step -> step.volts >= st.calFrom - 1.0e-6) run.steps of
    Just first -> { volts: first.volts, hz: first.hz }
    Nothing -> { volts: st.calFrom, hz: 0.0 }

-- | How far a step's pitch fell from its target, in cents.
errorOf :: Step -> Number
errorOf step = if step.hz > 0.0 && step.target > 0.0 then Judge.centsBetween step.target step.hz else 0.0

-- | A run with each step's target filled in: the pitch a perfect volt per
-- | octave would give at that voltage, from the anchor.
aimed :: { volts :: Number, hz :: Number } -> Run -> Run
aimed anchor run = run { steps = run.steps <#> \step -> step { target = if anchor.hz > 0.0 then anchor.hz * Number.pow 2.0 (step.volts - anchor.volts) else 0.0 } }

-- | The steps that were asked for, without the one added at each end.
asked :: forall r. Record (Fields r) -> Run -> Run
asked st run = run { steps = Array.filter (\step -> step.volts >= st.calFrom - 1.0e-6 && step.volts <= st.calTo + 1.0e-6) run.steps }

points :: Run -> Array StoredPoint
points run = run.steps <#> \step ->
  { volts: step.volts
  , hz: Number.round (step.hz * 1000.0) / 1000.0
  , errCents: Just (Number.round (errorOf step * 100.0) / 100.0)
  , reads: Just step.reads
  , spreadCents: Just (Number.round (step.spreadCents * 100.0) / 100.0)
  , agreementCents: Just (Number.round (step.agreementCents * 100.0) / 100.0)
  , octaveShift: Nothing
  }

-- | The table a run makes, as it will be kept, moved by the octaves the ear
-- | asked for.
tableOf :: forall r. Record (Fields r) -> String -> Run -> Stored
tableOf st measuredAt run =
  let
    base = Judge.baseOf (fromMaybe 0.0 (Array.head run.steps <#> _.hz))
    measured =
      { label: st.calLabel
      , points: points run
      , bus: busOf st.calJack
      , channel: 0
      , coarseSetting: if st.calCoarse == "" then Nothing else Just st.calCoarse
      , baseNote: Just base.note
      , baseOffsetCents: Just (Number.round (base.offsetCents * 100.0) / 100.0)
      , refHzAt0V: Array.head run.steps <#> \first -> Number.round (first.hz * 100.0) / 100.0
      , module: if st.calModule == "" then Nothing else Just st.calModule
      , device: Just st.calSource
      , measuredAt: Just measuredAt
      , probe: Just "quadrat (msm, mcleod)"
      , stimulus: Just "es9-cv"
      , fullScaleVolts: Nothing
      , resolutionCents: Nothing
      , role: Nothing
      , sampleRate: Nothing
      , usableHz: Nothing
      , octaveSnapCents: Nothing
      , octaveOffset: Nothing
      , derivedFrom: Nothing
      }
  in
    if st.calHeard == 0 then measured
    else case measured.refHzAt0V of
      Just refHz -> case Judge.heard measured (refHz * Number.pow 2.0 (Int.toNumber st.calHeard)) of
        Judge.Moved _ moved -> moved
        _ -> measured
      Nothing -> measured

-- ── The handlers ───────────────────────────────────────────────────────────

handle :: forall r act slots o m. MonadAff m => Msg -> Handler r act slots o m
handle = case _ of
  Load -> do
    owned <- liftAff Inventory.modules
    H.modify_ _ { calModules = owned }
    found <- liftAff Tables.curated
    for_ found \tables ->
      -- The latest table of each label, in the order of the labels.
      H.modify_ _
        { calPaths = Array.sortWith _.label $ Array.nubByEq (\one other -> one.label == other.label)
            (Array.reverse (Array.sortWith (_.measuredAt >>> fromMaybe "") tables))
        }

  -- Everything the last calibration of this path recorded, put back on the
  -- page: the jack, the module, the setting, and the notes kept beside it.
  -- The coarse setting is what it WAS; it is yours to confirm before a sweep.
  Pick label -> do
    st <- H.get
    case Array.find (\kept -> kept.label == label) st.calPaths of
      Nothing -> H.modify_ _ { calLabel = "", calModule = "", calWhere = "", calComment = "", calCoarse = "", calBase = Nothing, calBefore = Nothing, calAfter = Nothing, calSaid = "" }
      Just kept -> do
        H.modify_ _
          { calLabel = kept.label
          , calJack = if kept.bus >= 8 && kept.bus <= 15 then kept.bus - 7 else st.calJack
          , calModule = fromMaybe "" kept.module
          , calCoarse = fromMaybe "" kept.coarseSetting
          , calWhere = "", calComment = "", calBase = Nothing, calBefore = Nothing, calAfter = Nothing
          , calSaid = "last calibrated " <> String.take 10 (fromMaybe "at a time nobody recorded" kept.measuredAt)
              <> (if fromMaybe "" kept.stimulus == "es9-cv" then "" else ", through " <> fromMaybe "another kind of output" kept.stimulus <> ", which this page does not sweep yet")
              <> ". Check the coarse setting still describes the module before sweeping."
          }
        noted <- liftAff (ask Api.getRef { name: Notes.refFor label })
        for_ noted \{ hash } -> do
          content <- liftAff (ask Api.getContent { hash })
          for_ (either (const Nothing) (Notes.read <<< _.payload) content) \written ->
            H.modify_ _
              { calWhere = written.place, calComment = written.comment
              , calModule = if written.module == "" then fromMaybe "" kept.module else written.module
              }

  SetLabel written -> H.modify_ _ { calLabel = written }
  SetModule written -> H.modify_ _ { calModule = written }
  SetWhere written -> H.modify_ _ { calWhere = written }
  SetComment written -> H.modify_ _ { calComment = written }
  SetCoarse written -> H.modify_ _ { calCoarse = written }
  SetJack jack -> H.modify_ _ { calJack = clamp 1 8 jack }
  SetSource name -> H.modify_ _ { calSource = name }
  SetFrom written -> for_ (Number.fromString written) \volts -> H.modify_ _ { calFrom = clamp (-10.0) 10.0 volts }
  SetTo written -> for_ (Number.fromString written) \volts -> H.modify_ _ { calTo = clamp (-10.0) 10.0 volts }
  SetStep written -> for_ (Number.fromString written) \volts -> H.modify_ _ { calStep = clamp 0.05 2.0 volts }

  -- The daemon listens to the chosen input and every snapshot carries its
  -- pitch (`Tune`, itajara's `crate::pitch`), so the reading on the page
  -- follows a hand on the oscillator's knob. The starting voltage is held
  -- meanwhile: this is the pitch the sweep will begin from.
  Tune -> idle do
    st <- H.get
    case st.looper >>= \rig -> map (_ + 1) (Array.findIndex (\candidate -> candidate.name == st.calSource && candidate.available) rig.sources) of
      Nothing -> H.modify_ _ { calSaid = "choose the input the oscillator is heard on; the one chosen is not there" }
      Just index -> do
        void $ liftAff $ attempt $ toAffE $ Rig.setCv { set: [ { bus: busOf st.calJack, level: st.calFrom / 10.0 } ], esx: [] }
        _ <- liftEffect (verb (Verb.Tune index))
        -- Heard as well as read: the daemon plays the input it is listening
        -- to. Left as it was if it was already on.
        let monitored = maybe false (not <<< _.monitor) st.looper
        when monitored $ void $ liftEffect (verb (Verb.Monitor true))
        H.modify_ _ { calTuning = true, calMonitored = monitored, calBusy = "holding " <> show2 st.calFrom <> " V and listening", calSaid = "" }

  -- The jack back to rest, the daemon told to stop listening, and the
  -- setting written down if nobody has said what it is.
  StopTuning -> do
    st <- H.get
    _ <- liftEffect (verb (Verb.Tune 0))
    when st.calMonitored $ void $ liftEffect (verb (Verb.Monitor false))
    void $ liftAff $ attempt $ toAffE $ Rig.setCv { set: [ { bus: busOf st.calJack, level: 0.0 } ], esx: [] }
    H.modify_ _ { calBusy = "", calTuning = false, calMonitored = false }

  -- Which input is this oscillator on? Nobody should have to know what the
  -- daemon's source names mean. So each input in turn is listened to at the
  -- starting voltage and a volt above it: the one whose pitch moves by an
  -- octave is the one the jack is driving.
  FindInput -> idle do
    st <- H.get
    let
      bus = busOf st.calJack
      set volts = void $ liftAff $ attempt $ toAffE $ Rig.setCv { set: [ { bus, level: volts / 10.0 } ], esx: [] }
      -- The pitch the daemon reports for this source, once it has had time
      -- to hear the new voltage; zero if it finds none.
      hear index = do
        liftAff (delay (Milliseconds 500.0))
        snapshot <- liftEffect Socket.latest
        pure (maybe 0.0 (\rig -> if rig.pitch.src == index then rig.pitch.hz else 0.0) snapshot)
      candidates = maybe [] (\rig -> Array.catMaybes (Array.mapWithIndex (\at source -> if source.available then Just { index: at + 1, name: source.name, from: source.from } else Nothing) rig.sources)) st.looper
    H.modify_ _ { calBusy = "listening to each input in turn", calSaid = "" }
    heard <- candidates # traverse \candidate -> do
      _ <- liftEffect (verb (Verb.Tune candidate.index))
      set st.calFrom
      low <- hear candidate.index
      set (st.calFrom + 1.0)
      high <- hear candidate.index
      pure { candidate, octaves: if low > 0.0 && high > 0.0 then Number.log (high / low) / Number.log 2.0 else 0.0, low, high }
    _ <- liftEffect (verb (Verb.Tune 0))
    set 0.0
    H.modify_ case Array.find (\found -> Number.abs (found.octaves - 1.0) < 0.25) heard of
      Just found -> _
        { calBusy = "", calSource = found.candidate.name
        , calSaid = "jack " <> show st.calJack <> " is heard on " <> found.candidate.name <> " (" <> found.candidate.from <> "): its pitch moved "
            <> show2 found.octaves <> " octaves for one volt, " <> show2 found.low <> " to " <> show2 found.high <> " Hz."
        }
      Nothing -> _
        { calBusy = ""
        , calSaid = "no input answered: none changed pitch by about an octave when jack " <> show st.calJack
            <> " moved by a volt. Is the jack patched to the oscillator's pitch input, and the oscillator to an input?"
        }

  -- The note nearest what is heard becomes the note the starting voltage
  -- IS. Nobody can dial a panel knob to the cent, and nobody needs to: the
  -- sweep measures where the oscillator really is, and the table places the
  -- declared note exactly.
  Declare -> do
    st <- H.get
    for_ (heardNow st) \base ->
      H.modify_ _
        { calBase = Just base.midi
        , calCoarse = "tuned to " <> base.note <> " at " <> show2 st.calFrom <> " V, " <> show (Int.round (Number.abs base.offsetCents)) <> " cents " <> (if base.offsetCents >= 0.0 then "sharp" else "flat")
        , calSaid = show2 st.calFrom <> " V is " <> base.note <> ". The sweep will measure how far off it really is, and the table will place " <> base.note <> " exactly."
        }
    handle StopTuning

  CheckPatch -> idle do
    st <- H.get
    let high = min st.calTo (st.calFrom + 2.0)
    H.modify_ _ { calBusy = "checking the patch", calSaid = "" }
    result <- record "calibrate-check" sweeping [ { volts: st.calFrom, target: 0.0, reached: true }, { volts: high, target: 0.0, reached: true } ]
    H.modify_ _
      { calBusy = ""
      , calSaid = case result of
          Left why -> why
          Right run -> patchVerdict (high - st.calFrom) (map _.hz run.steps)
      }

  Sweep -> idle do
    st <- H.get
    let
      wanted = grid st.calFrom st.calTo st.calStep
      -- One step wider at each end than what was asked for. An oscillator
      -- that is a few cents sharp at the bottom, or flat at the top, has its
      -- end notes just outside a sweep of exactly the range, and a table
      -- does not guess past what it measured.
      below = if st.calFrom - st.calStep >= -10.0 then [ st.calFrom - st.calStep ] else []
      above = case Array.last wanted of
        Just top | top + st.calStep <= 10.0 -> [ top + st.calStep ]
        _ -> []
      voltages = below <> wanted <> above
    if st.calLabel == "" then H.modify_ _ { calSaid = "name the signal path first: the table is looked up by that name" }
    else if Array.length wanted < 2 then H.modify_ _ { calSaid = "the sweep needs at least two voltages: check from, to and the step" }
    else do
      H.modify_ _ { calBusy = "sweeping", calSaid = "", calBefore = Nothing, calAfter = Nothing, calHeard = 0, calKept = "" }
      result <- record "calibrate-before" sweeping (voltages <#> \volts -> { volts, target: 0.0, reached: true })
      case result of
        Left why -> H.modify_ _ { calBusy = "", calSaid = why }
        Right run -> case Array.find (\step -> step.reads == 0) run.steps of
          Just silent -> H.modify_ _
            { calBusy = "", calBefore = Just (aimed (anchorOf st run) run)
            , calSaid = "no pitch could be read at " <> show2 silent.volts <> " V, so there is no table. Listen to the take: is the oscillator sounding, and above about 25 Hz?"
            }
          Nothing -> do
            let before = aimed (anchorOf st run) run
            H.modify_ _
              { calBusy = "", calBefore = Just before
              , calSaid = case Judge.problems (points before) of
                  [] -> "swept " <> show (Array.length before.steps) <> " steps, one wider at each end than asked, from " <> noteAt (asked st before) <> ". Now play it back through the table to see what is left."
                  found -> "this sweep would not be kept: " <> String.joinWith "; " (map Judge.problemText found)
              }

  Verify -> idle do
    st <- H.get
    case st.calBefore of
      Nothing -> H.modify_ _ { calSaid = "sweep first: there is no table to play through yet" }
      Just before -> do
        now <- liftEffect nowIso
        let
          table = Stored.table (tableOf (st { calHeard = 0 }) now before)
          -- The same pitches the sweep aimed at, at the voltages the table
          -- says will produce them. An oscillator that runs flat never got
          -- as high as the ideal pitch of the top step, so the table has no
          -- voltage for it: that step is played (at the last voltage
          -- measured, which is what a realiser does) and marked out of reach.
          wanted = (asked st before).steps <#> \step -> { volts: realise table step.target, target: step.target, reached: covers table step.target }
        H.modify_ _ { calBusy = "playing through the table", calSaid = "" }
        result <- record "calibrate-after" sweeping wanted
        H.modify_ case result of
          Left why -> _ { calBusy = "", calSaid = why }
          Right after -> _
            { calBusy = "", calAfter = Just after
            , calSaid = "through the table, the worst step is " <> show2 (worst after) <> " cents from its target; before, it was " <> show2 (worst (asked st before)) <> "."
                <> case Array.length (Array.filter (not <<< _.reached) after.steps) of
                  0 -> ""
                  beyond -> " " <> show beyond <> (if beyond == 1 then " step is" else " steps are")
                    <> " beyond what the sweep measured and not counted: the oscillator did not get that far within these voltages, and a table does not guess past its last measurement. Sweep a little wider to cover them."
            }

  Heard octaves -> H.modify_ \st -> st { calHeard = st.calHeard + octaves }

  Keep -> idle do
    st <- H.get
    case st.calBefore of
      Nothing -> H.modify_ _ { calSaid = "sweep first: there is nothing to keep" }
      Just before -> do
        now <- liftEffect nowIso
        let table = tableOf st now before
        case Judge.problems table.points of
          found | not (Array.null found) ->
            H.modify_ _ { calSaid = "not kept: " <> String.joinWith "; " (map Judge.problemText found) }
          _ -> do
            H.modify_ _ { calBusy = "keeping the table", calSaid = "" }
            kept <- liftAff (keep st table)
            H.modify_ case kept of
              Left why -> _ { calBusy = "", calSaid = "not kept: " <> why }
              Right hash -> _ { calBusy = "", calKept = hash, calSaid = "kept as " <> st.calLabel <> ". Every page that plays this path will use it from its next load." }

  Play after withTone -> do
    st <- H.get
    for_ (if after then st.calAfter else st.calBefore) \run -> liftEffect $ playImpl run.take
      (run.steps <#> \step -> { at: step.at + settleSecs, hz: step.target, secs: holdMs / 1000.0 - settleSecs - guardSecs })
      withTone

  StopPlaying -> liftEffect stopImpl
  where
  -- One thing at a time: a sweep in progress owns the jack and the capture.
  idle work = do
    st <- H.get
    when (st.calBusy == "") work

-- | The note the daemon hears on the input it is listening to, if it hears
-- | one: the nearest note and how far the pitch sits from it.
heardNow :: forall r. Record (Fields r) -> Maybe Judge.Base
heardNow st = st.looper >>= \rig -> if rig.pitch.src > 0 && rig.pitch.hz > 0.0 then Just (Judge.baseOf rig.pitch.hz) else Nothing

-- | What two points say about the patch. Two, never one: a dead audio
-- | stream answers a ping happily, and one pitch says nothing about whether
-- | the voltage arrives.
patchVerdict :: Number -> Array Number -> String
patchVerdict volts pitches = case pitches of
  [ low, high ]
    | low <= 0.0 && high <= 0.0 -> "silent: no pitch at either voltage. Is the oscillator patched to the input being recorded, and sounding?"
    | low <= 0.0 || high <= 0.0 -> "a pitch at one voltage and none at the other: the oscillator may be falling out of range"
    | otherwise ->
        let
          moved = Number.log (high / low) / Number.log 2.0
        in
          if Number.abs moved < 0.1 then "open: the pitch did not move (" <> show2 low <> " Hz at both). The voltage is not reaching the oscillator's pitch input."
          else if Number.abs (moved - volts) > 0.25 then "wrong depth: the pitch moved " <> show2 moved <> " octaves for " <> show2 volts <> " V. An attenuator in the path, or not a volt-per-octave input."
          else "closed: " <> show2 low <> " Hz to " <> show2 high <> " Hz, " <> show2 moved <> " octaves for " <> show2 volts <> " V. Ready to sweep."
  _ -> "the check did not return two pitches"

-- | The largest error among the steps the table could reach.
worst :: Run -> Number
worst run = fromMaybe 0.0 (maximum (map (Number.abs <<< errorOf) (Array.filter _.reached run.steps)))

noteAt :: Run -> String
noteAt run = (Judge.baseOf (fromMaybe 0.0 (Array.head run.steps <#> _.hz))).note

-- | Keep a table: its content, its name, its place in the collection the
-- | realisers read, and a note of which unit it was and where, under a ref
-- | of its own so that a path's calibrations are a history.
keep :: forall r. Record (Fields r) -> Stored -> Aff (Either String String)
keep st table = do
  existing <- Tables.curated
  case existing of
    Left why -> pure (Left why)
    -- One label, one output. A realiser looks a table up by its label, so
    -- the same label on two outputs is two different facts under one name.
    Right tables | Just other <- Array.find (\kept -> kept.label == table.label && kept.bus /= table.bus) tables ->
      pure (Left ("the name " <> table.label <> " is already a table for another output (bus " <> show other.bus <> "). Give this path its own name."))
    Right _ -> ask Api.putContent { kind: Stored.kind, payload: Stored.write table } >>= case _ of
      Left trouble -> pure (Left (troubleText trouble))
      Right { hash } -> do
        labels <- ask Api.listLabels { hash: Just hash }
        unless (either (const false) (Array.any (\label -> label.name == table.label)) labels) $ void $
          ask Api.addLabel
            { contentHash: hash, name: table.label, source: Just "quadrat", harmonicRoot: Nothing, harmonicScale: Nothing, harmonicChord: Nothing, tags: Nothing }
        favourites <- ask Api.listFavorites { collection: Just Stored.collection }
        curated <-
          if either (const false) (Array.any (\favourite -> favourite.contentHash == hash)) favourites then pure (Right unit)
          else map (const unit) <$> ask Api.addFavorite { contentHash: hash, collection: Stored.collection }
        case curated of
          Left trouble -> pure (Left ("the table is stored, but not where realisers look: " <> troubleText trouble))
          Right _ -> do
            -- The notes: which unit, where, what was said, and the two
            -- recordings. Beside the table, not inside it, so that fixing a
            -- comment does not change what the table is called.
            let
              written = Notes.write
                { label: table.label, table: hash, module: st.calModule, moduleKey: Inventory.keyOf st.calModules st.calModule
                , place: st.calWhere, comment: st.calComment
                , before: maybe "" _.take st.calBefore, after: maybe "" _.take st.calAfter
                }
              refName = Notes.refFor table.label
            noted <- ask Api.putContent { kind: Notes.kind, payload: written }
            current <- ask Api.getRef { name: refName }
            for_ noted \{ hash: notesHash } ->
              ask Api.moveRef { name: refName, to: notesHash, expected: either (const Nothing) (Just <<< _.hash) current }
            pure (Right hash)

-- ── The page ───────────────────────────────────────────────────────────────

show2 :: Number -> String
show2 = toStringWith (fixed 2)

cls :: forall r i. String -> HP.IProp (class :: String | r) i
cls = HP.class_ <<< HH.ClassName

text :: forall w i. String -> HH.HTML w i
text = HH.text

view :: forall r w act. (Msg -> act) -> PageLayout.Side w act -> Record (Fields r) -> HH.HTML w act
view wrap drawer st =
  PageLayout.layout
    { statement
    , main: [ controls, said, curves, steps ]
    , side: [ about ]
    , drawer
    }
  where
  statement =
    Statement.statement Statement.defaultStatement
      { class_ = "q-say is-calibrate"
      , sentences =
          [ [ text "Sweep jack ", choice (show st.calJack) (map show (Array.range 1 8)) (\chosen -> wrap (SetJack (fromMaybe 1 (Int.fromString chosen))))
            , text " from ", number st.calFrom SetFrom, text " V to ", number st.calTo SetTo, text " V in steps of ", number st.calStep SetStep
            , text " V, heard on ", sourceChoice, text "."
            ]
          , [ text ("That is " <> show (Array.length (grid st.calFrom st.calTo st.calStep)) <> " steps and one more at each end, " <> show2 (holdMs / 1000.0) <> " s each, each read " <> show readings <> " times."
                <> maybe "" (\midi -> " " <> show2 st.calFrom <> " V is " <> Judge.noteName midi <> ".") st.calBase) ]
          ]
      , warnings =
          if st.calBusy == "" then []
          else [ HH.p [ cls "q-clash is-soft" ] [ text (st.calBusy <> "\x2026") ] ]
      }

  -- | The inputs, each saying what it is: a name alone ("hits") is no help
  -- | to anyone who did not write the daemon's start-up line.
  sourceChoice =
    HH.select [ cls "q-slot", HE.onValueChange (wrap <<< SetSource) ]
      ( [ HH.option [ HP.value "", HP.selected (st.calSource == "") ] [ text "\x2014" ] ]
          <> maybe [] (\rig -> Array.filter _.available rig.sources <#> \source ->
            HH.option [ HP.value source.name, HP.selected (source.name == st.calSource) ] [ text (source.name <> ": " <> source.from) ]) st.looper
      )

  -- | The paths already calibrated. Choosing one puts back what its last
  -- | calibration recorded.
  pathChoice =
    HH.label [ cls "q-field" ]
      [ HH.span [ cls "q-field-name" ] [ text "A path calibrated before" ]
      , HH.select [ cls "q-field-input", HE.onValueChange (wrap <<< Pick) ]
          ( [ HH.option [ HP.value "", HP.selected (not (Array.any (\kept -> kept.label == st.calLabel) st.calPaths)) ] [ text "a new path" ] ]
              <> (st.calPaths <#> \kept ->
                HH.option [ HP.value kept.label, HP.selected (kept.label == st.calLabel) ]
                  [ text (kept.label <> maybe "" (\named -> " (" <> named <> ")") kept.module) ])
          )
      ]

  choice current options pick =
    HH.select [ cls "q-slot", HE.onValueChange pick ]
      (options <#> \option -> HH.option [ HP.value option, HP.selected (option == current) ] [ text (if option == "" then "\x2014" else option) ])

  number value set =
    HH.input [ cls "q-slot is-number", HP.type_ HP.InputNumber, HP.value (show2 value), HP.step (HP.Step 0.05), HE.onValueChange (wrap <<< set) ]

  field label value set hint =
    HH.label [ cls "q-field" ]
      [ HH.span [ cls "q-field-name" ] [ text label ]
      , HH.input [ cls "q-field-input", HP.value value, HP.placeholder hint, HE.onValueInput (wrap <<< set) ]
      ]

  busy = st.calBusy /= ""

  button label title enabled msg =
    HH.button [ cls "q-pill", HP.title title, HP.disabled (busy || not enabled), HE.onClick \_ -> wrap msg ] [ text label ]

  controls =
    HH.section [ cls "q-calibrate-controls" ]
      [ HH.div [ cls "q-calibrate-names" ]
          [ pathChoice
          , field "Signal path" st.calLabel SetLabel "what realisers look it up by, e.g. plaits-es9-jack1"
          , HH.label [ cls "q-field" ]
              [ HH.span [ cls "q-field-name" ] [ text "Module" ]
              , HH.input
                  [ cls "q-field-input", HP.value st.calModule, HP.placeholder "choose from what you own, or type one that is not listed"
                  , HP.attr (HH.AttrName "list") "q-calibrate-modules", HE.onValueInput (wrap <<< SetModule)
                  ]
              , HH.datalist [ HP.id "q-calibrate-modules" ] (st.calModules <#> \owned -> HH.option [ HP.value (Inventory.said owned) ] [])
              ]
          , field "Where" st.calWhere SetWhere "which case, which row"
          , field "Coarse setting" st.calCoarse SetCoarse "the front panel as it stands: a table is true only for this"
          , field "Notes" st.calComment SetComment "anything worth knowing later"
          ]
      , HH.div [ cls "q-calibrate-acts" ]
          [ button "Find the input" "nudge the jack by a volt and see which input's pitch moves by an octave" true FindInput
          , if st.calTuning then HH.button [ cls "q-pill", HE.onClick \_ -> wrap StopTuning ] [ text "Stop tuning" ]
            else button "Tune" "hold the starting voltage and show the note being heard, to put the oscillator where the sweep should begin" true Tune
          , button "Check the patch" "two voltages, two pitches: is the voltage reaching the oscillator?" true CheckPatch
          , button "Sweep" "the before: ideal voltages, and how far each pitch falls from a perfect volt per octave" true Sweep
          , button "Play through the table" "the after: the same pitches at the voltages the table gives" (isJust st.calBefore) Verify
          , button "Keep" "store the table where every realiser looks for it" (isJust st.calBefore) Keep
          ]
      , case st.calBefore of
          Nothing -> text ""
          Just before ->
            HH.div [ cls "q-calibrate-hear" ]
              [ text ("The detector reads the first step as " <> noteAt (asked st before) <> moved <> ". If you hear another octave: ")
              , button "an octave down" "the ear decides the octave; the detector everything finer" true (Heard (-1))
              , button "an octave up" "the ear decides the octave; the detector everything finer" true (Heard 1)
              ]
      , HH.div [ cls "q-calibrate-play" ]
          ( play "before" false st.calBefore <> play "after" true st.calAfter
              <> (if isJust st.calBefore then [ HH.button [ cls "q-pill", HE.onClick \_ -> wrap StopPlaying ] [ text "Stop" ] ] else [])
          )
      ]

  moved = case st.calHeard of
    0 -> ""
    octaves -> ", moved " <> show (Int.round (Number.abs (Int.toNumber octaves))) <> (if octaves > 0 then " up" else " down") <> " by ear"

  play name after run = case run of
    Nothing -> []
    Just _ ->
      [ HH.button [ cls "q-pill", HP.title "the recording as it was made", HE.onClick \_ -> wrap (Play after false) ] [ text ("Play the " <> name) ]
      , HH.button [ cls "q-pill", HP.title "with a sine at each step's target: a step that is off beats against it", HE.onClick \_ -> wrap (Play after true) ] [ text ("Play the " <> name <> " with a reference tone") ]
      ]

  said =
    HH.div_
      [ if st.calTuning then tuner else text ""
      , if st.calSaid == "" then text "" else HH.p [ cls "q-calibrate-said" ] [ text st.calSaid ]
      ]

  -- | The note being heard, and a needle: the middle is the note, the ends
  -- | are half a semitone flat and sharp.
  tuner = case heardNow st of
    Nothing -> HH.p [ cls "q-calibrate-tuned" ] [ text "no pitch" ]
    Just base ->
      HH.div [ cls "q-calibrate-tuner" ]
        [ HH.p [ cls "q-calibrate-tuned" ]
            [ text base.note
            , HH.span [ cls "q-calibrate-cents" ]
                [ text ((if base.offsetCents >= 0.0 then " +" else " \x2212") <> show (Int.round (Number.abs base.offsetCents)) <> " cents") ]
            ]
        , HH.div [ cls "q-calibrate-needle" ]
            [ HH.span [ cls "q-calibrate-mark", HP.style ("left: " <> show2 (50.0 + clamp (-50.0) 50.0 base.offsetCents) <> "%") ] [] ]
        , HH.p [ cls "q-calibrate-hz" ] [ text (maybe "" (\rig -> show2 rig.pitch.hz <> " Hz at " <> show2 st.calFrom <> " V") st.looper) ]
        , HH.button [ cls "q-pill q-calibrate-declare", HP.title "a panel knob cannot be dialled to the cent, and need not be: the table will place this note exactly", HE.onClick \_ -> wrap Declare ]
            [ text ("This is " <> base.note <> ": calibrate from here") ]
        ]

  -- | Both error curves on one pair of axes: cents from the target at each
  -- | step, the before and the after.
  curves = case st.calBefore of
    Nothing -> text ""
    Just before ->
      let
        shown = asked st before
        runs = [ shown ] <> maybe [] Array.singleton st.calAfter
        errors = Array.concatMap (\run -> map errorOf (Array.filter _.reached run.steps)) runs
        reach = max 5.0 (fromMaybe 5.0 (maximum (map Number.abs errors)))
        count = max 1 (Array.length shown.steps - 1)
        x index = 40.0 + 540.0 * Int.toNumber index / Int.toNumber count
        y cents = 110.0 - 95.0 * cents / reach
        -- Only the steps the table could reach: one it could not is not a
        -- point on the table's curve.
        line run = String.joinWith " " (Array.catMaybes (Array.mapWithIndex (\index step -> if step.reached then Just (show2 (x index) <> "," <> show2 (y (errorOf step))) else Nothing) run.steps))
        svg name attributes children = HH.elementNS (HH.Namespace "http://www.w3.org/2000/svg") (HH.ElemName name) (map (\pair -> HP.attr (HH.AttrName pair.name) pair.value) attributes) children
        attribute name value = { name, value }
      in
        HH.figure [ cls "q-calibrate-curves" ]
          [ svg "svg" [ attribute "viewBox" "0 0 600 220", attribute "role" "img" ]
              ( [ svg "line" [ attribute "x1" "40", attribute "x2" "580", attribute "y1" "110", attribute "y2" "110", attribute "class" "q-calibrate-zero" ] []
                , svg "text" [ attribute "x" "4", attribute "y" (show2 (y reach + 4.0)), attribute "class" "q-calibrate-tick" ] [ text ("+" <> show2 reach) ]
                , svg "text" [ attribute "x" "4", attribute "y" (show2 (y (negate reach) + 4.0)), attribute "class" "q-calibrate-tick" ] [ text ("\x2212" <> show2 reach) ]
                , svg "polyline" [ attribute "points" (line shown), attribute "class" "q-calibrate-before" ] []
                ]
                  <> maybe [] (\after -> [ svg "polyline" [ attribute "points" (line after), attribute "class" "q-calibrate-after" ] [] ]) st.calAfter
              )
          , HH.figcaption_
              [ text
                  ( "Cents from a perfect volt per octave at each step. Before: worst " <> show2 (worst shown)
                      <> maybe "." (\after -> "; after, through the table: worst " <> show2 (worst after) <> ".") st.calAfter
                  )
              ]
          ]

  steps = case st.calBefore of
    Nothing -> text ""
    Just before ->
      HH.table [ cls "q-calibrate-steps" ]
        [ HH.thead_ [ HH.tr_ (map (\heading -> HH.th_ [ text heading ]) [ "V", "Hz", "cents off", "readings", "agreement", "after: cents off" ]) ]
        , HH.tbody_
            ( Array.mapWithIndex
                ( \index step ->
                    HH.tr [ cls (if step.reads == 0 || (step.reads > 2 && step.agreementCents > 5.0) then "is-suspect" else "") ]
                      [ HH.td_ [ text (show2 step.volts) ]
                      , HH.td_ [ text (if step.reads == 0 then "no pitch" else show2 step.hz) ]
                      , HH.td_ [ text (show2 (errorOf step)) ]
                      , HH.td_ [ text (show step.reads) ]
                      , HH.td_ [ text (show2 step.agreementCents) ]
                      , HH.td_ [ text (maybe "" (\after -> maybe "" (\played -> if played.reached then show2 (errorOf played) else "beyond the sweep") (Array.index after.steps index)) st.calAfter) ]
                      ]
                )
                (asked st before).steps
            )
        ]

  about =
    HH.div [ cls "q-calibrate-about" ]
      [ HH.p_ [ text "A table is true for one signal path: this output, this oscillator, this front-panel setting. Name the path, and say what the setting is." ]
      , HH.p_ [ text "Patch the jack to the oscillator's volt-per-octave input and the oscillator to the input being recorded. Let the oscillator warm up first." ]
      , HH.p_ [ text "Check the patch before sweeping. A sweep with no sound reaching the input reads the same pitch at every step, and is refused." ]
      , HH.p_ [ text ("The lowest pitch that can be read is about " <> show2 (fewestPeriods / 0.341) <> " Hz: below it a reading rests on too few periods to trust.") ]
      , if st.calKept == "" then text "" else HH.p_ [ text ("Kept: " <> String.take 12 st.calKept <> "\x2026") ]
      ]
