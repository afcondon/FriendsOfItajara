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
import Data.Looper.Verb (Verb(..))
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
import Quadrat.Layout as PageLayout
import Quadrat.Rig as Rig
import Quadrat.Schedule as Schedule
import Quadrat.Slug as Slug
import Quadrat.Tables as Tables
import Reef.Calibration (realise)
import Reef.Calibration.Judge as Judge
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
  | r
  )

initial
  :: { calLabel :: String, calModule :: String, calWhere :: String, calComment :: String, calCoarse :: String
     , calJack :: Int, calSource :: String, calFrom :: Number, calTo :: Number, calStep :: Number
     , calBusy :: String, calSaid :: String, calBefore :: Maybe Run, calAfter :: Maybe Run, calHeard :: Int, calKept :: String
     }
initial =
  { calLabel: "", calModule: "", calWhere: "", calComment: "", calCoarse: ""
  , calJack: 1, calSource: "", calFrom: 0.0, calTo: 5.0, calStep: 0.25
  , calBusy: "", calSaid: "", calBefore: Nothing, calAfter: Nothing, calHeard: 0, calKept: ""
  }

data Msg
  = SetLabel String
  | SetModule String
  | SetWhere String
  | SetComment String
  | SetCoarse String
  | SetJack Int
  | SetSource String
  | SetFrom String
  | SetTo String
  | SetStep String
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
  -> Array { volts :: Number, target :: Number }
  -> H.HalogenM (Record (Fields r)) act slots o m (Either String Run)
record prefix wanted = do
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
      liftEffect $ for_ [ CaptureArm false, CaptureQuantise 0, CaptureBars 0, CaptureStop 0, Capture index ] verb
      opened <- liftAff (awaitCapture 50)
      if not opened then pure (Left "the daemon did not start recording; is Itajara running?")
      else do
        marks <- wanted # traverse \step -> do
          set step.volts
          mark <- liftEffect Schedule.at
          H.modify_ _ { calBusy = "holding " <> show2 step.volts <> " V" }
          liftAff (delay (Milliseconds holdMs))
          pure (map (\at -> { volts: step.volts, target: step.target, at }) mark)
        set 0.0
        _ <- liftEffect (verb EndCapture)
        liftAff (delay (Milliseconds 200.0))
        _ <- liftEffect (verb (WriteCapture take))
        liftAff (delay (Milliseconds 1200.0))
        case Array.catMaybes marks of
          marked | Array.length marked /= Array.length wanted -> pure (Left "the recording stopped before the sweep ended")
          marked -> do
            H.modify_ _ { calBusy = "reading the pitches" }
            let regions = marked <#> \step -> { start: step.at + settleSecs, end: step.at + holdMs / 1000.0 - guardSecs }
            measured <- liftAff (toAffE (measureImpl take regions readings))
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
                          , spreadCents: combined.spreadCents, agreementCents: combined.agreementCents
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

-- | The pitch a perfect volt per octave would give at this voltage, from the
-- | first step of a run.
ideal :: Run -> Number -> Number
ideal run volts = case Array.head run.steps of
  Just first | first.hz > 0.0 -> first.hz * Number.pow 2.0 (volts - first.volts)
  _ -> 0.0

-- | How far a step's pitch fell from its target, in cents.
errorOf :: Step -> Number
errorOf step = if step.hz > 0.0 && step.target > 0.0 then Judge.centsBetween step.target step.hz else 0.0

-- | The before, with each step's target filled in now that the first step's
-- | pitch is known.
aimed :: Run -> Run
aimed run = run { steps = run.steps <#> \step -> step { target = ideal run step.volts } }

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

  CheckPatch -> idle do
    st <- H.get
    let high = min st.calTo (st.calFrom + 2.0)
    H.modify_ _ { calBusy = "checking the patch", calSaid = "" }
    result <- record "calibrate-check" [ { volts: st.calFrom, target: 0.0 }, { volts: high, target: 0.0 } ]
    H.modify_ _
      { calBusy = ""
      , calSaid = case result of
          Left why -> why
          Right run -> patchVerdict (high - st.calFrom) (map _.hz run.steps)
      }

  Sweep -> idle do
    st <- H.get
    let voltages = grid st.calFrom st.calTo st.calStep
    if st.calLabel == "" then H.modify_ _ { calSaid = "name the signal path first: the table is looked up by that name" }
    else if Array.length voltages < 2 then H.modify_ _ { calSaid = "the sweep needs at least two voltages: check from, to and the step" }
    else do
      H.modify_ _ { calBusy = "sweeping", calSaid = "", calBefore = Nothing, calAfter = Nothing, calHeard = 0, calKept = "" }
      result <- record "calibrate-before" (voltages <#> \volts -> { volts, target: 0.0 })
      case result of
        Left why -> H.modify_ _ { calBusy = "", calSaid = why }
        Right run -> case Array.find (\step -> step.reads == 0) run.steps of
          Just silent -> H.modify_ _
            { calBusy = "", calBefore = Just (aimed run)
            , calSaid = "no pitch could be read at " <> show2 silent.volts <> " V, so there is no table. Listen to the take: is the oscillator sounding, and above about 25 Hz?"
            }
          Nothing -> do
            let before = aimed run
            H.modify_ _
              { calBusy = "", calBefore = Just before
              , calSaid = case Judge.problems (points before) of
                  [] -> "swept " <> show (Array.length before.steps) <> " steps from " <> noteAt before <> ". Now play it back through the table to see what is left."
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
          -- says will produce them.
          wanted = before.steps <#> \step -> { volts: realise table step.target, target: step.target }
        H.modify_ _ { calBusy = "playing through the table", calSaid = "" }
        result <- record "calibrate-after" wanted
        H.modify_ case result of
          Left why -> _ { calBusy = "", calSaid = why }
          Right after -> _
            { calBusy = "", calAfter = Just after
            , calSaid = "through the table, the worst step is " <> show2 (worst after) <> " cents from its target; before, it was " <> show2 (worst before) <> "."
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

worst :: Run -> Number
worst run = fromMaybe 0.0 (maximum (map (Number.abs <<< errorOf) run.steps))

noteAt :: Run -> String
noteAt run = (Judge.baseOf (fromMaybe 0.0 (Array.head run.steps <#> _.hz))).note

-- | Keep a table: its content, its name, its place in the collection the
-- | realisers read, and a note of which unit it was and where, under a ref
-- | of its own so that a path's calibrations are a history.
keep :: forall r. Record (Fields r) -> Stored -> Aff (Either String String)
keep st table = do
  existing <- Tables.list
  case existing of
    Left why -> pure (Left why)
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
              notes = "{\"label\":" <> show table.label <> ",\"table\":" <> show hash
                <> ",\"module\":" <> show st.calModule
                <> ",\"where\":" <> show st.calWhere
                <> ",\"comment\":" <> show st.calComment
                <> ",\"before\":" <> show (maybe "" _.take st.calBefore)
                <> ",\"after\":" <> show (maybe "" _.take st.calAfter)
                <> "}"
              refName = "quadrat/calibrations/" <> table.label
            noted <- ask Api.putContent { kind: "calibration-notes", payload: notes }
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
            , text " V, heard on ", choice st.calSource sources (wrap <<< SetSource), text "."
            ]
          , [ text ("That is " <> show (Array.length (grid st.calFrom st.calTo st.calStep)) <> " steps of " <> show2 (holdMs / 1000.0) <> " s, each read " <> show readings <> " times.") ]
          ]
      , warnings =
          if st.calBusy == "" then []
          else [ HH.p [ cls "q-clash is-soft" ] [ text (st.calBusy <> "\x2026") ] ]
      }

  sources = [ "" ] <> maybe [] (\rig -> map _.name (Array.filter _.available rig.sources)) st.looper

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
          [ field "Signal path" st.calLabel SetLabel "what realisers look it up by, e.g. plaits-es9-jack1"
          , field "Module" st.calModule SetModule "which unit: the one in the top row, say"
          , field "Where" st.calWhere SetWhere "which case, which row"
          , field "Coarse setting" st.calCoarse SetCoarse "the front panel as it stands: a table is true only for this"
          , field "Notes" st.calComment SetComment "anything worth knowing later"
          ]
      , HH.div [ cls "q-calibrate-acts" ]
          [ button "Check the patch" "two voltages, two pitches: is the voltage reaching the oscillator?" true CheckPatch
          , button "Sweep" "the before: ideal voltages, and how far each pitch falls from a perfect volt per octave" true Sweep
          , button "Play through the table" "the after: the same pitches at the voltages the table gives" (isJust st.calBefore) Verify
          , button "Keep" "store the table where every realiser looks for it" (isJust st.calBefore) Keep
          ]
      , case st.calBefore of
          Nothing -> text ""
          Just before ->
            HH.div [ cls "q-calibrate-hear" ]
              [ text ("The detector reads the first step as " <> noteAt before <> moved <> ". If you hear another octave: ")
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

  said = if st.calSaid == "" then text "" else HH.p [ cls "q-calibrate-said" ] [ text st.calSaid ]

  -- | Both error curves on one pair of axes: cents from the target at each
  -- | step, the before and the after.
  curves = case st.calBefore of
    Nothing -> text ""
    Just before ->
      let
        runs = [ before ] <> maybe [] Array.singleton st.calAfter
        errors = Array.concatMap (\run -> map errorOf run.steps) runs
        reach = max 5.0 (fromMaybe 5.0 (maximum (map Number.abs errors)))
        count = max 1 (Array.length before.steps - 1)
        x index = 40.0 + 540.0 * Int.toNumber index / Int.toNumber count
        y cents = 110.0 - 95.0 * cents / reach
        line run = String.joinWith " " (Array.mapWithIndex (\index step -> show2 (x index) <> "," <> show2 (y (errorOf step))) run.steps)
        svg name attributes children = HH.elementNS (HH.Namespace "http://www.w3.org/2000/svg") (HH.ElemName name) (map (\pair -> HP.attr (HH.AttrName pair.name) pair.value) attributes) children
        attribute name value = { name, value }
      in
        HH.figure [ cls "q-calibrate-curves" ]
          [ svg "svg" [ attribute "viewBox" "0 0 600 220", attribute "role" "img" ]
              ( [ svg "line" [ attribute "x1" "40", attribute "x2" "580", attribute "y1" "110", attribute "y2" "110", attribute "class" "q-calibrate-zero" ] []
                , svg "text" [ attribute "x" "4", attribute "y" (show2 (y reach + 4.0)), attribute "class" "q-calibrate-tick" ] [ text ("+" <> show2 reach) ]
                , svg "text" [ attribute "x" "4", attribute "y" (show2 (y (negate reach) + 4.0)), attribute "class" "q-calibrate-tick" ] [ text ("\x2212" <> show2 reach) ]
                , svg "polyline" [ attribute "points" (line before), attribute "class" "q-calibrate-before" ] []
                ]
                  <> maybe [] (\after -> [ svg "polyline" [ attribute "points" (line after), attribute "class" "q-calibrate-after" ] [] ]) st.calAfter
              )
          , HH.figcaption_
              [ text
                  ( "Cents from a perfect volt per octave at each step. Before: worst " <> show2 (worst before)
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
                      , HH.td_ [ text (maybe "" (\after -> maybe "" (show2 <<< errorOf) (Array.index after.steps index)) st.calAfter) ]
                      ]
                )
                before.steps
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
