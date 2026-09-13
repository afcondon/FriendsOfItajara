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
import Data.Either (Either(..), either)
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
import Effect.Ref as Ref
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
  -- | **This run is being made in order to be measured, not kept.** It paces
  -- | itself flat, divides itself, and hands its per-cell decay times to the
  -- | plan. See `DryRun`.
  , dry :: Boolean
  -- | **A stored set, opened on the bench.**
  -- |
  -- | The bands live here and the audio lives in the daemon, so until this
  -- | existed a set could be recorded, measured, saved — and then never looked
  -- | at again, while any stray capture left the two describing different
  -- | recordings. When something is open the page draws IT: its envelope came
  -- | off the file, its length is this, and the daemon's capture is nobody's
  -- | business. Cleared by anything that starts a new one.
  , opened :: Maybe
      { set :: String, take :: String, secs :: Number
      -- | What was played into each of its regions, straight from `set.json`.
      -- | A stored set's notes are a fact about it, not about this page's MIDI
      -- | buffer, so an opened set reads them from disk and never from `heard`.
      , notes :: Array (Array Int)
      -- | And the same, kept as the chords they were struck as. See
      -- | `Http.Meant.struck`.
      , struck :: Array (Array (Array Int))
      }
  -- | **Whether the take on the page came from a measuring run.**
  -- |
  -- | `dry` is true only WHILE one is going; this outlives it, because the
  -- | take does. A measuring pass records at the flat spacing by construction
  -- | — that is what it is for — so its samples all carry the trailing
  -- | silence the measurement exists to remove, and they look exactly like a
  -- | real take on the page. On 2026-09-12 a set was saved from one.
  , takeIsDry :: Boolean
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
  -- | **Which bank LETTER this lands on**, which is the blast radius: a write
  -- | deletes the slot's whole directory first. `bank` beside it is only the
  -- | legend printed on the bank, and the two were one field until 2026-09-12,
  -- | when "L" went into the name, no letter was sent, and the compiler put a
  -- | 4 x 12 on `A` over Squarp's own content.
  , letter :: String
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
  -- | **Which stored sets are ticked**, by name rather than by index: the list
  -- | is re-fetched after every write and a position means nothing across that.
  , picked :: Set String
  -- | Deleting is the one thing here that destroys work, so it is asked.
  , confirmDrop :: Boolean
  -- | **The write question, held open against one card.**
  -- |
  -- | Every write goes through this now, not only the replacing kind. A write
  -- | refuses a kit slot that already exists — and refuses *all* of it, the
  -- | free banks included — so "will this go through" is a question about the
  -- | card in front of you and cannot be answered from the manifest. Asking it
  -- | is one subprocess, and it is asked before every write rather than after
  -- | a refusal.
  , confirmWrite :: Maybe String
  -- | The answer, when it arrives. `Nothing` while the card is being read:
  -- | the modal is open and says so, because a card that takes a moment must
  -- | not look like a card with nothing on it.
  , preview :: Maybe Http.Preview
  -- | **The mounted card, read without being asked.**
  -- |
  -- | The same report, fetched on every refresh when exactly one card is
  -- | mounted, so that the letter strip can show which banks are taken while
  -- | you are choosing one — rather than at the write, which is after the
  -- | choice. One card because with two mounted there is no "the" card and
  -- | the strip would have to say which; that wants the destination picker,
  -- | which has one entry.
  , cardPeek :: Maybe Http.Preview
  -- | **Which set's own page is open.** The library row is a checkbox, a name
  -- | and a picture now; everything that used to be written out on it — the
  -- | date, what moved, the pattern line, the verbs — is behind one click,
  -- | because none of it is read while you are looking for a set and all of
  -- | it is read once you have found one.
  , openSet :: Maybe String
  -- | Its detail, when it arrives. `Nothing` while loading.
  , openSetInfo :: Maybe Http.StoredSet
  -- | **Stacked or sliced**, for a set placed from the Library.
  -- |
  -- | A placement decision, not a property of the cut: the audio is the same
  -- | twelve files either way and what changes is whether the manifest lists
  -- | them as layers the layer CV picks between, or concatenates them into one
  -- | file the start point indexes. Which is worth having as a control because
  -- | the answers differ in what they cost — twelve layers fill a voice, one
  -- | sliced file uses one of its twelve and leaves eleven.
  , placeSliced :: Boolean
  -- | **The arrangement, as a layer count.** Zero means the sweep's own
  -- | extent decides, which is the natural arrangement and what happened
  -- | before there was a way to say otherwise. The slices follow from it.
  , placeLayers :: Int
  -- | Stand beside what is on the voice, or take its place. A voice holds a
  -- | stack, so the two things you might mean are opposites.
  , placeAppend :: Boolean
  -- | **What the owner calls each input**, against the wire name the daemon
  -- | uses. A source is identified by `--source board=AUDIO4c:1,2` and has to
  -- | be, because a name that cannot be resolved to jacks is a session
  -- | recorded off the wrong one — but a wire name is a poor thing to pick
  -- | from under pressure, and `board` and `hits` beside each other cost a
  -- | whole 4 x 12 on 2026-09-11. The right name is not a fact about this rig:
  -- | it is "Jupiter 8", or "the Neumann", and only the person holding the
  -- | cable knows it. So the wire name stays the identity and this sits on
  -- | top, chosen once and read everywhere. Nothing routes on it.
  , srcNames :: Array Http.SourceName
  -- | **What was played into this take**, for the takes nobody sweeps.
  -- |
  -- | A swept run knows its pitches because it asked for them; a hand-played
  -- | one knows nothing, because the chord that made the audio is not in the
  -- | audio. Held in the page rather than fetched at division time because it
  -- | only exists while it is happening — record the chords first and wire this
  -- | afterwards, and those particular sets can never have their notes.
  -- |
  -- | Stamped in `Rig.nowMs`'s clock and converted to take-relative seconds
  -- | only at the end; see `notesFor`.
  , heard :: Array Rig.Struck
  -- | The MIDI inputs the browser can see, so the page can say that it is
  -- | listening to nothing BEFORE a take rather than after one.
  , midiIn :: Array String
  -- | Whether Web MIDI exists on this origin at all. See `Rig.midiAvailable` —
  -- | "no ports" and "no API" look identical from the page and want opposite
  -- | remedies.
  , midiOk :: Boolean
  }

-- | The two panels that became modals.
data Modal = DivisionModal | TriggerModal | PitchModal | SaveModal
           | InputsModal

derive instance Eq Modal

data Action
  -- | **Make this parameter a pitch**, or (with an empty label) stop it being
  -- | one. An Action rather than a `Sweep.Msg` because it has to FETCH the
  -- | table: the plan carries the measurement, not a pointer to it.
  = PickPitch Int String
  | Init
  | Poll
  | NameSource String String
  -- | Pick the kind from the sentence, where it arrives as its own name
  -- | rather than as a `Kind` — the slot is a `<select>` and a select carries
  -- | strings. `Kind.Bars` keeps the bar count the page is holding, because
  -- | choosing "bars" is not a statement about how many.
  | PickKindNamed String
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
  | SetLetter String
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
  -- | Compile the manifest onto a card. The flag is `--overwrite`, which
  -- | **deletes each kit slot's whole directory before writing it** — so it is
  -- | a separate press with the letters it destroys named on it, never a
  -- | default. See `askWrite`.
  | WriteCard String Boolean
  -- | Hold the write question open against a card — which reads the card —
  -- | or drop it.
  | AskWrite (Maybe String)
  -- | The card, read. Carried as its own action because the read is a
  -- | subprocess and the modal opens before it answers.
  | Previewed Http.Preview
  -- | Lay a placed set out as one sliced file, or as a stack of layers.
  | SetPlaceSliced Boolean
  -- | Arrange the ticked set into this many layers. See `Http.arrangementsOf`.
  | SetPlaceLayers Int
  | SetPlaceAppend Boolean
  | Play Int
  | HoverPlay Int
  | SetHoverPlays Boolean
  | OpenSweep Boolean
  | SweepMsg Sweep.Msg
  | OpenParam (Maybe Int)
  | RunSweep Int
  -- | **Run it once to find out how long each cell needs.**
  | DryRun Int
  -- | Flat spacing, or what the dry run measured.
  | UsePaced Boolean
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
  -- | Put a stored set back on the bench, drawn from its own take.
  | OpenSet String
  -- | Tick or untick one stored set.
  -- | Hear a set before doing anything irreversible to it.
  | HearSet String Int
  -- | One sample of one set, by index. The hover verb: it costs nothing and
  -- | answers "which one was this" in the only way that cannot be misread.
  | HearOne String Int
  -- | Open a set's own page, or shut it. Distinct from `OpenSet`, which puts
  -- | a set back on the bench: this only reads it.
  | PeekSet (Maybe String)
  | PickSet String
  -- | Tick all of them, or none.
  | PickAllSets Boolean
  | AskDrop Boolean
  | DropPicked
  -- | Every ticked set onto the card, each as its own kit.
  | PlacePicked

component :: forall q i o m. MonadAff m => H.Component q i o m
component = H.mkComponent
  { initialState: \_ ->
      { looper: Nothing, kind: Kind.DrumHits, bars: 1
      , armed: false, name: "", log: []
      , peaks: Nothing, regions: [], keep: Set.empty, busy: false, dry: false
      , opened: Nothing
      , takeIsDry: false
      , hoverPlays: false, playing: Nothing, showing: "", waiting: false
      , minGap: 300.0, divider: Divider.Attacks, equalN: 16, mine: false, kitMine: false, layerMode: ""
      , cardView: Nothing, bank: "WORKSHOP", letter: "", kit: "", voice: 1, cardBusy: false
      , sweep: Sweep.emptyPlan, sweepOpen: false, sweepAt: Nothing
      , sweepFork: Nothing, midiPorts: [], swept: false, sweepEdit: Nothing
      , schedule: [], sets: [], tables: [], tablesErr: "", overran: false
      , page: Bench, fill: Swept, pivot: Nothing
      , levels: [], modal: Nothing, kept: false, confirmKeep: false
      , picked: Set.empty, confirmDrop: false, confirmWrite: Nothing, preview: Nothing
      , cardPeek: Nothing, openSet: Nothing, openSetInfo: Nothing
      , placeSliced: false, placeLayers: 0, placeAppend: false, srcNames: []
      , heard: [], midiIn: [], midiOk: true }
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

-- | **Does a region end quiet, or is it still sounding when it is cut?**
-- |
-- | "Clean, with no overlap" is a property of the RECORDING, not something a
-- | spacing gives you for free. Measured 2026-09-11: at 3000 ms on a BIA the
-- | tail was still at 0.031 against a noise floor of 0.022 when the next hit
-- | landed — so every sample carried the front of its successor, and any layer
-- | stitched from them would click at every join.
-- |
-- | Read off the envelope the daemon already drew, so it costs no second pass
-- | over the audio and no round trip. `tail` is the loudest the region's last
-- | 60 ms got; `floor` is the quietest the whole take ever got; both are
-- | fractions of the take's own peak, so they mean the same thing whatever the
-- | gain was.
-- |
-- | **Stored rather than judged.** How much tail is too much depends on what
-- | the sample is for: a layer stitched into a column wants silence at the
-- | join, a one-shot fired from a pad does not care. So the set records two
-- | numbers and the page draws the comparison — the decision stays with whoever
-- | is sampling, which is where it belongs.
type Settled = { decay :: Number, floor :: Number }

settledFor
  :: Socket.Peaks -> Number
  -> Array { start :: Number, end :: Number }
  -> Array Settled
settledFor pk total rs =
  let
    n = Array.length pk.hi
    mag v = if v < 0 then -v else v
    ampAt i = max (mag (fromMaybe 0 (Array.index pk.hi i)))
                  (mag (fromMaybe 0 (Array.index pk.lo i)))
    amps = map ampAt (Array.range 0 (max 0 (n - 1)))
    sorted = Array.sort amps
    loudest = Int.toNumber (fromMaybe 1 (Array.last sorted))
    quietest = fromMaybe 0 (Array.head sorted)
    scale = if loudest <= 0.0 then 1.0 else loudest
    -- **Three times the take's OWN floor, never an absolute level.** The ES-9's
    -- inputs are DC-coupled and that offset alone reads as -32.6 dBFS, so
    -- anything measured against a constant is measuring the wiring.
    thr = 3 * max 1 quietest
    one r =
      let span = max 0.001 (r.end - r.start)
          bk = Wave.bucketsFor n total r.start r.end
          inside = Array.slice bk.from bk.to amps
          m = max 1 (Array.length inside)
      in { decay: maybe 0.0
             (\i -> span * Int.toNumber (i + 1) / Int.toNumber m)
             (Array.findLastIndex (\v -> v > thr) inside)
         , floor: Int.toNumber quietest / scale
         }
  in
    map one rs

-- | **Still sounding when it was cut**, so this sample carries the front of its
-- | successor and a layer stitched from it will click at the join.
-- |
-- | The comparison is the page's opinion; the set stores the numbers. A
-- | one-shot fired from a pad does not care.
overlapping :: Settled -> { start :: Number, end :: Number } -> Boolean
overlapping x r = x.decay >= 0.98 * max 0.001 (r.end - r.start)

-- | **How finely the daemon draws the take** — and therefore how finely the
-- | dry run can measure a decay, because both read the same envelope.
-- |
-- | The resolution is `take length / buckets`, so it gets WORSE as a transect
-- | gets bigger: 12 cells at 3000 ms is 40 ms a bucket at 900, but a 48-cell
-- | two-dimensional transect paced at 9 s to clear its longest decay is a 7
-- | minute take, and 900 buckets across that is 480 ms — useless for measuring
-- | anything. At the daemon's ceiling of 4000 the same take reads to 108 ms,
-- | which the 300 ms of room added to every measured spacing absorbs.
-- |
-- | Costs nothing to raise: peaks are asked for once and answered once, not
-- | carried in the 30 Hz snapshot.
buckets :: Int
buckets = 4000

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
                    --
                    -- **This parameter's own axis, not the whole grid.** It
                    -- read `Encoding.total`, which is the same number on a
                    -- one-dimensional run and the PRODUCT on a grid: picking a
                    -- calibration on a 12 x 4 asked for 48 degrees and handed
                    -- back C2-E3-G4-B5, a stack of major thirds wearing the
                    -- name of an octave. Same mistake as `fixPitch` had, in
                    -- the one other place that sets a range — and this one
                    -- writes the plan directly, so `fixPitch` never got the
                    -- chance to correct it afterwards.
                    cells = Sweep.sizeOfAxis st0.sweep
                              (maybe 0 _.axis (Array.index st0.sweep.params i))
                    -- **Start on a C.** Sampling an instrument wants an octave
                    -- you can name, and a bank that begins on G#1 is one every
                    -- later decision has to work around. The nearest C at or
                    -- above the table's lowest measured note — above, because
                    -- below it the realiser clamps and the bottom of the run
                    -- would come out on one pitch.
                    lo = tlo + mod (12 - mod tlo 12) 12
                    hi = min thi (lo + cells - 1)
                -- Through `fixPitch`, so the one rule about how long a pitch
                -- run is lives in one place. `hi` here is only what will fit
                -- inside the measured span; the invariant decides the rest.
                H.modify_ \st -> st
                  { sweep = Sweep.fixPitch (st.sweep
                      { params = fromMaybe st.sweep.params
                          (Array.modifyAt i
                            (_ { pitch = Just
                                  { label: t.label
                                  , noteLo: lo
                                  , noteHi: hi
                                  , table: t.points } })
                            st.sweep.params) }) }
                stp <- H.get
                liftEffect (Sweep.remember stp.sweep)
                -- **Report what was STORED**, not what was asked for. The
                -- invariant sets the top from the size of the axis, so a run
                -- longer than the calibration reaches now says so here rather
                -- than coming back with its top notes all on one pitch, which
                -- is what a silent clamp looks like from the module.
                let saidHi = fromMaybe hi
                      (Array.index stp.sweep.params i >>= _.pitch >>> map _.noteHi)
                H.modify_ (note (label <> ": " <> Pitch.noteName lo <> "–" <> Pitch.noteName saidHi
                                  <> " (measured " <> Pitch.noteName tlo <> "–" <> Pitch.noteName thi
                                  <> ", " <> show (Array.length t.points) <> " points)"
                                  <> (if saidHi > thi
                                        then " — the top of this run is past what the table \
                                             \measures, where the realiser clamps and every \
                                             \note above it comes back the same"
                                        else "")))
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
    -- **Ask for MIDI at load, and never wait for the answer.**
    --
    -- It used to be asked for when the Trigger modal opened, which is right for
    -- the sending half — a sweep cannot afford to discover the prompt mid-run.
    -- The listening half needs it earlier still: a hand-played take has to be
    -- heard while it is played, and by the time anyone opens a modal the chords
    -- are gone. `openMidi` is written never to block (the prompt lives in
    -- browser chrome, above the window, and awaiting it stops the page dead),
    -- so this costs nothing if the answer is slow or never comes.
    void $ H.liftAff (attempt (toAffE Rig.openMidi))
    -- The labels, once. An empty list is the honest answer both when nothing
    -- has been named and when the server is not answering: neither is a fault
    -- here, because every source falls back to its wire name.
    lab <- H.liftAff (attempt (toAffE Http.sourceLabels))
    H.modify_ _ { srcNames = either (const []) identity lab }
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
    -- | **Write the default down.** An unset source resolves to the first
    -- | available one, which is the right behaviour and a bad record: a set
    -- | saved that way says it was recorded from "", when the one question
    -- | worth asking afterwards is which input it came from. Adopted once,
    -- | when the rig first says what it has, and never over a choice.
    do
      stw <- H.get
      when (stw.sweep.source == "") $
        for_ (stw.looper >>= \top -> Array.find _.available top.sources) \src ->
          handleAction (SweepMsg (Sweep.SetSource src.name))
    -- The daemon draws; ask it to, the first time a capture comes into view.
    -- That covers a reload as well as a recording — the daemon did not forget
    -- what it is holding just because the page did.
    now <- H.get
    let had = maybe false _.holds (cap before)
        has = maybe false _.holds (cap now)
    when (has && not had) (send (CapturePeaks buckets))
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
    -- **Listening, whether or not anything is being recorded.**
    --
    -- `access.inputs` is live, so a controller switched on after the page
    -- loaded only appears on a later pass; and the notes have to be arriving
    -- before the take opens, because a take that has already been played
    -- cannot be asked what was played into it. Cheap — a set lookup per port.
    liftEffect Rig.listenMidi
    do
      hs <- liftEffect Rig.heardNotes
      ins <- liftEffect Rig.inPorts
      ok <- liftEffect Rig.midiAvailable
      st4 <- H.get
      when (ok /= st4.midiOk) $ H.modify_ _ { midiOk = ok }
      when (Array.length hs /= Array.length st4.heard) $ H.modify_ _ { heard = hs }
      when (ins /= st4.midiIn) $ H.modify_ _ { midiIn = ins }
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
  PickKindNamed v -> do
    st <- H.get
    for_ (Array.find (\k -> Kind.name k == v) (Kind.all))
      (handleAction <<< PickKind <<< case _ of
          Kind.Bars _ -> Kind.Bars st.bars
          other -> other)

  PickKind k -> do
    -- **The voice comes with the kind.** Stereo material takes a pair, so the
    -- four offers become two; a voice chosen while the kind was mono can be
    -- one the new kind cannot use, and a select showing nothing selected still
    -- holds the old number underneath.
    H.modify_ \s -> s { kind = k, divider = Divider.defaultFor k
                      , voice = onlyVoices k s.voice }
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
      Right v -> do
        H.modify_ _ { cardView = Just v }
        -- With one card mounted, read it too. A letter is chosen before a
        -- write and the occupancy was only ever fetched at the write, which
        -- is after the choice it should have informed.
        case v.cards of
          [ one ] -> do
            pk <- H.liftAff (attempt (toAffE (Http.previewCard one)))
            H.modify_ _ { cardPeek = either (const Nothing) Just pk }
          -- No card, or a choice of them: nothing here is "the" card, and a
          -- strip that quietly showed one of two would be worse than one that
          -- shows only the manifest's own letters.
          _ -> H.modify_ _ { cardPeek = Nothing }
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
          , letter: st.letter
          , kit: if st.kit == "" then nm else st.kit
          , voice: st.voice
          , append: st.placeAppend
          , sliced: st.placeSliced
          , layerMode: st.layerMode
          , layers: st.placeLayers })))
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
  -- | **A stored set, back on the bench.**
  -- |
  -- | Andrew, 2026-09-12: a set could be recorded, measured, saved, and then
  -- | never looked at again — the bands live on the page and the audio lives
  -- | in the daemon, so leaving the page or starting any other capture left
  -- | nothing to return to. `set.json` has always held everything needed; what
  -- | was missing was an envelope, and that can be read off the take.
  -- |
  -- | It is also the thing stacking will be built on: two sets open at once is
  -- | a small step from one, and impossible from none.
  OpenSet nm -> do
    H.modify_ _ { busy = true }
    r <- H.liftAff (attempt (toAffE (Http.loadSet nm)))
    case r of
      Left e -> H.modify_ (note ("could not open " <> nm <> ": " <> Aff.message e)
                             <<< _ { busy = false })
      Right v
        | not v.ok -> H.modify_ (note v.output <<< _ { busy = false })
        | otherwise -> do
            pk <- H.liftAff (attempt (toAffE (Http.takePeaks v.take buckets)))
            case pk of
              Left e -> H.modify_ (note ("could not draw " <> v.take <> ": "
                                     <> Aff.message e) <<< _ { busy = false })
              Right p
                | not p.ok -> H.modify_ (note
                    (p.output <> " — the samples are on disk, but the take they \
                     \were cut from is not, so there is nothing to draw them on")
                      <<< _ { busy = false })
                | otherwise -> do
                    -- The spec too, so a position opens with what it meant
                    -- rather than with a row of numbers from the last run.
                    sp <- H.liftAff (attempt (toAffE (Http.loadSpec nm)))
                    let n = Array.length v.regions
                    H.modify_ \st -> st
                      { regions = v.regions
                      , keep = if n <= 0 then Set.empty
                               else Set.fromFoldable (Array.range 0 (n - 1))
                      , schedule = v.schedule
                      , showing = v.take
                      , name = nm
                      , mine = true
                      , kept = true
                      , swept = not (Array.null v.schedule)
                      , busy = false
                      , page = Bench
                      , pivot = Nothing
                      , opened = Just { set: nm, take: v.take, secs: p.secs
                                     , notes: v.notes, struck: v.struck }
                      , peaks = Just
                          { loop: 0, frames: p.frames, from: 0, to: p.frames
                          , buckets: p.buckets, winIn: 0, winOut: 0, rot: 0
                          , lo: p.lo, hi: p.hi }
                      , sweep = case sp of
                          Right q | q.ok -> Sweep.adopt Sweep.emptyPlan q.spec
                          _ -> st.sweep
                      }
                    H.modify_ (note (nm <> " — " <> show n <> " samples over "
                                 <> fmt p.secs <> " s of " <> v.take))

  -- | **Three samples, a second each.**
  -- |
  -- | The names are timestamps, so `drum-hits-0912-121546` and
  -- | `drum-hits-0912-120901` differ by one glance — and one of the things you
  -- | can do to them from here cannot be undone. First, middle and last,
  -- | because the files are in cell order and on a grid that spans the outer
  -- | axis: on a decay sweep you hear short, middle and long, which is the set
  -- | describing itself in the time it takes to read its name.
  HearOne nm i -> liftEffect $
    Audio.playEach [ "/api/set-audio?set=" <> nm <> "&i=" <> show i ] 1.0

  PeekSet v -> do
    H.modify_ _ { openSet = v, openSetInfo = Nothing }
    case v of
      Nothing -> pure unit
      Just nm -> do
        r <- H.liftAff (attempt (toAffE (Http.loadSet nm)))
        st <- H.get
        -- Only if it is still the set being looked at: two clicks in a row
        -- must not leave the first one's detail under the second one's name.
        when (st.openSet == Just nm) case r of
          Left _ -> pure unit
          Right s -> H.modify_ _ { openSetInfo = Just s }

  HearSet nm n -> liftEffect $
    Audio.playEach
      (map (\i -> "/api/set-audio?set=" <> nm <> "&i=" <> show i)
         (Array.nub [ 0, n / 2, max 0 (n - 1) ]))
      1.0

  PickSet nm -> H.modify_ \s0 ->
    s0 { picked = if Set.member nm s0.picked then Set.delete nm s0.picked
                  else Set.insert nm s0.picked
       , confirmDrop = false }
  PickAllSets on -> H.modify_ \s0 ->
    s0 { picked = if on then Set.fromFoldable (map _.name s0.sets) else Set.empty
       , confirmDrop = false }
  AskDrop on -> H.modify_ _ { confirmDrop = on }

  -- | **Deleting is the one act here that destroys work you made**, so it is
  -- | asked before it is done and the button says how many. The takes are left
  -- | alone: a set is a cut of a take, the take may have others cut from it or
  -- | be worth cutting again, and deleting the derived thing should not reach
  -- | back to what it was derived from.
  DropPicked -> do
    st <- H.get
    let names = Array.fromFoldable st.picked
    if Array.null names then H.modify_ _ { confirmDrop = false } else do
      H.modify_ _ { cardBusy = true }
      r <- H.liftAff (attempt (toAffE (Http.deleteSets names)))
      case r of
        Left e -> H.modify_ (note (Aff.message e) <<< _ { cardBusy = false })
        Right w -> H.modify_ (note (lastLine w.output) <<< _ { cardBusy = false })
      H.modify_ _ { picked = Set.empty, confirmDrop = false }
      handleAction RefreshSets

  -- | Each ticked set as its OWN kit, which is what a set is: `PlaceSet` names
  -- | the kit after the set, so doing it four times is four kits in one bank
  -- | rather than four things fighting over one voice.
  PlacePicked -> do
    st <- H.get
    let names = Array.fromFoldable st.picked
    for_ names \nm -> handleAction (PlaceSet nm)
    H.modify_ _ { picked = Set.empty }

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
  SetLetter v -> H.modify_ _ { letter = v }
  SetKit v -> H.modify_ _ { kit = v, kitMine = v /= "" }
  SetVoice v -> H.modify_ \s ->
    s { voice = onlyVoices s.kind (clamp 1 4 (fromMaybe s.voice (Int.fromString v))) }
  -- | **Opening the question reads the card.** Which is the whole change: the
  -- | page used to ask "replace?" from what the manifest said and nothing
  -- | else, so it named every slot the manifest holds as though all of them
  -- | were at risk, and knew about the one that actually was only once the
  -- | write had refused over it.
  AskWrite v -> do
    H.modify_ _ { confirmWrite = v, preview = Nothing }
    case v of
      Nothing -> pure unit
      Just dest -> do
        r <- H.liftAff (attempt (toAffE (Http.previewCard dest)))
        -- Only if the question is still open, and still about this card: the
        -- read takes a moment and cancelling during it must stay cancelled.
        st <- H.get
        when (st.confirmWrite == Just dest) case r of
          Left e -> H.modify_ (note (Aff.message e) <<< _ { confirmWrite = Nothing })
          Right pv -> handleAction (Previewed pv)

  Previewed pv -> H.modify_ _ { preview = Just pv }

  SetPlaceSliced b -> H.modify_ _ { placeSliced = b }

  SetPlaceLayers n -> H.modify_ _ { placeLayers = n }

  SetPlaceAppend b -> H.modify_ _ { placeAppend = b }

  WriteCard dest replace -> do
    H.modify_ _ { cardBusy = true, confirmWrite = Nothing, preview = Nothing }
    r <- H.liftAff (attempt (toAffE (Http.writeToCard dest replace)))
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
        settles = case st.peaks of
          Nothing -> []
          Just pk -> settledFor pk (max 0.001 (heldSecs st))
                       (map (\r -> { start: r.start, end: r.end }) st.regions)
        kept = Array.catMaybes
          (Array.mapWithIndex
            (\i r ->
              if not (Set.member i st.keep) then Nothing
              else Just
                { cell: maybe [] _.cell (Array.index runSteps i)
                , start: r.start, end: r.end
                , peak: r.peak, rms: r.rms, zcr: r.zcr, tilt: r.tilt
                -- **Whether the sound had finished when the region did.** See
                -- `settledFor`: two numbers, no verdict, because the verdict
                -- belongs to whoever is going to use the sample.
                , decay: maybe 0.0 _.decay (Array.index settles i)
                , floor: maybe 0.0 _.floor (Array.index settles i)
                , means: maybe [] _.means (Array.index runSteps i)
                -- **What was played into it**, which for a hand-played chord
                -- set is the material itself. Empty on a swept run, where the
                -- pitch is in `means` because the run asked for it, and empty
                -- when nothing was listening — which the page says out loud
                -- before a take rather than after one.
                , notes: maybe [] (\t0 -> notesIn (believed st) t0 r) (takeZero st)
                , struck: maybe [] (\t0 -> strikesIn (believed st) t0 r) (takeZero st)
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
              , letter: st.letter
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
              -- **The destination decides whether it is one file.** The
              -- kind says what the material is; the encoding says how it is
              -- laid out where it is going, and "one voice holding one joined
              -- file" is the entire content of `Rample · slices`. Taken from
              -- the kind alone, choosing slices moved the axis, the label and
              -- the arithmetic and then wrote separate files regardless.
              , join: Encoding.joined st.sweep.encoding
                        || Kind.joins st.kind
                        || isEqual st.divider
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
  -- | **Empty means "no pitch axis", and that is a SWITCH, not a deletion.**
  -- |
  -- | It used to `ClearPitch`, which strips the spec and leaves the parameter
  -- | behind — still named `pitch`, still routed to ES-9 jack 1, and no longer
  -- | a pitch. Toggling the statement back to pitched then found no pitch
  -- | parameter, made a second one, named it `pitch` and routed it to jack 1
  -- | as well: *"ES-9 jack 1 is claimed by pitch and pitch"*. The guard
  -- | caught it, which is the guard working, and the pile-up should not have
  -- | been reachable in the first place.
  -- |
  -- | `off` is exactly this question and costs nothing — the table, the
  -- | routing and the range all survive, and the card says "none". The
  -- | statement and the card's third button are now the same act.
  AddPitch label
    | label == "" -> do
        st <- H.get
        case thePitchIx st of
          Just i -> handleAction (SweepMsg (Sweep.SetOff i true))
          Nothing -> pure unit
    | otherwise -> do
        st <- H.get
        case thePitchIx st of
          -- Retarget the pitch there already is, rather than growing a second
          -- one: two pitch axes on one transect is a thing to mean deliberately
          -- and never a thing to arrive at by using a dropdown.
          Just i -> do
            handleAction (SweepMsg (Sweep.SetOff i false))
            handleAction (PickPitch i label)
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
            -- | **On the axis you address, not the one the module picks.**
            -- |
            -- | A Rample chooses between LAYERS itself — by velocity, at
            -- | random or in turn — so a pitch run on layers is an instrument
            -- | whose note the module decides. A SLICE is a position you send
            -- | it to, so the same twelve samples on slices are an octave you
            -- | can play. `Axis.yours` is that distinction, per encoding;
            -- | where nothing is yours (or everything is) the first axis is
            -- | as good an answer as any.
            let axs = Encoding.axes st2.sweep.encoding
            case Array.findIndex _.yours axs of
              Just a -> handleAction (SweepMsg (Sweep.SetAxis i a))
              Nothing -> pure unit
            handleAction (PickPitch i label)

  -- | **Opening the pitch door expands its parameter as STATE**, not as an
  -- | override.
  -- |
  -- | The modal used to force `open = pitchIx` in the handlers it passed down,
  -- | which meant the desk was always showing and the close button beside it
  -- | could not win: it set `sweepEdit = Nothing` and the next render forced
  -- | the value straight back. A button that cannot change anything is worse
  -- | than an absent one.
  OpenModal m -> do
    st <- H.get
    let ix = thePitchIx st
    H.modify_ _
      { modal = m
      , sweepEdit = case m of
          Just PitchModal -> ix
          _ -> st.sweepEdit
      }

  -- The statement's coarse toggle, saying the same thing as the card's third
  -- button and by the same means. "Pitched" on a pitch that is merely switched
  -- off puts it back rather than fetching its table again.
  SetPitched v -> do
    st <- H.get
    case v, thePitchIx st of
      "unpitched", Just i -> handleAction (SweepMsg (Sweep.SetOff i true))
      "pitched", Just i -> handleAction (SweepMsg (Sweep.SetOff i false))
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

  -- | **A silent take does not get written to disk.**
  -- |
  -- | 2026-09-12: forty-eight files of digital silence, peak 0.00012 in every
  -- | one, from a transect whose ES-9 input stream had died on a power cycle.
  -- | The page said so — `spread` prints "nothing was recorded" — and the page
  -- | saying so is evidently not enough, because this was the second transect
  -- | lost the same way. A warning you can walk past is a warning that will be
  -- | walked past; the fix is to refuse, and to say what to do instead.
  AskKeep -> do
    st <- H.get
    let setName = if st.name == "" then "set" else st.name
    case wontKeepOf st of
      Just why -> H.modify_ (note ("not saved — " <> why))
      Nothing ->
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
  -- **Hover always plays.** It was behind a checkbox, and after the division
  -- moved into a modal the checkbox went with it — so the page's most useful
  -- gesture was off by default and switched on somewhere you had no reason to
  -- look. There is no cost to it being on: the pointer is over a slice only
  -- because you are asking about that slice.
  HoverPlay i -> handleAction (Play i)
  -- | **The transect end to end**, which is the one listen the tiles cannot
  -- | give you: twelve samples heard in order, with the gaps, is how you hear
  -- | a sweep as a sweep rather than as twelve sounds.
  PlayWhole -> do
    st <- H.get
    when (st.showing /= "") $ liftEffect
      (Audio.playRange ("/api/take-audio?take=" <> st.showing) 0.0
        (let n = heldSecs st in if n > 0.0 then n else 1.0e6))
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
    s { keep = if on && Array.length s.regions > 0
                 then Set.fromFoldable (Array.range 0 (Array.length s.regions - 1))
                 else Set.empty }
  ArmOn src -> captureOn true src
  -- | **The sweep modal**, and asking the browser for MIDI when it opens.
  -- |
  -- | Asked once, on opening, rather than at Run: `requestMIDIAccess` prompts
  -- | the first time, and a permission dialog appearing in the middle of a run
  -- | would cost the take.
  GoTo pg -> H.modify_ _ { page = pg }
  -- | **The input is part of the plan now**, so it survives a reload like
  -- | every other choice. It was page state and reset to the first available
  -- | source every time the page loaded — which is how a transect went to
  -- | `board`. Stored by NAME: the rig's audio is an aggregate whose member
  -- | order is not stable, so a remembered index can come back pointing at a
  -- | different jack, where a remembered name either resolves or visibly does
  -- | not.
  PickSource v -> handleAction (SweepMsg (Sweep.SetSource v))
  -- | **Name an input after the thing on the end of the cable.** Written
  -- | through immediately rather than on a Save button: it is one short string
  -- | in a file of them, and a labelling pass that can be half-done is a
  -- | labelling pass someone abandons. An empty label clears it and the source
  -- | goes back to showing its wire name.
  NameSource wire v -> do
    H.modify_ \s0 ->
      s0 { srcNames =
             if v == "" then Array.filter (\r -> r.wire /= wire) s0.srcNames
             else case Array.findIndex (\r -> r.wire == wire) s0.srcNames of
               Just i -> fromMaybe s0.srcNames
                           (Array.modifyAt i (_ { label = v }) s0.srcNames)
               Nothing -> Array.snoc s0.srcNames { wire, label: v } }
    void $ H.liftAff (attempt (toAffE (Http.nameSource wire v)))
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
  -- | **A run made in order to be measured.**
  -- |
  -- | The same sweep, at the flat spacing, which then divides itself and keeps
  -- | only how long each cell took to go quiet. Everything else about it is
  -- | thrown away — it exists so the REAL run can be paced cell by cell, which
  -- | is the only way to size a transect whose own Decay or velocity is one of
  -- | the swept parameters.
  UsePaced on -> H.modify_ \x -> x { sweep = x.sweep { usePaced = on } }
  DryRun src -> do
    H.modify_ \x -> x { dry = true, sweep = x.sweep { usePaced = false } }
    handleAction (RunSweep src)
  RunSweep src -> do
    st <- H.get
    case st.sweepFork of
      Just _ -> H.modify_ (note "a sweep is already running")
      Nothing -> do
        -- **A transect that cannot mean what it says is not recorded.**
        --
        -- Refused rather than warned, because the failure is inaudible at the
        -- time and expensive later: a swept CV sharing the trigger's jack
        -- fires the module itself once its ramp crosses threshold, so the
        -- samples come back cut across their own attacks and every number
        -- measured from them is wrong without looking wrong. Measured
        -- 2026-09-11; see `Sweep.conflicts`.
        let clashes = Sweep.conflicts st.sweep
        if not (Array.null clashes)
          then H.modify_ (note ("nothing recorded — "
                 <> joinWith "; " (map Sweep.sayConflict clashes)))
          else do
            -- No head trim: see `captureOn`. The schedule declares when the
            -- first hit happens, so nothing needs to find it.
            captureOn false src
            -- `dry` is set by `DryRun` before it delegates here, so this is
            -- the one place that knows which kind of run is starting.
            H.modify_ _ { swept = false, sweepAt = Nothing, schedule = []
                        , takeIsDry = st.dry }
            fid <- H.fork runSweep
            H.modify_ _ { sweepFork = Just fid }
  StopSweep -> do
    st <- H.get
    for_ st.sweepFork H.kill
    restCv
    H.modify_ (note "sweep stopped" <<< _ { sweepFork = Nothing, sweepAt = Nothing })
    handleAction Close
  -- **Closing the take stops the rig too.** They are one act: a schedule
  -- still being played into a take that has ended is sound nobody catches,
  -- and the next take catches it instead.
  Close -> do
    st <- H.get
    for_ st.sweepFork H.kill
    H.modify_ _ { sweepFork = Nothing, sweepAt = Nothing }
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
  -- **Nothing may still be playing the rig when a new take opens.**
  --
  -- Measured 2026-09-11 on a take that came back with SEVENTEEN onsets for
  -- twelve cells: two independent 3.0 s trigger series interleaved 1.58 s
  -- apart, the older one contributing five more hits before it ran out. A
  -- sweep is a forked fiber, and only `StopSweep` ever killed it — every other
  -- way a take can end (Close, the daemon closing at its count, starting the
  -- next one) left it running and firing into whatever came next.
  --
  -- Killed here because this is the one place every run starts, so a stale
  -- fiber cannot survive into a take by any route.
  for_ st.sweepFork H.kill
  H.modify_ _ { sweepFork = Nothing, sweepAt = Nothing }
  -- **A new take is scratch until it is kept.** Cleared here rather than in
  -- either caller, because this is the one place both ways of filling a take
  -- go through, and a stale "kept" badge on a fresh recording is exactly the
  -- confusion the flag exists to remove.
  H.modify_ _ { kept = false, confirmKeep = false }
  -- **A soundcheck is not part of the take.** Notes struck before the capture
  -- opened would be aligned against a take they are not in, and the alignment
  -- is anchored on the FIRST note heard — so one stray note before recording
  -- would shift every chord in the set onto the wrong sample.
  liftEffect Rig.forgetHeard
  H.modify_ _ { heard = [] }
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
  -- | **Wait for the capture to be RUNNING, not for 400 ms.**
  -- |
  -- | `captureOn` asks; the daemon opens the stream and says so in a snapshot,
  -- | and on 2026-09-12 that took about four seconds. The grid was anchored
  -- | after a fixed 400 ms wait, so it was already four seconds behind before
  -- | the first gate — and an absolute grid catches up by firing every slot it
  -- | has missed, at once. Cells 1 and 2 came back ten milliseconds long, both
  -- | runs, and the samples for C2 and C#2 were simply not there.
  -- |
  -- | The 400 ms stays afterwards, because that is the real silence at the
  -- | head of the take that `by attack` wants in front of the first onset.
  let awaitOpen n = do
        stw <- H.get
        unless (n <= 0 || maybe false _.on (cap stw)) do
          H.liftAff (delay (Milliseconds 100.0))
          awaitOpen (n - 1)
  awaitOpen 100
  H.liftAff (delay (Milliseconds 400.0))
  -- The grid every step is timed against, fixed before the first one — and
  -- allowed to SLIP, never to be caught up. See `slipped`.
  anchor <- liftEffect (Ref.new 0.0)
  liftEffect (Rig.nowMs >>= flip Ref.write anchor)
  for_ (Sweep.steps p) \s -> do
    H.modify_ _ { sweepAt = Just s.index }
    -- | **A slot already gone is not a slot to catch up on.**
    -- |
    -- | Absolute-grid pacing is right about drift: a step that stalls must not
    -- | move the ones after it, and subtracting "what this step took" from the
    -- | next delay walked the phase 130 ms a step for a whole morning. It is
    -- | wrong about a stall LONGER than the spacing, where the slot is gone
    -- | and firing into it immediately destroys that cell and every one the
    -- | stall covered.
    -- |
    -- | So: absorb what can be absorbed — a late send just shortens `ahead` —
    -- | and when the slot has passed outright, move the whole grid rather than
    -- | the run. Timing slips by the length of the stall, once; nothing
    -- | accumulates, because the anchor only ever moves to now.
    tNow <- liftEffect Rig.nowMs
    t0was <- liftEffect (Ref.read anchor)
    let slot = t0was + Int.toNumber (Sweep.startsAt p s.index)
    when (slot < tNow) $ liftEffect (Ref.write (t0was + (tNow - slot)) anchor)
    t0 <- liftEffect (Ref.read anchor)
    -- **Where this gate belongs, stated before anything is sent.** Everything
    -- that follows is then free to be late without moving the sound.
    -- **A sum, not a multiple.** With measured pacing every cell has its own
    -- spacing — the only way to size a transect whose own Decay or velocity is
    -- one of the swept parameters. See `Sweep.spacingAt`.
    let fireAt = t0 + Int.toNumber (Sweep.startsAt p s.index + p.settleMs)
    tIn <- liftEffect Rig.nowMs
    unless (Array.null s.cv && Array.null s.esx) $ void $
      H.liftAff (attempt (toAffE (Rig.setCv { set: s.cv, esx: s.esx })))
    when (p.port /= "") $ liftEffect $ for_ s.cc \c ->
      Rig.sendCc { port: p.port, channel: c.channel, cc: c.cc, value: c.value }
    tCv <- liftEffect Rig.nowMs
    -- **The settle is a gap on the grid now, not a delay we sit through.** The
    -- module still gets `settleMs` between its CV and its gate; the difference
    -- is that the gap is guaranteed by WHEN THE GATE IS ASKED FOR rather than
    -- by how long this loop happened to wait.
    -- **Where in the take this hit is about to be.** `Schedule.at` reads the
    -- capture's position now and the gate is still in the future, so the mark
    -- is one plus the other — arithmetic, not a second round trip. Both halves
    -- are read at the same instant, because a mark is only as good as the gap
    -- between the two clocks it joins. See `Quadrat.Schedule`.
    mk <- liftEffect Schedule.at
    tMk <- liftEffect Rig.nowMs
    for_ mk \t -> H.modify_ \s0 ->
      s0 { schedule = Array.snoc s0.schedule (t + max 0.0 (fireAt - tMk) / 1000.0) }
    -- **The gate goes on the daemon's clock, and the page is then out of it.**
    --
    -- `Rig.pulse` fired when the message landed, so every millisecond this page
    -- was late went into the audio and STAYED there. Measured 2026-09-11:
    -- twelve gates paced from here carried a 120 ms step at the sixth, in the
    -- same place at 3000 ms and at 5000 ms; the identical sequence paced from
    -- node through these same endpoints carried none, twice. The page was the
    -- only difference, and the cause is still unnamed — which is the other
    -- reason to stop asking it to keep time.
    --
    -- `pulseAt` says WHEN. The daemon applies it in its audio callback against
    -- `current_frame()`, the counter the capture is written from, so the gate
    -- and the recording share one clock instead of two to be reconciled. Waking
    -- late now shortens `ahead` rather than moving the sound.
    -- **Read the clock in the same breath as the send.** `delayMs` counts from
    -- the moment the daemon RECEIVES this, so anything between measuring it and
    -- sending it is error that the scheduling was supposed to remove.
    tSend <- liftEffect Rig.nowMs
    let ahead = max 0.0 (fireAt - tSend)
    for_ p.trigger.gate \b -> void $
      H.liftAff (attempt (toAffE (Rig.pulseAt
        { bus: b, level: p.trigger.gateLevel, ms: p.trigger.ms, delayMs: ahead })))
    -- The ES-5 and MIDI paths have no scheduled form, so they are still fired
    -- by hand and still have to be waited for. A gate on a bus is already away
    -- and does not care how well this lands.
    tPre <- liftEffect Rig.nowMs
    H.liftAff (delay (Milliseconds (max 0.0 (fireAt - tPre))))
    for_ p.trigger.es5 \b -> void $
      H.liftAff (attempt (toAffE (Rig.es5pulse { bit: b, ms: p.trigger.ms })))
    when (p.port /= "") $ for_ p.trigger.note \n -> liftEffect $
      Rig.sendNote { port: p.port, channel: p.trigger.channel, note: n
                   , velocity: p.trigger.velocity, ms: p.trigger.ms }
    tFire <- liftEffect Rig.nowMs
    liftEffect $ Rig.mark
      { i: s.index + 1, inAt: tIn - t0, cv: tCv - tIn
      , want: fireAt - t0, ahead, at: tFire - t0 }
    -- **Wait until the next slot, not for an interval.**
    --
    -- Pacing by "spacing minus what this step took" measures the WHOLE step,
    -- including the sends that happen after the trigger has already fired. A
    -- stall in that tail shortens the next delay and advances the phase for
    -- good: a run asking for 3000 ms stepped every 3130 ms for a whole morning.
    --
    -- An absolute target cannot accumulate or persist an error. A late step
    -- shortens its own delay and the one after is back on the grid.
    now <- liftEffect Rig.nowMs
    let due = t0 + Int.toNumber (Sweep.startsAt p (s.index + 1))
    H.liftAff (delay (Milliseconds (max 0.0 (due - now))))
  -- The last hit gets the same gap as the others and then a little more, so
  -- that closing the take is never the thing that ends its decay. A final
  -- sample that is short because the recording stopped is indistinguishable
  -- from one that is short because the sound was.
  H.liftAff (delay (Milliseconds 300.0))
  liftEffect Rig.dumpMarks
  restCv
  H.modify_ (note ("swept " <> show (Encoding.total p.extent) <> " samples")
    <<< _ { sweepAt = Nothing, sweepFork = Nothing, sweepOpen = false, swept = true })
  -- **Keep the schedule, because it cannot be recomputed.** It is measured —
  -- the trigger times as they actually landed — and a reload that loses it
  -- downgrades a transect to a guess without saying so. See `Sweep.saveRun`.
  st1 <- H.get
  liftEffect (Sweep.saveRun { take: st1.name, schedule: st1.schedule })
  handleAction Close
  -- **A dry run measures itself.** Divide it, read how long each cell took to
  -- go quiet, and hand those times to the plan. Nothing else about the take is
  -- wanted, but it IS written to disk on the way through, because the division
  -- is done by `msm` over a file.
  when st1.dry do
    -- **Wait until the daemon says the capture is drawable.**
    --
    -- `holds` is FALSE while a capture is running and true once it has closed
    -- — measured 2026-09-11, mid-capture: `on: true, frames: 76288, holds:
    -- false`. The page's snapshot is up to a thirtieth of a second behind that,
    -- so dividing straight after `Close` asks `analyse` to work on a capture
    -- the page still believes is empty, and it correctly refuses. The dry run
    -- then measured nothing, silently, and left the pacing switch disabled.
    let settle n = do
          stw <- H.get
          unless (n <= 0 || maybe false _.holds (cap stw)) do
            H.liftAff (delay (Milliseconds 100.0))
            settle (n - 1)
    settle 40
    handleAction Analyse
    st2 <- H.get
    case st2.peaks of
      Nothing -> H.modify_ (note "dry run: no waveform came back to measure"
                              <<< _ { dry = false })
      Just pk -> do
        let total = max 0.001 (heldSecs st2)
            bounds = map (\r -> { start: r.start, end: r.end }) st2.regions
            ds = settledFor pk total bounds
            -- **Silence after the decay, not just up to it.** A cell that ends
            -- exactly as it goes quiet leaves no join to see, and `guardMs`
            -- wants somewhere to close.
            room = 300
            paced = map (\d -> clamp 200 30000
                          (Int.round (d.decay * 1000.0) + room)) ds
            -- **A cell still sounding at its own end was cut**, so its decay is
            -- a LOWER BOUND and the pacing derived from it is too short. Said
            -- out loud, because a dry run that quietly under-measures is worse
            -- than no dry run: it would hand back confident numbers that are
            -- wrong in the direction that ruins the samples.
            cut = Array.length
                    (Array.filter identity (Array.zipWith overlapping ds bounds))
        if Array.null paced
          then H.modify_ (note "dry run: the take did not divide, so no timings \
                                \were measured — divide it by hand and look at \
                                \what came back"
                            <<< _ { dry = false })
          else do
            let said = "dry run: paced " <> show (Array.length paced) <> " cells, "
                  <> fmt (Int.toNumber (Array.foldl (+) 0 paced) / 1000.0)
                  <> " s in total"
                  <> (if cut > 0
                        then " — but " <> show cut <> " were still sounding when \
                             \cut, so those are lower bounds: give the sweep \
                             \more space and dry-run it again"
                        else "")
            H.modify_ \x -> note said x
              { dry = false
              , sweep = x.sweep { paced = paced
                                , pacedFor = Sweep.fingerprint x.sweep
                                , usePaced = true }
              }
            -- | **Write it down.** Andrew, 2026-09-12: *"measures didn't
            -- | appear to have survived reload"*. They did not: the plan is
            -- | persisted by `SweepMsg` and only by `SweepMsg`, so a
            -- | measurement made here lived in memory until some unrelated
            -- | edit happened to save the plan around it. A measurement is
            -- | the most expensive thing on the page to reproduce — it costs
            -- | a whole run — so it is the last thing that should depend on
            -- | an accident for its survival.
            stp <- H.get
            liftEffect (Sweep.remember stp.sweep)

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
    -- The same resolution `srcNow` does, and for the same reason: by name,
    -- falling back to the first available one. A meter reading a different
    -- source from the one the run will record is worse than no meter.
    ix = fromMaybe 1
      (s.looper >>= \t ->
        case Array.findIndex (\x -> x.name == s.sweep.source) t.sources of
          Just i -> Just (i + 1)
          Nothing -> map (_ + 1) (Array.findIndex _.available t.sources))
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
        send (CapturePeaks buckets)
        send (WriteCapture st.name)
        -- The daemon writes on its own thread and the ack lands in a snapshot;
        -- the folder is there a moment later.
        H.liftAff (delay (Milliseconds 900.0))
      let takeName = if write then st.name else st.showing
          -- **The run divides its own take.** Empty for anything played by
          -- hand, which is when the detector is the only thing that could
          -- know. See `Quadrat.Schedule` for the lead, which is the one
          -- number the schedule cannot supply itself.
          -- The last region has no next trigger to end it, so the plan says
          -- how long it runs: its own spacing, which with measured pacing is
          -- the longest in the run exactly when the middle gap is least like
          -- it. Zero for a take played by hand, where `slots` falls back.
          lastGap =
            if Array.null st.schedule then 0.0
            else Int.toNumber
                   (Sweep.spacingAt st.sweep (Array.length st.schedule - 1)) / 1000.0
          declared = Schedule.slots
                       (Int.toNumber st.sweep.leadMs / 1000.0)
                       (Int.toNumber st.sweep.guardMs / 1000.0)
                       lastGap
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
              -- `Array.range 0 (-1)` is `[0, -1]`, not empty — so a take
              -- that divided into nothing used to come back with two ghost
              -- keeps, and the page would offer to save no samples under the
              -- name of two. Same trap as `Sweep.startsAt`.
              let n = Array.length d.regions
              H.modify_ _
                { regions = d.regions
                , keep = if n <= 0 then Set.empty
                         else Set.fromFoldable (Array.range 0 (n - 1))
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
      && (r.slicer > 0) == (Encoding.joined st.sweep.encoding
                              || Kind.joins st.kind
                              || isEqual st.divider)
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

-- | **The notes this take is actually listening to.**
-- |
-- | One port, named in the plan, and nothing from any other. The first version
-- | collected every input on the machine, which on this rig means the IAC
-- | buses the sequencers talk over: a four-second chord came back holding 204
-- | notes, none of which anybody played.
-- |
-- | Empty when no port has been chosen, which is deliberate — a set that
-- | records the wrong performance is worse than one that records none, because
-- | none is visible and the page says so before you play.
believed :: State -> Array Rig.Struck
believed st
  | st.sweep.notesFrom == "" = []
  | otherwise =
      Array.filter
        (\h -> h.from == st.sweep.notesFrom
                 && (st.sweep.notesChan == 0 || h.chan == st.sweep.notesChan))
        st.heard

-- | **The notes struck inside each region**, in register and in order.
-- |
-- | ## Aligning two clocks without either one telling you where it started
-- |
-- | The notes are stamped in the page's clock; the regions are seconds into a
-- | take file whose head has been **trimmed to the first sound**, so its zero
-- | is not the moment the capture opened and the page cannot know how much was
-- | cut. Estimating the capture's start from `secs` and the poll would work to
-- | about a tenth of a second and would still be an estimate of the wrong
-- | thing.
-- |
-- | So it anchors on the material instead: **the first note struck is the first
-- | sound, and the first sound is where the file begins.** One subtraction, no
-- | estimate, immune to the trim and to poll jitter — and the two clocks run at
-- | the same rate afterwards, so every later region lands on its own notes.
-- |
-- | That is why `forgetHeard` runs when a capture opens. A soundcheck note
-- | before the take would become the anchor and shift the whole set by however
-- | long you waited.
-- |
-- | ## The window
-- |
-- | A region cut by `Gaps` begins in the silence *before* its attack, so a note
-- | normally lands just inside. The tolerance is for the other order: MIDI
-- | reaches the page immediately while its audio is still crossing the
-- | interface, so a note can be stamped a few milliseconds before the sound it
-- | caused. 120 ms is far larger than any interface latency and far smaller
-- | than the silences these takes are divided on.
notesIn
  :: forall r s
   . Array { note :: Int, at :: Number | s }
  -> Number
  -> { start :: Number, end :: Number | r }
  -> Array Int
notesIn heard t0 r = Array.nub (Array.sort (join (strikesIn heard t0 r)))

-- | **The chords struck inside a region, in order, as separate chords.**
-- |
-- | A region usually holds one voicing. It does not always: *"I deliberately
-- | played two chords on the last two samples as an experiment... they sounded
-- | really nice."* Flattened, that pair is an eleven-note voicing, which is a
-- | different musical object and not the one that was played — and telling the
-- | two apart is exactly what a later tool wants, since bridging between two
-- | chords needs to know there were two.
-- |
-- | **A strike is a cluster in time.** Notes of one chord arrive within a few
-- | milliseconds of each other even when a keyboard is played by hand; 50 ms
-- | is wider than any of that and far narrower than a deliberate second chord.
-- |
-- | **A repeat of the group before it is dropped.** Progressions resends a
-- | chord held past about a bar, so most chords arrive twice; two strikes of
-- | the same pitches are one chord that went on ringing, and the sample
-- | already records how long for. Two strikes of DIFFERENT pitches are two
-- | chords and both are kept.
strikesIn
  :: forall r s
   . Array { note :: Int, at :: Number | s }
  -> Number
  -> { start :: Number, end :: Number | r }
  -> Array (Array Int)
strikesIn heard t0 r =
  let
    mine =
      Array.sortWith _.at
        (Array.filter
          (\h -> let t = (h.at - t0) / 1000.0
                 in t >= r.start - 0.120 && t < r.end)
          heard)
    -- Walked rather than grouped by a key, because the boundary is the GAP
    -- between one note and the last, not any property of the notes.
    clump acc h = case Array.last acc of
      Just grp | Just prev <- Array.last grp, h.at - prev.at <= 50.0 ->
        fromMaybe acc
          (Array.modifyAt (Array.length acc - 1) (\g -> Array.snoc g h) acc)
      _ -> Array.snoc acc [ h ]
    grouped = map (Array.nub <<< Array.sort <<< map _.note)
                (Array.foldl clump [] mine)
  in
    Array.filter (not <<< Array.null)
      (Array.mapWithIndex
        (\i g -> if Array.index grouped (i - 1) == Just g then [] else g)
        grouped)

-- | The page-clock instant that the take's own zero corresponds to, or nothing
-- | when there is not enough to anchor on. See `notesIn`.
takeZero :: State -> Maybe Number
takeZero st = do
  first <- Array.head (believed st)
  r0 <- Array.head st.regions
  pure (first.at - r0.start * 1000.0)

-- | **The voices this material can actually start on.**
-- |
-- | A stereo sample plays left on SPn and right on SP(n+1), so it consumes the
-- | voice after it: a kit is four mono voices, or two stereo ones, or a mix.
-- | Which leaves a stereo take exactly two places to begin. 4 is not one of
-- | them — there is no voice 5 — and 2 is not either, because it would leave
-- | voice 1 empty and a kit with no playable voice 1 is one **the module
-- | refuses to open at all**.
-- |
-- | Shared by the dropdown and by `freeVoice` so the offer and the suggestion
-- | cannot disagree.
voicesFor :: Kind -> Array Int
voicesFor = voicesWide <<< (_ /= ToMono) <<< Kind.foldsTo

-- | The same question asked of a stored set, which knows whether it is stereo
-- | without knowing what kind of material made it.
voicesWide :: Boolean -> Array Int
voicesWide wide = if wide then [ 1, 3 ] else [ 1, 2, 3, 4 ]

-- | **A pitch as a colour**, and `Nothing` for a sample that stands for no
-- | note.
-- |
-- | The twelve semitones around the wheel, which is the one mapping nobody
-- | has to learn: adjacent notes are adjacent hues, so a chromatic run reads
-- | as a sweep of colour and a repeat reads as a repeat. That is the whole
-- | point of colouring them — a 4 × 12 arranged as four layers of twelve
-- | shows four identical runs, and the same set arranged as one file of
-- | forty-eight shows the run four times over, where a start point cannot
-- | reach a pitch without also choosing which decay it comes with.
-- |
-- | Octave moves the lightness, not the hue, so an octave apart still reads
-- | as the same note — which is what it is.
pitchTint :: Int -> Maybe String
pitchTint n
  | n < 0 = Nothing
  | otherwise =
      let
        pc = n `mod` 12
        -- MIDI 60 is middle C. Two octaves either side is the span anything
        -- here plays in; beyond it the lightness stops moving rather than
        -- running to white or black.
        oct = clamp (-2) 2 ((n - 60) / 12)
        light = 58 - oct * 7
      in
        Just ("background: hsl(" <> show (pc * 30) <> "deg 48% " <> show light <> "%)")

-- | **A duration as a width**, log-scaled and clamped at both ends.
-- |
-- | The floor is the module\'s own minimum sample — 50 ms, below which `msm`
-- | refuses — and the ceiling is thirty seconds, past which nothing here is a
-- | sample. So the scale is absolute rather than per-set, and a drum hit is
-- | visibly shorter than a chord WHEREVER the two are seen together, which a
-- | per-set normalisation would hide.
-- |
-- | Log rather than linear because the range is two and a half decades: on a
-- | linear scale a 0.4 s hit and a 0.9 s hit differ by three pixels while a
-- | 20 s take runs off the page. Logarithmically the four decays of a decay
-- | sweep — 0.40, 0.92, 1.03, 1.85 — come out as four distinct widths, which
-- | is the axis the picture was missing.
-- | **An unknown length is not a short one.** Zero gets a narrow cell that
-- | is marked as unknown rather than a plausible default: a default width
-- | reads as a real measurement, and it produced a wrong conclusion on the
-- | first day it existed — two 166- and 124-second takes drawn at the width
-- | of a half-second hit, and read as "these are not long".
widthOf :: Number -> String
widthOf d
  | d <= 0.0 = "width: 9px"
  | otherwise = "width: " <> show (Int.round (5.0 + 26.0 * atOf d)) <> "px"

-- | Where a duration sits on the scale, 0 to 1. Separate from the width so
-- | that "off the top of the scale" can be drawn as such: a 166-second take
-- | and a 30-second one are both pinned at full width, and the difference
-- | between them is a fact the picture must not swallow.
atOf :: Number -> Number
atOf d = clampN 0.0 1.0 (Number.log (max 0.05 d / 0.05) / Number.log (30.0 / 0.05))

overScale :: Number -> Boolean
overScale d = d > 30.0

-- | Two decimals, for a duration in a tooltip.
secs2 :: Number -> String
secs2 d = show (Int.round (d * 100.0) / 100)

-- | A MIDI note as a person would say it. Sharps rather than flats, because
-- | the only thing naming them here is a tooltip and a consistent spelling
-- | beats a correct enharmonic nobody asked for.
noteName :: Int -> String
noteName n = fromMaybe "?" (Array.index names (n `mod` 12)) <> show ((n / 12) - 1)
  where
  names = [ "C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B" ]

-- | The nearest voice this kind can use, at or below the one asked for.
onlyVoices :: Kind -> Int -> Int
onlyVoices k v =
  if Array.elem v (voicesFor k) then v
  else fromMaybe 1 (Array.last (Array.filter (_ <= v) (voicesFor k)))

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
  in Array.find fits (voicesFor st.kind)

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

-- | **Which parameter is the pitch**, asked in one place.
-- |
-- | A parameter carrying a spec, or failing that one that was meant to be a
-- | pitch and lost its spec to an older version of the unpitched toggle. The
-- | second clause is repair: it adopts the stray rather than adding a second
-- | parameter beside it on the same jack, which is how a plan ends up claiming
-- | ES-9 jack 1 twice.
thePitchIx :: State -> Maybe Int
thePitchIx st =
  case Array.findIndex (\q -> Maybe.isJust q.pitch) st.sweep.params of
    Just i -> Just i
    Nothing -> Array.findIndex (\q -> q.name == "pitch") st.sweep.params

-- | **Why this take cannot be kept**, or nothing. One statement of the rule,
-- | read by the panel that greys the button and by the action that refuses —
-- | two copies of a precondition is two chances for the button to be live over
-- | a refusal.
wontKeepOf :: State -> Maybe String
wontKeepOf st =
  let loud = fromMaybe 0.0 (Array.last (Array.sort (map _.peak st.regions)))
  in
    if st.takeIsDry
      then Just "this take is the measuring pass, which runs at the flat \
                 \spacing by construction — so every sample carries the \
                 \silence the measurement exists to remove. Press Record for \
                 \the real one."
    else if not (Array.null st.regions) && loud < 0.003
      then Just "the loudest sample in this take peaks at silence. Check the \
                 \input is the one the module is patched to, and that a gate \
                 \makes it move."
    else Nothing

-- | **The name the owner gave this input, or the wire name if they gave none.**
-- |
-- | One function, because the page names the source in five places and four of
-- | them being right is how the fifth becomes the one you read. `wire` is
-- | always the value that goes into the spec and into a `<select>`; the label
-- | is only ever what is DRAWN.
labelFor :: State -> String -> String
labelFor st wire =
  case Array.find (\r -> r.wire == wire) st.srcNames of
    Just r | r.label /= "" -> r.label
    _ -> wire

-- | **What the rig is recording at, when that is not what the card wants.**
-- |
-- | A Rample card is 44.1 kHz, and this rig's aggregate came back from a
-- | reboot at 48 — which is a setting on the interface, not a thing this page
-- | can change. `msm` resamples correctly (measured 2026-09-12: twelve notes
-- | within four cents of what was asked), so the conversion is not wrong; it
-- | is simply a non-integer conversion done for no reason, and the reason it
-- | happens is that nobody was told.
-- |
-- | So the page says it, and says it where the decision is: beside the export,
-- | before a run rather than after one. Setting the aggregate back is a human
-- | act in Audio MIDI Setup and this deliberately does not pretend otherwise —
-- | it is a remark, not a guard, and nothing is disabled by it.
cardRate :: Int
cardRate = 44100

rateSays :: State -> Maybe String
rateSays st = do
  top <- st.looper
  if top.sampleRate <= 0 || top.sampleRate == cardRate then Nothing
    else pure (khz top.sampleRate <> " in, " <> khz cardRate <> " out — the \
               \card wants " <> khz cardRate <> " and msm will resample. Set \
               \the aggregate in Audio MIDI Setup to record it straight.")
  where
  khz n = fmt (Int.toNumber n / 1000.0) <> " kHz"

-- | **How long the take on the bench is.**
-- |
-- | The daemon's capture, unless a stored set is open — in which case it is
-- | that set's take, whose length came off the file. One function, because the
-- | alternative is six sites that each decide for themselves and five of them
-- | being right.
heldSecs :: State -> Number
heldSecs st = case st.opened of
  Just o -> o.secs
  Nothing -> maybe 0.0 _.secs (cap st)

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
        -- | **Acts, then the instrument, then what came back.**
        -- |
        -- | The parameters sat below the take, on the argument that they are
        -- | the things you adjust and then stop looking at. The keyboard
        -- | ended that: once a card can show you the notes it will play and
        -- | the schedule it will fire on, the cards are what you READ before
        -- | pressing anything, and they were a scroll below the button. So
        -- | they move up against the acts — Andrew, 2026-09-12 — and the
        -- | grid keeps the bottom of the page, where it is the answer rather
        -- | than the question.
        Bench ->
          HH.div_
            [ HH.section [ HP.class_ (HH.ClassName "q-hero") ]
                [ HH.div [ HP.class_ (HH.ClassName "q-actbar") ] [ goRow, transport, doors ] ]
            , HH.section [ HP.class_ (HH.ClassName "q-curverow is-first") ]
                [ SweepView.curves sweepHandlers ]
            , HH.section [ HP.class_ (HH.ClassName "q-hero is-take") ]
                [ if not (Array.null st.regions) || st.busy || hasTake
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
            -- **One hit, every parameter — as a modal.**
            --
            -- It opened at the BOTTOM of the page, below the curves, when the
            -- thing that summoned it was a band in the waveform at the top. So
            -- clicking a slice appeared to do nothing until you scrolled.
            , case st.pivot of
                Just j | st.fill == Swept ->
                  modalPivot ("Position " <> show (j + 1)) (pivotPanel j)
                _ -> HH.text ""
            , case st.modal of
                Just DivisionModal -> modalBox "Division details" divisionPanel
                -- **A take nobody triggers has no trigger settings.**
                --
                -- The gate, the note, the settle, flat-against-measured — all
                -- of it describes a schedule, and a take played by hand has
                -- none: there is no spacing to choose because nothing is being
                -- spaced, and the division comes from the audio afterwards.
                -- Shown anyway they read as settings that must be got right
                -- before playing, and the pacing pair in particular looks
                -- exactly like the missing "manual" a player goes looking for.
                -- The manual option is `triggered by → my own hands` in the
                -- sentence, which is what opened this door.
                Just TriggerModal -> case st.fill of
                  Played -> modalBox "Playing it yourself" handPanel
                  Swept -> modalBox "Trigger"
                    (HH.div_
                      [ SweepView.settings sweepHandlers
                      , SweepView.triggerView sweepHandlers
                      , pacingPanel
                      ])
                -- Opened with the pitch parameter ALREADY expanded: the reason
                -- to come in here is the values, and a door that opens onto a
                -- second door is a door too many.
                Just PitchModal -> modalBox "Pitch" (SweepView.pitchView sweepHandlers)
                Just SaveModal -> modalBox "Save to disk" keepBlock
                Just InputsModal -> modalBox "Name the inputs" inputsPanel
                Nothing -> HH.text ""
            ]
        -- | **Two places, and the transform between them.**
        -- |
        -- | The library on the left and the destination on the right, which
        -- | is DropSync's geometry and for DropSync's reason: you cannot
        -- | judge what a transfer will do from a list of sources alone. What
        -- | it is NOT is DropSync's other half — rsync is symmetrical and
        -- | this only goes one way. The card is compiled from the library and
        -- | never edited; nothing comes back, so there are no arrows and no
        -- | second direction to choose.
        Library ->
          HH.div [ HP.class_ (HH.ClassName "q-panes") ]
            [ HH.div [ HP.class_ (HH.ClassName "q-pane is-library") ] [ setsView ]
            , HH.div [ HP.class_ (HH.ClassName "q-pane is-dest") ]
                [ destHead, transformPanel, cardView ]
            , maybe (HH.text "") setModal st.openSet
            ]
    , HH.section [ HP.class_ (HH.ClassName "q-log") ]
        (map (\l -> HH.div_ [ HH.text l ]) st.log)
    ]
  where
  cp = cap st
  wontKeep = wontKeepOf st
  -- What the daemon says the capture is doing, never a second copy of it.
  --
  -- Four booleans where the loop needed `layers`, `armed`, `isWriting` and
  -- `sized` read together, and where the combination — not any one of them —
  -- said which of six states a loop was in.
  srcName = maybe "?" _.name
    (st.looper >>= \top -> Array.index top.sources (srcNow - 1))
  hasTake = Maybe.isJust st.opened || maybe false _.holds cp
  writing = maybe false _.on cp
  -- How long this take has been running, from the daemon's own frame count
  -- rather than from a clock here: a page that keeps its own time drifts from
  -- the recording it is describing.
  -- | **Whole seconds while it is running.**
  -- |
  -- | Two decimals updating ten times a second is four characters of churn
  -- | inside a flex row, and every door to the right of it stepped sideways
  -- | on each poll. What the number is FOR while recording is "is it still
  -- | going, and roughly how far in" — a question whole seconds answer, and
  -- | hundredths answer no better while making the row unusable. The exact
  -- | length is on the take, under the waveform, where it is read afterwards.
  elapsed = maybe "0" (\c -> show (Int.floor c.secs)) cp

  -- | **The transport, as a transport.**
  -- |
  -- | It was two labelled buttons under the waveform — "\x25b6 all of it" and
  -- | "stop" — which read as a caption rather than as controls, and put the
  -- | only two audio verbs on the page somewhere other than where the other
  -- | verbs are. Record, play and stop are one family and belong in one row;
  -- | the icons are the convention and need no words.
  transport =
    HH.div [ HP.class_ (HH.ClassName "q-transport") ]
      [ HH.button
          [ HP.class_ (HH.ClassName "q-tape")
          , HP.disabled (not hasTake)
          , HP.title "play the whole take, end to end, gaps and all"
          , HE.onClick \_ -> PlayWhole
          ]
          [ HH.text "\x25b6" ]
      , HH.button
          [ HP.class_ (HH.ClassName "q-tape")
          , HP.title "stop"
          , HE.onClick \_ -> StopAudio
          ]
          [ HH.text "\x25a0" ]
      ]

  -- | **What Measure knows about itself**, read from the plan rather than
  -- | from a flag here: `pacedStale` compares the fingerprint the numbers
  -- | were taken under against the sweep as it stands now, so a parameter
  -- | moved an hour ago is as stale as one moved a second ago.
  measHave = not (Array.null st.sweep.paced)
  measStale = Sweep.pacedStale st.sweep
  measureWants = not measHave || measStale
  canMeasure = st.sweepFork == Nothing && not st.busy && st.fill == Swept
  measureSays
    | st.dry = "measuring\x2026"
    | not measHave = show st.sweep.spacingMs <> " ms flat \x2014 not measured"
    | measStale = "stale \x2014 a parameter has changed"
    | otherwise =
        fmt (Int.toNumber (Sweep.startsAt st.sweep
               (Encoding.total st.sweep.extent)) / 1000.0)
          <> " s measured, per cell"

  -- | **Peers of Record.** They are all things you do to this take, and a
  -- | separate row for four of them implied a separation that is not there.
  doors =
    HH.div [ HP.class_ (HH.ClassName "q-doors") ]
      -- | **Measure, beside the other acts.**
      -- |
      -- | It was a button inside the Trigger door, on the argument that a dry
      -- | run is a setting you arrive at rather than an act you reach for.
      -- | That was wrong about how it gets used: the numbers go stale every
      -- | time a parameter moves, so measuring is something you do again and
      -- | again in a session — and burying a repeated act two clicks deep is
      -- | the definition of the wrong place for it. What stays behind the
      -- | door is the *choice* between flat and measured, and the numbers.
      -- |
      -- | The tint is the same one Divide wears: this is a verb that is
      -- | asking to be done. It lights when nothing has been measured, and
      -- | again the moment a measurement stops describing the sweep.
      [ HH.button
          [ HP.class_ (HH.ClassName ("q-door"
              <> (if measureWants then " is-verb" else "")
              <> (if canMeasure then "" else " is-moot")))
          , HP.disabled (not canMeasure)
          , HP.title "run the sweep once at the flat spacing and keep only how \
                     \long each cell took to go quiet — then every later run is \
                     \paced by what this instrument actually does"
          , HE.onClick \_ -> DryRun srcNow
          ]
          [ HH.span [ HP.class_ (HH.ClassName "q-doorname") ] [ HH.text "Measure" ]
          , HH.span [ HP.class_ (HH.ClassName "q-doorsays") ] [ HH.text measureSays ]
          ]
      -- | **The verb until it has happened, then the noun.**
      -- |
      -- | "Divide it" sat alone under the waveform while a card two feet away
      -- | read "nothing divided yet" — the same fact, twice, and the one you
      -- | could act on was the orphan. One slot now: it divides, and afterwards
      -- | it is the door onto what it produced. Dividing again lives inside,
      -- | because a different divider or gap is a thing you reach for second.
      , if Array.null st.regions
          then HH.button
                 [ HP.class_ (HH.ClassName ("q-door is-verb"
                     <> (if hasTake && not st.busy then "" else " is-moot")))
                 , HP.disabled (not hasTake || st.busy)
                 , HE.onClick \_ -> Analyse
                 ]
                 [ HH.span [ HP.class_ (HH.ClassName "q-doorname") ] [ HH.text "Divide" ]
                 , HH.span [ HP.class_ (HH.ClassName "q-doorsays") ]
                     [ HH.text (if st.busy then "dividing…"
                                else if hasTake then "cut the take by its own schedule"
                                else "nothing recorded yet") ]
                 ]
          else door DivisionModal "Division"
                 (show (Set.size st.keep) <> " of "
                    <> show (Array.length st.regions) <> " kept")
                 true
      , door SaveModal "Save to disk"
          (if st.kept then "kept as " <> setName
           else "\x2192 samples/" <> setName)
          (not (Set.isEmpty st.keep))
      ]

  -- | The pivot's own modal, closed by clearing the pivot rather than the
  -- | modal — it is summoned by a band and belongs to that band.
  modalPivot title inner =
    HH.div [ HP.class_ (HH.ClassName "q-scrim") ]
      [ HH.div [ HP.class_ (HH.ClassName "q-modal") ]
          [ HH.div [ HP.class_ (HH.ClassName "q-modalhead") ]
              [ HH.h2_ [ HH.text title ]
              , HH.button
                  [ HP.class_ (HH.ClassName "q-plain")
                  , HE.onClick \_ -> OpenPivot Nothing ]
                  [ HH.text "done" ]
              ]
          , inner
          ]
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

-- | **How long each cell gets** — one flat number, or what a dry run measured.
-- |
-- | Lives behind the Trigger door rather than in the control row because a dry
-- | run is a setting you arrive at, not an act you reach for: it is done once,
-- | read from afterwards, and the thing you press repeatedly is still Record.
  pacingPanel =
    let
      pl = st.sweep
      n = Encoding.total pl.extent
      stale = Sweep.pacedStale pl
      have = not (Array.null pl.paced)
      total = Int.toNumber (Sweep.startsAt pl n) / 1000.0
    in
      HH.div [ HP.class_ (HH.ClassName "q-pacing") ]
        [ HH.div [ HP.class_ (HH.ClassName "q-gridhead") ] [ HH.span_ [ HH.text "Spacing" ] ]
        , HH.label [ HP.class_ (HH.ClassName "q-pace-opt") ]
            [ HH.input
                [ HP.type_ HP.InputRadio, HP.name "pacing"
                , HP.checked (not pl.usePaced)
                , HE.onChange \_ -> UsePaced false
                ]
            , HH.text (" flat — " <> show pl.spacingMs <> " ms for every cell")
            ]
        , HH.label
            [ HP.class_ (HH.ClassName ("q-pace-opt" <> if have then "" else " is-moot")) ]
            [ HH.input
                [ HP.type_ HP.InputRadio, HP.name "pacing"
                , HP.checked pl.usePaced, HP.disabled (not have)
                , HE.onChange \_ -> UsePaced true
                ]
            , HH.text (if have
                         then " measured per cell — " <> fmt total <> " s in total"
                         else " measured per cell — nothing measured yet")
            ]
        -- **The choice lives here; the act does not.** Measure is in the
        -- action bar with Record and Divide, because it is done repeatedly
        -- and a repeated act two clicks deep is in the wrong place.
        , HH.div [ HP.class_ (HH.ClassName "q-pace-act") ]
            [ HH.span [ HP.class_ (HH.ClassName "q-muted") ]
                [ HH.text (if not have
                             then "Measure, in the action bar, runs the sweep once at \
                                  \the flat spacing and keeps only how long each cell \
                                  \took to go quiet"
                           else if stale
                             then "a parameter has changed since these were measured, \
                                  \so they no longer describe this sweep \x2014 Measure again"
                           else "these describe the sweep as it stands")
                ]
            ]
        , if not have then HH.text "" else
            HH.div [ HP.class_ (HH.ClassName "q-pace-cells") ]
              (Array.mapWithIndex
                (\i ms -> HH.span
                   [ HP.class_ (HH.ClassName ("q-pace-cell" <> if stale then " is-stale" else "")) ]
                   [ HH.text (show (i + 1) <> ": " <> fmt (Int.toNumber ms / 1000.0) <> " s") ])
                pl.paced)
        ]



  sweepHandlers =
    { ports: st.midiPorts, open: st.sweepEdit, plan: st.sweep
    , msg: SweepMsg, openParam: OpenParam
    , openTrigger: OpenModal (Just TriggerModal)
    , openPitch: OpenModal (Just PitchModal)
    , tables: st.tables, tablesErr: st.tablesErr, pickPitch: PickPitch
    , rigFires: st.fill == Swept, addPitch: AddPitch
    }

  -- | **A modal, for the jobs that are consulted rarely and read never.**
  -- |
  -- | Dividing settings and card addresses are each a handful of controls that
  -- | matter intensely for about ten seconds and then never again. On the page
  -- | they competed for attention with the two things you look at constantly.
  -- | **The scrim does not close on click.**
  -- |
  -- | It did, with a guard on the inner div that re-set the same modal to
  -- | absorb the click — and the guard never worked. Halogen dispatches both
  -- | handlers on a bubbling click, inner first, so the scrim's `Nothing`
  -- | always landed last and the modal shut the instant you touched anything
  -- | in it. Focusing a text field was enough. Closing is now the business of
  -- | the one control that says so.
  modalBox title inner =
    HH.div
      [ HP.class_ (HH.ClassName "q-scrim") ]
      [ HH.div
          [ HP.class_ (HH.ClassName "q-modal") ]
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
  -- | **One paragraph, and nothing under it.**
  -- |
  -- | It had a second line in small grey type carrying the calibration, the
  -- | note range, which parameter was on which axis, and how long the run
  -- | would take. Every one of those is now said better somewhere it can be
  -- | acted on: the keyboard draws the notes, the grid names the axes and
  -- | prints the per-cell times, and Measure's own subtitle carries the total.
  -- | What was left was a footnote restating the page, in the one place that
  -- | is supposed to be the page's whole claim in a breath.
  -- |
  -- | So the calibration joins the sentence — it is a choice, and every other
  -- | choice is in here — and the rest goes. Half width and centred, because a
  -- | statement that runs the full width of a wide screen reads as a caption.
  statement =
    HH.section [ HP.class_ (HH.ClassName "q-say") ]
      [ listening
      -- **The sentence says what will actually happen, and the two cases do
      -- not happen alike.**
      --
      -- A swept run makes a stated number of samples on a schedule it declares
      -- in advance, so the count, the pitch axis and the running time are all
      -- promises it can keep. A take played by hand makes as many samples as
      -- you play; its extent is unused, nothing tunes it, and no one can say
      -- how long it will run. Printing "Making 4 × 12 pitched samples" over a
      -- take that will produce nine chords is not a small inaccuracy — it is
      -- the page claiming to be in charge of something it is not, and it sent
      -- somebody looking for the count control that does not exist.
      --
      -- **`slotKind` is in both**, and it is the repair for the other half of
      -- that question: the kind decides the material, and the material decides
      -- the divider's default, the fold to mono or stereo, the shape on the
      -- card, and how long a decay the divider waits for — 4 seconds for hits,
      -- 20 for chords. All of that was chosen behind a door labelled Trigger,
      -- so the only visible trace of it was the generated name, which looks
      -- like a name.
      , HH.p [ HP.class_ (HH.ClassName "q-sayline") ]
          ( case st.fill of
              Played ->
                [ HH.text "Making ", slotKind
                , HH.text " from ", slotSource, slotSourceName
                , HH.text ", triggered by ", slotTrigger
                , HH.text ", kept as ", slotName
                , HH.text " for ", slotEncoding
                , HH.text ". As many as you play."
                ]
              Swept ->
                [ HH.text "Making ", slotExtent
                , HH.text " ", slotPitched
                , HH.text " ", slotKind
                , HH.text " from ", slotSource, slotSourceName
                , HH.text ", triggered by ", slotTrigger
                , HH.text ", kept as ", slotName
                , HH.text " for ", slotEncoding
                ]
                  -- Named only when there IS one to name: "tuned by nothing"
                  -- is a clause about an absence, and an unpitched run is not
                  -- missing anything.
                  <> (if Sweep.pitched st.sweep
                        then [ HH.text ", tuned by ", slotCalib ] else [])
                  <> [ HH.text ", about ", HH.text runSecs, HH.text " to record." ] )
      , clashSays
      , collapseSays
      , heardSays
      -- | **A remembered input that is not on this rig.** Silently falling
      -- | back to whatever is first is how the whole morning went to `board`;
      -- | the fallback is still the right behaviour, and saying so is the
      -- | other half of it.
      , if not sourceLost then HH.text "" else
          HH.p [ HP.class_ (HH.ClassName "q-clash is-soft") ]
            [ HH.text ("this set was recorded from \x201c"
                <> labelFor st st.sweep.source
                <> "\x201d, which the rig is not offering — using \x201c"
                <> labelFor st srcName <> "\x201d instead. Sources are named \
                   \per interface and jack, so a renamed or re-ordered \
                   \aggregate loses the reference rather than pointing it \
                   \somewhere wrong.") ]
      ]

  -- | **Whether anything is listening, said before the take and not after it.**
  -- |
  -- | A hand-played take is the only kind whose pitches cannot be recovered
  -- | afterwards: the chord that made the audio is not in the audio, and by the
  -- | time the set is on disk the performance is over. So this is a pre-flight
  -- | line, in the sentence, where it is read before pressing Record — the
  -- | alternative is finding out from an empty `notes` field in `set.json`,
  -- | which is exactly the shape of failure this page keeps being bitten by:
  -- | something that was never firing, reported only by what is missing.
  -- |
  -- | Silent for a swept run, which asks for its own pitches and does not need
  -- | to be told.
  -- | Every port, and how much each has said. Traffic on a port nobody played
  -- | is the whole diagnosis — a sequencer on an IAC bus looks exactly like a
  -- | performance until you see it counted beside the one you meant.
  portTally =
    map (\nm -> { name: nm
                , n: Array.length (Array.filter (\h -> h.from == nm) st.heard) })
        st.midiIn

  heardSays
    | st.fill /= Played = HH.text ""
    | not st.midiOk =
        HH.p [ HP.class_ (HH.ClassName "q-clash is-soft") ]
          [ HH.text "this page has no Web MIDI, so nothing can be recorded \
                    \about what you play. The browser only offers it on a \
                    \secure origin — open the page as http://localhost:3029 \
                    \rather than by the machine's name, and the ports appear." ]
    | Array.null st.midiIn =
        HH.p [ HP.class_ (HH.ClassName "q-clash is-soft") ]
          [ HH.text "no MIDI input is reaching the page, so nothing will be \
                    \recorded about what you play. The audio is unaffected — \
                    \but a chord set without its notes cannot be given them \
                    \afterwards." ]
    | st.sweep.notesFrom == "" =
        HH.p [ HP.class_ (HH.ClassName "q-clash is-soft") ]
          [ HH.text ("nothing is being kept about what you play: choose which \
                     \MIDI input carries it. " <> tallySays
                     <> " Listening to all of them is not the answer — the \
                        \rig's own buses are on this machine too.")
          , HH.span_ [ HH.text " " ]
          , notesFromPick
          ]
    | otherwise =
        HH.p [ HP.class_ (HH.ClassName "q-note is-quiet") ]
          [ HH.text (case Array.length (believed st) of
                       0 -> "listening to "
                       1 -> "1 note from "
                       n -> show n <> " notes from ")
          , notesFromPick
          , HH.text ", "
          , notesChanPick
          , HH.text (case Array.length (believed st) of
                       0 -> " — nothing played yet. " <> tallySays
                       _ -> ". " <> tallySays)
          , HH.text chanSays
          ]

  -- | What each port has said, named. The point is the comparison.
  tallySays = case Array.filter (\t -> t.n > 0) portTally of
    [] -> ""
    ts -> "Heard so far: "
            <> joinWith ", " (map (\t -> t.name <> " " <> show t.n) ts) <> "."

  -- | **What each channel on the chosen port has said.**
  -- |
  -- | The port narrows it to one cable, which is often not enough: a surface
  -- | handshake, a sequencer and a keyboard can share a port and differ only
  -- | by channel. Counted rather than guessed, because the tell is the
  -- | *proportion* — five to seven notes at a time is somebody playing chords,
  -- | and forty in a burst is not.
  chanTally =
    let onPort = Array.filter (\h -> h.from == st.sweep.notesFrom) st.heard
    in Array.sortWith _.c
         (map (\c -> { c, n: Array.length (Array.filter (\h -> h.chan == c) onPort) })
           (Array.nub (map _.chan onPort)))

  chanSays = case Array.filter (\t -> t.n > 0) chanTally of
    -- One channel is not a choice worth showing; it is just where the notes
    -- are.
    [ _ ] -> ""
    [] -> ""
    ts -> " By channel: "
            <> joinWith ", " (map (\t -> show t.c <> " \x2192 " <> show t.n) ts)
            <> "."

  notesChanPick =
    sel "q-slot is-small" (show st.sweep.notesChan) (SweepMsg <<< Sweep.SetNotesChan)
      (Array.cons { v: "0", t: "any channel" }
        (map (\c -> { v: show c, t: "channel " <> show c })
          (Array.range 1 16)))

  notesFromPick =
    sel "q-slot is-small" st.sweep.notesFrom (SweepMsg <<< Sweep.SetNotesFrom)
      (Array.cons { v: "", t: "\x2014 choose an input \x2014" }
        (map (\nm -> { v: nm, t: nm }) st.midiIn))

  -- | **Two things pointed at one jack, said in the sentence.**
  -- |
  -- | In the sentence and not in a panel, because both halves of this were
  -- | already correct and already displayed — the Trigger modal said "bus 15 ·
  -- | ES-9 jack 8" and the parameter row said "cv 15 (ES-9 jack 8)" — and it
  -- | still cost a day, because nothing ever put the two in one view or
  -- | compared them. A fact in the right place that nobody reads beside the
  -- | fact it contradicts is not information.
  clashSays = case Sweep.conflicts st.sweep of
    [] -> HH.text ""
    cs -> HH.p [ HP.class_ (HH.ClassName "q-clash") ]
            [ HH.text (joinWith " · " (map Sweep.sayConflict cs)
                <> " — one of them has to go somewhere else, or off. \
                   \Nothing on the page can tell you which was meant, and the \
                   \rig will happily sum them into one voltage.") ]

  -- | **A grid that is really a line**, said beside the conflicts because it
  -- | is the same kind of mistake: a fact about the plan that is legal,
  -- | silent, and produces a set shaped nothing like the one you asked for.
  -- | Softer than a clash, because four repeats of one transect is sometimes
  -- | exactly what you meant.
  collapseSays = case Sweep.collapsed st.sweep of
    Nothing -> HH.text ""
    Just says -> HH.p [ HP.class_ (HH.ClassName "q-clash is-soft") ] [ HH.text says ]

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
  -- | **A meter beside the sentence, not a word inside it.**
  -- |
  -- | Twenty block characters in a line of prose read as an empty underlined
  -- | word — Andrew, 2026-09-12, having just lost a transect to a silent
  -- | input: *"I just didn't make that association when it wasn't moving"*.
  -- | Which is the failure exactly: the one thing it exists to say is said by
  -- | NOT moving, and a flat row of `▁` in a sentence looks like a rendering
  -- | fault rather than like silence.
  -- |
  -- | Drawn, and to the left of the statement, it is a meter: an object with a
  -- | baseline, so flat is a reading rather than an absence. It also says so
  -- | in words underneath, because the whole point is that the picture alone
  -- | was not enough.
  listening =
    let
      n = 20
      recent = Array.takeEnd n st.levels
      padded = Array.replicate (n - Array.length recent) 0.0 <> recent
      w = 1.0 / Int.toNumber n
      bar i v =
        let h = max 0.02 (clampN 0.0 1.0 v)
        in Wave.el "rect"
             [ Wave.attr "x" (show (Int.toNumber i * w + w * 0.12))
             , Wave.attr "y" (show (1.0 - h))
             , Wave.attr "width" (show (w * 0.76))
             , Wave.attr "height" (show h) ] []
    in
      HH.div
        [ HP.class_ (HH.ClassName ("q-meter" <> if quiet then " is-quiet" else ""))
        , HP.title (fmt srcDb <> " dB — the last two seconds of input level"
                      <> (if quiet then ". Nothing is playing into it." else ""))
        ]
        [ Wave.el "svg"
            [ Wave.attr "viewBox" "0 0 1 1", Wave.attr "preserveAspectRatio" "none"
            , Wave.attr "class" "q-meterpic" ]
            (Array.mapWithIndex bar padded)
        , HH.span [ HP.class_ (HH.ClassName "q-meterword") ]
            [ HH.text (if quiet then "silent" else fmt srcDb <> " dB") ]
        ]

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
    -- **A grid's shape is editable too.** It printed "4 × 12" and could not be
    -- touched, so the only way to reshape a matrix was through a door — which
    -- is the opposite of what the sentence is for.
    ns -> HH.span [ HP.class_ (HH.ClassName "q-slotgrid") ]
            (Array.intercalate [ HH.span [ HP.class_ (HH.ClassName "q-by") ] [ HH.text "×" ] ]
              (Array.mapWithIndex
                (\a n ->
                  [ HH.input
                      [ HP.class_ (HH.ClassName "q-slot is-num"), HP.type_ HP.InputNumber
                      , HP.value (show n), HP.min 1.0, HP.max 512.0
                      , HP.title (maybe "" (\ax -> "how many " <> ax.name
                                    <> "s — along this axis the choice is made by "
                                    <> ax.picked) (Array.index (Encoding.axes st.sweep.encoding) a))
                      , HE.onValueInput \v -> SweepMsg (Sweep.SetExtent a v)
                      ]
                  ])
                ns))

  -- | **What is being recorded**, which is the one thing everything else
  -- | follows from — the divider's default, the fold to mono or stereo, the
  -- | shape on the card, and the decay the divider is willing to wait for. It
  -- | was a row of chips behind the Trigger door, where nothing about it could
  -- | be seen without opening one; the generated name was the only evidence,
  -- | and a name reads as a name.
  slotKind =
    sel "q-slot" (Kind.name st.kind) PickKindNamed
      -- `Kind.all` carries `Bars 1` as the representative, so the bar entry has
      -- to be labelled with the count the page is actually holding — otherwise
      -- a four-bar take reads "one bar" in its own sentence.
      (map (\k -> { v: Kind.name k
                  , t: sentenceKind (case k of
                          Kind.Bars _ -> Kind.Bars st.bars
                          other -> other) })
        Kind.all)

  -- | The kind as it reads mid-sentence: "12 pitched chord hits from …".
  -- | `Kind.label` is a heading and is capitalised for one.
  sentenceKind k = case k of
    Kind.Bars n -> if n == 1 then "one bar" else show n <> " bars"
    other -> String.toLower (Kind.label other)

  slotPitched =
    sel "q-slot" (if pitchOn then "pitched" else "unpitched") SetPitched
      [ { v: "unpitched", t: "unpitched" }, { v: "pitched", t: "pitched" } ]

  pitchOn = Sweep.pitched st.sweep

  slotSource = case st.looper of
    Nothing -> HH.span [ HP.class_ (HH.ClassName "q-slot is-fixed") ] [ HH.text "…" ]
    Just top ->
      sel "q-slot" srcName PickSource
        (map (\src -> { v: src.name
                      , t: labelFor st src.name
                             <> (if src.available then "" else " — off") })
          top.sources)

  -- | **The way in to naming the inputs**, beside the input it renames.
  -- |
  -- | It is a one-time exercise and it belongs at the one moment you notice it
  -- | is needed, which is while reading the sentence and not recognising what
  -- | it says. Not a door in the action row: those are things you do on every
  -- | run, and a sixth of them for a thing done once would be the row's worst
  -- | entry.
  -- |
  -- | **Set as a footnote marker rather than as small prose.** As a word at the
  -- | sentence's own weight it landed in the flow between the input's name and
  -- | the comma after it — "samples from ES9 channel 1 name…, triggered by" —
  -- | which reads as a broken word, not as a control. Raised and lettered, it
  -- | is a mark attached to the thing it renames, and the eye skips it until
  -- | it is wanted.
  slotSourceName = case st.looper of
    Nothing -> HH.text ""
    Just _ ->
      HH.button
        [ HP.class_ (HH.ClassName "q-rename")
        , HP.title "name the inputs after what is plugged into them"
        , HE.onClick \_ -> OpenModal (Just InputsModal)
        ]
        [ HH.text "rename" ]

  -- | **Name each input after the thing on the other end of the cable.**
  -- |
  -- | The daemon's names are wire names and have to be: `--source
  -- | board=AUDIO4c:1,2` resolves against the interface, and a name that
  -- | cannot be resolved is a session recorded off the wrong jack. But nobody
  -- | picks "hits" out of a list at speed — on 2026-09-11 a whole 4 x 12 was
  -- | recorded from "board" instead and came back silent — and no rename in
  -- | the launch args could fix that generally, because the right name is not
  -- | a fact about this rig. It is "Jupiter 8", or "the Neumann", and only the
  -- | person holding the cable knows it.
  -- |
  -- | So the wire name is shown beside the field rather than hidden by it: the
  -- | label is what you read afterwards, and the wire name is what you check
  -- | against a patch cable when something is wrong.
  inputsPanel = case st.looper of
    Nothing ->
      HH.p [ HP.class_ (HH.ClassName "q-note") ]
        [ HH.text "not connected to the daemon, so there are no inputs to name." ]
    Just top ->
      HH.div [ HP.class_ (HH.ClassName "q-inputs") ]
        ( [ HH.p [ HP.class_ (HH.ClassName "q-note") ]
              [ HH.text "Name each input after whatever is plugged into it. \
                        \The name on the right is the one the daemon was \
                        \launched with and is what a stored set records, so \
                        \renaming here never orphans a set — and clearing a \
                        \name puts that one back." ]
          ]
            <> map
                 (\src ->
                   HH.label [ HP.class_ (HH.ClassName "q-inputrow") ]
                     [ HH.input
                         [ HP.class_ (HH.ClassName "q-slot is-name")
                         , HP.type_ HP.InputText
                         , HP.value (maybe "" _.label
                             (Array.find (\r -> r.wire == src.name) st.srcNames))
                         , HP.placeholder src.name
                         , HE.onValueInput (NameSource src.name)
                         ]
                     , HH.span [ HP.class_ (HH.ClassName "q-inputwire") ]
                         [ HH.text (src.name
                             <> (if src.mono then " · mono" else " · stereo")
                             <> (if src.available then "" else " · off")) ]
                     ])
                 top.sources )

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
  -- | **The label alone.** A calibration names a signal path and the module
  -- | name is the longest thing on it; set at the sentence's own weight it was
  -- | half the paragraph, for a fact you check once. The label is the handle;
  -- | the module belongs where the table is being CHOSEN, which is the Pitch
  -- | modal, and it is there.
  slotCalib =
    sel "q-slot" (fromMaybe "" pitchLabel) AddPitch
      ( Array.cons { v: "", t: "— none, unpitched —" }
          (map (\t -> { v: t.label, t: t.label }) st.tables) )

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
  -- **`pitchSays` and `sweptSays` lived here.** The first printed the note
  -- range, which the keyboard now draws; the second grouped the parameters by
  -- axis to catch a grid that was really a line, which the grid itself now
  -- shows and `Sweep.collapsed` now says out loud. Both were footnotes
  -- restating the page under the one line that is meant to BE the page.

  -- | **What the run will actually take**, which with measured pacing is a sum
  -- | and not a multiple. Read off the same function the run paces itself by,
  -- | so the estimate cannot disagree with the thing it estimates.
  runSecs =
    let n = Encoding.total st.sweep.extent
    in fmt (Int.toNumber (Sweep.startsAt st.sweep n) / 1000.0) <> " s"

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
              , case st.fill of
                  -- A transect closing itself is how a transect works, said
                  -- once in the docs and never needed again beside the button.
                  Swept -> HH.text ""
                  Played ->
                    HH.span [ HP.class_ (HH.ClassName "q-state") ]
                      [ HH.text (maybe "" (\c -> if c.holds
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

  -- Whichever is chosen, or the first the daemon says is available.
  -- | **The remembered name, resolved against what the rig actually has.**
  -- |
  -- | Falls back to the first available source when the name is unset or gone,
  -- | and `sourceLost` says so out loud rather than quietly recording from
  -- | somewhere else — which is the failure this whole field exists to stop.
  srcNow = case st.looper of
    Nothing -> 1
    Just top ->
      case Array.findIndex (\s0 -> s0.name == st.sweep.source) top.sources of
        Just i -> i + 1
        Nothing -> 1 + fromMaybe 0 (Array.findIndex _.available top.sources)

  sourceLost = case st.looper of
    Just top | st.sweep.source /= ""
             , Array.all (\s0 -> s0.name /= st.sweep.source) top.sources -> true
    _ -> false
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
            , pickedBar
            ]
              <> map setRow st.sets
          )

  -- | What already sits at the address the Library is pointing at. Named by
  -- | the letter, because that is what the module shows and what a write
  -- | deletes; the bank *name* is only the legend.
  -- | **Which slot this will actually become** — `L1`, in the module's own
  -- | names.
  -- |
  -- | There is no control for the number, and there cannot be one: `kit build`
  -- | names each kit `{letter}{index}` where the index is its POSITION in the
  -- | bank's kit list. So the number is not a thing you set, it is a thing you
  -- | are TOLD — and it was being told to nobody, which left the page talking
  -- | about letters and names while the module talks about L0 and L1.
  -- |
  -- | An existing kit of the same name keeps its index; anything else lands
  -- | after the kits already in that bank. Blank ticks make a kit per set, so
  -- | they take consecutive slots and it says so as a range.
  landsAt =
    let
      inBank = Array.filter (\r -> r.letter == st.letter) (maybe [] _.rows st.cardView)
      here = Array.nub (map (\r -> { ix: r.kitIx, kit: r.kit }) inBank)
      named = if st.kit == "" then Nothing
              else map _.ix (Array.find (\k -> k.kit == st.kit) here)
      next = Array.length here
      n = max 1 (Set.size st.picked)
    in
      case named of
        Just i -> st.letter <> show i
        Nothing
          | st.kit /= "" -> st.letter <> show next
          -- One kit per ticked set, so a run of them.
          | n <= 1 -> st.letter <> show next
          | otherwise -> st.letter <> show next <> "\x2013"
                           <> st.letter <> show (next + n - 1)

  -- | Whether anything ticked is stereo, and so wants a voice pair.
  pickedWide =
    Array.any (\r -> Set.member r.name st.picked && r.stereo) st.sets

  -- | **The kit this placement will actually address.**
  -- |
  -- | A blank kit field does not mean "any kit" — it means each ticked set
  -- | makes a kit of its own name. Reading blank as a wildcard is what made
  -- | the warning fire on `bia oct x decay` for a placement that was never
  -- | going near it.
  targetKit
    | st.kit /= "" = Just st.kit
    | otherwise = case Array.fromFoldable st.picked of
        [ one ] -> Just one
        -- Several ticked, each making its own kit: no single address to warn
        -- about, and the slot readout already says which range they take.
        _ -> Nothing

  -- | What already sits at that address. Named by the letter, because that is
  -- | what the module shows and what a write deletes; the bank *name* is only
  -- | the legend.
  sittingAt = do
    v <- st.cardView
    k <- targetKit
    Array.find
      (\r -> r.letter == st.letter && r.voice == st.voice && r.kit == k)
      v.rows

  -- | **What to do with the ticked ones.**
  -- |
  -- | Above the list rather than below it, because it is the thing you are
  -- | reaching for once the ticking is done and a list of forty sets puts the
  -- | bottom of itself off the screen. Empty of verbs until something is
  -- | ticked: a row of buttons that cannot act is a row of buttons to read
  -- | past every time you come here.
  pickedBar =
    let n = Set.size st.picked
    in
      HH.div [ HP.class_ (HH.ClassName ("q-pickbar" <> if n == 0 then " is-idle" else "")) ]
        [ HH.label [ HP.class_ (HH.ClassName "q-pickall") ]
            [ HH.input
                [ HP.type_ HP.InputCheckbox
                , HP.checked (n > 0 && n == Array.length st.sets)
                , HE.onChange \_ -> PickAllSets (n < Array.length st.sets)
                ]
            -- **Say that the controls are behind the tick.** The whole
            -- destination row appears only once something is selected, which
            -- is right — settings for a set you have not chosen are noise —
            -- and left "where do I choose the bank?" with no answer on the
            -- page where the banks are. One word of it costs nothing.
            , HH.text (if n == 0 then " select \x2014 then say where it goes"
                       else " " <> show n <> " selected")
            ]
        , if n == 0 then HH.text "" else
            HH.div [ HP.class_ (HH.ClassName "q-pickdo") ]
              -- **The library's own verb, and only that one.**
              --
              -- Deleting a set destroys the artefact; putting one on a card
              -- makes a projection of it. They used to sit in one row, which
              -- read as two ways of sending — and the destructive one is not
              -- a way of sending at all. It stays here with the sets; the
              -- placement moved to the destination beside what it lands on.
              [ if st.confirmDrop
                  then HH.span [ HP.class_ (HH.ClassName "q-twoverbs") ]
                    [ HH.button
                        [ HP.class_ (HH.ClassName "q-plain is-replacing")
                        , HP.disabled st.cardBusy
                        , HP.title "the sample files are removed. The takes they \
                                   \were cut from are left alone."
                        , HE.onClick \_ -> DropPicked
                        ]
                        [ HH.text ("delete " <> show n <> " for good") ]
                    , HH.button
                        [ HP.class_ (HH.ClassName "q-plain")
                        , HE.onClick \_ -> AskDrop false ]
                        [ HH.text "cancel" ]
                    ]
                  else HH.button
                    [ HP.class_ (HH.ClassName "q-plain")
                    , HP.disabled st.cardBusy
                    , HE.onClick \_ -> AskDrop true
                    ]
                    [ HH.text "Delete\x2026" ]
              ]
        -- | **Say which ones, and let each be heard.**
        -- |
        -- | Andrew, 2026-09-12: *"should we add some audio preview in the
        -- | delete process to avoid disappointing accidents?"* — and the
        -- | accident is specifically likely here, because every name is a
        -- | timestamp and two runs of the same morning differ by four
        -- | characters in the middle. A count is not a check; the names are,
        -- | and the sound is the only check that cannot be misread.
        , if not st.confirmDrop then HH.text "" else
            HH.div [ HP.class_ (HH.ClassName "q-doomed") ]
              ( [ HH.span [ HP.class_ (HH.ClassName "q-warn") ]
                    [ HH.text "about to delete, for good — hear them first:" ] ]
                  <> map
                      (\r -> HH.button
                         [ HP.class_ (HH.ClassName "q-plain is-hear")
                         , HP.title ("hear " <> r.name)
                         , HE.onClick \_ -> HearSet r.name r.count
                         ]
                         [ HH.text ("\x266a " <> r.name) ])
                      (Array.filter (\r -> Set.member r.name st.picked) st.sets)
              )
        ]

  -- | **Which destination this pane is describing.**
  -- |
  -- | One entry today, and a picker anyway — because the shape of the
  -- | destination is what decides the controls beneath it, and that shape is
  -- | not a skin. A Rample is banks of kits of four voices of twelve layers
  -- | with one global SLICER; a QuadDrum is a folder per set at the root,
  -- | 128 to a voice; an Arbhar stick is six banks of thirty-six, addressed
  -- | by position. None of the three is the other with different words, so
  -- | none of them can share this pane's table — and the thing that must NOT
  -- | be shared is where a second one goes.
  -- |
  -- | It also says what the module imposes, which the page has never said
  -- | anywhere: the limits are enforced by `kit build` and discovered by
  -- | having a build refused.
  destHead =
    HH.div [ HP.class_ (HH.ClassName "q-desthead") ]
      [ HH.div [ HP.class_ (HH.ClassName "q-sechead") ]
          [ HH.h2_ [ HH.text "Destination" ]
          , HH.span [ HP.class_ (HH.ClassName "q-muted") ]
              [ HH.text "Squarp Rample \x2014 banks of kits, 4 voices, 12 layers each, \
                        \one SLICER for the whole card" ]
          ]
      ]

  -- | **The transform, between the two places.**
  -- |
  -- | Not a property of the set and not a property of the card: a set is the
  -- | same twelve files either way, and what is chosen here is how they are
  -- | ADDRESSED once they land — layers the layer CV picks between, or one
  -- | file the start point indexes. Which is why it sits between the library
  -- | and the card rather than inside either, and why it can be changed and
  -- | the placement repeated without going back to the take.
  transformPanel
    | Set.isEmpty st.picked =
        HH.p [ HP.class_ (HH.ClassName "q-muted") ]
          [ HH.text "Tick a set on the left and this says where it lands, and how \
                    \it is addressed when it gets there." ]
    | otherwise =
        HH.div [ HP.class_ (HH.ClassName "q-transform") ]
          [ -- | **Say what is about to be destroyed, before it is.** A kit's
            -- | voice is an address, and sending to one that is taken replaces
            -- | what is there unless you said to add.
            case sittingAt of
              Just r ->
                HH.p [ HP.class_ (HH.ClassName "q-occupied") ]
                  [ HH.text (landsAt <> " voice " <> show st.voice <> " holds "
                      -- One set can be several layers of one voice, and naming
                      -- it four times says nothing four times.
                      <> joinWith ", " (Array.nub r.sets)
                      <> (if r.slicer > 0 then " in " <> show r.slicer <> " slots" else "")
                      <> (if Array.length (Array.nub r.sets) == 1
                            then (if st.placeAppend then " \x2014 this will stand beside it"
                                  else " \x2014 this will take its place")
                            else (if st.placeAppend then " \x2014 this will stand beside them"
                                  else " \x2014 this will take their place"))) ]
              _ -> HH.text ""
          , HH.div [ HP.class_ (HH.ClassName "q-pickdo") ]
              -- | **Where it lands, beside the button that lands it.**
              -- |
              -- | These were only ever in the Bench's Export panel, so the
              -- | Library could tick sets and press "Onto the card" with no
              -- | way to say which bank — and its own tooltip promised a
              -- | letter "chosen below" that was on another page. The letter
              -- | is not optional: a write deletes the slot it lands on, so
              -- | `placeOnCard` refuses without one and the press did nothing
              -- | but say so.
              [ letterStrip
          , small "bank" st.bank SetBank
          , small "kit" st.kit SetKit
          , HH.label [ HP.class_ (HH.ClassName "q-field is-tight") ]
              [ HH.span_ [ HH.text "voice" ]
              -- A ticked set that is stereo needs a PAIR, so the offer
              -- narrows to the two starts a pair can have. Any ticked one
              -- being stereo is enough: they are all going to the same
              -- voice number.
              , HH.select [ HE.onValueChange SetVoice ]
                  (map (\vn -> HH.option
                          [ HP.value (show vn), HP.selected (vn == st.voice) ]
                          [ HH.text (show vn
                              <> (if pickedWide then " + " <> show (vn + 1) else "")) ])
                      (voicesWide pickedWide))
              ]
          , arrangePicker
          -- **Two things you might mean, and they are opposites.** A voice
          -- holds a stack, so sending to one that is taken either stands
          -- beside what is there or takes its place. Offered as a choice
          -- rather than two buttons because any number of sets can be
          -- ticked, and "replace" would mean something different for each.
          , HH.label [ HP.class_ (HH.ClassName "q-field is-tight") ]
              [ HH.span_ [ HH.text "if taken" ]
              , HH.select [ HE.onValueChange (SetPlaceAppend <<< (_ == "add")) ]
                  (map (\o -> HH.option
                          [ HP.value o.v
                          , HP.selected ((o.v == "add") == st.placeAppend) ]
                          [ HH.text o.t ])
                      [ { v: "replace", t: "replace what is there" }
                      , { v: "add", t: "add as another layer" }
                      ])
              ]
          -- **How the layer selector moves**, once a voice holds more than
          -- one thing to choose between. The whole reason to use layers
          -- rather than slices: these are the modes where the module
          -- decides for itself.
          , HH.label [ HP.class_ (HH.ClassName "q-field is-tight") ]
              [ HH.span_ [ HH.text "picked by" ]
              , HH.select [ HE.onValueChange SetLayerMode ]
                  (map (\m -> HH.option
                          [ HP.value m
                          , HP.selected (m == (if st.layerMode == "" then "manual" else st.layerMode)) ]
                          [ HH.text m ])
                      [ "manual", "velocity", "random", "cyclic" ])
              ]
          -- The module's own name for where this is going. Not a
          -- control: see `landsAt`.
          , HH.span [ HP.class_ (HH.ClassName "q-dest") ]
              [ HH.text (if st.letter == "" then "\x2192 name a bank letter"
                         else "\x2192 " <> landsAt) ]
          , HH.button
              [ HP.class_ (HH.ClassName "q-plain")
              , HP.disabled (st.cardBusy || st.letter == "")
              , HP.title (if st.letter == ""
                            then "name a bank letter first — a write deletes \
                                 \the slot it lands on, so nothing happens \
                                 \until you say which"
                            else "each one as its own kit in bank " <> st.letter)
              , HE.onClick \_ -> PlacePicked
              ]
              [ HH.text ("Onto the card") ]
              ]
          ]

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
    HH.article [ HP.class_ (HH.ClassName ("q-set"
        <> if Set.member r.name st.picked then " is-picked" else "")) ]
      -- **Ticked by name, not by position.** The list is re-fetched after
      -- every write and a position survives none of that.
      [ HH.label [ HP.class_ (HH.ClassName "q-set-tick") ]
          [ HH.input
              [ HP.type_ HP.InputCheckbox
              , HP.checked (Set.member r.name st.picked)
              , HE.onChange \_ -> PickSet r.name
              ]
          ]
      , HH.div [ HP.class_ (HH.ClassName "q-set-id") ]
          [ HH.div [ HP.class_ (HH.ClassName "q-set-name") ] [ HH.text r.name ]
          , HH.div [ HP.class_ (HH.ClassName "q-set-when") ]
              [ HH.text (String.take 10 r.made
                  <> (if r.take == "" then "" else " · from " <> r.take)) ]
          ]
      -- **The samples themselves, before the count of them.** A number and a
      -- shape are two readings of one fact and the shape is the faster, so
      -- the picture leads and the number annotates it.
      , HH.div [ HP.class_ (HH.ClassName "q-set-n") ] [ samplePic r ]
      -- **Mono or a pair.** A stereo sample plays its right channel on the
      -- voice AFTER it, so it occupies two of the four and a stereo kit
      -- answers on SP1 and SP3. That is a fact about the material, decided
      -- when the set was cut, and it halves what a kit can hold — so it is
      -- on the row rather than behind the click.
      , HH.div [ HP.class_ (HH.ClassName "q-set-ch")
               , HP.title (if r.stereo then "stereo — takes a voice PAIR"
                           else "mono — one voice") ]
          [ HH.text (if r.stereo then "\x25cf\x25cf" else "\x25cf") ]
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
          , HH.text (" Captured from " <> labelFor st srcName <> " as it comes"
              <> ", and folded to "
              <> (if Kind.foldsTo st.kind == ToMono then "mono" else "stereo")
              <> " on the way to a card"
              <> (if Kind.voicesOn st.kind == 2
                    then " — where it takes two of the four voices."
                    else "."))
          -- **What is deliberately absent, said out loud.**
          --
          -- A player arriving here looks for the setting that keeps their
          -- decays from being cut, and a swept run has several that look like
          -- it. None of them applies: nothing is triggered, so nothing is
          -- spaced, and where a sample ends is decided afterwards by the
          -- divider, over the audio you actually made.
          , HH.text " Nothing is triggered and nothing is timed: play it as \
                    \you like, and the divider finds the boundaries \
                    \afterwards over what you played."
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
        , HH.div [ HP.class_ (HH.ClassName "q-grid") ]
            ( Array.cons colHead
                (Array.mapWithIndex
                  (\k rng -> gridRow k (plannedRow rng))
                  (rowsOf (Array.length cells))) )
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
                  -- **No column heads over a recorded row.** The planned
                  -- grid divides its row into equal cells, so one header
                  -- lines up with all of them; a recorded row draws each band
                  -- over the stretch of take that made it, and with measured
                  -- pacing those are all different widths — and different
                  -- from row to row. A header saying "slice 10" above a band
                  -- that is slice 12 is worse than no header, and the bands
                  -- name themselves anyway.
                  [ HH.div [ HP.class_ (HH.ClassName "q-grid") ]
                      (Array.mapWithIndex
                        (\k rng -> gridRow k (strip pk rng))
                        (rowsOf (Array.length st.regions)))
                  -- | **The take's length, on the take.**
                  -- |
                  -- | It sat among Record/play/stop and grew a digit as the
                  -- | recording ran, which pushed every door along the row a
                  -- | few pixels at a time. A control row that moves while you
                  -- | are reaching for it is worse than one number short, and
                  -- | the length was never a control anyway — it is a fact
                  -- | about the thing drawn immediately above it, which is
                  -- | also where your eye already is while it is counting.
                  , HH.div [ HP.class_ (HH.ClassName "q-taketime") ]
                      [ HH.text (maybe "" (\c -> fmt c.secs <> " s") (cap st)) ]
                  ]
              _ -> HH.text ""
          -- The orphan "Divide it" is gone: the verb is in the control row,
          -- in the slot that becomes the Division door once it has run.
          , HH.text ""
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
              , strayRegions
              , dryTakeSays
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

  -- | **The destination's own word for each axis**, never "x" and "y": the
  -- | axis decides WHO chooses along it when the set is played, and a Rample
  -- | picks a layer itself where a slice is a position only you can send it
  -- | to. See `Encoding.axes`.
  axisNameOf a =
    maybe "" _.name (Array.index (Encoding.axes st.sweep.encoding) a)

  -- | **A row, named by where it sits on the outer axis.**
  -- |
  -- | Andrew, 2026-09-12, after a 12 × 4 that turned out to be a 12-long line
  -- | repeated four times: *"I need a visual reminder of the layer/slice
  -- | distinction BEFORE I measure or sample"*. Both parameters had been left
  -- | on axis 0, which is legal and was said once in the sentence and nowhere
  -- | else. With the axis named down the side and the values drawn in the
  -- | cells, a collapsed grid is unmissable: every row reads the same four
  -- | times across.
  gridRow k inner
    | innerN <= 0 = inner
    | otherwise =
        HH.div [ HP.class_ (HH.ClassName "q-gridrow") ]
          [ HH.span [ HP.class_ (HH.ClassName "q-rowlab") ]
              [ HH.text (axisNameOf 0 <> " " <> show (k + 1)) ]
          , HH.div [ HP.class_ (HH.ClassName "q-rowbody") ] [ inner ]
          ]

  colHead
    | innerN <= 0 = HH.text ""
    | otherwise =
        HH.div [ HP.class_ (HH.ClassName "q-gridrow is-head") ]
          [ HH.span [ HP.class_ (HH.ClassName "q-rowlab") ] [ HH.text "" ]
          , HH.div [ HP.class_ (HH.ClassName "q-rowbody q-colheads") ]
              (map
                (\j -> HH.span [ HP.class_ (HH.ClassName "q-colhead") ]
                         [ HH.text (axisNameOf 1 <> " " <> show (j + 1)) ])
                (Array.range 0 (innerN - 1)))
          ]

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
      total = max 0.001 (heldSecs st)
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
  ringingSays =
    " STILL SOUNDING at its own end, so this sample carries the front of the next one and a layer stitched from it will click at the join. Give the sweep more space, or keep it as a one-shot."

  -- | The notes struck inside region `i`, or none. The same function the send
  -- | uses, so what the grid shows and what `set.json` records cannot differ.
  playedAt i = case st.opened of
    -- A stored set's notes came off disk with it; the page's MIDI buffer holds
    -- some other session's chords and has no business describing this one.
    Just o -> fromMaybe [] (Array.index o.notes i)
    Nothing -> case takeZero st, Array.index st.regions i of
      Just t0, Just rg -> notesIn (believed st) t0 rg
      _, _ -> []

  -- | A voicing as you would say it: low to high, in register.
  saidAs ns = joinWith " " (map Pitch.noteName ns)

  -- | The chords struck inside region `i`, as chords. A stored set has them
  -- | flattened in `notes` and separate in `struck`; a live one is grouped
  -- | here by the same function that will write it.
  struckAt i = case st.opened of
    Just o -> case Array.index o.struck i of
      Just gs | not (Array.null gs) -> gs
      -- A set stored before strikes were recorded has only the flat list, and
      -- one chord is the honest reading of it.
      _ -> case fromMaybe [] (Array.index o.notes i) of
             [] -> []
             ns -> [ ns ]
    Nothing -> case takeZero st, Array.index st.regions i of
      Just t0, Just rg -> strikesIn (believed st) t0 rg
      _, _ -> []

  segment i r left wide =
    let
      kept = Set.member i st.keep
      w = witness i
      -- **Whether the sound had finished when this region did.** Shown on the
      -- object it is about rather than in a panel: a sample still sounding at
      -- its own end carries the front of the next one, and stitched into a
      -- layer it clicks at the join. See `settledFor`.
      still = case st.peaks of
        Nothing -> false
        Just pk ->
          maybe false (\x -> overlapping x { start: r.start, end: r.end })
            (Array.index
              (settledFor pk (max 0.001 (heldSecs st))
                 (map (\q -> { start: q.start, end: q.end }) st.regions)) i)
    in
      HH.div
        [ HP.class_ (HH.ClassName ("q-seg"
            <> (if kept then "" else " is-dropped")
            <> (if st.playing == Just i then " is-playing" else "")
            <> (if st.pivot == Just i then " is-open" else "")
            <> (if still then " is-ringing" else "")))
        , style ("left:" <> left r.start
                   <> ";width:" <> wide (max 0.0 (r.end - r.start))
                   <> (if kept then ";background:" <> w.tint else ""))
        , HP.title (w.label <> " — " <> fmt (r.end - r.start) <> " s at "
                      <> fmt r.start <> " s."
                      <> (case struckAt i of
                            [] -> ""
                            [ one ] -> " Played: " <> saidAs one <> "."
                            gs -> " Played: "
                                    <> joinWith " then " (map saidAs gs) <> ".")
                      <> (if still then ringingSays else "")
                      <> " Click for every parameter here.")
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
                        <> " — " <> cellSays steps i)
                  , HE.onClick \_ ->
                      if running then OpenPivot st.pivot
                      else OpenPivot (if st.pivot == Just i then Nothing else Just i)
                  ]
                  (planCell steps i))
              (if k <= 0 then [] else Array.range 0 (k - 1)))
        ]

  -- | **What this cell asks for, in the cell.**
  -- |
  -- | The note, then every other live parameter as its own slider, then what
  -- | Measure says this cell will take. Read across a row those are the values
  -- | the inner axis varies; read down a column, the outer's. A parameter on
  -- | the wrong axis shows as a row that repeats itself, which is the whole
  -- | reason the values are here rather than in a panel that has to be opened
  -- | one cell at a time.
  -- |
  -- | **Sliders rather than percentages** (2026-09-12). `C2 · 45%` is true and
  -- | it is the wrong representation for the question the grid is asked, which
  -- | is never "what is this one" but always "how do these forty-eight relate
  -- | to one another". Reading that off numerals means reading forty-eight of
  -- | them and holding the differences in your head; as a stack of little
  -- | tracks one per parameter, always in the same order and the same place,
  -- | a column of cells becomes a small-multiple and the shape of the sweep is
  -- | the shape on the screen. The numbers stay on hover, where a number is
  -- | what you want.
  planCell steps i =
    let
      means = maybe [] _.means (Array.index steps i)
      pitched = Array.find (\m -> m.note >= 0) means
      others = Array.filter (\m -> m.note < 0) means
      -- Only when the measurement still describes THIS sweep; a stale number
      -- in a cell is worse than none, because it reads as a fact.
      pacedFor =
        if Sweep.pacedStale st.sweep then Nothing
        else Array.index st.sweep.paced i
      -- Clamped because a curve may be edited past its ends while the grid is
      -- on screen, and a fill wider than its track reads as a different
      -- parameter rather than as an out-of-range one.
      slider m =
        HH.div [ HP.class_ (HH.ClassName "q-cellbar") ]
          [ HH.span
              [ HP.class_ (HH.ClassName "q-cellbar-f")
              , style ("width:" <> show (max 0.0 (min 100.0 (m.at * 100.0))) <> "%")
              ] []
          ]
    in
      Array.catMaybes
        [ Just (HH.span [ HP.class_ (HH.ClassName "q-seg-n") ]
            [ HH.text (maybe (show (i + 1)) (\m -> Pitch.noteName m.note) pitched) ])
        , if Array.null others then Nothing
          else Just (HH.div [ HP.class_ (HH.ClassName "q-cellbars") ]
                 (map slider others))
        , map
            (\ms -> HH.span [ HP.class_ (HH.ClassName "q-cellpaced") ]
                      [ HH.text (fmt (Int.toNumber ms / 1000.0) <> " s") ])
            pacedFor
        ]

  -- | The numbers the sliders stand for, for the cell's own tooltip. The
  -- | picture answers the comparison; this answers "what exactly is that one",
  -- | which is a different question and wants a different affordance.
  cellSays steps i =
    let means = maybe [] _.means (Array.index steps i)
    in joinWith " · "
         (map
           (\m ->
             if m.note >= 0 then Pitch.noteName m.note
             else m.name <> " " <> show (Int.round (m.at * 100.0)) <> "%")
           means)

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
      -- **A played chord names itself by its bass.** The band is 1/12th of a
      -- strip wide, so the whole voicing does not fit — and the bass plus the
      -- count is what distinguishes one of these from its neighbours at a
      -- glance, which is the question a row of them is asked. The full voicing
      -- is one hover away.
      played = playedAt i
    in
      case noted, Array.head played of
        Just n, _ ->
          { label: Pitch.noteName n
          , tint: hsl (Int.toNumber (n - lo) / Int.toNumber (hi - lo))
          }
        Nothing, Just b ->
          { label: Pitch.noteName b
              <> (if Array.length played > 1
                    then " \x00d7" <> show (Array.length played) else "")
          , tint: hsl (Int.toNumber (clamp lo hi b - lo) / Int.toNumber (hi - lo))
          }
        Nothing, Nothing ->
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
  setName = if st.name == "" then "set" else st.name

  keepBlock =
    HH.div [ HP.class_ (HH.ClassName "q-keep") ]
      [ HH.span [ HP.class_ (HH.ClassName "q-acthead") ] [ HH.text "Keep" ]
      -- A remark, not a guard: the rate is set on the interface and this page
      -- cannot change it, so all it can usefully do is not let it pass unsaid.
      -- Beside Keep rather than beside a card, because it is a fact about the
      -- recording and the recording is what is being committed here.
      , case rateSays st of
          Nothing -> HH.text ""
          Just t -> HH.p [ HP.class_ (HH.ClassName "q-rate") ] [ HH.text t ]
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
              , HP.disabled (st.cardBusy || Set.isEmpty st.keep || Maybe.isJust wontKeep)
              , HP.title "cut the kept pieces into a set, measure them, and write \
                         \the description beside them"
              , HE.onClick \_ -> AskKeep
              ]
              [ HH.text ("Keep " <> show (Set.size st.keep) <> " samples") ]
      -- | **A refusal belongs where the act is, not in the log.**
      -- |
      -- | Both guards answered by writing a line to the log at the foot of the
      -- | page and doing nothing else, so pressing Keep looked exactly like
      -- | keeping: the panel did not change, and the next thing to check was
      -- | forty lines below the button. On 2026-09-12 a take was believed
      -- | saved that the guard had correctly refused. Said here, ahead of the
      -- | press, with the button dead — a precondition rather than a verdict.
      , case wontKeep of
          Nothing -> HH.text ""
          Just why -> HH.span [ HP.class_ (HH.ClassName "q-warn") ] [ HH.text why ]
      -- | **What this set will NOT be able to say, said before it is written.**
      -- |
      -- | A played take's notes exist only in the page, only until the next
      -- | capture opens. Every other shortcoming of a set can be repaired
      -- | afterwards — re-cut it, re-measure it, rename it — and this one
      -- | cannot: the performance is over. So it is a remark at the moment of
      -- | saving, not a refusal, because a set of chords with no notes is still
      -- | a set of chords and you may not want them.
      -- |
      -- | It says it after the fact for the same reason `heardSays` says it
      -- | before: on 2026-09-12 twelve chords were saved with empty `notes` and
      -- | nothing on the page objected at either end.
      , case notesMissing of
          Nothing -> HH.text ""
          Just why -> HH.span [ HP.class_ (HH.ClassName "q-scratch") ] [ HH.text why ]
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

  -- | **Why this set will carry no notes**, or nothing. Only ever a remark.
  notesMissing
    | st.fill /= Played = Nothing
    | Maybe.isJust st.opened = Nothing
    | not st.midiOk =
        Just "this page has no Web MIDI (it needs a secure origin — try \
             \http://localhost:3029), so this set will say nothing about \
             \what was played. That cannot be added later."
    | Array.null st.midiIn =
        Just "no MIDI reached the page, so this set will say nothing about \
             \what was played. That cannot be added later."
    | st.sweep.notesFrom == "" =
        Just "no MIDI input was chosen, so this set will say nothing about \
             \what was played. Choose one above and keep it again — the \
             \notes are still held until the next take opens."
    | Array.null (believed st) =
        Just ("nothing arrived on " <> st.sweep.notesFrom <> ", so this set \
              \will say nothing about what was played.")
    | otherwise = Nothing

  -- | **Where a set lands on a card is not part of making it.**
  -- |
  -- | This used to be an "Export for card" door on the bench, holding the
  -- | letter, the bank, the kit, the voice and the two verbs — and every one
  -- | of those is a fact about a destination, not about a recording. Andrew,
  -- | 2026-09-12: *"the location on a card for a particular module isn't
  -- | something that's part of the sampling setup, it should be on the library
  -- | tab."* Which is right, and it is the cleaner seam as well: the bench
  -- | MAKES sets and the library PLACES them, so one set can be sent to a
  -- | Rample voice pair today and a QuadDrum folder tomorrow without the thing
  -- | that recorded it having an opinion.
  -- |
  -- | So the bench keeps one verb — Save to disk — and the address moved whole
  -- | to the Library's placement bar, where the sets being addressed are.
  -- | Duplicating it in both places is what made "which bank?" unanswerable
  -- | from the page you were standing on.

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
  -- | **Say it on the take, not only when you try to save it.**
  -- |
  -- | A measuring take is real audio, correctly divided, and indistinguishable
  -- | from a capture — which is how one got saved. What it is NOT is paced:
  -- | every cell got the flat spacing, so the short ones are mostly silence.
  dryTakeSays
    | not st.takeIsDry = HH.text ""
    | Array.null st.regions && not hasTake = HH.text ""
    | otherwise =
        HH.span [ HP.class_ (HH.ClassName "q-warn") ]
          [ HH.text "this is the measuring pass — flat spacing, so every sample \
                    \carries the trailing silence the measurement is for. \
                    \Press Record for the paced one." ]

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

  -- | **Regions that do not fit the take they are drawn on.**
  -- |
  -- | The regions live on the page and the audio lives in the daemon, so
  -- | anything that starts a new capture leaves the two describing different
  -- | recordings — and the page goes on drawing, cramming fifty seconds of
  -- | bands into six and leaving three rows of a grid empty. It looks like the
  -- | samples changed. Nothing changed; the picture is of two things.
  -- |
  -- | Measured 2026-09-12, and caused by my own diagnostics: `rig-alive.mjs`
  -- | captures through the daemon to read the input, which replaces whatever
  -- | take was being looked at.
  strayRegions =
    let lastEnd = fromMaybe 0.0 (map _.end (Array.last st.regions))
        held = heldSecs st
    in
      if Maybe.isJust st.opened || Array.null st.regions || held <= 0.0
           || lastEnd <= held + 0.5
        then HH.text ""
        else
          HH.span [ HP.class_ (HH.ClassName "q-warn") ]
            [ HH.text ("these bands run to " <> fmt lastEnd <> " s and the take \
                       \the daemon is holding is " <> fmt held <> " s — they \
                       \belong to a different recording, so what is drawn here \
                       \is two things at once. The samples on disk are not \
                       \affected; record again, or open the set from the \
                       \Library.") ]

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
            -- | **The act first, then what it will act on.**
            -- |
            -- | Andrew, 2026-09-12: the Write row sat under the table and the
            -- | plan, so on a card with any content it was below the fold —
            -- | and the one thing you came here to press was the one thing you
            -- | had to scroll for. The list can grow as long as it likes now;
            -- | it is underneath.
            | otherwise ->
                HH.div_
                  [ writeRow v
                  , HH.table [ HP.class_ (HH.ClassName "q-table") ]
                      [ HH.thead_ [ HH.tr_ (map (\h -> HH.th_ [ HH.text h ])
                          [ "bank", "kit", "voice", "holds" ]) ]
                      , HH.tbody_ (map row v.rows)
                      ]
                  , if v.plan == "" then HH.text ""
                    else HH.pre [ HP.class_ (HH.ClassName ("q-plan" <> if v.ok then "" else " is-bad")) ]
                           [ HH.text v.plan ]
                  ]
      , case st.confirmWrite of
          Nothing -> HH.text ""
          Just c -> writeModal c
      ]

  row r =
    HH.tr_
      [ HH.td_ [ HH.text r.bank ]
      , HH.td_ [ HH.text r.kit ]
      , HH.td_ [ HH.text (show r.voice <> (if r.stereo then " + " <> show (r.voice + 1) else "")) ]
      , HH.td_
          [ voicePicOf { layers: max 1 (Array.length r.sets), slices: r.slicer, dim: false }
              (Array.nub r.sets)
          , HH.div [ HP.class_ (HH.ClassName "q-holdsays") ]
              [ HH.text (joinWith ", " (Array.nub r.sets)
                  <> (if r.stereo then " · stereo" else "")
                  <> (if r.mode == "" || r.mode == "manual" then ""
                      else " · " <> r.mode)) ]
          ]
      ]

  -- | **The samples, as they were made.**
  -- |
  -- | One box per sample, on the grid the sweep laid them out on. No
  -- | waveform: the question a library is asked is "which one was this", and
  -- | forty-eight thumbnails answer it worse than one picture of the shape —
  -- | a 4 × 12 reads as four passes of twelve at a glance, and the sentence
  -- | for it ("decay, oct over 4 × 12") has to be parsed before it means
  -- | anything.
  -- |
  -- | The rows are banded because the outer axis is what a layer will become,
  -- | and seeing the bands here is what makes the arrangement beside it
  -- | legible as a rearrangement of these.
  samplePic r =
    let
      n = r.count
      cols = case r.extent of
        [ _, inner ] | inner > 0 -> inner
        _ -> min 16 (max 1 n)
      rows = max 1 ((n + cols - 1) / cols)
    in
      HH.div [ HP.class_ (HH.ClassName "q-spic")
             , HP.title (show n <> " samples"
                 <> (if Array.null r.extent then ""
                     else " on " <> joinWith " × " (map show r.extent))) ]
        (map
          (\y -> HH.div [ HP.class_ (HH.ClassName ("q-srow" <> if y `mod` 2 == 1 then " is-odd" else "")) ]
            (map (\x -> sampleCell r (y * cols + x))
              (Array.range 0 (min cols (n - y * cols) - 1))))
          (Array.range 0 (rows - 1)))

  -- | **One sample. Hover it and hear it; click it and read it.**
  -- |
  -- | The row used to carry a date, a sentence, a line of pattern code and
  -- | three buttons, none of which answers "which one was this" as fast as
  -- | playing it does. So the picture takes the verbs: the boxes ARE the
  -- | audition, one sample at a time rather than three spread across the set,
  -- | and everything that was written out goes behind a click.
  sampleCell r i =
    HH.div
      [ HP.class_ (HH.ClassName ("q-scell is-" <> sampleClass r i
          <> (if secsOf r i <= 0.0 then " is-unmeasured" else "")
          <> (if overScale (secsOf r i) then " is-over" else "")))
      , HP.attr (HH.AttrName "style")
          (widthOf (secsOf r i) <> fromMaybe "" (map (\s -> "; " <> s) (pitchTint (noteOf r i))))
      , HP.title (show (i + 1)
          <> (case noteOf r i of
                nn | nn >= 0 -> " \x00b7 " <> noteName nn
                _ -> "")
          <> (case secsOf r i of
                d | d > 0.0 -> " \x00b7 " <> secs2 d <> "s"
                _ -> " \x00b7 length unknown"))
      , HE.onMouseEnter \_ -> HearOne r.name i
      , HE.onClick \_ -> PeekSet (Just r.name)
      ]
      []

  noteOf r i = fromMaybe (-1) (Array.index r.notes i)
  secsOf r i = fromMaybe 0.0 (Array.index r.secs i)

  -- | **Three classes, all declared, none inferred.**
  -- |
  -- | A sample carries a pitch when the sweep set one, and that is per-sample
  -- | and exact. A sample is a chord when the TAKE was chord-hits, which is
  -- | per-set and was declared at the moment of recording — and is the only
  -- | reliable statement of it, since `samples[].notes` is carrying something
  -- | that is not note-ons. Everything else is neither, and says so by being
  -- | neither.
  -- |
  -- | Deliberately not measured. `decay`, `zcr` and `tilt` would let the page
  -- | GUESS that one sample is a hit and another a wash, and a half-built
  -- | shape inferred from its own state is exactly what this project keeps
  -- | having to unpick.
  sampleClass r i
    | noteOf r i >= 0 = "pitch"
    | r.kind == "chord-hits" = "chord"
    | otherwise = "plain"

  -- | **The first transformation: what these samples BECOME.**
  -- |
  -- | Separate from where they go, and asked first, because it is the
  -- | decision the module actually plays. The same forty-eight files are four
  -- | alternatives of twelve positions, or two of twenty-four, or one file of
  -- | forty-eight — and the two axes are not interchangeable: **layers are
  -- | alternatives the module picks between** and **slices are positions only
  -- | you can reach**.
  -- |
  -- | It was a two-option dropdown, and for any set with two axes it was a
  -- | no-op: the server read the sweep's extent and never consulted it. So
  -- | the arrangement was being decided by how the recording happened to be
  -- | swept, which is a fact about the take and not about the instrument it
  -- | is going to.
  arrangePicker =
    case Array.fromFoldable st.picked of
      [ one ] -> case Array.find (\r -> r.name == one) st.sets of
        Just r | Array.length (Http.arrangementsOf r.count) > 1 ->
          HH.div [ HP.class_ (HH.ClassName "q-arrange") ]
            [ HH.span [ HP.class_ (HH.ClassName "q-arrangelab") ] [ HH.text "becomes" ]
            , HH.div [ HP.class_ (HH.ClassName "q-arrangeopts") ]
                (map (opt r) (Http.arrangementsOf r.count))
            ]
        _ -> HH.text ""
      -- Several ticked, each its own kit: they need not share an arrangement
      -- and there is no single picture to draw. The sweep's own extent
      -- decides for each, which is what it did before any of this.
      _ -> HH.text ""
    where
    -- The sweep's own grouping, which is the natural one and keeps the layer
    -- names: a layer stands for a value of the outer parameter, and a
    -- regrouping stands for a position and nothing else.
    natural r = case r.extent of
      [ outer, _ ] -> outer
      _ -> 0
    opt r a =
      HH.button
        [ HP.class_ (HH.ClassName ("q-arrangeopt"
            <> (if chosen r a then " is-on" else "")
            <> (if a.layers == natural r then " is-natural" else "")))
        , HP.title (says a <> (if a.layers == natural r
                                 then " — how it was swept, so the layers keep \
                                      \their parameter values"
                                 else " — regrouped, so the layers carry positions \
                                      \rather than values"))
        , HE.onClick \_ -> SetPlaceLayers a.layers
        ]
        [ voicePic { layers: a.layers, slices: a.slices, dim: false }
        , HH.span [ HP.class_ (HH.ClassName "q-arrangesays") ] [ HH.text (says a) ]
        ]
    -- Zero is "let the extent decide", and the extent decides the natural
    -- one — so the natural option is what zero is showing.
    chosen r a = if st.placeLayers == 0 then a.layers == natural r
                 else st.placeLayers == a.layers
    says a
      | a.slices == 0 = show a.layers <> " layers"
      | a.layers == 1 = "1 file, " <> show a.slices <> " slices"
      | otherwise = show a.layers <> " × " <> show a.slices

  -- | **A set\'s own page.**
  -- |
  -- | Everything the row used to carry: the date, what moved, the pattern
  -- | line, the verbs — plus what it never could, because it costs a header
  -- | read. None of it is consulted while you are LOOKING for a set and all
  -- | of it is once you have found one, which is the whole case for a door.
  setModal nm =
    case Array.find (\r -> r.name == nm) st.sets of
      Nothing -> HH.text ""
      Just r ->
        HH.div [ HP.class_ (HH.ClassName "q-scrim") ]
          [ HH.div [ HP.class_ (HH.ClassName "q-modal is-set") ]
              [ HH.div [ HP.class_ (HH.ClassName "q-modalhead") ]
                  [ HH.h2_ [ HH.text nm ]
                  , HH.button
                      [ HP.class_ (HH.ClassName "q-plain")
                      , HE.onClick \_ -> PeekSet Nothing ]
                      [ HH.text "done" ]
                  ]
              , HH.div [ HP.class_ (HH.ClassName "q-setpage") ]
                  [ HH.div [ HP.class_ (HH.ClassName "q-setpic") ] [ samplePic r ]
                  , HH.dl [ HP.class_ (HH.ClassName "q-facts") ]
                      ( fact "made" (String.take 10 r.made)
                      <> fact "from take" r.take
                      <> fact "samples"
                          (show r.count
                            <> (if Array.null r.extent then ""
                                else " on " <> joinWith " × " (map show r.extent)))
                      <> fact "lengths" (lengthsSays r)
                      <> fact "moved" (joinWith ", " r.moved)
                      <> fact "encoding" r.encoding
                      <> fact "channels" (if r.stereo then "stereo — takes a voice pair"
                                          else "mono")
                      <> audioFacts
                      )
                  , pitchSet r
                  -- Every set is already a SuperDirt bank, whatever it was
                  -- recorded for — a folder of numbered files is a named,
                  -- indexed set at both ends. Said for all of them, because
                  -- that is the finding.
                  , if dirt r == "" then HH.text ""
                    else HH.div_
                      [ HH.div [ HP.class_ (HH.ClassName "q-factlab") ]
                          [ HH.text "in a pattern" ]
                      , HH.code [ HP.class_ (HH.ClassName "q-set-dirt") ] [ HH.text (dirt r) ]
                      ]
                  , HH.div [ HP.class_ (HH.ClassName "q-set-do") ] (setVerbs r)
                  ]
              ]
          ]
    where
    fact k v = if v == "" then [] else
      [ HH.dt_ [ HH.text k ], HH.dd_ [ HH.text v ] ]
    -- | **The lengths, in seconds, for sanity.** The shortest and the longest
    -- | bound the picture, and the longest is also the SLOT every slice in a
    -- | layer has to fit — so it is the number that decides what a sliced
    -- | arrangement wastes. The total says whether a set is minutes or
    -- | seconds, which is the difference between a bank of hits and a drone.
    lengthsSays r =
      let ds = Array.filter (_ > 0.0) r.secs
      in case Array.head (Array.sort ds), Array.last (Array.sort ds) of
        Just lo, Just hi ->
          (if lo == hi then secs2 hi <> "s each"
           else secs2 lo <> "\x2013" <> secs2 hi <> "s")
            <> " \x00b7 " <> secs2 (Array.foldl (+) 0.0 ds) <> "s in all"
            <> (if Array.length ds == r.count then ""
                else " \x00b7 " <> show (r.count - Array.length ds) <> " unmeasured")
        _, _ -> if r.count == 0 then "" else "unmeasured"
    audioFacts = case st.openSetInfo of
      Nothing -> fact "the files" "reading…"
      Just i | i.audio.rate > 0 ->
        fact "the files"
          (show i.audio.rate <> " Hz · " <> show i.audio.bits <> " bit · "
            <> (if i.audio.channels == 1 then "mono" else show i.audio.channels <> " ch")
            -- 0xFFFE is EXTENSIBLE, which the Arbhar refuses silently. `msm`
            -- canonicalises on the way to a card; this says what is on disk.
            <> (if i.audio.tag == 65534 then " · EXTENSIBLE header" else ""))
      _ -> []
    -- | **Which pitches this set holds**, in their own colours.
    -- |
    -- | The distinct notes rather than one per sample: forty-eight samples of
    -- | twelve pitches is a twelve-note set played four times, and saying
    -- | "twelve pitches, C2 to B2" is what a person then checks the picture
    -- | against.
    pitchSet r =
      let ps = Array.sort (Array.nub (Array.filter (_ >= 0) r.notes))
      in if Array.null ps then HH.text "" else
        HH.div_
          [ HH.div [ HP.class_ (HH.ClassName "q-factlab") ]
              [ HH.text (show (Array.length ps) <> " pitches") ]
          , HH.div [ HP.class_ (HH.ClassName "q-pitchset") ]
              (map (\n -> HH.span
                      [ HP.class_ (HH.ClassName "q-pitch")
                      , maybe (HP.attr (HH.AttrName "data-none") "")
                          (HP.attr (HH.AttrName "style")) (pitchTint n)
                      ]
                      [ HH.text (noteName n) ]) ps)
          ]

  -- | **The verbs, behind the click.**
  -- |
  -- | Four of them, on every row, was a column of buttons a library of forty
  -- | sets repeated forty times — and the one you reach for depends on which
  -- | set it is, which is what the row is for finding out.
  setVerbs r =
    [ if r.count > 0
            then HH.button
                   [ HP.class_ (HH.ClassName "q-plain is-hear")
                   , HP.title "three of its samples, a second each — first, \
                              \middle and last, which on a grid spans the \
                              \outer axis"
                   , HE.onClick \_ -> HearSet r.name r.count
                   ]
                   [ HH.text "\x266a" ]
            else HH.text ""
        , if r.described && r.count > 0
            then HH.button
                   [ HP.class_ (HH.ClassName "q-plain")
                   , HP.disabled st.busy
                   , HP.title "put it back on the bench, drawn on the take it \
                              \was cut from — nothing is re-cut or re-measured"
                   , HE.onClick \_ -> OpenSet r.name
                   ]
                   [ HH.text "Open" ]
            else HH.text ""
        , if r.runnable
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

  -- | **The bank letters, as twenty-six cells.**
  -- |
  -- | This was a text field you typed a letter into and a sentence elsewhere
  -- | listing the free ones — so choosing where a set goes meant reading a
  -- | string of twenty-three characters and checking your letter against it,
  -- | for the one decision on this page that destroys things. **The letter is
  -- | the blast radius**: a write deletes the whole slot it lands on, and a
  -- | letter chosen by accident once cost a bank of Squarp's own content.
  -- |
  -- | Three states, and they are genuinely different: free, held by this
  -- | manifest, and held by somebody else on the card. The third is the one
  -- | worth seeing without asking for it.
  letterStrip =
    HH.div [ HP.class_ (HH.ClassName "q-field is-tight q-letters") ]
      [ HH.span_ [ HH.text "letter" ]
      , HH.div [ HP.class_ (HH.ClassName "q-strip") ]
          (map cell alphabet)
      ]
    where
    alphabet = String.split (String.Pattern "")
      "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    -- Letters this manifest is already using. From the card view rather than
    -- from the peek, because the manifest exists whether or not a card is
    -- mounted — which is most of the time.
    ours = Array.nub (map _.letter (maybe [] _.rows st.cardView))
    -- What is on the card and is not ours. `free` already excludes the
    -- manifest's own letters, so anything neither free nor ours is somebody
    -- else's — and with no card read, nothing is known and nothing is shown.
    freeOn = map _.free st.cardPeek
    cell c =
      let
        mine = Array.elem c ours
        taken = case freeOn of
          Nothing -> false
          Just f -> not mine && String.contains (String.Pattern c) f == false
      in
        HH.button
          [ HP.class_ (HH.ClassName ("q-letter"
              <> (if c == st.letter then " is-on" else "")
              <> (if mine then " is-mine" else "")
              <> (if taken then " is-taken" else "")))
          , HP.title (c <> (if mine then " — this manifest writes here"
                            else if taken then " — already on the card; writing \
                                               \here deletes what is in it"
                            else case freeOn of
                                   Nothing -> ""
                                   Just _ -> " — free"))
          , HE.onClick \_ -> SetLetter c
          ]
          [ HH.text c ]

  -- | **What a voice holds, drawn.**
  -- |
  -- | The two axes of a Rample voice are the two things the page kept saying in
  -- | words and the two things you cannot hold in your head together: **layers
  -- | are alternatives the module picks between** (the layer CV, or velocity, or
  -- | random) and **slices are positions only you can reach** (the start point,
  -- | one CC per voice). A voice of four layers of twelve slices is a 4 × 12
  -- | grid, and the sentence for it — "bia oct x decay — one file, SLICER 12" —
  -- | said neither number in a way that could be compared with the voice below
  -- | it.
  -- |
  -- | So: a row per layer, a cell per slice. A plain stack of layers is one
  -- | column, which is honest — it has no positions — and a sliced voice is one
  -- | row of many, which is equally honest. The picture and the grid a set was
  -- | swept on are deliberately the same shape, because for most sets here they
  -- | are the same grid.
  -- |
  -- | `dim` draws what is ABOUT TO GO rather than what lands. Same shape, no
  -- | ink: the comparison is the whole point of showing it.
  voicePic o = voicePicOf o []

  -- | **The same picture, made of the same samples.**
  -- |
  -- | `from` is the sets this voice is built out of, in order, so the card
  -- | side is drawn in the material\'s own colours and lengths rather than in
  -- | anonymous ink. It is the point of the whole encoding: you arrange the
  -- | boxes on the left and see THOSE boxes land on the right, so a 4 × 12
  -- | that keeps its four runs and a 1 × 48 that strings them end to end are
  -- | two pictures of the same material and can be told apart at a glance.
  -- |
  -- | With no sets named it falls back to plain cells, which is right for a
  -- | slot already on the card: those files were written by somebody else and
  -- | nothing here knows what is in them.
  voicePicOf o from =
    HH.div
      [ HP.class_ (HH.ClassName ("q-vpic" <> if o.dim then " is-dim" else ""))
      , HP.title (show o.layers <> " layer" <> (if o.layers == 1 then "" else "s")
          <> (if cols > 1 then " of " <> show cols <> " slices" else ""))
      ]
      (map layerRow (Array.range 0 (max 1 (min 12 o.layers) - 1)))
    where
    -- Twelve is the module's ceiling for layers and the picture stops there for
    -- the same reason the build does: a thirteenth never plays.
    cols = max 1 o.slices
    -- Every sample of every set named, end to end — which is the order the
    -- arrangement lays them out in, layer by layer.
    pool = do
      nm <- from
      r <- Array.filter (\x -> x.name == nm) st.sets
      Array.range 0 (r.count - 1) <#> \i -> { r, i }
    layerRow y =
      HH.div [ HP.class_ (HH.ClassName "q-vrow") ]
        (map (\x -> cell (y * cols + x)) (Array.range 0 (cols - 1)))
    cell k = case Array.index pool k of
      Nothing -> HH.div [ HP.class_ (HH.ClassName "q-vcell") ] []
      Just { r, i } ->
        HH.div
          [ HP.class_ (HH.ClassName ("q-vcell is-" <> sampleClass r i
              <> (if overScale (secsOf r i) then " is-over" else "")))
          , HP.attr (HH.AttrName "style")
              (widthOf (secsOf r i)
                <> fromMaybe "" (map (\s -> "; " <> s) (pitchTint (noteOf r i))))
          , HP.title (r.name <> " " <> show (i + 1)
              <> (case noteOf r i of
                    nn | nn >= 0 -> " \x00b7 " <> noteName nn
                    _ -> "")
              <> (case secsOf r i of
                    d | d > 0.0 -> " \x00b7 " <> secs2 d <> "s"
                    _ -> ""))
          ]
          []

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
                         , HP.title ("read " <> c <> ", then say what writing to it would do")
                         , HE.onClick \_ -> AskWrite (Just c)
                         ]
                         [ HH.text c ]) v.cards)
      ]

  -- | **What writing to this card would do, before it does it.**
  -- |
  -- | The write used to be a button and a guess. `--overwrite` deletes each
  -- | kit slot's whole directory, and without it a single occupied slot
  -- | refuses the entire write — so both answers were wrong to give blind, and
  -- | the refusal arrived as the last line of a build log at the foot of the
  -- | page. On 2026-09-12 that cost an evening: a write of two free banks died
  -- | on a third slot that was already there, and the page showed nothing that
  -- | said so.
  -- |
  -- | So the card is read first, and every slot says which of the three things
  -- | happens to it. The rows the plan does not touch are listed too: a view
  -- | of only our own corner offers a bank letter that is taken, and a letter
  -- | chosen by accident once cost a bank of Squarp's own content.
  writeModal c =
    HH.div [ HP.class_ (HH.ClassName "q-scrim") ]
      [ HH.div [ HP.class_ (HH.ClassName "q-modal is-preview") ]
          [ HH.div [ HP.class_ (HH.ClassName "q-modalhead") ]
              [ HH.h2_ [ HH.text ("Writing to " <> c) ]
              , HH.button
                  [ HP.class_ (HH.ClassName "q-plain")
                  , HE.onClick \_ -> AskWrite Nothing ]
                  [ HH.text "cancel" ]
              ]
          , case st.preview of
              Nothing ->
                HH.p [ HP.class_ (HH.ClassName "q-muted") ]
                  [ HH.text "reading the card…" ]
              Just pv -> previewPanel c pv
          ]
      ]

  previewPanel c pv =
    HH.div [ HP.class_ (HH.ClassName "q-preview") ]
      [ if pv.unreadable == "" then HH.text ""
        else HH.p [ HP.class_ (HH.ClassName "q-warn") ]
               [ HH.text ("the card is mounted and cannot be read, so nothing here \
                          \can be trusted: " <> pv.unreadable) ]
      , if pv.output == "" then HH.text ""
        else HH.p [ HP.class_ (HH.ClassName "q-muted") ] [ HH.text pv.output ]
      , HH.div_ (map previewSlot pv.slots)
      , if Array.null pv.problems then HH.text ""
        else HH.div [ HP.class_ (HH.ClassName "q-plan is-bad") ]
               [ HH.text (joinWith "\n" pv.problems) ]
      -- The compiler's own asides — what SLICER wants setting to, which kits
      -- answer on which triggers. Worth saying and not worth refusing over,
      -- and this is the last moment anyone reads them before the card is in
      -- the module.
      , if Array.null pv.notes then HH.text ""
        else HH.details [ HP.class_ (HH.ClassName "q-notes") ]
               [ HH.summary_ [ HH.text (show (Array.length pv.notes) <> " things worth knowing") ]
               , HH.div [ HP.class_ (HH.ClassName "q-plan") ]
                   [ HH.text (joinWith "\n" pv.notes) ]
               ]
      , if pv.free == "" then HH.text ""
        else HH.p [ HP.class_ (HH.ClassName "q-muted") ]
               [ HH.text ("free bank letters on this card: " <> pv.free) ]
      , HH.div [ HP.class_ (HH.ClassName "q-send") ]
          [ HH.button
              [ HP.class_ (HH.ClassName ("q-plain" <> if replacing then " is-replacing" else ""))
              , HP.disabled (st.cardBusy || not pv.ok)
              , HE.onClick \_ -> WriteCard c replacing
              ]
              [ HH.text proceed ]
          , HH.button
              [ HP.class_ (HH.ClassName "q-plain")
              , HE.onClick \_ -> AskWrite Nothing ]
              [ HH.text "cancel" ]
          ]
      ]
    where
    -- **Replacing is decided by the card, not by which button was pressed.**
    -- The old page had two buttons and asked the person to know which applied;
    -- the card knows, and a collision is the only thing `--overwrite` is for.
    replacing = not (Array.null pv.collisions)
    proceed
      | not pv.ok = "the manifest will not build"
      | replacing = "Delete and rewrite " <> joinWith ", " pv.collisions
                      <> ", and write the rest"
      | otherwise = "Write " <> show (Array.length (Array.filter (\s -> s.fate == "create") pv.slots))
                      <> " kits"

  -- | One slot, both sides of it. DropSync's shape: what it is now above what
  -- | it becomes, and a line saying which of the two is at risk.
  previewSlot s =
    HH.div [ HP.class_ (HH.ClassName ("q-slot is-" <> s.fate)) ]
      [ HH.div [ HP.class_ (HH.ClassName "q-slotname") ]
          [ HH.strong_ [ HH.text s.slot ]
          , HH.span [ HP.class_ (HH.ClassName "q-fate") ] [ HH.text (fateSays s) ]
          ]
      , case Http.fateOf s.fate of
          Http.Keep ->
            HH.div [ HP.class_ (HH.ClassName "q-swap") ]
              [ HH.div [ HP.class_ (HH.ClassName "q-side") ]
                  [ voicePic { layers: s.thereFiles, slices: 0, dim: true }
                  , HH.div [ HP.class_ (HH.ClassName "q-sidesays") ]
                      [ HH.text (show s.thereFiles <> " files, untouched") ]
                  ]
              ]
          -- | **Both ends of it, side by side and the same shape.**
          -- |
          -- | Which is the one thing the sentences could not do. "deletes 1
          -- | file" and "4 files, SLICER /12" are two facts of the same kind
          -- | in two different units, and a person has to convert both into a
          -- | picture before they can be compared — so the page draws the
          -- | picture instead. The left is greyed because it is going.
          _ ->
            HH.div [ HP.class_ (HH.ClassName "q-swap") ]
              [ if s.thereFiles == 0
                  then HH.div [ HP.class_ (HH.ClassName "q-side is-empty") ]
                         [ HH.div [ HP.class_ (HH.ClassName "q-sidesays") ]
                             [ HH.text "empty" ] ]
                  else HH.div [ HP.class_ (HH.ClassName "q-side") ]
                         [ voicePic { layers: s.thereFiles, slices: 0, dim: true }
                         -- Named, never counted: two takes of one morning
                         -- differ by four characters in the middle, and this
                         -- is the last moment anyone can notice.
                         , HH.div [ HP.class_ (HH.ClassName "q-gone") ]
                             [ HH.text (joinWith ", " s.thereNames) ]
                         ]
              , HH.div [ HP.class_ (HH.ClassName "q-becomes") ] [ HH.text "\x2192" ]
              , HH.div [ HP.class_ (HH.ClassName "q-side") ]
                  [ voicePicOf { layers: s.files, slices: s.slots, dim: false }
                      (slotSets s.slot)
                  , HH.div [ HP.class_ (HH.ClassName "q-sidesays") ]
                      [ HH.text (s.name
                          <> (if s.slots > 0 then " \x00b7 SLICER /" <> show s.slots else "")) ]
                  , HH.div [ HP.class_ (HH.ClassName "q-muted") ] [ HH.text s.settings ]
                  ]
              ]
      ]

  -- | **Which sets a planned slot is made of.**
  -- |
  -- | The survey is `msm`\'s and knows nothing about sets — it reports files
  -- | and slots. The manifest does know, so the join runs through it: a slot
  -- | is a letter and a kit index, and the card view carries both on every
  -- | voice row along with the set names on it. That is what lets the write
  -- | preview be drawn in the material\'s own colours rather than in ink.
  slotSets slot = Array.nub do
    v <- Array.fromFoldable st.cardView
    r <- Array.filter (\x -> x.letter <> show x.kitIx == slot) v.rows
    r.sets

  fateSays s = case Http.fateOf s.fate of
    Http.Create -> "new"
    Http.Replace -> "REPLACES what is there"
    Http.Keep -> "not ours"
    -- A build of `msm` that knows a fate this page does not. Named rather
    -- than guessed: the mild reading of an unknown verdict is the dangerous
    -- one.
    Http.Unknown w -> "unrecognised (" <> w <> ")"

  kindBtn k =
    HH.button
      [ HP.class_ (HH.ClassName ("q-kind" <> if Kind.name k == Kind.name st.kind then " on" else ""))
      , HP.disabled (st.armed || writing)
      , HE.onClick \_ -> PickKind (case k of
                                     Kind.Bars _ -> Kind.Bars st.bars
                                     other -> other)
      ]
      [ HH.text (Kind.label k) ]
