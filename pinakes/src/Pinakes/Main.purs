-- | **Pinakes: the sets kept, and getting them onto a module.**
-- |
-- | Named for Callimachus's catalogue of the Library of Alexandria, the first
-- | library catalogue we know of; a *pinax* is a tablet, a board you write
-- | onto, which is what a card is.
-- |
-- | Split out of Quadrat on 2026-11-09 (`docs/kb/plans/quadrat-recording-views.md`):
-- | **Quadrat makes sets and Pinakes places them.** Quadrat saves a set to
-- | `~/.itajara/quadrat/samples` and never reads a card; Pinakes reads the
-- | sample store and the cards and owns every write to a module. Opening a
-- | set on the Bench, or running its transect again, is a link back into
-- | Quadrat (`quadrat.html?open=` / `?again=`). No live rig is needed here.
module Pinakes.Main where

import Prelude

import Control.Promise (toAffE)
import Data.Array as Array
import Data.Either (Either(..), either)
import Data.Foldable (for_, sum)
import Data.Int as Int
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Number as Number
import Data.Set (Set)
import Data.Set as Set
import Data.String as String
import Data.String.Common (joinWith)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (attempt)
import Effect.Aff as Aff
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import Pinakes.Dest as Dest
import Quadrat.Audio as Audio
import Quadrat.Clip (copyText)
import Quadrat.Http as Http
import Quadrat.RebusView as RebusView
import Quadrat.SetIdentity (markOf)
import Quadrat.Stave as Stave
import Quadrat.Tidal as Tidal

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI component unit body

-- | One entry in the card plan: a set of ours, and what we know about the reel
-- | it makes. `reel` is `Nothing` only while the facts are still being read.
type PlanItem = { name :: String, reel :: Maybe Http.Reel }

type State =
  { log :: Array String
  -- | **The sets on disk**, newest first, read from the server: a set
  -- | outlives the page, which is the point of storing it.
  , sets :: Array Http.SetRow
  -- | The virtual card, as the server flattens it, plus what `kit build` says
  -- | about it. Refreshed after anything that could change it.
  , cardView :: Maybe Http.CardView
  -- | Where a set is going: the bank's legend, the kit, and which voice.
  , bank :: String
  -- | **Which bank LETTER this lands on**, which is the blast radius: a write
  -- | deletes the slot's whole directory first. `bank` is only the legend
  -- | printed on the bank; the two were one field until 2026-09-12, when a
  -- | 4 x 12 went onto `A` over Squarp's own content.
  , letter :: String
  , kit :: String
  -- | Whether a person typed the kit name, rather than a write adopting one.
  , kitMine :: Boolean
  , voice :: Int
  , layerMode :: String
  , cardBusy :: Boolean
  -- | **Which stored sets are ticked**, by name: the list is re-fetched after
  -- | every write and a position means nothing across that.
  , picked :: Set String
  -- | Deleting is the one thing here that destroys work, so it is asked.
  , confirmDrop :: Boolean
  -- | **The write question, held open against one card**, and the card's
  -- | answer when it arrives (`Nothing` while it is being read).
  , confirmWrite :: Maybe String
  , preview :: Maybe Http.Preview
  -- | **The mounted card, read without being asked**, when exactly one is
  -- | mounted, so the letter strip shows which banks are taken while you
  -- | choose one.
  , cardPeek :: Maybe Http.Preview
  -- | **Which destination the right-hand pane describes.** See `Pinakes.Dest`.
  , dest :: Dest.Dest
  -- | How the listing is ordered, the free-text filter, and ticked-only.
  , setSort :: Sort
  , setQuery :: String
  , onlyPicked :: Boolean
  -- | **What the ticked set would become as a Morphagene reel**, the mounted
  -- | volumes, the volume it goes to and which of its slots are taken.
  , reel :: Maybe Http.Reel
  , volumes :: Array String
  , reelDest :: Maybe String
  , reelTaken :: Array Int
  -- | **The Morphagene card as an ordered list**, not a set of addresses: the
  -- | module compacts on load, so names are derived from position when the
  -- | card is written, which is the only moment they are true.
  , reelPlan :: Array PlanItem
  , reelBusy :: Boolean
  -- | **The QuadDrum card as staged**, how a stereo set folds to mono, whether
  -- | the next folder is `_RANDOM`, and the volume it is copied to.
  , qd :: Maybe Http.QdCard
  , qdFold :: String
  , qdRandom :: Boolean
  , qdDest :: Maybe String
  -- | **Which set's own page is open**, its detail when it arrives, and which
  -- | of its samples is being read.
  , openSet :: Maybe String
  , openSetInfo :: Maybe Http.StoredSet
  , peekSample :: Int
  -- | **How a placed set is laid out**: one sliced file or a stack of layers,
  -- | the arrangement as layers over voices (0 lets the set decide), and
  -- | whether it stands beside what is on the voice or takes its place.
  , placeSliced :: Boolean
  , placeLayers :: Int
  , placeVoices :: Int
  , placeAppend :: Boolean
  }

-- | **How the listing is ordered.**
-- |
-- | A control, and its whole reason for existing is that the rows stopped
-- | printing their date. Andrew, 2026-09-16: *"maybe just not show timestamp
-- | at all (except in the detail modal) and instead give a sort by date
-- | option"* — which is the right trade twice over. The date was three
-- | near-identical strings per row answering a question nobody asks while
-- | SCANNING (*when exactly was this?*) and failing to answer the one they do
-- | (*which of these is the recent one?*). An ordering answers the second and
-- | costs no ink at all.
data Sort = Newest | Oldest | Biggest | Alpha

derive instance Eq Sort

-- | `a` through `w` \x2014 the letters reels 10 to 32 wear. Twenty-three of them,
-- | and the run stopping at `w` is the module's, not a typo: nine digits plus
-- | twenty-three letters is the thirty-two reels a card holds.
reelLetters :: Array String
reelLetters = String.split (String.Pattern "") "abcdefghijklmnopqrstuvw"

sortOf :: String -> Sort
sortOf = case _ of
  "oldest" -> Oldest
  "biggest" -> Biggest
  "alpha" -> Alpha
  _ -> Newest

sortValue :: Sort -> String
sortValue = case _ of
  Newest -> "newest"
  Oldest -> "oldest"
  Biggest -> "biggest"
  Alpha -> "alpha"

sortChoices :: Array { value :: String, says :: String }
sortChoices =
  [ { value: "newest", says: "newest first" }
  , { value: "oldest", says: "oldest first" }
  , { value: "biggest", says: "most samples" }
  , { value: "alpha", says: "by name" }
  ]


data Action
  = Init
  | RefreshCard
  | RefreshSets
  -- | Which destination the right-hand pane describes. See `Pinakes.Dest`.
  | SetDest Dest.Dest
  -- | The listing: free text over it, ticked-only, and its order.
  | SetQuery String
  | SetOnlyPicked Boolean
  | SetSort String
  -- | Tick or untick one stored set; tick all or none.
  | PickSet String
  | PickAllSets Boolean
  | AskDrop Boolean
  | DropPicked
  -- | Open a set's own page, or shut it, and read one of its samples.
  | PeekSet (Maybe String)
  | PeekSample Int
  -- | Hear a set (first, middle, last), or one sample of it.
  | HearSet String Int
  | HearOne String Int
  -- | **Declare the key centre of a chord set**, or clear it: root and intent
  -- | in one act, so a half-applied pair cannot exist.
  | DeclareCentre String Http.Centre
  -- | **Say how a set's chords were played**: as blocks, or arpeggiated.
  | DeclareArp String Boolean
  -- | Rample: where a set lands, how it is laid out, and placing it.
  | SetBank String
  | SetLetter String
  | SetKit String
  | SetVoice String
  | SetLayerMode String
  | SetPlaceSliced Boolean
  | SetPlaceLayers Int
  -- | **Pick a whole arrangement**: voices and layers travel together.
  | SetPlaceWay { voices :: Int, layers :: Int }
  | SetPlaceAppend Boolean
  | PlaceSet String
  -- | Every ticked set onto the card, each as its own kit.
  | PlacePicked
  -- | Hold the write question open against a card (which reads the card), or
  -- | drop it; the card's answer; and the write, whose flag is `--overwrite`.
  | AskWrite (Maybe String)
  | Previewed Http.Preview
  | WriteCard String Boolean
  -- | Send to · tape: the set's take, projected whole. See `Http.projectTape`.
  | SendTape String
  -- | Send to · Arbhar: the set onto the mounted stick.
  | SendArbhar String
  -- | Morphagene: the reel plan, and writing it to a card.
  | IntoReel String
  | LookAtReel
  | SetReelDest String
  | PlanAdd
  | PlanDrop String
  | PlanUp String
  | PlanDown String
  | PlanClear
  | WriteReelCard
  -- | QuadDrum: the staged card, and copying it whole.
  | SendQd String
  | QdLook
  | QdFold String
  | QdRandom Boolean
  | QdDrop String
  | QdDest String
  | QdWrite Boolean
  -- | **Put a transferable line on the clipboard.**
  | Copy String

component :: forall q i o m. MonadAff m => H.Component q i o m
component = H.mkComponent
  { initialState: \_ ->
      { log: []
      , cardView: Nothing, bank: "WORKSHOP", letter: "", kit: "", kitMine: false, voice: 1
      , layerMode: "", cardBusy: false
      , sets: [], picked: Set.empty, confirmDrop: false, confirmWrite: Nothing, preview: Nothing
      , dest: Dest.Rample, setQuery: "", onlyPicked: false, setSort: Newest
      , reel: Nothing, volumes: [], reelDest: Nothing, reelTaken: [], reelPlan: []
      , reelBusy: false
      , qd: Nothing, qdFold: "sum", qdRandom: false, qdDest: Nothing
      , cardPeek: Nothing, openSet: Nothing, openSetInfo: Nothing, peekSample: 0
      , placeSliced: false, placeLayers: 0, placeVoices: 0, placeAppend: false
      }
  , render
  , eval: H.mkEval H.defaultEval { handleAction = handleAction, initialize = Just Init }
  }

note :: String -> State -> State
note l st = st { log = Array.takeEnd 40 (Array.snoc st.log l) }

handleAction :: forall o m. MonadAff m => Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  Init -> handleAction RefreshCard
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
  SendTape nm -> do
    H.modify_ _ { cardBusy = true }
    r <- H.liftAff (attempt (toAffE (Http.projectTape nm)))
    case r of
      Left e -> H.modify_ (note (Aff.message e) <<< _ { cardBusy = false })
      Right w -> H.modify_ (note (lastLine w.output) <<< _ { cardBusy = false })
  SendArbhar nm -> do
    H.modify_ _ { cardBusy = true }
    r <- H.liftAff (attempt (toAffE (Http.setToArbhar nm)))
    case r of
      Left e -> H.modify_ (note (Aff.message e) <<< _ { cardBusy = false })
      Right w -> H.modify_ (note (lastLine w.output) <<< _ { cardBusy = false })
  -- | The reel plan is the Morphagene's card, so a set sent there joins the
  -- | end of it, and the tab turns to show where it went.
  IntoReel nm -> do
    st0 <- H.get
    unless (Array.any (\i -> i.name == nm) st0.reelPlan) do
      r <- H.liftAff (attempt (toAffE (Http.reel nm)))
      H.modify_ \st -> st { reelPlan = Array.snoc st.reelPlan
                              { name: nm, reel: either (const Nothing) Just r } }
    H.modify_ (note (nm <> " is on the reel plan \x2014 write the card from the Morphagene tab"))
    handleAction (SetDest Dest.Morphagene)
  SendQd nm -> do
    st0 <- H.get
    H.modify_ _ { cardBusy = true }
    r <- H.liftAff (attempt (toAffE (Http.qdAdd { set: nm, fold: st0.qdFold, random: st0.qdRandom })))
    case r of
      Left e -> H.modify_ (note (Aff.message e) <<< _ { cardBusy = false })
      Right c -> H.modify_ (note (lastLine c.output) <<< _ { cardBusy = false, qd = Just c })
  QdLook -> do
    c <- H.liftAff (toAffE Http.qdCard)
    vols <- H.liftAff (toAffE Http.volumes)
    H.modify_ _ { qd = Just c, volumes = vols }
  QdFold f -> H.modify_ _ { qdFold = f }
  QdRandom b -> H.modify_ _ { qdRandom = b }
  QdDrop f -> do
    c <- H.liftAff (toAffE (Http.qdDrop f))
    H.modify_ (note c.output <<< _ { qd = Just c })
  QdDest v -> H.modify_ _ { qdDest = Just v }
  QdWrite replace -> do
    st0 <- H.get
    case st0.qdDest of
      Nothing -> H.modify_ (note "which card? choose a volume first")
      Just d -> do
        H.modify_ _ { cardBusy = true }
        c <- H.liftAff (toAffE (Http.qdWrite d replace))
        H.modify_ (note c.output <<< _ { cardBusy = false, qd = Just c })
  PlaceSet nm -> do
    st <- H.get
    -- The arrangement this set will actually take, resolved against what the
    -- module can hold. Falls back to the state's own numbers only if the set
    -- is not in the list, which would mean placing something that is not here.
    let chosenWay =
          fromMaybe { voices: st.placeVoices, layers: st.placeLayers, slices: 0 }
            (Array.find (\x -> x.name == nm) st.sets
               >>= wayFor st.placeVoices st.placeLayers)
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
          -- The arrangement resolved, not the raw state. See `wayFor`: zero
          -- means "let the set decide", and for a set of more than twelve
          -- what it decides is a shape the old default could not name.
          , layers: chosenWay.layers, voices: chosenWay.voices })))
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
  HearOne nm i -> liftEffect $
    Audio.playEach [ "/api/set-audio?set=" <> nm <> "&i=" <> show i ] 1.0

  PeekSet v -> do
    H.modify_ _ { openSet = v, openSetInfo = Nothing, peekSample = 0 }
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

  PickSet nm -> do
    H.modify_ \s0 ->
      s0 { picked = if Set.member nm s0.picked then Set.delete nm s0.picked
                    else Set.insert nm s0.picked
         , confirmDrop = false }
    st0 <- H.get
    when (st0.dest == Dest.Morphagene) (handleAction LookAtReel)
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

  SetBank v -> H.modify_ _ { bank = v }
  SetLetter v -> H.modify_ _ { letter = v }
  SetDest d -> do
    H.modify_ _ { dest = d, reel = Nothing }
    when (d == Dest.Morphagene) (handleAction LookAtReel)
    when (d == Dest.QuadDrum) (handleAction QdLook)

  -- | **Both halves at once**, because neither answers anything alone: a reel
  -- | with nowhere to go and a volume with no reel are the same non-answer.
  LookAtReel -> do
    st0 <- H.get
    vols <- H.liftAff (toAffE Http.volumes)
    r <- case Array.fromFoldable st0.picked of
      [ one ] -> Just <$> H.liftAff (toAffE (Http.reel one))
      _ -> pure Nothing
    H.modify_ _ { volumes = vols, reel = r }
    -- A card that went away takes its slot listing with it, rather than
    -- leaving the page showing what a different card held.
    st1 <- H.get
    case st1.reelDest of
      Just d | Array.elem d vols -> handleAction (SetReelDest d)
      _ -> H.modify_ _ { reelDest = Nothing, reelTaken = [] }

  -- | **Choosing the card reads the card.** The slots a write can take are a
  -- | fact about the card in the slot right now, not about the manifest \x2014 the
  -- | same correction the Rample's write question had to make.
  SetReelDest d -> do
    H.modify_ _ { reelDest = Just d, reelTaken = [] }
    on <- H.liftAff (toAffE (Http.reelsOn d))
    H.modify_ _ { reelTaken = on.reels }
    unless on.ok (H.modify_ (note on.output))

  -- | **Ticked sets join the card in the order the list shows them.** Nothing
  -- | is addressed here; a set takes a position, and its name on the card is
  -- | read off that position when the card is written.
  PlanAdd -> do
    st0 <- H.get
    let have = map _.name st0.reelPlan
        adding = Array.filter (\n -> not (Array.elem n have))
                              (Array.fromFoldable st0.picked)
    items <- traverse (\n -> do
               r <- H.liftAff (attempt (toAffE (Http.reel n)))
               pure { name: n, reel: either (const Nothing) Just r }) adding
    H.modify_ _ { reelPlan = st0.reelPlan <> items }

  PlanDrop n -> H.modify_ \st ->
    st { reelPlan = Array.filter (\i -> i.name /= n) st.reelPlan }

  PlanUp n -> H.modify_ \st -> st { reelPlan = shiftPlan (-1) n st.reelPlan }
  PlanDown n -> H.modify_ \st -> st { reelPlan = shiftPlan 1 n st.reelPlan }

  PlanClear -> H.modify_ _ { reelPlan = [] }

  -- | **The card is written whole, in order.** Slot by slot is what the
  -- | module undoes; a card written in one pass is the only arrangement it
  -- | leaves alone, and it is also the only one whose names we can promise.
  WriteReelCard -> do
    st0 <- H.get
    case st0.reelDest of
      Nothing -> H.modify_ (note "say which card it goes to")
      Just dest
        | Array.null st0.reelPlan -> H.modify_ (note "the card is empty \x2014 add some sets first")
        | otherwise -> do
            H.modify_ _ { reelBusy = true }
            r <- H.liftAff (attempt (toAffE (Http.writeCard (map _.name st0.reelPlan) dest)))
            case r of
              Left e -> H.modify_ (note ("the write did not get an answer: "
                                         <> Aff.message e
                                         <> " \x2014 reload and check the card")
                                   <<< _ { reelBusy = false })
              Right done -> do
                H.modify_ (note done.output <<< _ { reelBusy = false })
                handleAction (SetReelDest dest)

  SetQuery v -> H.modify_ _ { setQuery = v }
  SetOnlyPicked v -> H.modify_ _ { onlyPicked = v }
  SetSort v -> H.modify_ _ { setSort = sortOf v }
  SetKit v -> H.modify_ _ { kit = v, kitMine = v /= "" }
  SetVoice v -> H.modify_ \s ->
    s { voice = clamp 1 4 (fromMaybe s.voice (Int.fromString v)) }
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

  PeekSample i -> do
    st <- H.get
    H.modify_ _ { peekSample = i }
    case st.openSet of
      Nothing -> pure unit
      Just nm -> handleAction (HearOne nm i)

  DeclareCentre nm c -> do
    r <- H.liftAff (attempt (toAffE (Http.setCentre nm c)))
    case r of
      -- A failed declaration is worth saying out loud: silently keeping the
      -- old centre would leave every later transposition wrong with nothing
      -- on screen to explain it.
      Left e -> H.modify_ \st -> st { log = st.log <> [ "could not set the key centre: " <> show e ] }
      Right v | not v.ok -> H.modify_ \st -> st { log = st.log <> [ v.output ] }
      Right _ -> handleAction RefreshSets

  DeclareArp nm b -> do
    r <- H.liftAff (attempt (toAffE (Http.setArpeggiated nm b)))
    case r of
      Left e -> H.modify_ \st -> st { log = st.log <> [ "could not say how it was played: " <> show e ] }
      Right v | not v.ok -> H.modify_ \st -> st { log = st.log <> [ v.output ] }
      Right _ -> handleAction RefreshSets

  Copy s -> liftEffect (copyText s)

  SetPlaceSliced b -> H.modify_ _ { placeSliced = b }

  SetPlaceLayers n -> H.modify_ _ { placeLayers = n, placeVoices = 1 }

  SetPlaceWay w -> H.modify_ _ { placeLayers = w.layers, placeVoices = w.voices }

  SetPlaceAppend b -> H.modify_ _ { placeAppend = b }

  -- | **Say what was written, in the words the module will show.**
  -- |
  -- | The compiler's last line is about its own work — files and bytes — and
  -- | the question at this button is "is my thing on the card, and where". A
  -- | write that succeeds and reports something that does not name a slot is a
  -- | write you go and check, and checking means reading the card for a name
  -- | you expect to see. AC did exactly that, four times, and each time the
  -- | name on the card was another take's.
  -- |
  -- | The preview already knows: it was fetched to answer "what would this
  -- | do", and the same list answers "what did it do" the moment the write
  -- | comes back ok. Read before it is cleared, because clearing it first is
  -- | how this ended up with nothing to say.
  WriteCard dest replace -> do
    st0 <- H.get
    let going = Array.filter (\sl -> sl.fate /= "keep" && sl.name /= "")
                  (maybe [] _.slots st0.preview)
        oneSlot sl = sl.slot <> " " <> sl.name <> " \x00b7 " <> show sl.files
                       <> (if sl.files == 1 then " file" else " files")
        said = joinWith ", " (map oneSlot going)
    H.modify_ _ { cardBusy = true, confirmWrite = Nothing, preview = Nothing }
    r <- H.liftAff (attempt (toAffE (Http.writeToCard dest replace)))
    case r of
      Left e -> H.modify_ (note (Aff.message e) <<< _ { cardBusy = false })
      -- Never "wrote" on a failure, and never a slot list the compiler did not
      -- confirm: a confident sentence about the wrong outcome is the thing
      -- this is here to stop.
      Right w
        | not w.ok -> H.modify_ (note (lastLine w.output) <<< _ { cardBusy = false })
        | said == "" -> H.modify_ (note (lastLine w.output) <<< _ { cardBusy = false })
        | otherwise -> H.modify_
            (note ("wrote " <> said <> " to " <> dest) <<< _ { cardBusy = false })
    handleAction RefreshCard
  SetLayerMode m -> H.modify_ _ { layerMode = m }
  -- | `append` is the whole of the two-axis change at this end: the same set,
  -- | the same voice, and a choice about whether it stands beside what is
  -- | there or in its place.

render :: forall m. State -> H.ComponentHTML Action () m
render st =
  HH.div [ HP.class_ (HH.ClassName "q") ]
    [ HH.header [ HP.class_ (HH.ClassName "q-head") ]
        [ HH.div [ HP.class_ (HH.ClassName "q-brand") ]
            [ HH.h1_ [ HH.text "Pinakes" ]
            , HH.span [ HP.class_ (HH.ClassName "q-sub") ]
                [ HH.text "every set kept, and where it can go" ]
            ]
        , HH.div [ HP.class_ (HH.ClassName "q-headright") ]
            [ HH.a [ HP.class_ (HH.ClassName "q-elsewhere"), HP.href "quadrat.html" ]
                [ HH.text "Quadrat \x2197" ]
            ]
        ]
    , HH.div [ HP.class_ (HH.ClassName "q-panes") ]
        [ HH.div [ HP.class_ (HH.ClassName "q-pane is-library") ] [ setsView ]
        , HH.div [ HP.class_ (HH.ClassName "q-pane is-dest") ]
            ( [ destHead ]
                <> case st.dest of
                     Dest.Rample -> [ transformPanel, cardView ]
                     Dest.Morphagene -> [ reelPanel, destKnows Dest.Morphagene ]
                     Dest.QuadDrum -> [ qdPanel, destKnows Dest.QuadDrum ]
                     Dest.Arbhar -> [ arbharPanel, destKnows Dest.Arbhar ]
            )
        , maybe (HH.text "") setModal st.openSet
        ]
    , HH.section [ HP.class_ (HH.ClassName "q-log") ]
        (map (\l -> HH.div_ [ HH.text l ]) st.log)
    ]
  where
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
              [ HH.text "Nothing kept yet. Save a set from Quadrat \
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
            , filterBar
            , pickedBar
            ]
              <> (if Array.null shownSets
                    then [ HH.p [ HP.class_ (HH.ClassName "q-muted") ]
                             [ HH.text "nothing here matches that" ] ]
                    else map setRow shownSets)
          )

  -- | **The list is long, every name is a timestamp, and it can only grow.**
  -- |
  -- | So the library stops being something you read and becomes something you
  -- | search. One box over everything the listing already carries — name, kind,
  -- | the parameters that moved, what played it, the declared key — because a
  -- | person looking for a set does not know in advance which of those fields
  -- | holds the word they remember, and five controls would make them choose
  -- | before they can look.
  -- |
  -- | Every term must match, so terms narrow: `chord g minor` is the three of
  -- | them together and not any of them.
  -- |
  -- | Beside it, the other half of filtering, and the half that is not a
  -- | search at all: **narrow to what is ticked**. Assembling a pack out of a
  -- | hundred sets turns the question from "which sets exist" into "what have
  -- | I got", and no text can ask that.
  filterBar =
    HH.div [ HP.class_ (HH.ClassName "q-setfilter") ]
      [ HH.input
          [ HP.class_ (HH.ClassName "q-setq")
          , HP.type_ HP.InputSearch
          , HP.value st.setQuery
          , HP.placeholder "find \x2014 name, kind, parameter, instrument, key"
          , HP.title "matches the set name, the kind of take, the parameters that \
                     \moved, what played it, and the declared key. Several words \
                     \narrow: each one has to match."
          , HE.onValueInput SetQuery
          ]
      , HH.button
          [ HP.class_ (HH.ClassName ("q-chip is-only"
              <> (if st.onlyPicked then " is-on" else "")))
          , HP.title (if Set.isEmpty st.picked
                      then "nothing is ticked yet"
                      else "show only the " <> show (Set.size st.picked) <> " ticked")
          , HP.disabled (Set.isEmpty st.picked)
          , HE.onClick \_ -> SetOnlyPicked (not st.onlyPicked)
          ]
          [ HH.text ("ticked" <> (if Set.isEmpty st.picked then "" else " " <> show (Set.size st.picked))) ]
      , HH.label [ HP.class_ (HH.ClassName "q-field is-tight q-setsort") ]
          [ HH.span_ [ HH.text "order" ]
          , HH.select [ HE.onValueChange SetSort ]
              (map (\o -> HH.option
                      [ HP.value o.value, HP.selected (o.value == sortValue st.setSort) ]
                      [ HH.text o.says ])
                  sortChoices)
          ]
      , if Array.length shownSets == Array.length st.sets then HH.text "" else
          HH.span [ HP.class_ (HH.ClassName "q-muted") ]
            [ HH.text ("showing " <> show (Array.length shownSets)
                <> " of " <> show (Array.length st.sets)) ]
      ]

  shownSets = ordered (Array.filter
      (\r -> (not st.onlyPicked || Set.member r.name st.picked) && matches r)
      st.sets)
    where
    -- `made` is an ISO instant, so a string comparison IS the chronology and
    -- no date parsing is needed to get the order right.
    ordered = case st.setSort of
      Newest -> Array.sortBy (\a b -> compare b.made a.made)
      Oldest -> Array.sortBy (\a b -> compare a.made b.made)
      Biggest -> Array.sortBy (\a b -> compare b.count a.count)
      Alpha -> Array.sortBy (\a b -> compare (String.toLower (labelOf a))
                                             (String.toLower (labelOf b)))
    terms = Array.filter (_ /= "") (String.split (String.Pattern " ")
              (String.toLower (String.trim st.setQuery)))
    matches r = Array.all (\t -> String.contains (String.Pattern t) (hay r)) terms
    -- Everything the row already knows, lowered once per set per keystroke.
    -- Deliberately NOT the sample-level fields: `notes` and `secs` are
    -- hundreds of numbers each and matching "60" against them would return
    -- most of the library while looking like a search.
    hay r = String.toLower (joinWith " "
      ( [ r.name, r.kind, r.voice, r.made, r.take
        , if r.stereo then "stereo" else "mono"
        , if r.runnable then "re-runnable runnable" else ""
        , if r.centre.root < 0 then "" else pcName r.centre.root
        , r.centre.tonality
        ] <> r.moved <> [ r.encoding ]))

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
          [ HH.h2_ [ HH.text "Destination" ] ]
      , HH.div [ HP.class_ (HH.ClassName "q-desttabs") ]
          (map destTab Dest.all)
      , HH.p [ HP.class_ (HH.ClassName "q-muted q-destshape") ]
          [ HH.text (let f = Dest.factsOf st.dest
                     in f.maker <> " " <> f.name <> " \x2014 " <> f.shape) ]
      ]

  destTab d =
    let f = Dest.factsOf d
    in HH.button
         [ HP.class_ (HH.ClassName ("q-desttab is-" <> Dest.slug d
             <> (if d == st.dest then " is-on" else "")
             <> (case f.unbuilt of
                   Nothing -> ""
                   Just _ -> " is-unbuilt")))
         , HP.title (f.shape <> (case f.unbuilt of
                                   Nothing -> ""
                                   Just _ -> " \x2014 nothing here writes it yet"))
         , HE.onClick \_ -> SetDest d
         ]
         [ HH.text f.name ]
  -- | **The QuadDrum card, staged.** On the Rample a set is a kit; here it is
  -- | a curve: one folder, in order, that the MOD pot or a V/Oct sequencer
  -- | walks. The two choices that change what gets written are here, not
  -- | behind the button: how stereo becomes mono, and whether the folder is
  -- | played in order or at random.
  qdPanel =
    HH.div [ HP.class_ (HH.ClassName "q-transform q-qd") ]
      [ HH.div [ HP.class_ (HH.ClassName "q-send") ]
          [ HH.span [ HP.class_ (HH.ClassName "q-arm-label") ] [ HH.text "Fold" ]
          , HH.div [ HP.class_ (HH.ClassName "q-chips") ]
              (map (\{ f, says } -> HH.button
                      [ HP.class_ (HH.ClassName ("q-chip" <> (if st.qdFold == f then " is-on" else "")))
                      , HP.title says
                      , HE.onClick \_ -> QdFold f ]
                      [ HH.text f ])
                 [ { f: "sum", says: "(L + R) / 2 \x2014 safe for level, cancels anything out of phase" }
                 , { f: "left", says: "the left channel only" }
                 , { f: "right", says: "the right channel only" } ])
          , HH.label [ HP.class_ (HH.ClassName "q-check") ]
              [ HH.input [ HP.type_ HP.InputCheckbox, HP.checked st.qdRandom
                         , HE.onChecked QdRandom ]
              , HH.text " random per trigger" ]
          ]
      , case st.qd of
          Nothing -> HH.p [ HP.class_ (HH.ClassName "q-muted") ] [ HH.text "reading\x2026" ]
          Just c ->
            HH.div_
              [ HH.p [ HP.class_ (HH.ClassName "q-muted") ]
                  [ HH.text (show (Array.length c.folders) <> " of " <> show c.maxFolders
                      <> " folders \x00b7 " <> show c.files <> " of " <> show c.maxFiles
                      <> " files \x00b7 " <> show c.perFolder <> " to a folder") ]
              , if Array.null c.folders
                  then HH.p [ HP.class_ (HH.ClassName "q-muted") ]
                         [ HH.text "Empty. Open a set and send it: Onto the QuadDrum." ]
                  else HH.div [ HP.class_ (HH.ClassName "q-plan") ]
                         (map (\f -> HH.div [ HP.class_ (HH.ClassName "q-planrow") ]
                                 [ HH.span [ HP.class_ (HH.ClassName "q-planname") ] [ HH.text f.name ]
                                 , HH.span [ HP.class_ (HH.ClassName "q-muted q-plansays") ]
                                     [ HH.text (show f.files <> (if f.files == 1 then " sample" else " samples")
                                         <> (if f.files > 120 then " \x2014 past 120, V/Oct may not reach the top" else "")) ]
                                 , HH.div [ HP.class_ (HH.ClassName "q-planmove") ]
                                     [ HH.button [ HP.class_ (HH.ClassName "q-planbtn")
                                                 , HP.title "take it off the card"
                                                 , HE.onClick \_ -> QdDrop f.name ] [ HH.text "\x00d7" ] ]
                                 ]) c.folders)
              ]
      , HH.div [ HP.class_ (HH.ClassName "q-send") ]
          [ HH.span [ HP.class_ (HH.ClassName "q-arm-label") ] [ HH.text "Card" ]
          , if Array.null st.volumes
              then HH.span [ HP.class_ (HH.ClassName "q-muted") ]
                     [ HH.text "no volume is mounted \x2014 mount the card, then press the tab again" ]
              else HH.div [ HP.class_ (HH.ClassName "q-chips") ]
                     (map (\v -> HH.button
                             [ HP.class_ (HH.ClassName ("q-chip is-arm"
                                 <> (if st.qdDest == Just v then " is-on" else "")))
                             , HE.onClick \_ -> QdDest v ]
                             [ HH.text v ]) st.volumes)
          , HH.button
              [ HP.class_ (HH.ClassName "q-chip is-arm")
              , HP.disabled (st.cardBusy || st.qdDest == Nothing)
              , HP.title "copy every folder here onto the card, after what is already on it; \
                         \a folder of ours already there by name is refused"
              , HE.onClick \_ -> QdWrite false ]
              [ HH.text "Write" ]
          , HH.button
              [ HP.class_ (HH.ClassName "q-chip")
              , HP.disabled (st.cardBusy || st.qdDest == Nothing)
              , HP.title "the same, replacing our own folders already on the card \x2014 only those"
              , HE.onClick \_ -> QdWrite true ]
              [ HH.text "Write, replacing ours" ]
          ]
      , HH.p [ HP.class_ (HH.ClassName "q-muted") ]
          [ HH.text "No hot swap: power the module down before the card comes out. \
                    \For the best playback, erase the card and write it in one go." ]
      ]

  -- | Nothing to arrange: the stick is positional, and the server finds the
  -- | first empty bank and scene. What is worth a line is where the button is.
  arbharPanel =
    HH.p [ HP.class_ (HH.ClassName "q-muted") ]
      [ HH.text "Open a set and send it: Onto the stick. Each six samples take a \
                \library bank and a scene, the first ones that hold nothing; a taken \
                \slot is left alone." ]

  -- | The module's own arithmetic. Worth reading whether or not this page can
  -- | write the thing, which is why it is separate from the not-yet notice.
  destKnows d =
    let f = Dest.factsOf d
    in HH.div [ HP.class_ (HH.ClassName "q-destknows") ]
         [ HH.p [ HP.class_ (HH.ClassName "q-destsuits") ]
             [ HH.strong_ [ HH.text "suits" ], HH.text (" " <> f.suits) ]
         , HH.ul [ HP.class_ (HH.ClassName "q-destimposes") ]
             (map (\i -> HH.li_ [ HH.text i ]) f.imposes)
         , HH.p [ HP.class_ (HH.ClassName "q-muted") ] [ HH.text ("format: " <> f.format) ]
         ]

  -- | **A reel is the take, marked.**
  -- |
  -- | Not the cut samples glued back together, which would butt-join at every
  -- | boundary — and on a reel the gaps between progressions ARE material,
  -- | because a splice runs from one marker to the next. So the source is the
  -- | capture itself, and the placement is a copy plus a `cue ` chunk.
  -- |
  -- | Which costs nothing: the capture is already 48 kHz, 32-bit float,
  -- | stereo, format tag 3 — the reel format exactly, measured on a real take
  -- | rather than assumed from the profile. Nothing is converted and nothing
  -- | is re-encoded.
  reelPanel =
    HH.div [ HP.class_ (HH.ClassName "q-transform q-reel") ]
      [ HH.div [ HP.class_ (HH.ClassName "q-reelmove") ]
          [ HH.button
              [ HP.class_ (HH.ClassName "q-chip is-arm")
              , HP.disabled (Set.isEmpty st.picked)
              , HP.title "put the ticked sets on the card, in the order they are listed"
              , HE.onClick \_ -> PlanAdd
              ]
              [ HH.text ("Add ticked" <> (if Set.isEmpty st.picked then ""
                                          else " (" <> show (Set.size st.picked) <> ")")
                         <> " \x2192") ]
          , if Array.null st.reelPlan then HH.text "" else
              HH.button
                [ HP.class_ (HH.ClassName "q-chip")
                , HP.title "take everything off the card plan"
                , HE.onClick \_ -> PlanClear
                ]
                [ HH.text "clear" ]
          ]
      , planList
      , cardTarget
      ]

  -- | **The card, in order.**
  -- |
  -- | Position is the whole content of this list, because position is the only
  -- | thing the module preserves. It compacts on load \x2014 one contiguous run,
  -- | gaps closed \x2014 so an address chosen here would be rewritten underneath
  -- | us, and a name we invent and the module then changes is worse than no
  -- | name at all. What it cannot undo is the ORDER, so that is what is
  -- | chosen, and the `mgN` names are derived from it at the moment of writing,
  -- | which is the only moment they are true.
  planList
    | Array.null st.reelPlan =
        HH.p [ HP.class_ (HH.ClassName "q-muted") ]
          [ HH.text "Tick sets on the left and add them. They go on the card in \
                    \this order, and the module names them by position." ]
    | otherwise =
        HH.div [ HP.class_ (HH.ClassName "q-plan") ]
          (Array.mapWithIndex planRow st.reelPlan)

  planRow i item =
    let n = i + 1
        last = Array.length st.reelPlan - 1
    in HH.div [ HP.class_ (HH.ClassName "q-planrow") ]
         [ HH.span [ HP.class_ (HH.ClassName "q-planslot") ] [ HH.text (reelFile n) ]
         , HH.span [ HP.class_ (HH.ClassName "q-planname") ] [ HH.text item.name ]
         , HH.span [ HP.class_ (HH.ClassName "q-muted q-plansays") ]
             [ HH.text (case item.reel of
                          Nothing -> "reading\x2026"
                          Just r | not r.ok -> r.output
                          Just r -> show r.splices <> " splices \x00b7 " <> secs2 r.secs <> "s") ]
         , case item.reel of
             Just r | r.ok && not (Array.null r.starts) -> reelStrip r
             _ -> HH.text ""
         , case item.reel of
             Just r | not (Array.null r.refuses) ->
               HH.span [ HP.class_ (HH.ClassName "q-notyet q-plansays") ]
                 [ HH.text (joinWith "; " r.refuses) ]
             _ -> HH.text ""
         , HH.div [ HP.class_ (HH.ClassName "q-planmove") ]
             [ HH.button [ HP.class_ (HH.ClassName "q-planbtn")
                         , HP.disabled (i == 0)
                         , HP.title "earlier on the card"
                         , HE.onClick \_ -> PlanUp item.name ] [ HH.text "\x2191" ]
             , HH.button [ HP.class_ (HH.ClassName "q-planbtn")
                         , HP.disabled (i == last)
                         , HP.title "later on the card"
                         , HE.onClick \_ -> PlanDown item.name ] [ HH.text "\x2193" ]
             , HH.button [ HP.class_ (HH.ClassName "q-planbtn")
                         , HP.title "take it off the card"
                         , HE.onClick \_ -> PlanDrop item.name ] [ HH.text "\x00d7" ]
             ]
         ]

  -- | **Which card, and what is already on it that we did not put there.**
  -- |
  -- | Two kinds of stranger can be on a Morphagene card and we know nothing
  -- | about either: reels recorded on the module itself, and reels from
  -- | somewhere else entirely. Neither is handled yet, so rather than write
  -- | over them quietly the count is said and the decision is left with the
  -- | player. Writing the card whole is the only arrangement the module leaves
  -- | alone; it is also the only one whose names this page can promise.
  cardTarget =
    HH.div_
      [ HH.div [ HP.class_ (HH.ClassName "q-send") ]
          [ HH.span [ HP.class_ (HH.ClassName "q-arm-label") ] [ HH.text "Card" ]
          , if Array.null st.volumes
              then HH.span [ HP.class_ (HH.ClassName "q-muted") ]
                     [ HH.text "no volume is mounted \x2014 mount the card, then \
                               \press the tab again" ]
              else HH.div [ HP.class_ (HH.ClassName "q-chips") ]
                     (map (\v -> HH.button
                             [ HP.class_ (HH.ClassName ("q-chip is-arm"
                                 <> (if st.reelDest == Just v then " is-on" else "")))
                             , HP.title ("read " <> v <> ", and say which reels it holds")
                             , HE.onClick \_ -> SetReelDest v
                             ]
                             [ HH.text v ]) st.volumes)
          ]
      , case st.reelDest of
          Nothing -> HH.text ""
          Just dest ->
            HH.div_
              [ if Array.null st.reelTaken then HH.text "" else
                  HH.p [ HP.class_ (HH.ClassName "q-notyet") ]
                    [ HH.strong_ [ HH.text (show (Array.length st.reelTaken)
                                            <> " reels are already on this card") ]
                    , HH.text " \x2014 some may be recordings made on the module, or \
                              \reels this library has never seen. Nothing here can tell \
                              \them apart yet, so writing the card replaces the lot."
                    ]
              , HH.div [ HP.class_ (HH.ClassName "q-send") ]
                  [ HH.button
                      [ HP.class_ (HH.ClassName "q-chip is-arm is-go")
                      , HP.disabled (st.reelBusy || Array.null st.reelPlan
                                      || Array.any planRefused st.reelPlan)
                      , HP.title ("write " <> show (Array.length st.reelPlan)
                                  <> " reels into the root of " <> dest
                                  <> ", named mg1 upward in this order")
                      , HE.onClick \_ -> WriteReelCard
                      ]
                      [ HH.text (if Array.null st.reelPlan then "nothing to write"
                                 else "Write " <> show (Array.length st.reelPlan)
                                      <> " reels to " <> dest) ]
                  ]
              -- | The module writes back to this card whenever you record on
              -- | it. Straight from the manual, and the one fact that decides
              -- | what happens to these reels AFTER they are loaded.
              , HH.p [ HP.class_ (HH.ClassName "q-muted") ]
                  [ HH.text "the Morphagene stores its own recordings and splices \
                            \back to the card, so take the card out once the reel \
                            \is loaded if you want to keep it as written" ]
              ]
      ]

  planRefused i = case i.reel of
    Just r -> not (Array.null r.refuses)
    Nothing -> false

  -- | `mg1`..`mg9`, then `mga`..`mgw`. The module's own naming, mirrored here
  -- | so the page can say the filename before the write rather than after it.
  -- | See `reelName` in server.mjs \x2014 the two have to agree, and the page is
  -- | the one that has to SAY it.
  reelLabel n
    | n <= 9 = show n
    | otherwise = fromMaybe "?" (Array.index reelLetters (n - 10))
  reelFile n = "mg" <> reelLabel n <> ".wav"

  -- | **The reel, drawn.** One block per splice, as wide as the splice is
  -- | long, which is the one picture a count cannot give: a splice runs marker
  -- | to marker and so carries the silence after its sound. Uneven blocks mean
  -- | uneven progressions, and that is worth seeing before the card goes in
  -- | the module.
  reelStrip rl =
    let
      bounds = Array.cons 0.0 rl.starts <> [ rl.secs ]
      spans = Array.zipWith (\a b -> max 0.0 (b - a)) bounds (Array.drop 1 bounds)
      total = max 0.001 (sum spans)
    in
      HH.div [ HP.class_ (HH.ClassName "q-reelstrip") ]
        (Array.mapWithIndex
          (\i w -> HH.div
             [ HP.class_ (HH.ClassName ("q-reelsplice" <> if i == 0 then " is-lead" else ""))
             , HP.attr (HH.AttrName "style") ("flex: " <> show (w / total) <> " 1 0")
             , HP.title ("splice " <> show (i + 1) <> " \x00b7 " <> secs2 w <> "s"
                 <> (if i == 0 then " \x2014 the lead-in, before the first region"
                     else " \x2014 region " <> show i))
             ]
             [])
          spans)

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
  -- | **What a set is called, when its name is a timestamp.**
  -- |
  -- | Most names here are minted \x2014 `chord-hits-0915-232247` \x2014 and a column
  -- | of them is a column of near-identical strings that has to be READ
  -- | character by character. The rebus already solved this: it is a picture
  -- | WITH WORDS, and the words are sayable and typeable. So the minted name
  -- | steps aside for the alias, and a name somebody actually chose is kept,
  -- | because that is the one case where the string carries meaning.
  labelOf r
    | minted r.name = (markOf r).glyph.alias
    | otherwise = r.name

  -- | `<anything>-MMDD-HHMMSS` \x2014 the shape this page mints. Read off the
  -- | TAIL rather than the head, so a take of a kind that does not exist yet
  -- | still reads as minted.
  minted nm =
    case Array.take 2 (Array.reverse (String.split (String.Pattern "-") nm)) of
      [ hms, md ] -> digitsOfLength 6 hms && digitsOfLength 4 md
      _ -> false

  digitsOfLength n t =
    String.length t == n
      && Array.all (\c -> c >= "0" && c <= "9")
           (String.split (String.Pattern "") t)

  -- Labels are compared against what is ON SCREEN, not against the whole
  -- library: a collision you cannot see cannot mislead you, and naming every
  -- row against sets the filter has hidden would put the timestamps straight
  -- back.
  -- Counted once per render: comparing each row with every other, deriving both
  -- labels each time, was a quarter of this page's idle CPU.
  ambiguous r = fromMaybe 0 (Map.lookup (labelOf r) labelCounts) > 1

  labelCounts :: Map.Map String Int
  labelCounts = Map.fromFoldableWith (+) (map (\x -> Tuple (labelOf x) 1) shownSets)

  -- | **Only the part that differs.** Two sets wearing one rebus are two takes
  -- | of the same progression, so their names share everything but the last
  -- | two segments \x2014 and printing `chord-hits-0915-` twice, ellipsed at the
  -- | exact character where they diverge, is the worst of both: the ink of a
  -- | timestamp with none of its information.
  tiebreak r
    | minted r.name =
        joinWith "-" (Array.reverse
          (Array.take 2 (Array.reverse (String.split (String.Pattern "-") r.name))))
    | otherwise = r.name

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
      -- | **A picture that is not a timestamp.**
      -- |
      -- | The name of every set here is the minute it was made, so a library
      -- | of them is a column of near-identical strings — which is why
      -- | deleting one makes you read the names AND hear the audio first. The
      -- | rebus is the same identity said in a form the eye can take in at
      -- | once, and its words make it sayable and typeable too.
      , HH.div [ HP.class_ (HH.ClassName "q-set-id") ]
          [ HH.div [ HP.class_ (HH.ClassName "q-set-name") ]
              [ let m = markOf r
                in RebusView.chip
                     { height: 15.0, mono: m.mono
                     , title: m.glyph.alias <> " \x2014 " <> m.says }
                     m.glyph.icons
              , HH.span [ HP.class_ (HH.ClassName "q-set-nametext") ]
                  [ HH.text (labelOf r) ]
              -- | **The timestamp comes back exactly where it is load-bearing
              -- | and nowhere else.**
              -- |
              -- | A rebus is a CONTENT identity for a chord set, so two takes
              -- | of the same progression wear the same pair of icons on
              -- | purpose \x2014 that agreement is the whole point of drawing
              -- | them. It also means the picture cannot tell those two rows
              -- | apart, and a row that silently looks like its neighbour is
              -- | the failure this project keeps meeting. So when a label is
              -- | shared by more than one row now on screen, THOSE rows say
              -- | which they are, and the rest stay clean.
              , if not (ambiguous r) then HH.text "" else
                  HH.span [ HP.class_ (HH.ClassName "q-set-disamb")
                          , HP.title "another set on screen wears the same picture \
                                     \and words, so this one says its own name" ]
                    [ HH.text (tiebreak r) ]
              ]
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
  small lbl v act =
    HH.label [ HP.class_ (HH.ClassName "q-field is-tight") ]
      [ HH.span_ [ HH.text lbl ]
      , HH.input [ HP.type_ HP.InputText, HP.value v, HE.onValueInput act ]
      ]


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
  -- | The picture, and under it the bar that says how much of it there is.
  samplePic r =
    HH.div [ HP.class_ (HH.ClassName "q-swrap") ]
      [ samplePicOnly r
      , let total = Array.foldl (+) 0.0 (Array.filter (_ > 0.0) r.secs)
        in if total <= 0.0 then HH.text ""
           else HH.div
                  [ HP.class_ (HH.ClassName ("q-stotal"
                      <> (if total > 300.0 then " is-over" else "")))
                  , HP.attr (HH.AttrName "style") (totalWidth total)
                  , HP.title (secs2 total <> " seconds of material in all")
                  ]
                  []
      ]

  samplePicOnly r =
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
        Just r | Array.length (waysFor r) > 1 ->
          HH.div [ HP.class_ (HH.ClassName "q-arrange") ]
            [ HH.span [ HP.class_ (HH.ClassName "q-arrangelab") ] [ HH.text "becomes" ]
            , HH.div [ HP.class_ (HH.ClassName "q-arrangeopts") ]
                (map (opt r) (waysFor r))
            ]
        -- | **One way, and it is not the obvious one.** A set of more than
        -- | twelve has no single-voice answer at all, so the arrangement it
        -- | gets is a real decision even when there is nothing to choose —
        -- | and saying nothing would leave it looking like the default.
        Just r | [ a ] <- waysFor r, a.voices > 1 ->
          HH.div [ HP.class_ (HH.ClassName "q-arrange") ]
            [ HH.span [ HP.class_ (HH.ClassName "q-arrangelab") ] [ HH.text "becomes" ]
            , HH.div [ HP.class_ (HH.ClassName "q-arrangeopts") ] [ opt r a ]
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
    waysFor r = Http.arrangementsOf { count: r.count, stereo: r.stereo }
    opt r a =
      HH.button
        [ HP.class_ (HH.ClassName ("q-arrangeopt"
            <> (if chosen r a then " is-on" else "")
            <> (if a.voices == 1 && a.layers == natural r then " is-natural" else "")))
        , HP.title (says r a <> (if a.voices > 1
                                 then " \x2014 too many for one voice, so they spread \
                                      \across the voices this set can reach"
                                 else if a.layers == natural r
                                 then " \x2014 how it was swept, so the layers keep \
                                      \their parameter values"
                                 else " \x2014 regrouped, so the layers carry positions \
                                      \rather than values"))
        , HE.onClick \_ -> SetPlaceWay { voices: a.voices, layers: a.layers }
        ]
        -- One picture per voice: what a voice holds is what a voice holds,
        -- and two of them side by side is the arrangement.
        -- The voices side by side in a row of their own, because that IS the
        -- arrangement: two stacks of nine beside each other is a different
        -- object from one stack of eighteen, and stacked vertically they read
        -- as the second thing.
        -- | **What a voice holds is layers, and `splitOver` counts SAMPLES.**
        -- |
        -- | Which is the same number only when there are no slices, and the
        -- | one option where it was true \x2014 the plain stack \x2014 was the one
        -- | that looked right. Every grid drew `min 12 samples` rows of
        -- | `slices` columns: twenty-four samples as "1 file, 24 slices" came
        -- | out as twelve rows of twenty-four, 288 boxes for a set of 24.
        -- | Andrew, 2026-09-16: *"there are many more than 24 little squares
        -- | in each of them but we only have 24 samples so what is one meant
        -- | to read into it?"* \x2014 nothing, and that is the answer: the
        -- | picture is the whole argument for this control, so a picture that
        -- | is not of the material is worse than no picture.
        [ HH.div [ HP.class_ (HH.ClassName "q-arrangevoices") ]
            (Array.mapWithIndex
              (\vi n -> voicePicFrom
                          { layers: if a.slices <= 0 then n else max 1 (n / a.slices)
                          , slices: a.slices, dim: false }
                          [ r.name ]
                          (sum (Array.take vi (Http.splitOver a.voices r.count))))
              (Http.splitOver a.voices r.count))
        , HH.span [ HP.class_ (HH.ClassName "q-arrangesays") ] [ HH.text (says r a) ]
        ]
    -- Zero is "let the extent decide", and the extent decides the natural
    -- one — so the natural option is what zero is showing.
    chosen r a = case wayFor st.placeVoices st.placeLayers r of
      Just w -> w.voices == a.voices && w.layers == a.layers && w.slices == a.slices
      Nothing -> false
    says r a
      | a.voices > 1 =
          let per = Http.splitOver a.voices r.count
              same = Array.all (_ == a.layers) per
          in (if same then show a.layers <> " layers each"
              else joinWith " and " (map show per) <> " layers")
             <> " on " <> show a.voices
             <> (if r.stereo then " stereo voices" else " voices")
      | a.slices == 0 = show a.layers <> " layers"
      | a.layers == 1 = "1 file, " <> show a.slices <> " slices"
      | otherwise = show a.layers <> " \x00d7 " <> show a.slices

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
                  [ let m = markOf r
                    in RebusView.chip
                         { height: 19.0, mono: m.mono
                         , title: m.glyph.alias <> " \x2014 " <> m.says }
                         m.glyph.icons
                  , HH.h2_ [ HH.text nm ]
                  , HH.button
                      [ HP.class_ (HH.ClassName "q-plain")
                      , HE.onClick \_ -> PeekSet Nothing ]
                      [ HH.text "done" ]
                  ]
              , HH.div [ HP.class_ (HH.ClassName "q-setpage") ]
                  [ HH.div [ HP.class_ (HH.ClassName "q-setpic") ]
                      [ samplePicPeek r, voicingPanel r ]
                  , HH.dl [ HP.class_ (HH.ClassName "q-facts") ]
                      -- | The whole instant, and the only place it is
                      -- | printed now that the listing shows a name instead
                      -- | of a date. Marked UTC, because `made` is and the
                      -- | minted name is local \x2014 they differ by the offset
                      -- | and an unlabelled pair of times that disagree is
                      -- | worse than one time.
                      ( fact "made" (String.take 10 r.made
                          <> " \x00b7 " <> String.take 5 (String.drop 11 r.made)
                          <> " UTC")
                      <> fact "from take" r.take
                      <> fact "samples"
                          (show r.count
                            <> (if Array.null r.extent then ""
                                else " on " <> joinWith " × " (map show r.extent)))
                      <> fact "lengths" (lengthsSays r)
                      <> fact "played by" r.voice
                      <> fact "notes from" (listenedSays r)
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
                  , centrePanel r
                  , transferable "in a pattern" (dirt r)
                  , transferable "as a progression \x2014 paste into Vetula"
                      (Tidal.progression
                         { name: r.name
                         , alias: (markOf r).glyph.alias
                         , centre: centreWord r.centre
                         }
                         (Array.filter (not <<< Array.null) (join r.voicings)))
                  , HH.div [ HP.class_ (HH.ClassName "q-set-do") ] (setVerbs r)
                  ]
              ]
          ]
    where
    -- | The same picture, but its boxes read a sample rather than opening the
    -- | page you are already on.
    samplePicPeek r =
      let
        n = r.count
        cols = case r.extent of
          [ _, inner ] | inner > 0 -> inner
          _ -> min 16 (max 1 n)
        rows = max 1 ((n + cols - 1) / cols)
      in
        HH.div [ HP.class_ (HH.ClassName "q-spic") ]
          (map
            (\yy -> HH.div [ HP.class_ (HH.ClassName ("q-srow" <> if yy `mod` 2 == 1 then " is-odd" else "")) ]
              (map
                (\xx ->
                  let i = yy * cols + xx in
                  HH.div
                    [ HP.class_ (HH.ClassName ("q-scell is-" <> sampleClass r i
                        <> (if secsOf r i <= 0.0 then " is-unmeasured" else "")
                        <> (if overScale (secsOf r i) then " is-over" else "")
                        <> (if i == st.peekSample then " is-reading" else "")))
                    , HP.attr (HH.AttrName "style")
                        (widthOf (secsOf r i)
                          <> fromMaybe "" (map (\s -> "; " <> s) (pitchTint (noteOf r i))))
                    , HP.title (show (i + 1))
                    , HE.onMouseEnter \_ -> PeekSample i
                    ]
                    [])
                (Array.range 0 (min cols (n - yy * cols) - 1))))
            (Array.range 0 (rows - 1)))

    -- | **The whole set at once, as a progression.**
    -- |
    -- | A count of notes is the one reading of a chord that carries no music:
    -- | five could be a cluster or an open voicing two octaves wide, and
    -- | which of those decides whether the sample is any use. So they are
    -- | drawn on staves, where the spacing between the noteheads IS the
    -- | voicing.
    -- |
    -- | All of them rather than one on hover, because a chord set is not
    -- | seven separate chords — it is what you played, in the order you
    -- | played it, and the relation between one voicing and the next is the
    -- | thing you go back to a set for. Hovering still sounds one and marks
    -- | it in the picture above; it no longer decides what you can see.
    voicingPanel r =
      let
        voices = r.voicings
        -- One extent for the whole set, so the staff lines sit at the same
        -- pitch in every cell. Without it each stave sizes itself and a
        -- note that LOOKS higher than its neighbour need not be — which is
        -- the one comparison a progression is drawn for.
        ext = Stave.spanOf (join (join voices))
      in
        if Array.null (Array.filter (not <<< Array.null) voices) then HH.text "" else
        HH.div [ HP.class_ (HH.ClassName "q-voicing") ]
          [ HH.div [ HP.class_ (HH.ClassName "q-factlab") ]
              [ HH.text (show (Array.length (Array.filter (not <<< Array.null) voices))
                  <> " voicings, in the order they were played") ]
          , HH.div [ HP.class_ (HH.ClassName "q-staves") ]
              (Array.mapWithIndex (staveCell r ext) voices)
          ]

    staveCell r ext k chords
      | Array.null chords = HH.text ""
      | otherwise =
          HH.div
            [ HP.class_ (HH.ClassName ("q-stavecell"
                <> if k == st.peekSample then " is-reading" else ""))
            , HP.title ("sample " <> show (k + 1) <> " of " <> show r.count
                <> (if Array.length chords > 1
                      then " \x2014 " <> show (Array.length chords) <> " chords struck into one"
                      else ""))
            , HE.onMouseEnter \_ -> PeekSample k
            ]
            -- **The clefs once, on the first cell.** A system carries them
            -- at its start and not on every bar of it; seven copies of a G
            -- clef is six copies of something already read.
            [ HH.div [ HP.class_ (HH.ClassName "q-staverow") ]
                (map (Stave.grandIn { lo: ext.lo, hi: ext.hi, clefs: k == 0 }) chords)
            , HH.div [ HP.class_ (HH.ClassName "q-staveno") ] [ HH.text (show (k + 1)) ]
            ]

    -- | **The key centre, declared by hand.**
    -- |
    -- | Only for a set that HAS chords: a drum grid has no centre, and an
    -- | empty picker on it would be a question with no answer.
    -- |
    -- | Two controls and they do different jobs. The ROOT is what makes the
    -- | set transposable — intervals have to be measured from something — and
    -- | it is the only one transposition uses. The INTENT is context for
    -- | reading the set: against an intended minor, a major I is further out
    -- | than a minor chord of the same complexity, and no amount of looking at
    -- | the notes recovers what you meant.
    -- |
    -- | Both are declarations and neither is offered as a guess. For a capture
    -- | off a generator they are settings somebody chose, so a suggestion here
    -- | would be handing an input back — and a wrong centre is worse than
    -- | none, since everything downstream transposes from it in silence.
    centrePanel r
      | Array.null (Array.filter (not <<< Array.null) (join r.voicings)) = HH.text ""
      | otherwise =
          HH.div [ HP.class_ (HH.ClassName "q-centre") ]
            [ HH.div [ HP.class_ (HH.ClassName "q-factlab") ] [ HH.text "played" ]
            , HH.div [ HP.class_ (HH.ClassName "q-centrepick") ]
                [ HH.select
                    [ HP.class_ (HH.ClassName "q-sel")
                    , HP.title "the root these chords are measured from"
                    , HE.onValueChange \v ->
                        DeclareCentre r.name
                          { root: fromMaybe (-1) (Int.fromString v)
                          , tonality: r.centre.tonality }
                    ]
                    ( [ HH.option
                          [ HP.value "-1", HP.selected (r.centre.root < 0) ]
                          [ HH.text "root not said" ]
                      ]
                        <> map
                          (\pc -> HH.option
                            [ HP.value (show pc), HP.selected (r.centre.root == pc) ]
                            [ HH.text (pcName pc) ])
                          (Array.range 0 11)
                    )
                , HH.select
                    [ HP.class_ (HH.ClassName "q-sel")
                    , HP.title "what you meant it to be \x2014 not what the notes are"
                    , HE.onValueChange \v ->
                        DeclareCentre r.name { root: r.centre.root, tonality: v }
                    ]
                    ( map
                        (\o -> HH.option
                          [ HP.value o.value, HP.selected (r.centre.tonality == o.value) ]
                          [ HH.text o.label ])
                        [ { value: "", label: "intent not said" }
                        , { value: "major", label: "meant major" }
                        , { value: "minor", label: "meant minor" }
                        ]
                    )
                -- | **How, not what.** The other two say where the material
                -- | sits; this one says how to READ it, and it changes the
                -- | staves above as soon as it is answered.
                , HH.select
                    [ HP.class_ (HH.ClassName "q-sel")
                    , HP.title "an arpeggiated region is one chord, not one chord per note"
                    , HE.onValueChange \v -> DeclareArp r.name (v == "arp")
                    ]
                    ( map
                        (\o -> HH.option
                          [ HP.value o.value
                          , HP.selected (r.arpeggiated == (o.value == "arp")) ]
                          [ HH.text o.label ])
                        [ { value: "block", label: "as blocks" }
                        , { value: "arp", label: "as arpeggios" }
                        ]
                    )
                ]
            , HH.div [ HP.class_ (HH.ClassName "q-set-what") ]
                [ HH.text (centreSays r.centre <> " \x00b7 " <> arpSays r) ]
            ]

    -- | **A rendering of this set in somebody else's notation**, with the
    -- | button that sends it there.
    -- |
    -- | Both of them are the same shape of thing — a SuperDirt pattern and a
    -- | Vetula progression — and both are consumed by being pasted, so they
    -- | get one affordance rather than two. Empty draws nothing: a set with no
    -- | chords has no progression, and an empty code block would read as one.
    transferable lab text
      | text == "" = HH.text ""
      | otherwise =
          HH.div [ HP.class_ (HH.ClassName "q-xfer") ]
            [ HH.div [ HP.class_ (HH.ClassName "q-factlab") ]
                [ HH.text lab
                , HH.button
                    [ HP.class_ (HH.ClassName "q-copy")
                    , HP.title "copy to the clipboard"
                    , HE.onClick \_ -> Copy text ]
                    [ HH.text "copy" ]
                ]
            , HH.code [ HP.class_ (HH.ClassName "q-set-dirt") ] [ HH.text text ]
            ]

    fact k v = if v == "" then [] else
      [ HH.dt_ [ HH.text k ], HH.dd_ [ HH.text v ] ]
    -- | **The lengths, in seconds, for sanity.** The shortest and the longest
    -- | bound the picture, and the longest is also the SLOT every slice in a
    -- | layer has to fit — so it is the number that decides what a sliced
    -- | arrangement wastes. The total says whether a set is minutes or
    -- | seconds, which is the difference between a bank of hits and a drone.
    -- | **Which port and channel the notes were taken from.**
    -- |
    -- | "All channels" is worth naming rather than leaving blank: it is the
    -- | setting under which a sequencer sharing the port writes its own notes
    -- | into your chord, and it is the default.
    listenedSays r
      | r.notesFrom == "" = if r.count == 0 then "" else "not recorded"
      | r.notesChan == 0 = r.notesFrom <> " \x00b7 every channel"
      | otherwise = r.notesFrom <> " \x00b7 channel " <> show r.notesChan

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
            then HH.a
                   [ HP.class_ (HH.ClassName "q-plain")
                   , HP.href ("quadrat.html?open=" <> r.name)
                   , HP.title "open it on Quadrat's Bench, drawn on the take it \
                              \was cut from — nothing is re-cut or re-measured"
                   ]
                   [ HH.text "Open" ]
            else HH.text ""
        , if r.runnable
            then HH.a
                   [ HP.class_ (HH.ClassName "q-plain")
                   , HP.href ("quadrat.html?again=" <> r.name)
                   , HP.title "load the spec that made this set into Quadrat, so \
                              \it can be recorded again at a different resolution"
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
        -- **Send to · tape.** The same kept take, somewhere else: played whole
        -- by the grain engine rather than cut. Made from the take, so any set
        -- that has one can go; nothing about the set changes.
        , if r.described && r.take /= ""
            then HH.button
                   [ HP.class_ (HH.ClassName "q-plain")
                   , HP.disabled st.cardBusy
                   , HP.title "play the take this set was cut from WHOLE, as a \
                              \tape for Conspicillum: linked, never cut, with its \
                              \tempo and what each sixteenth sounds like \x2014 \
                              \loaded into SuperDirt as <take>-tape"
                   , HE.onClick \_ -> SendTape r.name
                   ]
                   [ HH.text "As a tape" ]
            else HH.text ""
        -- **Send to · the modules.** Each is the set, projected: nothing is
        -- re-cut and nothing about the set changes. The destination's own
        -- choices (the QuadDrum's fold, the reel's order) live on its tab.
        , if r.described && r.count > 0
            then HH.button
                   [ HP.class_ (HH.ClassName "q-plain")
                   , HP.title "add it to the end of the Morphagene reel plan \x2014 \
                              \one splice per sample, the take marked"
                   , HE.onClick \_ -> IntoReel r.name
                   ]
                   [ HH.text "Into a reel" ]
            else HH.text ""
        , if r.described && r.count > 0
            then HH.button
                   [ HP.class_ (HH.ClassName "q-plain")
                   , HP.disabled st.cardBusy
                   , HP.title "onto the mounted Arbhar stick: six samples to a library \
                              \bank and a scene, at the first ones that are empty"
                   , HE.onClick \_ -> SendArbhar r.name
                   ]
                   [ HH.text "Onto the stick" ]
            else HH.text ""
        , if r.described && r.count > 0
            then HH.button
                   [ HP.class_ (HH.ClassName "q-plain")
                   , HP.disabled st.cardBusy
                   , HP.title ("one folder on the QuadDrum card, in order, so the MOD \
                               \pot (or a V/Oct sequencer) walks the set \x2014 folded to \
                               \mono by " <> st.qdFold
                               <> (if st.qdRandom then ", random per trigger" else ""))
                   , HE.onClick \_ -> SendQd r.name
                   ]
                   [ HH.text "Onto the QuadDrum" ]
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

  -- | The same picture, starting `skip` samples into the material.
  -- |
  -- | Which a one-voice picture never needed and a several-voice one cannot do
  -- | without: voice 3 holds the second half of the set, and drawing it from
  -- | sample zero would show the same twelve boxes twice and call it an
  -- | arrangement.
  voicePicFrom o from skip = voicePicSkip o skip from

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
  voicePicOf o from = voicePicSkip o 0 from

  voicePicSkip o skip from =
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
    pool = Array.drop skip do
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
    HH.div [ HP.class_ (HH.ClassName ("q-wslot is-" <> s.fate)) ]
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

-- | **Move one entry of the card plan by one place**, keeping everything else
-- | in order. Arrows rather than drag: the list is short, the intent is exact,
-- | and a drag that lands one row off is a card written in the wrong order.
shiftPlan :: Int -> String -> Array PlanItem -> Array PlanItem
shiftPlan d n xs =
  case Array.findIndex (\i -> i.name == n) xs of
    Nothing -> xs
    Just at ->
      let to = clamp 0 (Array.length xs - 1) (at + d)
      in if to == at then xs
         else fromMaybe xs do
                item <- Array.index xs at
                without <- Array.deleteAt at xs
                Array.insertAt to item without

-- | The last thing a command said, which is its summary. The rest is a list of
-- | files and belongs in the log it already went to.
lastLine :: String -> String
lastLine s = fromMaybe s (Array.last (Array.filter (_ /= "") (String.split (String.Pattern "\n") s)))

-- | A pitch CLASS by name, with no octave — a key centre has no register, and
-- | showing one would invite the reading that the root must be played there.
pcName :: Int -> String
pcName pc = fromMaybe "?" (Array.index names (pc `mod` 12))
  where
  names = [ "C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B" ]

-- | Two decimals, for a duration in a tooltip.
secs2 :: Number -> String
secs2 d = show (Int.round (d * 100.0) / 100)

-- | **The arrangement a placement will actually use**, which is not always the
-- | one the state names.
-- |
-- | Zero means "let the set decide", and what the set decides used to be the
-- | single-voice natural grouping — fine while every set had one. A set of
-- | more than twelve has NO single-voice answer, so the default named a
-- | shape that is not on offer, nothing was selected, and the placement went
-- | out as zero voices: unplaceable, with a picker on screen showing the one
-- | arrangement that would have worked.
-- |
-- | So the fallback is the first thing actually offered. A page cannot choose
-- | an arrangement the module will not hold, and it should not be possible to
-- | ask for one.
wayFor
  :: forall r
   . Int -> Int
  -> { count :: Int, stereo :: Boolean, extent :: Array Int | r }
  -> Maybe Http.Arrangement
wayFor voices layers r =
  let
    ways = Http.arrangementsOf { count: r.count, stereo: r.stereo }
    natural = case r.extent of
      [ outer, _ ] -> outer
      _ -> 0
    want = if voices == 0 then 1 else voices
    asked = Array.find (\a -> a.layers == layers && a.voices == want) ways
    nat = Array.find (\a -> a.voices == 1 && a.layers == natural) ways
    fallback = Array.head ways
  in
    case (if voices > 0 || layers > 0 then asked else nat) of
      Just a -> Just a
      Nothing -> fallback

-- | The same question asked of a stored set, which knows whether it is stereo
-- | without knowing what kind of material made it.
voicesWide :: Boolean -> Array Int
voicesWide wide = if wide then [ 1, 3 ] else [ 1, 2, 3, 4 ]

-- | **How much material a set is**, which is a different question from how
-- | long any one sample is — and one that no per-cell width can answer.
-- |
-- | The arithmetic is against it: a set of one 166-second take draws one
-- | cell, and a set of twelve eight-second chords draws twelve. Twelve cells
-- | are longer than one at any scale, so the row that holds six times less
-- | material looks six times bigger. That is not a scale to be tuned, it is
-- | a second fact needing a second mark.
-- |
-- | Linear, not log: totals here run 26 to 166 seconds, a range of six, and
-- | a log would flatten the very comparison the bar exists to make. Past
-- | five minutes it clamps and notches, which is where a "set" has stopped
-- | being a set of samples anyway.
totalWidth :: Number -> String
totalWidth total = "width: " <> show (Int.round (2.0 + 258.0 * clampN 0.0 1.0 (total / 300.0))) <> "px"

-- | `clamp` for numbers, named apart because `clamp` is already in scope for
-- | the Int uses on this page.
clampN :: Number -> Number -> Number -> Number
clampN lo hi v = max lo (min hi v)

overScale :: Number -> Boolean
overScale d = d > 300.0

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
atOf d = clampN 0.0 1.0 (Number.log (max 0.05 d / 0.05) / Number.log (300.0 / 0.05))

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

noteName :: Int -> String
noteName n = fromMaybe "?" (Array.index names (n `mod` 12)) <> show ((n / 12) - 1)
  where
  names = [ "C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B" ]

-- | **What a declared centre buys**, said where it is declared.
-- |
-- | The root and the intent are worth different things and the sentence says
-- | which, because a control whose value does nothing visible is a control
-- | nobody fills in.
centreSays :: Http.Centre -> String
centreSays c
  | c.root < 0 =
      "without a root this set cannot be transposed \x2014 "
        <> "its chords are absolute, and intervals have to be measured "
        <> "from something"
  | otherwise =
      "transposable from " <> pcName c.root
        <> (case c.tonality of
              "major" -> ", meant major"
              "minor" -> ", meant minor"
              _ -> "; how far out a chord is needs the intent too")

-- | **What the manner is doing to the reading**, in the set's own numbers.
-- |
-- | Said with the count because the count is the whole point: the difference
-- | between nine voicings and sixty-four fragments is not a preference, and a
-- | sentence without it would read as one.
arpSays :: forall r. { arpeggiated :: Boolean, voicings :: Array (Array (Array Int)) | r } -> String
arpSays r =
  let n = Array.length (Array.filter (not <<< Array.null) (join r.voicings))
  in if r.arpeggiated
     then "read as " <> show n <> " chords, one per region \x2014 an arpeggio's notes are one voicing"
     else "read as " <> show n <> " chords, as they were struck"

-- | The centre as one word for a comment line, or empty when undeclared.
-- | `"D minor"` when both are known, `"D"` when only the root is — never
-- | `"D unknown"`, which would read as a claim.
centreWord :: Http.Centre -> String
centreWord c
  | c.root < 0 = ""
  | c.tonality == "" = pcName c.root
  | otherwise = pcName c.root <> " " <> c.tonality
