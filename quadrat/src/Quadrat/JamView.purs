-- | **The Jam page: you play, and it records.**
-- |
-- | Who plays decides the page (`kb/plans/quadrat-recording-views.md`): the
-- | Bench is the rig playing a transect, Score is Triggerfish playing what it
-- | published, and Jam is you. What a jam varies is how long it is and whether
-- | it is cut, so that is the choice in its sentence:
-- |
-- | * **for as long as it plays** — the daemon's `jam` (see
-- |   `itajara/daemon/src/jam.rs`): the audio goes to disk as it is made, so an
-- |   hour costs no memory and a crash keeps what was played.
-- | * **for N bars from the next bar line** — a take held in memory and closed
-- |   by the daemon at the count, kept whole.
-- | * **cut at each hit / chord** — a take held in memory and divided at its
-- |   onsets afterwards: as many samples as you play.
-- |
-- | The last two are the same take as the Bench's and Score's (waveform,
-- | Divide, Save), which Main draws. This module holds the choice, the
-- | streamed jam's pieces, and what each length is.
module Quadrat.JamView
  ( Handlers
  , Choice
  , Length(..)
  , lengthValue
  , lengthOf
  , lengthChoices
  , kindOf
  , Parts
  , whole
  , about
  , sourceIndex
  ) where

import Prelude

import Data.Array as Array
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Number as Number
import Data.String as String
import Foreign.LooperSocket (LooperState)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Quadrat.Kind (Kind(..))

type Handlers act =
  { setName :: String -> act
  , start :: act
  , stop :: act
  }

-- | **How long a jam is, and whether it is cut.**
data Length
  = Whole
  | ForBars Int
  | CutHits
  | CutChords

derive instance Eq Length

-- | What the page holds for the next jam: its length, and the streamed jam's
-- | name ("" to be stamped when it starts). The input is the page's, shared
-- | with the Bench and Score: one rig, one choice of what is plugged in.
type Choice = { name :: String, length :: Length }

lengthValue :: Length -> String
lengthValue = case _ of
  Whole -> "whole"
  ForBars n -> "bars:" <> show n
  CutHits -> "hits"
  CutChords -> "chords"

lengthOf :: String -> Maybe Length
lengthOf = case _ of
  "whole" -> Just Whole
  "hits" -> Just CutHits
  "chords" -> Just CutChords
  v -> case String.stripPrefix (String.Pattern "bars:") v of
    Just n -> map ForBars (Int.fromString n)
    Nothing -> Nothing

-- | The choices, as the sentence says them. The bar counts are the usual ones
-- | plus whatever count is held, so a count set elsewhere is shown as itself.
lengthChoices :: Int -> Array { v :: String, t :: String }
lengthChoices held =
  [ { v: "whole", t: "for as long as it plays" } ]
    <> map (\n -> { v: lengthValue (ForBars n), t: barsPhrase n })
         (if Array.elem held counts then counts else Array.sort (Array.snoc counts held))
    <> [ { v: "hits", t: "cut at each hit" }, { v: "chords", t: "cut at each chord" } ]
  where
  counts = [ 1, 2, 4, 8, 16 ]
  barsPhrase n =
    "for " <> (if n == 1 then "a bar" else show n <> " bars") <> " from the next bar line"

-- | The take a length records, if it is a take: the streamed jam is not one
-- | until it stops.
kindOf :: Length -> Maybe Kind
kindOf = case _ of
  Whole -> Nothing
  ForBars n -> Just (Bars n)
  CutHits -> Just DrumHits
  CutChords -> Just ChordHits

-- | The daemon's 1-based source number for the chosen name, or the first
-- | available source when none is chosen.
sourceIndex :: LooperState -> String -> Maybe Int
sourceIndex rig chosen =
  map (_ + 1) (Array.findIndex wanted rig.sources)
  where
  wanted s
    | chosen == "" = s.available
    | otherwise = s.name == chosen

-- | The streamed jam's pieces, for `Quadrat.Layout`: the sentence and its
-- | warnings, the main column (the button and what the jam is doing), and the
-- | side column. The input and the length are the page's slots, drawn by Main.
type Parts w act =
  { sentence :: Array (HH.HTML w act)
  , warnings :: Array (HH.HTML w act)
  , main :: Array (HH.HTML w act)
  , side :: Array (HH.HTML w act)
  }

whole
  :: forall w act
   . Handlers act
  -> { source :: Array (HH.HTML w act), length :: HH.HTML w act, sourceOk :: Boolean }
  -> Choice
  -> Maybe LooperState
  -> Parts w act
whole h slots choice = case _ of
  Nothing ->
    { sentence: [ HH.text "Recording nothing: a jam needs Itajara running." ]
    , warnings: []
    , main: []
    , side: [ about Whole ]
    }
  Just rig ->
    let
      jam = rig.jam
      running = jam.on
      -- While it runs, the name is the daemon's (the stamped one when none was
      -- typed), so the sentence never claims a name the file will not have.
      nameSlot =
        if running then HH.span [ cls "q-slot is-fixed" ] [ HH.text jam.name ]
        else
          HH.input
            [ cls "q-slot is-name"
            , HP.type_ HP.InputText
            , HP.value choice.name
            , HP.placeholder "jam-MMDD-HHMMSS"
            , HP.title "names the take; empty, it is stamped with the time it starts"
            , HE.onValueInput h.setName
            ]
    in
      { sentence:
          [ HH.text (if running then "Recording " else "Records ") ]
            <> slots.source
            <> [ HH.text " to disk as ", nameSlot, HH.text ", ", slots.length, HH.text "." ]
      , warnings: warnings jam <> behind jam
      , main:
          [ HH.div [ cls "q-jam-row" ]
              [ HH.button
                  [ cls ("q-jam-go" <> if running then " is-on" else "")
                  , HE.onClick \_ -> if running then h.stop else h.start
                  , HP.disabled (not running && not slots.sourceOk)
                  ]
                  [ HH.text (if running then "Stop" else "Record") ]
              ]
          ]
            <> status jam
      , side: [ about Whole ]
      }

  where
  behind jam =
    if jam.on && jam.behindSecs > 1.0
      then [ HH.p [ cls "q-warn" ] [ HH.text ("the disk is " <> secs1 jam.behindSecs <> " behind") ] ]
      else []

  status jam =
    if jam.on then
      [ HH.div [ cls "q-jam-status is-on" ]
          [ HH.span [ cls "q-jam-clock" ] [ HH.text (clock jam.secs) ]
          , HH.span_ [ HH.text ("into " <> jam.name) ]
          ]
      ]
    else if jam.name /= "" && jam.secs > 0.0 then
      [ HH.div [ cls "q-jam-status" ]
          [ HH.span [ cls "q-jam-clock" ] [ HH.text (clock jam.secs) ]
          , HH.span_ [ HH.text ("kept as " <> jam.name <> ". In Tidal: ") ]
          , HH.code_ [ HH.text ("s \"" <> jam.name <> "\"") ]
          ]
      ]
    else []

  warnings jam =
    (if jam.droppedSecs > 0.0
      then [ HH.p [ cls "q-warn" ] [ HH.text (secs1 jam.droppedSecs <> " lost: the disk fell behind, so the file is that much shorter than the time played.") ] ]
      else [])
      <> (if jam.full
        then [ HH.p [ cls "q-warn" ] [ HH.text "The file reached what a WAV can hold, and the jam stopped itself." ] ]
        else [])


-- | **What each length is**, in the side column: the one thing about the
-- | choice the sentence cannot say in a clause.
about :: forall w act. Length -> HH.HTML w act
about l =
  HH.p [ cls "q-note" ]
    [ HH.text case l of
        Whole ->
          "A jam records one source to disk as it plays, for as long as it plays: a modular jam, a rehearsal. \
          \The audio goes to disk as it is made, so an hour costs no memory and a crash keeps what was played. \
          \When it stops it is a take, and plays by name in Tidal straight away."
        ForBars _ ->
          "Starts on the next bar line and closes itself at the count, both in the rig's own bars \
          \(Link's, or loop 1's). Kept whole: a groove cut into hits stops being a groove. \
          \Held in memory until it is saved."
        CutHits ->
          "Play the hits, softest first; stop when you are done. The take is cut at each onset \
          \afterwards, as many samples as you played, and the order you played them in is the \
          \order velocity picks them in."
        CutChords ->
          "Play the chords, one strike each, and let them ring; stop when you are done. The take \
          \is cut at each onset afterwards, with a decay window long enough for a piano. If the \
          \instrument sends MIDI, the notes of each chord are kept with its sample."
    ]

cls :: forall r i. String -> HP.IProp (class :: String | r) i
cls = HP.class_ <<< HH.ClassName

-- | `h:mm:ss`, or `m:ss` under an hour.
clock :: Number -> String
clock t =
  let
    s = Int.floor t
    h = s / 3600
    m = (s `mod` 3600) / 60
    r = s `mod` 60
    two n = (if n < 10 then "0" else "") <> show n
  in
    if h > 0 then show h <> ":" <> two m <> ":" <> two r else show m <> ":" <> two r

secs1 :: Number -> String
secs1 x = secs1' x <> " s"

secs1' :: Number -> String
secs1' x = show (Number.round (x * 10.0) / 10.0)
