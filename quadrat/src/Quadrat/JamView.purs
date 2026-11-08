-- | **The Jam page: record a source to disk for as long as it plays.**
-- |
-- | Quadrat's other modes cut what they record by a schedule; a jam is cut by
-- | nothing. It is the daemon's `jam` (see `itajara/daemon/src/jam.rs`): the
-- | audio goes to disk as it is made, so an hour costs no memory and a crash
-- | keeps what was played. When it stops it is a take like any other, and
-- | plays by name in Tidal (`s "<name>"`) straight away.
-- |
-- | Views and nothing else: it reads the daemon's snapshot and the page's two
-- | choices (which source, what name), and says what it wants through
-- | `Handlers`, as `SweepView` does. Its pieces go into `Quadrat.Layout` with
-- | the page's statement and meter, the same as the Bench's and the Loop's.
module Quadrat.JamView
  ( Handlers
  , Choice
  , Parts
  , parts
  , sourceIndex
  ) where

import Prelude

import Data.Array as Array
import Data.Int as Int
import Data.Maybe (Maybe(..), maybe)
import Data.Number as Number
import Foreign.LooperSocket (LooperState)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type Handlers act =
  { setName :: String -> act
  , setSource :: String -> act
  , start :: act
  , stop :: act
  }

-- | What the page holds for the next jam: a source by name ("" for the first
-- | one available) and a name ("" to be stamped when it starts).
type Choice = { source :: String, name :: String }

-- | The daemon's 1-based source number for the chosen name, or the first
-- | available source when none is chosen.
sourceIndex :: LooperState -> String -> Maybe Int
sourceIndex rig chosen =
  map (_ + 1) (Array.findIndex wanted rig.sources)
  where
  wanted s
    | chosen == "" = s.available
    | otherwise = s.name == chosen

-- | The page's pieces, for `Quadrat.Layout`: the statement's sentence and
-- | warnings, the main column (the button and what the jam is doing), and the
-- | side column (what a jam is). The meter is the page's, drawn by Main.
-- |
-- | `label` is the page's name for an input (the renamed one where there is
-- | one), so the sentence says what the Bench's would.
type Parts w act =
  { sentence :: Array (HH.HTML w act)
  , warnings :: Array (HH.HTML w act)
  , main :: Array (HH.HTML w act)
  , side :: Array (HH.HTML w act)
  }

parts :: forall w act. Handlers act -> (String -> String) -> Choice -> Maybe LooperState -> Parts w act
parts h label choice = case _ of
  Nothing ->
    { sentence: [ HH.text "Recording nothing: a jam needs Itajara running." ]
    , warnings: []
    , main: []
    , side: [ about ]
    }
  Just rig ->
    let
      jam = rig.jam
      running = jam.on
      current = sourceIndex rig choice.source
      sourceName i = maybe "?" _.name (Array.index rig.sources (i - 1))
      sourceSlot =
        HH.select
          [ cls "q-slot"
          , HE.onValueChange h.setSource
          , HP.disabled running
          , HP.title "the input this jam records"
          ]
          ( map
              ( \src -> HH.option
                  [ HP.value src.name
                  , HP.selected (Just src.name == map sourceName current)
                  , HP.disabled (not src.available)
                  ]
                  [ HH.text (label src.name <> if src.available then "" else " \x2014 off") ]
              )
              rig.sources
          )
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
          [ HH.text (if running then "Recording " else "Records ")
          , sourceSlot
          , HH.text " to disk as "
          , nameSlot
          , HH.text ", for as long as it plays."
          ]
      , warnings: warnings jam <> behind jam
      , main:
          [ HH.div [ cls "q-jam-row" ]
              [ HH.button
                  [ cls ("q-jam-go" <> if running then " is-on" else "")
                  , HE.onClick \_ -> if running then h.stop else h.start
                  , HP.disabled (not running && current == Nothing)
                  ]
                  [ HH.text (if running then "Stop" else "Record") ]
              ]
          ]
            <> status jam
      , side: [ about ]
      }

  where
  about =
    HH.p [ cls "q-note" ]
      [ HH.text
          "A jam records one source to disk as it plays, for as long as it plays: a modular jam, a rehearsal. \
          \The audio goes to disk as it is made, so an hour costs no memory and a crash keeps what was played. \
          \When it stops it is a take, and plays by name in Tidal straight away."
      ]

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
