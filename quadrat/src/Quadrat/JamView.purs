-- | **The Jam page: record a source to disk for as long as it plays.**
-- |
-- | Quadrat's other modes cut what they record by a schedule; a jam is cut by
-- | nothing. It is the daemon's `jam` (see `itajara/daemon/src/jam.rs`): the
-- | audio goes to disk as it is made, so an hour costs no memory and a crash
-- | keeps what was played. When it stops it is a take like any other, and
-- | plays by name in Tidal (`s "<name>"`) straight away.
-- |
-- | A view and nothing else: it reads the daemon's snapshot and the page's two
-- | choices (which source, what name), and says what it wants through
-- | `Handlers`, as `SweepView` does.
module Quadrat.JamView
  ( Handlers
  , Choice
  , view
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

view :: forall w act. Handlers act -> Choice -> Maybe LooperState -> HH.HTML w act
view h choice = case _ of
  Nothing ->
    section
      [ HH.p [ cls "q-note" ] [ HH.text "No daemon: a jam needs Itajara running." ] ]
  Just rig ->
    let
      jam = rig.jam
      running = jam.on
      current = sourceIndex rig choice.source
      sourceName i = maybe "?" _.name (Array.index rig.sources (i - 1))
    in
      section
        ( [ HH.div [ cls "q-jam-row" ]
              [ HH.label [ cls "q-jam-field" ]
                  [ HH.span_ [ HH.text "Source" ]
                  , HH.select
                      [ HE.onValueChange h.setSource, HP.disabled running ]
                      ( map
                          ( \s -> HH.option
                              [ HP.value s.name
                              , HP.selected (Just s.name == map sourceName current)
                              , HP.disabled (not s.available)
                              ]
                              [ HH.text (s.name <> if s.available then "" else " (not connected)") ]
                          )
                          rig.sources
                      )
                  ]
              , meter (current >>= \i -> Array.index rig.sources (i - 1))
              , HH.label [ cls "q-jam-field" ]
                  [ HH.span_ [ HH.text "Name" ]
                  , HH.input
                      [ HP.value choice.name
                      , HP.placeholder "jam-MMDD-HHMMSS when it starts"
                      , HE.onValueInput h.setName
                      , HP.disabled running
                      ]
                  ]
              , HH.button
                  [ cls ("q-jam-go" <> if running then " is-on" else "")
                  , HE.onClick \_ -> if running then h.stop else h.start
                  , HP.disabled (not running && current == Nothing)
                  ]
                  [ HH.text (if running then "Stop" else "Record") ]
              ]
          ]
            <> status rig
        )

  where
  section body =
    HH.section [ cls "q-jam" ]
      ( [ HH.p [ cls "q-note" ]
            [ HH.text
                "Records one source to disk as it plays, for as long as it plays: a modular jam, a rehearsal. \
                \When it stops it is a take, and plays by name in Tidal straight away."
            ]
        ]
          <> body
      )

  status rig =
    let
      jam = rig.jam
    in
      if jam.on then
        [ HH.div [ cls "q-jam-status is-on" ]
            [ HH.span [ cls "q-jam-clock" ] [ HH.text (clock jam.secs) ]
            , HH.span_ [ HH.text ("into " <> jam.name) ]
            , if jam.behindSecs > 1.0
                then HH.span [ cls "q-warn" ] [ HH.text ("the disk is " <> secs1 jam.behindSecs <> " behind") ]
                else HH.text ""
            ]
        ]
          <> warnings jam
      else if jam.name /= "" && jam.secs > 0.0 then
        [ HH.div [ cls "q-jam-status" ]
            [ HH.span [ cls "q-jam-clock" ] [ HH.text (clock jam.secs) ]
            , HH.span_ [ HH.text ("kept as " <> jam.name <> ". In Tidal: ") ]
            , HH.code_ [ HH.text ("s \"" <> jam.name <> "\"") ]
            ]
        ]
          <> warnings jam
      else []

  warnings jam =
    (if jam.droppedSecs > 0.0
      then [ HH.p [ cls "q-warn" ] [ HH.text (secs1 jam.droppedSecs <> " lost: the disk fell behind, so the file is that much shorter than the time played.") ] ]
      else [])
      <> (if jam.full
        then [ HH.p [ cls "q-warn" ] [ HH.text "The file reached what a WAV can hold, and the jam stopped itself." ] ]
        else [])

  meter = case _ of
    Nothing -> HH.span [ cls "q-jam-meter" ] []
    Just s ->
      let
        -- -60 dB at the left, 0 at the right.
        frac = max 0.0 (min 1.0 ((s.db + 60.0) / 60.0))
      in
        HH.span [ cls "q-jam-meter", HP.title (secs1' s.db <> " dB") ]
          [ HH.span [ cls "q-jam-level", HP.style ("width: " <> show (frac * 100.0) <> "%") ] [] ]


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
