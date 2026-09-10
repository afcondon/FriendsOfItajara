-- | **The sweep: what to move, over what, and how to make a sound at each
-- | point.**
-- |
-- | Three ideas, and each replaced something that had turned out to be wrong.
-- |
-- | **A curve is an object, not a setting** (`Quadrat.Curve`). You add one for
-- | every parameter you want to move, so the list of curves and the list of
-- | controlled parameters are the same list, and a parameter with no curve is
-- | simply held. Click steps through the named shapes; editing a point turns it
-- | into a drawing, and the thumbnail says which it is. One field, two
-- | constructors, no second copy of the answer.
-- |
-- | **The shape of the set comes from the destination** (`Quadrat.Encoding`).
-- | Line or grid is not a preference offered beside the real choices; it is a
-- | consequence of how the thing playing it back can be addressed. Choose an
-- | encoding and the axes, their legal sizes and the recording order all follow.
-- |
-- | **The values are still the truth.** With seven interacting non-linear
-- | parameters the useful edit after listening is "position 8 is wrong", which
-- | is a point edit. The curve is a fast way to fill a row, never a claim about
-- | what is in it.
module Quadrat.Sweep
  ( Param
  , Trigger
  , Plan
  , emptyPlan
  , valuesFor
  , Msg(..)
  , update
  , Meaning
  , Step
  , steps
  , remember
  , restore
  , Plain
  , PlainParam
  , flatten
  , unflatten
  , adopt
  ) where

import Prelude

import Data.Array as Array
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number as Number
import Effect (Effect)
import Quadrat.Curve (Curve(..), Shape(..), shapeName, shapeOf)
import Quadrat.Curve as Curve
import Quadrat.Encoding (Cell, Encoding(..))
import Quadrat.Encoding as Encoding

-- | **One parameter of the instrument, and where it is reached.**
-- |
-- | CV *and* MIDI, not one or the other: a modular parameter has a CV input and
-- | no controller, an iPad synth has a controller and no CV, and some things
-- | answer both. Leaving both set is legal and means both are sent.
type Param =
  { name :: String
  -- | An es9-daemon bus, 0…15, or nothing. **Bus 8 is ES-9 panel jack 1** and
  -- | bus 15 is jack 8; the lower eight reach expanders and non-panel outputs.
  , cv :: Maybe Int
  -- | What 0.0 and 1.0 mean on that bus, as the daemon's own -1…1 level.
  -- |
  -- | **Not halved for you.** `/tidal/cv` multiplies by es9-daemon's
  -- | `SAFETY_SCALE`; the direct `/cv <bus> <val>` path this uses does not.
  , cvLo :: Number
  , cvHi :: Number
  -- | **An ESX-8CV channel, 0…7**, reached through Silent Way on one expander
  -- | bus. Beside `cv` rather than instead of it, for the same reason `cc` is
  -- | beside both: a parameter may be driven from more than one place, and
  -- | leaving two set is legal and means two.
  -- |
  -- | Shares `cvLo`/`cvHi`: both are voltages on the same -1…1 scale, and a
  -- | second pair of range fields for the same range would be a second thing
  -- | to keep in step. The ESX is 12-bit where the panel is the audio DAC's
  -- | full depth — about a sixteenth of a semitone on a V/oct.
  , esx :: Maybe Int
  , cc :: Maybe Int
  , ccLo :: Int
  , ccHi :: Int
  , channel :: Int
  -- | Its shape over its axis. See `Quadrat.Curve`.
  , curve :: Curve
  -- | **Which axis it moves along**, as an index into the encoding's axes.
  -- |
  -- | One curve per parameter means one axis per parameter — there is nothing
  -- | for a second to be a function of. That is also what makes a grid legible:
  -- | a column shares its value on one axis, a row on the other.
  , axis :: Int
  }

-- | **What actually makes the sound happen**, once the parameters have settled.
type Trigger =
  { gate :: Maybe Int
  -- | **An ES-5 gate, 0…7.** The expander's own eight, which are gates and
  -- | nothing else — no level, because they have none.
  , es5 :: Maybe Int
  , gateLevel :: Number
  , note :: Maybe Int
  , channel :: Int
  , velocity :: Int
  , ms :: Int
  }

type Plan =
  { encoding :: Encoding
  -- | One size per axis of the encoding. `Encoding.objections` says what the
  -- | destination will refuse, before anything is recorded.
  , extent :: Array Int
  , params :: Array Param
  , trigger :: Trigger
  -- | A substring of a WebMIDI output's name. Empty means MIDI is unused.
  , port :: String
  -- | **How long after setting the parameters before the trigger.**
  -- |
  -- | The one number here that can silently ruin a run: too short and the hit
  -- | is struck while the CVs are still on their way, so a cell is a blend of
  -- | itself and its neighbour — and a blend looks exactly like a value.
  , settleMs :: Int
  -- | Trigger to next trigger. Long enough for the sound to finish, plus enough
  -- | silence to see the join.
  , spacingMs :: Int
  -- | **How long after a trigger is issued before its sound is in the take**,
  -- | in milliseconds — the one number a schedule cannot know.
  -- |
  -- | UDP to es9-daemon, its next audio callback, the module's attack and the
  -- | converter round trip, none of which the page can see. Subtracted from
  -- | every boundary when the schedule divides the take, so the region opens
  -- | just before its sound. Too small clips the attack; too large puts silence
  -- | in front of every sample. Both are visible in the tiles — this is a knob
  -- | to set by looking, not a constant to believe. See `Quadrat.Schedule`.
  , leadMs :: Int
  }

-- | Twelve layers on one voice and one parameter rising — the smallest thing
-- | that can still fail visibly. If the twelve tiles come out identical then
-- | nothing is reaching the instrument.
emptyPlan :: Plan
emptyPlan =
  { encoding: RampleLayers
  , extent: Encoding.defaultExtent RampleLayers
  , params: [ param "morph" 8 ]
  , trigger: { gate: Just 15, es5: Nothing, gateLevel: 0.5, note: Nothing, channel: 1, velocity: 100, ms: 10 }
  , port: ""
  , settleMs: 120
  , spacingMs: 2000
  , leadMs: 30
  }

param :: String -> Int -> Param
param nm bus =
  { name: nm
  , cv: Just bus, cvLo: 0.0, cvHi: 0.5
  , esx: Nothing
  , cc: Nothing, ccLo: 0, ccHi: 127
  , channel: 1
  , curve: Named Linear false
  , axis: 0
  }

-- | How many points this parameter's curve is sampled at: the size of the axis
-- | it moves along.
sizeOfAxis :: Plan -> Int -> Int
sizeOfAxis p a = max 1 (fromMaybe 1 (Array.index p.extent a))

valuesFor :: Plan -> Param -> Array Number
valuesFor p q = Curve.valuesOf (sizeOfAxis p q.axis) q.curve

data Msg
  = PickEncoding String
  | SetExtent Int String
  | AddParam
  | DropParam Int
  | SetName Int String
  | SetCv Int String
  | SetCvLo Int String
  | SetCvHi Int String
  | SetEsx Int String
  | SetCc Int String
  | SetCcLo Int String
  | SetCcHi Int String
  | SetChannel Int String
  | SetAxis Int Int
  -- | Click: the next named shape. Deliberately a no-op over a drawing.
  | NextShape Int
  | FlipCurve Int
  -- | Back to a curve, which has to be its own deliberate act because it
  -- | discards hand-placed values.
  | ToCurve Int
  | SetValue Int Int String
  | SetPort String
  | SetGate String
  | SetEs5 String
  | SetGateLevel String
  | SetNote String
  | SetTrigChannel String
  | SetVelocity String
  | SetHold String
  | SetSettle String
  | SetSpacing String
  | SetLead String

update :: Msg -> Plan -> Plan
update = case _ of
  PickEncoding v -> \p ->
    case Array.find (\e -> Encoding.name e == v) Encoding.all of
      Nothing -> p
      Just e ->
        -- A new encoding is a new set of axes, so an extent from the old one
        -- would be meaningless and a parameter could be pointing at an axis
        -- that no longer exists.
        p { encoding = e
          , extent = Encoding.defaultExtent e
          , params = map (\q -> q { axis = min q.axis (Array.length (Encoding.axes e) - 1) }) p.params
          }
  SetExtent a v -> \p ->
    p { extent = fromMaybe p.extent
          (Array.updateAt a (clamp 1 128 (intOr (sizeOfAxis p a) v)) p.extent) }
  AddParam -> \p ->
    -- **Sixteen**, which is still not a law of anything.
    --
    -- Eight was "what the FH-2 has spare", and then the ES-9's own panel is
    -- eight, the ESX-8CV is eight more, and every FH-2 jack mapped to a
    -- controller is reachable as MIDI without any of them. The ceiling is now
    -- how many curves can be read down a column rather than how many places
    -- there are to send one.
    if Array.length p.params >= 16 then p
    else p { params = Array.snoc p.params
               (param ("param " <> show (Array.length p.params + 1))
                      (8 + Array.length p.params)) }
  DropParam i -> \p -> p { params = fromMaybe p.params (Array.deleteAt i p.params) }
  SetName i v -> onParam i \q -> q { name = v }
  SetCv i v -> onParam i \q -> q { cv = busOf v }
  SetCvLo i v -> onParam i \q -> q { cvLo = level q.cvLo v }
  SetCvHi i v -> onParam i \q -> q { cvHi = level q.cvHi v }
  SetEsx i v -> onParam i \q -> q { esx = slotOf v }
  SetCc i v -> onParam i \q -> q { cc = ccOf v }
  SetCcLo i v -> onParam i \q -> q { ccLo = clamp 0 127 (intOr q.ccLo v) }
  SetCcHi i v -> onParam i \q -> q { ccHi = clamp 0 127 (intOr q.ccHi v) }
  SetChannel i v -> onParam i \q -> q { channel = clamp 1 16 (intOr q.channel v) }
  SetAxis i a -> \p ->
    onParam i (\q -> q { axis = clamp 0 (Array.length (Encoding.axes p.encoding) - 1) a }) p
  NextShape i -> onParam i \q -> q { curve = Curve.nextShape q.curve }
  FlipCurve i -> \p -> onParam i (\q -> q { curve = Curve.flipped (sizeOfAxis p q.axis) q.curve }) p
  ToCurve i -> onParam i \q -> q { curve = Named Linear false }
  SetValue i j v -> \p ->
    onParam i
      (\q -> q { curve = Curve.setAt (sizeOfAxis p q.axis) j
                   (Int.toNumber (intOr 0 v) / 100.0) q.curve })
      p
  SetPort v -> \p -> p { port = v }
  SetGate v -> onTrig \t -> t { gate = busOf v }
  SetEs5 v -> onTrig \t -> t { es5 = slotOf v }
  SetGateLevel v -> onTrig \t -> t { gateLevel = level t.gateLevel v }
  SetNote v -> onTrig \t -> t { note = noteOf v }
  SetTrigChannel v -> onTrig \t -> t { channel = clamp 1 16 (intOr t.channel v) }
  SetVelocity v -> onTrig \t -> t { velocity = clamp 1 127 (intOr t.velocity v) }
  SetHold v -> onTrig \t -> t { ms = clamp 1 5000 (intOr t.ms v) }
  SetSettle v -> \p -> p { settleMs = clamp 0 5000 (intOr p.settleMs v) }
  SetSpacing v -> \p -> p { spacingMs = clamp 50 20000 (intOr p.spacingMs v) }
  -- Negative is legal and occasionally right: a module that answers a gate
  -- before the page hears about it is not a thing, but a trigger read late
  -- from a stale snapshot is, and the correction for it is a boundary moved
  -- the other way.
  SetLead v -> \p -> p { leadMs = clamp (-500) 2000 (intOr p.leadMs v) }
  where
  onParam i f p = p { params = fromMaybe p.params (Array.modifyAt i f p.params) }
  onTrig f p = p { trigger = f p.trigger }
  intOr d v = fromMaybe d (Int.fromString v)
  level d v = clamp (-1.0) 1.0 (fromMaybe d (Number.fromString v))
  -- An empty box means "not routed", which has to be distinguishable from bus
  -- zero — and bus zero is a perfectly ordinary place to send something.
  busOf v = if v == "" then Nothing else map (clamp 0 15) (Int.fromString v)
  slotOf v = if v == "" then Nothing else map (clamp 0 7) (Int.fromString v)
  ccOf v = if v == "" then Nothing else map (clamp 0 127) (Int.fromString v)
  noteOf v = if v == "" then Nothing else map (clamp 0 127) (Int.fromString v)

-- | **What one parameter is doing at one cell, in the instrument's own terms.**
-- |
-- | A sample's meaning is not its index. `morph 2.5 V` is a thing you can read
-- | a year later, look up in a manual and set by hand; `position 7 of 12` is
-- | only a fact about a run nobody remembers. The spec holds every one of
-- | these at the moment of capture, so writing them down beside the audio
-- | costs nothing and is the difference between a folder of WAVs and a
-- | library.
-- |
-- | `at` is the curve's own value, 0…1 — resolution-independent, so the same
-- | sample re-run at sixteen positions can still be recognised as the same
-- | point on the same curve. `level` is what actually went on the bus, and
-- | `cc` is `-1` where nothing was sent. Volts are deliberately NOT here: how
-- | many a level is worth is a fact about the interface, stated once for the
-- | whole set, not repeated on every sample as though it could differ.
type Meaning =
  { name :: String
  , at :: Number
  , level :: Number
  , cc :: Int
  }

-- | **One cell, resolved.** What to set, immediately before its hit — and,
-- | for the record afterwards, what that amounts to.
type Step =
  { index :: Int
  , cell :: Cell
  , cv :: Array { bus :: Int, level :: Number }
  , esx :: Array { slot :: Int, level :: Number }
  , cc :: Array { channel :: Int, cc :: Int, value :: Int }
  , means :: Array Meaning
  }

steps :: Plan -> Array Step
steps p = Array.mapWithIndex one (Encoding.cells p.encoding p.extent)
  where
  one i cell =
    { index: i
    , cell
    , cv: Array.mapMaybe (\q -> map (\b -> { bus: b, level: lerp q.cvLo q.cvHi (v q cell) }) q.cv) p.params
    , esx: Array.mapMaybe (\q -> map (\k -> { slot: k, level: lerp q.cvLo q.cvHi (v q cell) }) q.esx) p.params
    , cc: Array.mapMaybe
            (\q -> map
              (\c -> { channel: q.channel, cc: c
                     , value: Int.round (lerp (Int.toNumber q.ccLo) (Int.toNumber q.ccHi) (v q cell)) })
              q.cc)
            p.params
    , means: map
        (\q ->
          { name: q.name
          , at: v q cell
          , level: case q.cv, q.esx of
              Nothing, Nothing -> 0.0
              _, _ -> lerp q.cvLo q.cvHi (v q cell)
          , cc: case q.cc of
              Just _ -> Int.round (lerp (Int.toNumber q.ccLo) (Int.toNumber q.ccHi) (v q cell))
              Nothing -> -1
          })
        p.params
    }
  -- Each parameter reads the cell's position on ITS axis. That is the whole of
  -- what makes a grid legible: a column shares one axis's value, a row the
  -- other's.
  v q cell =
    fromMaybe 0.0 (Array.index (valuesFor p q) (fromMaybe 0 (Array.index cell q.axis)))
  lerp a b t = a + (b - a) * t

-- ---------------------------------------------------------------------------
-- Keeping it across a reload
-- ---------------------------------------------------------------------------

-- | **A plan has to survive a force-reload**, learned the hard way: the second
-- | BIA run captured all twelve hits and every one of them was at the default
-- | spacing, because reloading to pick up a fix had quietly reset the plan that
-- | had just been tuned. The take said so — 0.836 s between triggers, which is
-- | `120 + 700` and nothing else.
-- |
-- | It matters more here than for most settings, because the whole feature IS a
-- | loop of run, listen, bend, run again, and a page you reload between runs is
-- | a loop that keeps starting over.
-- |
-- | Stored flat, with `-1` for "not routed": `Maybe` does not survive a round
-- | trip through `JSON.stringify` in any shape worth defending, and a sentinel
-- | that cannot collide with a real bus number is honest about what it is.
type PlainParam =
  { name :: String
  , cv :: Int, cvLo :: Number, cvHi :: Number
  , esx :: Int
  , cc :: Int, ccLo :: Int, ccHi :: Int
  , channel :: Int
  , axis :: Int
  -- | `"named"` or `"drawn"` — the constructor, written down, because it is the
  -- | difference between a shape that can be resampled and points that can only
  -- | be interpolated.
  , kind :: String
  , shape :: String
  , flipped :: Boolean
  , values :: Array Number
  }

type Plain =
  { encoding :: String
  , extent :: Array Int
  , params :: Array PlainParam
  , gate :: Int
  , es5 :: Int
  , gateLevel :: Number
  , note :: Int
  , trigChannel :: Int
  , velocity :: Int
  , holdMs :: Int
  , port :: String
  , settleMs :: Int
  , spacingMs :: Int
  , leadMs :: Int
  }

foreign import savePlain :: Plain -> Effect Unit
foreign import loadPlain :: Plain -> Effect Plain

-- | **A stored spec, made into a plan** — field-wise over the current default,
-- | so a set written by an older build opens rather than half-opening.
-- |
-- | The same merge `loadPlain` uses, because a plan reaches this page from two
-- | places that must agree: `localStorage`, where it survives a reload, and a
-- | stored sample set, where it survives everything. One merge, so a field
-- | added later cannot arrive as a default down one path and as `undefined`
-- | down the other.
-- |
-- | This is the whole of "re-runnable": what comes back is a plan like any
-- | other, so changing the extent and pressing Run records the same transect
-- | at a resolution nobody chose at the time.
foreign import adoptPlain :: Plain -> Plain -> Plain

adopt :: Plan -> Plain -> Plan
adopt dflt stored = unflatten (adoptPlain (flatten dflt) stored)

remember :: Plan -> Effect Unit
remember = savePlain <<< flatten

restore :: Plan -> Effect Plan
restore d = map unflatten (loadPlain (flatten d))

flatten :: Plan -> Plain
flatten p =
  { encoding: Encoding.name p.encoding
  , extent: p.extent
  , params: map one p.params
  , gate: fromMaybe (-1) p.trigger.gate
  , es5: fromMaybe (-1) p.trigger.es5
  , gateLevel: p.trigger.gateLevel
  , note: fromMaybe (-1) p.trigger.note
  , trigChannel: p.trigger.channel
  , velocity: p.trigger.velocity
  , holdMs: p.trigger.ms
  , port: p.port
  , settleMs: p.settleMs
  , spacingMs: p.spacingMs
  , leadMs: p.leadMs
  }
  where
  one q =
    { name: q.name
    , cv: fromMaybe (-1) q.cv, cvLo: q.cvLo, cvHi: q.cvHi
    , esx: fromMaybe (-1) q.esx
    , cc: fromMaybe (-1) q.cc, ccLo: q.ccLo, ccHi: q.ccHi
    , channel: q.channel
    , axis: q.axis
    , kind: if Curve.isDrawn q.curve then "drawn" else "named"
    , shape: case q.curve of
        Named sh _ -> shapeName sh
        Drawn _ -> shapeName Linear
    , flipped: case q.curve of
        Named _ f -> f
        Drawn _ -> false
    , values: Curve.valuesOf 32 q.curve
    }

unflatten :: Plain -> Plan
unflatten p =
  let
    enc = fromMaybe RampleLayers (Array.find (\e -> Encoding.name e == p.encoding) Encoding.all)
    nAxes = Array.length (Encoding.axes enc)
    ext = Array.mapWithIndex
            (\i _ -> clamp 1 128 (fromMaybe 8 (Array.index p.extent i)))
            (Encoding.axes enc)
  in
    { encoding: enc
    , extent: ext
    , params: map (one nAxes) p.params
    , trigger:
        { gate: some p.gate
        , es5: some p.es5
        , gateLevel: clamp (-1.0) 1.0 p.gateLevel
        , note: some p.note
        , channel: clamp 1 16 p.trigChannel
        , velocity: clamp 1 127 p.velocity
        , ms: clamp 1 5000 p.holdMs
        }
    , port: p.port
    , settleMs: clamp 0 5000 p.settleMs
    , spacingMs: clamp 50 20000 p.spacingMs
    , leadMs: clamp (-500) 2000 p.leadMs
    }
  where
  some n = if n < 0 then Nothing else Just n
  one nAxes q =
    { name: q.name
    , cv: some q.cv, cvLo: clamp (-1.0) 1.0 q.cvLo, cvHi: clamp (-1.0) 1.0 q.cvHi
    , esx: some q.esx
    , cc: some q.cc, ccLo: clamp 0 127 q.ccLo, ccHi: clamp 0 127 q.ccHi
    , channel: clamp 1 16 q.channel
    , axis: clamp 0 (nAxes - 1) q.axis
    , curve:
        if q.kind == "drawn" then Drawn (map (clamp 0.0 1.0) q.values)
        else Named (fromMaybe Linear (shapeOf q.shape)) q.flipped
    }
