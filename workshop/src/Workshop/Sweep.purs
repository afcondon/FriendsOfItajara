-- | **The sweep: a table of values, not a set of curves.**
-- |
-- | Andrew's reframing, 2026-09-09, and it inverts what the plan said. The
-- | plan had a curve spec first and editable breakpoints as a later refinement.
-- | The right way round is the other one:
-- |
-- |   > "rather than tweaking functions for these discreet values the best
-- |   > thing would be to show n sliders for each parameter with one slider for
-- |   > each position that we're sweeping. We could APPLY a function to each
-- |   > slider set to speed the set up but this would let the user listen to a
-- |   > sweep and tweak any parameter at any point."
-- |
-- | The reason it is right is in what the edit *after listening* looks like.
-- | With seven interacting non-linear parameters the useful thought is almost
-- | always "position 8 is wrong" — a point edit — and no named curve can
-- | express one without bending everything either side of it. So the artefact
-- | is `Array Number` per parameter, one value per position, and an easing is a
-- | way of FILLING that array quickly rather than a way of describing it.
-- |
-- | It is also the smaller thing to store, to diff and to send.
-- |
-- | ## The grid is the same grid
-- |
-- | A parameter is a ROW and a position is a COLUMN, which means a column here
-- | and a tile in `What was caught` are the same index — the sliders above
-- | position 7 are the state the synth was in when the seventh hit was struck.
-- | That alignment is what makes measured-against-requested a matter of reading
-- | down the page rather than a second chart to build.
module Workshop.Sweep
  ( Shape(..)
  , shapes
  , shapeName
  , at
  , Param
  , Trigger
  , Plan
  , emptyPlan
  , Msg(..)
  , update
  , Step
  , steps
  , resample
  ) where

import Prelude

import Data.Array as Array
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number as Number

-- | **Only the monotone families.**
-- |
-- | `purescript-hylograph-selection` has twenty-five of these, and they are the
-- | same arithmetic — but they are ANIMATION easings, and Back, Elastic and
-- | Bounce all overshoot outside 0…1 on purpose, because a menu that springs
-- | past its resting place looks alive. A sweep's value is a voltage into a
-- | module, so an overshoot leaves the range you declared and reaches a place
-- | you never looked at. `at` clamps as well, but the honest fix is not to
-- | offer them.
-- |
-- | The names match hylograph's `EasingType` constructors exactly, so if this
-- | ever wants the full set it is an import rather than a rewrite. (Note that
-- | `applyEasing` lives in `Hylograph.Internal.Transition.Manager` and not in
-- | `Hylograph.Attribute`, which is what the plan said — `Attribute` has the
-- | type but not the function, and Internal is not an API.)
data Shape
  = Linear
  | QuadIn | QuadOut | QuadInOut
  | CubicIn | CubicOut | CubicInOut
  | SinIn | SinOut | SinInOut
  | ExpIn | ExpOut | ExpInOut
  | CircleIn | CircleOut | CircleInOut

derive instance Eq Shape

shapes :: Array Shape
shapes =
  [ Linear
  , QuadIn, QuadOut, QuadInOut
  , CubicIn, CubicOut, CubicInOut
  , SinIn, SinOut, SinInOut
  , ExpIn, ExpOut, ExpInOut
  , CircleIn, CircleOut, CircleInOut
  ]

shapeName :: Shape -> String
shapeName = case _ of
  Linear -> "Linear"
  QuadIn -> "QuadIn"
  QuadOut -> "QuadOut"
  QuadInOut -> "QuadInOut"
  CubicIn -> "CubicIn"
  CubicOut -> "CubicOut"
  CubicInOut -> "CubicInOut"
  SinIn -> "SinIn"
  SinOut -> "SinOut"
  SinInOut -> "SinInOut"
  ExpIn -> "ExpIn"
  ExpOut -> "ExpOut"
  ExpInOut -> "ExpInOut"
  CircleIn -> "CircleIn"
  CircleOut -> "CircleOut"
  CircleInOut -> "CircleInOut"

shapeOf :: String -> Maybe Shape
shapeOf s = Array.find (\x -> shapeName x == s) shapes

-- | The shape at `t`, clamped into the unit interval. Same arithmetic as
-- | hylograph's `applyEasing` for every constructor named here.
at :: Shape -> Number -> Number
at sh t = clamp 0.0 1.0 (raw sh (clamp 0.0 1.0 t))
  where
  raw = case _ of
    Linear -> identity
    QuadIn -> \u -> u * u
    QuadOut -> \u -> u * (2.0 - u)
    QuadInOut -> \u -> if u < 0.5 then 2.0 * u * u else -1.0 + (4.0 - 2.0 * u) * u
    CubicIn -> \u -> u * u * u
    CubicOut -> \u -> let v = u - 1.0 in v * v * v + 1.0
    CubicInOut -> \u ->
      if u < 0.5 then 4.0 * u * u * u
      else (u - 1.0) * (2.0 * u - 2.0) * (2.0 * u - 2.0) + 1.0
    SinIn -> \u -> 1.0 - Number.cos (u * Number.pi / 2.0)
    SinOut -> \u -> Number.sin (u * Number.pi / 2.0)
    SinInOut -> \u -> -(Number.cos (Number.pi * u) - 1.0) / 2.0
    ExpIn -> \u -> if u == 0.0 then 0.0 else Number.pow 2.0 (10.0 * (u - 1.0))
    ExpOut -> \u -> if u == 1.0 then 1.0 else 1.0 - Number.pow 2.0 (-10.0 * u)
    ExpInOut -> \u ->
      if u == 0.0 then 0.0
      else if u == 1.0 then 1.0
      else if u < 0.5 then Number.pow 2.0 (20.0 * u - 10.0) / 2.0
      else (2.0 - Number.pow 2.0 (-20.0 * u + 10.0)) / 2.0
    CircleIn -> \u -> 1.0 - Number.sqrt (1.0 - u * u)
    CircleOut -> \u -> Number.sqrt (1.0 - Number.pow (u - 1.0) 2.0)
    CircleInOut -> \u ->
      if u < 0.5 then (1.0 - Number.sqrt (1.0 - Number.pow (2.0 * u) 2.0)) / 2.0
      else (Number.sqrt (1.0 - Number.pow (-2.0 * u + 2.0) 2.0) + 1.0) / 2.0

-- | **One parameter of the synth, and where it is reached.**
-- |
-- | CV *and* MIDI, not one or the other: some things on the rig answer both, a
-- | BIA parameter that has a CV input also has none over MIDI, and an iPad
-- | synth is CC-only. Leaving both set is legal and means both are sent.
-- |
-- | `values` is the artefact. It is in the unit interval so the shapes apply to
-- | it directly and so the two destinations can scale it differently — the same
-- | sweep is 0…0.5 as a level and 20…110 as a controller.
type Param =
  { name :: String
  -- | An es9-daemon bus, 0…15, or nothing.
  , cv :: Maybe Int
  -- | What 0.0 and 1.0 mean on that bus, as the daemon's own -1…1 level.
  -- |
  -- | **Not halved for you.** `/tidal/cv` multiplies by es9-daemon's
  -- | `SAFETY_SCALE`; the direct `/cv <bus> <val>` path this uses does NOT, so
  -- | 1.0 here is the ES-9's full output. Hence the default range of 0…0.5.
  , cvLo :: Number
  , cvHi :: Number
  -- | A controller number, 0…127, or nothing.
  , cc :: Maybe Int
  , ccLo :: Int
  , ccHi :: Int
  -- | The MIDI channel this parameter's CC goes out on, 1…16.
  , channel :: Int
  -- | One per position, in the unit interval. **This is the thing you keep.**
  , values :: Array Number
  -- | What the Apply button would fill `values` with. A pending choice, never a
  -- | claim about what is in there — the first drag of a slider would make such
  -- | a claim false and nothing would say so.
  , seed :: Shape
  }

-- | **What actually makes the sound happen**, once the parameters have settled.
type Trigger =
  { gate :: Maybe Int      -- an es9-daemon bus to pulse
  , gateLevel :: Number    -- how high the pulse goes, on the same -1…1 scale
  , note :: Maybe Int      -- a MIDI note to play
  , channel :: Int
  , velocity :: Int
  , ms :: Int              -- how long it is held
  }

type Plan =
  { positions :: Int
  , params :: Array Param
  , trigger :: Trigger
  -- | A substring of a WebMIDI output's name, matched the way the rest of the
  -- | rig matches ports. Empty means MIDI is not used at all.
  , port :: String
  -- | **How long after setting the parameters before the trigger.**
  -- |
  -- | Unmeasured, and it is the one number here that can silently ruin a run:
  -- | too short and the hit is struck while the CVs are still on their way, so
  -- | position 7 sounds like a blend of 6 and 7. es9-daemon smooths per channel
  -- | with a first-order IIR, so "arrived" is asymptotic rather than a moment.
  , settleMs :: Int
  -- | Trigger to next trigger. Long enough for the sound to finish, plus enough
  -- | silence that the divider can see the join.
  , spacingMs :: Int
  }

-- | Twelve positions and one parameter, which is the smallest thing that can
-- | still fail visibly: one row, rising, and if the twelve tiles come out
-- | identical then nothing is reaching the module.
emptyPlan :: Plan
emptyPlan =
  { positions: 12
  , params: [ param "morph" 0 ]
  , trigger: { gate: Just 8, gateLevel: 0.5, note: Nothing, channel: 1, velocity: 100, ms: 10 }
  , port: ""
  , settleMs: 120
  , spacingMs: 700
  }

param :: String -> Int -> Param
param nm bus =
  { name: nm
  , cv: Just bus, cvLo: 0.0, cvHi: 0.5
  , cc: Nothing, ccLo: 0, ccHi: 127
  , channel: 1
  , values: rampOf 12
  , seed: Linear
  }

rampOf :: Int -> Array Number
rampOf n = valuesFor Linear n

valuesFor :: Shape -> Int -> Array Number
valuesFor sh n
  | n <= 1 = [ at sh 1.0 ]
  | otherwise = map
      (\i -> at sh (Int.toNumber i / Int.toNumber (n - 1)))
      (Array.range 0 (n - 1))

-- | **Changing the number of positions keeps the shape you drew.**
-- |
-- | Truncating would silently drop the top of a sweep and padding would repeat
-- | its end; interpolating keeps what a hand edit meant, which is the only
-- | thing in here that could not be recreated by pressing a button.
resample :: Int -> Array Number -> Array Number
resample n xs
  | n <= 0 = []
  | Array.length xs == n = xs
  | otherwise = case Array.length xs of
      0 -> Array.replicate n 0.0
      1 -> Array.replicate n (fromMaybe 0.0 (Array.head xs))
      m
        | n == 1 -> [ fromMaybe 0.0 (Array.last xs) ]
        | otherwise -> map
            (\i -> lerpAt xs
                     (Int.toNumber i / Int.toNumber (n - 1) * Int.toNumber (m - 1)))
            (Array.range 0 (n - 1))

lerpAt :: Array Number -> Number -> Number
lerpAt xs u =
  let
    i = Int.floor u
    f = u - Int.toNumber i
    a = fromMaybe 0.0 (Array.index xs i)
    b = fromMaybe a (Array.index xs (i + 1))
  in
    a + (b - a) * f

data Msg
  = SetPositions String
  | AddParam
  | DropParam Int
  | SetName Int String
  | SetCv Int String
  | SetCvLo Int String
  | SetCvHi Int String
  | SetCc Int String
  | SetCcLo Int String
  | SetCcHi Int String
  | SetChannel Int String
  | SetValue Int Int String
  | SetSeed Int String
  | ApplySeed Int
  | FlipRow Int
  | SetPort String
  | SetGate String
  | SetGateLevel String
  | SetNote String
  | SetTrigChannel String
  | SetVelocity String
  | SetHold String
  | SetSettle String
  | SetSpacing String

update :: Msg -> Plan -> Plan
update = case _ of
  SetPositions v -> \p ->
    let n = clamp 2 24 (int p.positions v)
    in p { positions = n, params = map (\q -> q { values = resample n q.values }) p.params }
  AddParam -> \p ->
    -- Seven is the FH-2's spare jacks and so the number the whole idea was
    -- sized to; it is not a law, but past it a row stops fitting the screen
    -- and the grid stops being readable, which is the actual limit.
    if Array.length p.params >= 7 then p
    else p { params = Array.snoc p.params
               (param ("param " <> show (Array.length p.params + 1))
                      (Array.length p.params))
                 { values = rampOf p.positions } }
  DropParam i -> \p -> p { params = fromMaybe p.params (Array.deleteAt i p.params) }
  SetName i v -> onParam i \q -> q { name = v }
  SetCv i v -> onParam i \q -> q { cv = busOf v }
  SetCvLo i v -> onParam i \q -> q { cvLo = level q.cvLo v }
  SetCvHi i v -> onParam i \q -> q { cvHi = level q.cvHi v }
  SetCc i v -> onParam i \q -> q { cc = ccOf v }
  SetCcLo i v -> onParam i \q -> q { ccLo = clamp 0 127 (intOr q.ccLo v) }
  SetCcHi i v -> onParam i \q -> q { ccHi = clamp 0 127 (intOr q.ccHi v) }
  SetChannel i v -> onParam i \q -> q { channel = clamp 1 16 (intOr q.channel v) }
  SetValue i j v -> onParam i \q ->
    q { values = fromMaybe q.values
          (Array.updateAt j (clamp 0.0 1.0 (Int.toNumber (intOr 0 v) / 100.0)) q.values) }
  SetSeed i v -> onParam i \q -> q { seed = fromMaybe q.seed (shapeOf v) }
  ApplySeed i -> \p -> onParam i (\q -> q { values = valuesFor q.seed p.positions }) p
  FlipRow i -> onParam i \q -> q { values = Array.reverse q.values }
  SetPort v -> \p -> p { port = v }
  SetGate v -> onTrig \t -> t { gate = busOf v }
  SetGateLevel v -> onTrig \t -> t { gateLevel = level t.gateLevel v }
  SetNote v -> onTrig \t -> t { note = noteOf v }
  SetTrigChannel v -> onTrig \t -> t { channel = clamp 1 16 (intOr t.channel v) }
  SetVelocity v -> onTrig \t -> t { velocity = clamp 1 127 (intOr t.velocity v) }
  SetHold v -> onTrig \t -> t { ms = clamp 1 5000 (intOr t.ms v) }
  SetSettle v -> \p -> p { settleMs = clamp 0 5000 (int p.settleMs v) }
  SetSpacing v -> \p -> p { spacingMs = clamp 50 20000 (int p.spacingMs v) }
  where
  onParam i f p = p { params = fromMaybe p.params (Array.modifyAt i f p.params) }
  onTrig f p = p { trigger = f p.trigger }
  int d v = intOr d v
  intOr d v = fromMaybe d (Int.fromString v)
  level d v = clamp (-1.0) 1.0 (fromMaybe d (Number.fromString v))
  -- An empty box means "not routed", which has to be distinguishable from bus
  -- zero — and bus zero is a perfectly ordinary place to send something.
  busOf v = if v == "" then Nothing else map (clamp 0 15) (Int.fromString v)
  ccOf v = if v == "" then Nothing else map (clamp 0 127) (Int.fromString v)
  noteOf v = if v == "" then Nothing else map (clamp 0 127) (Int.fromString v)

-- | **One position, resolved.** What to set, immediately before hit `index`.
type Step =
  { index :: Int
  , cv :: Array { bus :: Int, level :: Number }
  , cc :: Array { channel :: Int, cc :: Int, value :: Int }
  }

steps :: Plan -> Array Step
steps p = map one (Array.range 0 (p.positions - 1))
  where
  one i =
    { index: i
    , cv: Array.mapMaybe (\q -> map (\b -> { bus: b, level: lerp q.cvLo q.cvHi (v q i) }) q.cv) p.params
    , cc: Array.mapMaybe
            (\q -> map
              (\c -> { channel: q.channel, cc: c
                     , value: Int.round (lerp (Int.toNumber q.ccLo) (Int.toNumber q.ccHi) (v q i)) })
              q.cc)
            p.params
    }
  v q i = fromMaybe 0.0 (Array.index q.values i)
  lerp a b t = a + (b - a) * t
