-- | **Where the sounds are, because we put them there.**
-- |
-- | Quadrat has two ways to divide a take, and they are not two settings
-- | of one thing. A take you *played* has to be listened to: `msm onset` finds
-- | the attacks and you keep the ones you meant. A take the rig **ran** does
-- | not, because the page issued every trigger and already knows when.
-- |
-- | **Declared beats inferred**, and here the argument is quantitative. A
-- | detector is very good at twelve hits and its errors are still recoverable
-- | by eye. At 192 — a 12 x 16 grid, which the encodings allow — one missed
-- | onset shifts every sample after it, every label goes with it, and nothing
-- | downstream can tell a shifted set from an intended one. That is the silent
-- | failure the unattended half of this tool exists to design against: you come
-- | back to 192 samples and one is wrong.
-- |
-- | ## The one number that is not known
-- |
-- | Two clocks are in play. The page issues a trigger on its own clock; the
-- | take is measured in frames by the daemon. the capture's own frame count joins
-- | them — the daemon says how much it has laid down, so reading it at the
-- | instant of a trigger puts that trigger *in take time* directly. The snapshot is up to a
-- | frame or two of its 30 Hz old, which `snapshotAge` corrects for.
-- |
-- | What remains is the lag from issuing a trigger to the sound arriving back:
-- | UDP to es9-daemon, its next audio callback, the module's own attack, the
-- | converter round trip. Small, constant within a take, and **not** worth
-- | inferring — so it is a number on the plan, `leadMs`, subtracted from every
-- | boundary. Set it too small and the attack is clipped; too large and each
-- | sample carries silence in front of it. Both are visible in the tiles, which
-- | is the point: nothing here has to be trusted.
-- |
-- | ## Boundaries, not windows
-- |
-- | Each region runs from one trigger to the next. A decaying sound is not cut
-- | short by a window we guessed at, and the only thing that can end it is the
-- | next hit — which is exactly right, because the next hit is the only thing
-- | that actually does end it.
module Quadrat.Schedule
  ( at
  , slots
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Foreign.LooperSocket as Socket

-- | **How far into the take the recording has got, in seconds** — or nothing,
-- | if nothing is capturing right now.
-- |
-- | `Nothing` rather than zero, and the caller falls back to the detector.
-- | Zero would be a lie of exactly the wrong kind: a schedule of twelve marks
-- | all at the head of the take divides it into eleven empty regions and one
-- | long one, which is a set of measurements that looks like a run that went
-- | wrong rather than like a division that never happened.
-- |
-- | It read a loop's `recFrames` until 2026-09-10, with a guard for whether
-- | that loop was laying a first take down as opposed to overdubbing, sized,
-- | armed or empty. A capture is only ever recording or not.
at :: Effect (Maybe Number)
at = do
  snap <- Socket.latest
  age <- Socket.snapshotAge
  pure do
    top <- snap
    if not top.capture.on then Nothing
      else Just (top.capture.secs + max 0.0 age / 1000.0)

-- | **Trigger times into regions**, each running to the next trigger.
-- |
-- | `lead` is in seconds and is subtracted from every boundary, so the region
-- | opens a little before the sound the trigger caused. The last region is
-- | given the middle gap of the others, which is a description of a trigger
-- | that never happened and so names a moment a little past the end of the
-- | take; `msm` clamps it to the audio rather than refusing it.
-- |
-- | Fewer than two marks is not a division, and comes back empty.
slots :: Number -> Array Number -> Array { start :: Number, end :: Number }
slots lead marks
  | Array.length marks < 2 = []
  | otherwise =
      let
        edges = map (\m -> m - lead) marks
        gaps = Array.zipWith (-) (Array.drop 1 edges) edges
        -- The middle gap, not the mean and not the last: a run stopped by hand
        -- leaves one long interval, and a mean would stretch every region by a
        -- share of it.
        middle = case Array.index (Array.sort gaps) (Array.length gaps / 2) of
          Just g | g > 0.0 -> g
          _ -> 1.0
        ends = Array.snoc (Array.drop 1 edges)
                 (case Array.last edges of
                    Just e -> e + middle
                    Nothing -> middle)
      in
        Array.zipWith (\s e -> { start: max 0.0 s, end: e }) edges ends
