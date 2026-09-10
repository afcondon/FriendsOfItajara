-- | **A name for a take, made without asking.**
-- |
-- | The field used to start life holding `kick` and stay there, so a second
-- | take silently overwrote the first — the raw audio of a session's worth of
-- | drum hits was lost that way, to a name nobody had thought about since
-- | typing it an hour earlier. A default that destroys is worse than no
-- | default, and "remember to rename it" is not a design.
-- |
-- | So every take gets a fresh name at the moment it is armed: what it is,
-- | and when. `chord-hits-0909-115143`. It sorts, it says what it holds, it
-- | cannot collide with yesterday's, and it is still a plain editable string
-- | for when you would rather call it something.
module Quadrat.Slug (slugFor) where

import Prelude

import Effect (Effect)
import Quadrat.Kind (Kind)
import Quadrat.Kind as Kind

-- A nullary effect, which is exactly what a JS thunk is.
foreign import _stamp :: Effect String

-- | `<kind>-<MMDD>-<HHMMSS>` — the kind first, because that is what you scan
-- | a directory listing for.
slugFor :: Kind -> Effect String
slugFor k = do
  t <- _stamp
  pure (Kind.name k <> "-" <> t)
