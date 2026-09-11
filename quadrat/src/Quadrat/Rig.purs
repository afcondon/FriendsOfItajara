-- | **Reaching the instrument** — the two wires a sweep can go down.
-- |
-- | They are not symmetrical, and the asymmetry is the reason this module
-- | exists rather than one more function in `Http`.
-- |
-- | **MIDI leaves from the browser.** WebMIDI is right here, and the server has
-- | no MIDI at all — `server.mjs` is deliberately zero-dependency and CoreMIDI
-- | from Node is a native module. So a controller change is sent from the page.
-- |
-- | **CV leaves from the server**, because it is OSC over UDP and a browser
-- | cannot open a socket. The server relays to `es9-daemon`, which holds the
-- | ES-9 open and drives sixteen buses.
-- |
-- | ## Neither of them can tell you it arrived
-- |
-- | WebMIDI reports a port it opened, not a module that listened; UDP to the
-- | daemon is fire-and-forget by construction. So nothing in here should ever
-- | be read as confirmation that the instrument moved. **The confirmation is
-- | the audio** — twelve tiles that differ. That is not a shortcoming to work
-- | around later; it is why the measured readout beside the requested values is
-- | part of the feature rather than a nicety.
module Quadrat.Rig
  ( Sent
  , openMidi
  , ports
  , sendCc
  , sendNote
  , setCv
  , pulse
  , es5pulse
  , nowMs
  ) where

import Data.Unit (Unit)

import Control.Promise (Promise)
import Effect (Effect)

-- | **Ask for MIDI, and do not wait for the answer.**
-- |
-- | `requestMIDIAccess` stays pending until a permission prompt in the browser's
-- | own chrome is answered — somewhere the page cannot see, cannot mention and
-- | cannot dismiss. Awaited from a handler it stalls Halogen's action queue, and
-- | the page stops polling the daemon and reads as crashed.
-- |
-- | So this fires the request and resolves with whatever is known a moment
-- | later, which on a first visit is nothing. Empty means no WebMIDI, no answer
-- | yet, a refusal, or genuinely no ports; the CV half is unaffected by all
-- | four.
foreign import openMidi :: Effect (Promise (Array String))

-- | The ports as they stand, asking nothing. Read on the poll while the sweep
-- | is open, so the list fills the moment permission is granted rather than
-- | staying empty until the modal is closed and opened again.
foreign import ports :: Effect (Array String)

-- | `port` is matched as a SUBSTRING of an output's name, the way the rest of
-- | the rig matches ports, so "IAC" finds "IAC Driver Tidal".
foreign import sendCc
  :: { port :: String, channel :: Int, cc :: Int, value :: Int } -> Effect Unit

-- | Note on, and note off `ms` later — the page holds that timer, so a run that
-- | is stopped mid-way still releases what it pressed.
foreign import sendNote
  :: { port :: String, channel :: Int, note :: Int, velocity :: Int, ms :: Int }
  -> Effect Unit

type Sent = { ok :: Boolean, output :: String }

-- | Set every bus at once: one request, one UDP burst, so seven parameters
-- | arrive together rather than smeared across seven round trips.
-- | `esx` rides the same request: the ESX-8CV's eight channels reach the rig
-- | through Silent Way on one expander bus, which es9-daemon enables on first
-- | use. Eight more CVs for the price of bus 4 no longer being raw.
foreign import setCv
  :: { set :: Array { bus :: Int, level :: Number }
     , esx :: Array { slot :: Int, level :: Number }
     } -> Effect (Promise Sent)

-- | A gate: up to `level`, down after `ms`, timed by the daemon in frames
-- | rather than here in milliseconds.
foreign import pulse
  :: { bus :: Int, level :: Number, ms :: Int } -> Effect (Promise Sent)

-- | **An ES-5 gate**, which has no duration of its own — the daemon takes a
-- | bit and a state, so the hold happens in `server.mjs` rather than here,
-- | where a round trip would land inside the gate.
foreign import es5pulse :: { bit :: Int, ms :: Int } -> Effect (Promise Sent)

-- | A monotonic clock in milliseconds, so a run can pace itself by the time it
-- | has already spent rather than by hope. See `Rig.js`.
foreign import nowMs :: Effect Number
