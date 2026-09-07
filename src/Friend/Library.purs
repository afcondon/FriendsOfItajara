-- | **The sample libraries, as the page sees them.**
-- |
-- | A library is a directory of scenes; a scene is any directory holding
-- | audio. The server discovers that shape rather than being told it, so the
-- | same three columns read the takes we recorded, Instruo's v2 stick image,
-- | and any folder of samples put in `~/.itajara/libraries.json`.
-- |
-- | **This is the naming side only.** A library addresses by name and can hold
-- | as many scenes as it likes; a module's stick addresses positionally and
-- | holds exactly what it holds — six banks of thirty-six, and thirty-six
-- | scenes. `msm harvest` is the compiler from the one to the other, and
-- | nothing here should learn to speak in slots: a browser that pretends the
-- | stick is browsable by name teaches the wrong model of the module.
module Friend.Library
  ( Shelf
  , Scene
  , Layer
  , Text
  , SceneInfo
  , emptyScene
  , listLibrary
  , sceneInfo
  , audioUrl
  ) where

import Control.Promise (Promise)
import Effect (Effect)

-- | One group of scenes under one library — Instruo's `_arbhar_scenes`, or a
-- | take's eight loops. The name is the directory's own, unprettified: half of
-- | what the browser is for is learning to recognise `_arbhar_library_4` when
-- | the module shows it to you.
type Shelf =
  { id :: String
  , lib :: String
  , libName :: String
  , group :: String
  , name :: String
  , scenes :: Array Scene
  }

-- | A scene, with its audio by name only. Durations and formats cost a header
-- | read each, so they arrive from `sceneInfo` when a scene is opened.
type Scene =
  { path :: String
  , name :: String
  , layers :: Array String
  }

-- | One audio file, as its own header describes it. A zero in any field means
-- | the header did not say; the page prints nothing rather than a lie.
type Layer =
  { name :: String
  , secs :: Number
  , rate :: Int
  , bits :: Int
  , channels :: Int
  , bytes :: Number
  }

-- | A text file beside the audio. For an Arbhar scene this is the preset, and
-- | its *name* is the preset's name — `arbharClassic.txt` — so both halves
-- | are worth showing.
type Text = { name :: String, content :: String }

type SceneInfo = { layers :: Array Layer, texts :: Array Text }

emptyScene :: SceneInfo
emptyScene = { layers: [], texts: [] }

foreign import listLibraryImpl :: Effect (Promise (Array Shelf))
foreign import sceneInfoImpl :: String -> String -> Effect (Promise SceneInfo)

-- | Every library's shelves and scenes, names only.
listLibrary :: Effect (Promise (Array Shelf))
listLibrary = listLibraryImpl

-- | One scene's audio and texts, by library id and path within it.
sceneInfo :: String -> String -> Effect (Promise SceneInfo)
sceneInfo = sceneInfoImpl

-- | Where a file is, for an `<audio>` element. The server answers byte ranges
-- | on this, which is what lets a media element seek — and what Safari
-- | requires before it will play at all.
foreign import audioUrl :: String -> String -> String
