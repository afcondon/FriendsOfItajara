-- | The modules that exist, by the key Marquetry gives each.
-- |
-- | Marquetry keeps the inventory and publishes a short form of it to
-- | Amphora (`marquetry/tools/publish_inventory.py`): content of kind
-- | `marquetry-inventory` under the ref `marquetry/modules`. This reads it,
-- | so that a page can offer "which module is this?" from what is actually
-- | owned, and so that two units of one model stay distinct.
module Quadrat.Inventory
  ( Module
  , modules
  , said
  , keyOf
  ) where

import Prelude

import Amphora.Api as Api
import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe, fromMaybe)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Hawser.Fetch as Fetch
import Hawser.Message (field, recordOf)
import Hawser.Value (Value, arrayOf, fromText, string)
import Quadrat.Tables as Tables

type Module = { key :: String, maker :: String, model :: String, status :: String }

inventory :: Value (Array Module)
inventory = arrayOf $ recordOf "a module" $
  { key: _, maker: _, model: _, status: _ }
    <$> field "key" _.key string "Marquetry's key for this physical module"
    <*> field "maker" _.maker string "who made it"
    <*> field "model" _.model string "what it is"
    <*> field "status" _.status string "keep, gone, for_sale and so on"

-- | The modules still here, in the order of how they are said. Empty if the
-- | inventory has not been published or the store does not answer: the page
-- | works without it, with the module typed by hand.
modules :: Aff (Array Module)
modules = do
  base <- liftEffect Tables.storeBase
  let store = { base, timeoutMs: 2500 }
  Fetch.ask store Api.getRef { name: "marquetry/modules" } >>= case _ of
    Left _ -> pure []
    Right { hash } -> Fetch.ask store Api.getContent { hash } <#> case _ of
      Right content -> Array.sortWith said (Array.filter (\owned -> owned.status /= "gone") (fromMaybe [] (fromText inventory content.payload)))
      Left _ -> []

-- | A module as a person would say it, with its key, so that two units of
-- | one model read differently: "Mutable Instruments Plaits [mutable-instruments-plaits-2]".
said :: Module -> String
said owned = owned.maker <> " " <> owned.model <> " [" <> owned.key <> "]"

-- | The key of the module said this way, if it is one of them.
keyOf :: Array Module -> String -> Maybe String
keyOf owned text = Array.find (\candidate -> said candidate == text) owned <#> _.key
