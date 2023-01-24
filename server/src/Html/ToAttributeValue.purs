module Html.ToAttributeValue
  ( class ToAttributeValue
  , toAttributeValue
  ) where

import Data.Function (identity, (#))
import Data.Functor (map)
import Data.Map as Map
import Data.Semigroup ((<>))
import Data.Show (show)
import Data.String as String
import Data.Tuple (Tuple(..))
import Html.Types (DataMap(..))

class ToAttributeValue a where
  toAttributeValue :: a -> String

instance toAttributeValueString :: ToAttributeValue String where
  toAttributeValue = identity

instance toAttributeValueInt :: ToAttributeValue Int where
  toAttributeValue = show

instance toAttributeValueBoolean :: ToAttributeValue Boolean where
  toAttributeValue = show

instance toAttributeValueNumber :: ToAttributeValue Number where
  toAttributeValue = show

instance toAttributeValueDataMap :: ToAttributeValue DataMap where
  toAttributeValue (DataMap m) =
    let
      pairs = m # Map.toUnfoldable # map (\(Tuple k v) -> "data-" <> k <> "=\"" <> v <> "\"")
    in
      String.joinWith " " pairs
