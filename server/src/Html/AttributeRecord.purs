module Html.AttributeRecord
  ( makeAttributeValues
  , MakeAttributeValues
  , class AttributeRecord
  ) where

import Data.Array as Array
import Data.Eq ((==))
import Data.Semigroup ((<>))
import Data.Symbol (class IsSymbol)
import Data.Symbol as Symbol
import Heterogeneous.Folding
  ( class FoldingWithIndex
  , class FoldlRecord
  , class HFoldlWithIndex
  , hfoldlWithIndex
  )
import Html.ToAttributeValue (class ToAttributeValue, toAttributeValue)
import Html.Types (Attributes, HtmlAttribute(..))
import Prim.Row (class Union)
import Prim.RowList (class RowToList)
import Type.Prelude (Proxy)

data MakeAttributeValues = MakeAttributeValues

instance showAttributeValues ::
  ( ToAttributeValue a
  , IsSymbol symbol
  ) =>
  FoldingWithIndex MakeAttributeValues (Proxy symbol) Attributes a Attributes where
  foldingWithIndex MakeAttributeValues key acc value =
    let
      symbolString = Symbol.reflectSymbol key
      attributeValue = toAttributeValue value
    in
      if symbolString == "data" then
        acc <> Array.singleton (HtmlAttribute "" attributeValue)
      else
        acc <> Array.singleton (HtmlAttribute (Symbol.reflectSymbol key) (toAttributeValue value))

makeAttributeValues
  :: forall r
   . HFoldlWithIndex MakeAttributeValues Attributes { | r } Attributes
  => { | r }
  -> Attributes
makeAttributeValues = hfoldlWithIndex MakeAttributeValues ([] :: Attributes)

class
  ( Union attributes attributes' possibleValues
  , RowToList attributes r
  , FoldlRecord MakeAttributeValues Attributes r attributes Attributes
  ) <=
  AttributeRecord attributes attributes' r possibleValues

instance attributeRecord ::
  ( Union attributes attributes' possibleValues
  , RowToList attributes r
  , FoldlRecord MakeAttributeValues Attributes r attributes Attributes
  ) =>
  AttributeRecord attributes attributes' r possibleValues
