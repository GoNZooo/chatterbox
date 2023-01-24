module Html.Types where

import Data.Map (Map)

data Html
  = HtmlNode String Attributes Children
  | HtmlText String
  | HtmlLeaf String Attributes

data HtmlAttribute = HtmlAttribute String String

type Attributes = Array HtmlAttribute

type Children = Array Html

newtype DataMap = DataMap (Map String String)

