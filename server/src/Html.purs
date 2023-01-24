module Html where

import Prelude hiding ((+))

import Data.Foldable (fold, foldMap)
import Html.AttributeRecord (class AttributeRecord, makeAttributeValues)
import Html.Types (Children, DataMap, Html(..), HtmlAttribute(..))

-- | A row type describing attributes common to all HTML elements.
type CommonAttributes =
  ( accesskey :: String
  , class :: String
  , contenteditable :: Boolean
  , contextmenu :: String
  , dir :: String
  , draggable :: Boolean
  , dropzone :: String
  , hidden :: Boolean
  , id :: String
  , lang :: String
  , spellcheck :: Boolean
  , style :: String
  , tabindex :: Int
  , title :: String
  , translate :: String
  , data :: DataMap
  )

html5 :: Html -> Html -> Html
html5 head' body' = HtmlNode "html" [] [ head', body' ]

head :: Children -> Html
head children = HtmlNode "head" [] children

body :: forall as as' r. AttributeRecord as as' r CommonAttributes => Record as -> Children -> Html
body as = HtmlNode "body" (makeAttributeValues as)

text :: String -> Html
text = HtmlText

type AnchorAttributes =
  ( download :: String
  , href :: String
  , hreflang :: String
  , media :: String
  , ping :: String
  , rel :: String
  , target :: String
  , type :: String
  | CommonAttributes
  )

a :: forall as as' r. AttributeRecord as as' r AnchorAttributes => Record as -> Children -> Html
a attributes = HtmlNode "a" (makeAttributeValues attributes)

span :: forall as as' r. AttributeRecord as as' r CommonAttributes => Record as -> Children -> Html
span attributes = HtmlNode "span" (makeAttributeValues attributes)

li :: forall as as' r. AttributeRecord as as' r CommonAttributes => Record as -> Children -> Html
li attributes = HtmlNode "li" (makeAttributeValues attributes)

title :: forall as as' r. AttributeRecord as as' r CommonAttributes => Record as -> Children -> Html
title attributes = HtmlNode "title" (makeAttributeValues attributes)

h1 :: forall as as' r. AttributeRecord as as' r CommonAttributes => Record as -> Children -> Html
h1 attributes = HtmlNode "h1" (makeAttributeValues attributes)

h2 :: forall as as' r. AttributeRecord as as' r CommonAttributes => Record as -> Children -> Html
h2 attributes = HtmlNode "h2" (makeAttributeValues attributes)

h3 :: forall as as' r. AttributeRecord as as' r CommonAttributes => Record as -> Children -> Html
h3 attributes = HtmlNode "h3" (makeAttributeValues attributes)

h4 :: forall as as' r. AttributeRecord as as' r CommonAttributes => Record as -> Children -> Html
h4 attributes = HtmlNode "h4" (makeAttributeValues attributes)

h5 :: forall as as' r. AttributeRecord as as' r CommonAttributes => Record as -> Children -> Html
h5 attributes = HtmlNode "h5" (makeAttributeValues attributes)

h6 :: forall as as' r. AttributeRecord as as' r CommonAttributes => Record as -> Children -> Html
h6 attributes = HtmlNode "h6" (makeAttributeValues attributes)

p :: forall as as' r. AttributeRecord as as' r CommonAttributes => Record as -> Children -> Html
p attributes = HtmlNode "p" (makeAttributeValues attributes)

ul :: forall as as' r. AttributeRecord as as' r CommonAttributes => Record as -> Children -> Html
ul attributes = HtmlNode "ul" (makeAttributeValues attributes)

div :: forall as as' r. AttributeRecord as as' r CommonAttributes => Record as -> Children -> Html
div attributes = HtmlNode "div" (makeAttributeValues attributes)

type MetaAttributes =
  ( property :: String
  , content :: String
  , charset :: String
  , name :: String
  , "http-equiv" :: String
  )

nav :: forall as as' r. AttributeRecord as as' r CommonAttributes => Record as -> Children -> Html
nav attributes = HtmlNode "nav" (makeAttributeValues attributes)

meta :: forall as as' r. AttributeRecord as as' r MetaAttributes => Record as -> Html
meta attributes = HtmlLeaf "meta" (makeAttributeValues attributes)

type LinkAttributes =
  ( crossorigin :: String
  , href :: String
  , hreflang :: String
  , media :: String
  , referrerpolicy :: String
  , rel :: String
  , sizes :: String
  , title :: String
  , type :: String
  | CommonAttributes
  )

link :: forall as as' r. AttributeRecord as as' r LinkAttributes => Record as -> Html
link attributes = HtmlLeaf "link" (makeAttributeValues attributes)

type SvgAttributes =
  ( xmlns :: String
  , "xmlns:xlink" :: String
  , version :: String
  , width :: String
  , height :: String
  , viewBox :: String
  , preserveAspectRatio :: String
  | CommonAttributes
  )

svg :: forall as as' r. AttributeRecord as as' r SvgAttributes => Record as -> Children -> Html
svg attributes = HtmlNode "svg" (makeAttributeValues attributes)

type ScriptAttributes =
  ( async :: Boolean
  , charset :: String
  , crossorigin :: String
  , defer :: Boolean
  , integrity :: String
  , language :: String
  , src :: String
  , type :: String
  )

script :: forall as as' r. AttributeRecord as as' r ScriptAttributes => Record as -> Children -> Html
script attributes = HtmlNode "script" (makeAttributeValues attributes)

render :: Html -> String
render (HtmlNode "html" attributes children) =
  fold
    [ "<!doctype html>"
    , "<html"
    , foldMap renderAttribute attributes
    , ">"
    , foldMap render children
    , "</html>"
    ]
render (HtmlNode name attributes children) =
  fold
    [ "<"
    , name
    , " "
    , foldMap renderAttribute attributes
    , ">"
    , foldMap render children
    , "</"
    , name
    , ">"
    ]
render (HtmlText text') = text'
render (HtmlLeaf name attributes) =
  fold
    [ "<"
    , name
    , " "
    , foldMap renderAttribute attributes
    , " />"
    ]

renderAttribute :: HtmlAttribute -> String
renderAttribute (HtmlAttribute "" value) = value
renderAttribute (HtmlAttribute name' value) = name' <> "=\"" <> value <> "\""

