module Main where

import Prelude

import Chatterbox.Common.Types (ServerMessage(..))
import Effect (Effect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.VDom.Driver as VDomDriver

main :: Effect Unit
main = do
  HA.runHalogenAff do
    body <- HA.awaitBody
    VDomDriver.runUI mainComponent unit body

type State = { count :: Int }

data Action = Increment | Decrement

mainComponent :: forall query input output m. H.Component query input output m
mainComponent = H.mkComponent { initialState, render, eval }
  where
  initialState _ = { count: 0 }
  render { count } = HH.div_
    [ HH.button [ HE.onClick \_ -> Decrement ] [ HH.text "-" ]
    , HH.text $ show count
    , HH.button [ HE.onClick \_ -> Increment ] [ HH.text "+" ]
    , HH.text $ show $ EchoMessage "Hello"
    ]
  eval = H.mkEval H.defaultEval { handleAction = handleAction }
  handleAction Increment = do
    H.modify_ \s -> s { count = s.count + 1 }
    pure unit
  handleAction Decrement = do
    H.modify_ \s -> s { count = s.count - 1 }
    pure unit
