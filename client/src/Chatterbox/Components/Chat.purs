module Chatterbox.Components.Chat
  ( Input
  , Output
  , component
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), Replacement(..))
import Data.String as String
import Data.Traversable (traverse_)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Web.HTML as Html
import Web.HTML.Location as Location
import Web.HTML.Window as Window
import Web.Socket.WebSocket (WebSocket)
import Web.Socket.WebSocket as WebSocket

type Input = { username :: String }

type Output = Void

type State = { username :: String, messages :: Array String, socket :: Maybe WebSocket }

data Action
  = Initialize
  | Finalize

component :: forall query m. MonadAff m => H.Component query Input Output m
component =
  H.mkComponent
    { initialState: \{ username } -> { username, messages: [], socket: Nothing }
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction, initialize = Just Initialize, finalize = Just Finalize }
    }
  where
  render :: State -> H.ComponentHTML Action () m
  render { username } =
    HH.div_ [ HH.text $ "Hello " <> username ]

  handleAction :: Action -> H.HalogenM State Action () Output m Unit
  handleAction Initialize = do
    socket <- connectToWebSocket
    H.modify_ _ { socket = Just socket }
    pure unit
  handleAction Finalize = do
    socket <- H.gets _.socket
    liftEffect $ traverse_ WebSocket.close socket

connectToWebSocket :: forall m. MonadEffect m => m WebSocket
connectToWebSocket = do
  baseUrl <- getOrigin
  let url = Array.fold [ String.replace (Pattern "http") (Replacement "ws") baseUrl, "/ws" ]
  liftEffect $ WebSocket.create url []

getOrigin :: forall m. MonadEffect m => m String
getOrigin = liftEffect $ Html.window >>= Window.location >>= Location.origin
