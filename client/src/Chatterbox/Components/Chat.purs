module Chatterbox.Components.Chat
  ( Input
  , Output
  , component
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.String (Pattern(..), Replacement(..))
import Data.String as String
import Data.Traversable (traverse_)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.Event.Event (Event)
import Web.Event.Event as Event
import Web.HTML as Html
import Web.HTML.Location as Location
import Web.HTML.Window as Window
import Web.Socket.WebSocket (WebSocket)
import Web.Socket.WebSocket as WebSocket

type Input = { username :: String }

type Output = Void

newtype State = State StateRecord

derive instance newtypeState :: Newtype State _

type StateRecord =
  { username :: String
  , messages :: Array String
  , socket :: Maybe WebSocket
  , currentMessage :: String
  }

data Action
  = Initialize
  | Finalize
  | SetCurrentMessage String
  | SendMessage String Event

component :: forall query m. MonadAff m => H.Component query Input Output m
component =
  H.mkComponent
    { initialState: \{ username } ->
        State { username, messages: [], socket: Nothing, currentMessage: "" }
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction, initialize = Just Initialize, finalize = Just Finalize }
    }
  where
  render :: State -> H.ComponentHTML Action () m
  render (State { messages, currentMessage }) =
    HH.div_
      [ HH.h1_ [ HH.text "Chatterbox" ]
      , HH.div_ [ HH.textarea [ HP.value $ String.joinWith "\n" messages, HP.readOnly true ] ]
      , HH.form [ HE.onSubmit \e -> SendMessage currentMessage e ]
          [ HH.input
              [ HP.value currentMessage
              , HP.placeholder "Type a message..."
              , HE.onValueInput SetCurrentMessage
              ]
          ]
      ]

  handleAction :: Action -> H.HalogenM State Action () Output m Unit
  handleAction Initialize = do
    socket <- connectToWebSocket
    modify_ $ _ { socket = Just socket }
    pure unit
  handleAction Finalize = do
    socket <- gets _.socket
    liftEffect $ traverse_ WebSocket.close socket
  handleAction (SetCurrentMessage message) =
    modify_ $ _ { currentMessage = message }
  handleAction (SendMessage message e) = do
    liftEffect $ Event.preventDefault e
    modify_ $ _ { currentMessage = "" }
    socket <- gets _.socket
    liftEffect $ traverse_ (\s -> WebSocket.sendString s message) socket

connectToWebSocket :: forall m. MonadEffect m => m WebSocket
connectToWebSocket = do
  baseUrl <- getOrigin
  let url = Array.fold [ String.replace (Pattern "http") (Replacement "ws") baseUrl, "/ws" ]
  liftEffect $ WebSocket.create url []

modify_ :: forall m r state. Newtype state r => (r -> r) -> H.HalogenM state Action () Output m Unit
modify_ f = H.modify_ (unwrap >>> f >>> wrap)

gets :: forall m state r a. Newtype state r => (r -> a) -> H.HalogenM state Action () Output m a
gets f = H.gets (unwrap >>> f)

getOrigin :: forall m. MonadEffect m => m String
getOrigin = liftEffect $ Html.window >>= Window.location >>= Location.origin
