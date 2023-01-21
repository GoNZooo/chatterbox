module Chatterbox.Components.Chat
  ( Input
  , Output
  , component
  ) where

import Prelude

import Chatterbox.Common.Types
  ( Channel(..)
  , ChannelEvent(..)
  , ClientMessage(..)
  , ServerMessage(..)
  , User
  )
import Data.Array as Array
import Data.Either (Either(..))
import Data.List.NonEmpty as NonEmptyList
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.String (Pattern(..), Replacement(..))
import Data.String as String
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (traverse_)
import Effect.Aff as Aff
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console as Console
import Foreign as Foreign
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Subscription as HS
import Simple.JSON as Json
import Web.DOM.Element as Element
import Web.Event.Event (Event)
import Web.Event.Event as Event
import Web.Event.EventTarget as EventTarget
import Web.HTML as Html
import Web.HTML.Location as Location
import Web.HTML.Window as Window
import Web.Socket.Event.EventTypes as WebSocketEventTypes
import Web.Socket.Event.MessageEvent as MessageEvent
import Web.Socket.ReadyState as ReadyState
import Web.Socket.WebSocket (WebSocket)
import Web.Socket.WebSocket as WebSocket
import Web.HTML.HTMLElement as HtmlElement

type Input = { user :: User }

type Output = Void

newtype State = State StateRecord

derive instance newtypeState :: Newtype State _

type StateRecord =
  { user :: User
  , events :: Array ChannelEvent
  , socket :: Maybe WebSocket
  , currentMessage :: String
  , webSocketSubscription :: Maybe H.SubscriptionId
  }

data Action
  = Initialize
  | Finalize
  | SetCurrentMessage { message :: String }
  | SendCurrentMessage { message :: String, event :: Event }
  | SocketEvent { event :: ServerMessage }

instance showAction :: Show Action where
  show Initialize = "Initialize"
  show Finalize = "Finalize"
  show (SetCurrentMessage r) = "SetCurrentMessage " <> show r
  show (SendCurrentMessage { message }) = "SendCurrentMessage " <> show { message, event: "<event>" }
  show (SocketEvent r) = "SocketEvent " <> show r

component :: forall query m. MonadAff m => H.Component query Input Output m
component =
  H.mkComponent
    { initialState: \{ user } ->
        State
          { user, events: [], socket: Nothing, currentMessage: "", webSocketSubscription: Nothing }
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction, initialize = Just Initialize, finalize = Just Finalize }
    }
  where
  render :: State -> H.ComponentHTML Action () m
  render (State { events, currentMessage }) =
    HH.div [ "chat-window" # wrap # HP.class_ ]
      [ HH.h1_ [ HH.text "Chatterbox" ]
      , HH.div [ "chat-components" # wrap # HP.class_ ]
          [ HH.textarea
              [ events # map renderChannelEvent # String.joinWith "\n" # HP.value
              , HP.readOnly true
              , "message-box" # wrap # HP.class_
              , "message-box" # wrap # HP.ref
              ]
          , HH.form
              [ HE.onSubmit \e -> SendCurrentMessage { message: currentMessage, event: e }
              , "message-form" # wrap # HP.class_
              ]
              [ HH.input
                  [ HP.value currentMessage
                  , HP.placeholder "Type a message..."
                  , HE.onValueInput \message -> SetCurrentMessage { message }
                  , HP.autofocus true
                  ]
              ]
          ]
      ]

  renderChannelEvent :: ChannelEvent -> String
  renderChannelEvent (ChannelJoined { user, channel }) = unwrap user <> " joined " <> unwrap channel
  renderChannelEvent (ChannelLeft { user, channel }) = unwrap user <> " left " <> unwrap channel
  renderChannelEvent (ChannelMessageSent { user, message }) =
    unwrap user <> ": " <> message

  handleAction :: Action -> H.HalogenM State Action () Output m Unit
  handleAction Initialize = do
    socket <- connectToWebSocket
    modify_ $ _ { socket = Just socket }
    user <- gets _.user
    sendSetUsernameOnReady socket user
    subscription <- subscribeToSocketEvents socket \event -> SocketEvent { event }
    modify_ $ _ { webSocketSubscription = Just subscription }
  handleAction Finalize = do
    socket <- gets _.socket
    liftEffect $ traverse_ WebSocket.close socket
  handleAction (SetCurrentMessage { message }) =
    modify_ $ _ { currentMessage = message }
  handleAction (SendCurrentMessage { message, event: e }) = do
    liftEffect $ Event.preventDefault e
    modify_ $ _ { currentMessage = "" }
    socket <- gets _.socket
    liftEffect $
      traverse_
        ( \s -> { channel: Channel "general", message } # SendMessage # Json.writeJSON #
            WebSocket.sendString s
        )
        socket
    scrollMessagesToBottom
  handleAction (SocketEvent { event: ChannelMessage { event } }) = do
    modify_ $ \s -> s { events = Array.snoc s.events event }
    scrollMessagesToBottom
  handleAction (SocketEvent {}) = do
    pure unit

  scrollMessagesToBottom = do
    messageBox <- "message-box" # wrap # H.getHTMLElementRef
    liftEffect $ traverse_ (HtmlElement.toElement >>> scrollToBottom) messageBox

  scrollToBottom e = do
    scrollHeight <- Element.scrollHeight e
    Element.setScrollTop scrollHeight e

subscribeToSocketEvents
  :: forall m
   . MonadAff m
  => WebSocket
  -> (ServerMessage -> Action)
  -> H.HalogenM State Action () Output m H.SubscriptionId
subscribeToSocketEvents socket f = do
  emitter <- webSocketEmitter socket f
  H.subscribe emitter

webSocketEmitter
  :: forall m
   . MonadAff m
  => WebSocket
  -> (ServerMessage -> Action)
  -> m (HS.Emitter Action)
webSocketEmitter socket f = do
  { emitter, listener } <- liftEffect $ HS.create
  let socketEventTarget = WebSocket.toEventTarget socket
  eventListener <- liftEffect $ EventTarget.eventListener \e -> do
    case MessageEvent.fromEvent e of
      Just messageEvent -> do
        let data_ = MessageEvent.data_ messageEvent
        if Foreign.tagOf data_ == "String" then do
          case data_ # Foreign.unsafeFromForeign # Json.readJSON of
            Right (message :: ServerMessage) -> do
              message # f # HS.notify listener # liftEffect
            Left error -> do
              Console.error $ NonEmptyList.foldMap Foreign.renderForeignError error
              pure unit
        else
          pure unit
      Nothing -> do
        pure unit
  _ <- liftEffect $
    EventTarget.addEventListener WebSocketEventTypes.onMessage eventListener false socketEventTarget
  pure emitter

sendSetUsernameOnReady :: forall m. MonadAff m => WebSocket -> User -> m Unit
sendSetUsernameOnReady socket user = do
  readyState <- liftEffect $ WebSocket.readyState socket
  case readyState of
    ReadyState.Open -> do
      { user } # SetUsername # Json.writeJSON # WebSocket.sendString socket # liftEffect
    _ -> do
      liftAff $ Aff.delay $ Milliseconds 100.0
      sendSetUsernameOnReady socket user

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
