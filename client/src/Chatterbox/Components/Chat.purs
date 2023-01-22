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
import Data.Either (Either(..), hush)
import Data.List.NonEmpty as NonEmptyList
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.String (Pattern(..), Replacement(..))
import Data.String as String
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (traverse, traverse_)
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
import Simple.JSON (class ReadForeign, class WriteForeign)
import Simple.JSON as Json
import Web.DOM.Element as Element
import Web.Event.Event (Event)
import Web.Event.Event as Event
import Web.Event.EventTarget as EventTarget
import Web.HTML as Html
import Web.HTML.HTMLElement as HtmlElement
import Web.HTML.Location as Location
import Web.HTML.Window as Window
import Web.Socket.Event.EventTypes as WebSocketEventTypes
import Web.Socket.Event.MessageEvent as MessageEvent
import Web.Socket.ReadyState as ReadyState
import Web.Socket.WebSocket (WebSocket)
import Web.Socket.WebSocket as WebSocket
import Web.Storage.Storage as LocalStorage

type Input = { user :: User }

type Output = Void

newtype State = State StateRecord

derive instance newtypeState :: Newtype State _

type StateRecord =
  { user :: User
  , events :: Map Channel (Array ChannelEvent)
  , currentChannel :: Maybe Channel
  , socket :: Maybe WebSocket
  , currentMessage :: String
  , webSocketSubscription :: Maybe H.SubscriptionId
  }

data Action
  = Initialize
  | Finalize
  | Receive Input
  | SetCurrentMessage { message :: String }
  | SendCurrentMessage { message :: String, event :: Event }
  | SocketEvent { event :: ServerMessage }

instance showAction :: Show Action where
  show Initialize = "Initialize"
  show Finalize = "Finalize"
  show (Receive input) = "Receive " <> show input
  show (SetCurrentMessage r) = "SetCurrentMessage " <> show r
  show (SendCurrentMessage { message }) = "SendCurrentMessage " <> show { message, event: "<event>" }
  show (SocketEvent r) = "SocketEvent " <> show r

component :: forall query m. MonadAff m => H.Component query Input Output m
component =
  H.mkComponent
    { initialState: \{ user } ->
        State
          { user
          , events: Map.empty
          , socket: Nothing
          , currentMessage: ""
          , webSocketSubscription: Nothing
          , currentChannel: Nothing
          }
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        , finalize = Just Finalize
        , receive = Receive >>> Just
        }
    }
  where
  render :: State -> H.ComponentHTML Action () m
  render (State { events, currentMessage, currentChannel }) = do
    let channelEvents = fromMaybe [] $ currentChannel >>= \c -> Map.lookup c events
    HH.div [ "chat-window" # wrap # HP.class_ ]
      [ HH.h1_ [ HH.text "Chatterbox" ]
      , HH.h2_ [ currentChannel # map unwrap # fromMaybe "No channel selected" # HH.text ]
      , HH.div [ "chat-components" # wrap # HP.class_ ]
          [ HH.textarea
              [ channelEvents
                  # map renderChannelEvent
                  # String.joinWith "\n"
                  # HP.value
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
  renderChannelEvent (UserRenamed { oldName, newName }) =
    unwrap oldName <> " is now known as " <> unwrap newName

  handleAction :: Action -> H.HalogenM State Action () Output m Unit
  handleAction Initialize = do
    socket <- connectToWebSocket
    modify_ $ _ { socket = Just socket }
    user <- gets _.user
    channelsToJoin <- getChannelsToJoin
    sendInitialCommands socket user channelsToJoin
    modify_ $ _ { currentChannel = Array.head channelsToJoin }
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
  handleAction (SocketEvent { event: ChannelMessage { channel, event } }) = do
    modify_ $ \s -> s
      { events = Map.insertWith (<>) channel [ event ] s.events }
    scrollMessagesToBottom
  handleAction (SocketEvent {}) = do
    pure unit
  handleAction (Receive { user }) = do
    modify_ $ _ { user = user }
    socket <- gets _.socket
    traverse_ (\s -> sendOnReady s [ SetUsername { user } ]) socket

  scrollMessagesToBottom = do
    messageBox <- "message-box" # wrap # H.getHTMLElementRef
    liftEffect $ traverse_ (HtmlElement.toElement >>> scrollToBottom) messageBox

  scrollToBottom e = do
    scrollHeight <- Element.scrollHeight e
    Element.setScrollTop scrollHeight e

getChannelsToJoin :: forall m. MonadAff m => m (Array Channel)
getChannelsToJoin = do
  fromMaybe [ Channel "general" ] <$> getFromLocalStorage "chatterbox-auto-join"

getFromLocalStorage :: forall m a. MonadAff m => ReadForeign a => String -> m (Maybe a)
getFromLocalStorage key = do
  window <- liftEffect Html.window
  storage <- liftEffect $ Window.localStorage window
  maybeItem <- storage # LocalStorage.getItem key # liftEffect
  maybeItem
    # traverse (Json.readJSON >>> hush)
    # join
    # pure

subscribeToSocketEvents
  :: forall m message action state output childSlots
   . MonadAff m
  => ReadForeign message
  => WebSocket
  -> (message -> action)
  -> H.HalogenM state action childSlots output m H.SubscriptionId
subscribeToSocketEvents socket f = do
  emitter <- webSocketEmitter socket f
  H.subscribe emitter

webSocketEmitter
  :: forall m message action
   . MonadAff m
  => ReadForeign message
  => WebSocket
  -> (message -> action)
  -> m (HS.Emitter action)
webSocketEmitter socket f = do
  { emitter, listener } <- liftEffect $ HS.create
  let socketEventTarget = WebSocket.toEventTarget socket
  eventListener <- liftEffect $ EventTarget.eventListener \e -> do
    case MessageEvent.fromEvent e of
      Just messageEvent -> do
        let data_ = MessageEvent.data_ messageEvent
        if Foreign.tagOf data_ == "String" then do
          case data_ # Foreign.unsafeFromForeign # Json.readJSON of
            Right message -> do
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

sendInitialCommands :: forall m. MonadAff m => WebSocket -> User -> Array Channel -> m Unit
sendInitialCommands socket user channels = do
  let channelJoins = map (\channel -> JoinChannel { user, channel }) channels
  [ [ SetUsername { user } ], channelJoins ] # Array.fold # sendOnReady socket

sendOnReady :: forall m a. MonadAff m => WriteForeign a => WebSocket -> Array a -> m Unit
sendOnReady socket messages = do
  readyState <- liftEffect $ WebSocket.readyState socket
  case readyState of
    ReadyState.Open -> do
      traverse_ (Json.writeJSON >>> WebSocket.sendString socket >>> liftEffect) messages
    _ -> do
      liftAff $ Aff.delay $ Milliseconds 25.0
      sendOnReady socket messages

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
