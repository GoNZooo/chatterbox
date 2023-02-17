module Chatterbox.Components.Chat
  ( Input
  , Output
  , component
  ) where

import Prelude

import Chatterbox.Common.Types (Channel(..), ChannelEvent(..), ServerMessage, User)
import Chatterbox.Common.Types as ClientMessage
import Chatterbox.Common.Types as ServerMessage
import Chatterbox.Components.Settings as Settings
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Data.Array as Array
import Data.Either (Either(..), hush)
import Data.List.NonEmpty as NonEmptyList
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Set (Set)
import Data.Set as Set
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
import Type.Proxy (Proxy(..))
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

type ChildSlots = (settings :: forall query. H.Slot query Settings.Output Unit)

newtype State = State StateRecord

derive instance newtypeState :: Newtype State _

_settings = Proxy :: Proxy "settings"

type StateRecord =
  { user :: User
  , events :: Map Channel (Array ChannelEvent)
  , currentChannel :: Maybe Channel
  , socket :: Maybe WebSocket
  , currentMessage :: String
  , webSocketSubscription :: Maybe H.SubscriptionId
  , users :: Map Channel (Set User)
  }

data Action
  = Initialize
  | Finalize
  | Receive Input
  | SetCurrentMessage { message :: String }
  | SendCurrentMessage { message :: String }
  | SocketEvent { event :: ServerMessage }
  | SelectChannel { channel :: Channel }
  | SettingsOutput Settings.Output
  | SubmitTextInput { input :: String, event :: Event }
  | JoinChannel { channel :: Channel }
  | LeaveChannel { channel :: Channel }
  | ChangeUser { user :: User }

instance showAction :: Show Action where
  show Initialize = "Initialize"
  show Finalize = "Finalize"
  show (Receive input) = "Receive " <> show input
  show (SetCurrentMessage r) = "SetCurrentMessage " <> show r
  show (SendCurrentMessage { message }) = "SendCurrentMessage " <> show { message }
  show (SocketEvent r) = "SocketEvent " <> show r
  show (SelectChannel r) = "SelectChannel " <> show r
  show (SettingsOutput r) = "SettingsOutput " <> show r
  show (SubmitTextInput { input }) = "SubmitTextInput " <> show { input, event: "<event>" }
  show (JoinChannel r) = "JoinChannel " <> show r
  show (LeaveChannel r) = "LeaveChannel " <> show r
  show (ChangeUser r) = "ChangeUser " <> show r

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
          , users: Map.empty
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
  render :: State -> H.ComponentHTML Action ChildSlots m
  render (State { events, currentMessage, currentChannel, user, users }) = do
    let
      channelEvents = fromMaybe [] $ currentChannel >>= \c -> Map.lookup c events
      channelUsers = fromMaybe Set.empty $ currentChannel >>= \c -> Map.lookup c users
    HH.div [ "chat-window" # wrap # HP.class_ ]
      [ HH.h1_ [ HH.text "Chatterbox" ]
      , HH.slot _settings unit Settings.component { username: unwrap user } SettingsOutput
      , HH.h2_ [ currentChannel # map unwrap # fromMaybe "No channel selected" # HH.text ]
      , HH.div [ "channel-select-box" # wrap # HP.class_ ] $
          map renderSelectChannel (events # Map.keys # Set.toUnfoldable)
      , HH.div [ "chat-wrapper" # wrap # HP.class_ ]
          [ HH.div [ "chat-components" # wrap # HP.class_ ]
              [ renderChatMessages channelEvents
              , HH.form
                  [ HE.onSubmit \e -> SubmitTextInput { input: currentMessage, event: e }
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
          , renderChatUsers channelUsers
          ]
      ]

  renderChatMessages :: forall slots. Array ChannelEvent -> H.ComponentHTML Action slots m
  renderChatMessages events =
    HH.pre
      [ "message-box" # wrap # HP.class_, "message-box" # wrap # HP.ref ] $
      events # map renderChannelEvent

  renderChatUsers :: forall slots. Set User -> H.ComponentHTML Action slots m
  renderChatUsers users =
    HH.div [ "user-list" # wrap # HP.class_ ]
      [ HH.h3_ [ HH.text "Users" ]
      , HH.ul_ $ users # Set.toUnfoldable # map \user -> HH.li_ [ HH.text $ unwrap user ]
      ]

  renderChannelEvent :: forall slots. ChannelEvent -> H.ComponentHTML Action slots m
  renderChannelEvent (ChannelJoined { user, channel }) =
    HH.div [ "join-message" # wrap # HP.class_ ]
      [ renderUser user, HH.text $ " joined " <> unwrap channel ]
  renderChannelEvent (ChannelLeft { user, channel }) =
    HH.div [ "leave-message" # wrap # HP.class_ ]
      [ renderUser user, HH.text $ " left " <> unwrap channel ]
  renderChannelEvent (ChannelMessageSent { user, message }) =
    HH.div [ "chat-message" # wrap # HP.class_ ]
      [ renderUser user, HH.text $ ": " <> message ]
  renderChannelEvent (UserRenamed { oldName, newName }) =
    HH.div [ "rename-message" # wrap # HP.class_ ]
      [ renderUser oldName, HH.text $ " is now known as ", renderUser newName ]

  renderSelectChannel :: forall slots. Channel -> H.ComponentHTML Action slots m
  renderSelectChannel channel =
    HH.button
      [ HE.onClick \_ -> SelectChannel { channel }
      , "channel-select-button" # wrap # HP.class_
      ]
      [ channel # unwrap # HH.text ]

  renderUser :: forall slots. User -> H.ComponentHTML Action slots m
  renderUser user = HH.span [ "chat-user" # wrap # HP.class_ ] [ HH.text $ unwrap user ]

  handleAction :: forall slots. Action -> H.HalogenM State Action slots Output m Unit
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
  handleAction (SendCurrentMessage { message }) = do
    modify_ $ _ { currentMessage = "" }
    { socket: maybeSocket, currentChannel: maybeChannel } <- get
    void $ runMaybeT do
      socket <- MaybeT $ pure maybeSocket
      channel <- MaybeT $ pure maybeChannel
      { channel, message } # ClientMessage.SendMessage # sendString socket
    scrollMessagesToBottom
  handleAction (SocketEvent { event }) = handleServerMessage event
  handleAction (Receive { user }) = do
    modify_ $ _ { user = user }
    socket <- gets _.socket
    traverse_ (\s -> sendOnReady s [ ClientMessage.SetUsername { user } ]) socket
  handleAction (SelectChannel { channel }) = do
    modify_ $ _ { currentChannel = Just channel }
    scrollMessagesToBottom
  handleAction (SettingsOutput { username }) = do
    let user = wrap username
    modify_ $ _ { user = user }
    socket <- gets _.socket
    traverse_ (\s -> sendOnReady s [ ClientMessage.SetUsername { user } ]) socket
  handleAction (SubmitTextInput { input: "", event: e }) = do
    liftEffect $ Event.preventDefault e
  handleAction (SubmitTextInput { input, event: e }) = do
    liftEffect $ Event.preventDefault e
    modify_ _ { currentMessage = "" }
    user <- gets _.user
    let actions = interpretInput user input
    traverse_ handleAction actions
  handleAction (JoinChannel { channel }) = do
    { socket: maybeSocket, user } <- get
    traverse_ (\s -> sendOnReady s [ ClientMessage.JoinChannel { channel, user } ]) maybeSocket
    modify_ $ _ { currentChannel = Just channel }
    scrollMessagesToBottom
  handleAction (LeaveChannel { channel }) = do
    { socket: maybeSocket, user } <- get
    traverse_ (\s -> sendOnReady s [ ClientMessage.LeaveChannel { channel, user } ]) maybeSocket
  handleAction (ChangeUser { user }) = do
    { socket: maybeSocket } <- get
    traverse_ (\s -> sendOnReady s [ ClientMessage.SetUsername { user } ]) maybeSocket
    modify_ $ _ { user = user }

  handleServerMessage :: forall slots. ServerMessage -> H.HalogenM State Action slots Output m Unit
  handleServerMessage (ServerMessage.ChannelMessage { channel, event }) =
    handleChannelEvent channel event
  handleServerMessage (ServerMessage.UserMessage {}) =
    pure unit
  handleServerMessage (ServerMessage.ChannelPopulation { channel, users }) = do
    modify_ $ \s -> s { users = Map.insert channel (Set.fromFoldable users) s.users }
    pure unit
  handleServerMessage ServerMessage.SendPing =
    pure unit

  handleChannelEvent
    :: forall slots
     . Channel
    -> ChannelEvent
    -> H.HalogenM State Action slots Output m Unit
  handleChannelEvent channel event@(ChannelJoined { user }) = do
    { users, events } <- get
    let newUsersInChannel = Map.insertWith (<>) channel (Set.fromFoldable [ user ]) users
    Console.logShow newUsersInChannel
    modify_ $ _ { users = newUsersInChannel, events = Map.insertWith (<>) channel [ event ] events }
  handleChannelEvent channel event@(ChannelLeft { user }) = do
    { user: ourUser, currentChannel, events, users } <- get
    when (user == ourUser) do
      modify_ $ _ { events = Map.delete channel events }
      when (currentChannel == Just channel) do
        modify_ $ _ { currentChannel = events # Map.keys # Array.fromFoldable # Array.head }
    let newUsersInChannel = Map.alter (map (Set.delete user)) channel users
    modify_ $ _ { users = newUsersInChannel, events = Map.insertWith (<>) channel [ event ] events }
  handleChannelEvent channel event@(UserRenamed { oldName, newName }) = do
    { users, events } <- get
    let
      newUsersInChannel =
        Map.alter
          ( map
              ( Array.fromFoldable
                  >>> map (\u -> if u == oldName then newName else u)
                  >>> Set.fromFoldable
              )
          )
          channel
          users
    modify_ $ _ { events = Map.insertWith (<>) channel [ event ] events, users = newUsersInChannel }
  handleChannelEvent channel event@(ChannelMessageSent {}) = do
    modify_ $ \s -> s { events = Map.insertWith (<>) channel [ event ] s.events }
    scrollMessagesToBottom

  interpretInput :: User -> String -> Array Action
  interpretInput _user input = do
    let
      splits = String.split (Pattern " ") input
      command = Array.head splits
      arguments = splits # Array.tail # fromMaybe []
    case command of
      Just "/join" -> arguments # parseJoinChannel # map JoinChannel # Array.fromFoldable
      Just "/j" -> arguments # parseJoinChannel # map JoinChannel # Array.fromFoldable
      Just "/leave" -> arguments # parseLeaveChannel # map LeaveChannel # Array.fromFoldable
      Just "/l" -> arguments # parseLeaveChannel # map LeaveChannel # Array.fromFoldable
      Just "/nick" -> arguments # parseChangeUser # map ChangeUser # Array.fromFoldable
      Just "/name" -> arguments # parseChangeUser # map ChangeUser # Array.fromFoldable
      Just "/username" -> arguments # parseChangeUser # map ChangeUser # Array.fromFoldable
      Just "/user" -> arguments # parseChangeUser # map ChangeUser # Array.fromFoldable
      _ -> [ SendCurrentMessage { message: input } ]

  parseJoinChannel :: Array String -> Maybe { channel :: Channel }
  parseJoinChannel arguments = do
    channel <- Array.head arguments
    pure { channel: wrap channel }

  parseLeaveChannel :: Array String -> Maybe { channel :: Channel }
  parseLeaveChannel arguments = do
    channel <- Array.head arguments
    pure { channel: wrap channel }

  parseChangeUser :: Array String -> Maybe { user :: User }
  parseChangeUser arguments = do
    user <- Array.head arguments
    pure { user: wrap user }

  scrollMessagesToBottom :: forall slots. H.HalogenM State Action slots Output m Unit
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
  let channelJoins = map (\channel -> ClientMessage.JoinChannel { user, channel }) channels
  [ [ ClientMessage.SetUsername { user } ], channelJoins ] # Array.fold # sendOnReady socket

sendString :: forall m a. MonadEffect m => WriteForeign a => WebSocket -> a -> m Unit
sendString socket a = do
  a # Json.writeJSON # WebSocket.sendString socket # liftEffect

sendOnReady :: forall m a. MonadAff m => WriteForeign a => WebSocket -> Array a -> m Unit
sendOnReady socket messages = do
  readyState <- liftEffect $ WebSocket.readyState socket
  case readyState of
    ReadyState.Open -> do
      traverse_ (sendString socket) messages
    _ -> do
      liftAff $ Aff.delay $ Milliseconds 25.0
      sendOnReady socket messages

connectToWebSocket :: forall m. MonadEffect m => m WebSocket
connectToWebSocket = do
  baseUrl <- getOrigin
  let url = Array.fold [ String.replace (Pattern "http") (Replacement "ws") baseUrl, "/ws" ]
  liftEffect $ WebSocket.create url []

modify_ :: forall m r state slots. Newtype state r => (r -> r) -> H.HalogenM state Action slots Output m Unit
modify_ f = H.modify_ (unwrap >>> f >>> wrap)

gets :: forall m state r a slots. Newtype state r => (r -> a) -> H.HalogenM state Action slots Output m a
gets f = H.gets (unwrap >>> f)

get :: forall m state r slots. Newtype state r => H.HalogenM state Action slots Output m r
get = H.gets unwrap

getOrigin :: forall m. MonadEffect m => m String
getOrigin = liftEffect $ Html.window >>= Window.location >>= Location.origin
