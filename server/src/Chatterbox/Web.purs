module Chatterbox.Web where

import Prelude hiding ((/))

import Chatterbox.Channel as ChannelBus
import Chatterbox.ChannelCounter as ChannelCounter
import Chatterbox.Common.Types (ChannelEvent(..), ClientMessage(..), ServerMessage(..))
import Chatterbox.Types (WebsocketState(..))
import Chatterbox.User as UserBus
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.Generic.Rep (class Generic)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromJust)
import Data.Newtype (unwrap)
import Data.Show.Generic (genericShow)
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (for_)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console as Console
import Erl.Atom (atom)
import Erl.Cowboy.Handlers.WebSocket (Frame(..))
import Erl.Cowboy.Req (Req)
import Erl.Data.Binary.IOData (IOData)
import Erl.Data.Binary.IOData as IOData
import Erl.Data.Binary.UTF8 as Utf8Binary
import Erl.Data.List (nil, (:))
import Erl.Data.Tuple (Tuple2, tuple2)
import Erl.Kernel.Inet (Port(..), ip4)
import Erl.Process (class HasSelf)
import Html as H
import Html.Types (Html)
import Logger as Logger
import Partial.Unsafe (unsafePartial)
import Pinto.GenServer (InitFn, InitResult(..), ServerPid, ServerType)
import Pinto.GenServer as GenServer
import Pinto.GenServer.Helpers as GenServerHelpers
import Pinto.Timer (TimerRef)
import Pinto.Timer as Timer
import Pinto.Types (RegistryName(..), StartLinkResult)
import Routing.Duplex (RouteDuplex')
import Routing.Duplex as Routing
import Routing.Duplex.Generic as GenericDuplex
import Routing.Duplex.Generic.Syntax ((/))
import Simple.JSON as Json
import Stetson as Stetson
import Stetson.Rest as Rest
import Stetson.Types (RestResult, StaticAssetLocation(..), StetsonHandler, routeHandler)
import Stetson.WebSocket as WebSocket
import Unsafe.Coerce as Unsafe

type State = {}

type StartArgs = {}

data Route
  = Index
  | Static (Array String)
  | Websocket

derive instance genericRoute :: Generic Route _

instance showRoute :: Show Route where
  show = genericShow

serverName :: RegistryName (ServerType Unit Unit Unit State)
serverName = "Chatterbox.Web" # atom # Local

startLink :: StartArgs -> Effect (StartLinkResult (ServerPid Unit Unit Unit State))
startLink args =
  GenServer.startLink $ (GenServer.defaultSpec $ init args) { name = Just serverName }

restart :: Effect Unit
restart = GenServerHelpers.exit (Unsafe.unsafeCoerce serverName) $ atom "brutal_kill"

routes :: RouteDuplex' Route
routes =
  ( Routing.path "" $ GenericDuplex.sum
      { "Index": GenericDuplex.noArgs
      , "Static": "static" / Routing.rest
      , "Websocket": "ws" / GenericDuplex.noArgs
      }
  )

init :: StartArgs -> InitFn Unit Unit Unit State
init _args = do
  ( Stetson.configure
      { bindPort = Port 4200
      , bindAddress = unsafePartial fromJust $ ip4 0 0 0 0
      , routes = Stetson.routes2 routes
          { "Index": indexHandler
          , "Static": PrivDir "chatterbox" "static"
          , "Websocket": websocketHandler
          }
      }
  )
    # Stetson.startClear "Chatterbox.Web"
    # liftEffect
    # void
  pure $ InitOk {}

indexHandler :: StetsonHandler Unit Html
indexHandler =
  routeHandler
    { init: \r -> do
        Logger.info { domain: atom "http" : atom "get" : nil, type: Logger.Trace }
          { message: "GET /" }
        Rest.initResult r indexPage
    , allowedMethods: \r s -> Rest.result (Array.toUnfoldable [ Stetson.GET ]) r s
    , contentTypesProvided: \r s -> Rest.result (htmlWriter : nil) r s
    }

websocketHandler :: StetsonHandler ServerMessage WebsocketState
websocketHandler =
  routeHandler
    { init: \r ->
        WebSocket.initResult r $ WebsocketState
          { channels: Map.empty
          , user: Nothing
          , lastPing: Nothing
          , pingTimerRef: Nothing
          , userSubscriptionRef: Nothing
          , users: Map.empty
          }
    , wsInit
    , wsHandle
    , wsInfo
    , terminate
    }
  where
  wsInit (WebsocketState state) = do
    liftEffect $ Logger.debug { domain: atom "ws" : atom "init" : nil, type: Logger.Trace }
      { message: "Websocket init" }
    timerRef <- schedulePingMessage
    pure $ Stetson.NoReply $ WebsocketState $ state { pingTimerRef = Just timerRef }

  -- Handles messages from the client that is connected to the websocket
  wsHandle frame (WebsocketState state@{ pingTimerRef }) = do
    liftEffect $ traverse_ Timer.cancel pingTimerRef
    timerRef <- schedulePingMessage
    liftEffect $ Logger.debug { domain: atom "websocket" : atom "Frame" : nil, type: Logger.Trace }
      { message: "Received frame: " <> frameToString frame }
    handleFrame frame state timerRef

  -- Like a classic `handle_info`; handles arbitrary messages sent to the process. This will be
  -- used to react to messages that come in from subscriptions.
  wsInfo SendPing state = do
    liftEffect $ Logger.debug { domain: atom "websocket" : atom "SendPing" : nil, type: Logger.Trace }
      { message: "Sending ping" }
    pure $ Stetson.Reply ((PingFrame $ Utf8Binary.toBinary "42") : nil) state

  wsInfo (ChannelMessage { event, channel }) state = do
    liftEffect $ Logger.debug
      { domain: atom "websocket" : atom "ChannelMessage" : nil, type: Logger.Trace }
      { message: "Sending channel message: " <> show event <> " in " <> unwrap channel }
    pure $ Stetson.Reply ((TextFrame $ Json.writeJSON $ ChannelMessage { event, channel }) : nil)
      state

  wsInfo message state = do
    liftEffect $ Logger.debug { domain: atom "websocket" : atom "info" : nil, type: Logger.Trace }
      { message: "Received info: " <> show message }
    pure $ Stetson.Reply ((TextFrame $ Json.writeJSON message) : nil) state

  handleFrame (TextFrame text) state timerRef = do
    liftEffect $ Logger.debug { domain: atom "websocket" : atom "text" : nil, type: Logger.Trace }
      { message: "Received text: " <> text }
    case Json.readJSON text of
      Left err -> do
        liftEffect $ Logger.error
          { domain: atom "websocket" : atom "frame" : nil, type: Logger.Trace }
          { message: "Failed to parse frame: ", error: err }
        pure $ Stetson.NoReply $ WebsocketState state
      Right (clientMessage :: ClientMessage) -> do
        handleClientMessage clientMessage state timerRef
  handleFrame (PingFrame binary) state timerRef =
    pure $ Stetson.Reply ((PongFrame binary) : nil) $ WebsocketState $ state
      { pingTimerRef = Just timerRef }
  handleFrame (PongFrame _) state timerRef =
    pure $ Stetson.NoReply $ WebsocketState $ state { pingTimerRef = Just timerRef }
  handleFrame otherFrame state timerRef = do
    liftEffect $ Logger.warning { domain: atom "websocket" : atom "frame" : nil, type: Logger.Trace }
      { message: "Received unknown frame: " <> frameToString otherFrame }
    pure $ Stetson.NoReply $ WebsocketState $ state { pingTimerRef = Just timerRef }

  handleClientMessage
    (SetUsername { user })
    state@{ channels, user: Just oldUser, userSubscriptionRef: oldUserSubscriptionRef }
    timerRef = do
    liftEffect $ Logger.debug { domain: atom "websocket" : atom "message" : nil, type: Logger.Trace }
      { message: "Set username for existing user: " <> show user }
    traverse_ UserBus.unsubscribe oldUserSubscriptionRef
    userSubscriptionRef <- UserBus.subscribe user \event -> UserMessage { event }
    for_ (Map.keys channels) \c ->
      ChannelBus.send c $ UserRenamed { oldName: oldUser, newName: user }
    pure $ Stetson.NoReply $ WebsocketState $
      state
        { user = Just user
        , userSubscriptionRef = Just userSubscriptionRef
        , pingTimerRef = Just timerRef
        }

  handleClientMessage
    (SetUsername { user })
    state@{ user: Nothing, userSubscriptionRef: oldUserSubscriptionRef }
    timerRef = do
    liftEffect $ Logger.debug { domain: atom "websocket" : atom "message" : nil, type: Logger.Trace }
      { message: "Set username: " <> show user }
    traverse_ UserBus.unsubscribe oldUserSubscriptionRef
    userSubscriptionRef <- UserBus.subscribe user \event -> UserMessage { event }
    pure $ Stetson.NoReply $ WebsocketState $
      state
        { user = Just user
        , pingTimerRef = Just timerRef
        , userSubscriptionRef = Just userSubscriptionRef
        }

  handleClientMessage (SendMessage { channel, message }) state@{ user } timerRef = do
    liftEffect $ Logger.debug { domain: atom "websocket" : atom "message" : nil, type: Logger.Trace }
      { message: "Send message: " <> message <> " to channel: " <> unwrap channel }
    traverse_ (\u -> ChannelBus.send channel $ ChannelMessageSent { channel, message, user: u }) user
    pure $ Stetson.NoReply $ WebsocketState $ state { pingTimerRef = Just timerRef }

  handleClientMessage (JoinChannel { user, channel }) state@{ users } timerRef = do
    _pid <- liftEffect $ ChannelCounter.start channel
    liftEffect $ Logger.debug { domain: atom "websocket" : atom "message" : nil, type: Logger.Trace }
      { message: "Join channel: " <> show channel }
    subscriptionRef <- ChannelBus.subscribe channel \event -> ChannelMessage { event, channel }
    ChannelBus.send channel $ ChannelJoined { channel, user }
    usersInChannel <- liftEffect $ ChannelCounter.getUsers channel
    Console.log $ "Users in channel: " <> show usersInChannel

    let newUsers = Map.insert channel usersInChannel users
    Console.log $ "New users JOIN: " <> show newUsers
    let newChannels = Map.insert channel subscriptionRef state.channels
    pure $ Stetson.NoReply $ WebsocketState $
      state { pingTimerRef = Just timerRef, channels = newChannels, users = newUsers }

  handleClientMessage (LeaveChannel { user, channel }) state@{ channels, users } timerRef = do
    liftEffect $ Logger.debug { domain: atom "websocket" : atom "message" : nil, type: Logger.Trace }
      { message: "Leave channel: " <> show channel }
    ChannelBus.send channel $ ChannelLeft { channel, user }
    let maybeSubscriptionRef = Map.lookup channel channels
    traverse_ ChannelBus.unsubscribe maybeSubscriptionRef
    let newUsers = Map.delete channel users
    let newChannels = Map.delete channel channels
    pure $ Stetson.NoReply $ WebsocketState $
      state { pingTimerRef = Just timerRef, channels = newChannels, users = newUsers }

  terminate _foreign _r (WebsocketState { channels, user: Just user }) = do
    liftEffect $ Logger.debug
      { domain: atom "websocket" : atom "terminate" : nil, type: Logger.Trace }
      { message: "Terminating websocket" }
    traverse_ (\channel -> ChannelBus.send channel $ ChannelLeft { channel, user }) $
      Map.keys channels
  terminate _foreign _r (WebsocketState { user: Nothing }) = do
    pure unit

schedulePingMessage :: forall m. HasSelf m ServerMessage => MonadEffect m => m TimerRef
schedulePingMessage = Timer.sendAfter (Milliseconds 25_000.0) SendPing

frameToString :: Frame -> String
frameToString (TextFrame s) = "TextFrame " <> s
frameToString (BinaryFrame b) = "BinaryFrame " <> show b
frameToString (PingFrame b) = "PingFrame " <> show b
frameToString (PongFrame b) = "PongFrame " <> show b

layout :: Html -> Html
layout content =
  H.html5 head body
  where
  head = H.head [ H.link { rel: "stylesheet", href: "/static/app.css" } ]
  body = H.body {} [ H.script { src: "/static/app.js", async: true } [], content ]

indexPage :: Html
indexPage = layout $ H.div {} []

htmlWriter :: Tuple2 String (Req -> Html -> Effect (RestResult IOData Html))
htmlWriter = tuple2 "text/html" $ \r s ->
  Rest.result (IOData.fromString $ H.render s) r s
