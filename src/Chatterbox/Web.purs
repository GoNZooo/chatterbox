module Chatterbox.Web where

import Prelude hiding ((/))

import Chatterbox.Types (WebsocketMessage)
import Data.Array as Array
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..), fromJust)
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Class (liftEffect)
import Erl.Atom (atom)
import Erl.Cowboy.Handlers.WebSocket (Frame(..))
import Erl.Cowboy.Req (Req)
import Erl.Data.Binary.IOData (IOData)
import Erl.Data.Binary.IOData as IOData
import Erl.Data.List (nil, (:))
import Erl.Data.Tuple (Tuple2, tuple2)
import Erl.Kernel.Inet (Port(..), ip4)
import Html as H
import Html.Types (Html)
import Logger as Logger
import Partial.Unsafe (unsafePartial)
import Pinto.GenServer (InitFn, InitResult(..), ServerPid, ServerType)
import Pinto.GenServer as GenServer
import Pinto.GenServer.Helpers as GenServerHelpers
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

websocketHandler :: StetsonHandler WebsocketMessage Unit
websocketHandler =
  routeHandler
    { init: \r -> WebSocket.initResult r unit
    , wsInit
    , wsHandle
    , wsInfo
    }
  where
  wsInit state = do
    pure $ Stetson.NoReply state

  -- Handles messages from the client that is connected to the websocket
  wsHandle frame state = do
    liftEffect $ Logger.info { domain: atom "websocket" : atom "frame" : nil, type: Logger.Trace }
      { message: "Received frame: " <> frameToString frame }
    pure $ Stetson.NoReply state

  -- Like a classic `handle_info`; handles arbitrary messages sent to the process. This will be
  -- used to react to messages that come in from subscriptions.
  wsInfo message state = do
    liftEffect $ Logger.info { domain: atom "websocket" : atom "info" : nil, type: Logger.Trace }
      { message: "Received info: " <> show message }
    pure $ Stetson.Reply ((TextFrame $ Json.writeJSON message) : nil) state

frameToString :: Frame -> String
frameToString (TextFrame s) = "TextFrame " <> s
frameToString (BinaryFrame b) = "BinaryFrame" <> show b
frameToString (PingFrame b) = "PingFrame" <> show b
frameToString (PongFrame b) = "PongFrame" <> show b

layout :: Html -> Html
layout content =
  H.html5 head body
  where
  head = H.head []
  body = H.body {} [ H.script { src: "/static/app.js", async: true } [], content ]

indexPage :: Html
indexPage = layout $ H.div {} [ H.h1 {} [ H.text "Chatterbox" ] ]

htmlWriter :: Tuple2 String (Req -> Html -> Effect (RestResult IOData Html))
htmlWriter = tuple2 "text/html" $ \r s ->
  Rest.result (IOData.fromString $ H.render s) r s
