module Chatterbox.Web where

import Prelude hiding ((/))

import Data.Array as Array
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..), fromJust)
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Class (liftEffect)
import Erl.Atom (atom)
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
import Pinto.Types (RegistryName(..), StartLinkResult)
import Routing.Duplex (RouteDuplex')
import Routing.Duplex as Routing
import Routing.Duplex.Generic as GenericDuplex
import Routing.Duplex.Generic.Syntax ((/))
import Stetson as Stetson
import Stetson.Rest as Rest
import Stetson.Types (RestResult, StaticAssetLocation(..), StetsonHandler, routeHandler)

type State = {}

type StartArgs = {}

data Route
  = Index
  | Static (Array String)

derive instance genericRoute :: Generic Route _

instance showRoute :: Show Route where
  show = genericShow

serverName :: RegistryName (ServerType Unit Unit Unit State)
serverName = "Chatterbox.Web" # atom # Local

startLink :: StartArgs -> Effect (StartLinkResult (ServerPid Unit Unit Unit State))
startLink args =
  GenServer.startLink $ (GenServer.defaultSpec $ init args) { name = Just serverName }

routes :: RouteDuplex' Route
routes =
  ( Routing.path "" $ GenericDuplex.sum
      { "Index": GenericDuplex.noArgs
      , "Static": "static" / Routing.rest
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

layout :: Html -> Html
layout content =
  H.html5 head body
  where
  head = H.head []
  body = H.body {} [ content ]

indexPage :: Html
indexPage = layout $ H.div {} [ H.h1 {} [ H.text "Chatterbox" ] ]

htmlWriter :: Tuple2 String (Req -> Html -> Effect (RestResult IOData Html))
htmlWriter = tuple2 "text/html" $ \r s ->
  Rest.result (IOData.fromString $ H.render s) r s
