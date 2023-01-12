module Chatterbox.User
  ( serverName
  , startLink
  , state
  , Pid
  , Arguments
  , State
  , Message
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Erl.Atom (atom)
import Erl.Data.Tuple (tuple2)
import Foreign as Foreign
import Pinto.GenServer (InitResult(..), ServerPid, ServerType)
import Pinto.GenServer as GenServer
import Pinto.Types (RegistryName(..), RegistryReference(..), StartLinkResult)

type Arguments = { id :: String }

type State = { id :: String }

type Message = Unit

type Pid = ServerPid Unit Unit Message State

serverName :: String -> RegistryName (ServerType Unit Unit Message State)
serverName id = Global $ Foreign.unsafeToForeign $ tuple2 (atom "Chatterbox.User") id

startLink :: Arguments -> Effect (StartLinkResult (ServerPid Unit Unit Message State))
startLink arguments@{ id } =
  GenServer.startLink (GenServer.defaultSpec init) { name = Just $ serverName id }
  where
  init = pure $ InitOk arguments

state :: String -> Effect State
state id =
  GenServer.call (ByName $ serverName id) $ \_from s -> pure $ GenServer.reply s s
