module Chatterbox.User
  ( serverName
  , startLink
  , subscribe
  , broadcast
  , state
  , Pid
  , Arguments
  , Message
  , MonitorData
  , State
  ) where

import Prelude

import Chatterbox.Common.Types (Event)
import Data.Array as Array
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Erl.Atom (atom)
import Erl.Data.Tuple (tuple2)
import Erl.Process (Process, send)
import Foreign as Foreign
import Pinto.GenServer (InitResult(..), ServerPid, ServerType)
import Pinto.GenServer as GenServer
import Pinto.MessageRouting (RouterRef)
import Pinto.Monitor (MonitorMsg, MonitorRef)
import Pinto.Monitor as Process
import Pinto.Types (RegistryName(..), RegistryReference(..), StartLinkResult)

type Arguments = { id :: String }

type State event = { id :: String, subscribers :: Array (MonitorData event) }

data Message = MonitorDown MonitorMsg

type Pid event = ServerPid Unit Unit Message (State event)

newtype MonitorData event = MonitorData
  { pid :: Process event, ref :: RouterRef MonitorRef, eventWrapper :: Event -> event }

serverName :: forall event. String -> RegistryName (ServerType Unit Unit Message (State event))
serverName id = Global $ Foreign.unsafeToForeign $ tuple2 (atom "Chatterbox.User") id

subscribe :: forall event. String -> Process event -> (Event -> event) -> Effect Unit
subscribe id pid eventWrapper = GenServer.cast (ByName $ serverName id) \s -> do
  monitorRef <- Process.monitor pid MonitorDown
  pure $ GenServer.return $ s
    { subscribers = Array.snoc s.subscribers (MonitorData { pid, eventWrapper, ref: monitorRef }) }

broadcast :: String -> Event -> Effect Unit
broadcast id event = GenServer.cast (ByName $ serverName id) \s -> do
  liftEffect $ for_ s.subscribers \(MonitorData { pid, eventWrapper }) -> do
    send pid (eventWrapper event)
  pure $ GenServer.return s

startLink
  :: forall event
   . Arguments
  -> Effect (StartLinkResult (ServerPid Unit Unit Message (State event)))
startLink { id } =
  GenServer.startLink (GenServer.defaultSpec init) { name = Just $ serverName id }
  where
  init = pure $ InitOk { id, subscribers: [] }

state :: forall event. String -> Effect (State event)
state id =
  GenServer.call (ByName $ serverName id) $ \_from s -> pure $ GenServer.reply s s
