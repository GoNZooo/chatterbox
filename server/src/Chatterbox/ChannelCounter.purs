module Chatterbox.ChannelCounter
  ( serverName
  , startLink
  , start
  , state
  ) where

import Prelude

import Chatterbox.ChannelCounter.Types (Arguments, Message, State, Pid)
import Chatterbox.Common.Types (Channel)
import Chatterbox.Names as Names
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Erl.Atom (atom)
import Erl.Data.Tuple (tuple2)
import Foreign as Foreign
import Pinto.GenServer (InitResult(..), ServerPid, ServerType)
import Pinto.GenServer as GenServer
import Pinto.Supervisor (crashIfChildNotRunning)
import Pinto.Supervisor.SimpleOneForOne as Supervisor
import Pinto.Types (RegistryName(..), RegistryReference(..), StartLinkResult)

serverName :: Channel -> RegistryName (ServerType Unit Unit Message State)
serverName channel =
  Global $ Foreign.unsafeToForeign $ tuple2 (atom "Chatterbox.ChannelCounter") channel

startLink
  :: Arguments -> Effect (StartLinkResult (ServerPid Unit Unit Message State))
startLink { channel } =
  GenServer.startLink (GenServer.defaultSpec init) { name = Just $ serverName channel }
  where
  init = pure $ InitOk $ { channel }

state :: Channel -> Effect State
state channel = do
  _ <- start channel
  GenServer.call (ByName $ serverName channel) $ \_from s -> pure $ GenServer.reply s s

start :: Channel -> Effect Pid
start channel =
  crashIfChildNotRunning <$> Supervisor.startChild (ByName Names.channelCounterSupervisor) { channel }
