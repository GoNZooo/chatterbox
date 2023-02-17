module Chatterbox.ChannelCounter
  ( serverName
  , startLink
  , start
  , state
  , getUsers
  ) where

import Prelude

import Chatterbox.Channel as ChannelBus
import Chatterbox.ChannelCounter.Types (Arguments, Message, State, Pid)
import Chatterbox.Common.Types (Channel, ChannelEvent(..), User)
import Chatterbox.Names as Names
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Effect (Effect)
import Erl.Atom (atom)
import Erl.Data.Tuple (tuple2)
import Foreign as Foreign
import Pinto.GenServer (InitResult(..), ServerPid, ServerType, InfoFn)
import Pinto.GenServer as GenServer
import Pinto.Supervisor (crashIfChildNotRunning)
import Pinto.Supervisor.SimpleOneForOne as Supervisor
import Pinto.Types (RegistryName(..), RegistryReference(..), StartLinkResult)

serverName :: Channel -> RegistryName (ServerType Unit Unit Message State)
serverName channel =
  Global $ Foreign.unsafeToForeign $ tuple2 (atom "Chatterbox.ChannelCounter") channel

startLink
  :: Arguments -> Effect (StartLinkResult (ServerPid Unit Unit Message State))
startLink channel =
  GenServer.startLink (GenServer.defaultSpec init)
    { name = Just $ serverName channel, handleInfo = Just handleInfo }
  where
  handleInfo :: InfoFn Unit Unit Message State
  handleInfo (ChannelJoined { user }) state'@{ users } =
    pure $ GenServer.return $ state' { users = Set.insert user users }
  handleInfo (ChannelLeft { user }) state'@{ users } =
    pure $ GenServer.return $ state' { users = Set.delete user users }
  handleInfo (UserRenamed { newName, oldName }) state'@{ users } =
    pure $ GenServer.return $ state'
      { users = Set.map (\u -> if u == oldName then newName else u) users }
  handleInfo (ChannelMessageSent {}) state' = pure $ GenServer.return state'
  init = do
    _subscriptionRef <- ChannelBus.subscribe channel identity
    pure $ InitOk $ { channel, users: Set.empty }

state :: Channel -> Effect State
state channel = do
  _ <- start channel
  GenServer.call (ByName $ serverName channel) $ \_from s -> pure $ GenServer.reply s s

start :: Channel -> Effect Pid
start channel =
  crashIfChildNotRunning <$> Supervisor.startChild (ByName Names.channelCounterSupervisor) channel

getUsers :: Channel -> Effect (Set User)
getUsers channel = do
  _ <- start channel
  GenServer.call (ByName $ serverName channel) $ \_from s -> pure $ GenServer.reply s.users s
