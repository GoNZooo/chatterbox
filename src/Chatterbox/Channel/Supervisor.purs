module Chatterbox.Channel.Supervisor
  ( startLink
  , startChannel
  ) where

import Prelude

import Chatterbox.Channel as Channel
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..), Seconds(..))
import Effect (Effect)
import Erl.Atom (atom)
import Pinto.Supervisor
  ( ChildShutdownTimeoutStrategy(..)
  , ChildType(..)
  , RestartStrategy(..)
  , crashIfChildNotRunning
  )
import Pinto.Supervisor.SimpleOneForOne as Supervisor
import Pinto.Types (RegistryName(..), RegistryReference(..), StartLinkResult)

serverName :: RegistryName (Supervisor.SupervisorType Channel.Arguments Channel.Pid)
serverName = Local $ atom "Chatterbox.Channel.Supervisor"

startLink :: Effect (StartLinkResult (Supervisor.SupervisorPid Channel.Arguments Channel.Pid))
startLink =
  Supervisor.startLink (Just $ Local $ atom "Chatterbox.Channel.Supervisor") $ pure init
  where
  init = { childType, intensity, period, restartStrategy, start, shutdownStrategy }
  childType = Worker
  intensity = 5
  period = Seconds 10.0
  restartStrategy = RestartTransient
  start = Channel.startLink
  shutdownStrategy = ShutdownTimeout $ Milliseconds 5000.0

startChannel :: Channel.Arguments -> Effect Channel.Pid
startChannel args = crashIfChildNotRunning <$> Supervisor.startChild (ByName serverName) args
