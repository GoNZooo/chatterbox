module Chatterbox.User.Supervisor
  ( startLink
  , startUser
  ) where

import Prelude

import Chatterbox.User as User
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

serverName :: RegistryName (Supervisor.SupervisorType User.Arguments User.Pid)
serverName = Local $ atom "Chatterbox.User.Supervisor"

startLink :: Effect (StartLinkResult (Supervisor.SupervisorPid User.Arguments User.Pid))
startLink =
  Supervisor.startLink (Just $ Local $ atom "Chatterbox.User.Supervisor") $ pure init
  where
  init = { childType, intensity, period, restartStrategy, start, shutdownStrategy }
  childType = Worker
  intensity = 5
  period = Seconds 10.0
  restartStrategy = RestartTransient
  start = User.startLink
  shutdownStrategy = ShutdownTimeout $ Milliseconds 5000.0

startUser :: User.Arguments -> Effect User.Pid
startUser args = crashIfChildNotRunning <$> Supervisor.startChild (ByName serverName) args
