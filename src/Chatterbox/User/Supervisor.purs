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

serverName :: forall s. RegistryName (Supervisor.SupervisorType User.Arguments (User.Pid s))
serverName = Local $ atom "Chatterbox.User.Supervisor"

startLink :: forall s. Effect (StartLinkResult (Supervisor.SupervisorPid User.Arguments (User.Pid s)))
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

startUser :: forall s. User.Arguments -> Effect (User.Pid s)
startUser args = crashIfChildNotRunning <$> Supervisor.startChild (ByName serverName) args
