module Chatterbox.ChannelCounter.Supervisor
  ( startLink
  ) where

import Prelude

import Chatterbox.ChannelCounter as ChannelCounter
import Chatterbox.ChannelCounter.Types as ChannelCounterTypes
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..), Seconds(..))
import Effect (Effect)
import Erl.Atom (atom)
import Pinto.Supervisor (ChildShutdownTimeoutStrategy(..), ChildType(..), RestartStrategy(..))
import Pinto.Supervisor.SimpleOneForOne as Supervisor
import Pinto.Types (RegistryName(..), StartLinkResult)

startLink
  :: Effect
       ( StartLinkResult
           (Supervisor.SupervisorPid ChannelCounterTypes.Arguments ChannelCounterTypes.Pid)
       )
startLink =
  Supervisor.startLink (Just $ Local $ atom "Chatterbox.ChannelCounter.Supervisor") $ pure init
  where
  init = { childType, intensity, period, restartStrategy, start, shutdownStrategy }
  childType = Worker
  intensity = 5
  period = Seconds 10.0
  restartStrategy = RestartTransient
  start = ChannelCounter.startLink
  shutdownStrategy = ShutdownTimeout $ Milliseconds 5000.0

