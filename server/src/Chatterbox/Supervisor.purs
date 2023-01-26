module Chatterbox.Supervisor
  ( startLink
  ) where

import Prelude

import Chatterbox.ChannelCounter.Supervisor as ChannelCounterSupervisor
import Chatterbox.Web as Web
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Seconds(..))
import Effect (Effect)
import Erl.Atom (atom)
import Erl.Data.List as ErlList
import Pinto.Supervisor (SupervisorPid)
import Pinto.Supervisor as Supervisor
import Pinto.Supervisor.Helpers as SupervisorHelpers
import Pinto.Types (RegistryName(..), StartLinkResult)

startLink :: Effect (StartLinkResult SupervisorPid)
startLink = Supervisor.startLink (Just $ Local $ atom "Chatterbox.Supervisor") $ pure supervisorSpec
  where
  supervisorSpec = { childSpecs, flags }
  childSpecs = ErlList.fromFoldable
    [ SupervisorHelpers.supervisor
        "Chatterbox.ChannelCounter.Supervisor"
        ChannelCounterSupervisor.startLink
    , SupervisorHelpers.worker "Chatterbox.Web" $ Web.startLink {}
    ]
  flags = { strategy, intensity, period }
  strategy = Supervisor.OneForOne
  intensity = 3
  period = Seconds 5.0

