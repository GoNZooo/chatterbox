module Chatterbox.Names
  ( channelCounterSupervisor
  ) where

import Prelude

import Chatterbox.ChannelCounter.Types as ChannelCounter
import Erl.Atom (atom)
import Pinto.Supervisor.SimpleOneForOne as SimpleOneForOneSupervisor
import Pinto.Types (RegistryName(..))

channelCounterSupervisor
  :: RegistryName
       (SimpleOneForOneSupervisor.SupervisorType ChannelCounter.Arguments ChannelCounter.Pid)
channelCounterSupervisor = Local $ atom "Chatterbox.ChannelCounter.Supervisor"

