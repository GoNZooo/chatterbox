module Chatterbox.ChannelCounter.Types where

import Prelude

import Chatterbox.Common.Types (Channel)
import Pinto.GenServer (ServerPid)
import Pinto.Monitor (MonitorMsg)

type Arguments = { channel :: Channel }

type State = { channel :: Channel }

data Message = MonitorDown MonitorMsg

type Pid = ServerPid Unit Unit Message State

