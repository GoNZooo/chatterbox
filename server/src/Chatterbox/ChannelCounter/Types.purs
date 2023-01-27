module Chatterbox.ChannelCounter.Types where

import Prelude

import Chatterbox.Common.Types (Channel, ChannelEvent, User)
import Pinto.GenServer (ServerPid)

type Arguments = Channel

type State = { channel :: Channel, users :: Array User }

type Message = ChannelEvent

type Pid = ServerPid Unit Unit Message State

