module Chatterbox.Supervisor
  ( startLink
  ) where

import Prelude

import Chatterbox.Channel.Supervisor as ChannelSupervisor
import Chatterbox.User.Supervisor as UserSupervisor
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..), Seconds(..))
import Effect (Effect)
import Erl.Atom (atom)
import Erl.Data.List as ErlList
import Erl.Process.Raw (class HasPid)
import Pinto.Supervisor
  ( ChildShutdownTimeoutStrategy(..)
  , ChildType(..)
  , ErlChildSpec
  , RestartStrategy(..)
  , SupervisorPid
  , spec
  )
import Pinto.Supervisor as Supervisor
import Pinto.Types (RegistryName(..), StartLinkResult)

startLink :: Effect (StartLinkResult SupervisorPid)
startLink = Supervisor.startLink (Just $ Local $ atom "Chatterbox.Supervisor") $ pure supervisorSpec
  where
  supervisorSpec = { childSpecs, flags }
  childSpecs = ErlList.fromFoldable
    [ supervisor "Chatterbox.Channel.Supervisor" ChannelSupervisor.startLink
    , supervisor "Chatterbox.User.Supervisor" UserSupervisor.startLink
    ]
  flags = { strategy, intensity, period }
  strategy = Supervisor.OneForOne
  intensity = 3
  period = Seconds 5.0

supervisor
  :: forall childProcess
   . HasPid childProcess
  => String
  -> Effect (StartLinkResult childProcess)
  -> ErlChildSpec
supervisor id start =
  spec
    { id
    , childType: Supervisor
    , start
    , restartStrategy: RestartTransient
    , shutdownStrategy: ShutdownTimeout $ Milliseconds 5000.0
    }
