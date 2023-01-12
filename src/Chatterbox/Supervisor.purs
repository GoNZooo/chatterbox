module Chatterbox.Supervisor
  ( startLink
  , start_link
  ) where

import Prelude

import Chatterbox.Channel.Supervisor as ChannelSupervisor
import Chatterbox.Types (ErlangResult)
import Chatterbox.Utilities (StartErrorReason)
import Chatterbox.Utilities as Utilities
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..), Seconds(..))
import Effect (Effect)
import Effect.Unsafe as UnsafeEffect
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
    [ worker "Chatterbox.Channel.Supervisor" ChannelSupervisor.startLink ]
  flags = { strategy, intensity, period }
  strategy = Supervisor.OneForOne
  intensity = 3
  period = Seconds 5.0

start_link :: ErlangResult StartErrorReason SupervisorPid
start_link = UnsafeEffect.unsafePerformEffect $ Utilities.createUnsafeStartResult <$> startLink

worker
  :: forall childProcess
   . HasPid childProcess
  => String
  -> Effect (StartLinkResult childProcess)
  -> ErlChildSpec
worker id start =
  spec
    { id
    , childType: Worker
    , start
    , restartStrategy: RestartTransient
    , shutdownStrategy: ShutdownTimeout $ Milliseconds 5000.0
    }
