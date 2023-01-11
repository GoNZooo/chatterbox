module Chatterbox.Supervisor
  ( startLink
  , start_link
  ) where

import Prelude

import Chatterbox.Types (ErlangResult)
import Chatterbox.Utilities (StartErrorReason)
import Chatterbox.Utilities as Utilities
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Seconds(..))
import Effect (Effect)
import Effect.Unsafe as UnsafeEffect
import Erl.Atom (atom)
import Erl.Data.List (nil)
import Pinto.Supervisor (SupervisorPid)
import Pinto.Supervisor as Supervisor
import Pinto.Types (RegistryName(..), StartLinkResult)

startLink :: Effect (StartLinkResult SupervisorPid)
startLink = Supervisor.startLink (Just $ Local $ atom "Chatterbox.Supervisor") $ pure supervisorSpec
  where
  supervisorSpec = { childSpecs, flags }
  childSpecs = nil
  flags = { strategy, intensity, period }
  strategy = Supervisor.OneForOne
  intensity = 3
  period = Seconds 5.0

start_link :: ErlangResult StartErrorReason SupervisorPid
start_link = UnsafeEffect.unsafePerformEffect $ Utilities.createUnsafeStartResult <$> startLink

