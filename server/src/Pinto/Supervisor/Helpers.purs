module Pinto.Supervisor.Helpers
  ( supervisor
  , worker
  ) where

import Prelude

import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Erl.Process.Raw (class HasPid)
import Pinto.Supervisor
  ( ChildShutdownTimeoutStrategy(..)
  , ChildType(..)
  , ErlChildSpec
  , RestartStrategy(..)
  )
import Pinto.Supervisor as Supervisor
import Pinto.Types (StartLinkResult)

supervisor
  :: forall childProcess
   . HasPid childProcess
  => String
  -> Effect (StartLinkResult childProcess)
  -> ErlChildSpec
supervisor id start =
  Supervisor.spec
    { id
    , childType: Supervisor
    , start
    , restartStrategy: RestartTransient
    , shutdownStrategy: ShutdownTimeout $ Milliseconds 5000.0
    }

worker
  :: forall childProcess
   . HasPid childProcess
  => String
  -> Effect (StartLinkResult childProcess)
  -> ErlChildSpec
worker id start =
  Supervisor.spec
    { id
    , childType: Worker
    , start
    , restartStrategy: RestartTransient
    , shutdownStrategy: ShutdownTimeout $ Milliseconds 5000.0
    }
