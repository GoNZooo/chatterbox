module Chatterbox.Utilities
  ( createUnsafeStartResult
  , StartErrorReason(..)
  ) where

import Prelude

import Chatterbox.Types (ErlangResult(..), UnsafeStartResult)
import Data.Either (either)
import Pinto.Supervisor (SupervisorPid)
import Pinto.Types (NotStartedReason, StartLinkResult)

createUnsafeStartResult :: forall pid. StartLinkResult pid -> ErlangResult StartErrorReason pid
createUnsafeStartResult = either (translate_start_link_error >>> Error) Ok

foreign import create_unsafe_start_result :: StartLinkResult SupervisorPid -> UnsafeStartResult
foreign import translate_start_link_error :: forall pid. NotStartedReason pid -> StartErrorReason

foreign import data StartErrorReason :: Type

