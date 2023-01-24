module Chatterbox.Utilities where

import Prelude

import Chatterbox.Types (ErlangResult(..), UnsafeStartResult)
import Data.Either (Either(..))
import Pinto.Supervisor (SupervisorPid)
import Pinto.Types (NotStartedReason, StartLinkResult)

createUnsafeStartResult :: forall pid. StartLinkResult pid -> ErlangResult StartErrorReason pid
createUnsafeStartResult (Left reason) = reason # translate_start_link_error # Error
createUnsafeStartResult (Right pid) = Ok pid

foreign import create_unsafe_start_result :: StartLinkResult SupervisorPid -> UnsafeStartResult
foreign import translate_start_link_error :: forall pid. NotStartedReason pid -> StartErrorReason

foreign import data StartErrorReason :: Type

