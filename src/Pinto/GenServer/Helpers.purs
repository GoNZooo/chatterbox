module Pinto.GenServer.Helpers
  ( exit
  ) where

import Prelude

import Effect (Effect)
import Erl.Atom (Atom)
import Erl.Process (Process)

exit :: forall process. Process process -> Atom -> Effect Unit
exit = exit_

foreign import exit_ :: forall p. Process p -> Atom -> Effect Unit
