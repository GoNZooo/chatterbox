-- | Represents a process belonging to a single conceptual entity in the system. In reality this is
-- | potentially multiple processes, but if there are, it's because multiple tabs/devices might be
-- | open and we inform all of them about user events.
module Chatterbox.User
  ( bus
  , send
  , subscribe
  , unsubscribe
  ) where

import Prelude

import Chatterbox.Common.Types (User, UserEvent)
import Chatterbox.Types (ChatProcessName(..))
import Effect.Class (class MonadEffect, liftEffect)
import Erl.Process (class HasSelf)
import SimpleBus (SubscriptionRef)
import SimpleBus as SimpleBus

-- | The bus on which we can send user events.
bus :: User -> SimpleBus.Bus ChatProcessName UserEvent
bus user = SimpleBus.bus $ UserProcess user

-- | Subscribes a process to a user bus. Takes a callback for turning user events into events that
-- | the process can handle.
subscribe :: forall e m. HasSelf m e => MonadEffect m => User -> (UserEvent -> e) -> m SubscriptionRef
subscribe user f = SimpleBus.subscribe (bus user) f

-- | Unsubscribes a process from a channel bus.
unsubscribe :: forall m. MonadEffect m => SubscriptionRef -> m Unit
unsubscribe = SimpleBus.unsubscribe >>> liftEffect

-- | Sends a user event on the bus.
send :: forall m. MonadEffect m => User -> UserEvent -> m Unit
send user = SimpleBus.raise (bus user) >>> liftEffect
