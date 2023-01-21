-- | Represents a process belonging to a single conceptual channel entity in the system. This is
-- | subscribed to by processes that are interested in messages in that channel.
module Chatterbox.Channel
  ( bus
  , send
  , subscribe
  , unsubscribe
  ) where

import Prelude

import Chatterbox.Common.Types (Channel, ChannelEvent)
import Chatterbox.Types (ChatProcessName(..))
import Effect.Class (class MonadEffect, liftEffect)
import Erl.Process (class HasSelf)
import SimpleBus (SubscriptionRef)
import SimpleBus as SimpleBus

-- | The bus on which we can send channel events.
bus :: Channel -> SimpleBus.Bus ChatProcessName ChannelEvent
bus channel = SimpleBus.bus $ ChannelProcess channel

-- | Subscribes a process to a channel bus. Takes a callback for turning channel events into events
-- | that the process can handle.
subscribe
  :: forall e m
   . HasSelf m e
  => MonadEffect m
  => Channel
  -> (ChannelEvent -> e)
  -> m SubscriptionRef
subscribe channel f = SimpleBus.subscribe (bus channel) f

-- | Unsubscribes a process from a channel bus.
unsubscribe :: forall m. MonadEffect m => SubscriptionRef -> m Unit
unsubscribe = SimpleBus.unsubscribe >>> liftEffect

-- | Sends a channel event on the bus.
send :: forall m. MonadEffect m => Channel -> ChannelEvent -> m Unit
send channel = SimpleBus.raise (bus channel) >>> liftEffect
