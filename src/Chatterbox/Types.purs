module Chatterbox.Types where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Foreign (ForeignError(..), fail)
import Pinto.Timer (TimerRef)
import Simple.JSON (class ReadForeign, class WriteForeign, readImpl, writeImpl)

data WebsocketMessage
  = EchoMessage String
  | SendPing

derive instance genericWebsocketMessage :: Generic WebsocketMessage _
derive instance eqWebsocketMessage :: Eq WebsocketMessage

instance showWebsocketMessage :: Show WebsocketMessage where
  show (EchoMessage s) = "EchoMessage " <> show s
  show SendPing = "SendPing"

instance writeForeignWebsocketMessage :: WriteForeign WebsocketMessage where
  writeImpl (EchoMessage s) = writeImpl { type: "EchoMessage", message: s }
  writeImpl SendPing = writeImpl { type: "SendPing" }

instance readForeignWebsocketMessage :: ReadForeign WebsocketMessage where
  readImpl f = do
    HasTypeField { type: t } <- readImpl f
    case t of
      "EchoMessage" -> EchoMessage <$> readImpl f
      "SendPing" -> pure SendPing
      _ -> fail $ ForeignError $ "Unknown type: " <> t

newtype WebsocketState = WebsocketState { lastPing :: Maybe Int, pingTimerRef :: Maybe TimerRef }

derive instance newtypeWebsocketState :: Newtype WebsocketState _

instance showWebsocketState :: Show WebsocketState where
  show (WebsocketState { lastPing }) = "WebsocketState " <> show { lastPing }

data ErlangResult l r
  = Error l
  | Ok r

erlangResult :: forall l r a. (l -> a) -> (r -> a) -> ErlangResult l r -> a
erlangResult f _ (Error l) = f l
erlangResult _ f (Ok r) = f r

derive instance eqErlangResult :: (Eq l, Eq r) => Eq (ErlangResult l r)
derive instance ordErlangResult :: (Ord l, Ord r) => Ord (ErlangResult l r)
derive instance genericErlangResult :: Generic (ErlangResult l r) _

instance functorErlangResult :: Functor (ErlangResult l) where
  map f (Ok r) = Ok (f r)
  map _ (Error l) = Error l

instance applyErlangResult :: Apply (ErlangResult l) where
  apply (Ok f) (Ok r) = Ok (f r)
  apply (Error l) _ = Error l
  apply _ (Error l) = Error l

instance applicativeErlangResult :: Applicative (ErlangResult l) where
  pure = Ok

instance bindErlangResult :: Bind (ErlangResult l) where
  bind (Ok r) f = f r
  bind (Error l) _ = Error l

instance monadErlangResult :: Monad (ErlangResult l)

foreign import data UnsafeStartResult :: Type

newtype HasTypeField = HasTypeField { type :: String }

derive instance newtypeHasTypeField :: Newtype HasTypeField _

instance readForeignHasTypeField :: ReadForeign HasTypeField where
  readImpl f = HasTypeField <$> readImpl f

