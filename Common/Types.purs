module Chatterbox.Common.Types
  ( ServerMessage(..)
  , ClientMessage(..)
  , Event(..)
  ) where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Foreign (ForeignError(..), fail)
import Simple.JSON (class ReadForeign, class WriteForeign, readImpl, writeImpl)

data Event
  = UserJoined { username :: String, channel :: String }
  | UserLeft { username :: String, channel :: String }

derive instance genericEvent :: Generic Event _
derive instance eqEvent :: Eq Event

instance writeForeignEvent :: WriteForeign Event where
  writeImpl (UserJoined { username, channel }) = writeImpl { type: "UserJoined", username, channel }
  writeImpl (UserLeft { username, channel }) = writeImpl { type: "UserLeft", username, channel }

instance readForeignEvent :: ReadForeign Event where
  readImpl f = do
    HasTypeField { type: t } <- readImpl f
    case t of
      "UserJoined" -> UserJoined <$> readImpl f
      "UserLeft" -> UserLeft <$> readImpl f
      _ -> fail $ ForeignError $ "Unknown event type: " <> t

instance showEvent :: Show Event where
  show (UserJoined r) = "UserJoined " <> show r
  show (UserLeft r) = "UserLeft " <> show r

data ServerMessage
  = EchoMessage String
  | SendPing
  | EventMessage Event

derive instance genericServerMessage :: Generic ServerMessage _
derive instance eqServerMessage :: Eq ServerMessage

instance showServerMessage :: Show ServerMessage where
  show (EchoMessage s) = "EchoMessage " <> show s
  show SendPing = "SendPing"
  show (EventMessage e) = "EventMessage " <> show e

instance writeForeignServerMessage :: WriteForeign ServerMessage where
  writeImpl (EchoMessage s) = writeImpl { type: "EchoMessage", message: s }
  writeImpl SendPing = writeImpl { type: "SendPing" }
  writeImpl (EventMessage e) = writeImpl { type: "EventMessage", event: e }

instance readForeignServerMessage :: ReadForeign ServerMessage where
  readImpl f = do
    HasTypeField { type: t } <- readImpl f
    case t of
      "EchoMessage" -> EchoMessage <$> readImpl f
      "SendPing" -> pure SendPing
      _ -> fail $ ForeignError $ "Unknown type: " <> t

newtype HasTypeField = HasTypeField { type :: String }

derive instance newtypeHasTypeField :: Newtype HasTypeField _

instance readForeignHasTypeField :: ReadForeign HasTypeField where
  readImpl f = HasTypeField <$> readImpl f

data ClientMessage
  = SetUsername { username :: String }
  | SendMessage { channel :: String, message :: String }

derive instance genericClientMessage :: Generic ClientMessage _
derive instance eqClientMessage :: Eq ClientMessage

instance showClientMessage :: Show ClientMessage where
  show (SetUsername r) = "SetUsername " <> show r
  show (SendMessage r) = "SendMessage " <> show r

instance writeForeignClientMessage :: WriteForeign ClientMessage where
  writeImpl (SetUsername { username }) = writeImpl { type: "SetUsername", username }
  writeImpl (SendMessage { channel, message }) = writeImpl { type: "SendMessage", channel, message }

instance readForeignClientMessage :: ReadForeign ClientMessage where
  readImpl f = do
    HasTypeField { type: t } <- readImpl f
    case t of
      "SetUsername" -> SetUsername <$> readImpl f
      "SendMessage" -> SendMessage <$> readImpl f
      _ -> fail $ ForeignError $ "Unknown type: " <> t
