module Chatterbox.Common.Types
  ( ServerMessage(..)
  , ClientMessage(..)
  , User(..)
  , Channel(..)
  , UserEvent(..)
  , ChannelEvent(..)
  , Event(..)
  ) where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Foreign (ForeignError(..), fail)
import Simple.JSON (class ReadForeign, class WriteForeign, readImpl, writeImpl)

data UserEvent = ConnectSuccess

derive instance genericUserEvent :: Generic UserEvent _
derive instance eqUserEvent :: Eq UserEvent
derive instance ordUserEvent :: Ord UserEvent

instance showUserEvent :: Show UserEvent where
  show ConnectSuccess = "ConnectSuccess"

instance readForeignUserEvent :: ReadForeign UserEvent where
  readImpl f = do
    HasTypeField { type: t } <- readImpl f
    case t of
      "ConnectSuccess" -> pure ConnectSuccess
      _ -> fail $ ForeignError $ "Invalid tag for UserEvent: " <> t

instance writeForeignUserEvent :: WriteForeign UserEvent where
  writeImpl ConnectSuccess = writeImpl { type: "ConnectSuccess" }

data ChannelEvent
  = ChannelJoined { channel :: Channel, user :: User }
  | ChannelLeft { channel :: Channel, user :: User }
  | ChannelMessageSent { channel :: Channel, user :: User, message :: String }

derive instance genericChannelEvent :: Generic ChannelEvent _
derive instance eqChannelEvent :: Eq ChannelEvent
derive instance ordChannelEvent :: Ord ChannelEvent

instance showChannelEvent :: Show ChannelEvent where
  show (ChannelJoined r) = "ChannelJoined " <> show r
  show (ChannelLeft r) = "ChannelLeft " <> show r
  show (ChannelMessageSent r) = "ChannelMessageSent " <> show r

instance readForeignChannelEvent :: ReadForeign ChannelEvent where
  readImpl f = do
    HasTypeField { type: t } <- readImpl f
    case t of
      "ChannelJoined" -> ChannelJoined <$> readImpl f
      "ChannelLeft" -> ChannelLeft <$> readImpl f
      "ChannelMessageSent" -> ChannelMessageSent <$> readImpl f
      _ -> fail $ ForeignError $ "Invalid tag for ChannelEvent: " <> t

instance writeForeignChannelEvent :: WriteForeign ChannelEvent where
  writeImpl (ChannelJoined { channel, user }) = writeImpl { type: "ChannelJoined", channel, user }
  writeImpl (ChannelLeft { channel, user }) = writeImpl { type: "ChannelLeft", channel, user }
  writeImpl (ChannelMessageSent { channel, user, message }) =
    writeImpl { type: "ChannelMessage", channel, user, message }

newtype User = User String

derive newtype instance eqUser :: Eq User
derive newtype instance ordUser :: Ord User
derive instance genericUser :: Generic User _
derive instance newtypeUser :: Newtype User _

instance showUser :: Show User where
  show (User u) = "User " <> u

instance readForeignUser :: ReadForeign User where
  readImpl f = User <$> readImpl f

instance writeForeignUser :: WriteForeign User where
  writeImpl (User s) = writeImpl s

newtype Channel = Channel String

derive newtype instance eqChannel :: Eq Channel
derive newtype instance ordChannel :: Ord Channel
derive instance genericChannel :: Generic Channel _
derive instance newtypeChannel :: Newtype Channel _

instance showChannel :: Show Channel where
  show (Channel u) = "Channel " <> u

instance readForeignChannel :: ReadForeign Channel where
  readImpl f = Channel <$> readImpl f

instance writeForeignChannel :: WriteForeign Channel where
  writeImpl (Channel s) = writeImpl s

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
  | UserMessage UserEvent
  | ChannelMessage ChannelEvent

derive instance genericServerMessage :: Generic ServerMessage _
derive instance eqServerMessage :: Eq ServerMessage

instance showServerMessage :: Show ServerMessage where
  show (EchoMessage s) = "EchoMessage " <> show s
  show SendPing = "SendPing"
  show (UserMessage e) = "UserMessage " <> show e
  show (ChannelMessage e) = "ChannelMessage " <> show e

instance writeForeignServerMessage :: WriteForeign ServerMessage where
  writeImpl (EchoMessage s) = writeImpl { type: "EchoMessage", message: s }
  writeImpl SendPing = writeImpl { type: "SendPing" }
  writeImpl (UserMessage e) = writeImpl { type: "UserMessage", event: e }
  writeImpl (ChannelMessage e) = writeImpl { type: "ChannelMessage", event: e }

instance readForeignServerMessage :: ReadForeign ServerMessage where
  readImpl f = do
    HasTypeField { type: t } <- readImpl f
    case t of
      "EchoMessage" -> EchoMessage <$> readImpl f
      "SendPing" -> pure SendPing
      "UserMessage" -> UserMessage <$> readImpl f
      "ChannelMessage" -> ChannelMessage <$> readImpl f
      _ -> fail $ ForeignError $ "Unknown type: " <> t

newtype HasTypeField = HasTypeField { type :: String }

derive instance newtypeHasTypeField :: Newtype HasTypeField _

instance readForeignHasTypeField :: ReadForeign HasTypeField where
  readImpl f = HasTypeField <$> readImpl f

data ClientMessage
  = SetUsername { user :: User }
  | SendMessage { channel :: Channel, message :: String }
  | JoinChannel { user :: User, channel :: Channel }
  | LeaveChannel { user :: User, channel :: Channel }

derive instance genericClientMessage :: Generic ClientMessage _
derive instance eqClientMessage :: Eq ClientMessage

instance showClientMessage :: Show ClientMessage where
  show (SetUsername r) = "SetUsername " <> show r
  show (SendMessage r) = "SendMessage " <> show r
  show (JoinChannel r) = "JoinChannel " <> show r
  show (LeaveChannel r) = "LeaveChannel " <> show r

instance writeForeignClientMessage :: WriteForeign ClientMessage where
  writeImpl (SetUsername { user }) = writeImpl { type: "SetUsername", user }
  writeImpl (SendMessage { channel, message }) = writeImpl { type: "SendMessage", channel, message }
  writeImpl (JoinChannel { user, channel }) = writeImpl { type: "JoinChannel", user, channel }
  writeImpl (LeaveChannel { user, channel }) = writeImpl { type: "LeaveChannel", user, channel }

instance readForeignClientMessage :: ReadForeign ClientMessage where
  readImpl f = do
    HasTypeField { type: t } <- readImpl f
    case t of
      "SetUsername" -> SetUsername <$> readImpl f
      "SendMessage" -> SendMessage <$> readImpl f
      "JoinChannel" -> JoinChannel <$> readImpl f
      "LeaveChannel" -> LeaveChannel <$> readImpl f
      _ -> fail $ ForeignError $ "Unknown type: " <> t
