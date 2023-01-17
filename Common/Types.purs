module Chatterbox.Common.Types
  ( ServerMessage(..)
  ) where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Foreign (ForeignError(..), fail)
import Simple.JSON (class ReadForeign, class WriteForeign, readImpl, writeImpl)

data ServerMessage
  = EchoMessage String
  | SendPing

derive instance genericWebsocketMessage :: Generic ServerMessage _
derive instance eqWebsocketMessage :: Eq ServerMessage

instance showWebsocketMessage :: Show ServerMessage where
  show (EchoMessage s) = "EchoMessage " <> show s
  show SendPing = "SendPing"

instance writeForeignWebsocketMessage :: WriteForeign ServerMessage where
  writeImpl (EchoMessage s) = writeImpl { type: "EchoMessage", message: s }
  writeImpl SendPing = writeImpl { type: "SendPing" }

instance readForeignWebsocketMessage :: ReadForeign ServerMessage where
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

