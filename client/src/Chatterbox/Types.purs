module Chatterbox.Types where

import Data.Generic.Rep (class Generic)

data ClientRoute
  = ClientChat
  | ClientSettings

derive instance genericClientRoute :: Generic ClientRoute _
