module Chatterbox
  ( restartWeb
  ) where

import Prelude

import Chatterbox.Web as Web
import Effect (Effect)

restartWeb :: Effect Unit
restartWeb = Web.restart
