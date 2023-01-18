module Chatterbox.Components.Chat
  ( Input
  , Output
  , component
  ) where

import Prelude

import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH

type Input = { username :: String }

type Output = Void

type State = { username :: String, messages :: Array String }

data Action =
  NoOp

component :: forall query m. MonadAff m => H.Component query Input Output m
component =
  H.mkComponent
    { initialState: \{ username } -> { username, messages: [] }
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }
  where
  render :: State -> H.ComponentHTML Action () m
  render { username } =
    HH.div_ [ HH.text $ "Hello " <> username ]

  handleAction :: Action -> H.HalogenM State Action () Output m Unit
  handleAction NoOp = pure unit
