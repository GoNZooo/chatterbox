module Chatterbox.Components.PickUsername
  ( Input
  , Output(..)
  , component
  ) where

import Prelude

import Effect.Aff.Class (class MonadAff)
import Effect.Class as Effect
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.Event.Event (Event)
import Web.Event.Event as Event

type Input = {}

type State = { username :: String }

data Action
  = SetUsernameField String
  | PickUsername String Event

data Output = UsernamePicked String

component :: forall query m. MonadAff m => H.Component query Input Output m
component = H.mkComponent
  { initialState: \_ -> { username: "" }
  , render
  , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
  }
  where
  render :: State -> H.ComponentHTML Action () m
  render { username } =
    HH.div_
      [ HH.form [ HE.onSubmit \e -> PickUsername username e ]
          [ HH.input
              [ HP.type_ HP.InputText
              , HP.value username
              , HE.onValueInput SetUsernameField
              ]
          , HH.button [ HP.type_ HP.ButtonSubmit ] [ HH.text "Pick" ]
          ]
      ]

  handleAction :: Action -> H.HalogenM State Action () Output m Unit
  handleAction (SetUsernameField username) = H.modify_ _ { username = username }
  handleAction (PickUsername username e) = do
    Effect.liftEffect $ Event.preventDefault e
    H.raise $ UsernamePicked username

