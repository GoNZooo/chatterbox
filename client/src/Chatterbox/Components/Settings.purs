module Chatterbox.Components.Settings (component, Output) where

import Prelude

import Effect.Aff.Class (class MonadAff)
import Halogen (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.Event.Event (Event)
import Web.Event.Event as Event

type Input = { username :: String }
type Output = { username :: String }

type State = { username :: String }

data Action
  = SetUsername String
  | SubmitUsername String Event

component :: forall query m. MonadAff m => H.Component query Input Output m
component = H.mkComponent
  { initialState: identity
  , render
  , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
  }
  where
  render :: State -> H.ComponentHTML Action () m
  render { username } =
    HH.div_
      [ HH.label [ HP.for "username" ] [ HH.text "Change username" ]
      , HH.form
          [ HE.onSubmit (SubmitUsername username) ]
          [ HH.input
              [ HP.type_ HP.InputText
              , HP.id "username"
              , HP.value username
              , HE.onValueInput SetUsername
              ]
          ]
      ]

  handleAction :: Action -> H.HalogenM State Action () Output m Unit
  handleAction (SetUsername username) = H.modify_ _ { username = username }
  handleAction (SubmitUsername username e) = do
    liftEffect $ Event.preventDefault e
    H.raise { username }
