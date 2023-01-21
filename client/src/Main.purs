module Main where

import Prelude hiding ((/))

import Chatterbox.Common.Types (User(..))
import Chatterbox.Components.Chat as Chat
import Chatterbox.Components.PickUsername as PickUsername
import Data.Either (hush)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..), fromMaybe)
import Effect (Effect)
import Effect.Aff as Aff
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Effect.Class as Effect
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.VDom.Driver as VDomDriver
import Routing.Duplex (RouteDuplex')
import Routing.Duplex as RoutingDuplex
import Routing.Duplex.Generic as RoutingDuplexGeneric
import Routing.Duplex.Generic.Syntax ((/))
import Routing.Hash as RoutingHash
import Type.Proxy (Proxy(..))

data Query a = Navigate Route a

type State = { route :: Route }

data Action
  = Initialize
  | UsernamePickerOutput PickUsername.Output

type OpaqueSlot slot = forall query. H.Slot query Void slot

type ChildSlots =
  ( pickUsername :: forall query. H.Slot query PickUsername.Output Unit
  , chat :: OpaqueSlot Unit
  )

data Route
  = PickUsername
  | Chat String

derive instance genericClientRoute :: Generic Route _
derive instance eqClientRoute :: Eq Route

routeCodec :: RouteDuplex' Route
routeCodec = RoutingDuplex.root $ RoutingDuplexGeneric.sum
  { "PickUsername": RoutingDuplexGeneric.noArgs
  , "Chat": "chat" / RoutingDuplex.string RoutingDuplex.segment
  }

router :: forall m. MonadAff m => H.Component Query Unit Void m
router = H.mkComponent
  { initialState: \_ -> { route: PickUsername }
  , render
  , eval: H.mkEval $ H.defaultEval
      { handleQuery = handleQuery, handleAction = handleAction, initialize = Just Initialize }
  }
  where
  render :: State -> H.ComponentHTML Action ChildSlots m
  render { route: PickUsername } =
    HH.slot (Proxy :: _ "pickUsername") unit PickUsername.component {} UsernamePickerOutput
  render { route: Chat username } =
    HH.slot_ (Proxy :: _ "chat") unit Chat.component { user: User username }

  handleQuery :: forall a. Query a -> H.HalogenM State Action ChildSlots Void m (Maybe a)
  handleQuery (Navigate newRoute next) = do
    { route } <- H.get
    when (route /= newRoute) do
      H.modify_ _ { route = newRoute }

    pure $ Just next

  handleAction :: Action -> H.HalogenM State Action ChildSlots Void m Unit
  handleAction Initialize = do
    initialRoute <-
      (RoutingDuplex.parse routeCodec >>> hush) <$> Effect.liftEffect RoutingHash.getHash
    initialRoute # fromMaybe PickUsername # navigate
  handleAction (UsernamePickerOutput (PickUsername.UsernamePicked username)) = do
    navigate $ Chat username

navigate :: forall m. MonadEffect m => Route -> m Unit
navigate = RoutingDuplex.print routeCodec >>> RoutingHash.setHash >>> Effect.liftEffect

type ButtonState = { count :: Int }

data ButtonAction = Increment | Decrement

mainComponent :: forall query input output m. H.Component query input output m
mainComponent = H.mkComponent { initialState, render, eval }
  where
  initialState _ = { count: 0 }
  render { count } = HH.div_
    [ HH.div_
        [ HH.button [ HE.onClick \_ -> Decrement ] [ HH.text "-" ]
        , HH.text $ show count
        , HH.button [ HE.onClick \_ -> Increment ] [ HH.text "+" ]
        ]
    ]
  eval = H.mkEval H.defaultEval { handleAction = handleAction }
  handleAction Increment = do
    H.modify_ \s -> s { count = s.count + 1 }
    pure unit
  handleAction Decrement = do
    H.modify_ \s -> s { count = s.count - 1 }
    pure unit

main :: Effect Unit
main = do
  HA.runHalogenAff do
    body <- HA.awaitBody
    halogenIO <- VDomDriver.runUI router unit body
    void $ Effect.liftEffect $ RoutingHash.matchesWith (RoutingDuplex.parse routeCodec) \old new ->
      when (old /= Just new) $ Aff.launchAff_ do
        _response <- halogenIO.query $ H.mkTell $ Navigate new
        pure unit

