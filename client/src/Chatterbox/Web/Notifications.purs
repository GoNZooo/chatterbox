module Web.Notifications
  ( requestPermission
  , createNotification
  , Title(..)
  , Body(..)
  , NotificationData(..)
  , NotificationPermission(..)
  , Notification
  ) where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype, unwrap)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Compat (EffectFnAff)
import Effect.Aff.Compat as AffCompat
import Foreign (Foreign)
import Partial.Unsafe as Unsafe

data NotificationPermission
  = DefaultNotificationPermission
  | GrantedNotificationPermission
  | DeniedNotificationPermission

derive instance eqNotificationPermission :: Eq NotificationPermission
derive instance ordNotificationPermission :: Ord NotificationPermission
derive instance genericNotificationPermission :: Generic NotificationPermission _

instance showNotificationPermission :: Show NotificationPermission where
  show DefaultNotificationPermission = "default"
  show GrantedNotificationPermission = "granted"
  show DeniedNotificationPermission = "denied"

newtype Title = Title String

derive instance eqTitle :: Eq Title
derive instance ordTitle :: Ord Title
derive instance genericTitle :: Generic Title _
derive instance newtypeTitle :: Newtype Title _

instance showTitle :: Show Title where
  show (Title t) = "Title " <> t

newtype Body = Body String

derive instance eqBody :: Eq Body
derive instance ordBody :: Ord Body
derive instance genericBody :: Generic Body _
derive instance newtypeBody :: Newtype Body _

instance showBody :: Show Body where
  show (Body b) = "Body " <> b

newtype NotificationData = NotificationData Foreign

derive instance genericNotificationData :: Generic NotificationData _
derive instance newtypeNotificationData :: Newtype NotificationData _

requestPermission :: Aff NotificationPermission
requestPermission =
  unsafeStringToNotificationPermission <$> AffCompat.fromEffectFnAff requestPermission_

createNotification :: Title -> Body -> NotificationData -> Effect Notification
createNotification title body notificationData =
  createNotification_ (unwrap title) (unwrap body) (unwrap notificationData)

unsafeStringToNotificationPermission :: String -> NotificationPermission
unsafeStringToNotificationPermission "default" = DefaultNotificationPermission
unsafeStringToNotificationPermission "granted" = GrantedNotificationPermission
unsafeStringToNotificationPermission "denied" = DeniedNotificationPermission
unsafeStringToNotificationPermission other =
  Unsafe.unsafeCrashWith $ "Unknown notification permission: " <> other

foreign import requestPermission_ :: EffectFnAff String
foreign import createNotification_ :: String -> String -> Foreign -> Effect Notification
foreign import data Notification :: Type
