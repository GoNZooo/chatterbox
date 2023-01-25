module Web.Notifications
  ( requestPermission
  , NotificationPermission(..)
  ) where

import Prelude

import Data.Generic.Rep (class Generic)
import Effect.Aff (Aff)
import Effect.Aff.Compat (EffectFnAff)
import Effect.Aff.Compat as AffCompat
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

requestPermission :: Aff NotificationPermission
requestPermission =
  unsafeStringToNotificationPermission <$> AffCompat.fromEffectFnAff requestPermission_

unsafeStringToNotificationPermission :: String -> NotificationPermission
unsafeStringToNotificationPermission "default" = DefaultNotificationPermission
unsafeStringToNotificationPermission "granted" = GrantedNotificationPermission
unsafeStringToNotificationPermission "denied" = DeniedNotificationPermission
unsafeStringToNotificationPermission other =
  Unsafe.unsafeCrashWith $ "Unknown notification permission: " <> other

foreign import requestPermission_ :: EffectFnAff String
