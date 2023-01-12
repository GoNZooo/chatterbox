module Chatterbox.Types where

import Control.Applicative (class Applicative)
import Control.Apply (class Apply)
import Control.Bind (class Bind)
import Control.Monad (class Monad)
import Data.Eq (class Eq)
import Data.Functor (class Functor)
import Data.Generic.Rep (class Generic)
import Data.Ord (class Ord)

data ErlangResult l r
  = Error l
  | Ok r

erlangResult :: forall l r a. (l -> a) -> (r -> a) -> ErlangResult l r -> a
erlangResult f _ (Error l) = f l
erlangResult _ f (Ok r) = f r

derive instance eqErlangResult :: (Eq l, Eq r) => Eq (ErlangResult l r)
derive instance ordErlangResult :: (Ord l, Ord r) => Ord (ErlangResult l r)
derive instance genericErlangResult :: Generic (ErlangResult l r) _

instance functorErlangResult :: Functor (ErlangResult l) where
  map f (Ok r) = Ok (f r)
  map _ (Error l) = Error l

instance applyErlangResult :: Apply (ErlangResult l) where
  apply (Ok f) (Ok r) = Ok (f r)
  apply (Error l) _ = Error l
  apply _ (Error l) = Error l

instance applicativeErlangResult :: Applicative (ErlangResult l) where
  pure = Ok

instance bindErlangResult :: Bind (ErlangResult l) where
  bind (Ok r) f = f r
  bind (Error l) _ = Error l

instance monadErlangResult :: Monad (ErlangResult l)

foreign import data UnsafeStartResult :: Type
