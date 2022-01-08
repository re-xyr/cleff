{-# OPTIONS_HADDOCK not-home #-}
-- | This module contains definitions of some basic types related to effects. You won't need this module most of the
-- times; most functionalities are reexported in the "Cleff" module.
module Cleff.Internal.Effect (Effect, (:>), (:>>), type (++), type (~>)) where

import           Data.Kind (Constraint, Type)
import           Data.Rec  (Elem, type (++), type (~>))

-- | The type of effects. An effect @e m a@ takes an effect monad type @m :: 'Type' -> 'Type'@ and result type
-- @a :: 'Type'@.
type Effect = (Type -> Type) -> Type -> Type

-- | Constraint that indicates an effect @e@ is present in the effect stack @es@ (thus 'Cleff.send'able).
type (:>) = Elem
infix 0 :>

-- | A convenient type alias for @(e1 ':>' es, ..., en ':>' es)@.
type family xs :>> es :: Constraint where
  '[] :>> es = ()
  (x ': xs) :>> es = (x :> es, xs :>> es)
infix 0 :>>
