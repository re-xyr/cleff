-- | This module contains definitions of some basic types related to effects. You won't need this module most of the
-- times; most functionalities are reexported in the "Cleff" module.
{-# OPTIONS_HADDOCK not-home #-}
module Cleff.Internal.Effect (Effect, (:>), (:>>), type (++), type (~>)) where

import           Data.Kind     (Constraint, Type)
import           Data.Typeable (Typeable)
import           GHC.TypeLits  (ErrorMessage ((:<>:)))
import qualified GHC.TypeLits  as GHC

-- | The type of effects. An effect @e m a@ takes an effect monad type @m :: 'Type' -> 'Type'@ and result type
-- @a :: 'Type'@.
type Effect = (Type -> Type) -> Type -> Type

-- | Constraint that indicates an effect @e@ is present in the effect stack @es@ (thus 'Cleff.send'able).
class Typeable e => (e :: Effect) :> (es :: [Effect])
instance {-# OVERLAPPING #-} Typeable e => e :> (e ': es)
instance e :> es => e :> (e' ': es)
type EffectNotFound e = 'GHC.Text "The effect '" ':<>: 'GHC.ShowType e
  ':<>: 'GHC.Text "' is not present in the constraint"
instance (Typeable e, GHC.TypeError (EffectNotFound e)) => e :> '[]

-- | Constraint that indicates a list effect @xs@ is present in the effect stack @es@ (thus 'Cleff.send'able). This is
-- a convenient type alias for @(e1 ':>' es, ..., en ':>' es)@.
type family xs :>> es :: Constraint where
  '[] :>> es = ()
  (x ': xs) :>> es = (x :> es, xs :>> es)

-- | Type level list concatenation.
type family xs ++ ys where
  '[] ++ ys = ys
  (x ': xs) ++ ys = x ': (xs ++ ys)

-- | The type of natural transformation from @f@ to @g@.
type f ~> g = forall a. f a -> g a
