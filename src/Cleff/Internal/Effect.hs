{-# OPTIONS_HADDOCK not-home #-}
-- |
-- Copyright: (c) 2021 Xy Ren
-- License: BSD3
-- Maintainer: xy.r@outlook.com
-- Stability: unstable
-- Portability: non-portable (GHC only)
--
-- This module contains definitions of some basic types related to effects. You won't need this module directly;
-- these functionalities are reexported in the "Cleff" module.
--
-- __This is an /internal/ module and its API may change even between minor versions.__ Therefore you should be
-- extra careful if you're to depend on this module.
module Cleff.Internal.Effect (Effect, (:>), (:>>), type (++), type (~>)) where

import           Data.Kind (Constraint, Type)
import           Data.Rec  (Elem, type (++), type (~>))

-- | The type of effects. An effect @e m a@ takes an effect monad type @m :: 'Type' -> 'Type'@ and a result type
-- @a :: 'Type'@.
type Effect = (Type -> Type) -> Type -> Type

-- | @e ':>' es@ means the effect @e@ is present in the effect stack @es@, and therefore can be used in an
-- @'Cleff.Eff' es@ computation.
type (:>) = Elem
infix 0 :>

-- | @xs ':>>' es@ means the list of effects @xs@ are all present in the effect stack @es@. This is a convenient type
-- alias for @(e1 ':>' es, ..., en ':>' es)@.
type family xs :>> es :: Constraint where
  '[] :>> _ = ()
  (x ': xs) :>> es = (x :> es, xs :>> es)
infix 0 :>>
