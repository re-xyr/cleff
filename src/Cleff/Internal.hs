-- |
-- Copyright: (c) 2021 Xy Ren
-- License: BSD3
-- Maintainer: xy.r@outlook.com
-- Stability: unstable
-- Portability: non-portable (GHC only)
--
-- This module contains common definitions for the @cleff@ internals.
--
-- __This is an /internal/ module and its API may change even between minor versions.__ Therefore you should be
-- extra careful if you're to depend on this module.
module Cleff.Internal
  ( -- * Basic types
    Effect
  , type (~>)
  , type (++)
    -- * The 'Any' type
  , Any
  , fromAny
  , toAny
  ) where

import           Data.Kind     (Type)
import           GHC.Exts      (Any)
import           Unsafe.Coerce (unsafeCoerce)

-- | The type of effects. An effect @e m a@ takes an effect monad type @m :: 'Type' -> 'Type'@ and a result type
-- @a :: 'Type'@.
type Effect = (Type -> Type) -> Type -> Type

-- | A natural transformation from @f@ to @g@. With this, instead of writing
--
-- @
-- runSomeEffect :: 'Cleff.Eff' (SomeEffect : es) a -> 'Cleff.Eff' es a
-- @
--
-- you can write:
--
-- @
-- runSomeEffect :: 'Cleff.Eff' (SomeEffect : es) ~> 'Cleff.Eff' es
-- @
type f ~> g = ∀ a. f a -> g a

-- | Type level list concatenation.
type family xs ++ ys where
  '[] ++ ys = ys
  (x : xs) ++ ys = x : (xs ++ ys)
infixr 5 ++

-- | Coerce any boxed value into 'Any'.
toAny :: a -> Any
toAny = unsafeCoerce
{-# INLINE toAny #-}

-- | Coerce 'Any' to a boxed value. This is /generally unsafe/ and it is your responsibility to ensure that the type
-- you're coercing into is the original type that the 'Any' is coerced from.
fromAny :: Any -> a
fromAny = unsafeCoerce
{-# INLINE fromAny #-}
