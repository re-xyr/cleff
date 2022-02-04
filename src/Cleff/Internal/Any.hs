-- |
-- Copyright: (c) 2021 Xy Ren
-- License: BSD3
-- Maintainer: xy.r@outlook.com
-- Stability: unstable
-- Portability: non-portable (GHC only)
--
-- This module contains utility functions for 'Any'.
--
-- __This is an /internal/ module and its API may change even between minor versions.__ Therefore you should be
-- extra careful if you're to depend on this module.
module Cleff.Internal.Any (Any, fromAny, toAny) where

import           GHC.Exts      (Any)
import           Unsafe.Coerce (unsafeCoerce)

-- | Coerce any boxed value into 'Any'.
toAny :: a -> Any
toAny = unsafeCoerce
{-# INLINE toAny #-}

-- | Coerce 'Any' to a boxed value. This is /generally unsafe/ and it is your responsibility to ensure that the type
-- you're coercing into is the original type that the 'Any' is coerced from.
fromAny :: Any -> a
fromAny = unsafeCoerce
{-# INLINE fromAny #-}
