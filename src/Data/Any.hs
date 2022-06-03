-- |
-- Copyright: (c) 2021 Xy Ren
-- License: BSD3
-- Maintainer: xy.r@outlook.com
-- Stability: unstable
-- Portability: non-portable (GHC only)
module Data.Any (Any, pattern Any, fromAny) where

import           GHC.Exts      (Any)
import           Unsafe.Coerce (unsafeCoerce)

-- | A pattern synonym for coercing values to and from t'Any'. This is not any less unsafe but prevents possible
-- misuses.
pattern Any :: forall a. a -> Any
pattern Any {fromAny} <- (unsafeCoerce -> fromAny)
  where Any = unsafeCoerce
{-# COMPLETE Any #-}
