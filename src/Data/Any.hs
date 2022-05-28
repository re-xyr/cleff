module Data.Any (Any, pattern Any, fromAny) where

import           GHC.Exts      (Any)
import           Unsafe.Coerce (unsafeCoerce)

-- | A pattern synonym for coercing values to and from 'GHC.Exts.Any'. This is not any less unsafe but prevents
-- possible misuses.
pattern Any :: forall a. a -> Any
pattern Any {fromAny} <- (unsafeCoerce -> fromAny)
  where Any = unsafeCoerce
{-# COMPLETE Any #-}
