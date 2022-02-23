{-# LANGUAGE Trustworthy #-}
-- |
-- Copyright: (c) 2021 Xy Ren
-- License: BSD3
-- Maintainer: xy.r@outlook.com
-- Stability: experimental
-- Portability: non-portable (GHC only)
module Cleff.Fresh
  ( -- * Effect
    Fresh (..)
    -- * Operations
  , fresh
    -- * Interpretations
  , freshIntToState
  , freshEnumToState
  , runFreshAtomicCounter
  , runFreshUnique
  ) where

import           Cleff
import           Cleff.Internal.Base  (thisIsPureTrustMe)
import           Cleff.State
import           Data.Atomics.Counter (incrCounter, newCounter)
import           Data.Unique          (Unique, newUnique)

-- * Effect

-- | An effect capable of generating unique values. This effect can be useful in generating variable indices.
data Fresh u :: Effect where
  Fresh :: Fresh u m u

-- * Operations

makeEffect_ ''Fresh

-- | Obtain a fresh unique value.
fresh :: Fresh u :> es => Eff es u

-- * Interpretations

-- | Interpret a @'Fresh' a@ in terms of @'State' a@ for any 'Enum'. Every time 'succ' is called to generate the next
-- value.
--
-- @since 0.2.1.0
freshEnumToState :: Enum a => Eff (Fresh a : es) ~> Eff (State a : es)
freshEnumToState = reinterpret \case
  Fresh -> state \s -> (s, succ s)
{-# INLINE freshEnumToState #-}

-- | Interpret a @'Fresh' 'Int'@ effect in terms of @'State' 'Int'@. This is a specialized version of
-- 'freshEnumToState'.
freshIntToState :: Eff (Fresh Int : es) ~> Eff (State Int : es)
freshIntToState = freshEnumToState
{-# INLINE freshIntToState #-}

-- | Interpret a @'Fresh' 'Int'@ effect in terms of a 'Data.Atomics.Counter.AtomicCounter'. This is usually faster
-- than 'runFreshUnique'.
--
-- @since 0.2.1.0
runFreshAtomicCounter :: Eff (Fresh Int : es) ~> Eff es
runFreshAtomicCounter m = thisIsPureTrustMe do
  counter <- liftIO $ newCounter minBound
  reinterpret (\case
    Fresh -> liftIO $ incrCounter 1 counter) m
{-# INLINE runFreshAtomicCounter #-}

-- | Interpret a @'Fresh' 'Unique'@ effect in terms of IO actions. This is slower than 'runFreshAtomicCounter', but it
-- won't overflow on @'maxBound' :: 'Int'@.
runFreshUnique :: IOE :> es => Eff (Fresh Unique : es) ~> Eff es
runFreshUnique = interpret \case
  Fresh -> liftIO newUnique
{-# INLINE runFreshUnique #-}
