module Cleff.Fresh
  ( -- * Effect
    Fresh (..)
  , -- * Operations
    fresh
  , -- * Interpretations
    freshIntToState, runFreshUnique
  ) where

import           Cleff
import           Cleff.State
import           Data.Unique (Unique, newUnique)

-- * Effect

-- | An effect capable of generating unique values. This effect can be useful in generating variable indices.
data Fresh u :: Effect where
  Fresh :: Fresh u m u

-- * Operations

makeEffect ''Fresh

-- * Interpretations

-- | Interpret a @'Fresh' 'Int'@ effect in terms of @'State' 'Int'@.
freshIntToState :: Eff (Fresh Int ': es) ~> Eff (State Int ': es)
freshIntToState = reinterpret \case
  Fresh -> state \s -> (s, s + 1)
{-# INLINE freshIntToState #-}

-- | Interpret a @'Fresh' 'Unique'@ effect in terms of IO actions.
runFreshUnique :: IOE :> es => Eff (Fresh Unique ': es) ~> Eff es
runFreshUnique = interpret \case
  Fresh -> liftIO newUnique
{-# INLINE runFreshUnique #-}
