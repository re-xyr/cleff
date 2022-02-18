{-# LANGUAGE Trustworthy #-}
-- |
-- Copyright: (c) 2021 Xy Ren
-- License: BSD3
-- Maintainer: xy.r@outlook.com
-- Stability: experimental
-- Portability: non-portable (GHC only)
module Cleff.Input
  ( -- * Effect
    Input (..)
    -- * Operations
  , input
  , inputs
    -- * Interpretations
  , runInputConst
  , inputToListState
  , inputToReader
  , runInputEff
  , mapInput
  , bindInput
  ) where

import           Cleff
import           Cleff.Reader
import           Cleff.State

-- * Effect

-- | An effect that is capable of reading from some input source, such as an input stream.
data Input i :: Effect where
  Input :: Input i m i

-- * Operations

makeEffect_ ''Input

-- | Read an input value from an input source.
input :: Input i :> es => Eff es i

-- | Apply a function to the result of 'input'.
inputs :: Input i :> es => (i -> i') -> Eff es i'
inputs f = f <$> input

-- * Interpretations

-- | Run an 'Input' effect by giving a constant input value.
runInputConst :: i -> Eff (Input i ': es) ~> Eff es
runInputConst x = interpret \case
  Input -> pure x
{-# INLINE runInputConst #-}

-- | Run an 'Input' effect by going through a list of values.
inputToListState :: Eff (Input (Maybe i) ': es) ~> Eff (State [i] ': es)
inputToListState = reinterpret \case
  Input -> state \case
    []     -> (Nothing, [])
    x : xs -> (Just x, xs)
{-# INLINE inputToListState #-}

-- | Run an 'Input' in terms of a 'Reader'.
--
-- @since 0.2.1.0
inputToReader :: Eff (Input i ': es) ~> Eff (Reader i ': es)
inputToReader = reinterpret \case
  Input -> ask
{-# INLINE inputToReader #-}

-- | Run an 'Input' effect by performing a computation for each input request.
runInputEff :: Eff es i -> Eff (Input i ': es) ~> Eff es
runInputEff m = interpret \case
  Input -> m
{-# INLINE runInputEff #-}

-- | Transform an 'Input' effect into another one already in the effect stack, by a pure function.
--
-- @since 0.2.1.0
mapInput :: Input i' :> es => (i' -> i) -> Eff (Input i ': es) ~> Eff es
mapInput f = interpret \case
  Input -> f <$> input
{-# INLINE mapInput #-}

-- | Transform an 'Input' effect into another one already in the effect stack, by an effectful computation.
--
-- @since 0.2.1.0
bindInput :: Input i' :> es => (i' -> Eff es i) -> Eff (Input i ': es) ~> Eff es
bindInput f = interpret \case
  Input -> f =<< input
{-# INLINE bindInput #-}
