{-# LANGUAGE Trustworthy #-}
-- |
-- Copyright: (c) 2021 Xy Ren
-- License: BSD3
-- Maintainer: xy.r@outlook.com
-- Stability: experimental
-- Portability: non-portable (GHC only)
module Cleff.Output
  ( -- * Effect
    Output (..)
  , -- * Operations
    output
  , -- * Interpretations
    outputToListState, outputToWriter, ignoreOutput, runOutputEff, mapOutput, bindOutput
  ) where

import           Cleff
import           Cleff.State
import           Cleff.Writer

-- * Effect

-- | An effect that is capable of sending outputs, for example to a log file or an output stream.
data Output o :: Effect where
  Output :: o -> Output o m ()

-- * Operations

makeEffect ''Output

-- * Interpretations

-- | Run an 'Output' effect by accumulating a list. Note that outputs are being prepended to the head of the list, so
-- in many cases you would want to 'reverse' the result.
outputToListState :: Eff (Output o ': es) ~> Eff (State [o] ': es)
outputToListState = reinterpret \case
  Output x -> modify (x :)
{-# INLINE outputToListState #-}

-- | Run an 'Output' effect by translating it into a 'Writer'.
outputToWriter :: (o -> o') -> Eff (Output o ': es) ~> Eff (Writer o' ': es)
outputToWriter f = reinterpret \case
  Output x -> tell $ f x
{-# INLINE outputToWriter #-}

-- | Ignore outputs of an 'Output' effect altogether.
ignoreOutput :: Eff (Output o ': es) ~> Eff es
ignoreOutput = interpret \case
  Output _ -> pure ()
{-# INLINE ignoreOutput #-}

-- | Run an 'Output' effect by performing a computation for each output.
runOutputEff :: (o -> Eff es ()) -> Eff (Output o ': es) ~> Eff es
runOutputEff m = interpret \case
  Output x -> m x
{-# INLINE runOutputEff #-}

-- | Transform an 'Output' effect into another one already in the effect stack, by a pure function.
--
-- @since 0.2.1.0
mapOutput :: Output o' :> es => (o -> o') -> Eff (Output o ': es) ~> Eff es
mapOutput f = interpret \case
  Output x -> output $ f x
{-# INLINE mapOutput #-}

-- | Transform an 'Input' effect into another one already in the effect stack, by an effectful computation.
--
-- @since 0.2.1.0
bindOutput :: Output o' :> es => (o -> Eff es o') -> Eff (Output o ': es) ~> Eff es
bindOutput f = interpret \case
  Output x -> output =<< f x
{-# INLINE bindOutput #-}
