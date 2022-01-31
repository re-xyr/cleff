module Cleff.Output
  ( -- * Effect
    Output (..)
  , -- * Operations
    output
  , -- * Interpretations
    outputToListState, outputToWriter, ignoreOutput, runOutputEff
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

-- | Run an 'Output' effect by accumulating a list.
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
