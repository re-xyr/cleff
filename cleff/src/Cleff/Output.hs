module Cleff.Output where

import           Cleff
import           Cleff.State
import           Cleff.Writer
import           Data.Typeable (Typeable)

data Output o :: Effect where
  Output :: o -> Output o m ()
makeEffect ''Output

outputToListState :: Typeable o => Eff (Output o ': es) a -> Eff (State [o] ': es) a
outputToListState = reinterpret \case
  Output x -> modify (x :)
{-# INLINE outputToListState #-}

outputToWriter :: (Typeable o, Typeable o') => (o -> o') -> Eff (Output o ': es) a -> Eff (Writer o' ': es) a
outputToWriter f = reinterpret \case
  Output x -> tell $ f x
{-# INLINE outputToWriter #-}

ignoreOutput :: Typeable o => Eff (Output o ': es) a -> Eff es a
ignoreOutput = interpret \case
  Output _ -> pure ()
{-# INLINE ignoreOutput #-}

runOutputEff :: Typeable o => (o -> Eff es ()) -> Eff (Output o ': es) a -> Eff es a
runOutputEff m = interpret \case
  Output x -> m x
{-# INLINE runOutputEff #-}
