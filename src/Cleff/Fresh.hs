module Cleff.Fresh where

import           Cleff
import           Cleff.State

data Fresh :: Effect where
  Fresh :: Fresh m Int
makeEffect ''Fresh

runFresh :: Int -> Eff (Fresh ': es) a -> Eff es (a, Int)
runFresh n = runState n . reinterpret \case
  Fresh -> state \s -> (s, s + 1)
{-# INLINE runFresh #-}
