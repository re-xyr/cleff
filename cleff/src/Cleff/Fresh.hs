module Cleff.Fresh where

import           Cleff
import           Cleff.State
import           Control.Monad.IO.Class (liftIO)
import           Data.Unique

data Fresh u :: Effect where
  Fresh :: Fresh u m u
makeEffect ''Fresh

freshIntToState :: Eff (Fresh Int ': es) a -> Eff (State Int ': es) a
freshIntToState = reinterpret \case
  Fresh -> state \s -> (s, s + 1)
{-# INLINE freshIntToState #-}

runFreshUnique :: IOE :> es => Eff (Fresh Unique ': es) a -> Eff es a
runFreshUnique = interpret \case
  Fresh -> liftIO newUnique
{-# INLINE runFreshUnique #-}
