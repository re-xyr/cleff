module Cleff.Timeout where

import           Cleff
import qualified System.Timeout as T

data Timeout :: Effect where
  Timeout :: Int -> m a -> Timeout m (Maybe a)
makeEffect ''Timeout

runTimeout :: IOE :> es => Eff (Timeout ': es) a -> Eff es a
runTimeout = interpretIO \case
  Timeout n m -> T.timeout n (runInIO m)
{-# INLINE runTimeout #-}
