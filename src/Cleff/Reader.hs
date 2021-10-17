module Cleff.Reader where

import           Cleff
import           Data.Typeable (Typeable)

data Reader r :: Effect where
  Ask :: Reader r m r
  Local :: (r -> r) -> m a -> Reader r m a
makeEffect ''Reader

asks :: Reader r :> es => (r -> s) -> Eff es s
asks = (<$> ask)

runReader :: Typeable r => r -> Eff (Reader r ': es) ~> Eff es
runReader r = interpret \case
  Ask        -> pure r
  Local f m' -> runReader (f r) $ runHere m'
{-# INLINE runReader #-}
