module Cleff.Trace where

import           Cleff
import           Control.Monad.IO.Class (liftIO)

data Trace :: Effect where
  Trace :: String -> Trace m ()
makeEffect ''Trace

runTrace :: IOE :> es => Eff (Trace ': es) a -> Eff es a
runTrace = interpret \case
  Trace s -> liftIO $ putStrLn s
{-# INLINE runTrace #-}
