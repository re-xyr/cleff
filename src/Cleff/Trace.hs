module Cleff.Trace where

import           Cleff

data Trace :: Effect where
  Trace :: String -> Trace m ()
makeEffect ''Trace

runTrace :: IOE :> es => Eff (Trace ': es) a -> Eff es a
runTrace = interpretIO \case
  Trace s -> putStrLn s
{-# INLINE runTrace #-}
