module Effect.Trace where

import           Control.Monad.IO.Class (liftIO)
import           Effect
import           Effect.IO              (IOE)

data Trace :: Effect where
  Trace :: String -> Trace m ()

trace :: Trace :> es => String -> Eff es ()
trace s = send $ Trace s
{-# INLINE trace #-}

runTrace :: IOE :> es => Eff (Trace ': es) a -> Eff es a
runTrace = interpret \case
  Trace s -> liftIO $ putStrLn s
