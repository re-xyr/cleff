module Cleff.Trace where

import           Cleff
import           Cleff.Output
import           System.IO

data Trace :: Effect where
  Trace :: String -> Trace m ()
makeEffect ''Trace

runTraceHandle :: IOE :> es => Handle -> Eff (Trace ': es) a -> Eff es a
runTraceHandle h = interpretIO \case
  Trace s -> hPutStrLn h s
{-# INLINE runTraceHandle #-}

runTraceStdout :: IOE :> es => Eff (Trace ': es) a -> Eff es a
runTraceStdout = runTraceHandle stdout
{-# INLINE runTraceStdout #-}

runTraceStderr :: IOE :> es => Eff (Trace ': es) a -> Eff es a
runTraceStderr = runTraceHandle stderr
{-# INLINE runTraceStderr #-}

ignoreTrace :: Eff (Trace ': es) a -> Eff es a
ignoreTrace = interpret \case
  Trace _ -> pure ()
{-# INLINE ignoreTrace #-}

traceToOutput :: Eff (Trace ': es) a -> Eff (Output String ': es) a
traceToOutput = reinterpret \case
  Trace s -> output s
{-# INLINE traceToOutput #-}
