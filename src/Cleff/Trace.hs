module Cleff.Trace where

import           Cleff
import           Cleff.Output
import           System.IO    (Handle, hPutStrLn, stderr, stdout)

-- * Effect

-- | An effect capable of logging messages, mostly for debugging purposes.
data Trace :: Effect where
  Trace :: String -> Trace m ()

-- * Operations

makeEffect ''Trace

-- * Interpretations

-- | Run the 'Trace' effect by writing to a 'Handle'.
runTraceHandle :: IOE :> es => Handle -> Eff (Trace ': es) a -> Eff es a
runTraceHandle h = interpretIO \case
  Trace s -> hPutStrLn h s
{-# INLINE runTraceHandle #-}

-- | Run the 'Trace' effect by writing to 'stdout'.
runTraceStdout :: IOE :> es => Eff (Trace ': es) ~> Eff es
runTraceStdout = runTraceHandle stdout
{-# INLINE runTraceStdout #-}

-- | Run the 'Trace' effect by writing to 'stderr'.
runTraceStderr :: IOE :> es => Eff (Trace ': es) ~> Eff es
runTraceStderr = runTraceHandle stderr
{-# INLINE runTraceStderr #-}

-- | Run the 'Trace' effect by ignoring all outputs altogether.
ignoreTrace :: Eff (Trace ': es) ~> Eff es
ignoreTrace = interpret \case
  Trace _ -> pure ()
{-# INLINE ignoreTrace #-}

-- | Transform the 'Trace' effect into an @'Output' 'String'@ effect.
traceToOutput :: Eff (Trace ': es) ~> Eff (Output String ': es)
traceToOutput = reinterpret \case
  Trace s -> output s
{-# INLINE traceToOutput #-}
