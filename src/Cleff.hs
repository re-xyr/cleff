module Cleff
  ( -- * The @Eff@ monad
    Effect, Eff, (:>), (:>>)
  , -- * The @IOE@ effect
    IOE
  , -- * Performing effect operations
    send
  , -- * Unwrapping @Eff@
    runPure, runIOE
  , -- * Trivial effects handling
    raise, raiseN, subsume, subsumeN
  , -- * Interpreting effects
    Handler, Interpreter, interpret, reinterpret, reinterpret2, reinterpret3, reinterpretN, interpose,
    InterpreterIO, interpretIO
  , -- * Combinators for interpreting higher order effects
    runInIO, runHere, runThere, withLiftIO, withLiftEff
  , -- * Template Haskell
    makeEffect, makeEffect_
  ) where

import           Cleff.Internal.Base
import           Cleff.Internal.Handler
import           Cleff.Internal.Monad
import           Cleff.Internal.TH
