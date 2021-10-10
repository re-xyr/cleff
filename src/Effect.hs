module Effect
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
    Handler, interpret, reinterpret, reinterpret2, reinterpret3, reinterpretN, interpose
  , -- * Combinators for interpreting higher order effects
    runInIO, runHere, runHere', runThere, withLiftIO, withLiftEff
  , -- * Template Haskell
    makeEffect, makeEffect_
  ) where

import           Effect.Internal.Base
import           Effect.Internal.Handler
import           Effect.Internal.Monad
import           Effect.Internal.TH
