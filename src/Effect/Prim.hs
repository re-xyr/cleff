{-# OPTIONS_GHC -Wno-orphans #-}
module Effect.Prim where

import           Control.Monad.IO.Class  (liftIO)
import           Control.Monad.Primitive (PrimMonad (..))
import           Effect
import           GHC.Exts                (RealWorld, State#)
import           GHC.IO                  (IO (IO))

data Prim :: Effect where
  Primitive :: (State# RealWorld -> (# State# RealWorld, a #)) -> Prim m a

instance Prim :> es => PrimMonad (Eff es) where
  type PrimState (Eff es) = RealWorld
  primitive = send . Primitive

runPrim :: IOE :> es => Eff (Prim ': es) a -> Eff es a
runPrim = interpret \case
  Primitive m -> liftIO (IO m)
