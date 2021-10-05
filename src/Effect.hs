module Effect (Effect, Handler, Eff, Legal, (:>), (:>>), send, interpret, reinterpret, reinterpret2, reinterpret3, reinterpretN, interpose, unliftIO, unlift, withLift, raise, subsume, runPure) where

import           Data.Kind               (Constraint)
import           Effect.Internal.Handler
import           Effect.Internal.Monad

type family (xs :: [Effect]) :>> (ys :: [Effect]) :: Constraint where
  '[] :>> ys = ()
  (x ': xs) :>> ys = (x :> ys, xs :>> ys)
