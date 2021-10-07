module Effect.Fresh where

import           Effect
import           Effect.State

data Fresh :: Effect where
  Fresh :: Fresh m Int

fresh :: Fresh :> es => Eff es Int
fresh = send Fresh

runLocalFresh :: Int -> Eff (Fresh ': es) a -> Eff es (a, Int)
runLocalFresh n = runLocalState n . reinterpret \case
  Fresh -> state \s -> (s, s + 1)
