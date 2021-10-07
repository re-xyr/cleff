module Effect.Timeout where

import           Effect
import qualified UnliftIO.Timeout as T

data Timeout :: Effect where
  Timeout :: Int -> m a -> Timeout m (Maybe a)

timeout :: Timeout :> es => Int -> Eff es a -> Eff es (Maybe a)
timeout n m = send $ Timeout n m

runTimeout :: IOE :> es => Eff (Timeout ': es) a -> Eff es a
runTimeout = interpretH \handle -> \case
  Timeout n m -> T.timeout n (interpret handle $ unlift m)
