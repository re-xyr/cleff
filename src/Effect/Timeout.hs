module Effect.Timeout where

import           Control.Monad.IO.Class (liftIO)
import           Effect
import qualified UnliftIO.Timeout       as T

data Timeout :: Effect where
  Timeout :: Int -> m a -> Timeout m (Maybe a)

timeout :: Timeout :> es => Int -> Eff es a -> Eff es (Maybe a)
timeout n m = send $ Timeout n m

runTimeout :: IOE :> es => Eff (Timeout ': es) a -> Eff es a
runTimeout = interpret \case
  Timeout n m -> liftIO $ T.timeout n (runInIO m)
{-# INLINE runTimeout #-}
