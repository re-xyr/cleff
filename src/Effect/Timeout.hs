module Effect.Timeout where

import           Control.Monad.IO.Class (liftIO)
import           Effect
import qualified UnliftIO.Timeout       as T

data Timeout :: Effect where
  Timeout :: Int -> m a -> Timeout m (Maybe a)
makeEffect ''Timeout

runTimeout :: IOE :> es => Eff (Timeout ': es) a -> Eff es a
runTimeout = interpret \case
  Timeout n m -> liftIO $ T.timeout n (runInIO m)
{-# INLINE runTimeout #-}
