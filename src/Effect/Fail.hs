{-# OPTIONS_GHC -Wno-orphans #-}
module Effect.Fail where

import           Control.Exception  (Exception)
import           Control.Monad.Fail (MonadFail (..))
import           Effect
import           Effect.Error
import           Prelude            hiding (MonadFail (..))
import           UnliftIO.Exception (throwIO)

data Fail :: Effect where
  Fail :: String -> Fail m a

instance Exception String

instance Fail :> es => MonadFail (Eff es) where
  fail = send . Fail

runFail :: Eff (Fail ': es) a -> Eff es (Either String a)
runFail = runError . reinterpret \case
  Fail msg -> throwError msg
{-# INLINE runFail #-}

runFailIO :: IOE :> es => Eff (Fail ': es) a -> Eff es a
runFailIO = interpret \case
  Fail msg -> throwIO msg
{-# INLINE runFailIO #-}
