{-# OPTIONS_GHC -Wno-orphans #-}
module Effect.Fail where

import           Control.Exception  (Exception)
import           Effect
import           Effect.Error
import           UnliftIO.Exception (throwIO)

data Fail :: Effect where
  Fail :: String -> Fail m a

instance Exception String

instance Fail :> es => MonadFail (Eff es) where
  fail = send . Fail
  {-# INLINE fail #-}

runFail :: Exception String => Eff (Fail ': es) a -> Eff es (Either String a)
runFail = runError . reinterpret \case
  Fail msg -> throwError msg

runFailIO :: (IOE :> es, Exception String) => Eff (Fail ': es) a -> Eff es a
runFailIO = interpret \case
  Fail msg -> throwIO msg
