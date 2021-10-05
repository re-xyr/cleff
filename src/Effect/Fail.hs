{-# OPTIONS_GHC -Wno-orphans #-}
module Effect.Fail where

import           Control.Exception (Exception)
import           Effect
import           Effect.Error

data Fail :: Effect where
  Fail :: String -> Fail m a

instance Exception String

runFail :: Exception String => Eff (Fail ': es) a -> Eff es (Either String a)
runFail = runError . reinterpret \case
  Fail msg -> throwError msg
