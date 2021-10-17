{-# OPTIONS_GHC -Wno-orphans #-}
module Cleff.Fail where

import           Cleff
import           Cleff.Error
import           Control.Exception  (Exception)
import           Control.Monad.Fail (MonadFail (..))
import           Prelude            hiding (MonadFail (..))
import           UnliftIO.Exception (throwIO)

-- * Effect

-- | An effect that expresses failure with a message. This effect allows the use of the 'MonadFail' class.
data Fail :: Effect where
  Fail :: String -> Fail m a

instance Exception String

instance Fail :> es => MonadFail (Eff es) where
  fail = send . Fail

-- * Interpretations

-- | Run a 'Fail' effect in terms of 'Error'.
runFail :: Eff (Fail ': es) a -> Eff es (Either String a)
runFail = runError . reinterpret \case
  Fail msg -> throwError msg
{-# INLINE runFail #-}

-- | Run a 'Fail' effect in terms of throwing exceptions in 'IO'.
runFailIO :: IOE :> es => Eff (Fail ': es) ~> Eff es
runFailIO = interpret \case
  Fail msg -> throwIO msg
{-# INLINE runFailIO #-}
