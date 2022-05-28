{-# LANGUAGE Trustworthy #-}
{-# OPTIONS_GHC -Wno-orphans #-}
-- |
-- Copyright: (c) 2021 Xy Ren
-- License: BSD3
-- Maintainer: xy.r@outlook.com
-- Stability: experimental
-- Portability: non-portable (GHC only)
module Cleff.Fail
  ( -- * Effect
    Fail (..)
    -- * Interpretations
  , runFail
  , runFailIO
  ) where

import           Cleff
import           Cleff.Error
import qualified Control.Monad.Fail as Fail

-- * Effect

-- | An effect that expresses failure with a message. This effect allows the use of the 'MonadFail' class.
data Fail :: Effect where
  Fail :: String -> Fail m a

instance Fail :> es => Fail.MonadFail (Eff es) where
  fail = send . Fail

-- * Interpretations

-- | Run a 'Fail' effect in terms of 'Error'.
runFail :: Eff (Fail : es) a -> Eff es (Either String a)
runFail = runError . reinterpret \case
  Fail msg -> throwError msg

-- | Run a 'Fail' effect in terms of throwing exceptions in 'IO'.
runFailIO :: IOE :> es => Eff (Fail : es) ~> Eff es
runFailIO = interpret \case
  Fail msg -> liftIO $ Fail.fail msg
