{-# OPTIONS_GHC -Wno-orphans #-}
module Effect.IO where

import           Control.Monad.Base
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Control
import           Effect
import           Effect.Internal.Monad
import           Effect.Primitive.IO
import           UnliftIO

data IOE :: Effect where
  Lift :: IO a -> IOE m a
  Unlift :: ((forall x. m x -> IO x) -> IO a) -> IOE m a

-- Encouraged usage built upon @unliftio@
instance IOE :> es => MonadIO (Eff es) where
  liftIO = send . Lift
  {-# INLINE liftIO #-}

instance IOE :> es => MonadUnliftIO (Eff es) where
  withRunInIO f = send $ Unlift f
  {-# INLINE withRunInIO #-}

-- Compatibility with @exceptions@. This is not encouraged usage
instance IOE :> es => MonadThrow (Eff es) where
  throwM = throwIO
  {-# INLINE throwM #-}

instance IOE :> es => MonadCatch (Eff es) where
  catch = UnliftIO.catch
  {-# INLINE catch #-}

instance IOE :> es => MonadMask (Eff es) where
  mask = UnliftIO.mask
  {-# INLINE mask #-}
  uninterruptibleMask = UnliftIO.uninterruptibleMask
  {-# INLINE uninterruptibleMask #-}
  generalBracket ma mz m = UnliftIO.mask \restore -> do
    a <- ma
    x <- restore (m a) `UnliftIO.catch` \e -> do
      _ <- mz a (ExitCaseException e)
      throwIO e
    z <- mz a (ExitCaseSuccess x)
    pure (x, z)

-- Compatibility with @monad-control@. This is not encouraged usage
instance IOE :> es => MonadBase IO (Eff es) where
  liftBase = liftIO
  {-# INLINE liftBase #-}

instance IOE :> es => MonadBaseControl IO (Eff es) where
  type StM (Eff es) a = a
  liftBaseWith f = PrimEff \es -> f (`primRunEff` es)
  {-# INLINE liftBaseWith #-}
  restoreM = pure
  {-# INLINE restoreM #-}

primRunIOE :: Eff (IOE ': es) a -> Eff es a
primRunIOE = interpret \case
  Lift m   -> primLiftIO m
  Unlift f -> primUnliftIO \runInIO -> f (runInIO . unlift)

runIOE :: Eff '[IOE] a -> IO a
runIOE = (`primRunEff` emptyEnv) . primRunIOE
