{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Effect.Internal.Base where

import           Control.Monad.Base          (MonadBase (..))
import           Control.Monad.Catch         (ExitCase (ExitCaseException, ExitCaseSuccess),
                                              MonadCatch (..), MonadMask (..),
                                              MonadThrow (..))
import           Control.Monad.Trans.Control (MonadBaseControl (..))
import           Effect.Internal.Handler
import           Effect.Internal.Monad
import           System.IO.Unsafe            (unsafePerformIO)
import           UnliftIO

data IOE :: Effect where
#ifndef FAST_IOE
  Lift :: IO a -> IOE m a
  Unlift :: ((forall x. m x -> IO x) -> IO a) -> IOE m a
#endif

primLiftIO :: IO a -> Eff es a
primLiftIO = PrimEff . const
{-# INLINE primLiftIO #-}

primUnliftIO :: ((forall x. Eff es x -> IO x) -> IO a) -> Eff es a
primUnliftIO f = PrimEff \handlers -> f (`primRunEff` handlers)
{-# INLINE primUnliftIO #-}

-- Encouraged usage built upon @unliftio@
instance IOE :> es => MonadIO (Eff es) where
#ifdef FAST_IOE
  liftIO = primLiftIO
#else
  liftIO = send . Lift
#endif
  {-# INLINE liftIO #-}

instance IOE :> es => MonadUnliftIO (Eff es) where
#ifdef FAST_IOE
  withRunInIO f = primUnliftIO f
#else
  withRunInIO f = send $ Unlift f
#endif
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
  liftBaseWith = withRunInIO
  {-# INLINE liftBaseWith #-}
  restoreM = pure
  {-# INLINE restoreM #-}

thisIsPureTrustMe :: Eff (IOE ': es) a -> Eff es a
thisIsPureTrustMe = interpret \case
#ifndef FAST_IOE
  Lift m   -> primLiftIO m
  Unlift f -> primUnliftIO \runInIO -> f (runInIO . unlift)
#endif

runIOE :: Eff '[IOE] a -> IO a
runIOE = (`primRunEff` emptyEnv) . thisIsPureTrustMe

runPure :: Eff '[] a -> a
runPure m = unsafePerformIO $ primRunEff m emptyEnv
