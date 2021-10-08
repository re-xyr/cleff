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
import           System.IO.Unsafe            (unsafeDupablePerformIO)
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
  {-# INLINE liftIO #-}
#else
  liftIO = send . Lift
#endif

instance IOE :> es => MonadUnliftIO (Eff es) where
#ifdef FAST_IOE
  withRunInIO = primUnliftIO
  {-# INLINE withRunInIO #-}
#else
  withRunInIO f = send $ Unlift f
#endif

-- Compatibility with @exceptions@. This is not encouraged usage
instance IOE :> es => MonadThrow (Eff es) where
  throwM = throwIO

instance IOE :> es => MonadCatch (Eff es) where
  catch = UnliftIO.catch

instance IOE :> es => MonadMask (Eff es) where
  mask = UnliftIO.mask
  uninterruptibleMask = UnliftIO.uninterruptibleMask
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

instance IOE :> es => MonadBaseControl IO (Eff es) where
  type StM (Eff es) a = a
  liftBaseWith = withRunInIO
  restoreM = pure

thisIsPureTrustMe :: Eff (IOE ': es) a -> Eff es a
thisIsPureTrustMe = interpret \case
#ifndef FAST_IOE
  Lift m   -> primLiftIO m
  Unlift f -> primLiftIO $ f runInIO
#endif
{-# INLINE thisIsPureTrustMe #-}

runIOE :: Eff '[IOE] a -> IO a
runIOE = (`primRunEff` emptyEnv) . thisIsPureTrustMe
{-# INLINE runIOE #-}

runPure :: Eff '[] a -> a
runPure = unsafeDupablePerformIO . (`primRunEff` emptyEnv)
{-# NOINLINE runPure #-}
