-- | This module contains the 'IOE' effect together with a few primitives for using it. It is not usually needed as
-- safe functionalities are re-exported in the "Effect" module.
{-# LANGUAGE CPP #-}
{-# OPTIONS_HADDOCK not-home #-}
{-# OPTIONS_GHC -Wno-orphans -Wno-dodgy-exports #-}
module Effect.Internal.Base
  ( -- * The @IOE@ effect
    IOE (..)
  , -- * Unwrapping actions
    runIOE, runPure, thisIsPureTrustMe
  , -- * Primitive IO functions
    primLiftIO, primUnliftIO
  ) where

import           Control.Monad.Base          (MonadBase (..))
import           Control.Monad.Catch         (ExitCase (ExitCaseException, ExitCaseSuccess),
                                              MonadCatch (..), MonadMask (..),
                                              MonadThrow (..))
import           Control.Monad.Trans.Control (MonadBaseControl (..))
import           Effect.Internal.Handler
import           Effect.Internal.Monad
import           System.IO.Unsafe            (unsafeDupablePerformIO)
import           UnliftIO

-- | The effect for lifting and unlifting the 'IO' monad, allowing you to use 'MonadIO' and 'MonadUnliftIO'
-- functionalities. This is the "final" effect that most effects eventually are interpreted into.
--
-- However, note that this is /not/ a real effect and cannot be interpreted in any way besides 'thisIsPureTrustMe' and
-- 'runIOE'. It is similar to Polysemy's @Final@ effect which also cannot be interpreted. This is mainly for
-- performance concern, but also that there doesn't really exist reasonable interpretations other than the current one,
-- given the underlying implementation of the 'Eff' monad.
--
-- 'IOE' can be a real effect though, and you can enable the @dynamic-ioe@ build flag to have that. However it is only
-- for reference purposes and should not be used in production code.
data IOE :: Effect where
#ifdef DYNAMIC_IOE
  Lift :: IO a -> IOE m a
  Unlift :: ((forall x. m x -> IO x) -> IO a) -> IOE m a
#endif

-- | Lift an 'IO' action into 'Eff'. This function is /highly unsafe/ and should not be used directly; most of the
-- times you should use 'liftIO' that wraps this function in a safer type.
primLiftIO :: IO a -> Eff es a
primLiftIO = PrimEff . const
{-# INLINE primLiftIO #-}

-- | Give a runner function a way to run 'Eff' actions as an 'IO' action. This function is /highly unsafe/ and should
-- not be used directly; most of the times you should use 'runInIO' that wraps this function in a safer type.
primUnliftIO :: ((forall x. Eff es x -> IO x) -> IO a) -> Eff es a
primUnliftIO f = PrimEff \handlers -> f (`primRunEff` handlers)
{-# INLINE primUnliftIO #-}

-- Encouraged usage built upon @unliftio@
instance IOE :> es => MonadIO (Eff es) where
#ifdef DYNAMIC_IOE
  liftIO = send . Lift
#else
  liftIO = primLiftIO
  {-# INLINE liftIO #-}
#endif

instance IOE :> es => MonadUnliftIO (Eff es) where
#ifdef DYNAMIC_IOE
  withRunInIO f = send $ Unlift f
#else
  withRunInIO = primUnliftIO
  {-# INLINE withRunInIO #-}
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

-- | Eliminate an 'IOE' effect from the stack. This is mainly for implementing effects that don't really interact with
-- the outside world (i.e. can be safely used 'unsafeDupablePerformIO' on), such as a State effect using
-- 'Data.IORef.IORef'.
--
-- This function is /unsafe/ and you should be careful not to use it for interpreting effects that do affect the real
-- world.
thisIsPureTrustMe :: Eff (IOE ': es) a -> Eff es a
thisIsPureTrustMe = interpret \case
#ifdef DYNAMIC_IOE
  Lift m   -> primLiftIO m
  Unlift f -> primLiftIO $ f runInIO
#endif
{-# INLINE thisIsPureTrustMe #-}

-- | Unwrap the 'Eff' monad into an 'IO' action, given that all other effects are interpreted, and only 'IOE' remains
-- on the effect stack.
runIOE :: Eff '[IOE] a -> IO a
runIOE = (`primRunEff` emptyEnv) . thisIsPureTrustMe
{-# INLINE runIOE #-}

-- | Unwrap the 'Eff' monad to a pure action, given that all effects are interpreted and no 'IOE' stays on the effect
-- stack.
runPure :: Eff '[] a -> a
runPure = unsafeDupablePerformIO . (`primRunEff` emptyEnv)
{-# NOINLINE runPure #-}
