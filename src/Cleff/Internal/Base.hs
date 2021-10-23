-- | This module contains the 'IOE' effect together with a few primitives for using it. It is not usually needed as
-- safe functionalities are re-exported in the "Cleff" module.
{-# LANGUAGE CPP #-}
{-# OPTIONS_HADDOCK not-home #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Cleff.Internal.Base where

import           Cleff.Internal.Effect
import           Cleff.Internal.Interpret
import           Cleff.Internal.Monad
import           Control.Monad.Base          (MonadBase (..))
import           Control.Monad.Catch         (ExitCase (ExitCaseException, ExitCaseSuccess), MonadCatch (..),
                                              MonadMask (..), MonadThrow (..))
import           Control.Monad.Primitive     (PrimMonad (..), RealWorld)
import           Control.Monad.Trans.Control (MonadBaseControl (..))
import           GHC.IO                      (IO (IO))
import           System.IO.Unsafe            (unsafeDupablePerformIO)
import           UnliftIO

-- * The 'IOE' effect

-- | The effect for lifting and unlifting the 'IO' monad, allowing you to use 'MonadIO', 'MonadUnliftIO', 'PrimMonad',
-- 'MonadCatch', 'MonadThrow' and 'MonadMask' functionalities. This is the "final" effect that most effects eventually
-- are interpreted into. For example, you can do:
--
-- @
-- log :: 'IOE' :> es => 'Eff' es ()
-- log = 'liftIO' ('putStrLn' "Test logging")
-- @
--
-- It is not recommended to use this effect in application code, as it is too liberal and allows arbitrary IO. Ideally,
-- this is only used in interpreting more fine-grained effects.
--
-- Note that this is /not/ a real effect and cannot be interpreted in any way besides 'thisIsPureTrustMe' and
-- 'runIOE'. It is similar to Polysemy's @Final@ effect which also cannot be interpreted. This is mainly for
-- performance concern, but also that there doesn't really exist reasonable interpretations other than the current one,
-- given the underlying implementation of the 'Eff' monad.
--
-- 'IOE' can be a real effect though, and you can enable the @dynamic-ioe@ build flag to have that. However it is only
-- for reference purposes and should not be used in production code.
data IOE :: Effect where
#ifdef DYNAMIC_IOE
  Lift :: IO a -> IOE m a
  Unlift :: ((m ~> IO) -> IO a) -> IOE m a
#endif

-- * Primitive IO functions

-- | Lift an 'IO' action into 'Eff'. This function is /highly unsafe/ and should not be used directly; most of the
-- times you should use 'liftIO' that wraps this function in a safer type.
primLiftIO :: IO a -> Eff es a
primLiftIO = PrimEff . const
{-# INLINE primLiftIO #-}

-- | Give a runner function a way to run 'Eff' actions as an 'IO' action. This function is /highly unsafe/ and should
-- not be used directly; most of the times you should use 'runInIO' that wraps this function in a safer type.
primUnliftIO :: ((Eff es ~> IO) -> IO a) -> Eff es a
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

-- Compatibility with @exceptions@.
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

-- Compatibility with @primitive@.
instance IOE :> es => PrimMonad (Eff es) where
  type PrimState (Eff es) = RealWorld
  primitive = liftIO . IO

-- * Unwrapping 'Eff'

-- | Eliminate an 'IOE' effect from the stack. This is mainly for implementing effects that don't really interact with
-- the outside world (i.e. can be safely used 'unsafeDupablePerformIO' on), such as a State effect using
-- 'Data.IORef.IORef'.
--
-- This function is /unsafe/ and you should be careful not to use it for interpreting effects that do affect the real
-- world.
thisIsPureTrustMe :: Eff (IOE ': es) ~> Eff es
thisIsPureTrustMe = interpret \case
#ifdef DYNAMIC_IOE
  Lift m   -> primLiftIO m
  Unlift f -> primLiftIO $ f runInIO
#endif
{-# INLINE thisIsPureTrustMe #-}

-- | Unwrap the 'Eff' monad into an 'IO' action, given that all other effects are interpreted, and only 'IOE' remains
-- on the effect stack.
runIOE :: Eff '[IOE] ~> IO
runIOE = (`primRunEff` emptyEnv) . thisIsPureTrustMe
{-# INLINE runIOE #-}

-- | Unwrap the 'Eff' monad into a pure value, given that all effects are interpreted and no 'IOE' stays on the effect
-- stack.
runPure :: Eff '[] a -> a
runPure = unsafeDupablePerformIO . (`primRunEff` emptyEnv)
{-# NOINLINE runPure #-}

-- * Effect interpretation

-- | An effect handler that translates effect @e@ from arbitrary effect stacks into 'IO' actions.
type InterpreterIO es e = forall esSend. (e :> esSend, Handling esSend es e) => e (Eff esSend) ~> IO

-- | Interpret an effect in terms of 'IO'.
--
-- @
-- 'interpretIO' f = 'interpret' ('liftIO' '.' f)
-- @
interpretIO :: IOE :> es => InterpreterIO es e -> Eff (e ': es) ~> Eff es
interpretIO f = interpret (liftIO . f)
{-# INLINE interpretIO #-}
