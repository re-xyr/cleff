{-# LANGUAGE CPP #-}
{-# OPTIONS_HADDOCK not-home #-}
{-# OPTIONS_GHC -Wno-orphans #-}
-- | This module contains the 'IOE' effect together with a few primitives for using it, as well as interpretation
-- combinators for 'IO'-related effects. It is not usually needed because safe functionalities are re-exported in the
-- "Cleff" module.
--
-- __This is an /internal/ module and its API may change even between minor versions.__ Therefore you should be
-- extra careful if you're to depend on this module.
module Cleff.Internal.Base
  ( -- * The 'IOE' Effect
    IOE
  , -- * Primitive 'IO' functions
    primLiftIO, primUnliftIO
  , -- * Unwrapping 'Eff'
    thisIsPureTrustMe, runIOE, runPure
  , -- * Effect interpretation
    HandlerIO, interpretIO
  , -- * Combinators for interpreting higher-order effects
    withToIO, fromIO
  ) where

import           Cleff.Internal.Effect
import           Cleff.Internal.Interpret
import           Cleff.Internal.Monad
import           Control.Monad.Base          (MonadBase (liftBase))
import           Control.Monad.Catch         (ExitCase (ExitCaseException, ExitCaseSuccess), MonadCatch, MonadMask,
                                              MonadThrow)
import qualified Control.Monad.Catch         as Catch
import           Control.Monad.Primitive     (PrimMonad (PrimState, primitive), RealWorld)
import           Control.Monad.Trans.Control (MonadBaseControl (StM, liftBaseWith, restoreM))
import qualified Data.Mem                    as Mem
import           GHC.IO                      (IO (IO))
import           System.IO.Unsafe            (unsafeDupablePerformIO)
import           UnliftIO                    (MonadIO (liftIO), MonadUnliftIO (withRunInIO), throwIO)
import qualified UnliftIO

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

-- * Primitive 'IO' functions

-- | Lift an 'IO' computation into 'Eff'. This function is /highly unsafe/ and should not be used directly; use 'liftIO'
-- instead, or if you're interpreting higher-order effects, use 'fromIO'.
primLiftIO :: IO a -> Eff es a
primLiftIO = Eff . const
{-# INLINE primLiftIO #-}

-- | Give a runner function a way to run 'Eff' actions as an 'IO' computation. This function is /highly unsafe/ and
-- should not be used directly; use 'withRunInIO' instead, or if you're interpreting higher-order effects, use
-- 'withToIO'.
primUnliftIO :: ((Eff es ~> IO) -> IO a) -> Eff es a
primUnliftIO f = Eff \es -> f (`unEff` es)
{-# INLINE primUnliftIO #-}

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

-- | Compatibility instance; use 'MonadIO' if possible.
instance IOE :> es => MonadBase IO (Eff es) where
  liftBase = liftIO

-- | Compatibility instance; use 'MonadUnliftIO' if possible.
instance IOE :> es => MonadBaseControl IO (Eff es) where
  type StM (Eff es) a = a
  liftBaseWith = withRunInIO
  restoreM = pure

instance IOE :> es => PrimMonad (Eff es) where
  type PrimState (Eff es) = RealWorld
  primitive = liftIO . IO

-- * Unwrapping 'Eff'

-- | Unsafely eliminate an 'IOE' effect from the top of the effect stack. This is mainly for implementing effects that
-- uses 'IO' but does not do anything really /impure/ (i.e. can be safely used 'unsafeDupablePerformIO' on), such as a
-- State effect.
thisIsPureTrustMe :: Eff (IOE ': es) ~> Eff es
thisIsPureTrustMe = interpret \case
#ifdef DYNAMIC_IOE
  Lift m   -> primLiftIO m
  Unlift f -> primUnliftIO \runInIO -> f (runInIO . toEff)
#endif
{-# INLINE thisIsPureTrustMe #-}

-- | Extract the 'IO' computation out of an 'Eff' given no effect remains on the stack.
runEff :: Eff '[] a -> IO a
runEff m = unEff m Mem.empty
{-# INLINE runEff #-}

-- | Unwrap an 'Eff' computation with side effects into an 'IO' computation, given that all effects other than 'IOE' are
-- interpreted.
runIOE :: Eff '[IOE] ~> IO
runIOE = runEff . thisIsPureTrustMe
{-# INLINE runIOE #-}

-- | Unwrap a pure 'Eff' computation into a pure value, given that all effects are interpreted.
runPure :: Eff '[] a -> a
runPure = unsafeDupablePerformIO . runEff
{-# NOINLINE runPure #-}

-- * Effect interpretation

-- | The type of an /'IO' effect handler/, which is a function that transforms an effect @e@ into 'IO' computations.
-- This is used for 'interpretIO'.
type HandlerIO e es = ∀ esSend. (Handling e es esSend) => e (Eff esSend) ~> IO

-- | Interpret an effect in terms of 'IO', by transforming an effect into 'IO' computations.
--
-- @
-- 'interpretIO' f = 'interpret' ('liftIO' '.' f)
-- @
interpretIO :: IOE :> es => HandlerIO e es -> Eff (e ': es) ~> Eff es
interpretIO f = interpret (liftIO . f)
{-# INLINE interpretIO #-}

-- * Combinators for interpreting higher-order effects

-- | Temporarily gain the ability to unlift an @'Eff' esSend@ computation into 'IO'. This is useful for dealing with
-- higher-order effects that involves 'IO'.
withToIO :: (Handling e es esSend, IOE :> es) => ((Eff esSend ~> IO) -> IO a) -> Eff es a
withToIO f = Eff \es -> f \m -> unEff m (Mem.update es sendEnv)

-- | Lift an 'IO' computation into @'Eff' esSend@. This is useful for dealing with effect operations with the monad type in
-- the negative position within 'Cleff.IOE', like 'UnliftIO.mask'ing.
fromIO :: (Handling e es esSend, IOE :> es) => IO ~> Eff esSend
fromIO = Eff . const
