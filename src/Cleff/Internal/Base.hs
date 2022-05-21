{-# LANGUAGE CPP #-}
{-# OPTIONS_HADDOCK not-home #-}
{-# OPTIONS_GHC -Wno-orphans #-}
-- |
-- Copyright: (c) 2021 Xy Ren
-- License: BSD3
-- Maintainer: xy.r@outlook.com
-- Stability: unstable
-- Portability: non-portable (GHC only)
--
-- This module contains the 'IOE' effect together with a few primitives for using it, as well as interpretation
-- combinators for 'IO'-related effects. It is not usually needed because safe functionalities are re-exported in the
-- "Cleff" module.
--
-- __This is an /internal/ module and its API may change even between minor versions.__ Therefore you should be
-- extra careful if you're to depend on this module.
module Cleff.Internal.Base
  ( -- * The 'IOE' Effect
    IOE
    -- * Primitive 'IO' functions
  , primLiftIO
  , primUnliftIO
    -- * Unwrapping 'Eff'
  , thisIsPureTrustMe
  , runIOE
  , runPure
  , runPureIO
    -- * Effect interpretation
  , HandlerIO
  , interpretIO
    -- * Combinators for interpreting higher-order effects
  , withToIO
  , fromIO
  ) where

import           Cleff.Internal
import           Cleff.Internal.Interpret
import           Cleff.Internal.Monad
import qualified Cleff.Internal.Rec          as Rec
import           Control.Monad.Base          (MonadBase (liftBase))
import           Control.Monad.Catch         (ExitCase (ExitCaseException, ExitCaseSuccess), MonadCatch, MonadMask,
                                              MonadThrow)
import qualified Control.Monad.Catch         as Catch
import           Control.Monad.Primitive     (PrimMonad (PrimState, primitive), RealWorld)
import           Control.Monad.Trans.Control (MonadBaseControl (StM, liftBaseWith, restoreM))
import           GHC.IO                      (IO (IO))
import           System.IO.Unsafe            (unsafeDupablePerformIO)
import           UnliftIO                    (MonadIO (liftIO), MonadUnliftIO (withRunInIO), throwIO)
import qualified UnliftIO

-- * The 'IOE' effect

-- | The effect capable of lifting and unlifting the 'IO' monad, allowing you to use 'MonadIO', 'MonadUnliftIO',
-- 'PrimMonad', 'MonadCatch', 'MonadThrow' and 'MonadMask' functionalities. This is the "final" effect that most
-- effects eventually are interpreted into. For example, you can do:
--
-- @
-- log :: 'IOE' :> es => 'Eff' es ()
-- log = 'liftIO' ('putStrLn' "Test logging")
-- @
--
-- It is not recommended to use this effect directly in application code, as it is too liberal and allows arbitrary IO,
-- therefore making it harder to do proper effect management. Ideally, this is only used in interpreting more
-- fine-grained effects.
--
-- === Technical details
--
-- Note that this is /not/ a real effect and cannot be interpreted in any way besides 'thisIsPureTrustMe' and
-- 'runIOE'. This is mainly for performance concern, but also that there doesn't really exist reasonable
-- interpretations other than the current one, given the underlying implementation of the 'Eff' monad.
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
primUnliftIO f = Eff \es -> f \(Eff m) -> m es
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
-- uses 'IO' but does not do anything really /impure/ (/i.e./ can be safely used 'unsafeDupablePerformIO' on), such as a
-- State effect.
thisIsPureTrustMe :: Eff (IOE : es) ~> Eff es
thisIsPureTrustMe =
#ifndef DYNAMIC_IOE
  adjust (Rec.cons $ HandlerPtr (-1))
#else
  interpret \case
    Lift m   -> primLiftIO m
    Unlift f -> primUnliftIO \runInIO -> f (runInIO . toEff)
#endif
{-# INLINE thisIsPureTrustMe #-}

-- | Unwrap an 'Eff' computation with side effects into an 'IO' computation, given that all effects other than 'IOE' are
-- interpreted.
runIOE :: Eff '[IOE] ~> IO
runIOE = runPureIO . thisIsPureTrustMe
{-# INLINE runIOE #-}

-- | Unwrap a pure 'Eff' computation into a pure value, given that all effects are interpreted.
runPure :: Eff '[] a -> a
runPure = unsafeDupablePerformIO . runPureIO
{-# INLINE runPure #-}

-- | Unwrap a pure 'Eff' computation into an 'IO' computation. You may occasionally need this.
runPureIO :: Eff '[] ~> IO
runPureIO (Eff m) = m emptyEnv
{-# INLINE runPureIO #-}

-- * Effect interpretation

-- | The type of an /'IO' effect handler/, which is a function that transforms an effect @e@ into 'IO' computations.
-- This is used for 'interpretIO'.
type HandlerIO e es = ∀ esSend. Handling esSend e es => e (Eff esSend) ~> IO

-- | Interpret an effect in terms of 'IO', by transforming an effect into 'IO' computations.
--
-- @
-- 'interpretIO' f = 'interpret' ('liftIO' '.' f)
-- @
interpretIO :: IOE :> es => HandlerIO e es -> Eff (e : es) ~> Eff es
interpretIO f = interpret (liftIO . f)
{-# INLINE interpretIO #-}

-- * Combinators for interpreting higher-order effects

-- | Temporarily gain the ability to unlift an @'Eff' esSend@ computation into 'IO'. This is analogous to
-- 'withRunInIO', and is useful in dealing with higher-order effects that involves 'IO'. For example, the @Resource@
-- effect that supports bracketing:
--
-- @
-- data Resource m a where
--   Bracket :: m a -> (a -> m ()) -> (a -> m b) -> Resource m b
-- @
--
-- can be interpreted into 'Control.Exception.bracket' actions in 'IO', by converting all effect computations into
-- 'IO' computations via 'withToIO':
--
-- @
-- runResource :: 'IOE' ':>' es => 'Eff' (Resource : es) a -> 'Eff' es a
-- runResource = 'interpret' \\case
--   Bracket alloc dealloc use -> 'withToIO' $ \\toIO ->
--     'Control.Exception.bracket' (toIO alloc) (toIO . dealloc) (toIO . use)
-- @
withToIO :: (Handling esSend e es, IOE :> es) => ((Eff esSend ~> IO) -> IO a) -> Eff es a
withToIO f = Eff \es -> f \(Eff m) -> m (updateEnv es esSend)

-- | Lift an 'IO' computation into @'Eff' esSend@. This is analogous to 'liftIO', and is only useful in dealing with
-- effect operations with the monad type in the negative position, for example 'Control.Exception.mask'ing:
--
-- @
-- data Mask :: 'Effect' where
--   Mask :: ((m '~>' m) -> m a) -> Mask m a
--                  ^ this "m" is in negative position
-- @
--
-- See how the @restore :: IO a -> IO a@ from 'Control.Exception.mask' is "wrapped" into
-- @'Eff' esSend a -> 'Eff' esSend a@:
--
-- @
-- runMask :: 'IOE' ':>' es => 'Eff' (Mask : es) a -> 'Eff' es a
-- runMask = 'interpret' \\case
--   Mask f -> 'withToIO' $ \\toIO -> 'Control.Exception.mask' $
--     \\restore -> f ('fromIO' . restore . toIO)
-- @
--
-- Here, @toIO@ from 'withToIO' takes an @'Eff' esSend@ to 'IO', where it can be passed into the @restore@ function,
-- and the returned 'IO' computation is recovered into 'Eff' with 'fromIO'.
fromIO :: (Handling esSend e es, IOE :> es) => IO ~> Eff esSend
fromIO = Eff . const
