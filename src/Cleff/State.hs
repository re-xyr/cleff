{-# LANGUAGE Trustworthy #-}
-- |
-- Copyright: (c) 2021 Xy Ren
-- License: BSD3
-- Maintainer: xy.r@outlook.com
-- Stability: experimental
-- Portability: non-portable (GHC only)
module Cleff.State
  ( -- * Effect
    State (..)
  , -- * Operations
    get, put, state, gets, modify
  , -- * Interpretations
    runState, runStateIORef, runStateMVar, runStateTVar, zoom
  ) where

import           Cleff
import           Cleff.Internal.Base
import           Control.Monad       (void)
import           Data.Atomics        (atomicModifyIORefCAS)
import           Data.Tuple          (swap)
import           Lens.Micro          (Lens', (&), (.~), (^.))
import           UnliftIO.IORef      (IORef, newIORef, readIORef, writeIORef)
import           UnliftIO.MVar       (MVar, modifyMVar, readMVar, swapMVar)
import           UnliftIO.STM        (TVar, atomically, readTVar, readTVarIO, writeTVar)

-- * Effect

-- | An effect capable of providing a mutable state @s@ that can be read and written. This roughly corresponds to the
-- @MonadState@ typeclass and @StateT@ monad transformer in the @mtl@ approach.
data State s :: Effect where
  Get :: State s m s
  Put :: s -> State s m ()
  State :: (s -> (a, s)) -> State s m a

-- * Operations

makeEffect ''State

-- | Apply a function to the result of 'get'.
gets :: State s :> es => (s -> t) -> Eff es t
gets = (<$> get)

-- | Modify the value of the state via a function.
modify :: State s :> es => (s -> s) -> Eff es ()
modify f = state (((), ) . f)

-- * Interpretations

handleIORef :: IOE :> es => IORef s -> Handler (State s) es
handleIORef rs = \case
  Get     -> readIORef rs
  Put s'  -> writeIORef rs s'
  State f -> liftIO $ atomicModifyIORefCAS rs (swap . f)
{-# INLINE handleIORef #-}

-- | Run the 'State' effect.
--
-- __Caveat__: The 'runState' interpreter is implemented with 'Data.IORef.IORef's and there is no way to do arbitrary
-- atomic transactions. The 'state' operation is atomic though and it is implemented with 'atomicModifyIORefCAS', which
-- can be faster than @atomicModifyIORef@ in contention. For any more complicated cases of atomicity, please build your
-- own effect that uses either @MVar@s or @TVar@s based on your need.
--
-- Unlike @mtl@, in @cleff@ the state /will not revert/ when an error is thrown.
--
-- 'runState' will stop taking care of state operations done on forked threads as soon as the main thread finishes its
-- computation. Any state operation done /before main thread finishes/ is still taken into account.
runState :: s -> Eff (State s ': es) a -> Eff es (a, s)
runState s m = thisIsPureTrustMe do
  rs <- newIORef s
  x <- reinterpret (handleIORef rs) m
  s' <- readIORef rs
  pure (x, s')
{-# INLINE runState #-}

-- | Run the 'State' effect in terms of operations on a supplied 'IORef'. The 'state' operation is atomic.
--
-- @since 0.2.1.0
runStateIORef :: IOE :> es => IORef s -> Eff (State s ': es) a -> Eff es a
runStateIORef rs = interpret $ handleIORef rs
{-# INLINE runStateIORef #-}

-- | Run the 'State' effect in terms of operations on a supplied 'MVar'.
--
-- @since 0.2.1.0
runStateMVar :: IOE :> es => MVar s -> Eff (State s ': es) a -> Eff es a
runStateMVar rs = interpret \case
  Get     -> readMVar rs
  Put s'  -> void $ swapMVar rs s'
  State f -> modifyMVar rs \s -> let (x, !s') = f s in pure (s', x)
{-# INLINE runStateMVar #-}

-- | Run the 'State' effect in terms of operations on a supplied 'TVar'.
--
-- @since 0.2.1.0
runStateTVar :: IOE :> es => TVar s -> Eff (State s ': es) a -> Eff es a
runStateTVar rs = interpret \case
  Get -> readTVarIO rs
  Put s' -> atomically $ writeTVar rs s'
  State f -> atomically do
    s <- readTVar rs
    let (x, !s') = f s
    writeTVar rs s'
    pure x
{-# INLINE runStateTVar #-}

-- | Run a 'State' effect in terms of a larger 'State' via a 'Lens''.
zoom :: State t :> es => Lens' t s -> Eff (State s ': es) ~> Eff es
zoom field = interpret \case
  Get     -> gets (^. field)
  Put s   -> modify (& field .~ s)
  State f -> state \t -> let (a, !s) = f (t ^. field) in (a, t & field .~ s)
{-# INLINE zoom #-}
