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
    -- * Operations
  , get
  , put
  , state
  , gets
  , modify
    -- * Interpretations
  , runState
  , runStateLocal
  , runStateIORef
  , runStateMVar
  , runStateTVar
  , zoom
  ) where

import           Cleff
import           Cleff.Internal.Base
import           Control.Concurrent.MVar (MVar, modifyMVar, readMVar, swapMVar)
import           Control.Concurrent.STM  (TVar, atomically, readTVar, readTVarIO, writeTVar)
import           Control.Monad           (void)
import           Data.Atomics            (atomicModifyIORefCAS)
import           Data.IORef              (IORef, newIORef, readIORef, writeIORef)
import           Data.ThreadVar          (getThreadVar, newThreadVar)
import           Data.Tuple              (swap)
import           Lens.Micro              (Lens', (&), (.~), (^.))

-- * Effect

-- | An effect capable of providing a mutable state @s@ that can be read and written. This roughly corresponds to the
-- @MonadState@ typeclass and @StateT@ monad transformer in the @mtl@ library.
data State s :: Effect where
  Get :: State s m s
  Put :: s -> State s m ()
  State :: (s -> (a, s)) -> State s m a

-- * Operations

makeEffect_ ''State

-- | Read the current state.
get :: State s :> es => Eff es s

-- | Update the state with a new value.
put :: State s :> es => s -> Eff es ()

-- | Modify the state /and/ produce a value from the state via a function.
state :: State s :> es
  => (s -> (a, s)) -- ^ The function that takes the state and returns a result value together with a modified state
  -> Eff es a

-- | Apply a function to the result of 'get'.
gets :: State s :> es => (s -> t) -> Eff es t
gets = (<$> get)

-- | Modify the value of the state via a function.
modify :: State s :> es => (s -> s) -> Eff es ()
modify f = state (((), ) . f)

-- * Interpretations

handleIORef :: IOE :> es => IORef s -> Handler (State s) es
handleIORef rs = \case
  Get     -> liftIO $ readIORef rs
  Put s'  -> liftIO $ writeIORef rs s'
  State f -> liftIO $ atomicModifyIORefCAS rs (swap . f)

-- | Run the 'State' effect.
--
-- === Caveats
--
-- The 'runState' interpreter is implemented with 'Data.IORef.IORef's and there is no way to do arbitrary
-- atomic transactions. The 'state' operation is atomic though and it is implemented with 'atomicModifyIORefCAS', which
-- can be faster than @atomicModifyIORef@ in contention. For any more complicated cases of atomicity, please build your
-- own effect that uses either @MVar@s or @TVar@s based on your need.
--
-- Unlike @mtl@, in @cleff@ the state /will not revert/ when an error is thrown.
--
-- 'runState' will stop taking care of state operations done on forked threads as soon as the main thread finishes its
-- computation. Any state operation done /before main thread finishes/ is still taken into account.
runState :: s -> Eff (State s : es) a -> Eff es (a, s)
runState s m = thisIsPureTrustMe do
  rs <- liftIO $ newIORef s
  x <- reinterpret (handleIORef rs) m
  s' <- liftIO $ readIORef rs
  pure (x, s')

-- | Run a 'State' effect where each thread has its thread-local state.
--
-- This means that each thread will have an individual state that has the same initial value. Threfore, state
-- operations on one thread will not change the state for any other thread.
--
-- The returned final state is that of the current thread.
--
-- === Caveats
--
-- Like 'runState', the 'state' operation in this handler is atomic. Like 'runState', and unlike @mtl@, any errors will
-- not revert the state changes.
--
-- Be warned that if you use a thread pool, then when a thread is reused, it may read the state left from the last
-- usage, therefore losing locality. If you use a thread pool, you will want to manually reset the state after each
-- task.
--
-- @since 0.3.3.0
runStateLocal :: s -> Eff (State s : es) a -> Eff es (a, s)
runStateLocal s m = thisIsPureTrustMe do
  rs <- liftIO $ newThreadVar s
  x <- reinterpret (\e -> liftIO (getThreadVar rs) >>= \r -> handleIORef r e) m
  s' <- liftIO $ readIORef =<< getThreadVar rs
  pure (x, s')

-- | Run the 'State' effect in terms of operations on a supplied 'IORef'. The 'state' operation is atomic.
--
-- @since 0.2.1.0
runStateIORef :: IOE :> es => IORef s -> Eff (State s : es) a -> Eff es a
runStateIORef rs = interpret $ handleIORef rs

-- | Run the 'State' effect in terms of operations on a supplied 'MVar'.
--
-- @since 0.2.1.0
runStateMVar :: IOE :> es => MVar s -> Eff (State s : es) a -> Eff es a
runStateMVar rs = interpret \case
  Get     -> liftIO $ readMVar rs
  Put s'  -> liftIO $ void $ swapMVar rs s'
  State f -> liftIO $ modifyMVar rs \s -> let (x, !s') = f s in pure (s', x)

-- | Run the 'State' effect in terms of operations on a supplied 'TVar'.
--
-- @since 0.2.1.0
runStateTVar :: IOE :> es => TVar s -> Eff (State s : es) a -> Eff es a
runStateTVar rs = interpret \case
  Get -> liftIO $ readTVarIO rs
  Put s' -> liftIO $ atomically $ writeTVar rs s'
  State f -> liftIO $ atomically do
    s <- readTVar rs
    let (x, !s') = f s
    writeTVar rs s'
    pure x

-- | Run a 'State' effect in terms of a larger 'State' via a 'Lens''.
zoom :: State t :> es => Lens' t s -> Eff (State s : es) ~> Eff es
zoom field = interpret \case
  Get     -> gets (^. field)
  Put s   -> modify (& field .~ s)
  State f -> state \t -> let (a, !s) = f (t ^. field) in (a, t & field .~ s)
