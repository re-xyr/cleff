module Cleff.State where

import           Cleff
import           Cleff.Internal.Base         (thisIsPureTrustMe)
import           Control.Concurrent.STM.TVar (stateTVar)
import           Control.Monad               (void)
import           Data.Tuple                  (swap)
import           Lens.Micro                  (Lens', (&), (.~), (^.))
import           UnliftIO.IORef
import           UnliftIO.MVar
import           UnliftIO.STM

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

-- | Run a 'State' effect in terms of 'IORef'. This may not be what you want if you need to avoid deadlock in a
-- multithreaded setting.
runState :: forall s es a. s -> Eff (State s ': es) a -> Eff es (a, s)
runState s m = thisIsPureTrustMe do
  rs <- newIORef s
  x <- reinterpret (\case
    Get -> readIORef rs
    Put s' -> writeIORef rs s'
    State f -> do
      s' <- readIORef rs
      let (a, !s'') = f s'
      writeIORef rs s''
      pure a) m
  s' <- readIORef rs
  pure (x, s')
{-# INLINE runState #-}

-- | Run a 'State' effect in terms of 'IORef', and use 'atomicModifyIORef'' for the 'state' operation.
runAtomicState :: forall s es a. s -> Eff (State s ': es) a -> Eff es (a, s)
runAtomicState s m = thisIsPureTrustMe do
  rs <- newIORef s
  x <- reinterpret (\case
    Get     -> readIORef rs
    Put s'  -> writeIORef rs s'
    State f -> atomicModifyIORef' rs (swap . f)) m
  s' <- readIORef rs
  pure (x, s')
{-# INLINE runAtomicState #-}

-- | Run a 'State' effect in terms of 'MVar'.
runMVarState :: forall s es a. s -> Eff (State s ': es) a -> Eff es (a, s)
runMVarState s m = thisIsPureTrustMe do
  rs <- newMVar s
  x <- reinterpret (\case
    Get     -> readMVar rs
    Put s'  -> void $ swapMVar rs s'
    State f -> modifyMVar rs \s' -> let (!s'', a) = f s' in pure (a, s'')) m
  s' <- readMVar rs
  pure (x, s')
{-# INLINE runMVarState #-}

-- | Run a 'State' effect in terms of 'TVar'. This interpretation imposes an 'IOE' effect constraint in order to avoid
-- running atomic transactions within transactions.
runTVarState :: forall s es a. IOE :> es => s -> Eff (State s ': es) a -> Eff es (a, s)
runTVarState s m = do
  rs <- newTVarIO s
  x <- interpret (\case
    Get     -> readTVarIO rs
    Put s'  -> atomically $ writeTVar rs s'
    State f -> atomically $ stateTVar rs f) m
  s' <- readTVarIO rs
  pure (x, s')
{-# INLINE runTVarState #-}

-- | Run a 'State' effect in terms of a larger 'State' via a 'Lens''.
zoom :: State t :> es => Lens' t s -> Eff (State s ': es) ~> Eff es
zoom field = interpret \case
  Get     -> gets (^. field)
  Put s   -> modify (& field .~ s)
  State f -> state \t -> let (a, !s) = f (t ^. field) in (a, t & field .~ s)
{-# INLINE zoom #-}
