module Cleff.State where

import           Cleff
import           Cleff.Internal.Base (thisIsPureTrustMe)
import           Data.Atomics        (atomicModifyIORefCAS)
import           Data.Tuple          (swap)
import           Lens.Micro          (Lens', (&), (.~), (^.))
import           UnliftIO.IORef

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

-- | Run the 'State' effect.
--
-- __Caveat__: The 'runState' interpreter is implemented with `IORef`s and there is no way to do arbitrary atomic
-- transactions at all. The 'state' operation is atomic though and it is implemented with 'atomicModifyIORefCAS' which
-- can be faster in contention. For any more complicated cases of atomicity please build your own effect that uses
-- either @MVar@s or @TVar@s based on your need.
--
-- Unlike @mtl@, in @cleff@ the state /will not revert/ when an error is thrown.
--
-- 'runState' will stop taking care of state operations done on forked threads as soon as the main thread finishes its
-- computation. Any state operation done /before main thread finishes/ is still taken into account.
runState :: forall s es a. s -> Eff (State s ': es) a -> Eff es (a, s)
runState s m = thisIsPureTrustMe do
  rs <- newIORef s
  x <- reinterpret (\case
    Get     -> readIORef rs
    Put s'  -> writeIORef rs s'
    State f -> liftIO $ atomicModifyIORefCAS rs (swap . f)) m
  s' <- readIORef rs
  pure (x, s')
{-# INLINE runState #-}

-- | Run a 'State' effect in terms of a larger 'State' via a 'Lens''.
zoom :: State t :> es => Lens' t s -> Eff (State s ': es) ~> Eff es
zoom field = interpret \case
  Get     -> gets (^. field)
  Put s   -> modify (& field .~ s)
  State f -> state \t -> let (a, !s) = f (t ^. field) in (a, t & field .~ s)
{-# INLINE zoom #-}
