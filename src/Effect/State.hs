module Effect.State where

import           Control.Concurrent.MVar     (MVar)
import           Control.Concurrent.STM.TVar
import           Data.Tuple                  (swap)
import           Data.Typeable               (Typeable)
import           Effect
import           Effect.IO
import           Effect.Primitive.IORef
import           Effect.Primitive.MVar
import           Effect.Primitive.STM
import           UnliftIO.IORef

data State s :: Effect where
  Get :: State s m s
  Put :: s -> State s m ()
  State :: (s -> (a, s)) -> State s m a

get :: State s :> es => Eff es s
get = send Get
{-# INLINE get #-}

gets :: State s :> es => (s -> t) -> Eff es t
gets f = f <$> get
{-# INLINE gets #-}

put :: State s :> es => s -> Eff es ()
put s = send $ Put s
{-# INLINE put #-}

state :: State s :> es => (s -> (a, s)) -> Eff es a
state f = send $ State f
{-# INLINE state #-}

modify :: State s :> es => (s -> s) -> Eff es ()
modify f = state (((), ) . f)
{-# INLINE modify #-}

runLocalState :: forall s es a. Typeable s => s -> Eff (State s ': es) a -> Eff es (a, s)
runLocalState s m = do
  rs <- primNewIORef s
  x <- interpret (h rs) m
  s' <- primReadIORef rs
  pure (x, s')
  where
    h :: IORef s -> Handler es (State s)
    h rs = \case
      Get -> primReadIORef rs
      Put s' -> primWriteIORef rs s'
      State f -> do
        s' <- primReadIORef rs
        let (a, s'') = f s'
        primWriteIORef rs s''
        pure a

runAtomicLocalState :: forall s es a. Typeable s => s -> Eff (State s ': es) a -> Eff es (a, s)
runAtomicLocalState s m = do
  rs <- primNewIORef s
  x <- interpret (h rs) m
  s' <- primReadIORef rs
  pure (x, s')
  where
    h :: IORef s -> Handler es (State s)
    h rs = \case
      Get     -> primReadIORef rs
      Put s'  -> primWriteIORef rs s'
      State f -> primAtomicModifyIORef' rs (swap . f)

runSharedState :: forall s es a. Typeable s => s -> Eff (State s ': es) a -> Eff es (a, s)
runSharedState s m = do
  rs <- primNewMVar s
  x <- interpret (h rs) m
  s' <- primReadMVar rs
  pure (x, s')
  where
    h :: MVar s -> Handler es (State s)
    h rs = \case
      Get     -> primReadMVar rs
      Put s'  -> primWriteMVar rs s'
      State f -> primModifyMVar' rs (pure . swap . f)

runAtomicSharedState :: forall s es a. IOE :> es => Typeable s => s -> Eff (State s ': es) a -> Eff es (a, s)
runAtomicSharedState s m = do
  rs <- primNewTVarIO s
  x <- interpret (h rs) m
  s' <- primReadTVarIO rs
  pure (x, s')
  where
    h :: TVar s -> Handler es (State s)
    h rs = \case
      Get     -> primReadTVarIO rs
      Put s'  -> primAtomically $ writeTVar rs s'
      State f -> primAtomically $ stateTVar rs f
