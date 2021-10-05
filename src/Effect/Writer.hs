module Effect.Writer where

import           Control.Concurrent.MVar     (MVar)
import           Control.Concurrent.STM.TVar
import           Data.Foldable               (traverse_)
import           Data.Typeable               (Typeable)
import           Effect
import           Effect.IO                   (IOE)
import           Effect.Primitive.IORef
import           Effect.Primitive.MVar
import           Effect.Primitive.STM
import           UnliftIO.IORef

data Writer w :: Effect where
  Tell :: w -> Writer w m ()
  Listen :: m a -> Writer w m (a, w)

tell :: Writer w :> es => w -> Eff es ()
tell w = send $ Tell w
{-# INLINE tell #-}

listen :: Writer w :> es => Eff es a -> Eff es (a, w)
listen m = send $ Listen m
{-# INLINE listen #-}

listens :: Writer w :> es => (w -> x) -> Eff es a -> Eff es (a, x)
listens f m = do
  (a, w) <- listen m
  pure (a, f w)

runLocalWriter :: forall w es a. (Typeable w, Monoid w) => Eff (Writer w ': es) a -> Eff es (a, w)
runLocalWriter m = do
  rw <- primNewIORef mempty
  x <- interpret (h [rw]) m
  w' <- primReadIORef rw
  pure (x, w')
  where
    h :: forall es'. [IORef w] -> Handler es' (Writer w)
    h rws = \case
      Tell w' -> traverse_ (\rw -> primModifyIORef' rw (<> w')) rws
      Listen m' -> do
        rw' <- primNewIORef mempty
        x <- unlift $ interpose (h $ rw' : rws) m'
        w' <- primReadIORef rw'
        pure (x, w')

runAtomicLocalWriter :: forall w es a. (Typeable w, Monoid w) => Eff (Writer w ': es) a -> Eff es (a, w)
runAtomicLocalWriter m = do
  rw <- primNewIORef mempty
  x <- interpret (h [rw]) m
  w' <- primReadIORef rw
  pure (x, w')
  where
    h :: forall es'. [IORef w] -> Handler es' (Writer w)
    h rws = \case
      Tell w' -> traverse_ (\rw -> primAtomicModifyIORef' rw ((, ()) . (<> w'))) rws
      Listen m' -> do
        rw' <- primNewIORef mempty
        x <- unlift $ interpose (h $ rw' : rws) m'
        w' <- primReadIORef rw'
        pure (x, w')

runSharedWriter :: forall w es a. (Typeable w, Monoid w) => Eff (Writer w ': es) a -> Eff es (a, w)
runSharedWriter m = do
  rw <- primNewMVar mempty
  x <- interpret (h [rw]) m
  w' <- primReadMVar rw
  pure (x, w')
  where
    h :: forall es'. [MVar w] -> Handler es' (Writer w)
    h rws = \case
      Tell w' -> traverse_ (\rw -> primModifyMVarPure'_ rw (<> w')) rws
      Listen m' -> do
        rw' <- primNewMVar mempty
        x <- unlift $ interpose (h $ rw' : rws) m'
        w' <- primReadMVar rw'
        pure (x, w')

runAtomicSharedWriter :: forall w es a. (Typeable w, Monoid w, IOE :> es) => Eff (Writer w ': es) a -> Eff es (a, w)
runAtomicSharedWriter m = do
  rw <- primNewTVarIO mempty
  x <- interpret (h [rw]) m
  w' <- primReadTVarIO rw
  pure (x, w')
  where
    h :: forall es'. [TVar w] -> Handler es' (Writer w)
    h rws = \case
      Tell w' -> primAtomically $ traverse_ (\rw -> modifyTVar rw (<> w')) rws
      Listen m' -> do
        rw' <- primNewTVarIO mempty
        x <- unlift $ interpose (h $ rw' : rws) m'
        w' <- primReadTVarIO rw'
        pure (x, w')
