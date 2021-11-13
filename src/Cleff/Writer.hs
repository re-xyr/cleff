module Cleff.Writer where

import           Cleff
import           Cleff.Internal.Base (thisIsPureTrustMe)
import           Data.Foldable       (traverse_)
import           UnliftIO.IORef
import           UnliftIO.MVar
import           UnliftIO.STM

-- * Effect

-- | An effect capable of accumulating outputs. This roughly corresponds to the @MonadWriter@ typeclass and @WriterT@
-- monad transformer in the @mtl@ approach. Specifically, the @pass@ operation was omitted because it does not work
-- well with concurrency.
data Writer w :: Effect where
  Tell :: w -> Writer w m ()
  Listen :: m a -> Writer w m (a, w)

-- * Operations

makeEffect ''Writer

-- | Apply a function to the accumulated output of 'listen'.
listens :: Writer w :> es => (w -> x) -> Eff es a -> Eff es (a, x)
listens f m = do
  (a, w) <- listen m
  pure (a, f w)

-- * Interpretations

-- | Run a monoidal 'Writer' effect in terms of 'IORef'. This may not be what you want if you need to avoid deadlock
-- in a multithreaded setting,
runWriter :: forall w es a. Monoid w => Eff (Writer w ': es) a -> Eff es (a, w)
runWriter m = thisIsPureTrustMe do
  rw <- newIORef mempty
  x <- reinterpret (h [rw]) m
  w' <- readIORef rw
  pure (x, w')
  where
    h :: [IORef w] -> Handler (Writer w) '[IOE] es
    h rws = \case
      Tell w' -> traverse_ (\rw -> modifyIORef' rw (<> w')) rws
      Listen (m' :: Eff es'' a') -> do
        rw' <- newIORef mempty
        x <- reinterpret (h $ rw' : rws) $ runHere m'
        w' <- readIORef rw'
        pure (x, w')
{-# INLINE runWriter #-}

-- runState mempty . runTopLevel
--   where
--     runTopLevel :: Eff (Writer w ': es') a' -> Eff (State w ': es') a'
--     runTopLevel = reinterpret \case
--       Tell w   -> modify (<> w)
--       Listen m -> runTopLevel $ runState mempty $ runListen $ runHere m
--     runListen :: Writer w :> es' => Eff (Writer w ': es') a' -> Eff (State w ': es') a'
--     runListen = reinterpret \case
--       Tell w   -> modify (<> w) *> tell w
--       Listen m ->
--         runListen $ -- Interpret the upper Writer effect.
--         runState mempty $ -- run the accumulator...
--         runListen $ -- We can interpret this with a local accumulator and an upper Writer effect...
--         runHere m

-- f :: Writer String :> es => Int -> Eff es [String]
-- f 0 = tell "0" >> pure []
-- f n = do
--   tell (show n) >> uncurry (flip (:)) <$> listen (f $ n - 1)

-- >>> runPure $ runWriter @String $ f 10
-- (["9876543210","876543210","76543210","6543210","543210","43210","3210","210","10","0"],"109876543210")

-- | Run a monoidal 'Writer' effect in terms of 'IORef', using 'atomicModifyIORef'' for accumulating output.
runAtomicWriter :: forall w es a. Monoid w => Eff (Writer w ': es) a -> Eff es (a, w)
runAtomicWriter m = thisIsPureTrustMe do
  rw <- newIORef mempty
  x <- reinterpret (h [rw]) m
  w' <- readIORef rw
  pure (x, w')
  where
    h :: [IORef w] -> Handler (Writer w) '[IOE] es
    h rws = \case
      Tell w' -> traverse_ (\rw -> atomicModifyIORef' rw ((, ()) . (<> w'))) rws
      Listen m' -> do
        rw' <- newIORef mempty
        x <- reinterpret (h $ rw' : rws) $ runHere m'
        w' <- readIORef rw'
        pure (x, w')
{-# INLINE runAtomicWriter #-}

-- | Run a monoidal 'Writer' effect in terms of 'MVar'.
runMVarWriter :: forall w es a. Monoid w => Eff (Writer w ': es) a -> Eff es (a, w)
runMVarWriter m = thisIsPureTrustMe do
  rw <- newMVar mempty
  x <- reinterpret (h [rw]) m
  w' <- readMVar rw
  pure (x, w')
  where
    h :: [MVar w] -> Handler (Writer w) '[IOE] es
    h rws = \case
      Tell w' -> traverse_ (\rw -> modifyMVar_ rw \w'' -> pure $! (w'' <> w')) rws
      Listen m' -> do
        rw' <- newMVar mempty
        x <- reinterpret (h $ rw' : rws) $ runHere m'
        w' <- readMVar rw'
        pure (x, w')
{-# INLINE runMVarWriter #-}

-- | Run a monoidal 'Writer' effect in terms of 'TVar'. This interpretation imposes an 'IOE' effect constraint in order
-- to avoid running atomic transactions within transactions.
runTVarWriter :: forall w es a. (Monoid w, IOE :> es) => Eff (Writer w ': es) a -> Eff es (a, w)
runTVarWriter m = do
  rw <- newTVarIO mempty
  x <- interpret (h [rw]) m
  w' <- readTVarIO rw
  pure (x, w')
  where
    h :: [TVar w] -> Interpreter (Writer w) es
    h rws = \case
      Tell w' -> atomically $ traverse_ (\rw -> modifyTVar' rw (<> w')) rws
      Listen m' -> do
        rw' <- newTVarIO mempty
        x <- interpret (h $ rw' : rws) $ runHere m'
        w' <- readTVarIO rw'
        pure (x, w')
{-# INLINE runTVarWriter #-}
