module Cleff.Writer where

import           Cleff
import           Cleff.Internal.Base
import           Data.Atomics        (atomicModifyIORefCAS_)
import           Data.Foldable       (traverse_)
import           UnliftIO.IORef      (IORef, newIORef, readIORef)

-- * Effect

-- | An effect capable of accumulating outputs. This roughly corresponds to the @MonadWriter@ typeclass and @WriterT@
-- monad transformer in the @mtl@ approach.
--
-- However, note that this does not have a @pass@ operation as we are not sure what its semantics should be. In fact,
-- the @pass@ semantics in @mtl@ is also unclear and will change when handlers are put in different orders. To avoid
-- any confusion we decided it is best that we don't include it because no one seems to be relying on it anyway.
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

-- | Run a monoidal 'Writer' effect.
--
-- __Caveat__: Both 'runWriter' and 'listen's under 'runWriter' will stop taking care of writer operations done on
-- forked threads as soon as the main thread finishes its computation. Any writer operation done
-- /before main thread finishes/ is still taken into account.
runWriter :: ∀ w es a. Monoid w => Eff (Writer w ': es) a -> Eff es (a, w)
runWriter m = thisIsPureTrustMe do
  rw <- newIORef mempty
  x <- reinterpret (h [rw]) m
  w' <- readIORef rw
  pure (x, w')
  where
    h :: [IORef w] -> Handler (Writer w) (IOE ': es)
    h rws = \case
      Tell w' -> traverse_ (\rw -> liftIO $ atomicModifyIORefCAS_ rw (<> w')) rws
      Listen m' -> do
        rw' <- newIORef mempty
        x <- toEffWith (h $ rw' : rws) m'
        w' <- readIORef rw'
        pure (x, w')
{-# INLINE runWriter #-}

-- f :: Writer String :> es => Int -> Eff es [String]
-- f 0 = tell "0" >> pure []
-- f n = do
--   tell (show n) >> uncurry (flip (:)) <$> listen (f $ n - 1)

-- >>> runPure $ runWriter @String $ f 10
-- (["9876543210","876543210","76543210","6543210","543210","43210","3210","210","10","0"],"109876543210")
