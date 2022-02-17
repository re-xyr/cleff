{-# LANGUAGE Trustworthy #-}
-- |
-- Copyright: (c) 2021 Xy Ren
-- License: BSD3
-- Maintainer: xy.r@outlook.com
-- Stability: experimental
-- Portability: non-portable (GHC only)
module Cleff.Writer
  ( -- * Effect
    Writer (..)
    -- * Operations
  , tell
  , listen
  , listens
    -- * Interpretations
  , runWriter
  , runWriterBatch
  ) where

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

-- | Run a monoidal 'Writer' effect, but appends the listened output to the parent value only when the listen operation
-- finishes. This means that when you run two 'listen's on two threads, the values 'tell'ed inside will not be appended
-- to the parent value in real time, but only after the thread finishes 'listen'ing. For example, this code
--
-- @
-- 'UnliftIO.concurrently_'
--   ('listen' '$' 'tell' "1" '>>' 'tell' "2" '>>' 'tell' "3")
--   ('listen' '$' 'tell' "4" '>>' 'tell' "5" '>>' 'tell' "6")
-- @
--
-- will produce either @"123456"@ or @"456123"@ with 'runWriterBatch', but may produce these digits in any order with
-- 'runWriter'.
--
-- This version of interpreter can be faster than 'runWriter' in 'listen'-intense code. It is subject to all caveats
-- of 'runWriter'.
--
-- @since 0.2.0.0
runWriterBatch :: ∀ w es a. Monoid w => Eff (Writer w ': es) a -> Eff es (a, w)
runWriterBatch m = thisIsPureTrustMe do
  rw <- newIORef mempty
  x <- reinterpret (h rw) m
  w' <- readIORef rw
  pure (x, w')
  where
    h :: IORef w -> Handler (Writer w) (IOE ': es)
    h rw = \case
      Tell w' -> liftIO $ atomicModifyIORefCAS_ rw (<> w')
      Listen m' -> do
        rw' <- newIORef mempty
        x <- toEffWith (h rw') m'
        w' <- readIORef rw'
        liftIO $ atomicModifyIORefCAS_ rw (<> w')
        pure (x, w')
{-# INLINE runWriterBatch #-}
