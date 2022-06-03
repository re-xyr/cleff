{-# LANGUAGE CPP           #-}
{-# LANGUAGE UnboxedTuples #-}
{-# OPTIONS_HADDOCK not-home #-}
-- |
-- Copyright: (c) 2021 Xy Ren
-- License: BSD3
-- Maintainer: xy.r@outlook.com
-- Stability: unstable
-- Portability: non-portable (GHC only)
--
-- This module contains a contention-free thread-local variable datatype.
--
-- __This is an /internal/ module and its API may change even between minor versions.__ Therefore you should be
-- extra careful if you're to depend on this module.
module Data.ThreadVar (ThreadVar, newThreadVar, getThreadVar) where

import           Control.Concurrent     (myThreadId)
import           Control.Monad.IO.Class (MonadIO (liftIO))
import           Data.Atomics           (atomicModifyIORefCAS_)
import           Data.IntMap.Strict     (IntMap)
import qualified Data.IntMap.Strict     as Map
import           Data.IORef             (IORef, newIORef, readIORef)
import           Foreign.C.Types
import           GHC.Base               (noinline)
import           GHC.Conc               (ThreadId (ThreadId))
import           GHC.Exts               (ThreadId#, mkWeak#)
import           GHC.IO                 (IO (IO))

-- | Get the hash for a 'ThreadId' in terms of C types (RTS function).
#if __GLASGOW_HASKELL__ >= 903
foreign import ccall unsafe "rts_getThreadId"
  getThreadId :: ThreadId# -> CULLong
#elif __GLASGOW_HASKELL__ >= 900
foreign import ccall unsafe "rts_getThreadId"
  getThreadId :: ThreadId# -> CLong
#else
foreign import ccall unsafe "rts_getThreadId"
  getThreadId :: ThreadId# -> CInt
#endif

-- | Generates a numeric hash for a 'ThreadId'. Before GHC 9.4, this function has a practical possibility of hash
-- collision on 32-bit or Windows platforms, if threads are created rapidly and thread count exceeds 2^32. After GHC
-- 9.4, this function practically won't produce collision as the hash is extended to 64-bit on all platforms.
hashThreadId :: ThreadId -> Int
hashThreadId (ThreadId tid#) = fromIntegral (getThreadId tid#)

-- | Attach a finalizer (an 'IO' computation) to a thread.
attachFinalizer :: ThreadId -> IO () -> IO ()
attachFinalizer (ThreadId tid#) (IO finalize#) = IO \s1 -> let
  !(# s2, _ #) = mkWeak# tid# () finalize# s1
  in (# s2, () #)

-- | A thread-local variable. It is designed so that any operation originating from existing threads produce no
-- contention; thread contention only occurs when multiple new threads attempt to first-time access the variable
-- at the same time.
data ThreadVar a = ThreadVar a {-# UNPACK #-} !(IORef (IntMap (IORef a)))

-- | Create a thread variable with a same initial value for each thread.
newThreadVar :: a -> IO (ThreadVar a)
newThreadVar x = ThreadVar x <$> newIORef Map.empty

-- | Get the variable local to this thread, in the form of an 'IORef'. It is guaranteed that the returned 'IORef'
-- will not be read or mutated by other threads inadvertently.
getThreadVar :: ThreadVar a -> IO (IORef a)
getThreadVar (ThreadVar x0 table) = do
  tid <- myThreadId
  let thash = hashThreadId tid
  maybeRef <- Map.lookup thash <$> readIORef table
  case maybeRef of
    Nothing -> do
      ref <- newIORef x0
      liftIO $ noinline atomicModifyIORefCAS_ table (Map.insert thash ref)
      liftIO $ attachFinalizer tid $
        noinline atomicModifyIORefCAS_ table (Map.delete thash)
      pure ref
    Just ref -> pure ref
{-# INLINE getThreadVar #-}
