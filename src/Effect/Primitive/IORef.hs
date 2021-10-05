module Effect.Primitive.IORef where

import           Data.IORef
import           Effect.Internal.Monad
import           Effect.Primitive.IO

primNewIORef :: a -> Eff es (IORef a)
primNewIORef x = primLiftIO $ newIORef x
{-# INLINE primNewIORef #-}

primReadIORef :: IORef a -> Eff es a
primReadIORef r = primLiftIO $ readIORef r
{-# INLINE primReadIORef #-}

primWriteIORef :: IORef a -> a -> Eff es ()
primWriteIORef r x = primLiftIO $ writeIORef r x
{-# INLINE primWriteIORef #-}

primModifyIORef' :: IORef a -> (a -> a) -> Eff es ()
primModifyIORef' r f = primLiftIO $ modifyIORef' r f
{-# INLINE primModifyIORef' #-}

primAtomicModifyIORef' :: IORef a -> (a -> (a, b)) -> Eff es b
primAtomicModifyIORef' r f = primLiftIO $ atomicModifyIORef' r f
{-# INLINE primAtomicModifyIORef' #-}
