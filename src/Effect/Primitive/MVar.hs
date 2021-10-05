module Effect.Primitive.MVar where

import           Control.Concurrent.MVar
import           Data.Functor            (void)
import           Effect
import           Effect.Primitive.IO

primNewMVar :: a -> Eff es (MVar a)
primNewMVar x = primLiftIO $ newMVar x
{-# INLINE primNewMVar #-}

primReadMVar :: MVar a -> Eff es a
primReadMVar r = primLiftIO $ readMVar r
{-# INLINE primReadMVar #-}

primWriteMVar :: MVar a -> a -> Eff es ()
primWriteMVar r x = void $ primLiftIO $ swapMVar r $! x
{-# INLINE primWriteMVar #-}

primModifyMVar' :: MVar a -> (a -> Eff es (a, b)) -> Eff es b
primModifyMVar' r f = primUnliftIO \runInIO -> modifyMVar r \s -> do
  (a, s') <- runInIO $ f s
  s' `seq` pure (a, s')
{-# INLINE primModifyMVar' #-}

primModifyMVar'_ :: MVar a -> (a -> Eff es a) -> Eff es ()
primModifyMVar'_ r f = primUnliftIO \runInIO -> modifyMVar_ r \s -> do
  s' <- runInIO $ f s
  pure $! s'
{-# INLINE primModifyMVar'_ #-}

primModifyMVarPure' :: MVar a -> (a -> (a, b)) -> Eff es b
primModifyMVarPure' r f = primModifyMVar' r (pure . f)
{-# INLINE primModifyMVarPure' #-}

primModifyMVarPure'_ :: MVar a -> (a -> a) -> Eff es ()
primModifyMVarPure'_ r f = primModifyMVar'_ r (pure . f)
{-# INLINE primModifyMVarPure'_ #-}
