module Effect.Primitive.STM where

import           Control.Concurrent.STM
import           Effect
import           Effect.Primitive.IO    (primLiftIO)

primAtomically :: STM a -> Eff es a
primAtomically m = primLiftIO $ atomically m

primReadTVarIO :: TVar a -> Eff es a
primReadTVarIO r = primLiftIO $ readTVarIO r

primNewTVarIO :: a -> Eff es (TVar a)
primNewTVarIO x = primLiftIO $ newTVarIO x
