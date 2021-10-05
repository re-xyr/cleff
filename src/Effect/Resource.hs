{-# OPTIONS_GHC -Wno-orphans #-}
module Effect.Resource where

import           Control.Monad.IO.Class                (liftIO)
import           Control.Monad.Trans.Resource          (InternalState,
                                                        MonadResource (liftResourceT),
                                                        createInternalState)
import           Control.Monad.Trans.Resource.Internal (ResourceT (..),
                                                        stateCleanupChecked)
import           Effect
import           Effect.IO
import           UnliftIO.Exception

data Resource :: Effect where
  LiftResourceT :: ResourceT IO a -> Resource m a

runResource :: forall es a. IOE :> es => Eff (Resource ': es) a -> Eff es a
runResource m = mask \restore -> do
  istate <- createInternalState
  a <- restore (interpret (h istate) m) `catch` \e -> do
    liftIO $ stateCleanupChecked (Just e) istate
    throwIO e
  liftIO $ stateCleanupChecked Nothing istate
  pure a
  where
    h :: InternalState -> Effect.Handler es Resource
    h istate = \case
      LiftResourceT (ResourceT m') -> liftIO $ m' istate

instance (Resource :> es, IOE :> es) => MonadResource (Eff es) where
  liftResourceT = send . LiftResourceT
  {-# INLINE liftResourceT #-}