module Effect.Error where

import           Data.Bool            (bool)
import           Effect
import           Effect.Internal.Base (thisIsPureTrustMe)
import           UnliftIO             (liftIO)
import qualified UnliftIO.Exception   as Exc

data Error e :: Effect where
  ThrowError :: e -> Error e m a
  CatchError :: m a -> (e -> m a) -> Error e m a

throwError :: Error e :> es => e -> Eff es a
throwError e = send $ ThrowError e
{-# INLINE throwError #-}

catchError :: Error e :> es => Eff es a -> (e -> Eff es a) -> Eff es a
catchError m h = send $ CatchError m h
{-# INLINE catchError #-}

catchErrorJust :: Error e :> es => (e -> Maybe b) -> Eff es a -> (b -> Eff es a) -> Eff es a
catchErrorJust f m h = m `catchError` \e -> maybe (throwError e) h $ f e
{-# INLINE catchErrorJust #-}

catchErrorIf :: Error e :> es => (e -> Bool) -> Eff es a -> (e -> Eff es a) -> Eff es a
catchErrorIf f m h = m `catchError` \e -> bool (throwError e) (h e) $ f e
{-# INLINE catchErrorIf #-}

handleError :: Error e :> es => (e -> Eff es a) -> Eff es a -> Eff es a
handleError = flip catchError
{-# INLINE handleError #-}

handleErrorJust :: Error e :> es => (e -> Maybe b) -> (b -> Eff es a) -> Eff es a -> Eff es a
handleErrorJust f = flip $ catchErrorJust f
{-# INLINE handleErrorJust #-}

handleErrorIf :: Error e :> es => (e -> Bool) -> (e -> Eff es a) -> Eff es a -> Eff es a
handleErrorIf f = flip $ catchErrorIf f
{-# INLINE handleErrorIf #-}

tryError :: Error e :> es => Eff es a -> Eff es (Either e a)
tryError m = (Right <$> m) `catchError` (pure . Left)
{-# INLINE tryError #-}

tryErrorJust :: Error e :> es => (e -> Maybe b) -> Eff es a -> Eff es (Either b a)
tryErrorJust f m = (Right <$> m) `catchError` \e -> maybe (throwError e) (pure . Left) $ f e
{-# INLINE tryErrorJust #-}

runError :: forall e es a. (Exc.Exception e) => Eff (Error e ': es) a -> Eff es (Either e a)
runError = thisIsPureTrustMe . Exc.try . reinterpret \case
  ThrowError e     -> Exc.throwIO e
  CatchError m' h' -> liftIO $ Exc.catch (unliftIO m') (unliftIO . h')
