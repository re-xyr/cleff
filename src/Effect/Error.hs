module Effect.Error where

import           Control.Monad.IO.Class (liftIO)
import           Data.Bool              (bool)
import           Effect
import           Effect.Internal.Base   (thisIsPureTrustMe)
import qualified UnliftIO.Exception     as Exc

data Error e :: Effect where
  ThrowError :: e -> Error e m a
  CatchError :: m a -> (e -> m a) -> Error e m a
makeEffect ''Error

catchErrorJust :: Error e :> es => (e -> Maybe b) -> Eff es a -> (b -> Eff es a) -> Eff es a
catchErrorJust f m h = m `catchError` \e -> maybe (throwError e) h $ f e

catchErrorIf :: Error e :> es => (e -> Bool) -> Eff es a -> (e -> Eff es a) -> Eff es a
catchErrorIf f m h = m `catchError` \e -> bool (throwError e) (h e) $ f e

handleError :: Error e :> es => (e -> Eff es a) -> Eff es a -> Eff es a
handleError = flip catchError

handleErrorJust :: Error e :> es => (e -> Maybe b) -> (b -> Eff es a) -> Eff es a -> Eff es a
handleErrorJust = flip . catchErrorJust

handleErrorIf :: Error e :> es => (e -> Bool) -> (e -> Eff es a) -> Eff es a -> Eff es a
handleErrorIf = flip . catchErrorIf

tryError :: Error e :> es => Eff es a -> Eff es (Either e a)
tryError m = (Right <$> m) `catchError` (pure . Left)

tryErrorJust :: Error e :> es => (e -> Maybe b) -> Eff es a -> Eff es (Either b a)
tryErrorJust f m = (Right <$> m) `catchError` \e -> maybe (throwError e) (pure . Left) $ f e

newtype ErrorExc e = ErrorExc { getExc :: e }
  deriving (Show, Exc.Exception)

runError :: forall e es a. Exc.Exception e => Eff (Error e ': es) a -> Eff es (Either e a)
runError = thisIsPureTrustMe . Exc.try . reinterpret \case
  ThrowError e     -> Exc.throwIO $ ErrorExc e
  CatchError m' h' -> liftIO $ Exc.catch (runInIO m') (runInIO . h' . getExc)
{-# INLINE runError #-}
