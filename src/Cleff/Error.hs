module Cleff.Error where

import           Cleff
import           Cleff.Internal.Base    (thisIsPureTrustMe)
import           Control.Monad.IO.Class (liftIO)
import           Data.Bool              (bool)
import           Data.Either.Extra      (mapLeft)
import           UnliftIO               (withRunInIO)
import qualified UnliftIO.Exception     as Exc

data Error e :: Effect where
  ThrowError :: e -> Error e m a
  CatchError :: m a -> (e -> m a) -> Error e m a
makeEffect ''Error

fromEither :: Error e :> es => Either e a -> Eff es a
fromEither = either throwError pure

fromException :: forall e es a. (Exc.Exception e, '[Error e, IOE] :>> es) => IO a -> Eff es a
fromException m = Exc.catch (liftIO m) (throwError @e)

fromExceptionVia :: (Exc.Exception ex, '[Error er, IOE] :>> es) => (ex -> er) -> IO a -> Eff es a
fromExceptionVia f m = Exc.catch (liftIO m) (throwError . f)

fromExceptionEff :: forall e es a. (Exc.Exception e, '[Error e, IOE] :>> es) => Eff es a -> Eff es a
fromExceptionEff m = withRunInIO \unlift -> Exc.catch (unlift m) (unlift . throwError @e)

fromExceptionEffVia :: (Exc.Exception ex, '[Error er, IOE] :>> es) => (ex -> er) -> Eff es a -> Eff es a
fromExceptionEffVia f m = withRunInIO \unlift -> Exc.catch (unlift m) (unlift . throwError . f)

note :: Error e :> es => e -> Maybe a -> Eff es a
note e = maybe (throwError e) pure

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

runError :: Exc.Exception e => Eff (Error e ': es) a -> Eff es (Either e a)
runError = thisIsPureTrustMe . fmap (mapLeft getExc) . Exc.try . reinterpret \case
  ThrowError e   -> Exc.throwIO $ ErrorExc e
  CatchError m h -> liftIO $ Exc.catch (runInIO m) (runInIO . h . getExc)
{-# INLINE runError #-}

mapError :: (Exc.Exception e, Error e' :> es) => (e -> e') -> Eff (Error e ': es) a -> Eff es a
mapError f = interpret \case
  ThrowError e   -> throwError $ f e
  CatchError m h -> runError (runHere m) >>= \case
    Left e  -> runThere $ h e
    Right a -> pure a
{-# INLINE mapError #-}