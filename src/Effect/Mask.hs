module Effect.Mask where

import           Control.Monad.IO.Class (liftIO)
import           Effect
import           Effect.Internal.Base   (thisIsPureTrustMe)
import qualified UnliftIO.Exception     as Exc

data Mask :: Effect where
  Mask :: ((forall x. m x -> m x) -> m a) -> Mask m a
  UninterruptibleMask :: ((forall x. m x -> m x) -> m a) -> Mask m a
  Bracket :: m a -> (a -> m c) -> (a -> m b) -> Mask m b
  BracketOnError :: m a -> (a -> m c) -> (a -> m b) -> Mask m b

mask :: Mask :> es => ((forall x. Eff es x -> Eff es x) -> Eff es a) -> Eff es a
mask f = send $ Mask f

mask_ :: Mask :> es => Eff es a -> Eff es a
mask_ m = mask $ const m

uninterruptibleMask :: Mask :> es => ((forall x. Eff es x -> Eff es x) -> Eff es a) -> Eff es a
uninterruptibleMask f = send $ UninterruptibleMask f

uninterruptibleMask_ :: Mask :> es => Eff es a -> Eff es a
uninterruptibleMask_ m = uninterruptibleMask $ const m

bracket :: Mask :> es => Eff es a -> (a -> Eff es c) -> (a -> Eff es b) -> Eff es b
bracket ma mz = send . Bracket ma mz

bracket_ :: Mask :> es => Eff es a -> Eff es c -> (a -> Eff es b) -> Eff es b
bracket_ ma = bracket ma . const

bracketOnError :: Mask :> es => Eff es a -> (a -> Eff es c) -> (a -> Eff es b) -> Eff es b
bracketOnError ma mz = send . BracketOnError ma mz

finally :: Mask :> es => Eff es a -> Eff es b -> Eff es a
finally m mz = bracket_ (pure ()) mz (const m)

onError :: Mask :> es => Eff es a -> Eff es b -> Eff es a
onError m mz = bracketOnError (pure ()) (const mz) (const m)

runMask :: forall es a. Eff (Mask ': es) a -> Eff es a
runMask = thisIsPureTrustMe . reinterpret \case
  Mask f -> liftIO $ Exc.mask \restore -> withLiftIO \lift -> f (lift . restore . unliftIO)
  UninterruptibleMask f -> liftIO $ Exc.mask \restore -> withLiftIO \lift -> f (lift . restore . unliftIO)
  Bracket ma mz m -> liftIO $ Exc.bracket (unliftIO ma) (unliftIO . mz) (unliftIO . m)
  BracketOnError ma mz m -> liftIO $ Exc.bracketOnError (unliftIO ma) (unliftIO . mz) (unliftIO . m)
