module Cleff.Mask where

import           Cleff
import           Cleff.Internal.Base    (thisIsPureTrustMe)
import           Control.Monad.IO.Class (liftIO)
import qualified UnliftIO.Exception     as Exc

data Mask :: Effect where
  Mask :: ((m ~> m) -> m a) -> Mask m a
  UninterruptibleMask :: ((m ~> m) -> m a) -> Mask m a
  Bracket :: m a -> (a -> m c) -> (a -> m b) -> Mask m b
  BracketOnError :: m a -> (a -> m c) -> (a -> m b) -> Mask m b
makeEffect ''Mask

mask_ :: Mask :> es => Eff es a -> Eff es a
mask_ m = mask $ const m

uninterruptibleMask_ :: Mask :> es => Eff es a -> Eff es a
uninterruptibleMask_ m = uninterruptibleMask $ const m

bracket_ :: Mask :> es => Eff es a -> Eff es c -> (a -> Eff es b) -> Eff es b
bracket_ ma = bracket ma . const

finally :: Mask :> es => Eff es a -> Eff es b -> Eff es a
finally m mz = bracket_ (pure ()) mz (const m)

onError :: Mask :> es => Eff es a -> Eff es b -> Eff es a
onError m mz = bracketOnError (pure ()) (const mz) (const m)

runMask :: Eff (Mask ': es) ~> Eff es
runMask = thisIsPureTrustMe . reinterpret \case
  Mask f -> liftIO $ withLiftIO \lift -> Exc.mask \restore -> runInIO $ f (lift . restore . runInIO)
  UninterruptibleMask f -> liftIO $ withLiftIO \lift -> Exc.uninterruptibleMask \restore -> runInIO $ f (lift . restore . runInIO)
  Bracket ma mz m -> liftIO $ Exc.bracket (runInIO ma) (runInIO . mz) (runInIO . m)
  BracketOnError ma mz m -> liftIO $ Exc.bracketOnError (runInIO ma) (runInIO . mz) (runInIO . m)
{-# INLINE runMask #-}
