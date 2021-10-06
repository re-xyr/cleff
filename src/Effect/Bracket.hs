module Effect.Bracket where

import           Control.Monad.IO.Class (liftIO)
import           Effect
import           Effect.Internal.Base   (thisIsPureTrustMe)
import qualified UnliftIO.Exception     as Exc

data Bracket :: Effect where
  Mask :: ((forall x. m x -> m x) -> m a) -> Bracket m a
  UninterruptibleMask :: ((forall x. m x -> m x) -> m a) -> Bracket m a
  Bracket :: m a -> (a -> m c) -> (a -> m b) -> Bracket m b
  BracketOnError :: m a -> (a -> m c) -> (a -> m b) -> Bracket m b

bracket :: Bracket :> es => Eff es a -> (a -> Eff es c) -> (a -> Eff es b) -> Eff es b
bracket ma mz m = send $ Bracket ma mz m
{-# INLINE bracket #-}

bracket_ :: Bracket :> es => Eff es a -> Eff es c -> (a -> Eff es b) -> Eff es b
bracket_ ma mz = bracket ma (const mz)
{-# INLINE bracket_ #-}

bracketOnError :: Bracket :> es => Eff es a -> (a -> Eff es c) -> (a -> Eff es b) -> Eff es b
bracketOnError ma mz m = send $ BracketOnError ma mz m
{-# INLINE bracketOnError #-}

finally :: Bracket :> es => Eff es a -> Eff es b -> Eff es a
finally m mz = bracket_ (pure ()) mz (const m)
{-# INLINE finally #-}

onError :: Bracket :> es => Eff es a -> Eff es b -> Eff es a
onError m mz = bracketOnError (pure ()) (const mz) (const m)
{-# INLINE onError #-}

runBracket :: forall es a. Eff (Bracket ': es) a -> Eff es a
runBracket = thisIsPureTrustMe . reinterpret \case
  Mask f -> liftIO $ Exc.mask \restore -> withLiftIO \lift -> f (lift . restore . unliftIO)
  UninterruptibleMask f -> liftIO $ Exc.mask \restore -> withLiftIO \lift -> f (lift . restore . unliftIO)
  Bracket ma mz m -> liftIO $ Exc.bracket (unliftIO ma) (unliftIO . mz) (unliftIO . m)
  BracketOnError ma mz m -> liftIO $ Exc.bracketOnError (unliftIO ma) (unliftIO . mz) (unliftIO . m)
