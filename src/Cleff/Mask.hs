module Cleff.Mask where

import           Cleff
import           Cleff.Internal.Base (thisIsPureTrustMe)
import qualified UnliftIO.Exception  as Exc

-- * Effect

-- | An effect capable of 'Exc.mask'ing and specifically, 'Exc.bracket'ing operations, /i.e./ allowing cleanup after
-- operations that my raise exceptions.
data Mask :: Effect where
  Mask :: ((m ~> m) -> m a) -> Mask m a
  UninterruptibleMask :: ((m ~> m) -> m a) -> Mask m a
  Bracket :: m a -> (a -> m c) -> (a -> m b) -> Mask m b
  BracketOnError :: m a -> (a -> m c) -> (a -> m b) -> Mask m b

-- * Operations

makeEffect ''Mask

-- | Variant of 'mask' that does not provide a restoring function.
mask_ :: Mask :> es => Eff es a -> Eff es a
mask_ m = mask \_ -> m

-- | Variant of 'uninterruptibleMask' that does not provide a restoring function.
uninterruptibleMask_ :: Mask :> es => Eff es a -> Eff es a
uninterruptibleMask_ m = uninterruptibleMask \_ -> m

-- | Variant of 'bracket' that does not pass the allocated resource to the cleanup action.
bracket_ :: Mask :> es => Eff es a -> Eff es c -> (a -> Eff es b) -> Eff es b
bracket_ ma = bracket ma . const

-- | Attach a cleanup action that will always run to a potentially throwing action.
finally :: Mask :> es => Eff es a -> Eff es b -> Eff es a
finally m mz = bracket_ (pure ()) mz (const m)

-- | Attach an action that runs if the main action throws an exception.
onError :: Mask :> es => Eff es a -> Eff es b -> Eff es a
onError m mz = bracketOnError (pure ()) (const mz) (const m)

-- * Interpretations

-- | Interpret the 'Mask' effect in terms of primitive 'IO' actions.
runMask :: Eff (Mask ': es) ~> Eff es
runMask = thisIsPureTrustMe . reinterpret \case
  Mask f -> liftIO $ withLiftIO \lift -> Exc.mask \restore -> runInIO $ f (lift . restore . runInIO)
  UninterruptibleMask f -> liftIO $ withLiftIO \lift -> Exc.uninterruptibleMask \restore -> runInIO $ f (lift . restore . runInIO)
  Bracket ma mz m -> liftIO $ Exc.bracket (runInIO ma) (runInIO . mz) (runInIO . m)
  BracketOnError ma mz m -> liftIO $ Exc.bracketOnError (runInIO ma) (runInIO . mz) (runInIO . m)
{-# INLINE runMask #-}
