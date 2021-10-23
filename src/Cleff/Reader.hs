module Cleff.Reader where

import           Cleff
import           Lens.Micro (Lens', (%~), (&), (^.))

-- * Effect

-- | An effect capable of providing an immutable environment @r@ that can be read. This roughly corresponds to the
-- @MonadReader@ typeclass and @ReaderT@ monad transformer in the @mtl@ approach.
data Reader r :: Effect where
  Ask :: Reader r m r
  Local :: (r -> r) -> m a -> Reader r m a

-- * Operations

makeEffect ''Reader

-- | Apply a function on the result of 'ask'.
asks :: Reader r :> es => (r -> s) -> Eff es s
asks = (<$> ask)

-- * Interpretations

-- | Run a 'Reader' effect with a given environment value.
runReader :: r -> Eff (Reader r ': es) ~> Eff es
runReader r = interpret \case
  Ask        -> pure r
  Local f m' -> runReader (f r) $ runHere m'
{-# INLINE runReader #-}

-- | Run a 'Reader' effect in terms of a larger 'Reader' via a 'Lens''.
magnify :: Reader t :> es => Lens' t r -> Eff (Reader r ': es) ~> Eff es
magnify field = interpret \case
  Ask        -> asks (^. field)
  Local f m' -> local (& field %~ f) $ magnify field $ runHere m'
{-# INLINE magnify #-}
