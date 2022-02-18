{-# LANGUAGE Trustworthy #-}
-- |
-- Copyright: (c) 2021 Xy Ren
-- License: BSD3
-- Maintainer: xy.r@outlook.com
-- Stability: experimental
-- Portability: non-portable (GHC only)
module Cleff.Reader
  ( -- * Effect
    Reader (..)
    -- * Operations
  , ask
  , local
  , asks
    -- * Interpretations
  , runReader
  , magnify
  ) where

import           Cleff
import           Lens.Micro (Lens', (%~), (&), (^.))

-- * Effect

-- | An effect capable of providing an immutable environment @r@ that can be read. This roughly corresponds to the
-- @MonadReader@ typeclass and @ReaderT@ monad transformer in the @mtl@ library.
data Reader r :: Effect where
  Ask :: Reader r m r
  Local :: (r -> r) -> m a -> Reader r m a

-- * Operations

makeEffect_ ''Reader

-- | Obtain the environment value.
ask :: Reader r :> es => Eff es r

-- | Modify the environment value temporarily for a computation.
local :: Reader r :> es
  => (r -> r) -- ^ The function that modifies the environment
  -> Eff es a -- ^ The computation to run with the modified environment
  -> Eff es a

-- | Apply a function to the result of 'ask'.
asks :: Reader r :> es => (r -> s) -> Eff es s
asks = (<$> ask)

-- * Interpretations

-- | Run a 'Reader' effect with a given environment value.
runReader :: r -> Eff (Reader r ': es) ~> Eff es
runReader x = interpret (h x)
  where
    h :: r -> Handler (Reader r) es
    h r = \case
      Ask       -> pure r
      Local f m -> toEffWith (h (f r)) m
{-# INLINE runReader #-}

-- | Run a 'Reader' effect in terms of a larger 'Reader' via a 'Lens''.
magnify :: Reader t :> es => Lens' t r -> Eff (Reader r ': es) ~> Eff es
magnify field = interpret \case
  Ask       -> asks (^. field)
  Local f m -> local (& field %~ f) $ toEff m
{-# INLINE magnify #-}
