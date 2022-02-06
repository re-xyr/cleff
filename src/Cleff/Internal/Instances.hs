{-# OPTIONS_GHC -Wno-orphans #-}
-- |
-- Copyright: (c) 2021 Xy Ren
-- License: BSD3
-- Maintainer: xy.r@outlook.com
-- Stability: unstable
-- Portability: non-portable (GHC only)
--
-- This module contains lifted instances of some typeclasses for 'Eff' for convenience. They are all exported in the
-- "Cleff" module so you shouldn't need to import this module.
--
-- __This is an /internal/ module and its API may change even between minor versions.__ Therefore you should be
-- extra careful if you're to depend on this module.
module Cleff.Internal.Instances () where

import           Cleff.Internal.Monad (Eff)
import           Control.Applicative  (Applicative (liftA2))
import           Control.Monad.Zip    (MonadZip (munzip, mzipWith))
import           Data.String          (IsString (fromString))

instance Bounded a => Bounded (Eff es a) where
  minBound = pure minBound
  maxBound = pure maxBound

instance Num a => Num (Eff es a) where
  (+) = liftA2 (+)
  (-) = liftA2 (-)
  (*) = liftA2 (*)
  negate = fmap negate
  abs = fmap abs
  signum = fmap signum
  fromInteger = pure . fromInteger

instance Fractional a => Fractional (Eff es a) where
  (/) = liftA2 (/)
  recip = fmap recip
  fromRational = pure . fromRational

instance Floating a => Floating (Eff es a) where
  pi = pure pi
  exp = fmap exp
  log = fmap log
  sqrt = fmap sqrt
  (**) = liftA2 (**)
  logBase = liftA2 logBase
  sin = fmap sin
  cos = fmap cos
  tan = fmap tan
  asin = fmap asin
  acos = fmap acos
  atan = fmap atan
  sinh = fmap sinh
  cosh = fmap cosh
  tanh = fmap tanh
  asinh = fmap asinh
  acosh = fmap acosh
  atanh = fmap atanh

instance Semigroup a => Semigroup (Eff es a) where
  (<>) = liftA2 (<>)

instance Monoid a => Monoid (Eff es a) where
  mempty = pure mempty

instance IsString a => IsString (Eff es a) where
  fromString = pure . fromString

-- | Compatibility instance for @MonadComprehensions@.
instance MonadZip (Eff es) where
  mzipWith = liftA2
  munzip x = (fst <$> x, snd <$> x)
