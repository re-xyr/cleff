{-# OPTIONS_HADDOCK not-home #-}
-- |
-- Copyright: (c) 2021 Xy Ren
-- License: BSD3
-- Maintainer: xy.r@outlook.com
-- Stability: unstable
-- Portability: non-portable (GHC only)
--
-- This module contains the definition of the 'Eff' monad, as well as reexports of some shared utilities in the
-- internal modules. Most of the times, you won't need to use this module directly; user-facing functionalities are all
-- exported via the "Cleff" module.
--
-- __This is an /internal/ module and its API may change even between minor versions.__ Therefore you should be
-- extra careful if you're to depend on this module.
module Cleff.Internal.Monad
  ( -- * The 'Eff' monad
    Eff (Eff, unEff)
  , Effect
  , Env (Env)
  , HandlerPtr (HandlerPtr, unHandlerPtr)
    -- * Constraints
  , (:>)
  , (:>>)
  , KnownList
  , Subset
    -- * Misc types
  , type (++)
  , type (~>)
  ) where

import           Cleff.Internal.Stack (Effect, HandlerPtr (HandlerPtr, unHandlerPtr), KnownList, Stack, Subset,
                                       type (++), type (:>), type (:>>))
import           Control.Applicative  (Applicative (liftA2))
import           Control.Monad.Fix    (MonadFix (mfix))
import           Control.Monad.Zip    (MonadZip (munzip, mzipWith))
import           Data.Any             (Any)
import           Data.Monoid          (Ap (Ap))
import           Data.RadixVec        (RadixVec)
import           Data.String          (IsString (fromString))

-- * The 'Eff' monad

-- | The extensible effects monad. The monad @'Eff' es@ is capable of performing any effect in the /effect stack/ @es@,
-- which is a type-level list that holds all effects available.
--
-- The best practice is to always use a polymorphic type variable for the effect stack @es@, and then use the type
-- operator '(:>)' in constraints to indicate what effects are available in the stack. For example,
--
-- @
-- ('Cleff.Reader.Reader' 'String' ':>' es, 'Cleff.State.State' 'Bool' ':>' es) => 'Eff' es 'Integer'
-- @
--
-- means you can perform operations of the @'Cleff.Reader.Reader' 'String'@ effect and the @'Cleff.State.State' 'Bool'@
-- effect in a computation returning an 'Integer'. A convenient shorthand, '(:>>)', can also be used to indicate
-- multiple effects being in a stack:
--
-- @
-- '['Cleff.Reader.Reader' 'String', 'Cleff.State.State' 'Bool'] ':>>' es => 'Eff' es 'Integer'
-- @
--
-- The reason why you should always use a polymorphic effect stack as opposed to a concrete list of effects are that:
--
-- * it can contain other effects that are used by computations other than the current one, and
-- * it does not require you to run the effects in any particular order.
type role Eff nominal representational
newtype Eff es a = Eff { unEff :: Env es -> IO a }
  -- ^ The effect monad receives an effect environment 'Env' that contains all effect handlers and produces an 'IO'
  -- action.

-- | The /effect environment/ that corresponds effects in the stack to their respective handlers. This
-- structure simulates memory: handlers are retrieved via pointers ('HandlerPtr's), and for each effect in the stack
-- we can either change what pointer it uses or change the handler the pointer points to. The former is used for global
-- effect interpretation ('Cleff.reinterpretN') and the latter for local interpretation ('Cleff.toEffWith') in order to
-- retain correct HO semantics. For more details on this see https://github.com/re-xyr/cleff/issues/5.
type role Env nominal
data Env (es :: [Effect]) = Env
  {-# UNPACK #-} !(Stack es) -- ^ The effect stack storing pointers to handlers.
  {-# UNPACK #-} !(RadixVec Any) -- ^ The storage that corresponds pointers to handlers.

instance Functor (Eff es) where
  fmap f (Eff x) = Eff (fmap f . x)
  x <$ Eff y = Eff \es -> x <$ y es

instance Applicative (Eff es) where
  pure = Eff . const . pure
  Eff f <*> Eff x = Eff \es -> f es <*> x es
  Eff x <*  Eff y = Eff \es -> x es <*  y es
  Eff x  *> Eff y = Eff \es -> x es  *> y es
  liftA2 f (Eff x) (Eff y) = Eff \es -> liftA2 f (x es) (y es)

instance Monad (Eff es) where
  -- no 'return', because the default impl is correct and it is going to be deprecated anyway
  Eff x >>= f = Eff \es -> x es >>= \x' -> unEff (f x') es
  (>>) = (*>) -- More efficient, since the default is @x >> y = x >>= const y@

instance MonadFix (Eff es) where
  mfix f = Eff \es -> mfix \x -> unEff (f x) es

-- | @since 0.2.1.0
deriving via (Ap (Eff es) a) instance Bounded a => Bounded (Eff es a)

-- | @since 0.2.1.0
instance Num a => Num (Eff es a) where
  (+) = liftA2 (+)
  (-) = liftA2 (-)
  (*) = liftA2 (*)
  negate = fmap negate
  abs = fmap abs
  signum = fmap signum
  fromInteger = pure . fromInteger

-- | @since 0.2.1.0
instance Fractional a => Fractional (Eff es a) where
  (/) = liftA2 (/)
  recip = fmap recip
  fromRational = pure . fromRational

-- | @since 0.2.1.0
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

-- | @since 0.2.1.0
deriving newtype instance Semigroup a => Semigroup (Eff es a)

-- | @since 0.2.1.0
deriving newtype instance Monoid a => Monoid (Eff es a)

-- | @since 0.2.1.0
instance IsString a => IsString (Eff es a) where
  fromString = pure . fromString

-- | Compatibility instance for @MonadComprehensions@.
--
-- @since 0.2.1.0
instance MonadZip (Eff es) where
  mzipWith = liftA2
  munzip x = (fst <$> x, snd <$> x)

-- * Misc types

-- | A natural transformation from @f@ to @g@. With this, instead of writing
--
-- @
-- runSomeEffect :: 'Cleff.Eff' (SomeEffect : es) a -> 'Cleff.Eff' es a
-- @
--
-- you can write:
--
-- @
-- runSomeEffect :: 'Cleff.Eff' (SomeEffect : es) ~> 'Cleff.Eff' es
-- @
type f ~> g = ∀ a. f a -> g a
