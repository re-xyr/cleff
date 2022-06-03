{-# OPTIONS_HADDOCK not-home #-}
-- |
-- Copyright: (c) 2021 Xy Ren
-- License: BSD3
-- Maintainer: xy.r@outlook.com
-- Stability: unstable
-- Portability: non-portable (GHC only)
--
-- This module contains the definition of the 'Eff' monad. Most of the times, you won't need to use this module
-- directly; user-facing functionalities are all exported via the "Cleff" module.
--
-- __This is an /internal/ module and its API may change even between minor versions.__ Therefore you should be
-- extra careful if you're to depend on this module.
module Cleff.Internal.Monad
  ( -- * The 'Eff' monad
    Eff (Eff, unEff)
  , Env (Env)
    -- * Constraints on effect stacks
  , (:>)
  , (:>>)
  , KnownList
  , Subset
  ) where

import           Cleff.Internal
import           Cleff.Internal.Rec  (KnownList, Rec, Subset, type (:>), type (:>>))
import           Control.Applicative (Applicative (liftA2))
import           Control.Monad.Fix   (MonadFix (mfix))
import           Data.Any            (Any)
import           Data.RadixVec       (RadixVec)

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

-- | The /effect environment/ that corresponds effects in the stack to their respective 'InternalHandler's. This
-- structure simulates memory: handlers are retrieved via pointers ('HandlerPtr's), and for each effect in the stack
-- we can either change what pointer it uses or change the handler the pointer points to. The former is used for global
-- effect interpretation ('Cleff.reinterpretN') and the latter for local interpretation ('Cleff.toEffWith') in order to
-- retain correct HO semantics. For more details on this see https://github.com/re-xyr/cleff/issues/5.
type role Env nominal
data Env (es :: [Effect]) = Env
  {-# UNPACK #-} !(Rec es) -- ^ The effect stack storing pointers to handlers.
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
