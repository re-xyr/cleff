-- | This module contains the definition of the 'Eff' monad, which is basically an @'Env' es -> 'IO' a@, as well as
-- functions for manipulating the effect environment type 'Env'. Most of the times, you won't need to use this module
-- directly; user-facing functionalities are all exported via the "Cleff" module.
{-# OPTIONS_HADDOCK not-home #-}
module Cleff.Internal.Monad
  ( -- * Core types
    InternalHandler (..), Env, Eff (..)
  , -- * Performing effect operations
    KnownList, Subset, send
  ) where

import           Cleff.Internal.Effect
import           Control.Monad.Fix     (MonadFix (mfix))
import           Data.Rec              (KnownList, Rec, Subset)
import qualified Data.Rec              as Rec
import           Type.Reflection       (Typeable, typeRep)

-- | The internal representation of effect handlers. This is just a natural transformation from the effect type
-- @e ('Eff' es)@ to the effect monad @'Eff' es@ for any effect stack @es@ that has @e@ in it.
--
-- In interpreting functions (see "Cleff.Internal.Interpret"), the user-facing 'Cleff.Handler' type is transformed into
-- this type.
newtype InternalHandler e = InternalHandler
  { runHandler :: forall es. e :> es => e (Eff es) ~> Eff es }

-- | @
-- 'show' (handler :: 'InternalHandler' E) == "Handler E"
-- @
instance Typeable e => Show (InternalHandler e) where
  showsPrec p _ = ("Handler " ++) . showsPrec p (typeRep @e)

-- | The effect environment that stores handlers of any effect present in the stack @es@. Uses the 'Rec' type for fast
-- reads.
type Env = Rec InternalHandler

-- | The extensible effect monad. A monad @'Eff' es@ is capable of performing any effect in the /effect stack/ @es@.
-- Most of the times, @es@ should be a polymorphic effect stack, constrained by the '(:>)' and '(:>>)' operators that
-- indicate what effects are present in it. For example, the type
--
-- @
-- 'Cleff.Reader.Reader' 'String' ':>' es, 'Cleff.State.State' 'Bool' ':>' es => 'Eff' es 'Integer'
-- @
--
-- allows you to perform operations of the @'Cleff.Reader.Reader' 'String'@ effect and the @'Cleff.State.State' 'Bool'@
-- effect in a computation returning an 'Integer'.
type role Eff nominal representational
newtype Eff es a = PrimEff { primRunEff :: Env es -> IO a }
  deriving (Semigroup, Monoid)

instance Functor (Eff es) where
  fmap f (PrimEff m) = PrimEff (fmap f . m)
  a <$ PrimEff m = PrimEff \es -> a <$ m es

instance Applicative (Eff es) where
  pure x = PrimEff \_ -> pure x
  PrimEff mf <*> PrimEff mx = PrimEff \es -> mf es <*> mx es
  PrimEff ma  *> PrimEff mb = PrimEff \es -> ma es  *> mb es
  PrimEff ma <*  PrimEff mb = PrimEff \es -> ma es <*  mb es

instance Monad (Eff es) where
  return = pure
  PrimEff m >>= f = PrimEff \es -> m es >>= \a -> primRunEff (f a) es
  PrimEff ma >> PrimEff mb = PrimEff \es -> ma es >> mb es

instance MonadFix (Eff es) where
  mfix f = PrimEff \es -> mfix $ \a -> primRunEff (f a) es

-- | Perform an effect operation, /i.e./ a value constructed by a constructor of an effect type @e :: 'Effect'@, given
-- @e@ is in the effect stack.
send :: e :> es => e (Eff es) ~> Eff es
send eff = PrimEff \handlers -> primRunEff (runHandler (Rec.index handlers) eff) handlers
