-- | This module contains the definition of the 'Eff' monad, which is basically an @'Env' es -> 'IO' a@, as well as
-- functions for manipulating the effect environment type 'Env'. Most of the times, you won't need to use this module
-- directly; user-facing functionalities are all exported via the "Cleff" module.
{-# OPTIONS_HADDOCK not-home #-}
module Cleff.Internal.Monad
  ( -- * Core types
    InternalHandler (..), Env (..), Eff (..)
  , -- * Effect environment axioms
    KnownList, Subset,
    emptyEnv, contractEnv, expandEnv, getHandler, getSubsetEnv, modifyHandler, insertHandler
  , -- * Performing effect operations
    send
  ) where

import           Cleff.Internal.Effect
import           Control.Monad.Fix     (MonadFix (mfix))
import           Data.Proxy            (Proxy (Proxy))
import           Data.Rec              (KnownList, Rec, Subset, (~!~), (~+~), (~:~))
import qualified Data.Rec              as Rec
import           Data.Typeable         (Typeable, typeRep)

-- | The internal representation of effect handlers. This is just a natural transformation from the effect type
-- @e ('Eff' es)@ to the effect monad @'Eff' es@ for any effect stack @es@ that has @e@ in it.
--
-- In interpreting functions (see "Cleff.Internal.Interpret"), the user-facing 'Cleff.Handler' type is transformed into
-- this type.
newtype InternalHandler e = InternalHandler
  { runHandler :: forall esSend. e :> esSend => e (Eff esSend) ~> Eff esSend }

-- | @
-- 'show' (handler :: 'InternalHandler' E) == "Handler E"
-- @
instance Typeable e => Show (InternalHandler e) where
  showsPrec p _ = ("Handler " ++) . showsPrec p (typeRep (Proxy :: Proxy e))

-- | The effect environment that stores handlers of any effect present in the stack @es@. Uses the 'Rec' type for fast
-- reads.
newtype Env (es :: [Effect]) = Env { getEnv :: Rec InternalHandler es }

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
type role Eff nominal nominal
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

-- | The environment for the empty effect stack.
emptyEnv :: Env '[]
emptyEnv = Env Rec.empty

-- | Contract a large environment into a smaller one.
contractEnv :: forall es' es. KnownList es' => Env (es' ++ es) -> Env es
contractEnv = Env . Rec.drop @es' . getEnv

-- | Expand a small environment into a larger one, given the added part is already present in the original stack.
expandEnv :: forall es' es. Subset es' es => Env es -> Env (es' ++ es)
expandEnv env = Env $ Rec.pick @es' (getEnv env) ~+~ getEnv env

-- | Get the handler from the environment for an effect present in the effect stack.
getHandler :: forall e es. e :> es => Env es -> InternalHandler e
getHandler = Rec.index @e . getEnv

-- | Get a subset of the handlers from an environment.
getSubsetEnv :: forall es' es. Subset es' es => Env es -> Env es'
getSubsetEnv = Env . Rec.pick @es' . getEnv

-- | Modify a handler that is already on the stack.
modifyHandler :: forall e es. e :> es => InternalHandler e -> Env es -> Env es
modifyHandler f = Env . (f ~!~) . getEnv

-- | Insert a handler into an environment to extend the stack.
insertHandler :: forall e es. InternalHandler e -> Env es -> Env (e ': es)
insertHandler f = Env . (f ~:~) . getEnv

-- | Perform an effect operation, /i.e./ a value constructed by a constructor of an effect type @e :: 'Effect'@, given
-- @e@ is in the effect stack.
send :: e :> es => e (Eff es) ~> Eff es
send eff = PrimEff \handlers -> primRunEff (runHandler (getHandler handlers) eff) handlers
