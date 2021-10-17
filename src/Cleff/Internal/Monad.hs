-- | This module contains the definition of the 'Eff' monad, which is basically an @'Env' es -> 'IO' a@, as well as
-- functions for manipulating the effect environment type 'Env'. Most of the times, you won't need to use this module
-- directly; user-facing functionalities are all exported via the "Cleff" module.
{-# OPTIONS_HADDOCK not-home #-}
module Cleff.Internal.Monad
  ( -- * Core types
    InternalHandler (..), Env (..), Eff (..)
  , -- * Effect environment axioms
    emptyEnv, contractEnv, expandEnv, getHandler, insertHandler
  , -- * Performing effect operations
    send
  ) where

import           Cleff.Internal.Effect
import           Control.Monad.Fix     (MonadFix (mfix))
import           Data.Maybe            (fromJust)
import           Data.TypeRepMap       (TypeRepMap)
import qualified Data.TypeRepMap       as TMap
import           Data.Typeable         (Typeable)

-- | The internal representation of effect handlers. This is just a natural transformation from the effect type
-- @e ('Eff' es)@ to the effect monad @'Eff' es@ for any effect stack @es@ that has @e@ in it.
--
-- In interpreting functions (see "Cleff.Internal.Interpret"), the user-facing 'Cleff.Handler' type is transformed into
-- this type.
newtype InternalHandler e = InternalHandler
  { runHandler :: forall esSend. e :> esSend => e (Eff esSend) ~> Eff esSend }

-- | The effect environment that stores handlers of any effect present in the stack @es@.
type role Env nominal -- This blocks users from liberally 'coerce'ing between different effect stacks.
newtype Env es = Env { getEnv :: TypeRepMap InternalHandler }

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
emptyEnv = Env TMap.empty

-- | Trivially contract larger environment into a smaller one.
contractEnv :: Env (es' ++ es) -> Env es
contractEnv = Env . getEnv

-- | Trivially expand smaller environment into a larger one, given the added part is already present in the original
-- stack.
expandEnv :: es' :>> es => Env es -> Env (es' ++ es)
expandEnv = Env . getEnv

-- | Get the handler from the environment for an effect present in the effect stack.
getHandler :: forall e es. e :> es => Env es -> InternalHandler e
getHandler = fromJust . TMap.lookup . getEnv

-- | Insert a handler into an environment to extend the stack.
insertHandler :: forall e es. Typeable e => InternalHandler e -> Env es -> Env (e ': es)
insertHandler f = Env . TMap.insert f . getEnv

-- | Perform an effect operation, /i.e./ a value constructed by a constructor of an effect type @e@, given @e@ is in
-- the effect stack.
send :: e :> es => e (Eff es) ~> Eff es
send eff = PrimEff \handlers -> primRunEff (runHandler (getHandler handlers) eff) handlers
{-# INLINE send #-}
