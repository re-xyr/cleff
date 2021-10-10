{-# OPTIONS_HADDOCK not-home #-}
module Effect.Internal.Monad
  ( -- * Core types
    Effect, Originating (..), Handler, InternalHandler (..), Env (..), Eff (..)
  , -- * Effect lookup
    Lookup, (:>), (:>>), type (++)
  , -- * Effect environment axioms
    emptyEnv, contractEnv, expandEnv, getHandler, insertHandler
  , -- * Performing effect operations
    InstOriginating (..), instOriginating, send
  ) where

import           Control.Monad.Fix (MonadFix (mfix))
import           Data.Kind         (Constraint)
import           Data.Maybe        (fromJust)
import           Data.TypeRepMap   (TypeRepMap)
import qualified Data.TypeRepMap   as TMap
import           Data.Typeable     (Typeable)
import           GHC.TypeLits      (ErrorMessage ((:$$:), (:<>:)))
import qualified GHC.TypeLits      as GHC
import           Unsafe.Coerce     (unsafeCoerce)

-- | The type of effects. An effect @e m a@ takes an effect monad type @m :: * -> *@ and result type @a :: *@.
type Effect = (* -> *) -> * -> *

-- | The typeclass used to implicitly pass send-site environment to the handling site, so that higher order effects
-- interpretation is possible.
class Originating es where
  -- | Obtain the send-site environment.
  originatingEnv :: Env es

-- | The type of effect handler. An effect handler interprets an effect from the sendsite into an action in the handle
-- site.
type Handler es e = forall es' a. (e :> es', Originating es') => e (Eff es') a -> Eff es a

-- | The internal representation of effect handlers. The handle-site environment is captured via interpreting functions
-- (see "Effect.Internal.Handle") and thus unwraps the @'Eff' es a@ in the 'Handler' type into an 'IO'.
newtype InternalHandler e = InternalHandler
  { runHandler :: forall es' a. (e :> es', Originating es') => e (Eff es') a -> IO a }

-- This blocks users from liberally 'coerce'ing between different effect stacks.
type role Env nominal
-- | The effect environment that stores handlers of any effect present in the stack @es@.
newtype Env es = Env { getEnv :: TypeRepMap InternalHandler }

-- | The effect monad, i.e. a monad with support of the effects specified by the effect stack @es@. This is basically
-- an @'Env' es -> 'IO' a@.
newtype Eff (es :: [Effect]) a = PrimEff { primRunEff :: Env es -> IO a }
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

-- | Internal class used to lookup if @e@ is present in the effect stack @es@. The extra @oes@ is useful for custom
-- error reporting.
class Lookup e es oes
instance Lookup e (e ': es) oes
instance {-# OVERLAPPABLE #-} Lookup e es oes => Lookup e (f ': es) oes
instance (Typeable e, GHC.TypeError
  ('GHC.Text "The effect '" ':<>: 'GHC.ShowType e ':<>: 'GHC.Text "' is not present in the stack"
  ':$$: 'GHC.Text "  " ':<>: 'GHC.ShowType oes
  ':$$: 'GHC.Text "In the constraint (" ':<>: 'GHC.ShowType (e :> oes) ':<>: 'GHC.Text ")")) => Lookup e '[] oes

-- | Constraint that indicates an effect @e@ is present in the effect stack @es@ (thus 'send'able).
class (Typeable e, Lookup e es es) => (e :: Effect) :> (es :: [Effect])
instance (Typeable e, Lookup e es es) => e :> es

-- | Constraint that indicates a list effect @xs@ is present in the effect stack @es@ (thus 'send'able). This is a
-- convenient type alias for @(e1 ':>' es, ..., en ':>' es)@.
type family (xs :: [Effect]) :>> (es :: [Effect]) :: Constraint where
  '[] :>> es = ()
  (x ': xs) :>> es = (x :> es, xs :>> es)

-- | Type level list concatenation.
type family xs ++ ys where
  '[] ++ ys = ys
  (x ': xs) ++ ys = x ': (xs ++ ys)

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

-- | Newtype wrapper for instantiating the 'Originating' typeclass locally, a la the reflection trick. We do not use
-- the @reflection@ library directly so as not to expose this piece of implementation detail to the user.
newtype InstOriginating es e a = InstOriginating (Originating es => a)

-- | Instantiatiate an 'Originating' typeclass, i.e. pass an implicit send-site environment in. This function shouldn't
-- be directly used anyway.
instOriginating :: forall es a. (Originating es => a) -> Env es -> a
instOriginating x = unsafeCoerce (InstOriginating x :: InstOriginating es e a)
{-# INLINE instOriginating #-}

-- | Perform an effect operation, given the effect is in the effect stack.
send :: forall e es a. e :> es => e (Eff es) a -> Eff es a
send eff = PrimEff \handlers -> instOriginating (runHandler (getHandler handlers) eff) handlers
{-# INLINE send #-}
