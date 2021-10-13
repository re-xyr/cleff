{-# LANGUAGE FunctionalDependencies #-}
{-# OPTIONS_HADDOCK not-home #-}
module Cleff.Internal.Monad
  ( -- * Core types
    Effect, Handling (..), Handler, InternalHandler (..), Env (..), Eff (..)
  , -- * Effect lookup
    (:>), (:>>), type (++), Suffix
  , -- * Effect environment axioms
    emptyEnv, contractEnv, expandEnv, getHandler, insertHandler
  , -- * Performing effect operations
    InstHandling (..), instHandling, send
  ) where

import           Control.Monad.Fix (MonadFix (mfix))
import           Data.Kind         (Constraint)
import           Data.Maybe        (fromJust)
import           Data.TypeRepMap   (TypeRepMap)
import qualified Data.TypeRepMap   as TMap
import           Data.Typeable     (Typeable)
import           GHC.TypeLits      (ErrorMessage ((:<>:)))
import qualified GHC.TypeLits      as GHC
import           Unsafe.Coerce     (unsafeCoerce)

-- | The type of effects. An effect @e m a@ takes an effect monad type @m :: * -> *@ and result type @a :: *@.
type Effect = (* -> *) -> * -> *

-- | The typeclass that indicates a handler scope, handling effect @e@ sent from environment @esSend@ and being handled
-- on @esBase@.
class Handling esSend esBase e
  | esSend -> esBase, esSend -> e, esBase -> esSend, esBase -> e, e -> esSend, e -> esBase where
  -- | Obtain the send-site environment.
  sendEnv :: Env esSend

-- | The type of effect handler. An effect handler (re)interprets an @e ': es@ effect stack into @es' '++' es@ by
-- converting effect operations from arbitrary send sites into actions in the handling side @es' '++' es@.
type Handler es' es e = forall esSend a. (e :> esSend, Handling esSend es e) => e (Eff esSend) a -> Eff (es' ++ es) a

-- | The internal representation of effect handlers. The handle-site environment is captured via interpreting functions
-- (see "Cleff.Internal.Handle") and thus unwraps the @'Eff' es a@ in the 'Handler' type into an 'IO'.
newtype InternalHandler e = InternalHandler
  { runHandler :: forall esSend a. e :> esSend => Env esSend -> e (Eff esSend) a -> IO a }

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

-- | Constraint that indicates an effect @e@ is present in the effect stack @es@ (thus 'send'able).
class Typeable e => (e :: Effect) :> (es :: [Effect])
instance {-# OVERLAPPING #-} Typeable e => e :> (e ': es)
instance e :> es => e :> (e' ': es)
instance (Typeable e, GHC.TypeError
  ('GHC.Text "The effect '" ':<>: 'GHC.ShowType e ':<>: 'GHC.Text "' is not present in the constraint")) => e :> '[]

-- | Constraint that indicates a list effect @xs@ is present in the effect stack @es@ (thus 'send'able). This is a
-- convenient type alias for @(e1 ':>' es, ..., en ':>' es)@.
type family (xs :: [Effect]) :>> (es :: [Effect]) :: Constraint where
  '[] :>> es = ()
  (x ': xs) :>> es = (x :> es, xs :>> es)

-- | @esBase@ is a suffix of @es@. In other words, @esBase = es' ++ es@. This ensures the presence of 'Cleff.IOE' is
-- the same between @esBase@ and @es@.
class Suffix esBase es
instance {-# INCOHERENT #-} Suffix es es
instance Suffix esBase es => Suffix esBase (e ': es)

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

-- | Newtype wrapper for instantiating the 'Handling' typeclass locally, a la the reflection trick. We do not use
-- the @reflection@ library directly so as not to expose this piece of implementation detail to the user.
newtype InstHandling es' esBase e a = InstHandling (Handling es' esBase e => a)

-- | Instantiatiate an 'Handling' typeclass, i.e. pass an implicit send-site environment in. This function shouldn't
-- be directly used anyway.
instHandling :: forall es' esBase e a. (Handling es' esBase e => a) -> Env es' -> a
instHandling x = unsafeCoerce (InstHandling x :: InstHandling es' esBase e a)
{-# INLINE instHandling #-}

-- | Perform an effect operation, given the effect is in the effect stack.
send :: forall e es a. e :> es => e (Eff es) a -> Eff es a
send eff = PrimEff \handlers -> runHandler (getHandler handlers) handlers eff
{-# INLINE send #-}
