module Effect.Internal.Monad where

import           Control.Monad.Fix (MonadFix (mfix))
import           Data.Kind         (Constraint)
import           Data.Maybe        (fromJust)
import           Data.TypeRepMap   (TypeRepMap)
import qualified Data.TypeRepMap   as TMap
import           Data.Typeable     (Typeable)
import           Unsafe.Coerce     (unsafeCoerce)

type Effect = (* -> *) -> * -> *

class Originating es where
  originatingEnv :: Env es

newtype InstOriginating es e a = InstOriginating (Originating es => a)

instOriginating :: forall es a. (Originating es => a) -> Env es -> a
instOriginating x = unsafeCoerce (InstOriginating x :: InstOriginating es e a)
-- {-# INLINE instOriginating #-}

type Handler es e = forall es' a. (e :> es', Originating es') => e (Eff es') a -> Eff es a

newtype InternalHandler e = InternalHandler
  { runHandler :: forall es' a. (e :> es', Originating es') => e (Eff es') a -> IO a }

type role Env nominal
newtype Env es = PrimEnv { primGetEnv :: TypeRepMap InternalHandler }

type role Eff nominal nominal
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

class Typeable e => (e :: Effect) :> (es :: [Effect])
instance {-# OVERLAPPING #-} Typeable e => e :> (e ': es)
instance e :> es => e :> (f ': es)

type family (xs :: [Effect]) :>> (ys :: [Effect]) :: Constraint where
  '[] :>> ys = ()
  (x ': xs) :>> ys = (x :> ys, xs :>> ys)

type family xs ++ ys where
  '[] ++ ys = ys
  (x ': xs) ++ ys = x ': (xs ++ ys)

send :: forall e es a. e :> es => e (Eff es) a -> Eff es a
send eff = PrimEff \handlers -> instOriginating (runHandler (getHandler handlers) eff) handlers
{-# INLINE send #-}

emptyEnv :: Env '[]
emptyEnv = PrimEnv TMap.empty

contractEnv :: Env (es' ++ es) -> Env es
contractEnv = PrimEnv . primGetEnv

expandEnv :: es' :>> es => Env es -> Env (es' ++ es)
expandEnv = PrimEnv . primGetEnv

getHandler :: forall e es. e :> es => Env es -> InternalHandler e
getHandler = fromJust . TMap.lookup . primGetEnv

insertHandler :: forall e es. Typeable e => InternalHandler e -> Env es -> Env (e ': es)
insertHandler f = PrimEnv . TMap.insert f . primGetEnv
