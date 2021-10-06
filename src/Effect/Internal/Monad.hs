module Effect.Internal.Monad where

import           Control.Monad.Fix (MonadFix (mfix))
import           Data.Coerce       (Coercible)
import           Data.Kind         (Constraint)
import           Data.Maybe        (fromJust)
import           Data.Reflection   (Given, give)
import           Data.TypeRepMap   (TypeRepMap)
import qualified Data.TypeRepMap   as TMap
import           Data.Typeable     (Typeable)
import           Unsafe.Coerce     (unsafeCoerce)

type Effect = (* -> *) -> * -> *

class CanThread e where
  canThread :: ()

newtype InstCanThread e a = InstCanThread (CanThread e => a)

instCanThread :: forall e a. (CanThread e => a) -> a
instCanThread x = unsafeCoerce (InstCanThread x :: InstCanThread e a) ()
{-# INLINE instCanThread #-}

type Handler es e = forall es' a. (Given (Env es'), e :> es', CanThread e) => e (Eff es') a -> Eff es a

data InternalHandler e = Representational e => InternalHandler
  { runHandler :: forall es' a. (Given (Env es'), e :> es') => e (Eff es') a -> IO a }

newtype Env es = PrimEnv { primGetEnv :: TypeRepMap InternalHandler }

newtype Eff (es :: [Effect]) a = PrimEff { primRunEff :: Env es -> IO a }
  deriving (Semigroup, Monoid)

instance Functor (Eff es) where
  fmap f (PrimEff m) = PrimEff (fmap f . m)
  {-# INLINE fmap #-}
  a <$ PrimEff m = PrimEff \es -> a <$ m es
  {-# INLINE (<$) #-}

instance Applicative (Eff es) where
  pure x = PrimEff \_ -> pure x
  {-# INLINE pure #-}
  PrimEff mf <*> PrimEff mx = PrimEff \es -> mf es <*> mx es
  {-# INLINE (<*>) #-}
  PrimEff ma  *> PrimEff mb = PrimEff \es -> ma es  *> mb es
  {-# INLINE  (*>) #-}
  PrimEff ma <*  PrimEff mb = PrimEff \es -> ma es <*  mb es
  {-# INLINE (<*)  #-}

instance Monad (Eff es) where
  return = pure
  {-# INLINE return #-}
  PrimEff m >>= f = PrimEff \es -> m es >>= \a -> primRunEff (f a) es
  {-# INLINE (>>=) #-}
  PrimEff ma >> PrimEff mb = PrimEff \es -> ma es >> mb es
  {-# INLINE (>>) #-}

instance MonadFix (Eff es) where
  mfix f = PrimEff \es -> mfix $ \a -> primRunEff (f a) es
  {-# INLINE mfix #-}

class (forall m n a. Coercible m n => Coercible (e m a) (e n a)) => Representational e
instance (forall m n a. Coercible m n => Coercible (e m a) (e n a)) => Representational e
type Legit e = (Representational e, Typeable e)

class Legit e => (e :: Effect) :> (es :: [Effect])
instance {-# OVERLAPPING #-} Legit e => e :> (e ': es)
instance e :> es => e :> (f ': es)

type family (xs :: [Effect]) :>> (ys :: [Effect]) :: Constraint where
  '[] :>> ys = ()
  (x ': xs) :>> ys = (x :> ys, xs :>> ys)

type family xs ++ ys where
  '[] ++ ys = ys
  (x ': xs) ++ ys = x ': (xs ++ ys)

send :: forall e es a. e :> es => e (Eff es) a -> Eff es a
send eff = PrimEff \handlers -> give handlers $ runHandler (getHandler @e handlers) eff
{-# INLINE send #-}

emptyEnv :: Env '[]
emptyEnv = PrimEnv TMap.empty
{-# INLINE emptyEnv #-}

contractEnv :: Env (es' ++ es) -> Env es
contractEnv = PrimEnv . primGetEnv
{-# INLINE contractEnv #-}

expandEnv :: es' :>> es => Env es -> Env (es' ++ es)
expandEnv = PrimEnv . primGetEnv
{-# INLINE expandEnv #-}

getHandler :: forall e es. e :> es => Env es -> InternalHandler e
getHandler = fromJust . TMap.lookup . primGetEnv
{-# INLINE getHandler #-}

insertHandler :: forall e es. Legit e => InternalHandler e -> Env es -> Env (e ': es)
insertHandler f = PrimEnv . TMap.insert f . primGetEnv
{-# INLINE insertHandler #-}
