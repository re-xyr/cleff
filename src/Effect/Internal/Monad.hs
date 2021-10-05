module Effect.Internal.Monad where

import           Control.Monad.Fix    (MonadFix)
import           Control.Monad.Reader (ReaderT (..))
import           Data.Coerce          (Coercible)
import           Data.Kind            (Constraint)
import           Data.Maybe           (fromJust)
import           Data.Reflection      (Given, give)
import           Data.TypeRepMap      (TypeRepMap)
import qualified Data.TypeRepMap      as TMap
import           Data.Typeable        (Typeable)

type Effect = (* -> *) -> * -> *

type Handler es e = forall es' a. (Given (Env es'), e :> es') => e (Eff es') a -> Eff es a

data InternalHandler e = Representational e => InternalHandler
  { runHandler :: forall es' a. (Given (Env es'), e :> es') => e (Eff es') a -> IO a }

newtype Env es = PrimEnv { primGetEnv :: TypeRepMap InternalHandler }

newtype Eff (es :: [Effect]) a = PrimEff { primRunEff :: Env es -> IO a }
  deriving (Semigroup, Monoid)
  deriving (Functor, Applicative, Monad, MonadFix) via ReaderT (Env es) IO

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

raise :: forall e es a. Eff es a -> Eff (e ': es) a
raise = raiseN @'[e]
{-# INLINE raise #-}

raiseN :: forall es' es a. Eff es a -> Eff (es' ++ es) a
raiseN m = PrimEff (primRunEff m . contractEnv @es')
{-# INLINE raiseN #-}

subsume :: forall e es a. e :> es => Eff (e ': es) a -> Eff es a
subsume = subsumeN @'[e]
{-# INLINE subsume #-}

subsumeN :: forall es' es a. es' :>> es => Eff (es' ++ es) a -> Eff es a
subsumeN m = PrimEff (primRunEff m . expandEnv @es')
{-# INLINE subsumeN #-}

emptyEnv :: Env '[]
emptyEnv = PrimEnv TMap.empty
{-# INLINE emptyEnv #-}

contractEnv :: Env (es' ++ es) -> Env es
contractEnv = PrimEnv . primGetEnv
{-# INLINE contractEnv #-}

expandEnv :: es' :>> es => Env es -> Env (es' ++ es)
expandEnv = PrimEnv . primGetEnv
{-# INLINE expandEnv #-}

unionEnv :: Env es -> Env es' -> Env es'
unionEnv (PrimEnv es) (PrimEnv es') = PrimEnv $ TMap.union es es'
{-# INLINE unionEnv #-}

getHandler :: forall e es. e :> es => Env es -> InternalHandler e
getHandler = fromJust . TMap.lookup . primGetEnv
{-# INLINE getHandler #-}

insertHandler :: forall e es. Legit e => InternalHandler e -> Env es -> Env (e ': es)
insertHandler f = PrimEnv . TMap.insert f . primGetEnv
{-# INLINE insertHandler #-}
