module Effect.Internal.Monad where

import           Control.Monad.Fix    (MonadFix)
import           Control.Monad.Reader (ReaderT (..))
import           Data.Coerce          (Coercible)
import           Data.Maybe           (fromJust)
import           Data.Reflection      (Given, give)
import           Data.TypeRepMap      (TypeRepMap)
import qualified Data.TypeRepMap      as TMap
import           Data.Typeable        (Typeable)
import           Unsafe.Coerce        (unsafeCoerce)

type Effect = (* -> *) -> * -> *

type Handler es e = forall es' a. (Given (Env es'), e :> es') => e (Eff es') a -> Eff es a

data HandlerOf es e = Representational e => HandlerOf
  { getEnv     :: Env es
  , runHandler :: Handler es e
  }

newtype Env es = PrimEnv { primGetEnv :: TypeRepMap (HandlerOf es) }

newtype Eff (es :: [Effect]) a = PrimEff { primRunEff :: Env es -> IO a }
  deriving (Semigroup, Monoid)
  deriving (Functor, Applicative, Monad, MonadFix) via ReaderT (Env es) IO

class (forall m n a. Coercible m n => Coercible (e m a) (e n a)) => Representational e
instance (forall m n a. Coercible m n => Coercible (e m a) (e n a)) => Representational e
type Legal e = (Representational e, Typeable e)

class Legal e => (e :: Effect) :> (es :: [Effect])
instance {-# OVERLAPPING #-} Legal e => e :> (e ': es)
instance e :> es => e :> (f ': es)

type family xs ++ ys where
  '[] ++ ys = ys
  (x ': xs) ++ ys = x ': (xs ++ ys)

send :: forall e es a. e :> es => e (Eff es) a -> Eff es a
send e = PrimEff \hdls -> let hdl = getHandler hdls
  in primRunEff ((give hdls $ runHandler hdl) e) (getEnv hdl)

raise :: forall e es a. Eff es a -> Eff (e ': es) a
raise = unsafeCoerce
{-# INLINE raise #-}

subsume :: e :> es => Eff (e ': es) a -> Eff es a
subsume = unsafeCoerce
{-# INLINE subsume #-}

emptyEnv :: Env '[]
emptyEnv = PrimEnv TMap.empty
{-# INLINE emptyEnv #-}

getHandler :: forall e es. e :> es => Env es -> HandlerOf es e
getHandler = fromJust . TMap.lookup . primGetEnv
{-# INLINE getHandler #-}

insertHandler :: forall es' e es. Legal e => HandlerOf es e -> Env es -> Env es'
insertHandler f = unsafeCoerce . TMap.insert f . primGetEnv
{-# INLINE insertHandler #-}
