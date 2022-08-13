module Sp.Internal.Monad
  ( Eff
  , Effect
  , Handling
  , Handler
  , unsafeIO
  , unsafeState
  , lift
  , interpret
  , reinterpret
  , interpose
  , reinterpose
  , send
  , Localized
  , embed
  , withUnembed
  , abort
  , control
  , runEff
  , (:>)
  , IOE
  , runIOE
  ) where

import           Control.Monad          (ap, liftM)
import           Control.Monad.IO.Class (MonadIO (liftIO))
import           Data.IORef             (IORef)
import           Data.Kind              (Type)
import           Sp.Internal.Ctl
import           Sp.Internal.Env        (Rec, (:>))
import qualified Sp.Internal.Env        as Rec
import           System.IO.Unsafe       (unsafeDupablePerformIO)

-- | The kind of higher-order effects.
type Effect = (Type -> Type) -> Type -> Type

type Env = Rec InternalHandler

-- | The effect monad; it is parameterized by a row of effects available. This monad is implemented with evidence
-- passing and an delimited control monad with support of efficient tail-resumptive (non-capturing) computations and
-- @IO@ embedding.
type role Eff nominal representational
newtype Eff (es :: [Effect]) (a :: Type) = Eff { unEff :: Env es -> Ctl a }

type role InternalHandler nominal
newtype InternalHandler e = InternalHandler
  { runHandler :: forall es a. e :> es => e (Eff es) a -> Eff es a }

instance Functor (Eff es) where
  fmap = liftM

instance Applicative (Eff es) where
  pure x = Eff (const $ pure x)
  (<*>) = ap

instance Monad (Eff es) where
  Eff m >>= f = Eff \es -> m es >>= \x -> unEff (f x) es

-- | The handler token; it is parameterized by an existential sender environment so as not to let it escape the handler
-- scope.
type role Handling nominal nominal representational
data Handling (esSend :: [Effect]) (es :: [Effect]) (r :: Type) = Handling
  {-# UNPACK #-} !(Env es)
  {-# UNPACK #-} !(Marker r)

-- | The type of effect handlers.
type Handler e es r = forall esSend a. e :> esSend => Handling esSend es r -> e (Eff esSend) a -> Eff esSend a

-- This "unsafe" IO function is perfectly safe in the sense that it won't panic or otherwise cause undefined
-- behaviors; it is only unsafe when it is used to embed arbitrary IO actions in any effect environment,
-- therefore breaking effect abstraction.
unsafeIO :: IO a -> Eff es a
unsafeIO m = Eff (const $ liftIO m)
{-# INLINE unsafeIO #-}

unsafeState :: s -> (IORef s -> Eff es a) -> Eff es a
unsafeState x0 f = Eff \es -> promptState x0 \ref -> unEff (f ref) es
{-# INLINE unsafeState #-}

toInternalHandler :: Marker r -> Env es -> Handler e es r -> InternalHandler e
toInternalHandler mark es hdl = InternalHandler \e -> hdl (Handling es mark) e
{-# INLINE toInternalHandler #-}

alter :: (Env es' -> Env es) -> Eff es a -> Eff es' a
alter f (Eff m) = Eff \es -> m (f es)
{-# INLINE alter #-}

handle :: Handler e es' a -> (InternalHandler e -> Env es' -> Env es) -> Eff es a -> Eff es' a
handle hdl f (Eff m) = Eff \es -> prompt \mark -> m (f (toInternalHandler mark es hdl) es)
{-# INLINE handle #-}

-- | Handle an effect.
interpret :: Handler e es a -> Eff (e : es) a -> Eff es a
interpret hdl = handle hdl Rec.cons
{-# INLINE interpret #-}

-- | Handle an effect with another newly introduced effect. This allows for effect encapsulation because it does not
-- require placing constraints on the original row.
reinterpret :: Handler e (e' : es) a -> Eff (e : es) a -> Eff (e' : es) a
reinterpret hdl = handle hdl \ih es -> Rec.cons ih $ Rec.tail es
{-# INLINE reinterpret #-}

-- | Handle an effect already in the environment. This is particularly useful in scoped effect operations.
interpose :: e :> es => Handler e es a -> Eff es a -> Eff es a
interpose hdl = handle hdl Rec.update
{-# INLINE interpose #-}

-- | Handle an effect already in the environment with another newly introduced effect.
reinterpose :: e :> es => Handler e (e' : es) a -> Eff es a -> Eff (e' : es) a
reinterpose hdl = handle hdl \ih es -> Rec.update ih $ Rec.tail es
{-# INLINE reinterpose #-}

-- | List a computation to a larger effect environment; it can also be thought as "masking" the outermost effect for
-- the computation.
lift :: Eff es a -> Eff (e : es) a
lift = alter Rec.tail
{-# INLINE lift #-}

-- | Perform an effect operation.
send :: e :> es => e (Eff es) a -> Eff es a
send e = Eff \es -> unEff (runHandler (Rec.index es) e) es
{-# INLINE send #-}

-- | A "localized computaton"; this is parameterized with an existential variable so the computation with this effect
-- cannot escape a certain scope.
data Localized (tag :: k) :: Effect

-- | Perform an operation from the handle-site.
embed :: Handling esSend es r -> Eff es a -> Eff esSend a
embed (Handling es _) (Eff m) = Eff (const $ m es)
{-# INLINE embed #-}

-- | Perform an operation from the handle-site, while being able to convert an operation from the perform-site to the
-- handle-site.
withUnembed :: Handling esSend es r -> (forall tag. (Eff esSend a -> Eff (Localized tag : es) a) -> Eff (Localized tag : es) a) -> Eff esSend a
withUnembed (Handling es _) f = Eff \esSend -> unEff (f \(Eff m) -> Eff (const $ m esSend)) (Rec.cons undefined es)
{-# INLINE withUnembed #-}

-- | Abort with a result value.
abort :: Handling esSend es r -> Eff es r -> Eff esSend a
abort (Handling es mark) (Eff m) = Eff (const $ raise mark (m es))
{-# INLINE abort #-}

-- | Yield and gain control of the resumption. The resumption cannot escape the scope of the controlling function.
control :: Handling esSend es r -> (forall tag. (Eff esSend a -> Eff (Localized tag : es) r) -> Eff (Localized tag : es) r) -> Eff esSend a
control (Handling es mark) f = Eff \esSend -> yield mark \cont -> unEff (f \(Eff x) -> Eff (const $ cont $ x esSend)) (Rec.cons undefined es)
{-# INLINE control #-}

-- | Unpack the 'Eff' monad.
runEff :: Eff '[] a -> a
runEff (Eff m) = unsafeDupablePerformIO (runCtl $ m Rec.empty)
{-# INLINE runEff #-}

-- | Ability to embed 'IO' side effects.
data IOE :: Effect

instance IOE :> es => MonadIO (Eff es) where
  liftIO = unsafeIO

-- | Unpack an 'Eff' monad with 'IO' acitons.
runIOE :: Eff '[IOE] a -> IO a
runIOE m = runCtl $ unEff (interpret (const $ \case) m) Rec.empty
{-# INLINE runIOE #-}
