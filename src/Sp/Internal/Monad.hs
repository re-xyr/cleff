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
  , fromEff
  , withToEff
  , control
  , abort
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

type Effect = (Type -> Type) -> Type -> Type

type Env = Rec InternalHandler

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

type role Handling nominal nominal representational
data Handling (esSend :: [Effect]) (es :: [Effect]) (r :: Type) = Handling
  {-# UNPACK #-} !(Env es)
  {-# UNPACK #-} !(Marker r)

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

interpret :: Handler e es a -> Eff (e : es) a -> Eff es a
interpret hdl = handle hdl Rec.cons
{-# INLINE interpret #-}

reinterpret :: Handler e (e' : es) a -> Eff (e : es) a -> Eff (e' : es) a
reinterpret hdl = handle hdl \ih es -> Rec.cons ih $ Rec.tail es
{-# INLINE reinterpret #-}

interpose :: e :> es => Handler e es a -> Eff es a -> Eff es a
interpose hdl = handle hdl Rec.update
{-# INLINE interpose #-}

reinterpose :: e :> es => Handler e (e' : es) a -> Eff es a -> Eff (e' : es) a
reinterpose hdl = handle hdl \ih es -> Rec.update ih $ Rec.tail es
{-# INLINE reinterpose #-}

lift :: Eff es a -> Eff (e : es) a
lift = alter Rec.tail
{-# INLINE lift #-}

send :: e :> es => e (Eff es) a -> Eff es a
send e = Eff \es -> unEff (runHandler (Rec.index es) e) es
{-# INLINE send #-}

data Localized (tag :: k) :: Effect

fromEff :: Handling esSend es r -> Eff es a -> Eff esSend a
fromEff (Handling es _) = alter (const es)
{-# INLINE fromEff #-}

withToEff :: Handling esSend es r -> (forall tag. (Eff esSend a -> Eff (Localized tag : es) a) -> Eff (Localized tag : es) a) -> Eff esSend a
withToEff (Handling es _) f = Eff \esSend -> unEff (f \(Eff m) -> Eff (const $ m esSend)) (Rec.cons undefined es)
{-# INLINE withToEff #-}

control :: Handling esSend es r -> (forall tag. (Eff esSend a -> Eff (Localized tag : es) r) -> Eff (Localized tag : es) r) -> Eff esSend a
control (Handling es mark) f = Eff \esSend -> yield mark \cont -> unEff (f \(Eff x) -> Eff (const $ cont $ x esSend)) (Rec.cons undefined es)
{-# INLINE control #-}

abort :: Handling esSend es r -> r -> Eff esSend a
abort (Handling _ mark) x = Eff (const $ raise mark x)
{-# INLINE abort #-}

runEff :: Eff '[] a -> a
runEff (Eff m) = unsafeDupablePerformIO (runCtl $ m Rec.empty)
{-# INLINE runEff #-}

data IOE :: Effect

instance IOE :> es => MonadIO (Eff es) where
  liftIO = unsafeIO

runIOE :: Eff '[IOE] a -> IO a
runIOE m = runCtl $ unEff (interpret (const $ \case) m) Rec.empty
{-# INLINE runIOE #-}

-- guard :: Env es -> Ctl a -> Eff es a
-- guard es m = Eff \es' -> if es == es' then m
--   else error "Sp.Eff: continuation used in a context other than its belonging scope"
