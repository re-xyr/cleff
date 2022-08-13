module Sp.Internal.Ctl (Marker, Ctl, prompt, yield, raise, promptState, runCtl) where

import           Control.Monad          (ap, liftM)
import           Control.Monad.IO.Class (MonadIO (liftIO))
import           Data.Atomics.Counter   (AtomicCounter, incrCounter, newCounter)
import           Data.IORef             (IORef, newIORef, readIORef, writeIORef)
import           Data.Kind              (Type)
import           Data.Type.Equality     (TestEquality (testEquality), type (:~:) (Refl))
import           System.IO.Unsafe       (unsafePerformIO)
import           Unsafe.Coerce          (unsafeCoerce)

uniqueSource :: AtomicCounter
{-# NOINLINE uniqueSource #-}
uniqueSource = unsafePerformIO (newCounter 0)

freshMarker :: forall a. Ctl (Marker a)
freshMarker = liftIO $ Marker <$> incrCounter 1 uniqueSource

type role Marker representational
newtype Marker (a :: Type) = Marker Int

instance TestEquality Marker where
  testEquality (Marker l) (Marker r) =
    if l == r then Just (unsafeCoerce Refl) else Nothing

-- We don't force the value because that makes the semantics nonstandard
-- Plus lazy semantics seems to make benchmarks faster
type role Result representational
data Result (a :: Type)
  = Pure a
  | forall (r :: Type). Raise !(Marker r) (Ctl r)
  | forall (r :: Type) (b :: Type). Yield !(Marker r) ((Ctl b -> Ctl r) -> Ctl r) (Ctl b -> Ctl a)

type role Ctl representational
newtype Ctl (a :: Type) = Ctl { unCtl :: IO (Result a) }

instance Functor Ctl where
  fmap = liftM

instance Applicative Ctl where
  pure = Ctl . pure . Pure
  (<*>) = ap

instance Monad Ctl where
  (Ctl x) >>= f = Ctl $ x >>= \case
    Pure a              -> unCtl (f a)
    Raise mark r        -> pure (Raise mark r)
    Yield mark ctl cont -> pure $ Yield mark ctl (f `compose` cont)

compose :: (b -> Ctl c) -> (a -> Ctl b) -> a -> Ctl c
g `compose` f = \x -> Ctl $ unCtl (f x) >>= \case
  Pure a              -> unCtl (g a)
  Raise mark r        -> pure (Raise mark r)
  Yield mark ctl cont -> pure $ Yield mark ctl (g `compose` cont)

prompt :: forall a. (Marker a -> Ctl a) -> Ctl a
prompt f = do
  mark <- freshMarker @a
  promptWith mark (f mark)

promptWith :: Marker a -> Ctl a -> Ctl a
promptWith mark m = Ctl $ unCtl m >>= \case
  Pure a -> pure $ Pure a
  Raise mark' r -> case testEquality mark mark' of
    Just Refl -> unCtl r
    Nothing   -> pure $ Raise mark' r
  Yield mark' ctl cont -> case testEquality mark mark' of
    Just Refl -> unCtl $ ctl (promptWith mark . cont)
    Nothing   -> pure $ Yield mark' ctl (promptWith mark . cont)

-- yielding is not strict in f
yield :: Marker r -> ((Ctl a -> Ctl r) -> Ctl r) -> Ctl a
yield mark f = Ctl $ pure $ Yield mark f id

-- raising is not strict in r
raise :: Marker r -> Ctl r -> Ctl a
raise mark r = Ctl $ pure $ Raise mark r

promptState :: forall s r. s -> (IORef s -> Ctl r) -> Ctl r
promptState x0 f = do
  ref <- liftIO (newIORef x0)
  promptStateWith ref (f ref)

promptStateWith :: IORef s -> Ctl r -> Ctl r
promptStateWith ref (Ctl m) = Ctl $ m >>= \case
  Pure x -> pure $ Pure x
  Raise mark x -> pure $ Raise mark x
  Yield mark ctl cont -> do
    s0 <- liftIO (readIORef ref)
    pure $ Yield mark ctl \x -> do
      liftIO (writeIORef ref s0)
      promptStateWith ref (cont x)

runCtl :: Ctl a -> IO a
runCtl (Ctl m) = m >>= \case
  Pure a   -> pure a
  Raise {} -> error "Sp.Ctl: Unhandled raise operation. Forgot to pair it with a prompt?"
  Yield {} -> error "Sp.Ctl: Unhandled yield operation. Forgot to pair it with a prompt?"

instance MonadIO Ctl where
  liftIO = Ctl . fmap Pure
