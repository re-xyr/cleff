module Sp.Internal.Ctl (Marker, Ctl, unsafeIOCtl, prompt, yield, raise, runCtl, promptState) where

import           Control.Monad        (ap, liftM)
import           Data.Atomics.Counter (AtomicCounter, incrCounter, newCounter)
import           Data.IORef           (IORef, newIORef, readIORef, writeIORef)
import           Data.Type.Equality   (TestEquality (testEquality), type (:~:) (Refl))
import           System.IO.Unsafe     (unsafeDupablePerformIO, unsafePerformIO)
import           Unsafe.Coerce        (unsafeCoerce)

uniqueSource :: AtomicCounter
{-# NOINLINE uniqueSource #-}
uniqueSource = unsafePerformIO (newCounter 0)

freshMarker :: forall a. IO (Marker a)
freshMarker = Marker <$> incrCounter 1 uniqueSource

newtype Marker a = Marker Int

instance TestEquality Marker where
  testEquality (Marker l) (Marker r) =
    if l == r then Just (unsafeCoerce Refl) else Nothing

data Ctl a
  = Pure !a
  | forall r. Raise !(Marker r) !r
  | forall r b. Yield !(Marker r) !((b -> Ctl r) -> Ctl r) !(b -> Ctl a)

instance Functor Ctl where
  fmap = liftM

instance Applicative Ctl where
  pure = Pure
  (<*>) = ap

instance Monad Ctl where
  Pure x >>= f              = f x
  Raise mark r >>= _        = Raise mark r
  Yield mark ctl cont >>= f = Yield mark ctl (f `compose` cont)

compose :: (b -> Ctl c) -> (a -> Ctl b) -> a -> Ctl c
g `compose` f = \x -> case f x of
  Pure a              -> g a
  Raise mark r        -> Raise mark r
  Yield mark ctl cont -> Yield mark ctl (g `compose` cont)

unsafeIOCtl :: IO a -> Ctl a
unsafeIOCtl m = case unsafeDupablePerformIO m of
  x -> Pure x

prompt :: forall a. (Marker a -> Ctl a) -> Ctl a
prompt f = do
  mark <- unsafeIOCtl (freshMarker @a)
  promptWith mark (f mark)

promptWith :: Marker a -> Ctl a -> Ctl a
promptWith mark m = case m of
  Pure a -> Pure a
  Raise mark' r -> case testEquality mark mark' of
    Just Refl -> Pure r
    Nothing   -> Raise mark' r
  Yield mark' ctl cont -> case testEquality mark mark' of
    Just Refl -> ctl (promptWith mark . cont)
    Nothing   -> Yield mark' ctl (promptWith mark . cont)

yield :: Marker r -> ((a -> Ctl r) -> Ctl r) -> Ctl a
yield mark f = Yield mark f pure

raise :: Marker r -> r -> Ctl a
raise mark r = Raise mark r

promptState :: forall s r. s -> (IORef s -> Ctl r) -> Ctl r
promptState x0 f = do
  ref <- unsafeIOCtl (newIORef x0)
  promptStateWith ref (f ref)

promptStateWith :: IORef s -> Ctl r -> Ctl r
promptStateWith ref m = case m of
  Pure x -> Pure x
  Raise mark x -> Raise mark x
  Yield mark ctl cont -> do
    s0 <- unsafeIOCtl (readIORef ref)
    Yield mark ctl \x -> do
      unsafeIOCtl (writeIORef ref s0)
      promptStateWith ref (cont x)

runCtl :: Ctl a -> a
runCtl = \case
  Pure a   -> a
  Raise {} -> error "Unhandled operation"
  Yield {} -> error "Unhandled operation"
