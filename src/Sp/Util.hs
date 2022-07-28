{-# OPTIONS_GHC -Wno-orphans #-}
module Sp.Util
  ( Reader (..)
  , ask
  , local
  , runReader
  , State (..)
  , get
  , put
  , runState
  , Error (..)
  , throw
  , catch
  , runError
  , Writer (..)
  , tell
  , listen
  , runWriter
  , NonDet (..)
  , choice
  , runNonDet
  ) where

import           Control.Applicative (Alternative (empty, (<|>)))
import           Data.Atomics        (atomicModifyIORefCAS_)
import           Data.Foldable       (for_)
import           Data.IORef          (IORef, newIORef, readIORef, writeIORef)
import           Sp.Internal.Monad

data Reader r m a where
  Ask :: Reader r m r
  Local :: (r -> r) -> m a -> Reader r m a

ask :: Reader r :> es => Eff es r
ask = send Ask

local :: Reader r :> es => (r -> r) -> Eff es a -> Eff es a
local f m = send (Local f m)

handleReader :: r -> Handler (Reader r) es a
handleReader r ctx = \case
  Ask       -> pure r
  Local f m -> toEff ctx (interpose (handleReader (f r)) m)

runReader :: r -> Eff (Reader r : es) a -> Eff es a
runReader r = interpret (handleReader r)

data State s m a where
  Get :: State s m s
  Put :: s -> State s m ()

get :: State s :> es => Eff es s
get = send Get

put :: State s :> es => s -> Eff es ()
put x = send (Put x)

handleState :: IORef s -> Handler (State s) es a
handleState r _ = \case
  Get   -> unsafeIO (readIORef r)
  Put s -> unsafeIO (writeIORef r s)

runState :: s -> Eff (State s : es) a -> Eff es (a, s)
runState s m = do
  r <- unsafeIO (newIORef s)
  x <- interpret (handleState r) m
  s' <- unsafeIO (readIORef r)
  pure (x, s')

data Error e m a where
  Throw :: e -> Error e m a
  Catch :: m a -> (e -> m a) -> Error e m a

throw :: Error e :> es => e -> Eff es a
throw e = send (Throw e)

catch :: Error e :> es => Eff es a -> (e -> Eff es a) -> Eff es a
catch m h = send (Catch m h)

handleError :: forall e es a. Handler (Error e) es (Either e a)
handleError ctx = \case
  Throw e   -> abort ctx (Left e)
  Catch m f -> toEff ctx (either f pure =<< interpose (handleError @e) (Right <$> m))

runError :: forall e es a. Eff (Error e : es) a -> Eff es (Either e a)
runError = interpret (handleError @e) . fmap Right

data Writer w m a where
  Tell :: w -> Writer w m ()
  Listen :: m a -> Writer w m (a, w)

tell :: Writer w :> es => w -> Eff es ()
tell x = send (Tell x)

listen :: Writer w :> es => Eff es a -> Eff es (a, w)
listen m = send (Listen m)

handleWriter :: forall w es a. Monoid w => [IORef w] -> Handler (Writer w) es a
handleWriter rs ctx = \case
  Tell x   -> for_ rs \r -> unsafeIO (atomicModifyIORefCAS_ r (<> x))
  Listen m -> do
    r' <- unsafeIO (newIORef mempty)
    x <- toEff ctx (interpose (handleWriter (r' : rs)) m)
    w' <- unsafeIO (readIORef r')
    pure (x, w')
{-# INLINABLE handleWriter #-}

runWriter :: Monoid w => Eff (Writer w : es) a -> Eff es (a, w)
runWriter m = do
  r <- unsafeIO (newIORef mempty)
  x <- interpret (handleWriter [r]) m
  w' <- unsafeIO (readIORef r)
  pure (x, w')
{-# INLINABLE runWriter #-}

data NonDet m a where
  Empty :: NonDet m a
  Choice :: [a] -> NonDet m a

choice :: NonDet :> es => [a] -> Eff es a
choice etc = send (Choice etc)

handleNonDet :: Alternative f => Handler NonDet es (f a)
handleNonDet ctx = \case
  Empty      -> abort ctx empty
  Choice etc -> control ctx \cont ->
    let collect [] acc = pure acc
        collect (e : etc') acc = do
          xs <- cont e
          collect etc' $! (xs <|> acc)
    in collect etc empty
{-# INLINABLE handleNonDet #-}

runNonDet :: Alternative f => Eff (NonDet : es) a -> Eff es (f a)
runNonDet = interpret handleNonDet . fmap pure
{-# INLINABLE runNonDet #-}

instance NonDet :> es => Alternative (Eff es) where
  empty = send Empty
  m <|> n = do
    x <- send (Choice [True, False])
    if x then m else n
