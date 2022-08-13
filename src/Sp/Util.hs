{-# OPTIONS_GHC -Wno-orphans #-}
module Sp.Util
  ( -- * Reader
    Reader (..)
  , ask
  , local
  , runReader
    -- * State
  , State (..)
  , get
  , put
  , state
  , runState
    -- * Error
  , Error (..)
  , throw
  , catch
  , try
  , runError
    -- * Writer
  , Writer (..)
  , tell
  , listen
  , runWriter
    -- * Nondeterminism
  , NonDet (..)
  , choice
  , runNonDet
  ) where

import           Control.Applicative (Alternative (empty, (<|>)))
import           Data.Atomics        (atomicModifyIORefCAS, atomicModifyIORefCAS_)
import           Data.Foldable       (for_)
import           Data.IORef          (IORef, readIORef, writeIORef)
import           Data.Kind           (Type)
import           Data.Tuple          (swap)
import           Sp.Internal.Monad

data Reader (r :: Type) :: Effect where
  Ask :: Reader r m r
  Local :: (r -> r) -> m a -> Reader r m a

ask :: Reader r :> es => Eff es r
ask = send Ask

local :: Reader r :> es => (r -> r) -> Eff es a -> Eff es a
local f m = send (Local f m)

handleReader :: r -> Handler (Reader r) es a
handleReader r _ = \case
  Ask       -> pure r
  Local f m -> interpose (handleReader (f r)) m

runReader :: r -> Eff (Reader r : es) a -> Eff es a
runReader r = interpret (handleReader r)

data State s m a where
  Get :: State s m s
  Put :: s -> State s m ()
  State :: (s -> (a, s)) -> State s m a

get :: State s :> es => Eff es s
get = send Get

put :: State s :> es => s -> Eff es ()
put x = send (Put x)

state :: State s :> es => (s -> (a, s)) -> Eff es a
state f = send (State f)

handleState :: IORef s -> Handler (State s) es a
handleState r _ = \case
  Get     -> unsafeIO (readIORef r)
  Put s   -> unsafeIO (writeIORef r s)
  State f -> unsafeIO (atomicModifyIORefCAS r (swap . f))

runState :: s -> Eff (State s : es) a -> Eff es (a, s)
runState s m = unsafeState s \r -> do
  x <- interpret (handleState r) m
  s' <- unsafeIO (readIORef r)
  pure (x, s')

data Error (e :: Type) :: Effect where
  Throw :: e -> Error e m a
  Catch :: m a -> (e -> m a) -> Error e m a

throw :: Error e :> es => e -> Eff es a
throw e = send (Throw e)

catch :: Error e :> es => Eff es a -> (e -> Eff es a) -> Eff es a
catch m h = send (Catch m h)

try :: Error e :> es => Eff es a -> Eff es (Either e a)
try m = catch (Right <$> m) (pure . Left)

handleError :: forall e es a. Handler (Error e) es (Either e a)
handleError ctx = \case
  Throw e   -> abort ctx (pure $ Left e)
  Catch m f -> either f pure =<< interpose (handleError @e) (Right <$> m)

runError :: forall e es a. Eff (Error e : es) a -> Eff es (Either e a)
runError = interpret (handleError @e) . fmap Right

data Writer (w :: Type) :: Effect where
  Tell :: w -> Writer w m ()
  Listen :: m a -> Writer w m (a, w)

tell :: Writer w :> es => w -> Eff es ()
tell x = send (Tell x)

listen :: Writer w :> es => Eff es a -> Eff es (a, w)
listen m = send (Listen m)

handleWriter :: forall w es a. Monoid w => [IORef w] -> Handler (Writer w) es a
handleWriter rs _ = \case
  Tell x   -> for_ rs \r -> unsafeIO (atomicModifyIORefCAS_ r (<> x))
  Listen m -> unsafeState mempty \r' -> do
    x <- interpose (handleWriter (r' : rs)) m
    w' <- unsafeIO (readIORef r')
    pure (x, w')
{-# INLINABLE handleWriter #-}

runWriter :: Monoid w => Eff (Writer w : es) a -> Eff es (a, w)
runWriter m = unsafeState mempty \r -> do
  x <- interpret (handleWriter [r]) m
  w' <- unsafeIO (readIORef r)
  pure (x, w')
{-# INLINABLE runWriter #-}

data NonDet :: Effect where
  Empty :: NonDet m a
  Choice :: [a] -> NonDet m a
  -- Cut :: NonDet m ()
  -- Cull :: m a -> NonDet m a

choice :: NonDet :> es => [a] -> Eff es a
choice etc = send (Choice etc)

-- handleNonDet :: Alternative f => Handler NonDet es (Bool, f a)
-- handleNonDet ctx = \case
--   Empty -> abort ctx $ pure (False, empty)
--   Choice etc -> control ctx \cont ->
--     let collect [] acc = pure (False, acc)
--         collect (e : etc') acc = do
--           (isCut, xs) <- cont (pure e)
--           if isCut
--             then pure (True, acc <|> xs)
--             else collect etc' $! (acc <|> xs)
--     in collect etc empty
  -- Cut -> control ctx \cont -> first (const True) <$> cont (pure ())
  -- Cull m -> do
  --   (isCut, x) <- interpose handleNonDet $ fmap ((False, ) . Just) m
  --   control ctx \cont -> case x of
  --     Nothing -> pure (isCut, empty)
  --     Just x' -> first (isCut ||) <$> cont (pure x')
handleNonDet :: Alternative f => Handler NonDet es (f a)
handleNonDet ctx = \case
  Empty -> abort ctx $ pure empty
  Choice etc -> control ctx \cont ->
    let collect [] acc = pure acc
        collect (e : etc') acc = do
          xs <- cont (pure e)
          collect etc' $! (acc <|> xs)
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
