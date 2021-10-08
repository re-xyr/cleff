module EffectExample where

import           Control.Monad          (unless)
import           Control.Monad.IO.Class (liftIO)
import           Data.Monoid            (Sum (Sum))
import           Effect
import           Effect.Mask
import           Effect.Reader
import           Effect.State
import           Effect.Writer

data Teletype :: Effect where
  ReadTTY :: Teletype m String
  WriteTTY :: String -> Teletype m ()

data Dummy :: Effect where
  Dummy :: Dummy m ()

runTeletypeIO :: IOE :> es => Eff (Teletype ': es) a -> Eff es a
runTeletypeIO = interpret \case
  ReadTTY    -> liftIO getLine
  WriteTTY s -> liftIO $ putStrLn s

runTeletypePure :: [String] -> Eff (Teletype ': es) w -> Eff es [String]
runTeletypePure input = fmap snd . runLocalWriter . runLocalState input . reinterpret2 \case
  ReadTTY -> send Get >>= \case
    []     -> pure ""
    x : xs -> send (Put xs) >> pure x
  WriteTTY msg -> send (Tell [msg])

runDummy :: (State [String] :> es) => Eff (Dummy ': es) w -> Eff es w
runDummy = interpret \case
  Dummy -> send (Put @[String] [])

echo :: (Teletype :> es, Dummy :> es) => Eff es ()
echo = do
  x <- send ReadTTY
  unless (null x) $ do
    send $ WriteTTY x
    send Dummy
    echo

test :: [String] -> ([String], [String]) -- writer, dummy
test xs = runPure $ runLocalState [] $ runDummy $ runTeletypePure xs echo

-- >>> test ["abc", "def", "ghci"]

wowwee :: (Reader Integer :> es, Writer (Sum Integer) :> es) => Eff es ()
wowwee = do
  n <- ask
  if n == (0 :: Integer) then pure ()
  else do
    tell (Sum n)
    local (subtract (1 :: Integer)) wowwee

r :: IO ((), Sum Integer)
r = runIOE $ runReader (100 :: Integer) $ runLocalWriter @(Sum Integer) $ shit wowwee

-- >>> r

inner :: Reader Integer :> es => Eff es Integer
inner = ask

data Simple :: Effect where
  SimpleGet :: Simple m Integer
  Noop :: m a -> Simple m a

normal :: Eff (Simple ': es) a -> Eff (Reader Integer ': es) a
normal = reinterpretH \h -> \case
  SimpleGet -> ask
  Noop m    -> reinterpret h $ runHere m

outer :: '[Simple, Reader Integer] :>> es => Eff es (Integer, Integer, Integer)
outer = do
  x <- ask
  y <- send SimpleGet
  z <- send $ Noop ask
  pure (x, y, z)

-- >>> runPure $ runReader (2 :: Integer) $ bogus $ runReader (1 :: Integer) outer

shit :: '[Reader Integer, IOE] :>> es => Eff es a -> Eff es a
shit = interposeH @(Reader Integer) \handler -> \case
  Ask -> do
    liftIO $ putStrLn "tada"
    ask
  Local f m -> do
    local f $ interpose handler $ runHere m

aha :: IO ((), Integer)
aha = runIOE $ runMask $ runLocalState 0 $ bracket_
  (pure ())
  (put @Integer 2)
  (const (pure ()))
