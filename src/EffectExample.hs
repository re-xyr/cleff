module EffectExample where

import           Control.Monad (unless)
import           Data.Monoid   (Sum (Sum))
import           Effect
import           Effect.IO
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
  ReadTTY    -> send (Lift getLine)
  WriteTTY s -> send (Lift $ putStrLn s)

runTeletypePure :: (State [String] :> es, Writer [String] :> es) => Eff (Teletype ': es) w -> Eff es w
runTeletypePure = interpret \case
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

test :: [String] -> ((((), [String]), [String]), [String]) -- state, writer, dummy
test xs = runPure $ runLocalState [] $ runDummy $ runLocalWriter $ runLocalState xs $ runTeletypePure echo

-- >>> test ["abc", "def", "ghci"]
-- ((((),[]),["abc","def","ghci"]),[])

wowwee :: (Reader Integer :> es, Writer (Sum Integer) :> es) => Eff es ()
wowwee = do
  n <- ask
  if n == (0 :: Integer) then pure ()
  else do
    tell (Sum n)
    local (subtract (1 :: Integer)) wowwee

-- >>> runPure $ runReader 3 $ runLocalWriter @(Sum Integer) wowwee
-- ((),Sum {getSum = 6})
