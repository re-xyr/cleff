{-# LANGUAGE DeriveAnyClass #-}
module EffectExample where

import           Control.Exception      (Exception)
import           Control.Monad          (unless)
import           Control.Monad.IO.Class (liftIO)
import           Data.Monoid            (Sum (Sum))
import           Effect
import           Effect.Error
import           Effect.Mask
import           Effect.Reader
import           Effect.State
import           Effect.Writer

data Dummy :: Effect where
  Dummy :: Dummy m ()

data Teletype :: Effect where
  ReadTTY :: Teletype m String
  WriteTTY :: String -> Teletype m ()
makeEffect ''Teletype

runTeletypeIO :: IOE :> es => Eff (Teletype ': es) a -> Eff es a
runTeletypeIO = interpret \case
  ReadTTY    -> liftIO getLine
  WriteTTY s -> liftIO $ putStrLn s

runTeletypePure :: [String] -> Eff (Teletype ': es) w -> Eff es [String]
runTeletypePure input = fmap snd . runLocalWriter . runLocalState input . reinterpret2 \case
  ReadTTY -> get >>= \case
    []     -> pure ""
    x : xs -> put xs >> pure x
  WriteTTY msg -> tell [msg]

echo :: (Teletype :> es) => Eff es ()
echo = do
  x <- readTTY
  unless (null x) $ do
    writeTTY x
    echo

echoPure :: [String] -> [String]
echoPure input = runPure $ runTeletypePure input echo

main :: IO ()
main = runIOE $ runTeletypeIO echo

runDummy :: (State [String] :> es) => Eff (Dummy ': es) w -> Eff es w
runDummy = interpret \case
  Dummy -> send (Put @[String] [])

test :: [String] -> ([String], [String]) -- writer, dummy
test xs = runPure $ runLocalState [] $ runDummy $ runTeletypePure xs echo

data CustomException = ThisException | ThatException deriving (Show, Exception)

program :: '[Mask, Teletype, Error CustomException] :>> es => Eff es ()
program = catchError @CustomException work \e -> writeTTY $ "Caught " ++ show e
  where
    work = bracket readTTY (const $ writeTTY "exiting bracket") \input -> do
      writeTTY "entering bracket"
      case input of
        "explode"     -> throwError ThisException
        "weird stuff" -> writeTTY input *> throwError ThatException
        _             -> writeTTY input *> writeTTY "no exceptions"

-- >>> test ["abc", "def", "ghci"]
-- (["abc","def","ghci"],[])

summation :: (Reader Integer :> es, Writer (Sum Integer) :> es) => Eff es ()
summation = do
  n <- ask
  if n == (0 :: Integer) then pure ()
  else do
    tell (Sum n)
    local (subtract (1 :: Integer)) summation

r :: IO ((), Sum Integer)
r = runIOE $ runReader (100 :: Integer) $ runLocalWriter @(Sum Integer) $ annoy summation

-- >>> r
-- ((),Sum {getSum = 5050})

annoy :: '[Reader Integer, IOE] :>> es => Eff es a -> Eff es a
annoy = interpose @(Reader Integer) \case
  Ask -> do
    liftIO $ putStrLn "tada"
    ask
  Local f m ->
    local f $ annoy $ runHere' @(Reader Integer) m

properBracketing :: IO ((), Integer)
properBracketing = runIOE $ runMask $ runLocalState 0 $ bracket_
  (pure ())
  (put @Integer 2)
  (const (pure ()))
