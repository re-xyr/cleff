-- | This module is adapted from https://github.com/polysemy-research/polysemy/blob/master/README.md,
-- originally BSD3 license, authors Sandy Maguire et al.
module Teletype where

import           Cleff
import           Cleff.Error
import           Cleff.Input
import           Cleff.Mask
import           Cleff.Output
import           Cleff.State
import           Control.Exception (Exception)
import           Control.Monad     (unless)
import           Data.Maybe        (fromMaybe)

-- * Effect

-- | An effect for reading and writing lines to a tty.
data Teletype :: Effect where
  ReadTTY :: Teletype m String
  WriteTTY :: String -> Teletype m ()

-- * Operations

makeEffect ''Teletype

-- * Interpretations

-- | Run 'Teletype' via stdio.
runTeletypeIO :: IOE :> es => Eff (Teletype ': es) a -> Eff es a
runTeletypeIO = interpretIO \case
  ReadTTY    -> getLine
  WriteTTY s -> putStrLn s

-- | Run 'Teletype' from a fixed input list.
runTeletypePure :: [String] -> Eff (Teletype ': es) w -> Eff es [String]
runTeletypePure tty = fmap (reverse . snd)
  . runState [] . outputToListState
  . runState tty . inputToListState
  . reinterpret2 \case
    ReadTTY      -> fromMaybe "" <$> input
    WriteTTY msg -> output msg

-- * Examples

-- | An echoing program.
echo :: Teletype :> es => Eff es ()
echo = do
  x <- readTTY
  unless (null x) $
    writeTTY x >> echo

-- | The pure interpretation of 'echo', via 'runTeletypePure'.
-- >>> echoPure ["abc", "def", "ghci"]
-- ["abc","def","ghci"]
echoPure :: [String] -> [String]
echoPure tty = runPure $ runTeletypePure tty echo

-- | The impure interpretation of 'echo', via 'runTeletypeIO'.
echoIO :: IO ()
echoIO = runIOE $ runTeletypeIO echo

data CustomException = ThisException | ThatException
  deriving stock (Show)
  deriving anyclass (Exception)

program :: '[Mask, Teletype, Error CustomException] :>> es => Eff es ()
program = catchError @CustomException work \e -> writeTTY $ "Caught " ++ show e
  where
    work = bracket readTTY (const $ writeTTY "exiting bracket") \next -> do
      writeTTY "entering bracket"
      case next of
        "explode"     -> throwError ThisException
        "weird stuff" -> writeTTY next *> throwError ThatException
        _             -> writeTTY next *> writeTTY "no exceptions"

main :: IO (Either CustomException ())
main = runIOE $ runMask $ runError @CustomException $ runTeletypeIO program
