module Main where

import           Broker (runProg)

main :: IO ()
main = do
  putStrLn "This is cleff's example module! Browse /example to get started with the library."
  runProg
