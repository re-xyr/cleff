module Broker where

import           Cleff
import           Control.Concurrent (forkIO, threadDelay)
import           Control.Monad      (void)

nestedImpl :: Int -> (String -> (Bool -> IO ()) -> IO ()) -> IO ()
nestedImpl i cb = void . forkIO $ do
  threadDelay $ 1 * 1000 * 1000
  cb (show i) \b -> putStrLn $ "result " ++ show b

data Broker :: Effect where
  Subscribe :: Int -> (String -> m Bool) -> Broker m ()

subscribe :: Broker :> es => Int -> (String -> Eff es Bool) -> Eff es ()
subscribe channel = send . Subscribe channel

runBroker :: IOE :> es => Eff (Broker : es) a -> Eff es a
runBroker = interpret \case
  Subscribe channel cb -> withUnliftIO \unlift -> liftIO do
    putStrLn $ "Subscribe: " ++ show channel
    nestedImpl channel \s icb -> icb =<< unlift (cb s)

prog :: (IOE :> es, Broker :> es) => Eff es ()
prog = do
  subscribe 1 \_ -> pure True
  subscribe 2 \_ -> pure False
  liftIO $ threadDelay $ 3 * 1000 * 1000

runProg :: IO ()
runProg = runIOE $ runBroker prog
