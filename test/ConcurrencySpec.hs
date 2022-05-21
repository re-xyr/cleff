module ConcurrencySpec where

import           Cleff
import           Cleff.Error         (runError, throwError)
import           Cleff.State
import           Control.Monad       (void, when)
import           Data.Set            (Set)
import qualified Data.Set            as Set
import           Test.Hspec
import           UnliftIO            (concurrently_, replicateConcurrently_)
import           UnliftIO.Concurrent (threadDelay)

spec :: Spec
spec = do
  sharedState
  localState
  errorHandling

sharedState :: Spec
sharedState = it "should have shared state" do
  (_, set) <- runIOE $ runState (Set.empty @Int) do
    concurrently_ (addWhen even x) (addWhen odd x)
  set `shouldBe` Set.fromList [1..x]
  where
    x :: Int = 100
    addWhen :: State (Set Int) :> es => (Int -> Bool) -> Int -> Eff es ()
    addWhen f = \case
      0 -> pure ()
      n -> do
        when (f n) $ do
          modify $ Set.insert n
        addWhen f $ n - 1

localState :: Spec
localState = it "should have local state" $
  void $ runIOE $ runStateLocal x $ do
    replicateConcurrently_ 2 $ do
      r <- goDownward 0
      liftIO $ r `shouldBe` x
  where
    x :: Int
    x = 100000

    goDownward :: State Int :> es => Int -> Eff es Int
    goDownward acc = do
      end <- state @Int $ \case
        0 -> (True,  0)
        n -> (False, n - 1)
      if end
        then pure acc
        else goDownward $ acc + 1

errorHandling :: Spec
errorHandling = it "should handle errors properly" do
  (r, s) <- runIOE $ runState (0 :: Int) $ runError @String $ concurrently_
    (liftIO (threadDelay 10000) >> throwError err)
    (modify (+x))
  case r of
    Left e  -> e `shouldBe` err
    Right _ -> expectationFailure "no error caught - or error escaped"
  s `shouldBe` x
  where
    x :: Int = 67
    err = "error"
