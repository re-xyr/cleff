module ConcurrencySpec where

import           Cleff
import           Cleff.Error         (runError, throwError)
import           Cleff.State
import           Control.Monad       (when)
import           Data.Set            (Set)
import qualified Data.Set            as Set
import           Test.Hspec
import           UnliftIO            (concurrently_)
import           UnliftIO.Concurrent (threadDelay)

spec :: Spec
spec = do
  sharedState
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
