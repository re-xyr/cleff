-- | This module is adapted from https://github.com/arybczak/effectful/blob/master/effectful/tests/TimeoutTests.hs,
-- originally BSD3 license, authors Andrzej Rybczak et al.
module TimeoutSpec where

import           Cleff
import           Cleff.Timeout
import           Control.Monad.IO.Class (liftIO)
import           GHC.Conc               (threadDelay)
import           Test.Hspec

spec :: Spec
spec = parallel $ describe "Timeout" do
  it "returns Just when timeout is not exceeded" do
    result <- runIOE $ runTimeout $ timeout 1000 $ return ()
    result `shouldBe` Just ()

  it "returns Nothing when timeout is exceeded" do
    result <- runIOE $ runTimeout $ timeout 0 $ liftIO $ threadDelay 1000
    result `shouldBe` Nothing
