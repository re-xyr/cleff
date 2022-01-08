-- | This module is adapted from https://github.com/polysemy-research/polysemy/blob/master/test/HigherOrderSpec.hs,
-- originally BSD3 license, authors Sandy Maguire et al.
module HigherOrderSpec where

import           Cleff
import           Cleff.Error
import           Cleff.Reader
import           Test.Hspec

data SomeEff :: Effect where
  SomeAction :: SomeEff m String
makeEffect ''SomeEff

data Ex = Ex deriving (Eq, Show)

spec :: Spec
spec = describe "Reader local" $ do
  it "should nest with itself" $ do
    let foo = runPure . runReader "hello" $ do
                local (++ " world") $ do
                  local (++ "!") $ do
                    ask
    foo `shouldBe` "hello world!"

  it "should local for other interpreted effects" do
    let
      localed = runPure $ runReader "unlocaled" $ interpret (\SomeAction -> ask) do
        local (const "localed") someAction
    localed `shouldBe` "localed"

  it "should catch errors indirectly thrown from interpreted effects" do
    let
      caught = runPure $ runError @Ex $ interpret (\SomeAction -> throwError Ex) do
        someAction `catchError` \Ex -> return "caught"
    caught `shouldBe` Right "caught"
