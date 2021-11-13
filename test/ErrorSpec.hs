-- | This module is adapted from https://github.com/polysemy-research/polysemy/blob/master/test/ErrorSpec.hs,
-- originally BSD3 license, authors Sandy Maguire et al.
module ErrorSpec where

import           Cleff
import           Cleff.Error
import           Cleff.Fail
import           Cleff.Mask
import           Control.Monad.Fail (fail)
import           Prelude            hiding (fail)
import           Test.Hspec
import qualified UnliftIO.Exception as Exc

newtype MyExc = MyExc String
  deriving stock (Show, Eq)
  deriving anyclass (Exc.Exception)

spec :: Spec
spec = parallel do
  it "should catch exceptions" do
    a <- runIOE $ runError $ fromException @MyExc do
      _ <- Exc.throwIO $ MyExc "hello"
      pure ()
    a `shouldBe` Left (MyExc "hello")

  it "should not catch non-exceptions" do
    a <- runIOE $ runError @MyExc $ fromException @MyExc $ pure ()
    a `shouldBe` Right ()

  it "should interact well with Mask" do
    a <- runIOE $ runMask $ runError @MyExc $ onError (do
      _ <- throwError $ MyExc "hello"
      pure ()) $ throwError (MyExc "goodbye")
    a `shouldBe` Left (MyExc "hello")

  it "should not catch prematurely" do
    b <- runIOE $ runFail $ runError @String $ fail "Boom" >> pure ()
    b `shouldBe` Left "Boom"
