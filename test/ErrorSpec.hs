module ErrorSpec where

import           Effect
import           Effect.Error
import           Effect.Mask
import           Test.Hspec
import qualified UnliftIO.Exception as Exc

newtype MyExc = MyExc String
  deriving stock (Show, Eq)
  deriving anyclass (Exc.Exception)

spec :: Spec
spec = parallel $ describe "Error" do
  -- it "should catch exceptions" do
  --   a <- runIOE $ runError $ fromException @MyExc do
  --     _ <- Exc.throwIO $ MyExc "hello"
  --     pure ()
  --   a `shouldBe` Left (MyExc "hello")
  -- it "should not catch non-exceptions" do
  --   a <- runIOE $ runError @MyExc $ fromException @MyExc $ pure ()
  --   a `shouldBe` Right ()
  it "should interact well with Mask" do
    a <- runIOE $ runMask $ runError @MyExc $ onError (do
      _ <- throwError $ MyExc "hello"
      pure ()) $ throwError (MyExc "goodbye")
    a `shouldBe` Left (MyExc "hello")
