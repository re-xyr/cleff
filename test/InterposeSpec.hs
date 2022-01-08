module InterposeSpec where

import           Cleff
import           Cleff.Output
import           Cleff.Reader
import           Cleff.State
import           Cleff.Trace
import           Test.Hspec

annoy :: forall es. '[Reader Int, Trace] :>> es => Eff es ~> Eff es
annoy = interpose @(Reader Int) h
  where
    h :: Handler (Reader Int) es
    h = \case
      Ask -> do
        x <- ask
        trace $ show x
        pure x
      Local f m -> local f (runThere m)

countdown :: Reader Int :> es => Eff es ()
countdown = do
  x <- asks (== (0 :: Int))
  if x then pure () else local (subtract (1 :: Int)) countdown

spec :: Spec
spec = do
  it "should thread in effect environment correctly" do
    let (_, msgs) = runPure $ runState [] $ outputToListState $ traceToOutput $ runReader (100 :: Int) $ annoy countdown
    msgs `shouldBe` map show [0..100 :: Int]
