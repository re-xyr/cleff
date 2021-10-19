-- | This module is adapted from https://github.com/polysemy-research/polysemy/blob/master/test/ResourceSpec.hs,
-- originally BSD3 license, authors Sandy Maguire et al.
{-# OPTIONS_GHC -Wno-orphans #-}
module MaskSpec where

import           Cleff
import           Cleff.Error
import           Cleff.Mask
import           Cleff.Output
import           Cleff.State
import           Cleff.Trace
import           Cleff.Writer
import           Control.Exception (Exception)
import           Control.Monad     (void)
import           Data.Tuple.Extra  (second)
import           Test.Hspec

spec :: Spec
spec = parallel $ do
  testBoth "persist state and call the finalizer"
      (\((e, s), ts) -> do
        s `shouldBe` "finalized"
        e `shouldBe` Left ()
        ts `shouldBe` ["allocated", "starting block"]
      ) $ do
    bracket
      (put "allocated" >> pure ())
      (\() -> do
        get >>= trace
        put "finalized"
      )
      (\() -> do
        get >>= trace
        put "starting block"
        _ <- throwError ()
        put "don't get here"
      )

  testBoth "persist state and call the finalizer with bracketOnError"
      (\((e, s), ts) -> do
        ts `shouldContain` ["allocated"]
        ts `shouldContain` ["starting block"]
        s `shouldBe` "finalized"
        e `shouldBe` Left ()
      ) $ do
    bracketOnError
      (put "allocated" >> pure ())
      (\() -> do
        get >>= trace
        put "finalized"
      )
      (\() -> do
        get >>= trace
        put "starting block"
        _ <- throwError ()
        put "don't get here"
      )

  testBoth "should not call the finalizer if there no error"
      (\((e, s), ts) -> do
        ts `shouldContain` ["allocated"]
        ts `shouldNotContain` ["starting block"]
        s `shouldBe` "don't get here"
        e `shouldBe` Right ()
      ) $ do
    bracketOnError
      (put "allocated" >> pure ())
      (\() -> do
        get >>= trace
        put "finalized"
      )
      (\() -> do
        get >>= trace
        put "starting block"
        put "don't get here"
      )

  testBoth "should call the finalizer on Error"
      (\((e, s), ts) -> do
        ts `shouldContain` ["beginning transaction"]
        ts `shouldContain` ["rolling back transaction"]
        s `shouldBe` ""
        e `shouldBe` Left ()
      ) $ do
    withTransaction $ do
      void $ throwError ()
      pure "hello"

  testBoth "io dispatched bracket"
      (\((e, s), ts) -> do
        ts `shouldContain` ["allocated"]
        ts `shouldContain` ["starting block"]
        s `shouldBe` "finalized"
        e `shouldBe` Left ()
      ) $ do
    bracket
      (put "allocated" >> pure ())
      (\() -> do
        get >>= trace
        put "finalized"
      )
      (\() -> do
        get >>= trace
        put "starting block"
        _ <- throwError ()
        put "don't get here"
      )

  testBoth "should not lock when done recursively"
      (\((e, s), ts) -> do
        ts `shouldContain` [ "hello 1"
                           , "hello 2"
                           , "RUNNING"
                           , "goodbye 2"
                           ]
        s `shouldBe` "finished"
        e `shouldBe` Left ()
      ) $ do
    bracket
      (put "hello 1")
      (\() -> do
        get >>= trace
        put "finished"
      )
      (\() -> do
        get >>= trace
        void $
          bracket (put "hello 2")
                  (const $ do
                    get >>= trace
                    put "goodbye 2"
                  )
                  (const $ do
                    get >>= trace
                    put "RUNNING"
                    throwError ()
                  )
        -- This doesn't run due to the thrown error above
        get >>= trace
        put "goodbye 1"
      )

instance Exception ()

runTest
  :: Eff '[Error (), Mask, State [Char], Trace, Output String] a
  -> IO ((Either () a, [Char]), [String])
runTest = pure
        . runPure
        . fmap (second reverse) . runState []
        . outputToListState
        . subsume @(Output String)
        . traceToOutput
        . runState ""
        . runMask
        . runError @()

runTest2
  :: Eff '[Error (), Mask, State [Char], Trace, Output String] a
  -> IO ((Either () a, [Char]), [String])
runTest2 = pure
         . runPure
         . runWriter
         . outputToWriter (:[])
         . subsume @(Output String)
         . traceToOutput
         . runState ""
         . runMask
         . runError @()

testBoth
    :: String
    -> (((Either () a, [Char]), [String]) -> Expectation)
    -> Eff '[Error (), Mask, State [Char], Trace, Output String] a
    -> Spec
testBoth name k m = do
  describe name $ do
    it "via outputToListState" $ do
      z <- runTest m
      k z
    it "via outputToWriter" $ do
      z <- runTest2 m
      k z

withTransaction :: '[Mask, Trace] :>> r => Eff r a -> Eff r a
withTransaction m =
  bracketOnError
    (trace "beginning transaction")
    (const $ trace "rolling back transaction")
    (const $ m <* trace "committing transaction")
