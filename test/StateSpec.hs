module StateSpec where

import           Cleff
import           Cleff.State
import qualified Control.Exception.Lifted as LE
import qualified Control.Monad.Catch      as E
import           Test.Hspec
import           UnliftIO.Exception
import qualified UnliftIO.Exception       as UE

spec :: Spec
spec = describe "State" do
  it "should run with correct results" basic
  it "should run in a deep stack" deepStack
  it "should interact well with exceptions" exceptionInteract
  it "should run in nested cases" nested

basic, deepStack, exceptionInteract, nested :: IO ()

basic = do
  (end, len) <- runIOE . runLocalState (0::Int) . fmap snd . runLocalState collatzStart $ collatz
  end `shouldBe` 1
  len `shouldBe` collatzLength

deepStack = do
  n <- runIOE . fmap fst . runLocalState () . fmap snd . runLocalState (0::Int) $ do
    fmap fst . runLocalState () . fmap fst . runLocalState () $ do
      fmap fst . runLocalState () $ do
        fmap fst . runLocalState () . fmap fst . runLocalState () . fmap fst . runLocalState () $ do
          modify @Int (+1)
        modify @Int (+2)
      modify @Int (+4)
    modify @Int (+8)
  n `shouldBe` 15

exceptionInteract = do
  testTry   E.try
  testCatch E.catch
  testTry   LE.try
  testCatch LE.catch
  testTry   UE.try
  testCatch UE.catch
  where
    testTry
      :: (forall a es. IOE :> es => Eff es a -> Eff es (Either Ex a))
      -> IO ()
    testTry tryImpl = do
      e <- runIOE $ tryImpl $ runLocalState (0::Int) action
      e `shouldBe` Left Ex
      s <- runIOE $ fmap snd $ runLocalState (0::Int) $ tryImpl action
      s `shouldBe` 1
    testCatch
      :: (forall a es. IOE :> es => Eff es a -> (Ex -> Eff es a) -> Eff es a)
      -> IO ()
    testCatch catchImpl = do
      s <- runIOE . fmap snd . runLocalState (0::Int) $ do
        _ <- (fmap fst . runLocalState () $ action) `catchImpl` \Ex -> modify @Int (+4)
        modify @Int (+8)
      s `shouldBe` 13
    action :: '[State Int, IOE] :>> es => Eff es ()
    action = do
      modify @Int (+1)
      _ <- throwIO Ex
      modify @Int (+2)

nested = do
  x <- runIOE do
    runHasInt 0 do
      putInt 1
      fmap snd . runLocalState () $ do
        putInt 2
        fmap snd . runLocalState () $ do
          putInt expected
      getInt
  x `shouldBe` expected
  where
    expected :: Int
    expected = 4

data HasInt :: Effect where
  GetInt :: HasInt m Int
  PutInt :: Int -> HasInt m ()

getInt :: HasInt :> es => Eff es Int
getInt = send GetInt

putInt :: HasInt :> es => Int -> Eff es ()
putInt = send . PutInt

runHasInt :: Int -> Eff (HasInt : es) a -> Eff es a
runHasInt n =
  fmap fst . runLocalState () . fmap fst . runLocalState n . fmap fst . runLocalState True . reinterpret3 \case
    GetInt   -> get
    PutInt i -> put i

data Ex = Ex deriving (Eq, Show, Exception)

collatzStart :: Integer
collatzStart = 9780657630

collatzLength :: Int
collatzLength = 1132

-- | Tests multiple 'State'S, 'put', 'get' and 'modify'.
collatz :: (State Integer :> es, State Int :> es) => Eff es ()
collatz = get @Integer >>= \case
  1 -> pure ()
  n -> if even n
       then do put $ n `div` 2
               modify @Int (+1)
               collatz
       else do put $ 3*n + 1
               modify @Int (+1)
               collatz
{-# NOINLINE collatz #-}
