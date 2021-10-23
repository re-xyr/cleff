module RecSpec where

import           Data.Functor.Identity (Identity (Identity))
import           Data.Rec              (Rec, invariant, (~!~), (~$~), (~+~), (~/~), (~:~))
import qualified Data.Rec              as Rec
import           Data.Typeable         (cast)
import           Test.Hspec

type I = Identity
i :: a -> Identity a
i = Identity

spec :: Spec
spec = describe "Rec" do
  it "is Typeable" do
    let
      x = i (5 :: Int) ~:~ i False ~:~ Rec.empty
      y = cast x :: Maybe (Rec I '[Int, String])
      z = cast x :: Maybe (Rec I '[Int, Bool])
    y `shouldBe` Nothing
    z `shouldBe` Just x

  it "is Show" do
    let
      s = "Identity 5 ~:~ Identity False ~:~ Identity 'X' ~:~ Identity (Just 'O') ~:~ empty"
      x = i (5 :: Int) ~:~ i False ~:~ i 'X' ~:~ i (Just 'O') ~:~ Rec.empty
    show x `shouldBe` s

  it "is Eq" do
    let
      x = i (5 :: Int) ~:~ i False ~:~ i 'X' ~:~ i (Just 'O') ~:~ Rec.empty
      y = invariant $ id ~$~ x
    x `shouldBe` y

  it "can be constructed with 'empty', 'singleton', 'cons', 'concat'" do
    let
      x = invariant $ i (5 :: Int) ~:~ i False ~:~ i 'X' ~:~ i (Just 'O') ~:~ Rec.empty
      y = invariant $ Rec.singleton (i (5 :: Int)) ~+~ Rec.singleton (i False)
        ~+~ Rec.singleton (i 'X') ~+~ Rec.singleton (i (Just 'O'))
      a = invariant $ i (5 :: Int) ~:~ Rec.singleton (i False)
      b = invariant $  Rec.singleton (i 'X') ~+~ Rec.singleton (i (Just 'O'))
    x `shouldBe` y
    invariant (a ~+~ b) `shouldBe` x

  it "can contain multiple fields of the same type" do
    let
      x = i (5 :: Int) ~:~ i False ~:~ i 'X' ~:~ i (Just 'O') ~:~ Rec.empty
      y = i (5 :: Int) ~:~ i False ~:~ i 'X' ~:~ i (Just 'O') ~:~ i (6 :: Int) ~:~ i (Just 'A') ~:~ Rec.empty
    invariant (x ~+~ 5 ~:~ i (Just 'A') ~:~ Rec.empty) `shouldBe` y

  it "can be destructed via 'head', 'tail', 'take', 'drop'" do
    let
      a = (x ~:~ y) ~+~ Rec.singleton z
      x = i (5 :: Int)
      y = i (Rec.singleton $ i False) ~:~ i 'X' ~:~ Rec.empty
      z = i (Just 'O')
    Rec.head a `shouldBe` x
    invariant (Rec.drop @'[Int, Rec I '[Bool], Char] a) `shouldBe` Rec.singleton z
    invariant (Rec.tail a) `shouldBe` invariant (y ~+~ Rec.singleton z)
    invariant (Rec.take @'[Int, Rec I '[Bool], Char] a) `shouldBe` (x ~:~ y)

  it "can get elements via 'index'" do
    let x = i (5 :: Int) ~:~ i False ~:~ i 'X' ~:~ i (Just 'O') ~:~ Rec.empty
    Rec.index @Int x `shouldBe` 5
    Rec.index @Bool x `shouldBe` i False
    Rec.index @Char x `shouldBe` i 'X'
    Rec.index @(Maybe Char) x `shouldBe` i (Just 'O')

  it "can get the topmost element among the duplicate ones" do
    let y = i (5 :: Int) ~:~ i False ~:~ i 'X' ~:~ i (Just 'O') ~:~ i (6 :: Int) ~:~ i (Just 'A') ~:~ Rec.empty
    Rec.index @Int y `shouldBe` 5
    Rec.index @Bool y `shouldBe` i False
    Rec.index @Char y `shouldBe` i 'X'
    Rec.index @(Maybe Char) y `shouldBe` i (Just 'O')

  it "can set elements via 'modify'" do
    let x = i (5 :: Int) ~:~ i False ~:~ i 'X' ~:~ i (Just 'O') ~:~ Rec.empty
    invariant (Rec.modify @Int 6 x) `shouldBe` 5 ~:~ i False ~:~ i 'X' ~:~ i (Just 'O') ~:~ Rec.empty
    invariant (i True ~!~ x) `shouldBe` 5 ~:~ i True ~:~ i 'X' ~:~ i (Just 'O') ~:~ Rec.empty
    invariant (i 'O' ~!~ x) `shouldBe` 5 ~:~ i False ~:~ i 'O' ~:~ i (Just 'O') ~:~ Rec.empty
    invariant (i (Just 'P') ~!~ x) `shouldBe` 5 ~:~ i False ~:~ i 'X' ~:~ i (Just 'P') ~:~ Rec.empty

  it "can get multiple elements via 'pick'" do
    let x = i (5 :: Int) ~:~ i False ~:~ i 'X' ~:~ i (Just 'O') ~:~ Rec.empty
    invariant (Rec.pick @'[Int, Maybe Char] x) `shouldBe` 5 ~:~ i (Just 'O') ~:~ Rec.empty

  it "can reorder elements via 'pick'" do
    let x = i (5 :: Int) ~:~ i False ~:~ i 'X' ~:~ i (Just 'O') ~:~ Rec.empty
    invariant (Rec.pick @'[Bool, Int, Maybe Char] x) `shouldBe` i False ~:~ 5 ~:~ i (Just 'O') ~:~ Rec.empty

  it "can set multiple fields via 'batch'" do
    let x = i (5 :: Int) ~:~ i False ~:~ i 'X' ~:~ i (Just 'O') ~:~ Rec.empty
    invariant ((i (6 :: Int) ~:~ i (Just 'X') ~:~ Rec.empty) ~/~ x)
      `shouldBe` 6 ~:~ i False ~:~ i 'X' ~:~ i (Just 'X') ~:~ Rec.empty
