{-# OPTIONS_GHC -Wno-orphans #-}
module RecSpec where

import           Cleff.Internal.Rec        (Elem, Rec (Rec), type (++))
import qualified Cleff.Internal.Rec        as Rec
import           Data.Functor.Identity     (Identity (Identity))
import           Data.Primitive.SmallArray (indexSmallArray, sizeofSmallArray)
import           Data.Typeable             (cast)
import           Test.Hspec

type I = Identity
i :: a -> Identity a
i = Identity

-- | Test the size invariant of 'Rec'.
sizeInvariant :: Rec f es -> Rec f es
sizeInvariant xs@(Rec off len arr)
  | tracked == actual = xs
  | otherwise = error $
    "Cleff.Internal.Rec.sizeInvariant: tracked size " <> show tracked <> ", actual size " <> show actual
  where
    tracked = len + off
    actual = sizeofSmallArray arr

-- | Test whether all fields of 'Rec' are really set.
allAccessible :: Rec f es -> Rec f es
allAccessible xs@(Rec off len arr) = go 0
  where
    go n
      | n == len = xs
      | otherwise = indexSmallArray arr (off + n) `seq` go (n + 1)

-- | Test all invariants.
invariant :: Rec f es -> Rec f es
invariant = allAccessible . sizeInvariant

singleton :: f e -> Rec f '[e]
singleton x = x <:> Rec.empty

(<:>) :: f e -> Rec f es -> Rec f (e : es)
(<:>) = Rec.cons
infixr 5 <:>

(<++>) :: Rec f es -> Rec f es' -> Rec f (es ++ es')
(<++>) = Rec.concat
infixr 5 <++>

(<//>) :: Elem e es => f e -> Rec f es -> Rec f es
(<//>) = Rec.update
infixr 9 <//>

instance Eq (Rec f '[]) where
  _ == _ = True

instance (Eq (Rec f xs), Eq (f x)) => Eq (Rec f (x : xs)) where
  xs == ys = Rec.head xs == Rec.head ys && Rec.tail xs == Rec.tail ys

instance Show (Rec f '[]) where
  show _ = "empty"

instance (Show (f x), Show (Rec f xs)) => Show (Rec f (x : xs)) where
  showsPrec p xs = showParen (p > consPrec) $
    showsPrec (consPrec + 1) (Rec.head xs) . showString " <:> " . showsPrec consPrec (Rec.tail xs)
    where consPrec = 5

spec :: Spec
spec = describe "Rec (SmallArray)" $ parallel do
  it "is Typeable" do
    let
      x = i (5 :: Int) <:> i False <:> Rec.empty
      y = cast x :: Maybe (Rec I '[Int, String])
      z = cast x :: Maybe (Rec I '[Int, Bool])
    y `shouldBe` Nothing
    z `shouldBe` Just x

  it "can be constructed with 'empty', 'cons', 'concat'" do
    let
      x = invariant $ i (5 :: Int) <:> i False <:> i 'X' <:> i (Just 'O') <:> Rec.empty
      y = invariant $ singleton (i (5 :: Int)) <++> singleton (i False)
        <++> singleton (i 'X') <++> singleton (i (Just 'O'))
      a = invariant $ i (5 :: Int) <:> singleton (i False)
      b = invariant $  singleton (i 'X') <++> singleton (i (Just 'O'))
    x `shouldBe` y
    invariant (a <++> b) `shouldBe` x

  it "can contain multiple fields of the same type" do
    let
      x = i (5 :: Int) <:> i False <:> i 'X' <:> i (Just 'O') <:> Rec.empty
      y = i (5 :: Int) <:> i False <:> i 'X' <:> i (Just 'O') <:> i (6 :: Int) <:> i (Just 'A') <:> Rec.empty
    invariant (x <++> 6 <:> i (Just 'A') <:> Rec.empty) `shouldBe` y

  it "can be destructed via 'head', 'tail', 'take', 'drop'" do
    let
      a = (x <:> y) <++> singleton z
      x = i (5 :: Int)
      y = i (singleton $ i False) <:> i 'X' <:> Rec.empty
      z = i (Just 'O')
    Rec.head a `shouldBe` x
    invariant (Rec.drop @'[Int, Rec I '[Bool], Char] a) `shouldBe` singleton z
    invariant (Rec.tail a) `shouldBe` invariant (y <++> singleton z)
    invariant (Rec.take @'[Int, Rec I '[Bool], Char] a) `shouldBe` (x <:> y)

  it "can get elements via 'index'" do
    let x = i (5 :: Int) <:> i False <:> i 'X' <:> i (Just 'O') <:> Rec.empty
    Rec.index @Int x `shouldBe` 5
    Rec.index @Bool x `shouldBe` i False
    Rec.index @Char x `shouldBe` i 'X'
    Rec.index @(Maybe Char) x `shouldBe` i (Just 'O')

  it "can get the topmost element among the duplicate ones" do
    let y = i (5 :: Int) <:> i False <:> i 'X' <:> i (Just 'O') <:> i (6 :: Int) <:> i (Just 'A') <:> Rec.empty
    Rec.index @Int y `shouldBe` 5
    Rec.index @Bool y `shouldBe` i False
    Rec.index @Char y `shouldBe` i 'X'
    Rec.index @(Maybe Char) y `shouldBe` i (Just 'O')

  it "can set elements via 'update'" do
    let x = i (5 :: Int) <:> i False <:> i 'X' <:> i (Just 'O') <:> Rec.empty
    invariant (Rec.update @Int 6 x) `shouldBe` 6 <:> i False <:> i 'X' <:> i (Just 'O') <:> Rec.empty
    invariant (i True <//> x) `shouldBe` 5 <:> i True <:> i 'X' <:> i (Just 'O') <:> Rec.empty
    invariant (i 'O' <//> x) `shouldBe` 5 <:> i False <:> i 'O' <:> i (Just 'O') <:> Rec.empty
    invariant (i (Just 'P') <//> x) `shouldBe` 5 <:> i False <:> i 'X' <:> i (Just 'P') <:> Rec.empty

  it "can get multiple elements via 'pick'" do
    let x = i (5 :: Int) <:> i False <:> i 'X' <:> i (Just 'O') <:> Rec.empty
    invariant (Rec.pick @'[Int, Maybe Char] x) `shouldBe` 5 <:> i (Just 'O') <:> Rec.empty

  it "can reorder elements via 'pick'" do
    let x = i (5 :: Int) <:> i False <:> i 'X' <:> i (Just 'O') <:> Rec.empty
    invariant (Rec.pick @'[Bool, Int, Maybe Char] x) `shouldBe` i False <:> 5 <:> i (Just 'O') <:> Rec.empty
