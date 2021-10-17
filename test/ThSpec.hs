
-- | This module is adapted from https://github.com/arybczak/effectful/blob/master/effectful/tests/ThEffectSpec.hs,
-- originally BSD3 license, authors Andrzej Rybczak et al.
module ThSpec where

import           Cleff
import           Data.Kind    (Type)
import           GHC.TypeLits
import           Test.Hspec

spec :: Spec
spec = it "should compile" True

data SimpleADT m a = SimpleADTC1 Int | SimpleADTC2 String

makeEffect ''SimpleADT

data GADTSyntax m a where
  GADTSyntaxC1 :: Int -> GADTSyntax m a
  GADTSyntaxC2 :: String -> GADTSyntax m a

makeEffect ''GADTSyntax

data ADTSyntax1 m a = a ~ Int => ADTSyntax1C String

makeEffect ''ADTSyntax1

data ADTSyntax2 m a
  = a ~ Int    => ADTSyntax2C1 Int
  | a ~ String => ADTSyntax2C2 String

makeEffect ''ADTSyntax2

data ADTSyntax3 m a = Show a => ADTSyntax3C a

makeEffect ''ADTSyntax3

data Fields m a = FieldsC { fieldsCF1 :: Int, fieldsCF2 :: String }

makeEffect ''Fields

newtype Newtype1 m a = Newtype1C Int

makeEffect ''Newtype1

newtype Newtype2 m a where
  Newtype2C :: String -> Newtype2 m a

makeEffect ''Newtype2

data Instance = ADTI | GADTI | NTI | MMI

data family Family (s :: Instance) (m :: Type -> Type) a

data instance Family 'ADTI m a = ADTIC1 Int | ADTIC2 String

makeEffect 'ADTIC1

data instance Family 'GADTI m a where
  GADTIC1 :: Int -> Family 'GADTI m Int
  GADTIC2 :: String -> Family 'GADTI m String

makeEffect 'GADTIC1

newtype instance Family 'NTI m a = NTIC Int

makeEffect 'NTIC

data instance Family 'MMI m (f m) where
  MMIC1 :: f m -> Family 'MMI m (f m)
  MMIC2 :: (forall x. m x -> m (f m)) -> Family 'MMI m (f m)

-- TODO(daylily): This cannot produce desired result.
-- makeEffect 'MMIC1

data Complex m a where
  Mono            :: Int -> Complex m Bool
  Poly            :: a -> Complex m a
  PolyIn          :: a -> Complex m Bool
  PolyOut         :: Int -> Complex m a
  Lots            :: a -> b -> c -> d -> e -> f -> Complex m ()
  Nested          :: Maybe b -> Complex m (Maybe a)
  MultiNested     :: (Maybe a, [b]) -> Complex m (Maybe a, [b])
  Existential     :: (forall e. e -> Maybe e) -> Complex m a
  LotsNested      :: Maybe a -> [b] -> (c, c) -> Complex m (a, b, c)
  Dict            :: Ord a => a -> Complex m a
  MultiDict       :: (Eq a, Ord b, Enum a, Num c)
                  => a -> b -> c -> Complex m ()
  IndexedMono     :: f 0 -> Complex m Int
  IndexedPoly     :: forall f (n :: Nat) m . f n -> Complex m (f (n + 1))
  IndexedPolyDict :: KnownNat n => f n -> Complex m Int

makeEffect ''Complex

data HOEff m a where
  EffArgMono :: m () -> HOEff m ()
  EffArgPoly :: m a -> HOEff m a
  EffArgComb :: m a -> (m a -> m b) -> HOEff m b
  EffRank2   :: (forall x. m x -> m (Maybe x)) -> HOEff m a

makeEffect ''HOEff

data ComplexEffArgs b c m a where
  EffMono     :: Int -> ComplexEffArgs Int String m Bool
  EffPoly1    :: a -> ComplexEffArgs a b m a
  EffPoly2    :: a -> ComplexEffArgs a (Maybe a) m Bool
  EffPolyFree :: String -> ComplexEffArgs a b m Int
  EffSame1    :: ComplexEffArgs a a m a
  EffSame2    :: ComplexEffArgs b b m a
  EffHO       :: m b -> ComplexEffArgs b Int m String

-- TODO(daylily): This cannot produce desired result. This is almost certainly caused by us not annotating types
-- explicitly, but that's too much effort.
-- makeEffect ''ComplexEffArgs

data HKEffArgs f g m a where
  HKRank2 :: (forall x . f x -> g x) -> HKEffArgs f g m a

makeEffect ''HKEffArgs

data ByCon m a where
  ByConC :: Int -> ByCon m String

makeEffect 'ByConC

data ByField m a where
  ByFieldC :: { byFieldCF :: Int } -> ByField m Int

makeEffect 'byFieldCF
