{-# OPTIONS_HADDOCK not-home #-}
module Data.Rec
  ( Rec, length
  , -- * Construction
    empty, singleton
  , -- * Addition
    cons, type (++), concat
  , -- * Deletion
    uncons, KnownList, drop
  , -- * Retrieval
    Elem, index, Subset, take
  , -- * Modification
    modify, batch
  , -- * Mapping
    type (~>), natural
  , -- * Debugging
    invariant
  ) where

import           Control.Monad.Primitive   (PrimMonad (PrimState))
import           Data.Kind                 (Type)
import           Data.Primitive.SmallArray
import           GHC.Exts                  (Any)
import           GHC.TypeLits              (ErrorMessage (ShowType, Text, (:<>:)), TypeError)
import           Prelude                   hiding (concat, drop, length, take)
import           Unsafe.Coerce             (unsafeCoerce)

-- | Extensible record type supporting efficient O(1) reads. The underlying implementation is 'SmallArray' slices,
-- therefore suits small numbers of entries (/i.e./ less than 128).
type role Rec representational nominal
data Rec (f :: k -> Type) (es :: [k]) = Rec
  {-# UNPACK #-} !Int -- ^ The offset.
  {-# UNPACK #-} !Int -- ^ The length.
  {-# UNPACK #-} !(SmallArray Any) -- ^ The array content.

-- | Get the length of the record.
length :: Rec f es -> Int
length (Rec _ len _) = len

-- | Create a new 'SmallMutableArray' with no contents.
newArr :: PrimMonad m => Int -> m (SmallMutableArray (PrimState m) a)
newArr len = newSmallArray len $ error "reading nonexistent data"

-- | Create an empty record. O(1).
empty :: Rec f '[]
empty = Rec 0 0 $ runSmallArray $ newArr 0

-- | Create a record with one entry. O(1).
singleton :: f e -> Rec f '[e]
singleton x = Rec 0 1 $ runSmallArray do
  marr <- newArr 1
  writeSmallArray marr 0 (toAny x)
  pure marr

-- | Append one entry to the record. O(n).
cons :: f e -> Rec f es -> Rec f (e ': es)
cons x (Rec off len arr) = Rec 0 (len + 1) $ runSmallArray do
  marr <- newArr (len + 1)
  writeSmallArray marr 0 (toAny x)
  copySmallArray marr 1 arr off len
  pure marr

-- | Type level list concatenation.
type family xs ++ ys where
  '[] ++ ys = ys
  (x ': xs) ++ ys = x ': (xs ++ ys)
infixr 5 ++

-- | Concatenate two records. O(m+n).
concat :: Rec f es -> Rec f es' -> Rec f (es ++ es')
concat (Rec off len arr) (Rec off' len' arr') = Rec 0 (len + len') $ runSmallArray do
  marr <- newArr (len + len')
  copySmallArray marr 0 arr off len
  copySmallArray marr len arr' off' len'
  pure marr

-- | Slice off one entry from the top of the record. O(1).
uncons :: Rec f (e ': es) -> Rec f es
uncons (Rec off len arr) = Rec (off + 1) (len - 1) arr

-- | Typeclass that shows a list has a known structure (/i.e./ known length).  Practically, this means you know the
-- contents of the list.
class KnownList (es :: [k]) where
  -- | Get the length of the list.
  reifyLen :: Int
  reifyLen = error "unimplemented Data.Rec.reifyLen"

instance KnownList '[] where
  reifyLen = 0

instance KnownList es => KnownList (e ': es) where
  reifyLen = 1 + reifyLen @_ @es

-- | Slice off several entries from the top of the record. O(1).
drop :: forall es f es'. KnownList es => Rec f (es ++ es') -> Rec f es'
drop (Rec off len arr) = Rec (off + len') (len - len') arr
  where len' = reifyLen @_ @es

-- | Witnesses the presence of an element in a type level list.
class Elem (e :: k) (es :: [k]) where
  -- | Get the index of the element.
  reifyIndex :: Int
  reifyIndex = error "unimplemented Data.Rec.reifyIndex"

instance {-# OVERLAPPING #-} Elem e (e ': es) where
  reifyIndex = 0

instance Elem e es => Elem e (e' ': es) where
  reifyIndex = 1 + reifyIndex @_ @e @es

type ElemNotFound e = 'Text "The element '" ':<>: 'ShowType e ':<>: 'Text "' is not present in the constraint"

instance TypeError (ElemNotFound e) => Elem e '[] where
  reifyIndex = error "trying to refer to a nonexistent member"

-- | Get an element in the record. Amortized O(1).
index :: forall e es f. Elem e es => Rec f es -> f e
index (Rec off _ arr) = fromAny $ indexSmallArray arr (off + reifyIndex @_ @e @es)

-- | Typeclass that witnesses @es@ being a subset of @es'@.
class KnownList es => Subset (es :: [k]) (es' :: [k]) where
  -- | Get a list of indices of the elements.
  reifyIndices :: [Int]
  reifyIndices = error "unimplemented Data.Rec.reifyIndices"

instance Subset '[] es where
  reifyIndices = []

instance (Subset es es', Elem e es') => Subset (e ': es) es' where
  reifyIndices = reifyIndex @_ @e @es' : reifyIndices @_ @es @es'

-- | Get a subset of the record. Amortized O(m).
take :: forall es es' f. Subset es es' => Rec f es' -> Rec f es
take (Rec off _ arr) = Rec 0 (reifyLen @_ @es) $ runSmallArray do
  marr <- newArr (reifyLen @_ @es)
  go marr (0 :: Int) (reifyIndices @_ @es @es')
  pure marr
  where
    go :: PrimMonad m => SmallMutableArray (PrimState m) Any -> Int -> [Int] -> m ()
    go _ _ [] = pure ()
    go marr newIx (ix : ixs) = do
      writeSmallArray marr newIx (indexSmallArray arr (off + ix))
      go marr (newIx + 1) ixs

-- | Modify an entry in the record. Amortized O(n).
modify :: forall e es f. Elem e es => f e -> Rec f es -> Rec f es
modify x (Rec off len arr) = Rec 0 len $ runSmallArray do
  marr <- newArr len
  copySmallArray marr 0 arr off len
  writeSmallArray marr (reifyIndex @_ @e @es) (toAny x)
  pure marr

-- | Merge a subset into the original record, updating several entries at once. Amortized O(m+n).
batch :: forall es es' f. Subset es es' => Rec f es -> Rec f es' -> Rec f es'
batch (Rec off _ arr) (Rec off' len' arr') = Rec 0 len' $ runSmallArray do
  marr <- newArr len'
  copySmallArray marr 0 arr' off' len'
  go marr (0 :: Int) (reifyIndices @_ @es @es')
  pure marr
  where
    go :: PrimMonad m => SmallMutableArray (PrimState m) Any -> Int -> [Int] -> m ()
    go _ _ [] = pure ()
    go marr updIx (ix : ixs) = do
      writeSmallArray marr ix (indexSmallArray arr (off + updIx))
      go marr (updIx + 1) ixs

-- | The type of natural transformations from functor @f@ to @g@.
type f ~> g = forall a. f a -> g a
infixr 0 ~>

-- | Apply a natural transformation to the record. O(n).
natural :: (f ~> g) -> Rec f es -> Rec g es
natural f (Rec off len arr) = Rec 0 len $ runSmallArray do
  marr <- newArr len
  go marr 0
  pure marr
  where
    go :: PrimMonad m => SmallMutableArray (PrimState m) Any -> Int -> m ()
    go marr n
      | n == len = pure ()
      | otherwise = writeSmallArray marr n (toAny $ f $ fromAny $ indexSmallArray arr (off + n))

toAny :: a -> Any
toAny = unsafeCoerce
{-# INLINE toAny #-}

fromAny :: Any -> a
fromAny = unsafeCoerce
{-# INLINE fromAny #-}

-- | Test the size invariant of 'Rec'.
invariant :: Rec f es -> Rec f es
invariant xs@(Rec off len arr)
  | tracked == actual = xs
  | otherwise = error $ "Rec invariant violated: tracked size " <> show tracked <> ", actual size " <> show actual
  where
    tracked = len + off
    actual = sizeofSmallArray arr
