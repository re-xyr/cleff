{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE QuantifiedConstraints #-}
-- |
-- Copyright: (c) 2021 Xy Ren
-- License: BSD3
-- Maintainer: xy.r@outlook.com
-- Stability: experimental
-- Portability: non-portable (GHC only)
--
-- This module defines an immutable extensible record type, similar to @vinyl@ and @data-diverse@. However this
-- implementation focuses on fast reads, hence has very different performance characteristics from other libraries:
--
-- * Lookup: Amortized \( O(1) \).
-- * Update: \( O(n) \).
-- * Shrink: \( O(1) \).
-- * Append: \( O(n) \).
module Sp.Internal.Env
  ( Rec
  , length
  , empty
    -- * Construction
  , cons
  , type (++)
  , concat
    -- * Deconstruction
  , tail
  , KnownList
  , drop
    -- * Retrieval
  , head
  , take
  , (:>)
  , index
  , Subset
  , pick
    -- * Updating
  , update
  ) where

import           Control.Monad.Primitive (PrimMonad (PrimState))
import           Data.Function           (on)
import           Data.Kind               (Type)
import           Data.Monoid             (All (All, getAll))
import           Data.Primitive.Array    (Array, MutableArray, cloneArray, copyArray, emptyArray, indexArray, newArray,
                                          runArray, thawArray, writeArray)
import           GHC.Exts                (Any)
import           GHC.TypeLits            (ErrorMessage (ShowType, Text, (:<>:)), TypeError)
import           Prelude                 hiding (concat, drop, head, length, tail, take)
import           Unsafe.Coerce           (unsafeCoerce)

-- | Extensible record type supporting efficient \( O(1) \) reads. The underlying implementation is 'Array'
-- slices, therefore suits small numbers of entries (/i.e./ less than 128).
type role Rec representational nominal
data Rec (f :: k -> Type) (es :: [k]) = Rec
  {-# UNPACK #-} !Int -- ^ The offset.
  {-# UNPACK #-} !Int -- ^ The length.
  {-# UNPACK #-} !(Array Any) -- ^ The array content.

instance (forall a. Eq (f a)) => Eq (Rec f es) where
  Rec off len arr == Rec off' len' arr' = if len /= len' then False else
    getAll $ foldMap All $
      zipWith ((==) `on` fromAny @(f Any))
        (fmap (\x -> indexArray arr (off + x)) [0 .. len - 1])
        (fmap (\x -> indexArray arr' (off' + x)) [0 .. len - 1])

-- | Get the length of the record.
length :: Rec f es -> Int
length (Rec _ len _) = len

-- | Create a new 'MutableArray' with no contents.
newArr :: PrimMonad m => Int -> m (MutableArray (PrimState m) a)
newArr len = newArray len $ error
  "Sp.Env: Attempting to read an element of the underlying array of a 'Rec'. Please report this as a bug."

-- | Create an empty record. \( O(1) \).
empty :: Rec f '[]
empty = Rec 0 0 $ emptyArray

-- | Prepend one entry to the record. \( O(n) \).
cons :: f e -> Rec f es -> Rec f (e ': es)
cons x (Rec off len arr) = Rec 0 (len + 1) $ runArray do
  marr <- newArray (len + 1) (toAny x)
  copyArray marr 1 arr off len
  pure marr

-- | Type level list concatenation.
type family (xs :: [k]) ++ (ys :: [k]) where
  '[] ++ ys = ys
  (x ': xs) ++ ys = x ': (xs ++ ys)
infixr 5 ++

-- | Concatenate two records. \( O(m+n) \).
concat :: Rec f es -> Rec f es' -> Rec f (es ++ es')
concat (Rec off len arr) (Rec off' len' arr') = Rec 0 (len + len') $ runArray do
  marr <- newArr (len + len')
  copyArray marr 0 arr off len
  copyArray marr len arr' off' len'
  pure marr

-- | Slice off one entry from the top of the record. \( O(1) \).
tail :: Rec f (e ': es) -> Rec f es
tail (Rec off len arr) = Rec (off + 1) (len - 1) arr

unreifiable :: String -> String -> String -> a
unreifiable clsName funName comp = error $
  funName <> ": Attempting to access " <> comp <> " without a reflected value. This is perhaps because you are trying \
  \to define an instance for the '" <> clsName <> "' typeclass, which you should not be doing whatsoever. If that or \
  \other shenanigans seem unlikely, please report this as a bug."

-- | The list @es@ list is concrete, i.e. is of the form @'[a1, a2, ..., an]@, i.e. is not a type variable.
class KnownList (es :: [k]) where
  -- | Get the length of the list.
  reifyLen :: Int
  reifyLen = unreifiable "KnownList" "Sp.Env" "the length of a type-level list"

instance KnownList '[] where
  reifyLen = 0

instance KnownList es => KnownList (e ': es) where
  reifyLen = 1 + reifyLen @_ @es

-- | Slice off several entries from the top of the record. \( O(1) \).
drop :: ∀ es es' f. KnownList es => Rec f (es ++ es') -> Rec f es'
drop (Rec off len arr) = Rec (off + len') (len - len') arr
  where len' = reifyLen @_ @es

-- | Get the head of the record. \( O(1) \).
head :: Rec f (e ': es) -> f e
head (Rec off _ arr) = fromAny $ indexArray arr off

-- | Take elements from the top of the record. \( O(m) \).
take :: ∀ es es' f. KnownList es => Rec f (es ++ es') -> Rec f es
take (Rec off _ arr) = Rec 0 len $ cloneArray arr off (off + len)
  where len = reifyLen @_ @es

-- | The element @e@ is present in the list @es@.
class (e :: k) :> (es :: [k]) where
  -- | Get the index of the element.
  reifyIndex :: Int
  reifyIndex = unreifiable "Elem" "Sp.Env" "the index of an element of a type-level list"
infix 0 :>

instance {-# OVERLAPPING #-} e :> e : es where
  reifyIndex = 0

instance e :> es => e :> e' : es where
  reifyIndex = 1 + reifyIndex @_ @e @es

type ElemNotFound e = 'Text "The element '" ':<>: 'ShowType e ':<>: 'Text "' is not present in the constraint"

instance TypeError (ElemNotFound e) => e :> '[] where
  reifyIndex = error
    "Sp.Env: Attempting to refer to a nonexistent member. Please report this as a bug."

-- | Get an element in the record. Amortized \( O(1) \).
index :: ∀ e es f. e :> es => Rec f es -> f e
index (Rec off _ arr) = fromAny $ indexArray arr (off + reifyIndex @_ @e @es)

-- | @es@ is a subset of @es'@.
class KnownList es => Subset (es :: [k]) (es' :: [k]) where
  -- | Get a list of indices of the elements.
  reifyIndices :: [Int]
  reifyIndices = unreifiable
    "Subset" "Sp.Env" "the index of multiple elements of a type-level list"

instance Subset '[] es where
  reifyIndices = []

instance (Subset es es', e :> es') => Subset (e ': es) es' where
  reifyIndices = reifyIndex @_ @e @es' : reifyIndices @_ @es @es'

-- | Get a subset of the record. Amortized \( O(m) \).
pick :: ∀ es es' f. Subset es es' => Rec f es' -> Rec f es
pick (Rec off _ arr) = Rec 0 (reifyLen @_ @es) $ runArray do
  marr <- newArr (reifyLen @_ @es)
  go marr 0 (reifyIndices @_ @es @es')
  pure marr
  where
    go :: PrimMonad m => MutableArray (PrimState m) Any -> Int -> [Int] -> m ()
    go _ _ [] = pure ()
    go marr newIx (ix : ixs) = do
      writeArray marr newIx (indexArray arr (off + ix))
      go marr (newIx + 1) ixs

-- | Update an entry in the record. \( O(n) \).
update :: ∀ e es f. e :> es => f e -> Rec f es -> Rec f es
update x (Rec off len arr) = Rec 0 len $ runArray do
  marr <- thawArray arr off len
  writeArray marr (reifyIndex @_ @e @es) (toAny x)
  pure marr

-- Helpers

-- | Coerce any boxed value into 'Any'.
toAny :: a -> Any
toAny = unsafeCoerce
{-# INLINE toAny #-}

-- | Coerce 'Any' to a boxed value. This is /generally unsafe/ and it is your responsibility to ensure that the type
-- you're coercing into is the original type that the 'Any' is coerced from.
fromAny :: Any -> a
fromAny = unsafeCoerce
{-# INLINE fromAny #-}
