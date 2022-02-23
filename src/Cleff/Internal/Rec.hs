{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_HADDOCK not-home #-}
-- |
-- Copyright: (c) 2021 Xy Ren
-- License: BSD3
-- Maintainer: xy.r@outlook.com
-- Stability: unstable
-- Portability: non-portable (GHC only)
--
-- This module defines an immutable extensible record type, similar to @vinyl@ and @data-diverse@. However this
-- implementation focuses on fast reads, hence has very different performance characteristics from other libraries:
--
-- * Lookup: Amortized \( O(1) \).
-- * Update: \( O(n) \).
-- * Shrink: \( O(1) \).
-- * Append: \( O(n) \).
--
-- __This is an /internal/ module and its API may change even between minor versions.__ Therefore you should be
-- extra careful if you're to depend on this module.
module Cleff.Internal.Rec
  ( Rec (Rec)
  , type (++)
    -- * Construction
  , empty
  , cons
  , concat
    -- * Deconstruction
  , KnownList
  , head
  , take
  , tail
  , drop
    -- * Retrieval and updating
  , Elem
  , Subset
  , index
  , pick
  , update
    -- * Helpers
  , newArr
  ) where

import           Cleff.Internal.Any        (Any, fromAny, toAny)
import           Control.Monad.ST          (ST)
import           Data.Kind                 (Type)
import           Data.Primitive.SmallArray (SmallArray, SmallMutableArray, cloneSmallArray, copySmallArray,
                                            indexSmallArray, newSmallArray, runSmallArray, thawSmallArray,
                                            writeSmallArray)
import           GHC.TypeLits              (ErrorMessage (ShowType, Text, (:<>:)), TypeError)
import           Prelude                   hiding (all, any, concat, drop, head, length, tail, take, zipWith)

-- | Extensible record type supporting efficient \( O(1) \) reads. The underlying implementation is 'SmallArray'
-- slices, therefore suits small numbers of entries (/i.e./ less than 128).
type role Rec representational nominal
data Rec (f :: k -> Type) (es :: [k]) = Rec
  {-# UNPACK #-} !Int -- ^ The offset.
  {-# UNPACK #-} !Int -- ^ The length.
  {-# UNPACK #-} !(SmallArray Any) -- ^ The array content.

-- | Create a new 'SmallMutableArray' with no contents.
newArr :: Int -> ST s (SmallMutableArray s Any)
newArr len = newSmallArray len $ error
  "Cleff.Internal.Rec.newArr: Attempting to read an element of the underlying array of a 'Rec'. Please report this \
  \as a bug."

unreifiable :: String -> String -> String -> a
unreifiable clsName funName comp = error $
  funName <> ": Attempting to access " <> comp <> " without a reflected value. This is perhaps because you are trying \
  \to define an instance for the '" <> clsName <> "' typeclass, which you should not be doing whatsoever. If that or \
  \other shenanigans seem unlikely, please report this as a bug."

-- | Create an empty record. \( O(1) \).
empty :: Rec f '[]
empty = Rec 0 0 $ runSmallArray $ newArr 0

-- | Prepend one entry to the record. \( O(n) \).
cons :: f e -> Rec f es -> Rec f (e : es)
cons x (Rec off len arr) = Rec 0 (len + 1) $ runSmallArray do
  marr <- newArr (len + 1)
  writeSmallArray marr 0 (toAny x)
  copySmallArray marr 1 arr off len
  pure marr

-- | Type level list concatenation.
type family xs ++ ys where
  '[] ++ ys = ys
  (x : xs) ++ ys = x : (xs ++ ys)
infixr 5 ++

-- | Concatenate two records. \( O(m+n) \).
concat :: Rec f es -> Rec f es' -> Rec f (es ++ es')
concat (Rec off len arr) (Rec off' len' arr') = Rec 0 (len + len') $ runSmallArray do
  marr <- newArr (len + len')
  copySmallArray marr 0 arr off len
  copySmallArray marr len arr' off' len'
  pure marr

-- | Slice off one entry from the top of the record. \( O(1) \).
tail :: Rec f (e : es) -> Rec f es
tail (Rec off len arr) = Rec (off + 1) (len - 1) arr

-- | @'KnownList' es@ means the list @es@ is concrete, i.e. is of the form @'[a1, a2, ..., an]@ instead of a type
-- variable.
class KnownList (es :: [k]) where
  -- | Get the length of the list.
  reifyLen :: Int
  reifyLen = unreifiable "KnownList" "Cleff.Internal.Rec.reifyLen" "the length of a type-level list"

instance KnownList '[] where
  reifyLen = 0

instance KnownList es => KnownList (e : es) where
  reifyLen = 1 + reifyLen @_ @es

-- | Slice off several entries from the top of the record. \( O(1) \).
drop :: ∀ es es' f. KnownList es => Rec f (es ++ es') -> Rec f es'
drop (Rec off len arr) = Rec (off + len') (len - len') arr
  where len' = reifyLen @_ @es

-- | Get the head of the record. \( O(1) \).
head :: Rec f (e : es) -> f e
head (Rec off _ arr) = fromAny $ indexSmallArray arr off

-- | Take elements from the top of the record. \( O(m) \).
take :: ∀ es es' f. KnownList es => Rec f (es ++ es') -> Rec f es
take (Rec off _ arr) = Rec 0 len $ cloneSmallArray arr off len
  where len = reifyLen @_ @es

-- | The element @e@ is present in the list @es@.
class Elem (e :: k) (es :: [k]) where
  -- | Get the index of the element.
  reifyIndex :: Int
  reifyIndex = unreifiable "Elem" "Cleff.Internal.Rec.reifyIndex" "the index of an element of a type-level list"

-- | The element closer to the head takes priority.
instance {-# OVERLAPPING #-} Elem e (e : es) where
  reifyIndex = 0

instance Elem e es => Elem e (e' : es) where
  reifyIndex = 1 + reifyIndex @_ @e @es

type ElemNotFound e = Text "The element '" :<>: ShowType e :<>: Text "' is not present in the constraint"

instance TypeError (ElemNotFound e) => Elem e '[] where
  reifyIndex = error
    "Cleff.Internal.Rec.reifyIndex: Attempting to refer to a nonexistent member. Please report this as a bug."

-- | Get an element in the record. Amortized \( O(1) \).
index :: ∀ e es f. Elem e es => Rec f es -> f e
index (Rec off _ arr) = fromAny $ indexSmallArray arr (off + reifyIndex @_ @e @es)

-- | @es@ is a subset of @es'@, i.e. all elements of @es@ are in @es'@.
class KnownList es => Subset (es :: [k]) (es' :: [k]) where
  -- | Get a list of indices of the elements.
  reifyIndices :: [Int]
  reifyIndices = unreifiable
    "Subset" "Cleff.Internal.Rec.reifyIndices" "the index of multiple elements of a type-level list"

instance Subset '[] es where
  reifyIndices = []

instance (Subset es es', Elem e es') => Subset (e : es) es' where
  reifyIndices = reifyIndex @_ @e @es' : reifyIndices @_ @es @es'

-- | Get a subset of the record. Amortized \( O(m) \).
pick :: ∀ es es' f. Subset es es' => Rec f es' -> Rec f es
pick (Rec off _ arr) = Rec 0 (reifyLen @_ @es) $ runSmallArray do
  marr <- newArr (reifyLen @_ @es)
  go marr 0 (reifyIndices @_ @es @es')
  pure marr
  where
    go :: SmallMutableArray s Any -> Int -> [Int] -> ST s ()
    go _ _ [] = pure ()
    go marr newIx (ix : ixs) = do
      writeSmallArray marr newIx $ indexSmallArray arr (off + ix)
      go marr (newIx + 1) ixs

-- | Update an entry in the record. \( O(n) \).
update :: ∀ e es f. Elem e es => f e -> Rec f es -> Rec f es
update x (Rec off len arr) = Rec 0 len $ runSmallArray do
  marr <- thawSmallArray arr off len
  writeSmallArray marr (reifyIndex @_ @e @es) (toAny x)
  pure marr
