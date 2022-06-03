{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UnboxedTuples       #-}
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
  ( Rec
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
  , (:>)
  , (:>>)
  , Subset
  , index
  , pick
  , update
  ) where

import           Cleff.Internal
import           Data.Foldable            (for_)
import           Data.Kind                (Constraint)
import           Data.Primitive.PrimArray (MutablePrimArray (MutablePrimArray), PrimArray (PrimArray), copyPrimArray,
                                           indexPrimArray, newPrimArray, writePrimArray)
import           GHC.Exts                 (runRW#, unsafeFreezeByteArray#)
import           GHC.ST                   (ST (ST))
import           GHC.TypeLits             (ErrorMessage (ShowType, Text, (:<>:)), TypeError)
import           Prelude                  hiding (concat, drop, head, tail, take)

-- | Extensible record type supporting efficient \( O(1) \) reads. The underlying implementation is 'PrimArray'
-- slices.
type role Rec nominal
data Rec (es :: [Effect]) = Rec
  {-# UNPACK #-} !Int -- ^ The offset.
  {-# UNPACK #-} !Int -- ^ The length.
  {-# UNPACK #-} !(PrimArray Int) -- ^ The array content.

unreifiable :: String -> String -> String -> a
unreifiable clsName funName comp = error $
  funName <> ": Attempting to access " <> comp <> " without a reflected value. This is perhaps because you are trying \
  \to define an instance for the '" <> clsName <> "' typeclass, which you should not be doing whatsoever. If that or \
  \other shenanigans seem unlikely, please report this as a bug."

runPrimArray :: (∀ s. ST s (MutablePrimArray s a)) -> PrimArray a
runPrimArray (ST f) = let
  !(# _, ba# #) = runRW# \s1 ->
    let !(# s2, MutablePrimArray mba# #) = f s1
    in unsafeFreezeByteArray# mba# s2
  in PrimArray ba#

-- | Create an empty record. \( O(1) \).
empty :: Rec '[]
empty = Rec 0 0 $ runPrimArray $ newPrimArray 0

-- | Prepend one entry to the record. \( O(n) \).
cons :: HandlerPtr e -> Rec es -> Rec (e : es)
cons x (Rec off len arr) = Rec 0 (len + 1) $ runPrimArray do
  marr <- newPrimArray (len + 1)
  writePrimArray marr 0 (unHandlerPtr x)
  copyPrimArray marr 1 arr off len
  pure marr

-- | Concatenate two records. \( O(m+n) \).
concat :: Rec es -> Rec es' -> Rec (es ++ es')
concat (Rec off len arr) (Rec off' len' arr') = Rec 0 (len + len') $ runPrimArray do
  marr <- newPrimArray (len + len')
  copyPrimArray marr 0 arr off len
  copyPrimArray marr len arr' off' len'
  pure marr

-- | Slice off one entry from the top of the record. \( O(1) \).
tail :: Rec (e : es) -> Rec es
tail (Rec off len arr) = Rec (off + 1) (len - 1) arr

-- | @'KnownList' es@ means the list @es@ is concrete, /i.e./ is of the form @'[a1, a2, ..., an]@ instead of a type
-- variable.
class KnownList (es :: [Effect]) where
  -- | Get the length of the list.
  reifyLen :: Int
  reifyLen = unreifiable "KnownList" "Cleff.Internal.Rec.reifyLen" "the length of a type-level list"

instance KnownList '[] where
  reifyLen = 0

instance KnownList es => KnownList (e : es) where
  reifyLen = 1 + reifyLen @es

-- | Slice off several entries from the top of the record. \( O(1) \).
drop :: ∀ es es'. KnownList es => Rec (es ++ es') -> Rec es'
drop (Rec off len arr) = Rec (off + len') (len - len') arr
  where len' = reifyLen @es

-- | Get the head of the record. \( O(1) \).
head :: Rec (e : es) -> HandlerPtr e
head (Rec off _ arr) = HandlerPtr $ indexPrimArray arr off

-- | Take elements from the top of the record. \( O(m) \).
take :: ∀ es es'. KnownList es => Rec (es ++ es') -> Rec es
take (Rec off _ arr) = Rec 0 len $ runPrimArray do
  marr <- newPrimArray len
  copyPrimArray marr 0 arr off len
  pure marr
  where len = reifyLen @es

-- | @e ':>' es@ means the effect @e@ is present in the effect stack @es@, and therefore can be 'Cleff.send'ed in an
-- @'Cleff.Eff' es@ computation.
class (e :: Effect) :> (es :: [Effect]) where
  -- | Get the index of the element.
  reifyIndex :: Int
  reifyIndex = unreifiable "Elem" "Cleff.Internal.Rec.reifyIndex" "the index of an effect in the effect stack"
infix 0 :>

-- | The element closer to the head takes priority.
instance {-# OVERLAPPING #-} e :> e : es where
  reifyIndex = 0

instance e :> es => e :> e' : es where
  reifyIndex = 1 + reifyIndex @e @es

type ElemNotFound e = Text "The element '" :<>: ShowType e :<>: Text "' is not present in the constraint"

instance TypeError (ElemNotFound e) => e :> '[] where
  reifyIndex = error
    "Cleff.Internal.reifyIndex: Attempting to refer to a nonexistent member. Please report this as a bug."

-- | @xs ':>>' es@ means the list of effects @xs@ are all present in the effect stack @es@. This is a convenient type
-- alias for @(e1 ':>' es, ..., en ':>' es)@.
type family xs :>> es :: Constraint where
  '[] :>> _ = ()
  (x : xs) :>> es = (x :> es, xs :>> es)
infix 0 :>>

-- | Get an element in the record. Amortized \( O(1) \).
index :: ∀ e es. e :> es => Rec es -> HandlerPtr e
index (Rec off _ arr) = HandlerPtr $ indexPrimArray arr (off + reifyIndex @e @es)

-- | @es@ is a subset of @es'@, /i.e./ all elements of @es@ are in @es'@.
class KnownList es => Subset (es :: [Effect]) (es' :: [Effect]) where
  -- | Get a list of indices of the elements.
  reifyIndices :: [Int]
  reifyIndices = unreifiable
    "Subset" "Cleff.Internal.Rec.reifyIndices" "the indices of a subset of the effect stack"

instance Subset '[] es where
  reifyIndices = []

instance (Subset es es', e :> es') => Subset (e : es) es' where
  reifyIndices = reifyIndex @e @es' : reifyIndices @es @es'

-- | Get a subset of the record. Amortized \( O(m) \).
pick :: ∀ es es'. Subset es es' => Rec es' -> Rec es
pick (Rec off _ arr) = Rec 0 (reifyLen @es) $ runPrimArray do
  marr <- newPrimArray (reifyLen @es)
  for_ (zip [0..] (reifyIndices @es @es')) \(newIx, ix) ->
    writePrimArray marr newIx $ indexPrimArray arr (off + ix)
  pure marr

-- | Update an entry in the record. \( O(n) \).
update :: ∀ e es. e :> es => HandlerPtr e -> Rec es -> Rec es
update x (Rec off len arr) = Rec 0 len $ runPrimArray do
  marr <- newPrimArray len
  copyPrimArray marr 0 arr off len
  writePrimArray marr (reifyIndex @e @es) (unHandlerPtr x)
  pure marr
