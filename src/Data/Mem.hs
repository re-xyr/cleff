-- | 'Mem' is a data structure that is a simulation of an array of thread-local references. This structure supports:
--
-- * \( O(n) \) creation of a new reference;
-- * \( O(n) \) changing the reference in an array cell;
-- * \( O(1) \) modification of the memory a reference points to;
-- * \( O(1) \) read.
module Data.Mem (Mem, MemRef, empty, adjust, read, write, replace, append, update) where

import           Data.Any           (Any, fromAny, toAny)
import           Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as Map
import           Data.Kind          (Type)
import           Data.Rec           (Rec, pattern (:~:))
import qualified Data.Rec           as Rec
import           Prelude            hiding (read)

-- | The representation of a reference in a 'Mem'.
type role MemRef representational nominal
newtype MemRef (f :: k -> Type) (a :: k) = MemRef { unMemRef :: Int }
  deriving
    ( Eq  -- ^ Pointer equality.
    , Ord -- ^ An arbitrary total order on the references.
    )

-- | An simulated array of thread-local references. This means for each array cell, you can either change the reference or
-- change the memory the reference points to.
type role Mem representational nominal
data Mem (f :: k -> Type) (es :: [k]) = Mem
  {-# UNPACK #-} !(Rec (MemRef f) es)
  {-# UNPACK #-} !Int
  !(IntMap Any)

-- | Create a 'Mem' with no references.
empty :: Mem f '[]
empty = Mem Rec.empty 0 Map.empty
{-# INLINE empty #-}

-- | Adjust the array of references.
adjust :: (Rec (MemRef f) es -> Rec (MemRef f) es') -> Mem f es -> Mem f es'
adjust f (Mem re n mem) = Mem (f re) n mem
{-# INLINE adjust #-}

-- | Read a reference. \( O(1) \).
read :: forall e es f. Rec.Elem e es => Mem f es -> f e
read (Mem re _ mem) = fromAny $ mem Map.! unMemRef (Rec.index @e re)
{-# INLINE read #-}

-- | Write to the memory a reference points to. \( O(1) \).
write :: forall e es f. Rec.Elem e es => f e -> Mem f es -> Mem f es
write x (Mem re n mem) = Mem re n (Map.insert (unMemRef $ Rec.index @e re) (toAny x) mem)
{-# INLINE write #-}

-- | Replace a reference with a new one. \( O(n) \).
replace :: forall e es f. Rec.Elem e es => f e -> Mem f es -> Mem f es
replace x (Mem re n mem) = Mem (Rec.modify @e (MemRef n) re) (succ n) (Map.insert n (toAny x) mem)
{-# INLINE replace #-}

-- | Add a new reference to the array. \( O(n) \).
append :: forall e es f. f e -> Mem f es -> Mem f (e ': es)
append x (Mem re n mem) = Mem (MemRef n :~: re) (succ n) (Map.insert n (toAny x) mem)
{-# INLINE append #-}

-- | Use the memory of LHS as a newer version for the memory of RHS. \( O(1) \).
--
-- This is a potentially unsafe operation and specifically, is only safe when LHS is a revision of RHS.
update :: Mem f es' -> Mem f es -> Mem f es
update (Mem _ n mem) (Mem re' _ _) = Mem re' n mem
{-# INLINE update #-}
