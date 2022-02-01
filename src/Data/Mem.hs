{-# LANGUAGE UnboxedTuples #-}
-- |
-- Copyright: (c) 2021 Xy Ren
-- License: BSD3
-- Maintainer: xy.r@outlook.com
-- Stability: unstable
-- Portability: non-portable (GHC only)
--
-- 'Mem' is a data structure that is a simulation of an array of thread-local pointers. This structure supports:
--
-- * \( O(n) \) creation of a new pointer;
-- * \( O(n) \) changing the pointer in an array cell;
-- * \( O(1) \) modification of the memory a pointer points to;
-- * \( O(1) \) read.
--
-- __This is an /internal/ module and its API may change even between minor versions.__ Therefore you should be
-- extra careful if you're to depend on this module.
module Data.Mem (Mem, MemPtr, empty, adjust, alloca, read, write, replace, append, update) where

import           Data.Any
import           Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as Map
import           Data.Kind          (Type)
import           Data.Rec           (Rec, pattern (:~:))
import qualified Data.Rec           as Rec
import           Prelude            hiding (read)

-- | The representation of a pointer in a 'Mem'.
type role MemPtr representational nominal
newtype MemPtr (f :: k -> Type) (a :: k) = MemPtr { unMemPtr :: Int }
  deriving newtype
    ( Eq  -- ^ Pointer equality.
    , Ord -- ^ An arbitrary total order on the pointers.
    )

-- | A simulated array of thread-local pointers. This means for each array cell, you can either change the pointer or
-- change the memory the pointer points to.
--
-- Note that like real memory, any of the operations provided is not generally safe and it is your responsibility to
-- ensure the correctness of your calls.
type role Mem representational nominal
data Mem (f :: k -> Type) (es :: [k]) = Mem
  {-# UNPACK #-} !(Rec (MemPtr f) es) -- ^ The array.
  {-# UNPACK #-} !Int -- ^ The next memory address to allocate.
  !(IntMap Any) -- ^ The simulated memory.

-- | Create a 'Mem' with no pointers.
empty :: Mem f '[]
empty = Mem Rec.empty 0 Map.empty
{-# INLINE empty #-}

-- | Adjust the array of pointers.
adjust :: ∀ es' es f. (Rec (MemPtr f) es -> Rec (MemPtr f) es') -> Mem f es -> Mem f es'
adjust f (Mem re n mem) = Mem (f re) n mem
{-# INLINE adjust #-}

-- | Allocate a new address. \( O(1) \).
alloca :: ∀ e es f. Mem f es -> (# MemPtr f e, Mem f es #)
alloca (Mem re n mem) = (# MemPtr n, Mem re (succ n) mem #)
{-# INLINE alloca #-}

-- | Read a pointer. \( O(1) \).
read :: ∀ e es f. Rec.Elem e es => Mem f es -> f e
read (Mem re _ mem) = fromAny $ mem Map.! unMemPtr (Rec.index @e re)
{-# INLINE read #-}

-- | Write to the memory a pointer points to. \( O(1) \).
write :: ∀ e es f. MemPtr f e -> f e -> Mem f es -> Mem f es
write (MemPtr m) x (Mem re n mem) = Mem re n (Map.insert m (toAny x) mem)
{-# INLINE write #-}

-- | Replace a pointer with a new one. \( O(n) \).
replace :: ∀ e es f. Rec.Elem e es => MemPtr f e -> f e -> Mem f es -> Mem f es
replace (MemPtr m) x (Mem re n mem) = Mem (Rec.modify @e (MemPtr m) re) n (Map.insert m (toAny x) mem)
{-# INLINE replace #-}

-- | Add a new pointer to the array. \( O(n) \).
append :: ∀ e es f. MemPtr f e -> f e -> Mem f es -> Mem f (e ': es)
append (MemPtr m) x (Mem re n mem) = Mem (MemPtr m :~: re) n (Map.insert m (toAny x) mem)
{-# INLINE append #-}

-- | Use the memory of LHS as a newer version for the memory of RHS. \( O(1) \).
update :: ∀ es es' f. Mem f es' -> Mem f es -> Mem f es
update (Mem _ n mem) (Mem re' _ _) = Mem re' n mem
{-# INLINE update #-}
