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
-- This module defines the effect stack as an immutable extensible stack type, and provides functions for manipulating
-- it. The effect stack type has the following time complexities:
--
-- * Lookup: Amortized \( O(1) \).
-- * Update: \( O(n) \).
-- * Shrink: \( O(1) \).
-- * Append: \( O(n) \).
--
-- __This is an /internal/ module and its API may change even between minor versions.__ Therefore you should be
-- extra careful if you're to depend on this module.
module Cleff.Internal.Stack
  ( Effect
  , Stack
  , HandlerPtr (HandlerPtr, unHandlerPtr)
    -- * Construction
  , type (++)
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

import           Data.Coerce  (coerce)
import           Data.Kind    (Constraint, Type)
import           Data.PrimVec (PrimVec)
import qualified Data.PrimVec as Vec
import           GHC.TypeLits (ErrorMessage (ShowType, Text, (:<>:)), TypeError)
import           Prelude      hiding (concat, drop, head, tail, take)

-- | The type of effects. An effect @e m a@ takes an effect monad type @m :: 'Type' -> 'Type'@ and a result type
-- @a :: 'Type'@.
type Effect = (Type -> Type) -> Type -> Type

-- | The effect stack, storing pointers to handlers. It is essentially an extensible stack type supporting
-- efficient \( O(1) \) reads.
type role Stack nominal
newtype Stack (es :: [Effect]) = Stack (PrimVec Int)

-- | A pointer to an effect handler.
type role HandlerPtr nominal
newtype HandlerPtr (e :: Effect) = HandlerPtr { unHandlerPtr :: Int }

unreifiable :: String -> String -> String -> a
unreifiable clsName funName comp = error $
  funName <> ": Attempting to access " <> comp <> " without a reflected value. This is perhaps because you are trying \
  \to define an instance for the '" <> clsName <> "' typeclass, which you should not be doing whatsoever. If that or \
  \other shenanigans seem unlikely, please report this as a bug."

-- | Create an empty stack. \( O(1) \).
empty :: Stack '[]
empty = coerce (Vec.empty @Int)

-- | Prepend one entry to the stack. \( O(n) \).
cons :: HandlerPtr e -> Stack es -> Stack (e : es)
cons = coerce (Vec.cons @Int)

-- | Type level list concatenation.
type family xs ++ ys where
  '[] ++ ys = ys
  (x : xs) ++ ys = x : (xs ++ ys)
infixr 5 ++

-- | Concatenate two stacks. \( O(m+n) \).
concat :: Stack es -> Stack es' -> Stack (es ++ es')
concat = coerce (Vec.concat @Int)

-- | Slice off one entry from the top of the stack. \( O(1) \).
tail :: Stack (e : es) -> Stack es
tail = coerce (Vec.tail @Int)

-- | @'KnownList' es@ means the list @es@ is concrete, /i.e./ is of the form @'[a1, a2, ..., an]@ instead of a type
-- variable.
class KnownList (es :: [Effect]) where
  -- | Get the length of the list.
  reifyLen :: Int
  reifyLen = unreifiable "KnownList" "Cleff.Internal.Stack.reifyLen" "the length of a type-level list"

instance KnownList '[] where
  reifyLen = 0

instance KnownList es => KnownList (e : es) where
  reifyLen = 1 + reifyLen @es

-- | Slice off several entries from the top of the stack. \( O(1) \).
drop :: ∀ es es'. KnownList es => Stack (es ++ es') -> Stack es'
drop = coerce (Vec.drop @Int) (reifyLen @es)

-- | Get the head of the stack. \( O(1) \).
head :: Stack (e : es) -> HandlerPtr e
head = coerce (Vec.head @Int)

-- | Take elements from the top of the stack. \( O(m) \).
take :: ∀ es es'. KnownList es => Stack (es ++ es') -> Stack es
take = coerce (Vec.take @Int) (reifyLen @es)

-- | @e ':>' es@ means the effect @e@ is present in the effect stack @es@, and therefore can be 'Cleff.send'ed in an
-- @'Cleff.Eff' es@ computation.
class (e :: Effect) :> (es :: [Effect]) where
  -- | Get the index of the element.
  reifyIndex :: Int
  reifyIndex = unreifiable "Elem" "Cleff.Internal.Stack.reifyIndex" "the index of an effect in the effect stack"
infix 0 :>

-- | The element closer to the head takes priority.
instance {-# OVERLAPPING #-} e :> e : es where
  reifyIndex = 0

instance e :> es => e :> e' : es where
  reifyIndex = 1 + reifyIndex @e @es

type ElemNotFound e = Text "The element '" :<>: ShowType e :<>: Text "' is not present in the constraint"

instance TypeError (ElemNotFound e) => e :> '[] where
  reifyIndex = error
    "Cleff.Internal.Stack.reifyIndex: Attempting to refer to a nonexistent member. Please report this as a bug."

-- | @xs ':>>' es@ means the list of effects @xs@ are all present in the effect stack @es@. This is a convenient type
-- alias for @(e1 ':>' es, ..., en ':>' es)@.
type family xs :>> es :: Constraint where
  '[] :>> _ = ()
  (x : xs) :>> es = (x :> es, xs :>> es)
infix 0 :>>

-- | Get an element in the stack. Amortized \( O(1) \).
index :: ∀ e es. e :> es => Stack es -> HandlerPtr e
index = coerce (Vec.index @Int) (reifyIndex @e @es)

-- | @es@ is a subset of @es'@, /i.e./ all elements of @es@ are in @es'@.
class KnownList es => Subset (es :: [Effect]) (es' :: [Effect]) where
  -- | Get a list of indices of the elements.
  reifyIndices :: [Int]
  reifyIndices = unreifiable
    "Subset" "Cleff.Internal.Stack.reifyIndices" "the indices of a subset of the effect stack"

instance Subset '[] es where
  reifyIndices = []

instance (Subset es es', e :> es') => Subset (e : es) es' where
  reifyIndices = reifyIndex @e @es' : reifyIndices @es @es'

-- | Get a subset of the stack. Amortized \( O(m) \).
pick :: ∀ es es'. Subset es es' => Stack es' -> Stack es
pick = coerce (Vec.pick @Int) (reifyLen @es) (reifyIndices @es @es')

-- | Update an entry in the stack. \( O(n) \).
update :: ∀ e es. e :> es => HandlerPtr e -> Stack es -> Stack es
update = coerce (Vec.update @Int) (reifyIndex @e @es)
