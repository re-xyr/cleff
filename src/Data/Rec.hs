{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_HADDOCK not-home #-}
-- | This module defines an immutable extensible record type, similar to @vinyl@ and @data-diverse@. However this
-- implementation focuses on fast reads, hence has very different performance characteristics from other libraries:
--
-- * Lookup: Amortized \( O(1) \).
-- * Update: \( O(n) \).
-- * Shrink: \( O(1) \).
-- * Append: \( O(n) \).
--
-- __This is an /internal/ module and its API may change even between minor versions.__ Therefore you should be
-- extra careful if you're to depend on this module.
module Data.Rec
  ( Rec, length
  , -- * Construction
    empty, singleton
  , -- * Addition
    cons, pattern (:~:), type (++), concat, pattern (:++:)
  , -- * Deletion
    tail, KnownList, drop
  , -- * Retrieval
    head, take, Elem, index, Subset, pick
  , -- * Modification
    modify, (/~/), batch, (/++/)
  , -- * Mapping
    type (~>), natural, (<#>), zipWith, all, any, degenerate, extract
  , -- * Debugging
    invariant, sizeInvariant, allAccessible
  ) where

import           Control.Monad.Primitive   (PrimMonad (PrimState))
import           Data.Any
import           Data.Functor.Const        (Const (Const, getConst))
import           Data.Kind                 (Type)
import           Data.List                 (intersperse)
import           Data.Primitive.SmallArray (SmallArray, SmallMutableArray, copySmallArray, indexSmallArray,
                                            newSmallArray, runSmallArray, sizeofSmallArray, writeSmallArray)
import           Data.Tuple.Extra          ((&&&))
import           GHC.TypeLits              (ErrorMessage (ShowType, Text, (:<>:)), TypeError)
import           Prelude                   hiding (all, any, concat, drop, head, length, tail, take, zipWith)
import           Text.Read                 (readPrec)
import qualified Text.Read                 as R
import qualified Text.Read.Lex             as RL

-- | Extensible record type supporting efficient \( O(1) \) reads. The underlying implementation is 'SmallArray'
-- slices, therefore suits small numbers of entries (/i.e./ less than 128).
type role Rec representational nominal
data Rec (f :: k -> Type) (es :: [k]) = Rec
  {-# UNPACK #-} !Int -- ^ The offset.
  {-# UNPACK #-} !Int -- ^ The length.
  {-# UNPACK #-} !(SmallArray Any) -- ^ The array content.

instance Eq (Rec f '[]) where
  _ == _ = True

instance (Eq (Rec f xs), Eq (f x)) => Eq (Rec f (x ': xs)) where
  x :~: xs == y :~: ys = x == y && xs == ys

instance {-# OVERLAPPABLE #-} (∀ x. Eq (f x)) => Eq (Rec f xs) where
  xs == ys = all (== Const True) $ zipWith (\x y -> Const $ x == y) xs ys

-- | @
-- 'show' 'empty' == "empty"
-- @
instance Show (Rec f '[]) where
  show _ = "empty"

-- | @
-- 'read' \"empty\" == 'empty'
-- @
instance Read (Rec f '[]) where
  readPrec = R.parens $ R.prec appPrec $
    empty <$ R.lift (RL.expect (R.Ident "empty"))
    where appPrec = 10

-- | @
-- 'show' ('Data.Functor.Identity.Identity' 'True' ':~:' 'Data.Functor.Identity.Identity' \"Hi\" ':~:' 'empty')
-- == "Identity True :~: Identity \\"Hi\\" :~: empty"
-- @
instance (Show (f x), Show (Rec f xs)) => Show (Rec f (x ': xs)) where
  showsPrec p (x :~: xs) = showParen (p > consPrec) $
    showsPrec (consPrec + 1) x . showString " :~: " . showsPrec consPrec xs

-- | @
-- 'read' "Identity True :~: Identity \\"Hi\\" :~: empty"
-- == 'Data.Functor.Identity.Identity' 'True' ':~:' 'Data.Functor.Identity.Identity' \"Hi\" ':~:' 'empty'
-- @
instance (Read (f x), Read (Rec f xs)) => Read (Rec f (x ': xs)) where
  readPrec = R.parens $ R.prec consPrec $
    cons <$> R.step (readPrec @(f x)) <* R.lift (RL.expect (R.Symbol ":~:")) <*> readPrec @(Rec f xs)

-- | @
-- 'show' ('Const' 'False' ':~:' 'Const' 'True' ':~:' 'empty')
-- == "Const False :~: Const True :~: empty"
-- @
instance {-# OVERLAPPABLE #-} (∀ x. Show (f x)) => Show (Rec f xs) where
  showsPrec p xs = showParen (p > consPrec) $
    foldr (.) id $ intersperse (showString " :~: ") $ extract (showsPrec (consPrec + 1)) xs

instance Semigroup (Rec f '[]) where
  xs <> _ = xs

-- | One-by-one semigroup operation instead of concatenation.
--
-- @
-- (x ':~:' xs) '<>' (y ':~:' ys) == x '<>' y ':~:' xs '<>' ys
-- @
instance (Semigroup (f x), Semigroup (Rec f xs)) => Semigroup (Rec f (x ': xs)) where
  (x :~: xs) <> (y :~: ys) = x <> y :~: xs <> ys

instance {-# OVERLAPPABLE #-} (∀ x. Semigroup (f x)) => Semigroup (Rec f xs) where
  xs <> ys = zipWith (<>) xs ys

-- | @
-- 'mempty' == 'empty'
-- @
instance Monoid (Rec f '[]) where
  mempty = empty

-- | The unit of a record type are the units of its element types:
--
-- @
-- 'mempty' == 'mempty' ':~:' 'mempty'
-- @
instance (Monoid (f x), Monoid (Rec f xs)) => Monoid (Rec f (x ': xs)) where
  mempty = mempty :~: mempty

-- | Get the length of the record.
length :: Rec f es -> Int
length (Rec _ len _) = len

-- | Create a new 'SmallMutableArray' with no contents.
newArr :: PrimMonad m => Int -> m (SmallMutableArray (PrimState m) a)
newArr len = newSmallArray len $ error
  "Data.Rec.newArr: Attempting to read an element of the underlying array of a 'Rec'. Please report this as a bug."

-- | Create an empty record. \( O(1) \).
empty :: Rec f '[]
empty = Rec 0 0 $ runSmallArray $ newArr 0

-- | Create a record with one entry. \( O(1) \).
singleton :: f e -> Rec f '[e]
singleton x = Rec 0 1 $ runSmallArray do
  marr <- newArr 1
  writeSmallArray marr 0 (toAny x)
  pure marr

-- | Prepend one entry to the record. \( O(n) \).
cons :: f e -> Rec f es -> Rec f (e ': es)
cons x (Rec off len arr) = Rec 0 (len + 1) $ runSmallArray do
  marr <- newArr (len + 1)
  writeSmallArray marr 0 (toAny x)
  copySmallArray marr 1 arr off len
  pure marr

-- | Infix version of 'cons' that also supports destructuring.
pattern (:~:) :: f e -> Rec f es -> Rec f (e ': es)
pattern x :~: xs <- (head &&& tail -> (x, xs))
  where (:~:) = cons
infixr 5 :~:
{-# COMPLETE (:~:) #-}

-- | @infixr 5 :~:@
consPrec :: Int
consPrec = 5

-- | Type level list concatenation.
type family xs ++ ys where
  '[] ++ ys = ys
  (x ': xs) ++ ys = x ': (xs ++ ys)
infixr 5 ++

-- | Concatenate two records. \( O(m+n) \).
concat :: Rec f es -> Rec f es' -> Rec f (es ++ es')
concat (Rec off len arr) (Rec off' len' arr') = Rec 0 (len + len') $ runSmallArray do
  marr <- newArr (len + len')
  copySmallArray marr 0 arr off len
  copySmallArray marr len arr' off' len'
  pure marr

-- | Infix version of 'concat' that also supports destructuring.
pattern (:++:) :: ∀ es es' f. KnownList es => Rec f es -> Rec f es' -> Rec f (es ++ es')
pattern xs :++: xs' <- (take @es @es' &&& drop @es @es' -> (xs, xs'))
  where (:++:) = concat
infixr 5 :++:
{-# COMPLETE (:++:) #-}

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
  reifyLen = unreifiable "KnownList" "Data.Rec.reifyLen" "the length of a type-level list"

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
head (Rec off _ arr) = fromAny $ indexSmallArray arr off

-- | Take elements from the top of the record. \( O(m) \).
take :: ∀ es es' f. KnownList es => Rec f (es ++ es') -> Rec f es
take (Rec off _ arr) = Rec 0 len $ runSmallArray do
  marr <- newArr len
  copySmallArray marr 0 arr off (off + len)
  pure marr
  where len = reifyLen @_ @es

-- | The element @e@ is present in the list @es@.
class Elem (e :: k) (es :: [k]) where
  -- | Get the index of the element.
  reifyIndex :: Int
  reifyIndex = unreifiable "Elem" "Data.Rec.reifyIndex" "the index of an element of a type-level list"

instance {-# OVERLAPPING #-} Elem e (e ': es) where
  reifyIndex = 0

instance Elem e es => Elem e (e' ': es) where
  reifyIndex = 1 + reifyIndex @_ @e @es

type ElemNotFound e = 'Text "The element '" ':<>: 'ShowType e ':<>: 'Text "' is not present in the constraint"

instance TypeError (ElemNotFound e) => Elem e '[] where
  reifyIndex = error "Data.Rec.reifyIndex: Attempting to refer to a nonexistent member. Please report this as a bug."

-- | Get an element in the record. Amortized \( O(1) \).
index :: ∀ e es f. Elem e es => Rec f es -> f e
index (Rec off _ arr) = fromAny $ indexSmallArray arr (off + reifyIndex @_ @e @es)

-- | @es@ is a subset of @es'@.
class KnownList es => Subset (es :: [k]) (es' :: [k]) where
  -- | Get a list of indices of the elements.
  reifyIndices :: [Int]
  reifyIndices = unreifiable "Subset" "Data.Rec.reifyIndices" "the index of multiple elements of a type-level list"

instance Subset '[] es where
  reifyIndices = []

instance (Subset es es', Elem e es') => Subset (e ': es) es' where
  reifyIndices = reifyIndex @_ @e @es' : reifyIndices @_ @es @es'

-- | Get a subset of the record. Amortized \( O(m) \).
pick :: ∀ es es' f. Subset es es' => Rec f es' -> Rec f es
pick (Rec off _ arr) = Rec 0 (reifyLen @_ @es) $ runSmallArray do
  marr <- newArr (reifyLen @_ @es)
  go marr (0 :: Int) (reifyIndices @_ @es @es')
  pure marr
  where
    go :: PrimMonad m => SmallMutableArray (PrimState m) Any -> Int -> [Int] -> m ()
    go _ _ [] = pure ()
    go marr newIx (ix : ixs) = do
      writeSmallArray marr newIx (indexSmallArray arr (off + ix))
      go marr (newIx + 1) ixs

-- | Modify an entry in the record. \( O(n) \).
modify :: ∀ e es f. Elem e es => f e -> Rec f es -> Rec f es
modify x (Rec off len arr) = Rec 0 len $ runSmallArray do
  marr <- newArr len
  copySmallArray marr 0 arr off len
  writeSmallArray marr (reifyIndex @_ @e @es) (toAny x)
  pure marr

-- | Infix version of 'modify'.
(/~/) :: Elem e es => f e -> Rec f es -> Rec f es
(/~/) = modify
infixl 9 /~/

-- | Merge a subset into the original record, updating several entries at once. \( O(m+n) \).
batch :: ∀ es es' f. Subset es es' => Rec f es -> Rec f es' -> Rec f es'
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

-- | Infix version of 'batch'.
(/++/) :: Subset es es' => Rec f es -> Rec f es' -> Rec f es'
(/++/) = batch
infixl 9 /++/

-- | The type of natural transformations from functor @f@ to @g@.
type f ~> g = ∀ a. f a -> g a
infixr 0 ~>

-- | Apply a natural transformation to the record. \( O(n) \).
natural :: (f ~> g) -> Rec f es -> Rec g es
natural f (Rec off len arr) = Rec 0 len $ runSmallArray do
  marr <- newArr len
  go marr 0
  pure marr
  where
    go :: PrimMonad m => SmallMutableArray (PrimState m) Any -> Int -> m ()
    go marr n
      | n == len = pure ()
      | otherwise = do
        writeSmallArray marr n (toAny $ f $ fromAny $ indexSmallArray arr (off + n))
        go marr (n + 1)

-- | Infix version of 'natural'.
(<#>) :: (f ~> g) -> Rec f es -> Rec g es
(<#>) = natural
infixl 4 <#>

-- | Zip two records with a natural transformation. \( O(n) \).
zipWith :: (∀ x. f x -> g x -> h x) -> Rec f es -> Rec g es -> Rec h es
zipWith f (Rec off len arr) (Rec off' _ arr') = Rec 0 len $ runSmallArray do
  marr <- newArr len
  go marr (0 :: Int)
  pure marr
  where
    go :: PrimMonad m => SmallMutableArray (PrimState m) Any -> Int -> m ()
    go marr n
      | n == len = pure ()
      | otherwise = do
        writeSmallArray marr n
          (toAny $ f (fromAny $ indexSmallArray arr (off + n)) (fromAny $ indexSmallArray arr' (off' + n)))
        go marr (n + 1)

-- | Check if a predicate is true on all elements. \( O(n) \).
all :: (∀ x. f x -> Bool) -> Rec f es -> Bool
all f (Rec off len arr) = go 0
  where
    go n
      | n == len = True
      | otherwise = f (fromAny $ indexSmallArray arr (off + n)) && go (n + 1)

-- | Check if a predicate is true on at least one element. \( O(n) \).
any :: (∀ x. f x -> Bool) -> Rec f es -> Bool
any f (Rec off len arr) = go 0
  where
    go n
      | n == len = False
      | otherwise = f (fromAny $ indexSmallArray arr (off + n)) || go (n + 1)

-- | Convert a record that effectively contains a fixed type into a list of the fixed type. \( O(n) \).
degenerate :: Rec (Const a) es -> [a]
degenerate (Rec off len arr) = go 0
  where
    go n
      | n == len = []
      | otherwise = getConst (fromAny $ indexSmallArray arr (off + n)) : go (n + 1)

-- | Map each element to a fixed type. \( O(n) \).
extract :: (∀ x. f x -> a) -> Rec f es -> [a]
extract f xs = degenerate $ natural (Const . f) xs

-- | Test the size invariant of 'Rec'.
sizeInvariant :: Rec f es -> Rec f es
sizeInvariant xs@(Rec off len arr)
  | tracked == actual = xs
  | otherwise = error $ "Data.Rec.sizeInvariant: tracked size " <> show tracked <> ", actual size " <> show actual
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
