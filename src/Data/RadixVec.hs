{-# OPTIONS_HADDOCK not-home #-}
-- |
-- Copyright: (c) 2021 Xy Ren
-- License: BSD3
-- Maintainer: xy.r@outlook.com
-- Stability: unstable
-- Portability: non-portable (GHC only)
--
-- This module contains an efficient vector datatype that is implemented as a radix tree.
--
-- __This is an /internal/ module and its API may change even between minor versions.__ Therefore you should be
-- extra careful if you're to depend on this module.
module Data.RadixVec (RadixVec, empty, lookup, update, snoc) where

import           Control.Monad.ST          (ST)
import           Data.Bits                 (Bits (unsafeShiftL, unsafeShiftR, (.&.)), FiniteBits (countTrailingZeros))
import           Data.Primitive.MachDeps   (sIZEOF_INT)
import           Data.Primitive.SmallArray (SmallArray, SmallMutableArray, copySmallArray, indexSmallArray,
                                            newSmallArray, readSmallArray, runSmallArray, sizeofSmallArray,
                                            thawSmallArray, writeSmallArray)
import           Prelude                   hiding (lookup)

-- | An efficient vector type, implemented as a radix tree. It has the following time complexities:
--
-- * Lookup: \( O(\log n) \)
-- * Update: \( O(\log n) \)
-- * Append: \( O(\log n) \)
--
-- The branching factor (base of log) is 32 therefore the time is close to constant in most cases. Note that in
-- practice, lookup is faster than update, and update is faster than append.
data RadixVec a = RadixVec !Int !(Tree a)

type Shift = Int

-- | The \( \log_2 \) of the branching factor. The branching factor is set to be 32 for now but may change in the
-- future.
factor :: Int
factor = 5

-- | A mask covering one chunk of an index.
initialMask :: Int
initialMask = (1 `unsafeShiftL` factor) - 1

-- | A radix tree. The tree is always left-leaning.
data Tree a
  = Tip
    {-# UNPACK #-} !(SmallArray a)
  | Node
    {-# UNPACK #-} !Shift
    {-# UNPACK #-} !(SmallArray (Tree a))

-- | Mask a portion of an index.
mask :: Shift -> Int -> Int
mask s x = initialMask .&. (x `unsafeShiftR` s)

-- | Mask the zeroth portion of the index.
mask0 :: Int -> Int
mask0 x = initialMask .&. x

-- | Alter an element in a 'SmallMutableArray' by a function.
alterSmallArray :: SmallMutableArray s a -> Int -> (a -> a) -> ST s ()
alterSmallArray marr ix f = do
  x <- readSmallArray marr ix
  writeSmallArray marr ix $! f x

-- | The empty 'RadixVec'.
empty :: RadixVec a
empty = RadixVec 0 $ Tip $ runSmallArray $ newSmallArray 0 $ error
  "Cleff.Internal.RadixVec: Encountered an element in an empty RadixVec. Please report this as a bug."

-- | Lookup in a 'RadixVec' by an index. This does not perform any bounds check.
lookup :: Int -> RadixVec a -> a
lookup ix (RadixVec _ tree) = go tree
  where
    go (Tip arr)    = indexSmallArray arr (initialMask .&. ix)
    go (Node s arr) = go (indexSmallArray arr (mask s ix))

-- | Update a value in a 'RadixVec' by an index. The value will be forced before installing. This does not perform any
-- bounds check.
update :: Int -> a -> RadixVec a -> RadixVec a
update ix x (RadixVec len tree) = RadixVec len (go tree)
  where
    go (Tip arr) = Tip $ runSmallArray do
      marr <- thawSmallArray arr 0 (sizeofSmallArray arr)
      writeSmallArray marr (mask0 ix) $! x
      pure marr
    go (Node s arr) = Node s $ runSmallArray do
      marr <- thawSmallArray arr 0 (sizeofSmallArray arr)
      alterSmallArray marr (mask s ix) go
      pure marr

-- | Append a value to a 'RadixVec'. The value will be forced before installing. This does not perform any bounds check.
snoc :: RadixVec a -> a -> RadixVec a
snoc (RadixVec len tree) x
  | ins <= topShift = RadixVec (len + 1) (go tree)
  | otherwise = RadixVec (len + 1) $ Node (topShift + factor) $ runSmallArray $ do
    marr <- newSmallArray 2 $! tree
    writeSmallArray marr 1 $! branch topShift
    pure marr
  where
    topShift = case tree of
      Tip _    -> 0
      Node s _ -> s
    ins = (countTrailingZeros len `mod` sIZEOF_INT `div` factor) * factor
    branch 0 = Tip $ runSmallArray $ newSmallArray 1 $! x
    branch s = Node s $ runSmallArray $ newSmallArray 1 $! branch (s - factor)
    enlarge arr new = runSmallArray do
      let sz = sizeofSmallArray arr
      marr <- newSmallArray (sz + 1) $! new
      copySmallArray marr 0 arr 0 sz
      pure marr
    go (Tip arr) = Tip $ enlarge arr x
    go (Node s arr)
      | ins == s = Node s $ enlarge arr $ branch (s - factor)
      | otherwise = Node s $ runSmallArray do
        marr <- thawSmallArray arr 0 (sizeofSmallArray arr)
        alterSmallArray marr (mask s len) go
        pure marr
