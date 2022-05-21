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
module Cleff.Internal.Vec (Vec, empty, lookup, update, snoc) where

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
data Vec a = Vec !Int !(Tree a)

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

-- | The empty 'Vec'.
empty :: Vec a
empty = Vec 0 $ Tip $ runSmallArray $ newSmallArray 0 $ error
  "Cleff.Internal.Vec: Encountered an element in an empty Vec. Please report this as a bug."

-- | Lookup in a 'Vec' by an index. This does not perform any bounds check.
lookup :: Int -> Vec a -> a
lookup ix (Vec _ tree) = go tree
  where
    go (Tip arr)    = indexSmallArray arr (initialMask .&. ix)
    go (Node s arr) = go (indexSmallArray arr (mask s ix))

-- | Update a value in a 'Vec' by an index. The value will be forced before installing. This does not perform any
-- bounds check.
update :: Int -> a -> Vec a -> Vec a
update ix x (Vec len tree) = Vec len (go tree)
  where
    go (Tip arr) = Tip $ runSmallArray do
      marr <- thawSmallArray arr 0 (sizeofSmallArray arr)
      writeSmallArray marr (mask0 ix) $! x
      pure marr
    go (Node s arr) = Node s $ runSmallArray do
      marr <- thawSmallArray arr 0 (sizeofSmallArray arr)
      alterSmallArray marr (mask s ix) go
      pure marr

-- | Append a value to a 'Vec'. The value will be forced before installing. This does not perform any bounds check.
snoc :: Vec a -> a -> Vec a
snoc (Vec len tree) x
  | ins <= topShift = Vec (len + 1) (go tree)
  | otherwise = Vec (len + 1) $ Node (topShift + factor) $ runSmallArray $ do
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
