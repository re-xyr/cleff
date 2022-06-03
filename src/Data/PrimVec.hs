{-# LANGUAGE UnboxedTuples #-}
{-# OPTIONS_HADDOCK not-home #-}
-- |
-- Copyright: (c) 2021 Xy Ren
-- License: BSD3
-- Maintainer: xy.r@outlook.com
-- Stability: unstable
-- Portability: non-portable (GHC only)
module Data.PrimVec (PrimVec, empty, head, tail, take, drop, index, cons, concat, pick, update) where

import           Data.Foldable            (for_)
import           Data.Primitive           (Prim, indexPrimArray)
import           Data.Primitive.PrimArray (MutablePrimArray (MutablePrimArray), PrimArray (PrimArray), copyPrimArray,
                                           newPrimArray, writePrimArray)
import           GHC.Exts                 (runRW#, unsafeFreezeByteArray#)
import           GHC.ST                   (ST (ST))
import           Prelude                  hiding (concat, drop, head, tail, take)

-- | Slices of 'PrimArray'.
data PrimVec a = PrimVec
  {-# UNPACK #-} !Int
  {-# UNPACK #-} !Int
  {-# UNPACK #-} !(PrimArray a)

runPrimArray :: (∀ s. ST s (MutablePrimArray s a)) -> PrimArray a
runPrimArray (ST f) = let
  !(# _, ba# #) = runRW# \s1 ->
    let !(# s2, MutablePrimArray mba# #) = f s1
    in unsafeFreezeByteArray# mba# s2
  in PrimArray ba#

empty :: Prim a => PrimVec a
empty = PrimVec 0 0 $ runPrimArray $ newPrimArray 0

head :: Prim a => PrimVec a -> a
head (PrimVec off _ arr) = indexPrimArray arr off

tail :: PrimVec a -> PrimVec a
tail (PrimVec off len arr) = PrimVec (off + 1) (len - 1) arr

take :: Prim a => Int -> PrimVec a -> PrimVec a
take n (PrimVec off _ arr) = PrimVec 0 n $ runPrimArray do
  marr <- newPrimArray n
  copyPrimArray marr 0 arr off n
  pure marr

drop :: Int -> PrimVec a -> PrimVec a
drop n (PrimVec off len arr) = PrimVec (off + n) (len - n) arr

index :: Prim a => Int -> PrimVec a -> a
index n (PrimVec off _ arr) = indexPrimArray arr (off + n)

cons :: Prim a => a -> PrimVec a -> PrimVec a
cons x (PrimVec off len arr) = PrimVec 0 (len + 1) $ runPrimArray do
  marr <- newPrimArray (len + 1)
  writePrimArray marr 0 x
  copyPrimArray marr 1 arr off len
  pure marr

concat :: Prim a => PrimVec a -> PrimVec a -> PrimVec a
concat (PrimVec off len arr) (PrimVec off' len' arr') = PrimVec 0 (len + len') $ runPrimArray do
  marr <- newPrimArray (len + len')
  copyPrimArray marr 0 arr off len
  copyPrimArray marr len arr' off' len'
  pure marr

pick :: Prim a => Int -> [Int] -> PrimVec a -> PrimVec a
pick len' ns (PrimVec off _ arr) = PrimVec 0 len' $ runPrimArray do
  marr <- newPrimArray len'
  for_ (zip [0 ..] ns) \(new, old) ->
    writePrimArray marr new (indexPrimArray arr (off + old))
  pure marr

update :: Prim a => Int -> a -> PrimVec a -> PrimVec a
update n x (PrimVec off len arr) = PrimVec 0 len $ runPrimArray do
  marr <- newPrimArray len
  copyPrimArray marr 0 arr off len
  writePrimArray marr n x
  pure marr
