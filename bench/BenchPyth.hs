module BenchPyth where

import qualified Control.Ev.Eff  as E
import qualified Control.Ev.Util as E
import qualified Sp.Eff          as S
import qualified Sp.Util         as S

programSp :: (S.NonDet S.:> e) => Int -> S.Eff e (Int, Int, Int)
programSp upbound = do
  x <- S.choice [1..upbound]
  y <- S.choice [1..upbound]
  z <- S.choice [1..upbound]
  if (x*x + y*y == z*z) then return (x,y,z) else S.send S.Empty
{-# NOINLINE programSp #-}

pythSp :: Int -> [(Int, Int, Int)]
pythSp n = S.runEff $ S.runNonDet $ programSp n

programEv :: (E.Choose E.:? e) => Int -> E.Eff e (Int, Int, Int)
programEv upbound = do
  x <- E.perform E.choose upbound
  y <- E.perform E.choose upbound
  z <- E.perform E.choose upbound
  if (x*x + y*y == z*z) then return (x,y,z) else E.perform E.none ()
{-# NOINLINE programEv #-}

pythEv :: Int -> [(Int, Int, Int)]
pythEv n = E.runEff $ E.chooseAll $ programEv n
