module BenchCountdown where

import qualified Control.Ev.Eff  as E
import qualified Control.Ev.Util as E
import qualified Sp.Eff          as S
import qualified Sp.Util         as S

programSp :: S.State Int S.:> es => S.Eff es Int
programSp = do
  x <- S.get @Int
  if x == 0
    then pure x
    else do
      S.put (x - 1)
      programSp
{-# NOINLINE programSp #-}

countdownSp :: Int -> (Int, Int)
countdownSp n = S.runEff $ S.runState n programSp

countdownSpDeep :: Int -> (Int, Int)
countdownSpDeep n = S.runEff $ runR $ runR $ runR $ runR $ runR $ S.runState n $ runR $ runR $ runR $ runR $ runR $ programSp
  where runR = S.runReader ()

programEv :: E.State Int E.:? es => E.Eff es Int
programEv = do
  x <- E.perform (E.get @Int) ()
  if x == 0
    then pure x
    else do
      E.perform E.put (x - 1)
      programEv
{-# NOINLINE programEv #-}

countdownEv :: Int -> Int
countdownEv n = E.runEff $ E.state n programEv

countdownEvDeep :: Int -> Int
countdownEvDeep n = E.runEff $ runR $ runR $ runR $ runR $ runR $ E.state n $ runR $ runR $ runR $ runR $ runR $ programEv
  where runR = E.reader ()
