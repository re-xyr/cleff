module Cleff.Time where

import           Cleff
import           Data.Time (UTCTime)
import qualified Data.Time as T

data Time :: Effect where
  GetCurrentTime :: Time m UTCTime
makeEffect ''Time

runCurrentTimeIO :: IOE :> es => Eff (Time ': es) a -> Eff es a
runCurrentTimeIO = interpretIO \case
  GetCurrentTime -> T.getCurrentTime
{-# INLINE runCurrentTimeIO #-}

runCurrentTimePure :: UTCTime -> Eff (Time ': es) a -> Eff es a
runCurrentTimePure t = interpret \case
  GetCurrentTime -> pure t
{-# INLINE runCurrentTimePure #-}
