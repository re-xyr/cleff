module Cleff.Input where

import           Cleff
import           Cleff.State
import           Data.Typeable (Typeable)

data Input i :: Effect where
  Input :: Input i m i
makeEffect ''Input

inputs :: Input i :> es => (i -> i') -> Eff es i'
inputs f = f <$> input

runInputConst :: Typeable i => i -> Eff (Input i ': es) a -> Eff es a
runInputConst x = interpret \case
  Input -> pure x
{-# INLINE runInputConst #-}

inputToListState :: Typeable i => Eff (Input (Maybe i) ': es) a -> Eff (State [i] ': es) a
inputToListState = reinterpret \case
  Input -> get >>= \case
    []      -> pure Nothing
    x : xs' -> Just x <$ put xs'
{-# INLINE inputToListState #-}

runInputEff :: Typeable i => Eff es i -> Eff (Input i ': es) a -> Eff es a
runInputEff m = interpret \case
  Input -> m
{-# INLINE runInputEff #-}
