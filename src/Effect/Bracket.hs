module Effect.Bracket where

import           Effect
import           Effect.Primitive.Exception

data Bracket :: Effect where
  Bracket :: m a -> (a -> m c) -> (a -> m b) -> Bracket m b
  BracketOnError :: m a -> (a -> m c) -> (a -> m b) -> Bracket m b

bracket :: Bracket :> es => Eff es a -> (a -> Eff es c) -> (a -> Eff es b) -> Eff es b
bracket ma mz m = send $ Bracket ma mz m
{-# INLINE bracket #-}

bracket_ :: Bracket :> es => Eff es a -> Eff es c -> (a -> Eff es b) -> Eff es b
bracket_ ma mz = bracket ma (const mz)
{-# INLINE bracket_ #-}

bracketOnError :: Bracket :> es => Eff es a -> (a -> Eff es c) -> (a -> Eff es b) -> Eff es b
bracketOnError ma mz m = send $ BracketOnError ma mz m
{-# INLINE bracketOnError #-}

finally :: Bracket :> es => Eff es a -> Eff es b -> Eff es a
finally m mz = bracket_ (pure ()) mz (const m)
{-# INLINE finally #-}

onError :: Bracket :> es => Eff es a -> Eff es b -> Eff es a
onError m mz = bracketOnError (pure ()) (const mz) (const m)
{-# INLINE onError #-}

runBracket :: forall es a. Eff (Bracket ': es) a -> Eff es a
runBracket = interpret \case
  Bracket ma mz m        -> unlift $ primBracket ma mz m
  BracketOnError ma mz m -> unlift $ primBracketOnError ma mz m
