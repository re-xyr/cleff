module Effect.Primitive.IO where

import           Effect.Internal.Monad

primLiftIO :: IO a -> Eff es a
primLiftIO m = PrimEff $ const m
{-# INLINE primLiftIO #-}

primUnliftIO :: ((forall x. Eff es x -> IO x) -> IO a) -> Eff es a
primUnliftIO f = PrimEff \es -> f (`primRunEff` es)
{-# INLINE primUnliftIO #-}
