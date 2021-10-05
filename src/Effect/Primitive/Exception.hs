module Effect.Primitive.Exception where

import           Control.Exception
import           Effect.Internal.Monad
import           Effect.Primitive.IO

primThrow :: Exception e => e -> Eff es a
primThrow e = primLiftIO $ throwIO e
{-# INLINE primThrow #-}

primCatch :: Exception e => Eff es a -> (e -> Eff es a) -> Eff es a
primCatch m h = primUnliftIO \unlift -> catch (unlift m) (unlift . h)
{-# INLINE primCatch #-}

primTry :: Exception e => Eff es a -> Eff es (Either e a)
primTry m = primUnliftIO \unlift -> try (unlift m)
{-# INLINE primTry #-}

primBracket :: Eff es a -> (a -> Eff es b) -> (a -> Eff es c) -> Eff es c
primBracket a r m = primUnliftIO \unlift -> bracket (unlift a) (unlift . r) (unlift . m)
{-# INLINE primBracket #-}

primBracketOnError :: Eff es a -> (a -> Eff es b) -> (a -> Eff es c) -> Eff es c
primBracketOnError a r m = primUnliftIO \unlift -> bracketOnError (unlift a) (unlift . r) (unlift . m)
{-# INLINE primBracketOnError #-}

primMask :: ((forall x. Eff es x -> Eff es x) -> Eff es a) -> Eff es a
primMask f = primUnliftIO \unlift -> mask \restore -> unlift $ f (primLiftIO . restore . unlift)
{-# INLINE primMask #-}

primUninterruptibleMask :: ((forall x. Eff es x -> Eff es x) -> Eff es a) -> Eff es a
primUninterruptibleMask f = primUnliftIO \unlift -> uninterruptibleMask \restore -> unlift $ f (primLiftIO . restore . unlift)
{-# INLINE primUninterruptibleMask #-}
