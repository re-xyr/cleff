{-# LANGUAGE Trustworthy #-}
-- |
-- Copyright: (c) 2021 Xy Ren
-- License: BSD3
-- Maintainer: xy.r@outlook.com
-- Stability: experimental
-- Portability: non-portable (GHC only)
module Cleff.Mask
  ( -- * Effect
    Mask (..)
    -- * High-level operations
  , bracket
  , bracketOnError
  , bracket_
  , bracketOnError_
  , onError
  , finally
    -- * Primitive operations
  , mask
  , uninterruptibleMask
  , onException
  , mask_
  , uninterruptibleMask_
    -- * Interpretations
  , runMask
  ) where

import           Cleff
import           Cleff.Internal.Base
import qualified Control.Exception   as Exc

-- * Effect

-- | An effect capable of 'Exc.mask'ing and performing cleanup operations when an computation is interrupted. In
-- particular, this effects allows the use of 'bracket'.
--
-- === Technical details
--
-- Regarding the nuances of 'bracket' semantics, this effect uses the semantics of "UnliftIO.Exception" rather than
-- "Control.Exception". They are more sensible defaults and users can implement other semantics out of the primitive
-- operations if they want to.
data Mask :: Effect where
  Mask :: ((m ~> m) -> m a) -> Mask m a
  UninterruptibleMask :: ((m ~> m) -> m a) -> Mask m a
  OnException :: m a -> m b -> Mask m a

-- * Operations

makeEffect_ ''Mask

-- | Prevents a computation from receiving asynchronous exceptions, /i.e./ being interrupted by another thread. Also
-- provides a function to restore receiving async exceptions for a computation.
--
-- However, some potentially blocking actions like @takeMVar@ can still be interrupted, and for them also not to be
-- interrupted in any case you'll need 'uninterruptibleMask'. See 'Control.Exception.mask' for details.
mask :: Mask :> es => ((Eff es ~> Eff es) -> Eff es a) -> Eff es a

-- | Prevents a computation from receiving asynchronous exceptions, even if there is an interruptible operation
-- (operations that potentially deadlocks or otherwise blocks indefinitely). Therefore this function is potentially
-- dangerous in the sense that it can make a thread both unresponsive and unkillable. See
-- 'Control.Exception.uninterruptibleMask' for details.
uninterruptibleMask :: Mask :> es => ((Eff es ~> Eff es) -> Eff es a) -> Eff es a

-- | Like 'onError', but without 'uninterruptibleMask'ing the cleanup action, making it possible that a cleanup action
-- is interrupted. Use 'onError' is usually the safer option.
onException :: Mask :> es
  => Eff es a -- ^ The main computation that may throw an exception
  -> Eff es b -- ^ The computation that runs when an exception is thrown
  -> Eff es a

-- | Run a computation that acquires a resource (@alloc@), then a main computation using that resource, then a cleanup
-- computation (@dealloc@). 'bracket' guarantees that @alloc@ and @dealloc@ will always run, regardless of whether an
-- exception is thrown in the main computation. Note that if an exception is thrown in the main computation, it will
-- be rethrown after 'bracket' finishes.
--
-- === Technical details
--
-- Note that this function uses @unliftio@ semantics: resource acquiring action is interruptibly 'mask'ed while
-- resource cleanup is 'uninterruptibleMask'ed. Most of the times, this will be what you want. Other functions in this
-- module use @unliftio@ semantics too.
bracket :: Mask :> es
  => Eff es a -- ^ The computation to run first, usually acquires a resource
  -> (a -> Eff es c) -- ^ The computation to run after the main computation, usually cleans up
  -> (a -> Eff es b) -- ^ The main computation that uses the resource
  -> Eff es b
bracket alloc dealloc action = mask \restore -> do
  res <- alloc
  ret <- restore (action res) `onError` dealloc res
  _ <- uninterruptibleMask_ (dealloc res)
  pure ret

-- | Like 'bracket', but only runs cleanup if an exception is thrown in the main computation.
bracketOnError :: Mask :> es
  => Eff es a -- ^ The computation to run first, usually acquires a resource
  -> (a -> Eff es c) -- ^ The computation to run when the main computation throws an exception, usually cleans up
  -> (a -> Eff es b) -- ^ The main computation that uses the resource
  -> Eff es b
bracketOnError alloc dealloc action = mask \restore -> do
  res <- alloc
  restore (action res) `onError` dealloc res

-- | Variant of 'mask' that does not provide a restoring function.
mask_ :: Mask :> es => Eff es a -> Eff es a
mask_ m = mask \_ -> m

-- | Variant of 'uninterruptibleMask' that does not provide a restoring function.
uninterruptibleMask_ :: Mask :> es => Eff es a -> Eff es a
uninterruptibleMask_ m = uninterruptibleMask \_ -> m

-- | Variant of 'bracket' that does not pass the allocated resource to the cleanup action.
bracket_ :: Mask :> es => Eff es a -> Eff es c -> (a -> Eff es b) -> Eff es b
bracket_ ma = bracket ma . const

-- | Variant of 'bracketOnError' that does not pass the allocated resource to the cleanup action.
bracketOnError_ :: Mask :> es => Eff es a -> Eff es c -> (a -> Eff es b) -> Eff es b
bracketOnError_ ma = bracketOnError ma . const

-- | Attach an action that runs if the main computation throws an exception. Note that this will rethrow the exception
-- instead of returning to normal control flow.
--
-- The cleanup action is guaranteed not to be interrupted halfways.
onError :: Mask :> es
  => Eff es a -- ^ The main computation that may throw an exception
  -> Eff es b -- ^ The computation that runs when an exception is thrown
  -> Eff es a
onError m n = m `onException` uninterruptibleMask_ n

-- | Attach a cleanup action that will always run after a potentially throwing computation.
finally :: Mask :> es
  => Eff es a -- ^ The main computation that may throw an exception
  -> Eff es b -- ^ The computation that runs after the main computation, regardless of whether an exception is thrown
  -> Eff es a
finally m mz = (m `onError` mz) <* uninterruptibleMask_ mz

-- * Interpretations

-- | Interpret the 'Mask' effect in terms of primitive 'IO' actions.
runMask :: Eff (Mask : es) ~> Eff es
runMask = thisIsPureTrustMe . reinterpret \case
  Mask f -> withToIO \toIO -> Exc.mask \restore -> toIO $ f (fromIO . restore . toIO)
  UninterruptibleMask f -> withToIO \toIO -> Exc.uninterruptibleMask \restore -> toIO $ f (fromIO . restore . toIO)
  OnException m n -> withToIO \toIO -> toIO m `Exc.catch` \(e :: Exc.SomeException) ->
    Exc.try @Exc.SomeException (toIO n) *> Exc.throwIO e
{-# INLINE runMask #-}
