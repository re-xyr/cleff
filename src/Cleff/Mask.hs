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
    -- * Operations
  , mask
  , uninterruptibleMask
  , bracket
  , bracketOnError
    -- ** Simplified versions
  , mask_
  , uninterruptibleMask_
  , bracket_
  , finally
  , onError
    -- * Interpretations
  , runMask
  ) where

import           Cleff
import           Cleff.Internal.Base
import qualified UnliftIO.Exception  as Exc

-- * Effect

-- | An effect capable of 'Exc.mask'ing and specifically, 'Exc.bracket'ing operations, /i.e./ allowing cleanup after
-- operations that may raise exceptions.
data Mask :: Effect where
  Mask :: ((m ~> m) -> m a) -> Mask m a
  UninterruptibleMask :: ((m ~> m) -> m a) -> Mask m a
  Bracket :: m a -> (a -> m c) -> (a -> m b) -> Mask m b
  BracketOnError :: m a -> (a -> m c) -> (a -> m b) -> Mask m b

-- * Operations

makeEffect_ ''Mask

-- | Prevents a computation from receiving asynchronous exceptions, i.e. being interrupted by another thread. Also
-- provides a function to restore receiving async exceptions for a computation. See 'Control.Exception.mask' for
-- details.
mask :: Mask :> es => ((Eff es ~> Eff es) -> Eff es a) -> Eff es a

-- | Prevents a computation from receiving asynchronous exceptions, even if there is an interruptible operation
-- (operations that potentially deadlocks or otherwise blocks indefinitely). Therefore this function is potentially
-- dangerous in the sense that it can make a thread both unresponsive and unkillable, and you should prefer 'mask'
-- whenever you can. See 'Control.Exception.uninterruptibleMask' for details.
uninterruptibleMask :: Mask :> es => ((Eff es ~> Eff es) -> Eff es a) -> Eff es a

-- | Run a computation that acquires a resource (@alloc@), then a main computation using that resource, then a cleanup
-- computation (@dealloc@). 'bracket' guarantees that @alloc@ and @dealloc@ will always run, regardless of whether an
-- exception is thrown in the main computation. Note that if an exception is thrown in the main computation, it will
-- be rethrown after 'bracket' finishes.
bracket :: Mask :> es
  => Eff es a -- ^ The computation to run first, usually acquires a resource
  -> (a -> Eff es c) -- ^ The computation to run after the main computation, usually cleans up
  -> (a -> Eff es b) -- ^ The main computation that uses the resource
  -> Eff es b

-- | Like 'bracket', but only runs cleanup if an exception is thrown in the main computation.
bracketOnError :: Mask :> es
  => Eff es a -- ^ The computation to run first, usually acquires a resource
  -> (a -> Eff es c) -- ^ The computation to run when the main computation throws an exception, usually cleans up
  -> (a -> Eff es b) -- ^ The main computation that uses the resource
  -> Eff es b

-- | Variant of 'mask' that does not provide a restoring function.
mask_ :: Mask :> es => Eff es a -> Eff es a
mask_ m = mask \_ -> m

-- | Variant of 'uninterruptibleMask' that does not provide a restoring function.
uninterruptibleMask_ :: Mask :> es => Eff es a -> Eff es a
uninterruptibleMask_ m = uninterruptibleMask \_ -> m

-- | Variant of 'bracket' that does not pass the allocated resource to the cleanup action.
bracket_ :: Mask :> es => Eff es a -> Eff es c -> (a -> Eff es b) -> Eff es b
bracket_ ma = bracket ma . const

-- | Attach a cleanup action that will always run after a potentially throwing computation.
finally :: Mask :> es
  => Eff es a -- ^ The main computation that may throw an exception
  -> Eff es b -- ^ The computation that runs after the main computation, regardless of whether an exception is thrown
  -> Eff es a
finally m mz = bracket_ (pure ()) mz (const m)

-- | Attach an action that runs if the main computation throws an exception. Note that this will rethrow the exception.
onError :: Mask :> es
  => Eff es a -- ^ The main computation that may throw an exception
  -> Eff es b -- ^ The computation that runs when an exception is thrown
  -> Eff es a
onError m mz = bracketOnError (pure ()) (const mz) (const m)

-- * Interpretations

-- | Interpret the 'Mask' effect in terms of primitive 'IO' actions.
runMask :: Eff (Mask ': es) ~> Eff es
runMask = thisIsPureTrustMe . reinterpret \case
  Mask f                 -> withToIO \toIO -> Exc.mask \restore -> toIO $ f (fromIO . restore . toIO)
  UninterruptibleMask f  -> withToIO \toIO -> Exc.uninterruptibleMask \restore -> toIO $ f (fromIO . restore . toIO)
  Bracket ma mz m        -> withToIO \toIO -> Exc.bracket (toIO ma) (toIO . mz) (toIO . m)
  BracketOnError ma mz m -> withToIO \toIO -> Exc.bracketOnError (toIO ma) (toIO . mz) (toIO . m)
{-# INLINE runMask #-}
