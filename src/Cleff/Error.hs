{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE Trustworthy         #-}
-- |
-- Copyright: (c) 2021 Xy Ren
-- License: BSD3
-- Maintainer: xy.r@outlook.com
-- Stability: experimental
-- Portability: non-portable (GHC only)
module Cleff.Error
  ( -- * Effect
    Error (..)
    -- * Operations
  , throwError
  , catchError
    -- ** Other ways of throwing errors
  , fromEither
  , fromException
  , fromExceptionVia
  , fromExceptionEff
  , fromExceptionEffVia
  , note
    -- ** Other ways of handling errors
  , catchErrorJust
  , catchErrorIf
  , handleError
  , handleErrorJust
  , handleErrorIf
  , tryError
  , tryErrorJust
    -- * Interpretations
  , runError
  , mapError
  ) where

import           Cleff
import           Cleff.Internal.Any
import           Cleff.Internal.Base
import           Control.Exception    (Exception)
import           Data.Atomics.Counter (AtomicCounter, incrCounter, newCounter)
import           Data.Bool            (bool)
import           System.IO.Unsafe     (unsafePerformIO)
import qualified UnliftIO.Exception   as Exc

-- * Effect

-- | An effect capable of breaking out of current control flow by throwing an error of type @e@, and handling the
-- errors thrown from computations. This effect roughly corresponds to the @MonadError@ typeclass and @ExceptT@ monad
-- transformer in @mtl@.
data Error e :: Effect where
  ThrowError :: e -> Error e m a
  CatchError :: m a -> (e -> m a) -> Error e m a

-- * Operations

makeEffect_ ''Error

-- | Throw an error in the current computation.
throwError :: Error e :> es => e -> Eff es a

-- | Handle an error if one is thrown from a computation, and then return to normal control flow.
catchError :: Error e :> es
  => Eff es a -- ^ The computation that may throw errors
  -> (e -> Eff es a) -- ^ The handler that is called when an error is thrown
  -> Eff es a

-- | Lift an 'Either' value into the 'Error' effect.
fromEither :: Error e :> es => Either e a -> Eff es a
fromEither = either throwError pure

-- | Lift exceptions generated by an 'IO' computation into the 'Error' effect.
fromException :: ∀ e es a. (Exception e, '[Error e, IOE] :>> es) => IO a -> Eff es a
fromException m = Exc.catch (liftIO m) (throwError @e)

-- | Like 'fromException', but allows to transform the exception into another error type.
fromExceptionVia :: (Exception ex, '[Error er, IOE] :>> es) => (ex -> er) -> IO a -> Eff es a
fromExceptionVia f m = Exc.catch (liftIO m) (throwError . f)

-- | Lift exceptions generated by an 'Eff' computation into the 'Error' effect.
fromExceptionEff :: ∀ e es a. (Exception e, '[Error e, IOE] :>> es) => Eff es a -> Eff es a
fromExceptionEff m = withRunInIO \unlift -> Exc.catch (unlift m) (unlift . throwError @e)

-- | Like 'fromExceptionEff', but allows to transform the exception into another error type.
fromExceptionEffVia :: (Exception ex, '[Error er, IOE] :>> es) => (ex -> er) -> Eff es a -> Eff es a
fromExceptionEffVia f m = withRunInIO \unlift -> Exc.catch (unlift m) (unlift . throwError . f)

-- | Try to extract a value from 'Maybe', throw an error otherwise.
note :: Error e :> es => e -> Maybe a -> Eff es a
note e = maybe (throwError e) pure

-- | A variant of 'catchError' that allows a predicate to choose whether to catch ('Just') or rethrow ('Nothing') the
-- error.
catchErrorJust :: Error e :> es => (e -> Maybe b) -> Eff es a -> (b -> Eff es a) -> Eff es a
catchErrorJust f m h = m `catchError` \e -> maybe (throwError e) h $ f e

-- | A variant of 'catchError' that allows a predicate to choose whether to catch ('True') or rethrow ('False') the
-- error.
catchErrorIf :: Error e :> es => (e -> Bool) -> Eff es a -> (e -> Eff es a) -> Eff es a
catchErrorIf f m h = m `catchError` \e -> bool (throwError e) (h e) $ f e

-- | Flipped version of 'catchError'.
handleError :: Error e :> es => (e -> Eff es a) -> Eff es a -> Eff es a
handleError = flip catchError

-- | Flipped version of 'catchErrorJust'.
handleErrorJust :: Error e :> es => (e -> Maybe b) -> (b -> Eff es a) -> Eff es a -> Eff es a
handleErrorJust = flip . catchErrorJust

-- | Flipped version of 'catchErrorIf'.
handleErrorIf :: Error e :> es => (e -> Bool) -> (e -> Eff es a) -> Eff es a -> Eff es a
handleErrorIf = flip . catchErrorIf

-- | Runs a computation, returning a 'Left' value if an error was thrown.
tryError :: Error e :> es => Eff es a -> Eff es (Either e a)
tryError m = (Right <$> m) `catchError` (pure . Left)

-- | A variant of 'tryError' that allows a predicate to choose whether to catch ('True') or rethrow ('False') the
-- error.
tryErrorJust :: Error e :> es => (e -> Maybe b) -> Eff es a -> Eff es (Either b a)
tryErrorJust f m = (Right <$> m) `catchError` \e -> maybe (throwError e) (pure . Left) $ f e

-- * Interpretations

type ExcUid = Int

-- | Exception wrapper used in 'runError' in order not to conflate error types with exception types.
data ErrorExc = ErrorExc {-# UNPACK #-} !ExcUid Any
  deriving anyclass (Exception)

instance Show ErrorExc where
  showsPrec _ (ErrorExc uid _) =
    ("Cleff.Error.runError: Escaped error (error UID: " <>) . shows uid . ("). This is possibly due \
    \to trying to 'throwError' in a forked thread, or trying to 'wait' on an error-throwing 'Async' computation out \
    \of the effect scope where it is created. Refer to the haddock of 'runError' for details on the caveats. If all \
    \those shenanigans mentioned or other similar ones seem unlikely, please report this as a bug." <>)

catch' :: ∀ e m a. MonadUnliftIO m => ExcUid -> m a -> (e -> m a) -> m a
catch' eid m h = m `Exc.catch` \ex@(ErrorExc eid' e) ->
  if eid == eid' then h (fromAny e) else Exc.throwIO ex
{-# INLINE catch' #-}

try' :: ∀ e m a. MonadUnliftIO m => ExcUid -> m a -> m (Either e a)
try' eid m = catch' eid (Right <$> m) (pure . Left)
{-# INLINE try' #-}

excUidSource :: AtomicCounter
excUidSource = unsafePerformIO (newCounter 0)
{-# NOINLINE excUidSource #-}

newExcUid :: IO ExcUid
newExcUid = incrCounter 1 excUidSource
{-# INLINE newExcUid #-}

errorHandler :: ExcUid -> Handler (Error e) (IOE : es)
errorHandler eid = \case
  ThrowError e     -> Exc.throwIO $ ErrorExc eid (toAny e)
  CatchError m' h' -> withToIO \toIO -> liftIO $ catch' eid (toIO m') (toIO . h')
{-# INLINE errorHandler #-}

-- | Run an 'Error' effect.
--
-- === Caveats
--
-- 'runError' is implemented with 'Exc.Exception's therefore inherits some of its unexpected behaviors.
-- Errors thrown in forked threads will /not/ be directly caught by 'catchError's in the parent thread. Instead it will
-- incur an exception, and we won't be quite able to display the details of that exception properly at that point.
-- Therefore please properly handle the errors in the forked threads separately.
--
-- However if you use @async@ and @wait@ for the action in the same effect scope (i.e. they get to be interpreted by
-- the same 'runError' handler), the error /will/ be caught in the parent thread even if you don't deal with it in the
-- forked thread. But if you passed the @Async@ value out of the effect scope and @wait@ed for it elsewhere, the error
-- will again not be caught. The best choice is /not to pass @Async@ values around randomly/.
runError :: ∀ e es a. Eff (Error e : es) a -> Eff es (Either e a)
runError m = thisIsPureTrustMe do
  eid <- liftIO newExcUid
  try' eid $ reinterpret (errorHandler eid) m
{-# INLINE runError #-}

-- | Transform an 'Error' into another. This is useful for aggregating multiple errors into one type.
mapError :: ∀ e e' es. Error e' :> es => (e -> e') -> Eff (Error e : es) ~> Eff es
mapError f = thisIsPureTrustMe . reinterpret \case
  ThrowError e   -> throwError $ f e
  CatchError m h -> do
    eid <- liftIO newExcUid
    res <- try' @e eid $ toEffWith (errorHandler eid) m
    case res of
      Left e  -> toEff (h e)
      Right a -> pure a
{-# INLINE mapError #-}
