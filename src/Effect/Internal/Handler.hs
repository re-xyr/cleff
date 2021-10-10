{-# OPTIONS_HADDOCK not-home #-}
module Effect.Internal.Handler
  ( -- * Trivial handling
    raise, raiseN, subsume, subsumeN
  , -- * Interpreting effects
    interpret, reinterpret, reinterpret2, reinterpret3, reinterpretN, interpose
  , -- * Combinators for handling higher effects
    runInIO, runThere, runHere, runHere', withLiftIO, withLiftEff
  ) where

import           Data.Typeable         (Typeable)
import           Effect.Internal.Monad

-- | Raise an action into a bigger effect environment. For a more general version see 'raiseN'.
raise :: forall e es a. Eff es a -> Eff (e ': es) a
raise = raiseN @'[e]
{-# INLINE raise #-}

-- | Raise an action into a bigger effect environment. This function requires @TypeApplications@.
raiseN :: forall es' es a. Eff es a -> Eff (es' ++ es) a
raiseN m = PrimEff (primRunEff m . contractEnv @es')
{-# INLINE raiseN #-}

-- | Trivially eliminate a duplicate effect on the effect stack. For a more general version see 'subsumeN'.
subsume :: forall e es a. e :> es => Eff (e ': es) a -> Eff es a
subsume = subsumeN @'[e]
{-# INLINE subsume #-}

-- | Trivially eliminate several duplicate effects on the effect stack. This function requires @TypeApplications@.
subsumeN :: forall es' es a. es' :>> es => Eff (es' ++ es) a -> Eff es a
subsumeN m = PrimEff (primRunEff m . expandEnv @es')
{-# INLINE subsumeN #-}

-- | Interpret an effect through an effect handler.
interpret :: forall e es a. Typeable e => Handler es e -> Eff (e ': es) a -> Eff es a
interpret = reinterpretN @'[]
{-# INLINE interpret #-}

-- | Reinterpret an effect in terms of another through an effect handler.
reinterpret :: forall e' e es a. Typeable e => Handler (e' ': es) e -> Eff (e ': es) a -> Eff (e' ': es) a
reinterpret = reinterpretN @'[e']
{-# INLINE reinterpret #-}

-- | Reinterpret an effect in terms of two other ones through an effect handler.
reinterpret2 :: forall e' e'' e es a. Typeable e => Handler (e' ': e'' ': es) e -> Eff (e ': es) a -> Eff (e' ': e'' ': es) a
reinterpret2 = reinterpretN @'[e', e'']
{-# INLINE reinterpret2 #-}

-- | Reinterpret an effect in terms of three other ones through an effect handler.
reinterpret3 :: forall e' e'' e''' e es a. Typeable e => Handler (e' ': e'' ': e''' ': es) e -> Eff (e ': es) a -> Eff (e' ': e'' ': e''' ': es) a
reinterpret3 = reinterpretN @'[e', e'', e''']
{-# INLINE reinterpret3 #-}

-- | Reinterpret an effect in terms of arbitrarily many other ones through an effect handler. This function requires
-- @TypeApplications@.
reinterpretN :: forall es' e es a. Typeable e => Handler (es' ++ es) e -> Eff (e ': es) a -> Eff (es' ++ es) a
reinterpretN handle m = PrimEff \handlers ->
  let handler = InternalHandler \eff -> primRunEff (handle eff) handlers
  in primRunEff m $ insertHandler handler $ contractEnv @es' handlers
{-# INLINE reinterpretN #-}

-- | Respond to an effect while being able to leave it unhandled (i.e. you can resend the effects in the handler).
interpose :: forall e es a. e :> es => Handler es e -> Eff es a -> Eff es a
interpose handle = interpret handle . raise
{-# INLINE interpose #-}

-- | Run a send-site action in the 'IO' monad. This is useful when interpreting an effect in terms of 'Effect.IOE'.
runInIO :: Originating es => Eff es a -> IO a
runInIO m = primRunEff m originatingEnv

-- | Run a send-site action with the original send-site environment. This is useful when interpreting effects that do
-- not have local contexts.
runThere :: forall es es' a. Originating es' => Eff es' a -> Eff es a
runThere m = PrimEff $ const $ primRunEff m originatingEnv

-- | Run a send-site action, but allowing an effect to be interpreted differently. This is useful when interpreting
-- effects that have local contexts.
runHere :: forall e es es' a. (Originating es', Typeable e) => Eff es' a -> Eff (e ': es) a
runHere = runHere' @e

-- | Run a send-site action, but allowing an effect to be interpreted differently. This is a more general version of
-- 'runHere' that works better with 'interpose' but requires @TypeApplications@.
runHere' :: forall e es es' a. (Originating es', e :> es) => Eff es' a -> Eff es a
runHere' m = PrimEff \es -> primRunEff m (contractEnv @'[e] $ insertHandler (getHandler es) originatingEnv)

-- | Temporarily gain the ability to lift arbitrary 'IO' actions into 'Eff' as long as an 'IO' action is finally
-- returned. This is useful for dealing with effect operations with the monad type in the negative position within
-- 'Effect.IOE'.
withLiftIO :: forall es a. ((forall x. IO x -> Eff es x) -> IO a) -> IO a
withLiftIO f = f (PrimEff . const)

-- | Temporarily gain the ability to lift some @'Eff' es@ actions into some other @'Eff' es'@ as long as an @'Eff' es@
-- is finally returned. This is useful for dealing with effect operations with the monad type in the negative position.
withLiftEff :: forall es es' a. ((forall x. Eff es x -> Eff es' x) -> Eff es a) -> Eff es a
withLiftEff f = PrimEff \handlers -> primRunEff (f \m -> PrimEff \_ -> primRunEff m handlers) handlers
