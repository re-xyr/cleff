module Effect.Internal.Handler where

import           Data.Typeable         (Typeable)
import           Effect.Internal.Monad

raise :: forall e es a. Eff es a -> Eff (e ': es) a
raise = raiseN @'[e]
{-# INLINE raise #-}

raiseN :: forall es' es a. Eff es a -> Eff (es' ++ es) a
raiseN m = PrimEff (primRunEff m . contractEnv @es')
{-# INLINE raiseN #-}

subsume :: forall e es a. e :> es => Eff (e ': es) a -> Eff es a
subsume = subsumeN @'[e]
{-# INLINE subsume #-}

subsumeN :: forall es' es a. es' :>> es => Eff (es' ++ es) a -> Eff es a
subsumeN m = PrimEff (primRunEff m . expandEnv @es')
{-# INLINE subsumeN #-}

type Recursive a = a -> a

interpret :: forall e es a. Typeable e => Handler es e -> Eff (e ': es) a -> Eff es a
interpret = reinterpretN @'[]
{-# INLINE interpret #-}

reinterpret :: forall e' e es a. Typeable e => Handler (e' ': es) e -> Eff (e ': es) a -> Eff (e' ': es) a
reinterpret = reinterpretN @'[e']
{-# INLINE reinterpret #-}

reinterpret2 :: forall e' e'' e es a. Typeable e => Handler (e' ': e'' ': es) e -> Eff (e ': es) a -> Eff (e' ': e'' ': es) a
reinterpret2 = reinterpretN @'[e', e'']
{-# INLINE reinterpret2 #-}

reinterpret3 :: forall e' e'' e''' e es a. Typeable e => Handler (e' ': e'' ': e''' ': es) e -> Eff (e ': es) a -> Eff (e' ': e'' ': e''' ': es) a
reinterpret3 = reinterpretN @'[e', e'', e''']
{-# INLINE reinterpret3 #-}

reinterpretN :: forall es' e es a. Typeable e => Handler (es' ++ es) e -> Eff (e ': es) a -> Eff (es' ++ es) a
reinterpretN handle m = PrimEff \handlers ->
  let handler = InternalHandler \eff -> primRunEff (handle eff) handlers
  in primRunEff m $ insertHandler handler $ contractEnv @es' handlers
{-# INLINE reinterpretN #-}

interpose :: forall e es a. e :> es => Handler es e -> Eff es a -> Eff es a
interpose handle = interpret handle . raise
{-# INLINE interpose #-}

withLiftIO :: forall es a. ((forall x. IO x -> Eff es x) -> IO a) -> IO a
withLiftIO f = f (PrimEff . const)

withLiftEff :: forall es es' a. ((forall x. Eff es x -> Eff es' x) -> Eff es a) -> Eff es a
withLiftEff f = PrimEff \handlers -> primRunEff (f \m -> PrimEff \_ -> primRunEff m handlers) handlers

runInIO :: Originating es => Eff es a -> IO a
runInIO m = primRunEff m originatingEnv

runThere :: forall es es' a. Originating es' => Eff es' a -> Eff es a
runThere m = PrimEff $ const $ primRunEff m originatingEnv

runHere :: forall e es es' a. (Originating es', Typeable e) => Eff es' a -> Eff (e ': es) a
runHere = runHere' @e

runHere' :: forall e es es' a. (Originating es', e :> es) => Eff es' a -> Eff es a
runHere' m = PrimEff \es -> primRunEff m (contractEnv @'[e] $ insertHandler (getHandler es) originatingEnv)
