module Effect.Internal.Handler where

import           Data.Reflection       (Given (given), give)
import           Effect.Internal.Monad
import           System.IO.Unsafe      (unsafePerformIO)

interpret :: forall e es a. Legit e => Handler es e -> Eff (e ': es) a -> Eff es a
interpret = reinterpretN @'[]
{-# INLINE interpret #-}

reinterpret :: forall e' e es a. Legit e => Handler (e' ': es) e -> Eff (e ': es) a -> Eff (e' ': es) a
reinterpret = reinterpretN @'[e']
{-# INLINE reinterpret #-}

reinterpret2 :: forall e' e'' e es a. Legit e => Handler (e' ': e'' ': es) e -> Eff (e ': es) a -> Eff (e' ': e'' ': es) a
reinterpret2 = reinterpretN @'[e', e'']
{-# INLINE reinterpret2 #-}

reinterpret3 :: forall e' e'' e''' e es a. Legit e => Handler (e' ': e'' ': e''' ': es) e -> Eff (e ': es) a -> Eff (e' ': e'' ': e''' ': es) a
reinterpret3 = reinterpretN @'[e', e'', e''']
{-# INLINE reinterpret3 #-}

reinterpretN :: forall es' e es a. Legit e => Handler (es' ++ es) e -> Eff (e ': es) a -> Eff (es' ++ es) a
reinterpretN handle m = PrimEff \handlers ->
  let handler = InternalHandler \eff -> primRunEff (handle eff) handlers
  in primRunEff m $ insertHandler handler $ contractEnv @es' handlers

interpose :: forall e es a. e :> es => Handler es e -> Eff es a -> Eff es a
interpose = imposeN @'[]
{-# INLINE interpose #-}

impose :: forall e' e es a. e :> es => Handler (e' ': es) e -> Eff es a -> Eff (e' ': es) a
impose = imposeN @'[e']
{-# INLINE impose #-}

impose2 :: forall e' e'' e es a. e :> es => Handler (e' ': e'' ': es) e -> Eff es a -> Eff (e' ': e'' ': es) a
impose2 = imposeN @'[e', e'']
{-# INLINE impose2 #-}

impose3 :: forall e' e'' e''' e es a. e :> es => Handler (e' ': e'' ': e''' ': es) e -> Eff es a -> Eff (e' ': e'' ': e''' ': es) a
impose3 = imposeN @'[e', e'', e''']
{-# INLINE impose3 #-}

imposeN :: forall es' e es a. e :> es => Handler (es' ++ es) e -> Eff es a -> Eff (es' ++ es) a
imposeN handle m = PrimEff \handlers ->
  let handler = InternalHandler \eff -> primRunEff (handle eff) handlers
  in primRunEff m $ contractEnv @'[e] $ insertHandler handler $ contractEnv @es' handlers

runPure :: Eff '[] a -> a
runPure m = unsafePerformIO $ primRunEff m emptyEnv

unliftIO :: Given (Env es) => Eff es a -> IO a
unliftIO m = primRunEff m given
{-# INLINE unliftIO #-}

unlift :: forall es' es a. Given (Env es') => Eff es' a -> Eff es a
unlift m = PrimEff \es -> primRunEff m (unionEnv es given :: Env es')
{-# INLINE unlift #-}

withLiftMap :: Given (Env es') => (((Eff es a -> Eff es a) -> (Eff es' a -> Eff es' a)) -> Eff es a) -> Eff es a
withLiftMap f = withLift \lift -> f \f' m -> lift $ f' $ unlift m

withLift :: ((forall es' x. Eff es x -> Eff es' x) -> Eff es a) -> Eff es a
withLift f = PrimEff \es -> give es $ unliftIO $ f unlift
{-# INLINE withLift #-}
