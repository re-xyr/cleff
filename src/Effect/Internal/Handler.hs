module Effect.Internal.Handler where

import           Data.Reflection       (Given (given), give)
import           Effect.Internal.Monad
import           System.IO.Unsafe      (unsafePerformIO)

interpret :: forall e es a. Legal e => Handler es e -> Eff (e ': es) a -> Eff es a
interpret = reinterpretN @'[]
{-# INLINE interpret #-}

reinterpret :: forall e' e es a. Legal e => Handler (e' ': es) e -> Eff (e ': es) a -> Eff (e' ': es) a
reinterpret = reinterpretN @'[e']
{-# INLINE reinterpret #-}

reinterpret2 :: forall e' e'' e es a. Legal e => Handler (e' ': e'' ': es) e -> Eff (e ': es) a -> Eff (e' ': e'' ': es) a
reinterpret2 = reinterpretN @'[e', e'']
{-# INLINE reinterpret2 #-}

reinterpret3 :: forall e' e'' e''' e es a. Legal e => Handler (e' ': e'' ': e''' ': es) e -> Eff (e ': es) a -> Eff (e' ': e'' ': e''' ': es) a
reinterpret3 = reinterpretN @'[e', e'', e''']
{-# INLINE reinterpret3 #-}

reinterpretN :: forall es' e es a. Legal e => Handler (es' ++ es) e -> Eff (e ': es) a -> Eff (es' ++ es) a
reinterpretN f m = PrimEff \hdl -> primRunEff m $ insertHandler (HandlerOf hdl f) hdl

interpose :: forall e es a. e :> es => Handler es e -> Eff es a -> Eff es a
interpose f m = PrimEff \hdl -> primRunEff m $ insertHandler (HandlerOf hdl f) hdl

runPure :: Eff '[] a -> a
runPure m = unsafePerformIO $ primRunEff m emptyEnv

unliftIO :: Given (Env es) => Eff es a -> IO a
unliftIO m = primRunEff m given
{-# INLINE unliftIO #-}

unlift :: Given (Env es') => Eff es' a -> Eff es a
unlift = PrimEff . const . unliftIO
{-# INLINE unlift #-}

withLiftMap :: Given (Env es') => (((Eff es a -> Eff es a) -> (Eff es' a -> Eff es' a)) -> Eff es a) -> Eff es a
withLiftMap f = withLift \lift -> f \f' m -> lift $ f' $ unlift m

withLift :: ((forall es' x. Eff es x -> Eff es' x) -> Eff es a) -> Eff es a
withLift f = PrimEff \es -> give es $ unliftIO $ f unlift
{-# INLINE withLift #-}
