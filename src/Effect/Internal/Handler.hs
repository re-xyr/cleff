module Effect.Internal.Handler where

import           Data.Reflection       (Given (given))
import           Effect.Internal.Monad

raise :: forall e es a. Eff es a -> Eff (e ': es) a
raise = raiseN @'[e]
{-# INLINE raise #-}

raiseN :: forall es' es a. Eff es a -> Eff (es' ++ es) a
raiseN m = PrimEff (primRunEff m . contractEnv @es')

subsume :: forall e es a. e :> es => Eff (e ': es) a -> Eff es a
subsume = subsumeN @'[e]
{-# INLINE subsume #-}

subsumeN :: forall es' es a. es' :>> es => Eff (es' ++ es) a -> Eff es a
subsumeN m = PrimEff (primRunEff m . expandEnv @es')

type Recursive a = a -> a

interpret :: forall e es a. Legit e => Handler es e -> Eff (e ': es) a -> Eff es a
interpret = reinterpretN @'[]
{-# INLINE interpret #-}

interpretH :: forall e es a. Legit e => Recursive (Handler es e) -> Eff (e ': es) a -> Eff es a
interpretH = reinterpretNH @'[]
{-# INLINE interpretH #-}

reinterpret :: forall e' e es a. Legit e => Handler (e' ': es) e -> Eff (e ': es) a -> Eff (e' ': es) a
reinterpret = reinterpretN @'[e']
{-# INLINE reinterpret #-}

reinterpretH :: forall e' e es a. Legit e => Recursive (Handler (e' ': es) e) -> Eff (e ': es) a -> Eff (e' ': es) a
reinterpretH = reinterpretNH @'[e']
{-# INLINE reinterpretH #-}

reinterpret2 :: forall e' e'' e es a. Legit e => Handler (e' ': e'' ': es) e -> Eff (e ': es) a -> Eff (e' ': e'' ': es) a
reinterpret2 = reinterpretN @'[e', e'']
{-# INLINE reinterpret2 #-}

reinterpret2H :: forall e' e'' e es a. Legit e => Recursive (Handler (e' ': e'' ': es) e) -> Eff (e ': es) a -> Eff (e' ': e'' ': es) a
reinterpret2H = reinterpretNH @'[e', e'']
{-# INLINE reinterpret2H #-}

reinterpret3 :: forall e' e'' e''' e es a. Legit e => Handler (e' ': e'' ': e''' ': es) e -> Eff (e ': es) a -> Eff (e' ': e'' ': e''' ': es) a
reinterpret3 = reinterpretN @'[e', e'', e''']
{-# INLINE reinterpret3 #-}

reinterpret3H :: forall e' e'' e''' e es a. Legit e => Recursive (Handler (e' ': e'' ': e''' ': es) e) -> Eff (e ': es) a -> Eff (e' ': e'' ': e''' ': es) a
reinterpret3H = reinterpretNH @'[e', e'', e''']
{-# INLINE reinterpret3H #-}

reinterpretN :: forall es' e es a. Legit e => Handler (es' ++ es) e -> Eff (e ': es) a -> Eff (es' ++ es) a
reinterpretN handle = reinterpretNH @es' \_ -> handle
{-# INLINE reinterpretN #-}

reinterpretNH :: forall es' e es a. Legit e => Recursive (Handler (es' ++ es) e) -> Eff (e ': es) a -> Eff (es' ++ es) a
reinterpretNH handleRec m = PrimEff \handlers ->
  let handler = InternalHandler \eff -> primRunEff (instCanThread @e $ handle eff) handlers
  in primRunEff m $ insertHandler handler $ contractEnv @es' handlers
  where
    handle :: Handler (es' ++ es) e
    handle = handleRec handle

interpose :: forall e es a. e :> es => Handler es e -> Eff es a -> Eff es a
interpose = imposeN @'[]
{-# INLINE interpose #-}

interposeH :: forall e es a. e :> es => Recursive (Handler es e) -> Eff es a -> Eff es a
interposeH = imposeNH @'[]
{-# INLINE interposeH #-}

impose :: forall e' e es a. e :> es => Handler (e' ': es) e -> Eff es a -> Eff (e' ': es) a
impose = imposeN @'[e']
{-# INLINE impose #-}

imposeH :: forall e' e es a. e :> es => Recursive (Handler (e' ': es) e) -> Eff es a -> Eff (e' ': es) a
imposeH = imposeNH @'[e']
{-# INLINE imposeH #-}

impose2 :: forall e' e'' e es a. e :> es => Handler (e' ': e'' ': es) e -> Eff es a -> Eff (e' ': e'' ': es) a
impose2 = imposeN @'[e', e'']
{-# INLINE impose2 #-}

impose2H :: forall e' e'' e es a. e :> es => Recursive (Handler (e' ': e'' ': es) e) -> Eff es a -> Eff (e' ': e'' ': es) a
impose2H = imposeNH @'[e', e'']
{-# INLINE impose2H #-}

impose3 :: forall e' e'' e''' e es a. e :> es => Handler (e' ': e'' ': e''' ': es) e -> Eff es a -> Eff (e' ': e'' ': e''' ': es) a
impose3 = imposeN @'[e', e'', e''']
{-# INLINE impose3 #-}

impose3H :: forall e' e'' e''' e es a. e :> es => Recursive (Handler (e' ': e'' ': e''' ': es) e) -> Eff es a -> Eff (e' ': e'' ': e''' ': es) a
impose3H = imposeNH @'[e', e'', e''']
{-# INLINE impose3H #-}

imposeN :: forall es' e es a. e :> es => Handler (es' ++ es) e -> Eff es a -> Eff (es' ++ es) a
imposeN handle = imposeNH @es' \_ -> handle
{-# INLINE imposeN #-}

imposeNH :: forall es' e es a. e :> es => Recursive (Handler (es' ++ es) e) -> Eff es a -> Eff (es' ++ es) a
imposeNH handleRec m = reinterpretNH @es' handleRec (raise m)
{-# INLINE imposeNH #-}

primEnv :: Eff es (Env es)
primEnv = PrimEff pure

unliftIO :: Given (Env es) => Eff es a -> IO a
unliftIO m = primRunEff m given
{-# INLINE unliftIO #-}

withLiftIO :: forall es a. Given (Env es) => ((forall x. IO x -> Eff es x) -> Eff es a) -> IO a
withLiftIO f = unliftIO $ f (PrimEff . const)
{-# INLINE withLiftIO #-}

unlift :: forall e es es' a. (Legit e, Given (Env es'), CanThread e) => Eff es' a -> Eff (e ': es) a
unlift = unlift' @e
{-# INLINE unlift #-}

unlift' :: forall e es es' a. (e :> es, Given (Env es'), CanThread e) => Eff es' a -> Eff es a
unlift' m = PrimEff \es -> primRunEff m (contractEnv @'[e] $ insertHandler (getHandler es) given)
{-# INLINE unlift' #-}
