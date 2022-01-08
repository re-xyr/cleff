-- | This module contains functions for interpreting effects. Most of the times you won't need to import this directly;
-- the module "Cleff" reexports most of the functionalities.
{-# OPTIONS_HADDOCK not-home #-}
module Cleff.Internal.Interpret where

import           Cleff.Internal.Effect
import           Cleff.Internal.Monad
import qualified Data.Mem              as Mem
import           Data.Rec              (pattern (:++:))
import qualified Data.Rec              as Env
import           Unsafe.Coerce         (unsafeCoerce)

-- * Trivial handling

-- | Lift an action into a bigger effect stack with one more effect. For a more general version see 'raiseN'.
raise :: forall e es. Eff es ~> Eff (e ': es)
raise = raiseN @'[e]

-- | Lift an action into a bigger effect stack with arbitrarily more effects. This function requires
-- @TypeApplications@.
raiseN :: forall es' es. KnownList es' => Eff es ~> Eff (es' ++ es)
raiseN m = Eff (unEff m . Mem.adjust (Env.drop @es'))

-- | Eliminate a duplicate effect from the top of the effect stack. For a more general version see 'subsumeN'.
subsume :: forall e es. e :> es => Eff (e ': es) ~> Eff es
subsume = subsumeN @'[e]

-- | Eliminate several duplicate effects from the top of the effect stack. This function requires @TypeApplications@.
subsumeN :: forall es' es. Subset es' es => Eff (es' ++ es) ~> Eff es
subsumeN m = Eff (unEff m . Mem.adjust (\re -> Env.pick @es' re :++: re))

-- | Lift an action with a known effect stack into some superset of the stack.
inject :: forall es' es. Subset es' es => Eff es' ~> Eff es
inject m = Eff (unEff m . Mem.adjust (Env.pick @es'))

-- * Handler types

-- | The typeclass that indicates a handler scope: effect @e@ sent from an arbitrary effect stack @esSend@ being
-- handled in the environment @es@.
--
-- You should not define instances for this typeclass whatsoever.
class Handling e es esSend
  | esSend -> es, esSend -> e, es -> esSend, es -> e, e -> esSend, e -> es where
  -- | Obtain the send-site environment.
  sendEnv :: Env esSend
  sendEnv = error "unimplemented"

-- | Newtype wrapper for instantiating the 'Handling' typeclass locally, a la the reflection trick. We do not use
-- the @reflection@ library directly so as not to expose this piece of implementation detail to the user.
newtype InstHandling e es esSend a = InstHandling (Handling e es esSend => a)

-- | Instantiate an 'Handling' typeclass, i.e. pass an implicit send-site environment in. This function shouldn't
-- be directly used anyway.
instHandling :: forall e es esSend a. (Handling e es esSend => a) -> Env esSend -> a
instHandling x = unsafeCoerce (InstHandling x :: InstHandling e es esSend a)
{-# INLINE instHandling #-}

-- | The type of an effect handler, /i.e./ a function that interprets an effect @e@ from an arbitrary effect stack into
-- the effect stack @es@.
type Handler e es = forall esSend. (e :> esSend, Handling e es esSend) => e (Eff esSend) ~> Eff es

-- * Interpreting effects

mkInternalHandler :: Env es -> Handler e es -> InternalHandler e
mkInternalHandler es handle = InternalHandler \eff -> Eff \esSend ->
  unEff (instHandling handle esSend eff) (Mem.update esSend es)

-- | Interpret an effect @e@ in terms of effects in the effect stack @es@.
interpret :: Handler e es -> Eff (e ': es) ~> Eff es
interpret = reinterpretN @'[]

-- | Like 'interpret', but adds a new effect @e'@ that can be used in the handler.
reinterpret :: forall e' e es. Handler e (e' ': es) -> Eff (e ': es) ~> Eff (e' ': es)
reinterpret = reinterpretN @'[e']

-- | Like 'reinterpret', but adds two new effects.
reinterpret2 :: forall e' e'' e es. Handler e (e' ': e'' ': es) -> Eff (e ': es) ~> Eff (e' ': e'' ': es)
reinterpret2 = reinterpretN @'[e', e'']

-- | Like 'reinterpret', but adds three new effects.
reinterpret3 :: forall e' e'' e''' e es. Handler e (e' ': e'' ': e''' ': es) -> Eff (e ': es) ~> Eff (e' ': e'' ': e''' ': es)
reinterpret3 = reinterpretN @'[e', e'', e''']

-- | Like 'reinterpret', but adds arbitrarily many new effects. This function requires @TypeApplications@.
reinterpretN :: forall es' e es. KnownList es' => Handler e (es' ++ es) -> Eff (e ': es) ~> Eff (es' ++ es)
reinterpretN handle m = Eff \es ->
  unEff m $ Mem.append (mkInternalHandler es handle) $ Mem.adjust (Env.drop @es') es

-- | Respond to an effect while being able to leave it unhandled (i.e. you can resend the effects in the handler).
interpose :: e :> es => Handler e es -> Eff es ~> Eff es
interpose = imposeN @'[]

-- | Like 'interpose', but allows to introduce one new effect to use in the handler.
impose :: forall e' e es. e :> es => Handler e (e' ': es) -> Eff es ~> Eff (e' ': es)
impose = imposeN @'[e']

-- | Like 'impose', but allows introducing arbitrarily many effects. This requires @TypeApplications@.
imposeN :: forall es' e es. (KnownList es', e :> es) => Handler e (es' ++ es) -> Eff es ~> Eff (es' ++ es)
imposeN handle m = Eff \es ->
  unEff m $ Mem.replace (mkInternalHandler es handle) $ Mem.adjust (Env.drop @es') es

-- * Combinators for interpreting higher effects

-- | Run a computation in the current effect stack. The effects will have the same interpretations inside the
-- computation. This is useful for interpreting effects that don't have local contexts, like a bracketing effect:
--
-- @
-- data Resource m a where
--   Bracket :: m a -> (a -> m ()) -> (a -> m b) -> Resource m b
-- @
--
-- @
-- Bracket alloc dealloc use ->
--   'UnliftIO.bracket'
--     ('runThere' alloc)
--     ('runThere' . dealloc)
--     ('runThere' . use)
-- @
runThere :: Handling e es esSend => Eff esSend ~> Eff es
runThere m = Eff \es -> unEff m (Mem.update es sendEnv)

-- | Run a computation in the current effect stack, but handles the current effect inside the computation differently
-- by providing a new 'Handler'. This is useful for interpreting effect with local contexts, like 'Cleff.Reader.Local':
--
-- @
-- runReader :: r -> 'Eff' ('Cleff.Reader.Reader' r ': es) '~>' 'Eff' es
-- runReader x = 'interpret' (handle x)
--   where
--     handle :: r -> 'Handler' ('Reader' r) es
--     handle r = \\case
--       'Cleff.Reader.Ask'       -> 'pure' r
--       'Cleff.Reader.Local' f m -> 'runHere' (handle $ f r) m
-- @
runHere :: (Handling e es esSend, e :> esSend) => Handler e es -> Eff esSend ~> Eff es
runHere handle m = Eff \es -> unEff m $ Mem.write (mkInternalHandler es handle) $ Mem.update es sendEnv

-- | Temporarily gain the ability to unlift an @'Eff' esSend@ action into 'IO', as long as an @'Eff' es@ action can
-- be returned. This is useful for dealing with higher-order effects that involves 'IO'.
withUnliftIO :: Handling e es esSend => ((Eff esSend ~> IO) -> Eff es a) -> Eff es a
withUnliftIO f = Eff \es -> unEff (f \m -> unEff m (Mem.update es sendEnv)) es

-- | Temporarily gain the ability to lift arbitrary 'IO' actions into 'Eff' as long as an 'IO' action is finally
-- returned. This is useful for dealing with effect operations with the monad type in the negative position within
-- 'Cleff.IOE', like masking. It's unlikely that you need to use this function in implementing your effects.
withLiftIO :: Handling e es esSend => ((IO ~> Eff esSend) -> IO a) -> IO a
withLiftIO f = f (Eff . const)

-- | Temporarily gain the ability to lift some @'Eff' es@ actions into some other @'Eff' es'@ as long as an @'Eff' es@
-- is finally returned. This is useful for dealing with effect operations with the monad type in the negative position.
-- It's unlikely that you need to use this function in implementing your effects.
withLiftEff :: Handling e es esSend => ((Eff es ~> Eff esSend) -> Eff es a) -> Eff es a
withLiftEff f = Eff \es -> unEff (f \m -> Eff \esSend -> unEff m (Mem.update esSend es)) es
