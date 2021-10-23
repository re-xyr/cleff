-- | This module contains functions for interpreting effects. Most of the times you won't need to import this directly;
-- the module "Cleff" reexports most of the functionalities.
{-# OPTIONS_HADDOCK not-home #-}
module Cleff.Internal.Interpret where

import           Cleff.Internal.Effect
import           Cleff.Internal.Monad
import           Data.Rec              ((~!~), (~+~), (~:~))
import qualified Data.Rec              as Env
import           Unsafe.Coerce         (unsafeCoerce)

-- * Trivial handling

-- | Lift an action into a bigger effect stack with one more effect. For a more general version see 'raiseN'.
raise :: forall e es. Eff es ~> Eff (e ': es)
raise = raiseN @'[e]

-- | Lift an action into a bigger effect stack with arbitrarily more effects. This function requires
-- @TypeApplications@.
raiseN :: forall es' es. KnownList es' => Eff es ~> Eff (es' ++ es)
raiseN m = PrimEff (primRunEff m . Env.drop @es')

-- | Eliminate a duplicate effect from the top of the effect stack. For a more general version see 'subsumeN'.
subsume :: forall e es. e :> es => Eff (e ': es) ~> Eff es
subsume = subsumeN @'[e]

-- | Eliminate several duplicate effects from the top of the effect stack. This function requires @TypeApplications@.
subsumeN :: forall es' es. Subset es' es => Eff (es' ++ es) ~> Eff es
subsumeN m = PrimEff \es -> primRunEff m $ Env.pick @es' es ~+~ es

-- | Lift an action with a known effect stack into some superset of the stack.
inject :: forall es' es. Subset es' es => Eff es' ~> Eff es
inject m = PrimEff (primRunEff m . Env.pick @es')

-- * Handler types

-- | The typeclass that indicates a handler scope: effect @e@ sent from an arbitrary effect stack @esSend@ being
-- handled in the environment @es@.
--
-- You should not define instances for this typeclass whatsoever.
class Handling esSend es e
  | esSend -> es, esSend -> e, es -> esSend, es -> e, e -> esSend, e -> es where
  -- | Obtain the send-site environment.
  sendEnv :: Env esSend
  sendEnv = error "unimplemented"

-- | Newtype wrapper for instantiating the 'Handling' typeclass locally, a la the reflection trick. We do not use
-- the @reflection@ library directly so as not to expose this piece of implementation detail to the user.
newtype InstHandling es' es e a = InstHandling (Handling es' es e => a)

-- | Instantiate an 'Handling' typeclass, i.e. pass an implicit send-site environment in. This function shouldn't
-- be directly used anyway.
instHandling :: forall es' es e a. (Handling es' es e => a) -> Env es' -> a
instHandling x = unsafeCoerce (InstHandling x :: InstHandling es' es e a)
{-# INLINE instHandling #-}

-- | The type of an effect handler, /i.e./ a function that interprets an effect @e@ from an arbitrary effect stack into
-- the effect stack @es@, with additional effects @es'@ added on the top.
type Handler es' es e = forall esSend. (e :> esSend, Handling esSend es e) => e (Eff esSend) ~> Eff (es' ++ es)

-- | An effect handler that does not add new effects. This is used with 'interpret' and 'interpose'.
type Interpreter es e = Handler '[] es e

-- * Interpreting effects

-- | Interpret an effect @e@ in terms of effects in the effect stack @es@.
interpret :: Interpreter es e -> Eff (e ': es) ~> Eff es
interpret = reinterpretN @'[]

-- | Like 'interpret', but adds a new effect @e'@ that can be used in the handler.
reinterpret :: forall e' e es. Handler '[e'] es e -> Eff (e ': es) ~> Eff (e' ': es)
reinterpret = reinterpretN @'[e']

-- | Like 'reinterpret', but adds two new effects.
reinterpret2 :: forall e' e'' e es. Handler '[e', e''] es e -> Eff (e ': es) ~> Eff (e' ': e'' ': es)
reinterpret2 = reinterpretN @'[e', e'']

-- | Like 'reinterpret', but adds three new effects.
reinterpret3 :: forall e' e'' e''' e es. Handler '[e', e'', e'''] es e -> Eff (e ': es) ~> Eff (e' ': e'' ': e''' ': es)
reinterpret3 = reinterpretN @'[e', e'', e''']

-- | Like 'reinterpret', but adds arbitrarily many new effects. This function requires @TypeApplications@.
reinterpretN :: forall es' e es. KnownList es' => Handler es' es e -> Eff (e ': es) ~> Eff (es' ++ es)
reinterpretN handle m = PrimEff \es ->
  let handler = InternalHandler \eff -> PrimEff \esSend -> primRunEff (instHandling handle esSend eff) es
  in primRunEff m $ handler ~:~ Env.drop @es' es

-- | Respond to an effect while being able to leave it unhandled (i.e. you can resend the effects in the handler).
interpose :: e :> es => Interpreter es e -> Eff es ~> Eff es
interpose = imposeN @'[]

-- | Like 'interpose', but allows to introduce one new effect to use in the handler.
impose :: forall e' e es. e :> es => Handler '[e'] es e -> Eff es ~> Eff (e' ': es)
impose = imposeN @'[e']

-- | Like 'impose', but allows introducing arbitrarily many effects. This requires @TypeApplications@.
imposeN :: forall es' e es. (KnownList es', e :> es) => Handler es' es e -> Eff es ~> Eff (es' ++ es)
imposeN handle m = PrimEff \es ->
  let handler = InternalHandler \eff -> PrimEff \esSend -> primRunEff (instHandling handle esSend eff) es
  in primRunEff m $ handler ~!~ Env.drop @es' es

-- * Combinators for interpreting higher effects

-- | Run a computation in the 'IO' monad. This is useful when interpreting an effect in terms of 'Cleff.IOE'.
runInIO :: Handling esSend es e => Eff esSend ~> IO
runInIO m = primRunEff m sendEnv

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
runThere :: Handling esSend es e => Eff esSend ~> Eff es
runThere = runThereN @'[]

-- | Like 'runThere', but allows the effect stack to be raised. This requires @TypeApplications@.
runThereN :: forall es' esSend es e. Handling esSend es e => Eff esSend ~> Eff (es' ++ es)
runThereN m = PrimEff $ const $ primRunEff m sendEnv

-- | Run a computation in the current effect stack, but allowing the current effect being interpreted to be handled
-- differently by wrapping an 'interpret', 'reinterpret' etc. over it. This is useful for interpreting effect with
-- local contexts, like 'Cleff.Reader.Local':
--
-- @
-- runReader :: r -> 'Eff' ('Cleff.Reader.Reader' r ': es) '~>' 'Eff' es
-- runReader r = 'interpret' \\case
--   'Cleff.Reader.Ask'       -> 'pure' r
--   'Cleff.Reader.Local' f m -> runReader (f r) ('runHere' m)
-- @
runHere :: forall e es esSend. (Handling esSend es e, e :> esSend) => Eff esSend ~> Eff (e ': es)
runHere = runHereN @'[e]

-- | Like 'runHere', but works with 'interpose' insead of 'interpret' or 'reinterpret'.
runHere' :: forall e es esSend. (Handling esSend es e, e :> es, e :> esSend) => Eff esSend ~> Eff es
runHere' = runHereN @'[] @e

-- | Like 'runHere', but allows arbitrarily many effects to be added on top of the stack. This function requires
-- @TypeApplications@.
runHereN :: forall es' e es esSend. (Handling esSend es e, e :> es' ++ es, e :> esSend) => Eff esSend ~> Eff (es' ++ es)
runHereN m = PrimEff \es -> primRunEff m (Env.index @e es ~!~ sendEnv)

-- | Temporarily gain the ability to lift arbitrary 'IO' actions into 'Eff' as long as an 'IO' action is finally
-- returned. This is useful for dealing with effect operations with the monad type in the negative position within
-- 'Cleff.IOE', like masking. It's unlikely that you need to use this function in implementing your effects.
withLiftIO :: Handling esSend es e => ((IO ~> Eff esSend) -> IO a) -> IO a
withLiftIO f = f (PrimEff . const)

-- | Temporarily gain the ability to lift some @'Eff' es@ actions into some other @'Eff' es'@ as long as an @'Eff' es@
-- is finally returned. This is useful for dealing with effect operations with the monad type in the negative position.
-- It's unlikely that you need to use this function in implementing your effects.
withLiftEff :: Handling esSend es e => ((Eff es ~> Eff esSend) -> Eff es a) -> Eff es a
withLiftEff = withLiftEffN @'[]

-- | Like 'withLiftEff', but allows the effect stack to be raised. This requires @TypeApplications@.
withLiftEffN :: forall es' es esSend e a. Handling esSend es e => ((Eff (es' ++ es) ~> Eff esSend) -> Eff (es' ++ es) a) -> Eff (es' ++ es) a
withLiftEffN f = PrimEff \es -> primRunEff (f \m -> PrimEff \_ -> primRunEff m es) es
