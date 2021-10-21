-- | This module contains functions for interpreting effects. Most of the times you won't need to import this directly;
-- the module "Cleff" reexports most of the functionalities.
{-# OPTIONS_HADDOCK not-home #-}
module Cleff.Internal.Interpret
  ( -- * Trivial handling
    raise, raiseN, subsume, subsumeN, inject
  , -- * Handler types
    Handling, InstHandling, instHandling, Handler, Interpreter
  , -- * Interpreting effects
    interpret, reinterpret, reinterpret2, reinterpret3, reinterpretN, reinterpretBy, interpose
  , -- * Combinators for handling higher order effects
    Lift, runInIO, runThere, runHere, withLiftIO, withLiftEff
  ) where

import           Cleff.Internal.Effect
import           Cleff.Internal.Monad
import           GHC.TypeLits          (ErrorMessage ((:<>:)))
import qualified GHC.TypeLits          as GHC
import           Unsafe.Coerce         (unsafeCoerce)

-- | Add an effect on the effect stack. For a more general version see 'raiseN'.
raise :: forall e es. Eff es ~> Eff (e ': es)
raise = raiseN @'[e]

-- | Add several effects on the effect stack. This function requires @TypeApplications@.
raiseN :: forall es' es. KnownList es' => Eff es ~> Eff (es' ++ es)
raiseN m = PrimEff (primRunEff m . contractEnv @es')

-- | Eliminate a duplicate effect from the top of the effect stack. For a more general version see 'subsumeN'.
subsume :: forall e es. e :> es => Eff (e ': es) ~> Eff es
subsume = subsumeN @'[e]

-- | Eliminate several duplicate effects from the top of the effect stack. This function requires @TypeApplications@.
subsumeN :: forall es' es. Subset es' es => Eff (es' ++ es) ~> Eff es
subsumeN m = PrimEff (primRunEff m . expandEnv @es')

-- | Transform the effect stack into some superset of it. This function requires @TypeApplications@.
inject :: forall es' es. Subset es' es => Eff es' ~> Eff es
inject m = PrimEff (primRunEff m . getSubsetEnv @es')

-- | The typeclass that indicates a handler scope: effect @e@ sent from an arbitrary effect stack @esSend@ being
-- handled in the environment @esBase@.
--
-- You should not define instances for this typeclass whatsoever.
class Handling esSend esBase e
  | esSend -> esBase, esSend -> e, esBase -> esSend, esBase -> e, e -> esSend, e -> esBase where
  -- | Obtain the send-site environment.
  sendEnv :: Env esSend
  sendEnv = error "unimplemented"

-- | Newtype wrapper for instantiating the 'Handling' typeclass locally, a la the reflection trick. We do not use
-- the @reflection@ library directly so as not to expose this piece of implementation detail to the user.
newtype InstHandling es' esBase e a = InstHandling (Handling es' esBase e => a)

-- | Instantiatiate an 'Handling' typeclass, i.e. pass an implicit send-site environment in. This function shouldn't
-- be directly used anyway.
instHandling :: forall es' esBase e a. (Handling es' esBase e => a) -> Env es' -> a
instHandling x = unsafeCoerce (InstHandling x :: InstHandling es' esBase e a)
{-# INLINE instHandling #-}

-- | The type of effect handler. An effect handler translates effect @e@ from arbitrary effect stacks into computations
-- in the effect stack @es' '++' es@.
type Handler es' es e = forall esSend. (e :> esSend, Handling esSend es e) => e (Eff esSend) ~> Eff (es' ++ es)

-- | An effect handler being passed to 'interpret'.
type Interpreter es e = Handler '[] es e

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
  in primRunEff m $ insertHandler handler $ contractEnv @es' es

-- | Interpret an effect in an arbitrary effect stack and then transform it back to the original stack.
reinterpretBy :: forall esHandle e es. (Eff esHandle ~> Eff es) -> Interpreter esHandle e -> Eff (e ': es) ~> Eff es
reinterpretBy transform handle m = PrimEff \es ->
  let handler = InternalHandler \eff -> PrimEff \esSend -> primRunEff (transform $ instHandling handle esSend eff) es
  in primRunEff m $ insertHandler handler es

-- | Typeclass that indicates @esBase@ can be lifted into @es@. In other words, @esBase = es' ++ es@.
--
-- This ensures the presence of 'Cleff.IOE' is the same between @esBase@ and @es@.
class Lift esBase es
instance {-# INCOHERENT #-} Lift es es
instance Lift esBase es => Lift esBase (e ': es)
type CannotLift esBase es = 'GHC.Text "The effect stack '" ':<>: 'GHC.ShowType esBase
  ':<>: 'GHC.Text "' cannot be lifted into '" ':<>: 'GHC.ShowType es ':<>: 'GHC.Text "'"
instance {-# OVERLAPPABLE #-} GHC.TypeError (CannotLift esBase es) => Lift esBase es

-- | Respond to an effect while being able to leave it unhandled (i.e. you can resend the effects in the handler).
--
-- @
-- 'interpose' f = 'interpret' f '.' 'raise'
-- @
interpose :: e :> es => Interpreter es e -> Eff es ~> Eff es
interpose handle m = PrimEff \es ->
  let handler = InternalHandler \eff -> PrimEff \esSend -> primRunEff (instHandling handle esSend eff) es
  in primRunEff m $ modifyHandler handler es

-- | Run a computation in the 'IO' monad. This is useful when interpreting an effect in terms of 'Cleff.IOE'.
runInIO :: Handling esSend esBase e => Eff esSend ~> IO
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
runThere :: (Handling esSend esBase e, Lift esBase es) => Eff esSend ~> Eff es
runThere m = PrimEff $ const $ primRunEff m sendEnv

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
runHere :: forall e es esSend esBase. (Handling esSend esBase e, Lift esBase es, e :> es, e :> esSend) => Eff esSend ~> Eff es
runHere m = PrimEff \es -> primRunEff m (modifyHandler (getHandler @e es) sendEnv)

-- | Temporarily gain the ability to lift arbitrary 'IO' actions into 'Eff' as long as an 'IO' action is finally
-- returned. This is useful for dealing with effect operations with the monad type in the negative position within
-- 'Cleff.IOE', like masking. It's unlikely that you need to use this function in implementing your effects.
withLiftIO :: Handling esSend esBase e => ((IO ~> Eff esSend) -> IO a) -> IO a
withLiftIO f = f (PrimEff . const)

-- | Temporarily gain the ability to lift some @'Eff' es@ actions into some other @'Eff' es'@ as long as an @'Eff' es@
-- is finally returned. This is useful for dealing with effect operations with the monad type in the negative position.
-- It's unlikely that you need to use this function in implementing your effects.
withLiftEff :: (Handling esSend esBase e, Lift esBase es) => ((Eff es ~> Eff esSend) -> Eff es a) -> Eff es a
withLiftEff f = PrimEff \es -> primRunEff (f \m -> PrimEff \_ -> primRunEff m es) es
