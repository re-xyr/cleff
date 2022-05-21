{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_HADDOCK not-home #-}
-- |
-- Copyright: (c) 2021 Xy Ren
-- License: BSD3
-- Maintainer: xy.r@outlook.com
-- Stability: unstable
-- Portability: non-portable (GHC only)
--
-- This module contains functions for interpreting effects. Most of the times you won't need to import this directly;
-- the module "Cleff" reexports most of the functionalities.
--
-- __This is an /internal/ module and its API may change even between minor versions.__ Therefore you should be
-- extra careful if you're to depend on this module.
module Cleff.Internal.Interpret
  ( -- * General transformation
    alter
  , adjust
    -- * Trivial handling
  , raise
  , raiseN
  , inject
  , subsume
  , subsumeN
  , raiseUnder
  , raiseNUnder
  , raiseUnderN
  , raiseNUnderN
    -- * Handler types
  , Handling
  , esSend
  , Handler
  , Translator
    -- * Interpreting effects
  , interpret
  , reinterpret
  , reinterpret2
  , reinterpret3
  , reinterpretN
  , interpose
  , impose
  , imposeN
    -- * Translating effects
  , transform
  , translate
    -- * Combinators for interpreting higher effects
  , toEff
  , toEffWith
  , withFromEff
  ) where

import           Cleff.Internal
import           Cleff.Internal.Monad
import           Cleff.Internal.Rec   (Rec)
import qualified Cleff.Internal.Rec   as Rec
import           Unsafe.Coerce        (unsafeCoerce)

-- * Trivial handling

-- | Alter the effect environment by a contravariant transformation function over it. This function reveals the
-- profunctorial nature of 'Eff'; in particular, 'Eff' is a profunctor @['Effect'] -> 'Data.Kind.Type'@, @lmap@ is
-- 'alter', and @rmap@ is 'fmap'.
alter :: ∀ es es'. (Env es' -> Env es) -> Eff es ~> Eff es'
alter f = \(Eff m) -> Eff \es -> m (f es)

-- | A specialized version of 'alter' that only adjusts the effect stack.
adjust :: ∀ es es'. (Rec es' -> Rec es) -> Eff es ~> Eff es'
adjust f = alter (adjustEnv f)

-- | Lift a computation into a bigger effect stack with one more effect. For a more general version see 'raiseN'.
raise :: ∀ e es. Eff es ~> Eff (e : es)
raise = raiseN @'[e]

-- | Lift a computation into a bigger effect stack with arbitrarily more effects. This function requires
-- @TypeApplications@.
raiseN :: ∀ es' es. KnownList es' => Eff es ~> Eff (es' ++ es)
raiseN = adjust (Rec.drop @es')

-- | Like 'raise', but adds the new effect under the top effect. This is useful for transforming an interpreter
-- @e' ':>' es => 'Eff' (e : es) '~>` 'Eff' es@ into a reinterpreter @'Eff' (e : es) '~>' 'Eff' (e' : es)@:
--
-- @
-- myInterpreter :: Bar ':>' es => 'Eff' (Foo : es) '~>' 'Eff' es
-- myInterpreter = ...
--
-- myReinterpreter :: 'Eff' (Foo : es) '~>' 'Eff' (Bar : es)
-- myReinterpreter = myInterpreter '.' 'raiseUnder'
-- @
--
-- In other words,
--
-- @
-- 'reinterpret' h == 'interpret' h . 'raiseUnder'
-- @
--
-- However, note that this function is suited for transforming an existing interpreter into a reinterpreter; if you
-- want to define a reinterpreter from scratch, you should still prefer 'reinterpret', which is both easier to use and
-- more efficient.
--
-- @since 0.2.0.0
raiseUnder :: ∀ e' e es. Eff (e : es) ~> Eff (e : e' : es)
raiseUnder = raiseNUnder @'[e']

-- | Like 'raiseUnder', but allows introducing multiple effects. This function requires @TypeApplications@.
--
-- @since 0.2.0.0
raiseNUnder :: ∀ es' e es. KnownList es' => Eff (e : es) ~> Eff (e : es' ++ es)
raiseNUnder = raiseNUnderN @es' @'[e]

-- | Like 'raiseUnder', but allows introducing the effect under multiple effects. This function requires
-- @TypeApplications@.
--
-- @since 0.2.0.0
raiseUnderN :: ∀ e es' es. KnownList es' => Eff (es' ++ es) ~> Eff (es' ++ e : es)
raiseUnderN = raiseNUnderN @'[e] @es' @es

-- | A generalization of both 'raiseUnderN' and 'raiseNUnder', allowing introducing multiple effects under multiple
-- effects. This function requires @TypeApplications@ and is subject to serious type ambiguity; you most likely will
-- need to supply all three type variables explicitly.
--
-- @since 0.2.0.0
raiseNUnderN :: ∀ es'' es' es. (KnownList es', KnownList es'') => Eff (es' ++ es) ~> Eff (es' ++ (es'' ++ es))
raiseNUnderN = adjust \re -> Rec.concat
  (Rec.take @es' @(es'' ++ es) re) (Rec.drop @es'' @es (Rec.drop @es' @(es'' ++ es) re))

-- | Lift a computation with a fixed, known effect stack into some superset of the stack.
inject :: ∀ es' es. Subset es' es => Eff es' ~> Eff es
inject = adjust (Rec.pick @es')

-- | Eliminate a duplicate effect from the top of the effect stack. For a more general version see 'subsumeN'.
subsume :: ∀ e es. e :> es => Eff (e : es) ~> Eff es
subsume = subsumeN @'[e]

-- | Eliminate several duplicate effects from the top of the effect stack. This function requires @TypeApplications@.
subsumeN :: ∀ es' es. Subset es' es => Eff (es' ++ es) ~> Eff es
subsumeN = adjust \re -> Rec.concat (Rec.pick @es' re) re

-- * Handler types

-- | The send-site environment.
data SendSite esSend e = SendSite
  {-# UNPACK #-} !(Env esSend) -- ^ The send-site 'Env'.
  {-# UNPACK #-} !(HandlerPtr e) -- ^ The pointer to the current effect handler.

-- | The typeclass that denotes a handler scope, handling effect @e@ sent from the effect stack @esSend@ in the
-- effect stack @es@.
--
-- You should not define instances for this typeclass whatsoever.
class Handling esSend e es | esSend -> e es where
  -- @esSend@ is existential so it uniquely determines the other two variables. As handling scopes can nest, the other
  -- two variables cannot determine anything.

  -- | Obtain the send-site environment.
  sendSite :: SendSite esSend e
  sendSite = error
    "Cleff.Internal.Interpret.sendSite: Attempting to access the send site without a reflected value. This is perhaps \
    \because you are trying to define an instance for the 'Handling' typeclass, which you should not be doing \
    \whatsoever. If that or other shenanigans seem unlikely, please report this as a bug."

-- | Get the pointer to the current effect handler itself.
hdlPtr :: ∀ esSend e es. Handling esSend e es => HandlerPtr e
hdlPtr = let SendSite _ ptr = sendSite @esSend in ptr

-- | Get the send-site 'Env'.
esSend :: Handling esSend e es => Env esSend
esSend = let SendSite env _ = sendSite in env

-- | Newtype wrapper for instantiating the 'Handling' typeclass locally, a la the reflection trick. We do not use
-- the @reflection@ library directly so as not to expose this piece of implementation detail to the user.
newtype InstHandling esSend e es a = InstHandling (Handling esSend e es => a)

-- | Instantiate an 'Handling' typeclass, /i.e./ pass an implicit send-site environment in. This function shouldn't
-- be directly used anyhow.
instHandling :: ∀ esSend e es a. (Handling esSend e es => a) -> SendSite esSend e -> a
instHandling x = unsafeCoerce (InstHandling x :: InstHandling esSend e es a)

-- | The type of an /effect handler/, which is a function that transforms an effect @e@ from an arbitrary effect stack
-- into computations in the effect stack @es@.
type Handler e es = ∀ esSend. Handling esSend e es => e (Eff esSend) ~> Eff es

-- | The type of a simple transformation function from effect @e@ to @e'@.
type Translator e e' = ∀ esSend. e (Eff esSend) ~> e' (Eff esSend)

-- * Interpreting effects

-- | Transform a 'Handler' into an 'InternalHandler' given a pointer that is going to point to the 'InternalHandler'
-- and the current 'Env'.
mkInternalHandler :: HandlerPtr e -> Env es -> Handler e es -> InternalHandler e
mkInternalHandler ptr es handle = InternalHandler \e -> Eff \ess ->
  unEff (instHandling handle (SendSite ess ptr) e) (updateEnv ess es)

-- | Interpret an effect @e@ in terms of effects in the effect stack @es@ with an effect handler.
interpret :: ∀ e es. Handler e es -> Eff (e : es) ~> Eff es
interpret = reinterpretN @'[]
{-# INLINE interpret #-}

-- | Like 'interpret', but adds a new effect @e'@ to the stack that can be used in the handler.
reinterpret :: ∀ e' e es. Handler e (e' : es) -> Eff (e : es) ~> Eff (e' : es)
reinterpret = reinterpretN @'[e']
{-# INLINE reinterpret #-}

-- | Like 'reinterpret', but adds two new effects.
reinterpret2 :: ∀ e' e'' e es. Handler e (e' : e'' : es) -> Eff (e : es) ~> Eff (e' : e'' : es)
reinterpret2 = reinterpretN @'[e', e'']
{-# INLINE reinterpret2 #-}

-- | Like 'reinterpret', but adds three new effects.
reinterpret3 :: ∀ e' e'' e''' e es. Handler e (e' : e'' : e''' : es) -> Eff (e : es) ~> Eff (e' : e'' : e''' : es)
reinterpret3 = reinterpretN @'[e', e'', e''']
{-# INLINE reinterpret3 #-}

-- | Like 'reinterpret', but adds arbitrarily many new effects. This function requires @TypeApplications@.
reinterpretN :: ∀ es' e es. KnownList es' => Handler e (es' ++ es) -> Eff (e : es) ~> Eff (es' ++ es)
reinterpretN handle = alter \es -> appendEnv
  (mkInternalHandler (peekEnv es) es handle)
  (adjustEnv (Rec.drop @es') es)
{-# INLINE reinterpretN #-}

-- | Respond to an effect, but does not eliminate it from the stack. This means you can re-send the operations in the
-- effect handler; it is often useful when you need to "intercept" operations so you can add extra behaviors like
-- logging.
interpose :: ∀ e es. e :> es => Handler e es -> Eff es ~> Eff es
interpose = imposeN @'[]
{-# INLINE interpose #-}

-- | Like 'interpose', but allows to introduce one new effect to use in the handler.
impose :: ∀ e' e es. e :> es => Handler e (e' : es) -> Eff es ~> Eff (e' : es)
impose = imposeN @'[e']
{-# INLINE impose #-}

-- | Like 'impose', but allows introducing arbitrarily many effects. This requires @TypeApplications@.
imposeN :: ∀ es' e es. (KnownList es', e :> es) => Handler e (es' ++ es) -> Eff es ~> Eff (es' ++ es)
imposeN handle = alter \es -> replaceEnv
  (mkInternalHandler (peekEnv es) es handle)
  (adjustEnv (Rec.drop @es') es)
{-# INLINE imposeN #-}

-- * Translating effects

-- | Interpret an effect in terms of another effect in the stack via a simple 'Translator'.
--
-- @
-- 'transform' trans = 'interpret' ('sendVia' 'toEff' '.' trans)
-- @
transform :: ∀ e e' es. e' :> es => Translator e e' -> Eff (e : es) ~> Eff es
transform trans = interpret (sendVia toEff . trans)
{-# INLINE transform #-}

-- | Like 'transform', but instead of using an effect in stack, add a new one to the top of it.
--
-- @
-- 'translate' trans = 'reinterpret' ('sendVia' 'toEff' '.' trans)
-- @
translate :: ∀ e e' es. Translator e e' -> Eff (e : es) ~> Eff (e' : es)
translate trans = reinterpret (sendVia toEff . trans)
{-# INLINE translate #-}

-- * Combinators for interpreting higher effects

-- | Run a computation in the current effect stack; this is useful for interpreting higher-order effects. For example,
-- if you want to interpret a bracketing effects in terms of 'IO':
--
-- @
-- data Resource m a where
--   Bracket :: m a -> (a -> m ()) -> (a -> m b) -> Resource m b
-- @
--
-- You will not be able to simply write this for the effect:
--
-- @
-- runBracket :: IOE ':>' es => 'Eff' (Resource : es) a -> 'Eff' es a
-- runBracket = 'interpret' \\case
--   Bracket alloc dealloc use -> UnliftIO.'UnliftIO.bracket' alloc dealloc use
-- @
--
-- This is because effects are sended from all kinds of stacks that has @Resource@ in it, so effect handlers received
-- the effect as @Resource esSend a@, where @esSend@ is an arbitrary stack with @Resource@, instead of
-- @Resource es a@. This means @alloc@, @dealloc@ and @use@ are of type @'Eff' esSend a@, while 'UnliftIO.bracket' can
-- only take and return @'Eff' es a@. So we need to use 'toEff', which converts an @'Eff' esSend a@ into
-- an @'Eff' es a@:
--
-- @
-- runBracket :: IOE ':>' es => 'Eff' (Resource : es) a -> 'Eff' es a
-- runBracket = 'interpret' \\case
--   Bracket alloc dealloc use -> UnliftIO.'UnliftIO.bracket'
--     ('toEff' alloc)
--     ('toEff' . dealloc)
--     ('toEff' . use)
-- @
toEff :: Handling esSend e es => Eff esSend ~> Eff es
toEff = alter \es -> updateEnv es esSend
{-# INLINE toEff #-}

-- | Run a computation in the current effect stack, just like 'toEff', but takes a 'Handler' of the current effect
-- being interpreted, so that inside the computation being ran, the effect is interpreted differently. This is useful
-- for interpreting effects with local contexts, like 'Cleff.Reader.Local':
--
-- @
-- runReader :: r -> 'Eff' ('Cleff.Reader.Reader' r : es) '~>' 'Eff' es
-- runReader x = 'interpret' (handle x)
--   where
--     handle :: r -> 'Handler' ('Cleff.Reader.Reader' r) es
--     handle r = \\case
--       'Cleff.Reader.Ask'       -> 'pure' r
--       'Cleff.Reader.Local' f m -> 'toEffWith' (handle $ f r) m
-- @
toEffWith :: ∀ esSend e es. Handling esSend e es => Handler e es -> Eff esSend ~> Eff es
toEffWith handle = alter \es ->
  -- The 'Handling' constraint of 'handle' will NOT be prematurely initialized here because that will make 'handle'
  -- monomorphic. Therefore this usage is safe.
  writeEnv (hdlPtr @esSend) (mkInternalHandler (hdlPtr @esSend) es handle) $ updateEnv es esSend
{-# INLINE toEffWith #-}

-- | Temporarily gain the ability to lift some @'Eff' es@ actions into @'Eff' esSend@. This is only useful for dealing
-- with effect operations with the monad type in the negative position, which means it's unlikely that you need to use
-- this function in implementing your effects.
withFromEff :: Handling esSend e es => ((Eff es ~> Eff esSend) -> Eff esSend a) -> Eff es a
withFromEff f = Eff \es -> unEff (f $ alter \ess -> updateEnv ess es) (updateEnv es esSend)
{-# INLINE withFromEff #-}
