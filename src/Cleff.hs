-- | This library implements an /extensible effects system/, where effects are encoded as datatypes, tracked at the
-- type level and can be handled in multiple different ways. The notion of "effect" is general here: it can be an
-- IO-performing side effect, or just obtaining the value of a static global environment.
--
-- In particular, this library consists of
--
-- * The 'Eff' monad, which is the core type of an extensible effects system, where all effects are performed within.
--   Effects are tracked at type level, and can be given multiple interpretations at runtime.
-- * A set of predefined general effects, like 'Cleff.Reader.Reader' and 'Cleff.State.State' that can be used out of
--   the box.
-- * Combinators for defining new effects and interpreting them /on your own/. These effects can be translated into
--   operations in the 'IO' monad, or in terms of other already existing effects.
module Cleff
  ( -- * Using effects
    Eff, (:>), (:>>), Effect, IOE
  , -- * Defining effects
    -- $definingEffects
    send, makeEffect, makeEffect_
  , -- * Unwrapping @Eff@
    -- $unwrappingEff
    runPure, runIOE
  , -- * Trivial effects handling
    raise, raiseN, KnownList, subsume, subsumeN, inject, Subset
  , -- * Interpreting effects
    -- $interpretingEffects
    Handler,
    interpret, reinterpret, reinterpret2, reinterpret3, reinterpretN,
    interpose, impose, imposeN,
    HandlerIO, interpretIO
  , -- * Combinators for interpreting higher order effects
    -- $higherOrderEffects
    Handling, runThere, runHere, withUnliftIO, withLiftIO, withLiftEff
  , -- * Miscellaneous
    type (~>), type (++), MonadIO (..), MonadUnliftIO (..)
  ) where

import           Cleff.Internal.Base
import           Cleff.Internal.Effect
import           Cleff.Internal.Interpret
import           Cleff.Internal.Monad
import           Cleff.Internal.TH
import           UnliftIO                 (MonadIO (..), MonadUnliftIO (..))

-- $definingEffects
-- An effect should be defined as a GADT and have the kind 'Effect'. Each operation in the effect is a constructor of
-- the effect type. For example, an effect supporting reading/writing files can be as following:
--
-- @
-- data Filesystem :: 'Effect' where
--   ReadFile :: 'FilePath' -> Filesystem m 'String'
--   WriteFile :: 'FilePath' -> 'String' -> Filesystem m ()
-- @
--
-- Operations constructed with these constructors can be performed via the 'send' function. You can also use the
-- Template Haskell function 'makeEffect' to automatically generate definitions of functions that perform the effects.
-- For example,
--
-- @
-- 'makeEffect' ''Filesystem
-- @
--
-- generates the following definitions:
--
-- @
-- readFile      :: Filesystem ':>' es => 'FilePath' -> 'Eff' es 'String'
-- readFile  x   =  'send' (ReadFile x)
-- writeFile     :: Filesystem ':>' es => 'FilePath' -> 'String' -> 'Eff' es ()
-- writeFile x y =  'send' (WriteFile x y)
-- @

-- $unwrappingEff
-- For any effect @T@, there should be /interpreters/ of the form
--
-- @
-- runT :: 'Eff' (T ': es) a -> 'Eff' es a
-- @
--
-- that can eliminate @T@ from the effect stack by transforming effect operations of @T@ in terms of other effects in
-- @es@.
--
-- By applying interpreters to an 'Eff' computation, you can eventually obtain a /pure computation/ with the type
-- @'Eff' '[] a@, which you can obtain the value via 'Cleff.runPure', or an /impure computation/ with type
-- @'Eff' '['Cleff.IOE'] a@ that can be transformed into an IO action via 'Cleff.runIOE'.

-- $interpretingEffects
-- An effect can be understood as the "grammar" (or /syntax/) of a language, while it is also important to define the
-- "meaning" (or /semantics/) of the language. In other words, we need to specify what is actually done when an effect
-- is performed. In an extensible effects system, this is achieved by writing /effect handlers/, or /interpreters/,
-- which transforms operations of one effect in terms of other effects.
--
-- Mostly, or even always, a user-defined effect should be interpreted into other "more primitive" effects. This is
-- why Cleff provides many convenient combinators for that.
--
-- This is very easy to do. For example, for the @Filesystem@ effect
--
-- @
-- data Filesystem :: 'Effect' where
--   ReadFile :: 'FilePath' -> Filesystem m 'String'
--   WriteFile :: 'FilePath' -> 'String' -> Filesystem m ()
-- @
--
-- We can easily handle it in terms of 'IO' operations via 'interpretIO', by pattern matching on the effect
-- constructors:
--
-- @
-- runFilesystemIO :: 'IOE' ':>' es => 'Eff' (Filesystem ': es) a -> 'Eff' es a
-- runFilesystemIO = 'interpretIO' \\case
--   ReadFile path           -> 'readFile' path
--   WriteFile path contents -> 'writeFile' path contents
-- @
--
-- Alternatively, we can also construct an in-memory filesystem in terms of the 'Cleff.State.State' effect via
-- the 'reinterpret' function.
--
-- @
-- runFilesystemPure :: 'Cleff.Fail.Fail' ':>' es => 'Data.Map.Map' 'FilePath' 'String' -> 'Eff' (Filesystem ': es) a -> 'Eff' es a
-- runFilesystemPure fs = 'fmap' 'fst' '.' 'Cleff.State.runState' fs '.' 'reinterpret' \\case
--   ReadFile path -> 'Cleff.State.gets' ('Data.Map.lookup' path) >>= \\case
--     'Nothing'       -> 'fail' ("File not found: " ++ 'show' path)
--     'Just' contents -> 'pure' contents
--   WriteFile path contents -> 'Cleff.State.modify' ('Data.Map.insert' path contents)
-- @
--
-- These interpreters can then be applied to computations with the @Filesystem@ effect to give different semantics
-- to the effect.

-- $higherOrderEffects
-- /Higher order effects/ are effects whose operations that can take effect actions as arguments. For example, the
-- 'Cleff.Error.Error' effect is a higher order effect, because its 'Cleff.Error.CatchError' operation takes an effect
-- action that may throw errors and also an error handler that also returns an effect action:
--
-- @
-- data Error e :: 'Effect' where
--   ThrowError :: e -> Error e m a
--   CatchError :: m a -> (e -> m a) -> Error e m a
-- @
--
-- More literally, an high order effect makes use of the monad type paramenter @m@, while a first order effect, like
-- 'Cleff.State.State', does not.
--
-- It is harder to write interpreters for higher order effects, because we need to transform computations from
-- arbitrary effect stacks into a specific stack that the current is interpreted into. In other words, they need to
-- thread other effects through themselves. This is why Cleff also provides convenient combinators for doing so.
--
-- In a 'Handler', you can temporarily "unlift" a computation from an arbitrary effect stack into the current stack via
-- 'runThere', explicitly change current effect interpretation in the computation via 'runHere', or directly express
-- the effect in terms of 'IO' via 'runInIO'.
