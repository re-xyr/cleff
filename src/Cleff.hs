{-# LANGUAGE Trustworthy #-}
-- |
-- Copyright: (c) 2021 Xy Ren
-- License: BSD3
-- Maintainer: xy.r@outlook.com
-- Stability: experimental
-- Portability: non-portable (GHC only)
--
-- This library implements an /extensible effects system/, where sets of monadic actions ("effects") are encoded as
-- datatypes, tracked at the type level and can have multiple different implementations. This means you can swap out
-- implementations of certain monadic actions in mock tests or in different environments. The notion of "effect" is
-- general here: it can be an 'IO'-performing side effect, or just obtaining the value of a static global environment.
--
-- In particular, this library consists of
--
-- * The 'Eff' monad, which is the core of an extensible effects system. All effects are performed within it and it
--   will be the "main" monad of your application. This monad tracks effects at the type level.
-- * A set of predefined general effects, like 'Cleff.Reader.Reader' and 'Cleff.State.State' that can be used out of
--   the box.
-- * Combinators for defining new effects and interpreting them /on your own/. These effects can be translated in terms
--   of other already existing effects, or into operations in the 'IO' monad.
--
-- So, this library allows you to do two things:
--
-- * __Effect management:__ The 'Eff' monad tracks what effects are used explicitly at the type level, therefore you
--   are able to be certain about what effects are involved in each function.
-- * __Effect decoupling:__ You can decouple the implementation of the effects from your application and swap them
--   easily.
module Cleff
  ( -- * Using effects
    Eff, (:>), (:>>), Effect, IOE
  , -- ** Running effects
    -- $runningEffects
    runPure, runIOE
  , -- * Defining effects
    -- $definingEffects
    send, sendVia, makeEffect, makeEffect_
  , -- * Trivial effects handling
    raise, raiseN, inject, subsume, subsumeN, KnownList, Subset
  , -- * Interpreting effects
    -- $interpretingEffects
    Handler, interpret, reinterpret, reinterpret2, reinterpret3, reinterpretN, interpose, impose, imposeN
  , -- ** Interpreting in terms of 'IO'
    HandlerIO, interpretIO
  , -- ** Translating effects
    Translator, transform, translate
  , -- ** Transforming interpreters
    raiseUnder, raiseNUnder, raiseUnderN, raiseNUnderN
  , -- * Combinators for interpreting higher order effects
    -- $higherOrderEffects
    Handling, toEff, toEffWith, withFromEff
  , -- ** Interpreting 'IO'-related higher order effects
    withToIO, fromIO
  , -- * Miscellaneous
    type (~>), type (++), MonadIO (..), MonadUnliftIO (..)
  ) where

import           Cleff.Internal.Base
import           Cleff.Internal.Instances ()
import           Cleff.Internal.Interpret
import           Cleff.Internal.Monad
import           Cleff.Internal.TH
import           UnliftIO                 (MonadIO (liftIO), MonadUnliftIO (withRunInIO))

-- $runningEffects
-- To run an effect @T@, we should use an /interpreter/ of @T@, which is a function that has type like this:
--
-- @
-- runT :: 'Eff' (T ': es) a -> 'Eff' es a
-- @
--
-- Such an interpreter provides an implementation of @T@ and eliminates @T@ from the effect stack. All builtin effects
-- in @cleff@ have interpreters coming together with them.
--
-- By applying interpreters to an 'Eff' computation, you can eventually obtain an /end computation/, where there are no
-- more effects present on the effect stack. There are two kinds of end computations:
--
-- * A /pure computation/ with the type @'Eff' '[] a@, which you can obtain the value via 'Cleff.runPure'; or,
-- * An /impure computation/ with type @'Eff' '['Cleff.IOE'] a@ that can be transformed into an IO computation via
--   'Cleff.runIOE'.

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

-- $interpretingEffects
-- An effect can be understood as the "grammar" (or /syntax/) of a small language; however we also need to define the
-- "meaning" (or /semantics/) of the language. In other words, we need to specify the implementation of effects.
--
-- In an extensible effects system, this is achieved by writing /effect handlers/, which are functions that transforms
-- operations of one effect into other "more primitive" effects. These handlers can then be used to make interpreters
-- with library functions that we'll now see.
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
-- These interpreters can then be applied to computations with the @Filesystem@ effect to give different implementations
-- to the effect.

-- $higherOrderEffects
-- /Higher order effects/ are effects whose operations take other effect computations as arguments. For example, the
-- 'Cleff.Error.Error' effect is a higher order effect, because its 'Cleff.Error.CatchError' operation takes an effect
-- computation that may throw errors and also an error handler that returns an effect computation:
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
-- arbitrary effect stacks into a specific stack that the effect is currently interpreted into. In other words, they
-- need to thread other effects through themselves. This is why Cleff also provides convenient combinators for doing so.
--
-- In a 'Handler', you can temporarily "unlift" a computation from an arbitrary effect stack into the current stack via
-- 'toEff', explicitly change the current effect interpretation in the computation via 'toEffWith', or directly express
-- the effect in terms of 'IO' via 'withToIO'.
