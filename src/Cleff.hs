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
-- general here: it can be an 'IO'-performing side effect, or just reading the value of a static global environment.
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
-- In terms of structuring your application, this library helps you to do two things:
--
-- * __Effect management:__ The 'Eff' monad tracks what effects are used explicitly at the type level, therefore you
--   are able to enforce what effects are involved in each function, and avoid accidentally introduced behaviors.
-- * __Effect decoupling:__ You can swap between the implementations of the effects in your application easily,
--   so you can refactor and test your applications with less clutter.
module Cleff
  ( -- * Using effects
    Eff
  , (:>)
  , (:>>)
  , Effect
  , IOE
    -- ** Running effects
    -- $runningEffects
  , runPure
  , runIOE
    -- * Defining effects
    -- $definingEffects
  , send
  , sendVia
  , makeEffect
  , makeEffect_
    -- * Trivial effects handling
  , raise
  , raiseN
  , inject
  , subsume
  , subsumeN
  , KnownList
  , Subset
    -- * Interpreting effects
    -- $interpretingEffects
  , Handler
  , interpret
  , reinterpret
  , reinterpret2
  , reinterpret3
  , reinterpretN
  , interpose
  , impose
  , imposeN
    -- ** Interpreting in terms of 'IO'
  , HandlerIO
  , interpretIO
    -- ** Translating effects
  , Translator
  , transform
  , translate
    -- ** Transforming interpreters
  , raiseUnder
  , raiseNUnder
  , raiseUnderN
  , raiseNUnderN
    -- * Combinators for interpreting higher order effects
    -- $higherOrderEffects
  , Handling
  , toEff
  , toEffWith
  , withFromEff
    -- ** Interpreting 'IO'-related higher order effects
  , withToIO
  , fromIO
    -- * Miscellaneous
  , type (~>)
  , type (++)
  , MonadIO (..)
  , MonadUnliftIO (..)
  ) where

import           Cleff.Internal
import           Cleff.Internal.Base
import           Cleff.Internal.Env       (Handler, Handling)
import           Cleff.Internal.Instances ()
import           Cleff.Internal.Interpret
import           Cleff.Internal.Monad
import           Cleff.Internal.TH
import           UnliftIO                 (MonadIO (liftIO), MonadUnliftIO (withRunInIO))

-- $runningEffects
-- To run an effect @T@, we should use an /interpreter/ of @T@, which is a function that has a type like this:
--
-- @
-- runT :: 'Eff' (T : es) a -> 'Eff' es a
-- @
--
-- Such an interpreter provides an implementation of @T@ and eliminates @T@ from the effect stack. All builtin effects
-- in @cleff@ have interpreters out of the box in their respective modules.
--
-- By applying interpreters to an 'Eff' computation, you can eventually obtain an /end computation/, where there are no
-- more effects to be interpreted on the effect stack. There are two kinds of end computations:
--
-- * A /pure computation/ with the type @'Eff' '[] a@, which you can obtain the value via 'runPure'; or,
-- * An /impure computation/ with type @'Eff' '['IOE'] a@ that can be unwrapped into an IO computation via
--   'runIOE'.

-- $definingEffects
-- An effect should be defined as a GADT and have the kind 'Effect'. Each operation in the effect is a constructor of
-- the effect type. For example, an effect supporting reading and writing files can be like this:
--
-- @
-- data Filesystem :: 'Effect' where
--   ReadFile :: 'FilePath' -> Filesystem m 'String'
--   WriteFile :: 'FilePath' -> 'String' -> Filesystem m ()
-- @
--
-- Here, @ReadFile@ is an operation that takes a 'FilePath' and returns a 'String', presumably the content of the file;
-- @WriteFile@ is an operation that takes a 'FilePath' and a 'String' and returns @()@, meaning it only performs
-- side effects - presumably writing the 'String' to the file specified.
--
-- Operations constructed with these constructors can be performed via the 'send' function. You can also use the
-- Template Haskell function 'makeEffect' to automatically generate definitions of functions that perform the effects.

-- $interpretingEffects
-- An effect can be understood as the /syntax/ of a tiny language; however we also need to define the /meaning/ (or
-- /semantics/) of the language. In other words, we need to specify the implementations of effects.
--
-- In an extensible effects system, this is achieved by writing /effect handlers/, which are functions that transforms
-- operations of one effect into other "more primitive" effects. These handlers can then be used to make interpreters
-- with library functions that we'll now see.
--
-- For example, for the @Filesystem@ effect:
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
-- runFilesystemIO :: 'IOE' ':>' es => 'Eff' (Filesystem : es) a -> 'Eff' es a
-- runFilesystemIO = 'interpretIO' \\case
--   ReadFile path           -> 'readFile' path
--   WriteFile path contents -> 'writeFile' path contents
-- @
--
-- Specifically, a @ReadFile@ operation is mapped to a real 'readFile' IO computation, and similarly a @WriteFile@
-- operation is mapped to a 'writeFile' computation.
--
-- An effect is a set of abstract operations, and naturally, they can have more than one interpretations. Therefore,
-- here we can also construct an in-memory filesystem that reads from and writes into a 'Cleff.State.State' effect, via
-- the 'reinterpret' function that adds another effect to the stack for the effect handler to use:
--
-- @
-- filesystemToState
--   :: 'Cleff.Fail.Fail' ':>' es
--   => 'Eff' (Filesystem : es) a
--   -> 'Eff' ('Cleff.State.State' ('Data.Map.Map' 'FilePath' 'String') : es) a
-- filesystemToState = 'reinterpret' \\case
--   ReadFile path -> 'Cleff.State.gets' ('Data.Map.lookup' path) >>= \\case
--     'Nothing'       -> 'fail' ("File not found: " ++ 'show' path)
--     'Just' contents -> 'pure' contents
--   WriteFile path contents -> 'Cleff.State.modify' ('Data.Map.insert' path contents)
-- @
--
-- Here, we used the 'reinterpret' function to introduce a @'Cleff.State.State' ('Data.Map.Map' 'FilePath' 'String')@ as
-- the in-memory filesystem, making @filesystemToState@ a /reinterpreter/ that "maps" an effect into another effect.
-- We also added a @'Cleff.Fail.Fail' ':>' es@ constraint to our reinterpreter so that we're able to report errors.
-- To make an /interpreter/ out of this is simple, as we just need to interpret the remaining 'Cleff.State.State'
-- effect:
--
-- @
-- runFilesystemPure
--   :: 'Cleff.Fail.Fail' ':>' es
--   => 'Data.Map.Map' 'FilePath' 'String'
--   -> 'Eff' (Filesystem : es) a
--   -> 'Eff' es a
-- runFilesystemPure fs
--   = 'fmap' 'fst'           -- runState returns (Eff es (a, s)), so we need to extract the first component to get (Eff es a)
--   . 'Cleff.State.runState' fs        -- (State (Map FilePath String) : es) ==> es
--   . filesystemToState  -- (Filesystem : es) ==> (State (Map FilePath String) : es)
-- @
--
-- Both of these interpreters can then be applied to computations with the @Filesystem@ effect to give different
-- implementations to the effect.

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
-- It is harder to write interpreters for higher order effects, because the operations of these effects carry
-- computations from arbitrary effect stacks, and we'll need to convert the to the current effect stack that the effect
-- is being interpreted into. Fortunately, Cleff provides convenient combinators for doing so.
--
-- In a 'Handler', you can temporarily "unlift" a computation from an arbitrary effect stack into the current stack via
-- 'toEff', explicitly change the current effect interpretation in the computation via 'toEffWith', or directly express
-- the effect in terms of 'IO' via 'withToIO'.
