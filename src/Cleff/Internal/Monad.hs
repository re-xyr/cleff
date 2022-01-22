{-# OPTIONS_HADDOCK not-home #-}
-- | This module contains the definition of the 'Eff' monad, which is basically an @'Env' es -> 'IO' a@, as well as
-- functions for manipulating the effect environment type 'Env'. Most of the times, you won't need to use this module
-- directly; user-facing functionalities are all exported via the "Cleff" module.
--
-- __This is an /internal/ module and its API may change even between minor versions.__ Therefore you should be
-- extra careful if you're to depend on this module.
module Cleff.Internal.Monad
  ( -- * Core types
    InternalHandler (..), Env, Eff (..)
  , -- * Performing effect operations
    KnownList, Subset, send
  ) where

import           Cleff.Internal.Effect
import           Control.Monad.Fix          (MonadFix)
import           Control.Monad.Trans.Reader (ReaderT (ReaderT))
import           Data.Mem                   (Mem)
import qualified Data.Mem                   as Mem
import           Data.Rec                   (KnownList, Subset)
import           Type.Reflection            (Typeable, typeRep)

-- | The internal representation of effect handlers. This is just a natural transformation from the effect type
-- @e ('Eff' es)@ to the effect monad @'Eff' es@ for any effect stack @es@.
--
-- In interpreting functions (see "Cleff.Internal.Interpret"), the user-facing 'Cleff.Handler' type is transformed into
-- this type.
newtype InternalHandler e = InternalHandler
  { runHandler :: forall es. e (Eff es) ~> Eff es }

-- | @
-- 'show' (handler :: 'InternalHandler' E) == "Handler E"
-- @
instance Typeable e => Show (InternalHandler e) where
  showsPrec p _ = ("Handler " ++) . showsPrec p (typeRep @e)

-- | The effect memironment that stores handlers of any effect present in the stack @es@.
type Env = Mem InternalHandler

-- | The extensible effect monad. A monad @'Eff' es@ is capable of performing any effect in the /effect stack/ @es@,
-- which is a type-level list that holds all effects available. However, most of the times, for flexibility, @es@
-- should be a polymorphic type variable, and you should use the '(:>)' and '(:>>)' operators in constraints to
-- indicate what effects are in the stack. For example,
--
-- @
-- 'Cleff.Reader.Reader' 'String' ':>' es, 'Cleff.State.State' 'Bool' ':>' es => 'Eff' es 'Integer'
-- @
--
-- allows you to perform operations of the @'Cleff.Reader.Reader' 'String'@ effect and the @'Cleff.State.State' 'Bool'@
-- effect in a computation returning an 'Integer'.
type role Eff nominal representational
newtype Eff es a = Eff { unEff :: Env es -> IO a }
  deriving newtype (Semigroup, Monoid)
  deriving (Functor, Applicative, Monad, MonadFix) via (ReaderT (Env es) IO)

-- | Perform an effect operation, /i.e./ a value of an effect type @e :: 'Effect'@. This requires @e@ to be in the
-- effect stack.
send :: e :> es => e (Eff es) ~> Eff es
send eff = Eff \handlers -> unEff (runHandler (Mem.read handlers) eff) handlers
