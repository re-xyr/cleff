{-# OPTIONS_HADDOCK not-home #-}
-- |
-- Copyright: (c) 2021 Xy Ren
-- License: BSD3
-- Maintainer: xy.r@outlook.com
-- Stability: unstable
-- Portability: non-portable (GHC only)
--
-- This module contains the definition of the 'Eff' monad. Most of the times, you won't need to use this module
-- directly; user-facing functionalities are all exported via the "Cleff" module.
--
-- __This is an /internal/ module and its API may change even between minor versions.__ Therefore you should be
-- extra careful if you're to depend on this module.
module Cleff.Internal.Monad
  ( -- * The 'Eff' monad
    InternalHandler (InternalHandler, runHandler)
  , Eff (Eff, unEff)
    -- * Effect environment
  , Env
  , HandlerPtr
  , emptyEnv
  , adjustEnv
  , peekEnv
  , readEnv
  , writeEnv
  , replaceEnv
  , appendEnv
  , updateEnv
    -- * Constraints on effect stacks
  , (:>)
  , (:>>)
  , KnownList
  , Subset
    -- * Performing effect operations
  , send
  , sendVia
  ) where

import           Cleff.Internal
import           Cleff.Internal.Rec  (KnownList, Rec, Subset, type (:>))
import qualified Cleff.Internal.Rec  as Rec
import           Control.Applicative (Applicative (liftA2))
import           Control.Monad.Fix   (MonadFix (mfix))
import           Data.Any            (Any, fromAny, pattern Any)
import           Data.Kind           (Constraint)
import           Data.RadixVec       (RadixVec)
import qualified Data.RadixVec       as Vec

-- * The 'Eff' monad

-- | The internal representation of effect handlers. This is just a natural transformation from the effect type
-- @e ('Eff' es)@ to the effect monad @'Eff' es@ for any effect stack @es@.
--
-- In interpreting functions (see "Cleff.Internal.Interpret"), the user-facing 'Cleff.Handler' type is transformed into
-- this type.
newtype InternalHandler e = InternalHandler { runHandler :: ∀ es. e (Eff es) ~> Eff es }

-- | The extensible effects monad. The monad @'Eff' es@ is capable of performing any effect in the /effect stack/ @es@,
-- which is a type-level list that holds all effects available.
--
-- The best practice is to always use a polymorphic type variable for the effect stack @es@, and then use the type
-- operator '(:>)' in constraints to indicate what effects are available in the stack. For example,
--
-- @
-- ('Cleff.Reader.Reader' 'String' ':>' es, 'Cleff.State.State' 'Bool' ':>' es) => 'Eff' es 'Integer'
-- @
--
-- means you can perform operations of the @'Cleff.Reader.Reader' 'String'@ effect and the @'Cleff.State.State' 'Bool'@
-- effect in a computation returning an 'Integer'. A convenient shorthand, '(:>>)', can also be used to indicate
-- multiple effects being in a stack:
--
-- @
-- '['Cleff.Reader.Reader' 'String', 'Cleff.State.State' 'Bool'] ':>>' es => 'Eff' es 'Integer'
-- @
--
-- The reason why you should always use a polymorphic effect stack as opposed to a concrete list of effects are that:
--
-- * it can contain other effects that are used by computations other than the current one, and
-- * it does not require you to run the effects in any particular order.
type role Eff nominal representational
newtype Eff es a = Eff { unEff :: Env es -> IO a }
  -- ^ The effect monad receives an effect environment 'Env' that contains all effect handlers and produces an 'IO'
  -- action.

instance Functor (Eff es) where
  fmap f (Eff x) = Eff (fmap f . x)
  x <$ Eff y = Eff \es -> x <$ y es

instance Applicative (Eff es) where
  pure = Eff . const . pure
  Eff f <*> Eff x = Eff \es -> f es <*> x es
  Eff x <*  Eff y = Eff \es -> x es <*  y es
  Eff x  *> Eff y = Eff \es -> x es  *> y es
  liftA2 f (Eff x) (Eff y) = Eff \es -> liftA2 f (x es) (y es)

instance Monad (Eff es) where
  -- no 'return', because the default impl is correct and it is going to be deprecated anyway
  Eff x >>= f = Eff \es -> x es >>= \x' -> unEff (f x') es
  (>>) = (*>) -- More efficient, since the default is @x >> y = x >>= const y@

instance MonadFix (Eff es) where
  mfix f = Eff \es -> mfix \x -> unEff (f x) es

-- * Effect environment

-- | The /effect environment/ that corresponds effects in the stack to their respective 'InternalHandler's. This
-- structure simulates memory: handlers are retrieved via pointers ('HandlerPtr's), and for each effect in the stack
-- we can either change what pointer it uses or change the handler the pointer points to. The former is used for global
-- effect interpretation ('Cleff.reinterpretN') and the latter for local interpretation ('Cleff.toEffWith') in order to
-- retain correct HO semantics. For more details on this see https://github.com/re-xyr/cleff/issues/5.
type role Env nominal
data Env (es :: [Effect]) = Env
  {-# UNPACK #-} !Int -- ^ The next address to allocate.
  {-# UNPACK #-} !(Rec es) -- ^ The effect stack storing pointers to handlers.
  {-# UNPACK #-} !(RadixVec Any) -- ^ The storage that corresponds pointers to handlers.

-- | Create an empty 'Env' with no address allocated.
emptyEnv :: Env '[]
emptyEnv = Env 0 Rec.empty Vec.empty
{-# INLINE emptyEnv #-}

-- | Adjust the effect stack via an function over 'Rec'.
adjustEnv :: ∀ es' es. (Rec es -> Rec es') -> Env es -> Env es'
adjustEnv f = \(Env n re mem) -> Env n (f re) mem
{-# INLINE adjustEnv #-}

-- | Peek the next address to be allocated. \( O(1) \).
peekEnv :: ∀ e es. Env es -> HandlerPtr e
peekEnv (Env n _ _) = HandlerPtr n
{-# INLINE peekEnv #-}

-- | Read the handler a pointer points to. \( O(1) \).
readEnv :: ∀ e es. e :> es => Env es -> InternalHandler e
readEnv (Env _ re mem) = fromAny $ Vec.lookup (unHandlerPtr (Rec.index @e re)) mem
{-# INLINE readEnv #-}

-- | Overwrite the handler a pointer points to. \( O(1) \).
writeEnv :: ∀ e es. HandlerPtr e -> InternalHandler e -> Env es -> Env es
writeEnv (HandlerPtr m) x (Env n re mem) = Env n re $ Vec.update m (Any x) mem
{-# INLINE writeEnv #-}

-- | Replace the handler pointer of an effect in the stack. \( O(n) \).
replaceEnv :: ∀ e es. e :> es => InternalHandler e -> Env es -> Env es
replaceEnv x (Env n re mem) = Env (n + 1) (Rec.update @e (HandlerPtr n) re) (Vec.snoc mem $ Any x)
{-# INLINE replaceEnv #-}

-- | Add a new effect to the stack with its corresponding handler pointer. \( O(n) \).
appendEnv :: ∀ e es. InternalHandler e -> Env es -> Env (e : es)
appendEnv x (Env n re mem) = Env (n + 1) (Rec.cons (HandlerPtr n) re) (Vec.snoc mem $ Any x)
{-# INLINE appendEnv #-}

-- | Use the state of LHS as a newer version for RHS. \( O(1) \).
updateEnv :: ∀ es es'. Env es' -> Env es -> Env es
updateEnv (Env n _ mem) (Env _ re' _) = Env n re' mem
{-# INLINE updateEnv #-}

-- * Performing effect operations

-- | @xs ':>>' es@ means the list of effects @xs@ are all present in the effect stack @es@. This is a convenient type
-- alias for @(e1 ':>' es, ..., en ':>' es)@.
type family xs :>> es :: Constraint where
  '[] :>> _ = ()
  (x : xs) :>> es = (x :> es, xs :>> es)
infix 0 :>>

-- | Perform an effect operation, /i.e./ a value of an effect type @e :: 'Effect'@. This requires @e@ to be in the
-- effect stack.
send :: e :> es => e (Eff es) ~> Eff es
send = sendVia id
{-# INLINE send #-}

-- | Perform an action in another effect stack via a transformation to that stack; in other words, this function "maps"
-- the effect operation from effect stack @es@ to @es'@. This is a largely generalized version of 'send'; only use this
-- if you are sure about what you're doing.
--
-- @
-- 'send' = 'sendVia' 'id'
-- @
--
-- @since 0.2.0.0
sendVia :: e :> es' => (Eff es ~> Eff es') -> e (Eff es) ~> Eff es'
sendVia f e = Eff \es -> unEff (f (runHandler (readEnv es) e)) es
{-# INLINE sendVia #-}
