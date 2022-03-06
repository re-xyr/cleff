{-# LANGUAGE UnboxedTuples #-}
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
  ( -- * Basic types
    Effect
  , type (:>)
  , type (:>>)
  , type (~>)
  , type (++)
    -- * The 'Eff' monad
  , InternalHandler (InternalHandler, runHandler)
  , Eff (Eff, unEff)
    -- * Effect environment
  , Env
  , HandlerPtr
  , emptyEnv
  , adjustEnv
  , allocaEnv
  , readEnv
  , writeEnv
  , replaceEnv
  , appendEnv
  , updateEnv
    -- * Performing effect operations
  , KnownList
  , Subset
  , send
  , sendVia
  ) where

import           Cleff.Internal.Any  (Any, fromAny, toAny)
import           Cleff.Internal.Rec  (Effect, Elem, HandlerPtr (HandlerPtr, unHandlerPtr), KnownList, Rec, Subset,
                                      type (++))
import qualified Cleff.Internal.Rec  as Rec
import           Control.Applicative (Applicative (liftA2))
import           Control.Monad.Fix   (MonadFix (mfix))
import           Data.IntMap.Strict  (IntMap)
import qualified Data.IntMap.Strict  as Map
import           Data.Kind           (Constraint)

-- * Basic types

-- | @e ':>' es@ means the effect @e@ is present in the effect stack @es@, and therefore can be used in an
-- @'Cleff.Eff' es@ computation.
type (:>) = Elem
infix 0 :>

-- | @xs ':>>' es@ means the list of effects @xs@ are all present in the effect stack @es@. This is a convenient type
-- alias for @(e1 ':>' es, ..., en ':>' es)@.
type family xs :>> es :: Constraint where
  '[] :>> _ = ()
  (x : xs) :>> es = (x :> es, xs :>> es)
infix 0 :>>

-- | A natural transformation from @f@ to @g@. With this, instead of writing
--
-- @
-- runSomeEffect :: 'Eff' (SomeEffect : es) a -> 'Eff' es a
-- @
--
-- you can write:
--
-- @
-- runSomeEffect :: 'Eff' (SomeEffect : es) ~> 'Eff' es
-- @
type f ~> g = ∀ a. f a -> g a

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
-- operators '(:>)' and '(:>>)' in constraints to indicate what effects are available in the stack. For example,
--
-- @
-- ('Cleff.Reader.Reader' 'String' ':>' es, 'Cleff.State.State' 'Bool' ':>' es) => 'Eff' es 'Integer'
-- @
--
-- means you can perform operations of the @'Cleff.Reader.Reader' 'String'@ effect and the @'Cleff.State.State' 'Bool'@
-- effect in a computation returning an 'Integer'. The reason why you should always use a polymorphic effect stack as
-- opposed to a concrete list of effects are that
--
-- * it can contain other effects that are used by computations other than the current one, and
-- * it does not require you to run the effects in any particular order.
type role Eff nominal representational
newtype Eff es a = Eff { unEff :: Env es -> IO a }
  -- ^ The effect monad receives an effect environment 'Env' that contains all effect handlers and produces an 'IO'
  -- action.

instance Functor (Eff es) where
  fmap f (Eff x) = Eff (fmap f . x)
  {-# INLINE fmap #-}
  x <$ Eff y = Eff \es -> x <$ y es
  {-# INLINE (<$) #-}

instance Applicative (Eff es) where
  pure = Eff . const . pure
  {-# INLINE pure #-}
  Eff f <*> Eff x = Eff \es -> f es <*> x es
  {-# INLINE (<*>) #-}
  Eff x <*  Eff y = Eff \es -> x es <*  y es
  {-# INLINE (<*) #-}
  Eff x  *> Eff y = Eff \es -> x es  *> y es
  {-# INLINE (*>) #-}
  liftA2 f (Eff x) (Eff y) = Eff \es -> liftA2 f (x es) (y es)
  {-# INLINE liftA2 #-}

instance Monad (Eff es) where
  -- no 'return', because the default impl is correct and it is going to be deprecated anyway
  Eff x >>= f = Eff \es -> x es >>= \x' -> unEff (f x') es
  {-# INLINE (>>=) #-}
  (>>) = (*>) -- More efficient, since the default is @x >> y = x >>= const y@
  {-# INLINE (>>) #-}

instance MonadFix (Eff es) where
  mfix f = Eff \es -> mfix \x -> unEff (f x) es
  {-# INLINE mfix #-}

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
  !(IntMap Any) -- ^ The map that corresponds pointers to handlers.

-- | Create an empty 'Env' with no address allocated.
emptyEnv :: Env '[]
emptyEnv = Env 0 Rec.empty Map.empty
{-# INLINE emptyEnv #-}

-- | Adjust the effect stack via an function over 'Rec'.
adjustEnv :: ∀ es' es. (Rec es -> Rec es') -> Env es -> Env es'
adjustEnv f (Env n re mem) = Env n (f re) mem
{-# INLINE adjustEnv #-}

-- | Allocate a new, empty address for a handler. \( O(1) \).
allocaEnv :: ∀ e es. Env es -> (# HandlerPtr e, Env es #)
allocaEnv (Env n re mem) = (# HandlerPtr n, Env (n + 1) re mem #)
{-# INLINE allocaEnv #-}

-- | Read the handler a pointer points to. \( O(1) \).
readEnv :: ∀ e es. Elem e es => Env es -> InternalHandler e
readEnv (Env _ re mem) = fromAny $ mem Map.! unHandlerPtr (Rec.index @e re)
{-# INLINE readEnv #-}

-- | Overwrite the handler a pointer points to. \( O(1) \).
writeEnv :: ∀ e es. HandlerPtr e -> InternalHandler e -> Env es -> Env es
writeEnv (HandlerPtr m) x (Env n re mem) = Env n re (Map.insert m (toAny x) mem)
{-# INLINE writeEnv #-}

-- | Replace the handler pointer of an effect in the stack. \( O(n) \).
replaceEnv :: ∀ e es. Elem e es => HandlerPtr e -> InternalHandler e -> Env es -> Env es
replaceEnv (HandlerPtr m) x (Env n re mem) = Env n (Rec.update @e (HandlerPtr m) re) (Map.insert m (toAny x) mem)
{-# INLINE replaceEnv #-}

-- | Add a new effect to the stack with its corresponding handler pointer. \( O(n) \).
appendEnv :: ∀ e es. HandlerPtr e -> InternalHandler e -> Env es -> Env (e : es)
appendEnv (HandlerPtr m) x (Env n re mem) = Env n (Rec.cons (HandlerPtr m) re) (Map.insert m (toAny x) mem)
{-# INLINE appendEnv #-}

-- | Use the state of LHS as a newer version for RHS. \( O(1) \).
updateEnv :: ∀ es es'. Env es' -> Env es -> Env es
updateEnv (Env n _ mem) (Env _ re' _) = Env n re' mem
{-# INLINE updateEnv #-}

-- * Performing effect operations

-- | Perform an effect operation, /i.e./ a value of an effect type @e :: 'Effect'@. This requires @e@ to be in the
-- effect stack.
send :: e :> es => e (Eff es) ~> Eff es
send = sendVia id

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
