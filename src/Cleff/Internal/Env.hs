module Cleff.Internal.Env
  ( InternalHandler (InternalHandler, runHandler)
  , emptyEnv
  , adjustEnv
  , peekEnv
  , readEnv
  , writeEnv
  , replaceEnv
  , appendEnv
  , updateEnv
  ) where

import           Cleff.Internal
import           Cleff.Internal.Monad
import           Cleff.Internal.Rec   (Rec)
import qualified Cleff.Internal.Rec   as Rec
import           Data.Any             (fromAny, pattern Any)
import qualified Data.RadixVec        as Vec

-- | The internal representation of effect handlers. This is just a natural transformation from the effect type
-- @e ('Eff' es)@ to the effect monad @'Eff' es@ for any effect stack @es@.
--
-- In interpreting functions (see "Cleff.Internal.Interpret"), the user-facing 'Cleff.Handler' type is transformed into
-- this type.
newtype InternalHandler e = InternalHandler { runHandler :: ∀ es. e (Eff es) ~> Eff es }

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

