-- | This module contains the definition of the 'Eff' monad, which is basically an @'Env' es -> 'IO' a@, as well as
-- functions for manipulating the effect environment type 'Env'. Most of the times, you won't need to use this module
-- directly; user-facing functionalities are all exported via the "Cleff" module.
{-# OPTIONS_HADDOCK not-home #-}
module Cleff.Internal.Monad
  ( -- * Core types
    InternalHandler (..), Env (..), Eff (..)
  , -- * Effect environment axioms
    emptyEnv, KnownList (..), contractEnv, ExpandEnv (..), expandEnv, getHandler, modifyHandler, insertHandler
  , -- * Performing effect operations
    send
  ) where

import           Cleff.Internal.Effect
import           Control.Monad.Fix         (MonadFix (mfix))
import           Control.Monad.Primitive   (PrimMonad (PrimState))
import           Data.Primitive.SmallArray
import           GHC.Exts                  (Any)
import           Unsafe.Coerce             (unsafeCoerce)

-- | The internal representation of effect handlers. This is just a natural transformation from the effect type
-- @e ('Eff' es)@ to the effect monad @'Eff' es@ for any effect stack @es@ that has @e@ in it.
--
-- In interpreting functions (see "Cleff.Internal.Interpret"), the user-facing 'Cleff.Handler' type is transformed into
-- this type.
newtype InternalHandler e = InternalHandler
  { runHandler :: forall esSend. e :> esSend => e (Eff esSend) ~> Eff esSend }

-- | The effect environment that stores handlers of any effect present in the stack @es@.
type role Env nominal -- This blocks users from liberally 'coerce'ing between different effect stacks.
data Env (es :: [Effect]) = Env
  { envLen :: {-# UNPACK #-} !Int
  , envArr :: {-# UNPACK #-} !(SmallArray Any)
  }

-- | The extensible effect monad. A monad @'Eff' es@ is capable of performing any effect in the /effect stack/ @es@.
-- Most of the times, @es@ should be a polymorphic effect stack, constrained by the '(:>)' and '(:>>)' operators that
-- indicate what effects are present in it. For example, the type
--
-- @
-- 'Cleff.Reader.Reader' 'String' ':>' es, 'Cleff.State.State' 'Bool' ':>' es => 'Eff' es 'Integer'
-- @
--
-- allows you to perform operations of the @'Cleff.Reader.Reader' 'String'@ effect and the @'Cleff.State.State' 'Bool'@
-- effect in a computation returning an 'Integer'.
type role Eff nominal nominal
newtype Eff es a = PrimEff { primRunEff :: Env es -> IO a }
  deriving (Semigroup, Monoid)

instance Functor (Eff es) where
  fmap f (PrimEff m) = PrimEff (fmap f . m)
  a <$ PrimEff m = PrimEff \es -> a <$ m es

instance Applicative (Eff es) where
  pure x = PrimEff \_ -> pure x
  PrimEff mf <*> PrimEff mx = PrimEff \es -> mf es <*> mx es
  PrimEff ma  *> PrimEff mb = PrimEff \es -> ma es  *> mb es
  PrimEff ma <*  PrimEff mb = PrimEff \es -> ma es <*  mb es

instance Monad (Eff es) where
  return = pure
  PrimEff m >>= f = PrimEff \es -> m es >>= \a -> primRunEff (f a) es
  PrimEff ma >> PrimEff mb = PrimEff \es -> ma es >> mb es

instance MonadFix (Eff es) where
  mfix f = PrimEff \es -> mfix $ \a -> primRunEff (f a) es

-- | The environment for the empty effect stack.
emptyEnv :: Env '[]
emptyEnv = Env 0 $ runSmallArray $ newSmallArray 0 $ error "Reading nonexistent data"

-- | A list with known content and allows a concrete representation to be computed. You should not define instances
-- for this class anyhow.
class KnownList es where
  tailSize :: Int
  tailSize = error "unimplemented"
instance KnownList '[] where
  tailSize = 0
instance KnownList es => KnownList (e ': es) where
  tailSize = 1 + tailSize @es

-- | Contract larger environment into a smaller one; O(1).
contractEnv :: forall es' es. KnownList es' => Env (es' ++ es) -> Env es
contractEnv (Env len arr) = Env (len - tailSize @es') arr

-- | Witnesses that @es@ is contained in @es@, so that @es@ can be expanded to @es' '++' es@. You should not define
-- instance for this class anyhow.
class KnownList es' => ExpandEnv es' es where
  unsafeExtractInto :: PrimMonad m => SmallMutableArray (PrimState m) Any -> Env es -> m ()
  unsafeExtractInto = error "unimplemented"
instance ExpandEnv '[] es where
  unsafeExtractInto _ _ = pure ()
instance (e :> es, ExpandEnv es' es) => ExpandEnv (e ': es') es where
  unsafeExtractInto marr env = do
    writeSmallArray marr (tailSize @es') (unsafeCoerce $ getHandler @e env)
    unsafeExtractInto @es' marr env

-- | Expand smaller environment into a larger one, given the added part is already present in the original stack;
-- amortized O(n).
expandEnv :: forall es' es. ExpandEnv es' es => Env es -> Env (es' ++ es)
expandEnv env@(Env len arr) = Env len' $ runSmallArray do
  marr <- newSmallArray (tailSize @es') $ error "Reading nonexistent data"
  unsafeExtractInto @es' marr env
  go (sizeofSmallArray arr) marr
  where
    len' = len + tailSize @es'
    go :: PrimMonad m => Int ->SmallMutableArray (PrimState m) Any -> m (SmallMutableArray (PrimState m) Any)
    go cap ext = if len' > cap
      then go (growCapacity cap) ext
      else do
        marr <- newSmallArray cap $ error "Reading nonexistent data"
        copySmallArray marr 0 arr 0 len
        copySmallMutableArray marr len ext 0 (tailSize @es')
        pure marr

-- | Get the handler from the environment for an effect present in the effect stack; O(1).
getHandler :: forall e es. e :> es => Env es -> InternalHandler e
getHandler (Env len arr) = unsafeCoerce $ indexSmallArray arr (len - effectIx @e @es - 1)

-- | Modify a handler that is already on the stack; O(n).
modifyHandler :: forall e es. e :> es => InternalHandler e -> Env es -> Env es
modifyHandler f (Env len arr) = Env len $ runSmallArray do
  mclone <- thawSmallArray arr 0 (sizeofSmallArray arr)
  writeSmallArray mclone (len - effectIx @e @es - 1) (unsafeCoerce f)
  pure mclone

-- | Insert a handler into an environment to extend the stack; O(n).
insertHandler :: forall e es. InternalHandler e -> Env es -> Env (e ': es)
insertHandler f (Env len arr) = Env (len + 1) $ runSmallArray case compare len capacity of
  LT -> do
    mclone <- thawSmallArray arr 0 capacity
    writeSmallArray mclone len (unsafeCoerce f)
    pure mclone
  EQ -> do
    mclone <- newSmallArray (growCapacity capacity) $ error "Reading nonexistent data"
    copySmallArray mclone 0 arr 0 len
    writeSmallArray mclone len (unsafeCoerce f)
    pure mclone
  GT -> error "Handler takes imaginary capacity"
  where
    capacity = sizeofSmallArray arr

-- | Apply the 1.5 growth factor.
growCapacity :: Int -> Int
growCapacity cap = cap * 3 `div` 2 + 1
{-# INLINE growCapacity #-}

-- | Perform an effect operation, /i.e./ a value constructed by a constructor of an effect type @e@, given @e@ is in
-- the effect stack.
send :: e :> es => e (Eff es) ~> Eff es
send eff = PrimEff \handlers -> primRunEff (runHandler (getHandler handlers) eff) handlers
{-# INLINE send #-}
