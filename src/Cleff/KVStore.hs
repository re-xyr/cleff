module Cleff.KVStore where

import           Cleff
import           Cleff.Error
import           Cleff.State
import           Control.Monad.Extra (fromMaybeM)
import qualified Data.HashMap.Lazy   as HL
import qualified Data.HashMap.Strict as H
import           Data.Hashable       (Hashable)
import qualified Data.IntMap.Lazy    as IL
import qualified Data.IntMap.Strict  as I
import qualified Data.Map            as O
import qualified Data.Map.Lazy       as OL
import           Data.Maybe          (isJust)
import           Data.Typeable       (Typeable)

-- * Effect

-- | An effect that provides operations of accessing a key-value store, like a map data structure or a key-value
-- database.
data KVStore k v :: Effect where
  LookupKV :: k -> KVStore k v m (Maybe v)
  UpdateKV :: k -> Maybe v -> KVStore k v m ()

-- * Operations

makeEffect ''KVStore

-- | Write a value to the given entry.
writeKV :: KVStore k v :> es => k -> v -> Eff es ()
writeKV k = updateKV k . Just

-- | Delete the given entry.
deleteKV :: forall k v es. KVStore k v :> es => k -> Eff es ()
deleteKV k = updateKV k $ Nothing @v

-- | Lookup the value of the given key, if not found, then throw some error.
lookupOrThrowKV :: '[KVStore k v, Error e] :>> es => (k -> e) -> k -> Eff es v
lookupOrThrowKV f k = fromMaybeM (throwError $ f k) $ lookupKV k

-- | Sees if the key is present in the store.
existsKV :: forall k v es. KVStore k v :> es => k -> Eff es Bool
existsKV = fmap isJust . lookupKV @k @v

-- | If the key is present, changes the value via a function.
adjustKV :: forall k v es. KVStore k v :> es => (v -> v) -> k -> Eff es ()
adjustKV f k = lookupKV k >>= \case
  Nothing -> pure ()
  Just x  -> writeKV k $ f x

-- | If the key is present, updates the entry (potentially deleting it) via a function.
modifyKV :: forall k v es. KVStore k v :> es => (v -> Maybe v) -> k -> Eff es ()
modifyKV f k = lookupKV k >>= \case
  Nothing -> pure ()
  Just x  -> updateKV k $ f x

-- | Updates the entry via a function, no matter the key is present or not.
alterKV :: forall k v es. KVStore k v :> es => (Maybe v -> Maybe v) -> k -> Eff es ()
alterKV f k = lookupKV k >>= updateKV k . f

-- * Interpretations

-- | Interpret a 'KVStore' effect in terms of 'O.Map' and strict operations.
kvStoreToMapState :: (Ord k, Typeable k, Typeable v) => Eff (KVStore k v ': es) ~> Eff (State (O.Map k v) ': es)
kvStoreToMapState = reinterpret \case
  LookupKV k   -> gets (O.lookup k)
  UpdateKV k v -> modify (O.update (const v) k)
{-# INLINE kvStoreToMapState #-}

-- | Interpret a 'KVStore' effect in terms of 'OL.Map' and lazy operations.
kvStoreToLazyMapState :: (Ord k, Typeable k, Typeable v) => Eff (KVStore k v ': es) ~> Eff (State (OL.Map k v) ': es)
kvStoreToLazyMapState = reinterpret \case
  LookupKV k   -> gets (OL.lookup k)
  UpdateKV k v -> modify (OL.update (const v) k)
{-# INLINE kvStoreToLazyMapState #-}

-- | Interpret a @'KVStore' 'Int' v@ effect in terms of 'I.IntMap' and strict operations.
kvStoreToIntMapState :: (Typeable k, Typeable v) => Eff (KVStore I.Key v ': es) ~> Eff (State (I.IntMap v) ': es)
kvStoreToIntMapState = reinterpret \case
  LookupKV k   -> gets (I.lookup k)
  UpdateKV k v -> modify (I.update (const v) k)
{-# INLINE kvStoreToIntMapState #-}

-- | Interpret a @'KVStore' 'Int' v@ effect in terms of 'IL.IntMap' and lazy operations.
kvStoreToLazyIntMapState :: (Typeable k, Typeable v) => Eff (KVStore IL.Key v ': es) ~> Eff (State (IL.IntMap v) ': es)
kvStoreToLazyIntMapState = reinterpret \case
  LookupKV k   -> gets (IL.lookup k)
  UpdateKV k v -> modify (IL.update (const v) k)
{-# INLINE kvStoreToLazyIntMapState #-}

-- | Interpret a 'KVStore' effect in terms of 'H.HashMap' and strict operations.
kvStoreToHashMapState :: (Hashable k, Eq k, Typeable k, Typeable v) => Eff (KVStore k v ': es) ~> Eff (State (H.HashMap k v) ': es)
kvStoreToHashMapState = reinterpret \case
  LookupKV k   -> gets (H.lookup k)
  UpdateKV k v -> modify (H.update (const v) k)
{-# INLINE kvStoreToHashMapState #-}

-- | Interpret a 'KVStore' effect in terms of 'HL.HashMap' and lazy operations.
kvStoreToLazyHashMapState :: (Hashable k, Eq k, Typeable k, Typeable v) => Eff (KVStore k v ': es) ~> Eff (State (HL.HashMap k v) ': es)
kvStoreToLazyHashMapState = reinterpret \case
  LookupKV k   -> gets (HL.lookup k)
  UpdateKV k v -> modify (HL.update (const v) k)
{-# INLINE kvStoreToLazyHashMapState #-}
