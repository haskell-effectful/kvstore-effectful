module Effectful.KVStore
  ( KVStore (..)
  , exists
  , lookup
  , update
  ) where

import Data.Kind
import Effectful
import Effectful.Dispatch.Dynamic
import Prelude                    hiding (lookup)

type KVStore :: Type -> Type -> Effect
data KVStore k v :: Effect where
  Update :: k -> v -> KVStore k v m ()
  Lookup :: k -> KVStore k v m (Maybe v)
  Exists :: k -> KVStore k v m Bool

type instance DispatchOf (KVStore k v) = 'Dynamic

update :: (HasCallStack, KVStore k v :> es) => k -> v -> Eff es ()
update k v = send $ Update k v

lookup :: (HasCallStack, KVStore k v :> es) => k -> Eff es (Maybe v)
lookup k = send $ Lookup k

exists :: forall k v es .(HasCallStack, KVStore k v :> es) => k -> Eff es Bool
exists k = send $ Exists @k @v k
