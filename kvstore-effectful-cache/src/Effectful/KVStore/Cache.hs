module Effectful.KVStore.Cache
  ( runKVStoreWithCache
  ) where

import Data.Functor               ((<&>))
import Data.Hashable
import Data.Kind
import Data.Maybe                 (isJust)
import Effectful
import Effectful.Cache            as Cache
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.KVStore          as KVS

runKVStoreWithCache ::
  forall (k :: Type) (v :: Type) (es :: [Effect]) (a :: Type).
  (Hashable k, Cache k v :> es) => Eff (KVStore k v : es) a -> Eff es a
runKVStoreWithCache = interpret $ \_ -> \case
  KVS.Update k v -> Cache.insert k v
  KVS.Lookup k   -> Cache.lookup k
  KVS.Exists k   -> Cache.lookup @k @v k <&> isJust
