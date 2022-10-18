
module Effectful.KVStore.StateMap
  ( runKVStoreWithStateMap
  ) where

import           Data.Functor               ((<&>))
import           Data.Kind
import           Data.Map                   (Map)
import qualified Data.Map                   as Map
import           Effectful
import           Effectful.Dispatch.Dynamic (reinterpret)
import           Effectful.KVStore          as KVS
import           Effectful.State.Dynamic    as State

runKVStoreWithStateMap ::
  forall (k :: Type) (v :: Type) (es :: [Effect]) (a :: Type).
     Ord k
  => Map k v -> Eff (KVStore k v : es) a -> Eff es a
runKVStoreWithStateMap initMap = reinterpret (evalStateLocal initMap) $ \_ -> \case
  KVS.Update k v -> State.modify (Map.insert k v)
  KVS.Lookup k   -> State.get <&> Map.lookup k
  KVS.Exists k   -> State.get <&> Map.member @k @v k
