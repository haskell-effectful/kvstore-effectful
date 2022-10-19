{-# LANGUAGE OverloadedStrings #-}
module Test.Effectful.KVStore.Cache
  ( tests
  ) where

import Data.Cache              as Data
import Data.Hashable
import Data.Kind
import Effectful
import Effectful.Cache         as Cache
import Effectful.KVStore
import Effectful.KVStore.Cache
import Hedgehog.Gen            qualified as Gen
import Hedgehog.Range          qualified as Range
import Test.Effectful.KVStore

import Test.Tasty

tests :: TestTree
tests =
     kvStoreTests genK genV (createCacheKVStoreInterpreter @String @Int) "Cache"
  where
    genK = Gen.string (Range.singleton 128) Gen.hexit
    genV = Gen.int (Range.singleton 128)

createCacheKVStoreInterpreter ::
     forall (k :: Type) v a . (Hashable k)
  => IO ( Eff '[KVStore k v, Cache.Cache k v, IOE] a -> IO a)
createCacheKVStoreInterpreter = do
  cache <- newCache Nothing :: IO (Data.Cache k v)
  pure $ runEff . runCacheIO cache . runKVStoreWithCache
