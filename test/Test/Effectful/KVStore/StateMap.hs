{-# LANGUAGE OverloadedStrings #-}
module Test.Effectful.KVStore.StateMap
  ( tests
  ) where

import Data.Kind
import Data.Map                   qualified as Map
import Effectful
import Effectful.KVStore
import Effectful.KVStore.StateMap
import Hedgehog.Gen               qualified as Gen
import Hedgehog.Range             qualified as Range
import Test.Effectful.KVStore     (kvStoreTests)

import Test.Tasty

tests :: TestTree
tests =
     kvStoreTests genK genV
                  (createStateMapKVStoreInterpreter @String @Int)
                  "StateMap"
  where
    genK = Gen.string (Range.singleton 128) Gen.hexit
    genV = Gen.int (Range.singleton 128)

createStateMapKVStoreInterpreter ::
     forall (k :: Type) v a . (Ord k)
  => IO ( Eff '[KVStore k v, IOE] a -> IO a)
createStateMapKVStoreInterpreter =
  pure $ runEff . runKVStoreWithStateMap (Map.empty @k @v)
