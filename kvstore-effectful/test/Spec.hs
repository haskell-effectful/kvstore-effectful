module Main
  ( main
  ) where

import qualified Test.Effectful.KVStore.StateMap as KVStore.StateMap (tests)
import           Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [KVStore.StateMap.tests]
