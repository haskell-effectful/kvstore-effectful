module Main
  ( main
  ) where

import qualified Test.Effectful.KVStore.Cache as KVStore.Cache (tests)
import           Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [KVStore.Cache.tests]
