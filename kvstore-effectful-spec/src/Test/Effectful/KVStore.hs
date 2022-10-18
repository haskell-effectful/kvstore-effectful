{-# LANGUAGE OverloadedStrings #-}
module Test.Effectful.KVStore
  ( kvStoreTests
  ) where

import           Prelude                   hiding (lookup)

import           Control.Monad
import           Control.Monad.Trans.Class
import           Effectful
import           Effectful.KVStore

import           Hedgehog
import           Test.Tasty
import           Test.Tasty.Hedgehog       (testPropertyNamed)

kvStoreTests ::
     forall es k v .
     ( KVStore k v :> es
     , Show k
     , Eq v
     , Show v
     )
  => Gen k
  -> Gen v
  -> (forall a . IO (Eff es a -> IO a))
  -> String
  -> TestTree
kvStoreTests genK genV mkInterpreter name = testGroup ("KVStore." ++ name)
  [ testPropertyNamed
      "when update and then lookup, value should be returned"
      "testUpdateLookup"
      (testInterpreter mkInterpreter keyAndValue testUpdateLookup)
  , testPropertyNamed
      "when double update and then lookup, second value returned"
      "testDoubleUpdateLookup"
      (testInterpreter mkInterpreter keyAndTwoValues testDoubleUpdateLookup)
  , testPropertyNamed
      "when updated, it should exists"
      "testUpdateThenExists"
      (testInterpreter mkInterpreter keyAndValue testUpdateThenExists)
  , testPropertyNamed
      "when empty store, exists should always return False for any key"
      "testExistsFalseForEmpty"
      (testInterpreter mkInterpreter keyAndValue testExistsFalseForEmpty)
  ]
  where
    keyAndValue = forAll $ do
      k <- genK
      v <- genV
      pure (k, v)
    keyAndTwoValues = forAll $ do
      k <- genK
      v1 <- genV
      v2 <- genV
      pure (k, v1, v2)

testInterpreter ::
     (forall a . IO (Eff es a -> IO a))
  -> PropertyT IO env
  -> (env -> Eff es (PropertyT IO ()))
  -> Property
testInterpreter mkInterpreter createEnv createProg = property $ do
  env <- createEnv
  interpreter <- lift mkInterpreter
  let prog = createProg env
  join $ lift $ interpreter prog

testUpdateLookup ::
  ( MonadTest m
  , KVStore k v :> es
  , Eq v
  , Show v
  )
  => (k, v) -> Eff es (m ())
testUpdateLookup (k, v) = do
  update k v
  res <- lookup k
  pure $ res === Just v

testDoubleUpdateLookup ::
  ( MonadTest m
  , KVStore k v :> es
  , Eq v
  , Show v
  )
  => (k, v, v) -> Eff es (m ())
testDoubleUpdateLookup (k, v1, v2) = do
  update k v1
  update k v2
  res <- lookup k
  pure $ res === Just v2

testUpdateThenExists ::
    forall k v m es .
  ( MonadTest m
  , KVStore k v :> es
  )
  => (k, v) -> Eff es (m ())
testUpdateThenExists (k, v) = do
  update k v
  res <- exists @k @v k
  pure $ res === True

testExistsFalseForEmpty ::
    forall k v m es .
  ( MonadTest m
  , KVStore k v :> es
  )
  => (k, v) -> Eff es (m ())
testExistsFalseForEmpty (k, _v) = do
  res <- exists @k @v k
  pure $ res === False
