module Test.Int.Int8 (testTree) where

import Hedgehog (forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Test.Core (TestTree, testGroup, testProp)

import Data.Int.Prim.Compat (fromInt8#, toInt8#)

--------------------------------------------------------------------------------

testTree :: TestTree
testTree =
  testGroup
    "Int8"
    [ testProp "Unbox" $ property do
        x <- forAll (Gen.int8 Range.constantBounded)
        x === fromInt8# (toInt8# x)
    ]
