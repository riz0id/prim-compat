module Test.Int.Int (testTree) where

import Hedgehog (forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Test.Core (TestTree, testGroup, testProp)

import Data.Int.Prim.Compat (fromInt#, toInt#)

--------------------------------------------------------------------------------

testTree :: TestTree
testTree =
  testGroup
    "Int"
    [ testProp "Unbox" $ property do
        x <- forAll (Gen.int Range.constantBounded)
        x === fromInt# (toInt# x)
    ]
