module Test.Int.Int16 (testTree) where

import Hedgehog (forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Test.Core (TestTree, testGroup, testProp)

import Data.Int.Prim.Compat (fromInt16, toInt16)

--------------------------------------------------------------------------------

testTree :: TestTree
testTree =
  testGroup
    "Int16"
    [ testProp "Unbox" $ property do
        x <- forAll (Gen.int16 Range.constantBounded)
        x === fromInt16 (toInt16 x)
    ]
