module Test.Int.Int32 (testTree) where

import Hedgehog (forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Test.Tasty (DependencyType (AllSucceed), after)

import Test.Core (TestTree, testGroup, testProp)

import Data.Int.Prim.Compat
  ( eqInt32#,
    fromInt32,
    geInt32#,
    gtInt32#,
    leInt32#,
    ltInt32#,
    neInt32#,
    toInt32,
  )

import GHC.Exts (tagToEnum#)

--------------------------------------------------------------------------------

testTree :: TestTree
testTree =
  testGroup
    "Int32"
    [ testProp "Unbox" $ property do
        x <- forAll (Gen.int32 Range.constantBounded)
        x === fromInt32 (toInt32 x)
    , after AllSucceed "Int32.Unbox" testTreeComparison
    ]

testTreeComparison :: TestTree
testTreeComparison =
  testGroup
    "Comparison"
    [ testProp "gtInt32#" $ property do
        x@(toInt32 -> x#) <- forAll (Gen.int32 Range.constantBounded)
        y@(toInt32 -> y#) <- forAll (Gen.int32 Range.constantBounded)
        (x > y) === tagToEnum# (gtInt32# x# y#)
    , testProp "geInt32#" $ property do
        x@(toInt32 -> x#) <- forAll (Gen.int32 Range.constantBounded)
        y@(toInt32 -> y#) <- forAll (Gen.int32 Range.constantBounded)
        (x >= y) === tagToEnum# (geInt32# x# y#)
    , testProp "eqInt32#" $ property do
        x@(toInt32 -> x#) <- forAll (Gen.int32 Range.constantBounded)
        y@(toInt32 -> y#) <- forAll (Gen.int32 Range.constantBounded)
        (x == y) === tagToEnum# (eqInt32# x# y#)
    , testProp "neInt32#" $ property do
        x@(toInt32 -> x#) <- forAll (Gen.int32 Range.constantBounded)
        y@(toInt32 -> y#) <- forAll (Gen.int32 Range.constantBounded)
        (x /= y) === tagToEnum# (neInt32# x# y#)
    , testProp "ltInt32#" $ property do
        x@(toInt32 -> x#) <- forAll (Gen.int32 Range.constantBounded)
        y@(toInt32 -> y#) <- forAll (Gen.int32 Range.constantBounded)
        (x < y) === tagToEnum# (ltInt32# x# y#)
    , testProp "leInt32#" $ property do
        x@(toInt32 -> x#) <- forAll (Gen.int32 Range.constantBounded)
        y@(toInt32 -> y#) <- forAll (Gen.int32 Range.constantBounded)
        (x <= y) === tagToEnum# (leInt32# x# y#)
    ]