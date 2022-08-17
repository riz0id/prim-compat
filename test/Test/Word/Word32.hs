module Test.Word.Word32 (testTree) where

import Hedgehog (forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Test.Tasty (DependencyType (AllSucceed), after)

import Test.Core (TestTree, testGroup, testProp)

import Data.Word.Prim.Compat
  ( eqWord32#,
    fromWord32,
    geWord32#,
    gtWord32#,
    leWord32#,
    ltWord32#,
    neWord32#,
    plusWord32#,
    toWord32,
  )

import GHC.Exts (tagToEnum#)

--------------------------------------------------------------------------------

testTree :: TestTree
testTree =
  testGroup
    "Word32"
    [ testProp "Unbox" $ property do
        x@(toWord32 -> x#) <- forAll (Gen.word32 Range.constantBounded)
        y@(toWord32 -> y#) <- forAll (Gen.word32 Range.constantBounded)
        x + y === fromWord32 (plusWord32# x# y#)
    , after AllSucceed "Word32.Unbox" testTreeComparison
    ]

testTreeComparison :: TestTree
testTreeComparison =
  testGroup
    "Comparison"
    [ testProp "gtWord32#" $ property do
        x@(toWord32 -> x#) <- forAll (Gen.word32 Range.constantBounded)
        y@(toWord32 -> y#) <- forAll (Gen.word32 Range.constantBounded)
        (x > y) === tagToEnum# (gtWord32# x# y#)
    , testProp "geWord32#" $ property do
        x@(toWord32 -> x#) <- forAll (Gen.word32 Range.constantBounded)
        y@(toWord32 -> y#) <- forAll (Gen.word32 Range.constantBounded)
        (x >= y) === tagToEnum# (geWord32# x# y#)
    , testProp "eqWord32#" $ property do
        x@(toWord32 -> x#) <- forAll (Gen.word32 Range.constantBounded)
        y@(toWord32 -> y#) <- forAll (Gen.word32 Range.constantBounded)
        (x == y) === tagToEnum# (eqWord32# x# y#)
    , testProp "neWord32#" $ property do
        x@(toWord32 -> x#) <- forAll (Gen.word32 Range.constantBounded)
        y@(toWord32 -> y#) <- forAll (Gen.word32 Range.constantBounded)
        (x /= y) === tagToEnum# (neWord32# x# y#)
    , testProp "ltWord32#" $ property do
        x@(toWord32 -> x#) <- forAll (Gen.word32 Range.constantBounded)
        y@(toWord32 -> y#) <- forAll (Gen.word32 Range.constantBounded)
        (x < y) === tagToEnum# (ltWord32# x# y#)
    , testProp "leWord32#" $ property do
        x@(toWord32 -> x#) <- forAll (Gen.word32 Range.constantBounded)
        y@(toWord32 -> y#) <- forAll (Gen.word32 Range.constantBounded)
        (x <= y) === tagToEnum# (leWord32# x# y#)
    ]