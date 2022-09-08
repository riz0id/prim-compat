module Test.Word.Word (testTree) where

import Hedgehog (forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Test.Core (TestTree, testGroup, testProp)

import Data.Word.Prim.Compat (fromWord#, toWord#)

import GHC.Exts (plusWord#)

--------------------------------------------------------------------------------

testTree :: TestTree
testTree =
  testGroup
    "Word"
    [ testProp "Unbox" $ property do
        x@(toWord# -> x#) <- forAll (Gen.word Range.constantBounded)
        y@(toWord# -> y#) <- forAll (Gen.word Range.constantBounded)
        x + y === fromWord# (plusWord# x# y#)
    ]
