module Test.Word.Word16 (testTree) where

import Hedgehog (forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Test.Core (TestTree, testGroup, testProp)

import Data.Word.Prim.Compat (fromWord16, toWord16)

import GHC.Exts (plusWord16#)

--------------------------------------------------------------------------------

testTree :: TestTree
testTree =
  testGroup
    "Word16"
    [ testProp "Unbox" $ property do
        x@(toWord16 -> x#) <- forAll (Gen.word16 Range.constantBounded)
        y@(toWord16 -> y#) <- forAll (Gen.word16 Range.constantBounded)
        x + y === fromWord16 (plusWord16# x# y#)
    ]
