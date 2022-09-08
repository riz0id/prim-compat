module Test.Word.Word8 (testTree) where

import Hedgehog (forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Test.Core (TestTree, testGroup, testProp)

import Data.Word.Prim.Compat (fromWord8#, toWord8#)

import GHC.Exts (plusWord8#)

--------------------------------------------------------------------------------

testTree :: TestTree
testTree =
  testGroup
    "Word8"
    [ testProp "Unbox" $ property do
        x@(toWord8# -> x#) <- forAll (Gen.word8 Range.constantBounded)
        y@(toWord8# -> y#) <- forAll (Gen.word8 Range.constantBounded)
        x + y === fromWord8# (plusWord8# x# y#)
    ]
