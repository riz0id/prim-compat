module Test.Word (testTree) where

import Test.Tasty (TestTree, testGroup)

--------------------------------------------------------------------------------

import qualified Test.Word.Word
import qualified Test.Word.Word16
import qualified Test.Word.Word32
import qualified Test.Word.Word8

--------------------------------------------------------------------------------

testTree :: TestTree
testTree =
  testGroup
    "Word"
    [ Test.Word.Word.testTree
    , Test.Word.Word8.testTree
    , Test.Word.Word16.testTree
    , Test.Word.Word32.testTree
    ]