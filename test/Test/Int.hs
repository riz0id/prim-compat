module Test.Int (testTree) where

import Test.Tasty (TestTree, testGroup)

--------------------------------------------------------------------------------

import qualified Test.Int.Int
import qualified Test.Int.Int16
import qualified Test.Int.Int32
import qualified Test.Int.Int8

--------------------------------------------------------------------------------

testTree :: TestTree
testTree =
  testGroup
    "Int"
    [ Test.Int.Int.testTree
    , Test.Int.Int8.testTree
    , Test.Int.Int16.testTree
    , Test.Int.Int32.testTree
    ]