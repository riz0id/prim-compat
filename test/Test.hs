module Main (main) where

import Test.Tasty (TestTree, defaultMain, localOption, testGroup)
import Test.Tasty.Hedgehog (HedgehogTestLimit (HedgehogTestLimit))

--------------------------------------------------------------------------------

import qualified Test.Int
import qualified Test.Word

--------------------------------------------------------------------------------

main :: IO ()
main = defaultMain (localOption (HedgehogTestLimit (Just 100000)) testTree)

testTree :: TestTree
testTree =
  testGroup
    "Test"
    [ Test.Int.testTree
    , Test.Word.testTree
    ]