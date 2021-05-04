module Main (main) where

import Data.List
import System.Environment

import Test.Tasty
import Test.Tasty.QuickCheck
import Data.RBTree

main :: IO ()
main = do
  -- setEnv "TASTY_QUICKCHECK_VERBOSE" "true"
  setEnv "TASTY_QUICKCHECK_MAX_SIZE" "100"
  defaultMain $ testGroup "tests" [insertTests, balancedTests]

insertTests :: TestTree
insertTests =
  testGroup
    "insert"
    [ testProperty "int" $ \list -> toList (fromList list) == sort (list :: [Int])
    , testProperty "double" $ \list -> toList (fromList list) == sort (list :: [Double])
    , testProperty "ansiistring" $ \list -> toList (fromList list) == sort (list :: [ASCIIString])
    ]

balancedTests :: TestTree
balancedTests =
  testGroup
    "balanced"
    [testProperty "balanced" $ \list -> balanced (fromList (list :: [Int]))]