{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Data.List ( sort )
import qualified Data.List as L ( delete )
import System.Environment

import Test.Tasty
import Test.Tasty.QuickCheck
import Data.RBTree as RBT
    ( size, singleton, balanced, toList, fromList, delete )

main :: IO ()
main = do
  -- setEnv "TASTY_QUICKCHECK_VERBOSE" "true"
  setEnv "TASTY_QUICKCHECK_MAX_SIZE" "1000"
  defaultMain $ testGroup "tests" [insertTests, balancedTests, sizeTests, deleteTests, hugeTests]

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
    [ testProperty "balanced" $ \list -> balanced (fromList (list :: [Int]))
    , testProperty "singleton" $ \(x :: [Int]) -> toList (singleton x) == [x]
    ]

sizeTests :: TestTree
sizeTests =
  testGroup
    "size"
    [ testProperty "size" $ \list -> size (fromList (list :: [Int])) == length list ]

deleteTests :: TestTree
deleteTests =
  testGroup
    "delete"
    [ testProperty "basic delete" $ \(list :: [Int]) (x :: Int) ->
        toList (RBT.delete x (fromList list)) == sort (L.delete x list)
    , testProperty "delete occured" $ \(list :: [Int]) (x :: Int) ->
        x `elem` list ==>
        toList (RBT.delete x (fromList list)) == sort (L.delete x list)
    , testProperty "delete not occured" $ \(list :: [Int]) (x :: Int) ->
        x `notElem` list ==>
        toList (RBT.delete x (fromList list)) == sort (L.delete x list)
    ]

hugeTests :: TestTree
hugeTests =
  testGroup
    "huge tests"
  [ testProperty "huge list" $ \(list :: [Int]) -> length list > 1 ==> label ("test length: " <> show (length list)) $
      let l1 = [ x * y | x <- list, y <- tail list]
          l2 = drop (9 * length l1 `div` 10) l1
          -- l2 = l1
       in toList (fromList l2) == sort l2
  ]