{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import           Data.List             (sort, nub)
import qualified Data.List             as L (delete)
import           Data.RBTree           as RBT (balanced, delete, fromList,
                                               singleton, size, toList)
import           System.Environment    (setEnv)
import           Test.Tasty            (TestTree, defaultMain, testGroup)
import           Test.Tasty.QuickCheck (ASCIIString, label, testProperty, (==>))

main :: IO ()
main = do
  -- setEnv "TASTY_QUICKCHECK_VERBOSE" "true"
  setEnv "TASTY_QUICKCHECK_MAX_SIZE" "1000"
  defaultMain $ testGroup "tests"
    [ insertTests
    , balancedTests
    , sizeTests
    , deleteTests
    -- , hugeTests
    ]

insertTests :: TestTree
insertTests =
  testGroup
    "insert"
    [ testProperty "int" $ \list -> toList (fromList list) == nubSort (list :: [Int])
    , testProperty "double" $ \list -> toList (fromList list) == nubSort (list :: [Double])
    , testProperty "ansiistring" $ \list -> toList (fromList list) == nubSort (list :: [ASCIIString])
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
    [ testProperty "size" $ \list -> size (fromList (list :: [Int])) == length (nub list)
    , testProperty "duplicate size" $ \list -> nub list /= list ==>
        size (fromList (list :: [Int])) == length (nub list)
    ]

deleteTests :: TestTree
deleteTests =
  testGroup
    "delete"
    [ testProperty "basic delete" $ \(list :: [Int]) (x :: Int) ->
        toList (RBT.delete x (fromList list)) == nubSort (deleteAll x list)
    , testProperty "delete occured" $ \(list :: [Int]) (x :: Int) ->
        x `elem` list ==>
        toList (RBT.delete x (fromList list)) == nubSort (deleteAll x list)
    , testProperty "delete not occured" $ \(list :: [Int]) (x :: Int) ->
        x `notElem` list ==>
        toList (RBT.delete x (fromList list)) == nubSort (deleteAll x list)
    ]

hugeTests :: TestTree
hugeTests =
  testGroup
    "huge tests"
  [ testProperty "huge list" $ \(list :: [Int]) -> length list > 1 ==> label ("test length: " <> show (length list)) $
      let l1 = [ x * y | x <- list, y <- tail list]
          l2 = drop (length l1 `div` 10) l1
          -- l2 = l1
       in toList (fromList l2) == nubSort l2
  ]

nubSort :: Ord a => [a] -> [a]
nubSort = nub . sort

deleteAll :: Eq a => a -> [a] -> [a]
deleteAll v l = foldr L.delete l (replicate (length l) v)
