{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Data.RBTree.Internal where

import Prelude hiding (null)

data Color = Red | Black deriving (Show, Eq)

type Size = Int

data RBNode a = RBNode {
  left :: RBNode a
, right :: RBNode a
, color :: Color
, value :: a
, sz :: Size
} | Nil

isNil :: RBNode a -> Bool
isNil = null

-- | /O(1)/. Is this the empty tree?
null :: RBNode a -> Bool
null Nil = True
null RBNode{} = False

-- | /O(1)/. The number of elements in the tree.
size :: RBNode a -> Int
size = \case Nil -> 0
             node -> sz node

member :: Ord a => a -> RBNode a -> Bool
member _ Nil = False
member target node =
  case target `compare` value node of
    EQ -> True
    LT -> target `member` left node
    GT -> target `member` right node
-- TODO: inlinable

notMember :: Ord a => a -> RBNode a -> Bool
notMember target = not . member target

-- | /O(log n)/. Find largest element smaller than the given one.
-- reference: https://github.com/haskell/containers/blob/master/containers/src/Data/Set/Internal.hs
lookupLT :: Ord a => a -> RBNode a -> Maybe a
lookupLT = goNothing
  where
    goNothing _ Nil = Nothing
    goNothing target node =
      case target `compare` value node of
        LT -> goJust (value node) $ right node
        _  -> goNothing target $ left node

    goJust best Nil = Just best
    goJust target node =
      case target `compare` value node of
        LT -> goJust (value node) $ right node
        _  -> goJust target $ left node

-- TODO: looupGT, lookupLE, lookupGE

empty :: RBNode a
empty = Nil

singleton :: a -> RBNode a
singleton val = RBNode {
  left = Nil
, right = Nil
, value = val
, color = Black
, sz = 1
}

-- balanced :: (Ord a) => RBNode a -> Bool
-- balanced Nil = True
-- balanced node = colorCheck node && allEqual (blackCounts node) &&
--                 balanced (left node) && balanced (right node)
--   where
--     allEqual [] = True
--     allEqual (x:xs) = all (==x) xs

-- reference: https://en.wikipedia.org/wiki/Red%E2%80%93black_tree#Proof_of_bounds
balanced :: RBNode a -> Bool
balanced x = fromIntegral (height x) <= 2 * logBase 2 (fromIntegral (size x + 1))

height :: RBNode a -> Int
height Nil = 0
height RBNode{..} = h
  where
    l = height left
    r = height right
    h = max l r + 1

colorCheck :: RBNode a -> Bool
colorCheck Nil = True
colorCheck RBNode{..}
  | color == Red = isBlack left && isBlack right
  | color == Black = True
  where
    isBlack Nil = True
    isBlack node = color == Black

blackCounts :: RBNode a -> [Int]
blackCounts Nil = [1]
blackCounts RBNode{..}
  | color == Red = l ++ r
  | color == Black = map (+1) $ l ++ r
  where
    l = blackCounts left
    r = blackCounts right

toList :: RBNode a -> [a]
toList Nil = []
toList node = toList (left node) ++ [value node] ++ toList (right node)

fromList :: Ord a => [a] -> RBNode a
fromList = foldr insert Nil

insert :: Ord a => a -> RBNode a -> RBNode a
insert x = mkBlack . insert' x
  where
    insert' target Nil = RBNode Nil Nil Red target 1
    insert' target node =
      case target `compare` value node of
        GT -> rotate node{ right = insert' target (right node), sz = sz node + 1 }
        _  -> rotate node{ left = insert' target (left node), sz = sz node + 1 }

    rotate :: RBNode a -> RBNode a
    rotate = \case Nil -> Nil
                   RBNode (RBNode (RBNode l3 r3 Red v3 sz3) r2 Red v2 sz2) r1 Black v1 sz1
                     -> RBNode (RBNode l3 r3 Black v3 sz3)
                               (RBNode r2 r1 Black v1 (1 + sz r1 + sz r2))
                               Red v2 sz1
                   RBNode (RBNode l2 (RBNode l3 r3 Red v3 sz3) Red v2 sz2) r1 Black v1 sz1
                     -> RBNode (RBNode l2 l3 Black v2 (1 + sz l2 + sz l3))
                               (RBNode r3 r1 Black v1 (1 + sz r3 + sz r1))
                               Red v3 sz1
                   RBNode l1 (RBNode (RBNode l3 r3 Red v3 sz3) r2 Red v2 sz2) Black v1 sz1
                     -> RBNode (RBNode l1 l3 Black v1 (1 + sz l1 + sz l3))
                               (RBNode r3 r2 Black v2 (1 + sz r3 + sz r2))
                               Red v3 sz1
                   RBNode l1 (RBNode l2 (RBNode l3 r3 Red v3 sz3) Red v2 sz2) Black v1 sz1
                     -> RBNode (RBNode l1 l2 Black v1 (1 + sz l1 + sz l2))
                               (RBNode l3 r3 Black v3 sz3)
                               Red v2 sz1
                   node -> node

    mkBlack Nil = Nil
    mkBlack node = node{ color = Black }