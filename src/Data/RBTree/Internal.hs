{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wincomplete-patterns #-}

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

instance (Show a) => Show (RBNode a) where
  show Nil = "Nil"
  show RBNode{..} = " (" <> show left <> " " <> show value <> " " <> show color <> " " <> show right <> " " <> show sz <> ") "

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
member target node = compare target `memberBy` node
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

balanced :: (Ord a) => RBNode a -> Bool
balanced Nil = True
balanced node@RBNode{..} = colorCheck &&
                           blackCountCheck &&
                           heightCheck &&
                           balanced left &&
                           balanced right
  where
    blackCounts Nil = [1]
    blackCounts RBNode {..} =
      let l = blackCounts left
          r = blackCounts right
       in if color == Red
            then l ++ r
            else map (+ 1) $ l ++ r

    allEqual [] = True
    allEqual (x:xs) = all (==x) xs

    blackCountCheck = allEqual $ blackCounts node

    colorCheck = let isBlack Nil = True
                     isBlack RBNode{..} = color == Black
                  in color /= Red || (isBlack left && isBlack right)

    height Nil = 0
    height RBNode{..} = max (height left) (height right) + 1

    -- reference: https://en.wikipedia.org/wiki/Red%E2%80%93black_tree#Proof_of_bounds
    heightCheck = fromIntegral (height node) <= 2 * logBase 2 (fromIntegral (sz + 1))

toList :: RBNode a -> [a]
toList Nil = []
toList node = toList (left node) ++ [value node] ++ toList (right node)

fromList :: Ord a => [a] -> RBNode a
fromList = foldr insert Nil

mkBlack :: RBNode a -> RBNode a
mkBlack Nil = Nil
mkBlack node = node{ color = Black }

mkRed :: RBNode a -> RBNode a
mkRed Nil = Nil
mkRed node = node{ color = Red }

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
                             (RBNode r2 r1 Black v1 (1 + size r1 + size r2))
                             Red v2 sz1
               RBNode (RBNode l2 (RBNode l3 r3 Red v3 sz3) Red v2 sz2) r1 Black v1 sz1
                   -> RBNode (RBNode l2 l3 Black v2 (1 + size l2 + size l3))
                             (RBNode r3 r1 Black v1 (1 + size r3 + size r1))
                             Red v3 sz1
               RBNode l1 (RBNode (RBNode l3 r3 Red v3 sz3) r2 Red v2 sz2) Black v1 sz1
                   -> RBNode (RBNode l1 l3 Black v1 (1 + size l1 + size l3))
                             (RBNode r3 r2 Black v2 (1 + size r3 + size r2))
                             Red v3 sz1
               RBNode l1 (RBNode l2 (RBNode l3 r3 Red v3 sz3) Red v2 sz2) Black v1 sz1
                   -> RBNode (RBNode l1 l2 Black v1 (1 + size l1 + size l2))
                             (RBNode l3 r3 Black v3 sz3)
                             Red v2 sz1
               node -> node

delete :: Ord a => a -> RBNode a -> RBNode a
delete x = mkBlack . delete' x
  where
    delete' _ Nil = Nil
    delete' target node@RBNode{..} =
      case target `compare` value of
        LT -> deleteL target node
        GT -> deleteR target node
        EQ -> fuse left right

    deleteL :: Ord a => a -> RBNode a -> RBNode a
    deleteL target node
      | color node == Red   = node{ left = delete' target (left node) }
      | color node == Black = balanceL $ node{ left = delete' target (left node) }
      | otherwise           = error "Unexpected1"

    deleteR :: Ord a => a -> RBNode a -> RBNode a
    deleteR target node
      | color node == Red   = node{ right = delete' target (right node) }
      | color node == Black = balanceR $ node{ right = delete' target (right node) }
      | otherwise           = error "Unexpected2"

    -- deletion occurred from the left subtree, left subtree is shorter than the right,
    -- rebalance required.
    balanceL :: RBNode a -> RBNode a
    balanceL = \case -- Case 1: root is black and left subtree root is red
                     RBNode (RBNode l2 r2 Red v2 sz2) r1 Black v1 sz1
                          -> RBNode (RBNode l2 r2 Black v2 sz2) r1 Red v1 sz1
                     -- Case 2: root is black and left subtree root is black too
                     -- Case 2.1: right subtree root is black too
                     RBNode l1 (RBNode l2 r2 Black v2 sz2) Black v1 sz1
                          -> rotate $ RBNode l1 (RBNode l2 r2 Red v2 sz2) Black v1 sz1
                     -- case 2.2: right subtree root is red.
                     RBNode l1 (RBNode l2@(RBNode _ _ Black _ _) r2@(RBNode _ _ Black _ _) Red v2 sz2) Black v1 sz1
                          -> RBNode (RBNode l1 (left l2) Black v1 (1 + size l1 + size (left l2)))
                                 (rotate (RBNode (right l2) (mkRed r2) Black v2 (sz2 - size (left l2))))
                                 Red (value l2) sz1
                     node -> node

    -- vice versa
    balanceR :: RBNode a -> RBNode a
    balanceR = \case RBNode l1 (RBNode l2 r2 Red v2 sz2) Black v1 sz1
                          -> RBNode l1 (RBNode l2 r2 Black v2 sz2) Red v1 sz1
                     RBNode (RBNode l2 r2 Black v2 sz2) r1 Black v1 sz1
                          -> rotate $ RBNode (RBNode l2 r2 Red v2 sz2) r1 Black v1 sz1
                     RBNode (RBNode l2@(RBNode _ _ Black _ _) r2@(RBNode _ _ Black _ _) Red v2 sz2) r1 Black v1 sz1
                          -> RBNode (rotate (RBNode (mkRed l2) (left r2) Black v2 (sz2 - size (right r2))))
                                 (RBNode (right r2) r1 Black v1 (1 + size r1 + size (right r2)))
                                 Red (value r2) sz1
                     node -> node

    fuse :: RBNode a -> RBNode a -> RBNode a
    fuse Nil node = node
    fuse node Nil = node
    fuse node@(RBNode _ _ Black _ sz1) (RBNode l2 r2 Red v2 sz2) =
      RBNode (fuse node l2) r2 Red v2 (sz1 + sz2)
    fuse (RBNode l1 r1 Red v1 sz1) node@(RBNode _ _ Black _ sz2) =
      RBNode l1 (fuse r1 node) Red v1 (sz1 + sz2)
    fuse (RBNode l1 r1 Red v1 sz1) node@(RBNode l2 r2 Red v2 sz2) =
      let s = fuse r1 l2
      in case s of
           (RBNode l r Red v sz)   -> RBNode (RBNode l1 l Red v1 (1 + size l1 + size l))
                                           (RBNode r r2 Red v2 (1 + size r + size r2))
                                           Red v (sz1 + sz2)
           (RBNode _ _ Black _ sz) -> RBNode l1 (RBNode s r2 Red v2 (1 + sz + size r2)) Red v1 (sz1 + sz2)
           Nil                     -> RBNode l1 node Black v1 (sz1 + sz2)
    fuse (RBNode l1 r1 Black v1 sz1) node@(RBNode l2 r2 Black v2 sz2) =
      let s = fuse r1 l2
      in case s of
           (RBNode l r Red v sz)   -> RBNode (RBNode l1 l Black v1 (1 + size l1 + size l))
                                             (RBNode r r2 Black v2 (1 + size r + size r2))
                                             Red v (sz1 + sz2)
           (RBNode _ _ Black _ sz) -> balanceL $ RBNode l1
                                                        (RBNode s r2 Black v2 (1 + sz + size r2))
                                                        Black v1 (sz1 + sz2)
           Nil                     -> RBNode l1 node Black v1 (sz1 + sz2)

memberBy :: Ord a => (a -> Ordering) -> RBNode a -> Bool
memberBy _ Nil = False
memberBy cmp node =
  case cmp $ value node of
    EQ -> True
    LT -> cmp `memberBy` left node
    GT -> cmp `memberBy` right node
