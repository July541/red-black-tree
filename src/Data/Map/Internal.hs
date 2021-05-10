module Data.Map.Internal where

import Prelude hiding (lookup)

import Data.Maybe ( fromMaybe )

import qualified Data.RBTree.Internal as RB
import           Data.RBTree.Internal ( RBNode(Nil) )

data Pair k v = Pair {
  key :: k
, value :: v
}

instance (Eq k) => Eq (Pair k v) where
  (Pair k1 _) == (Pair k2 _) = k1 == k2

instance (Ord k) => Ord (Pair k v) where
  (Pair k1 v1) `compare` (Pair k2 v2) = k1 `compare` k2

newtype Map k v = Map (RBNode (Pair k v))

mkPair :: k -> v -> Pair k v
mkPair = Pair

byKey :: Ord k => k -> Pair k v -> Ordering
byKey k (Pair k' _) = k `compare` k'

updateValueBy :: (v -> v) -> Pair k v -> Pair k v
updateValueBy f p = p { value = f $ value p }

{--------------------------------------------------------------------
  Construction
--------------------------------------------------------------------}

-- | /O(1)/. The empty map.
empty :: Map k v
empty = Map Nil

-- | /O(1)/. A map with a single element.
singleton :: k -> v -> Map k v
singleton k v = Map $ RB.singleton $ mkPair k v

{--------------------------------------------------------------------
  Query
--------------------------------------------------------------------}

-- | /O(1)/. Is the map empty?
null :: Map k v -> Bool
null (Map node) = RB.null node

-- | /O(1)/. The number of elements in the map.
size :: Map k v -> Int
size (Map node) = RB.size node

-- | /O(log n)/. Is the key a member of the map?
member :: Ord k => k -> Map k v -> Bool
member k (Map node) = RB.memberBy (byKey k) node

-- | /O(log n)/. Is the key not a member of the map?
notMember :: Ord k => k -> Map k v -> Bool
notMember x = not . member x

-- | /O(log n)/. Lookup the value at a key in the map.
-- The function will return the corresponding value as @('Just' value)@,
-- or 'Nothing' of the key is not in the map.
lookup :: Ord k => k -> Map k v -> Maybe v
lookup k (Map node) = RB.lookupBy (byKey k) node
                        >>= Just . value

-- | /O(log n)/. Lookup the value at a key in the map, and return a default
-- value if the key is not in the map.
findWithDefault :: Ord k => v -> k -> Map k v -> v
findWithDefault v k m = fromMaybe v (lookup k m)

{--------------------------------------------------------------------
  Insertion
--------------------------------------------------------------------}

-- | /O(log n)/.
insert :: Ord k => k -> v -> Map k v -> Map k v
insert k v (Map node) = Map $ RB.insert (Pair k v) node

-- | Insert with a fucntion, combining new value and old value.
-- A new pair will be inserted if the key is not exist.
insertWith :: Ord k => (v -> v -> v) -> k -> v -> Map k v -> Map k v
insertWith f k v mp = snd $ insertLookupWithKey (const f) k v mp

insertWithKey :: Ord k => (k -> v -> v -> v) -> k -> v -> Map k v -> Map k v
insertWithKey f k v mp = snd $ insertLookupWithKey f k v mp

-- | Insert with a function, combining key, new value and old value.
-- A new pair will be inserted directly if the key is not exist.
insertLookupWithKey :: Ord k => (k -> v -> v -> v) -> k -> v -> Map k v -> (Maybe v, Map k v)
insertLookupWithKey f k v mp@(Map node)
  | k `member` mp = (oldValue, Map $ RB.updateBy (byKey k) updateF node)
  | otherwise     = (oldValue, insert k v mp)
  where
    updateF = \(Pair k v') -> Pair k (f k v v')
    oldValue = lookup k mp

delete :: Ord k => k -> Map k v -> Map k v
delete k (Map node) = Map $ RB.deleteBy (byKey k) node

adjust :: Ord k => (v -> v) -> k -> Map k v -> Map k v
adjust f k (Map node) = Map $ RB.updateBy (byKey k) (updateValueBy f) node

adjustWithKey :: Ord k => (k -> v -> v) -> k -> Map k v -> Map k v
adjustWithKey f k = adjust (f k) k
