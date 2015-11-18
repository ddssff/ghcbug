{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, FlexibleInstances, FunctionalDependencies,
             ImpredicativeTypes, MultiParamTypeClasses, ScopedTypeVariables, TemplateHaskell, TypeFamilies,
             UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Order
    ( OrderKey
    , OrderMap(Order, fromPairs, order, permute)
    , toPairs
    , fromList
    ) where

import Data.Data (Data)
import Data.List as List (partition, elem, foldl', filter)
import Data.Map as Map (Map, (!))
import qualified Data.Map as Map
import Data.Typeable (Typeable)
import Prelude hiding (init, succ)
import qualified Prelude (succ)

class Ord k => OrderKey k where
    init :: k      -- ^ The initial key value (toEnum 0?)
    succ :: k -> k -- ^ The successor function for k

-- | The OrderMap instance is inferred from the OrderKey instance.
-- An OrderMap is a map from k to v (returned by 'elems') and an
-- ordering of keys [k] (returned by 'order'.)  This is a better data
-- structure to use in a collaborative application because the keys
-- are a stable way to address the element, not dependant on the
-- changing order of the list.  Even better would be to divorce the
-- keys and the ordering.
class OrderKey k => OrderMap k where
    data Order k :: * -> *
    -- | The next available key
    next :: Order k v -> k
    next = next'
    -- | Return the an empty Order.
    empty :: Order k v     -- ^ The empty Order
    empty = Order {elems' = mempty, order' = mempty, next' = init}
    -- | Return the key-value map
    elems :: Order k v -> Map k v
    elems = elems'
    -- | Return the list of keys in order.
    order :: Order k v -> [k]
    order = order'
    -- | Update the value of an existing item
    putItem :: k -> v -> Order k v -> Order k v
    putItem k a m =
        m {elems' = Map.alter f k (elems m)}
            where f Nothing = (error "putItem: bad key")
                  f (Just _) = Just a
    -- | Partition an Order into the element at k and the Order
    -- containing the remaining elements.
    view :: k -> Order k v -> Maybe (v, Order k v) -- like Data.Set.minView
    view k m = case Map.lookup k (elems m) of
                 Nothing -> Nothing
                 Just x -> Just (x, m {elems' = Map.delete k (elems m), order' = filter (/= k) (order m)})
    -- | Build an order from a list of (key, value) pairs.  No
    -- uniqueness check of the keys is performed.
    fromPairs :: [(k, v)] -> Order k v
    fromPairs prs =
      let ks = map fst prs in
      Order { elems' = Map.fromList prs
            {-, deleted = mempty-}
            , order' = ks
            , next' = succ (foldl1 max ks) }
    -- | Put a new element at the end of the Order, returning a pair
    -- containing the new Order and the new key.
    insert :: v -> Order k v -> (Order k v, k)
    insert a m = let k = next m in (m {next' = succ k, elems' = Map.insert k a (elems m), order' = order m ++ [k]}, k)
    -- | Replace the current ordering with the given key list.  The
    -- result is a triple: (new, missing, invalid).  Missing pairs are
    -- those not mentioned in the new list, invalid are those
    -- mentioned but not present in the old Order.
    permute :: [k] -> Order k v -> (Order k v, [(k, v)], [k])
    permute neworder m =
      reorder $ collect $ sanitize
      where
        -- Make sure the new key order doesn't have any unknown keys
        -- sanitize :: ([k], [k]) -- (known, invalid)
        sanitize = List.partition (`List.elem` (order' m)) neworder
        -- Collect the values that are missing from the new key order
        -- collect :: ([k], [k]) -> ([k], [k], [k]) -- (present, missing, invalid)
        collect (valid, invalid) =
            let deleted = List.filter (not . (`List.elem` invalid)) (order' m) in (valid, deleted, invalid)
        -- Reorder the OrderMap according to the now safe permutation,
        -- also return the portion of the OrderMap not mentioned in the
        -- new order and the list of invalid keys.
        -- reorder :: ([k], [k], [k]) -> (OrderMap k a, OrderMap k a, [k])
        reorder (valid, _missing, invalid) =
            let (validmap, missingmap) = Map.partitionWithKey (\ k _ -> List.elem k valid) (elems m) in
            (m {elems' = validmap, order' = valid},
             (Map.toList missingmap),
             invalid)

-- | Given any instance of OrderKey, we can create an instance of
-- OrderMap which includes an Order type containing a Map, a list
-- of keys, and a field to hold the next available key.
instance OrderKey k => OrderMap k where
    data Order k v = Order { elems' :: Map k v
                           -- , deleted :: Map k a
                           , order' :: [k]
                           , next' :: k
                           } deriving (Data, Typeable, Show)

instance OrderMap k => Monoid (Order k m) where
    mempty = empty
    mappend a b = foldr (\ x m -> fst (insert x m)) a (toList b)

-- Not sure how correct these three instances are in the presence of
-- randomly allocated keys and the like.
instance (OrderMap k, Eq a) => Eq (Order k a) where
    a == b = toList a == toList b

instance (OrderMap k, Eq a, Ord a) => Ord (Order k a) where
    compare a b = compare (toList a) (toList b)

instance (OrderMap k, Read a) => Read (Order k a) where
    -- readsPrec :: Int -> String -> [(OrderMap k a, String)]
    readsPrec _ s = let l = (read s :: [a]) in [(fromList l, "")]

instance (Ord a, Enum a) => OrderKey a where
    init = toEnum 1            -- Yeah, that's right, 1.  F**k zeroth elements.
    succ = Prelude.succ

-- | Put a new element at the end of the order, allocating a new key
-- for it.
appendItem :: OrderMap k => v -> Order k v -> Order k v
appendItem x = fst . insert x

-- | Return the keys and values of the order.
toPairs :: OrderMap k => Order k v -> [(k, v)]
toPairs m = map (\ k -> (k, (elems m) ! k)) (order m)

-- | Return only the values of the order, discarding the keys.
toList :: OrderMap k => Order k v -> [v]
toList = map snd . toPairs

-- | Build an order from a list of values, allocating new all keys.
fromList :: OrderMap k => [v] -> Order k v
fromList xs = foldl' (flip appendItem) empty xs
