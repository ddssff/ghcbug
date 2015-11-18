{-# LANGUAGE FlexibleInstances, TypeFamilies, UndecidableInstances #-}
module Order
    ( OrderKey
    , OrderMap(Order, order, permute, insert, empty)
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

    -- | Put a new element at the end of the Order, returning a pair
    -- containing the new Order and the new key.
    insert :: v -> Order k v -> (Order k v, k)
    insert a m = let k = next m in (m {next' = succ k, elems' = Map.insert k a (elems m), order' = order m ++ [k]}, k)
    -- | Replace the current ordering with the given key list.  The
    -- result is a triple: (new, missing, invalid).  Missing pairs are
    -- those not mentioned in the new list, invalid are those
    -- mentioned but not present in the old Order.
    permute :: [k] -> Order k v -> (Order k v, [(k, v)], [k])
    permute neworder m = sanitize `seq` error "done"
      where
        sanitize = List.partition (`List.elem` (order' m)) neworder

-- | Given any instance of OrderKey, we can create an instance of
-- OrderMap which includes an Order type containing a Map, a list
-- of keys, and a field to hold the next available key.
instance OrderKey k => OrderMap k where
    data Order k v = Order { elems' :: Map k v
                           -- , deleted :: Map k a
                           , order' :: [k]
                           , next' :: k
                           }

instance (Ord a, Enum a) => OrderKey a where
    init = toEnum 1            -- Yeah, that's right, 1.  F**k zeroth elements.
    succ = Prelude.succ
