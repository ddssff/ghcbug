{-# LANGUAGE FlexibleInstances, TypeFamilies, UndecidableInstances #-}
module Order
    ( OrderKey
    , OrderMap(Order, permute, insert)
    ) where

import Prelude hiding (init, succ)

class Ord k => OrderKey k where
    init :: k      -- ^ The initial key value (toEnum 0?)

class OrderKey k => OrderMap k where
    data Order k :: * -> *

    insert :: Order k v
    insert = Order init

    permute :: k -> Order k v -> ()
    permute neworder (Order k) = sanitize `seq` error "done"
      where
        sanitize = neworder == k

-- | Given any instance of OrderKey, we can create an instance of
-- OrderMap which includes an Order type containing a Map, a list
-- of keys, and a field to hold the next available key.
instance OrderKey k => OrderMap k where
    data Order k v = Order  k

instance (Ord a, Enum a) => OrderKey a where
    init = toEnum 1            -- Yeah, that's right, 1.  F**k zeroth elements.
