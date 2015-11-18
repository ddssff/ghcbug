{-# LANGUAGE FlexibleInstances, TypeFamilies, UndecidableInstances #-}
module Order
    ( OrderKey
    , OrderMap(Order, permute, insert)
    ) where

import Prelude hiding (init, succ)

class Ord k => OrderKey k where
    init :: k      -- ^ The initial key value (toEnum 0?)

class OrderKey k => OrderMap k where
    data Order k :: *

    insert :: Order k
    insert = Order init

    permute :: k -> Order k -> ()
    permute k' (Order k) = k' == k `seq` error "done"

instance OrderKey k => OrderMap k where
    data Order k = Order k

instance (Ord a, Enum a) => OrderKey a where
    init = toEnum 1            -- Yeah, that's right, 1.  F**k zeroth elements.
