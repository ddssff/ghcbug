{-# LANGUAGE FlexibleInstances, TypeFamilies, UndecidableInstances #-}
module Order where

import Prelude hiding (init, succ)

class Eq k => OrderKey k where
    init :: k

class OrderKey k => OrderMap k where
    insert :: k
    insert = init

    permute :: k -> k -> ()
    permute k' k = k' == k `seq` error "done"

instance OrderKey k => OrderMap k

instance (Eq a, Enum a) => OrderKey a where
    init = toEnum 1
