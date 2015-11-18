{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
module Order where

class Eq k => OrderKey k where
    unused :: k

class OrderKey k => OrderMap k where
    permute :: k -> k -> Bool
    permute = (==)

instance OrderKey k => OrderMap k

instance (Eq a) => OrderKey a where
    unused = error "unused"
