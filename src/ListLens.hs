{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module ListLens where

import qualified Data.Map as M
import Order (Order, permute, OrderKey)
import Report (ReportElemID)

type ReportElems = Order ReportElemID ()

-- this indirection is needed
listReorder :: [ReportElemID] ->  ReportElems -> ReportElems
listReorder = listReorder'' 

-- Type signature is needed
listReorder'' :: forall k v. (Enum k, OrderKey k) =>
                 [k] -> Order k v -> ReportElems
listReorder'' ps order = reorder
    where
      -- This indirection is needed
      reorder = Order.permute ps order `seq` error "reorder"
