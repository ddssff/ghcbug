{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module ListLens where

import Report (ReportElemID)
import Order

-- this indirection is needed
listReorder :: ReportElemID ->  ReportElemID -> ReportElemID
listReorder = listReorder''

-- Type signature is needed
listReorder'' :: forall k. (Enum k, OrderKey k) =>
                 k -> k -> ReportElemID
listReorder'' ps order = reorder
    where
      -- This indirection is needed
      reorder = Order.permute ps order `seq` error "reorder"
