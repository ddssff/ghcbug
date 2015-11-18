{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module ListLens  where

import Report
import Order

-- this indirection is needed
listReorder :: ReportElemID
listReorder = listReorder'' (ReportElemID 1) insert

-- Type signature is needed
listReorder'' :: forall k. (Enum k, OrderKey k) =>
                 k -> k -> ReportElemID
listReorder'' ps order = reorder
    where
      -- This indirection is needed
      reorder = Order.permute ps order `seq` ReportElemID 1
