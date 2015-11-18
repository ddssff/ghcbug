module ListLens  where

import Report
import Order

-- this indirection is needed
listReorder :: Bool
listReorder = listReorder'' (ReportElemID 1) (ReportElemID 1)

-- Type signature is needed
listReorder'' :: OrderKey k => k -> k -> Bool
listReorder'' k1 k2 = reorder
    where
      -- This indirection is needed
      reorder = Order.permute k1 k2
