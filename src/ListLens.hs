module ListLens  where

import Report
import Order

-- this indirection is needed
listReorder :: ReportElemID
listReorder = listReorder'' (ReportElemID 1) (ReportElemID 1)

-- Type signature is needed
listReorder'' :: (Enum k, OrderKey k) =>
                 k -> k -> ReportElemID
listReorder'' ps order = reorder
    where
      -- This indirection is needed
      reorder = Order.permute ps order `seq` ReportElemID 1
