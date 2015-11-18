-- {-# OPTIONS_GHC -O0 -Wall -fno-warn-orphans #-}
module Report
    ( module Report0
    ) where

import Report0

instance Eq ReportElemID where (==) _ _ = True
instance Enum ReportElemID where
      toEnum = ReportElemID . toEnum
      fromEnum = fromEnum . unReportElemID
