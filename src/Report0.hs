-- Removing this -O0 fixes the bug
module Report0
    ( ReportElemID(..),
    ) where

newtype ReportElemID = ReportElemID {unReportElemID :: Integer}
