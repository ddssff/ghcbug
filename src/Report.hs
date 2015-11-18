{-# OPTIONS_GHC -O0 -Wall -fno-warn-orphans #-}
module Report
    ( module Report0
    ) where

import Report0

instance Eq ReportElemID where (==) _ _ = error "unused"
instance Enum ReportElemID where
      toEnum = error "unused"
      fromEnum = error "unused"
