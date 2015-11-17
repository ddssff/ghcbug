{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS -Wall -fno-warn-orphans #-}
module Appraisal.ReportMap
    ( ReportID(ReportID, unReportID)
    , ReportMap(ReportMap, unReportMap)
    ) where

import Appraisal.Report (Report)
import Data.UUID.Types.Internal (UUID(..))
import Data.Data (Data, Typeable)
import qualified Data.Map as M (Map)

newtype ReportID = ReportID { unReportID :: UUID } deriving (Eq, Ord, Read, Show, Typeable, Data)
newtype ReportMap = ReportMap { unReportMap :: M.Map ReportID Report } deriving (Eq, Ord, Read, Show, Typeable, Data)
