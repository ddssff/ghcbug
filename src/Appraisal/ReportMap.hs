{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS -Wall -fno-warn-orphans #-}
module Appraisal.ReportMap
    ( ReportID (..)
    , ReportMap (..)
    ) where

import Appraisal.Report (Report)
import Data.UUID.Types.Internal (UUID(..))
import Control.Lens (makeLensesFor)
import Data.Data (Data, Typeable)
import qualified Data.Map as M (Map)
import Data.SafeCopy (base, deriveSafeCopy)
import Language.Haskell.TH.Path.Graph (SelfPath)
-- import Language.Haskell.TH.Path.Lens (nameMakeLens)

newtype ReportID = ReportID { unReportID :: UUID } deriving (Eq, Ord, Read, Show, Typeable, Data)
newtype ReportMap = ReportMap { unReportMap :: M.Map ReportID Report } deriving (Eq, Ord, Read, Show, Typeable, Data)

instance SelfPath ReportID

$(makeLensesFor [("unReportMap", "lens_ReportMap_ReportMap")] ''ReportMap)

$(deriveSafeCopy 1 'base ''ReportID)
$(deriveSafeCopy 2 'base ''ReportMap)
