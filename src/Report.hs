{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses,
             OverloadedStrings, RecordWildCards, StandaloneDeriving, TypeSynonymInstances, TemplateHaskell, TypeFamilies #-}
{-# OPTIONS -fcontext-stack=100 -fno-warn-orphans -fno-warn-missing-signatures -fno-warn-name-shadowing -fwarn-incomplete-patterns #-}
-- Removing this -O0 fixes the bug
{-# OPTIONS_GHC -O0 -Wall -fno-warn-orphans #-}
module Report
    ( IntJS,
      ToIntJS(intJS),
      ReportElemID(..),
      ReportElems,
      Report(Report, reportUUID, reportBody),
      ReportElem(ReportUndecided),
      ReportID(ReportID, unReportID),
      ReportMap(ReportMap, unReportMap)
    ) where

import Data.Data (Data, Typeable)
import Data.Int (Int32)
import qualified Data.Map as M (Map)
import Data.UUID.Types (UUID)
import Data.UUID.Orphans ()
import Prelude hiding (read)
import Data.Aeson (ToJSON(toJSON), FromJSON(parseJSON))
import Order (Order)

type IntJS = Int32

class ToIntJS k where
    intJS :: k -> IntJS

data Author
    = Author { _authorName :: String, _authorCredentials :: String }
    deriving (Show, Eq, Ord, Typeable, Data)

data ReportElem
    = ReportUndecided
    deriving (Show, Eq, Ord, Typeable, Data)

data ReportIntendedUse
    = SalesAdvisory
    | EstatePlanning
    | EstateTax
    | InsuranceCoverage
    | InsuranceClaim
    | CharitableDonation
    | EquitableDistribution
    | MaritalDissolution
    | EstateDivision
    deriving (Read, Show, Eq, Ord, Bounded, Enum, Typeable, Data)

data ReportValueTypeInfo
    = ReportValueTypeInfo
      { _reportValueTypeName :: String
      , _reportValueTypeDescription :: String
      , _reportValueTypeDefinition :: String
      } deriving (Read, Show, Eq, Ord, Typeable, Data)

data ReportValueApproachInfo
    = ReportValueApproachInfo
    { _reportValueApproachName :: String
    , _reportValueApproachDescription :: String
    } deriving (Read, Show, Eq, Ord, Typeable, Data)

data ReportStatus
    = Draft
    -- ^ This is the current authoritative version of the report
    | Final
    -- ^ The report has been downloaded, and perhaps uploaded to a
    -- different server.
    deriving (Read, Show, Eq, Ord, Typeable, Data)

-- Does ReportStatus belong in ReportFlags?  Seems more like meta info.
data ReportFlags = ReportFlags {
  _hideEmptyItemFields :: Bool
  } deriving (Read, Show, Eq, Ord, Typeable, Data)

newtype ReportElemID
      = ReportElemID {unReportElemID :: IntJS}
      deriving (Eq, Ord, Read, Show, Data, Typeable)
type ReportElems = Order ReportElemID ReportElem
instance Enum ReportElemID where
      toEnum = (ReportElemID . toEnum)
      fromEnum = (fromEnum . unReportElemID)
instance ToIntJS ReportElemID where
      intJS = unReportElemID
instance ToJSON ReportElemID where
      toJSON = (toJSON . unReportElemID)
instance FromJSON ReportElemID where
      parseJSON = ((fmap ReportElemID) . parseJSON)

data Report
    = Report { reportBody :: ReportElems
             , reportUUID :: UUID
             }
    deriving (Show, Eq, Ord, Typeable, Data)

newtype ReportID = ReportID { unReportID :: UUID } deriving (Eq, Ord, Read, Show, Typeable, Data)
newtype ReportMap = ReportMap { unReportMap :: M.Map ReportID Report } deriving (Eq, Ord, Show, Typeable, Data)
