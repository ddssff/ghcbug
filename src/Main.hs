{-# LANGUAGE CPP, DeriveDataTypeable, FlexibleContexts, FlexibleInstances, FunctionalDependencies,
             ImpredicativeTypes, MultiParamTypeClasses, ScopedTypeVariables, TemplateHaskell, TypeFamilies,
             UndecidableInstances #-}
{-# OPTIONS_GHC -Wall #-}
module Main where

import Data.Maybe (fromJust)
import Data.UUID (fromString)
import qualified Data.Map as Map

import Appraisal.Report as Report (Report, ReportElem(ReportUndecided), reportUUID, reportBody, ReportElemID(ReportElemID), unReportElemID)
import Appraisal.ReportMap (ReportMap(ReportMap), ReportID(ReportID, unReportID))
import SiteMap (ListOp(ListOpReorder), WhichList(ElementList), ElemID(ReportElemID, unReportElemID))
import ListLens (listReorder)
import Appraisal.Utils.Builders as Builders (empty)
import qualified Language.Haskell.TH.Path.Order as Order

rid :: ReportID
rid = ReportID {unReportID = fromJust $ fromString "604b4920-7a6d-4ff2-9138-b9c0ba69290b"}

op :: ListOp
op = ListOpReorder rid  ElementList

bdy :: Order.Order ReportElemID ReportElem
bdy = Order.fromList [ Report.ReportUndecided ]

report :: Report
report =
    Builders.empty { reportUUID = unReportID rid
                   , reportBody = bdy
                   }

reportMap' :: ReportMap
reportMap' = ReportMap $ Map.fromList [(rid, report)]

main :: IO ()
main = print $ listReorder   (ElementList, [SiteMap.ReportElemID {SiteMap.unReportElemID = Report.ReportElemID {Report.unReportElemID = 1}}]) rid reportMap'
