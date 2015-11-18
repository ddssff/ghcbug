{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, FlexibleInstances, FunctionalDependencies,
             ImpredicativeTypes, MultiParamTypeClasses, ScopedTypeVariables, TemplateHaskell, TypeFamilies,
             UndecidableInstances #-}
{-# OPTIONS_GHC -Wall #-}
module Main
    ( main
    ) where

import Report (Report(Report), ReportElem(ReportUndecided), reportUUID, reportBody, ReportElemID(ReportElemID),
               ReportMap(ReportMap), ReportID(ReportID, unReportID))
import qualified Data.Map as Map
import Order hiding (order)
import ListLens (listReorder, WhichList(ElementList), ElemID(ElemID))

-- Main creates a value of type ReportMap with one element, and then
-- tries to reorder a list inside that element using listReorder.
main :: IO ()
main = print $ listReorder   (ElementList, order) rid reportMap'
    where
      order :: [ElemID]
      order = [ElemID (ReportElemID 1)]

      rid :: ReportID
      rid = ReportID "604b4920-7a6d-4ff2-9138-b9c0ba69290b"

      report :: Report
      report =
          Report { reportUUID = unReportID rid
                 , reportBody = Order.fromList [ Report.ReportUndecided ] }

      reportMap' :: ReportMap
      reportMap' = ReportMap $ Map.fromList [(rid, report)]
