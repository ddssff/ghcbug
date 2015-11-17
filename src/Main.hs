{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, FlexibleInstances, FunctionalDependencies,
             ImpredicativeTypes, MultiParamTypeClasses, ScopedTypeVariables, TemplateHaskell, TypeFamilies,
             UndecidableInstances #-}
{-# OPTIONS_GHC -Wall #-}
module Main
    ( main
    ) where

import Report (Report, ReportElem(ReportUndecided), reportUUID, reportBody, ReportElemID(ReportElemID),
               ReportMap(ReportMap), ReportID(ReportID, unReportID))
import Data.Data
import Data.Generics.Aliases (extB)
import Data.Int  (Int64, Int32)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import qualified Data.Text as T (Text, empty)
import qualified Data.UUID as UUID
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
      rid = ReportID (fromJust $ UUID.fromString "604b4920-7a6d-4ff2-9138-b9c0ba69290b")

      report :: Report
      report =
          empty { reportUUID = unReportID rid
                , reportBody = Order.fromList [ Report.ReportUndecided ] }

      reportMap' :: ReportMap
      reportMap' = ReportMap $ Map.fromList [(rid, report)]

-- | Construct the empty value for a datatype. For algebraic datatypes, the
-- leftmost constructor is chosen.
empty :: forall a. Data a => a
empty = general
      `extB` char
      `extB` int
      `extB` int64
      `extB` int32
      `extB` integer
      `extB` float
      `extB` double
      `extB` text
      `extB` uuid where
  -- Generic case
  general :: Data a => a
  general = fromConstrB empty (indexConstr (dataTypeOf general) 1)

  -- Base cases
  char    = '\NUL'
  int     = 0      :: Int
  int64   = 0      :: Int64
  int32   = 0      :: Int32
  integer = 0      :: Integer
  float   = 0.0    :: Float
  double  = 0.0    :: Double
  text    = T.empty :: T.Text
  uuid    = UUID.nil
