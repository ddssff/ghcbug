{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, FlexibleInstances, FunctionalDependencies,
             ImpredicativeTypes, MultiParamTypeClasses, ScopedTypeVariables, TemplateHaskell, TypeFamilies,
             UndecidableInstances #-}
{-# OPTIONS_GHC -Wall #-}
module Main
    ( main
    ) where

import Report
import ListLens
import Order

-- Main creates a value of type ReportMap with one element, and then
-- tries to reorder a list inside that element using listReorder.
main :: IO ()
main = (`seq` return ()) $ listReorder order reportMap'
    where
      order :: ReportElemID
      order = ReportElemID 1

      report :: ReportElemID
      report = Order.insert

      reportMap' :: ReportElemID
      reportMap' =  report
