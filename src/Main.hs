{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, FlexibleInstances, FunctionalDependencies,
             ImpredicativeTypes, MultiParamTypeClasses, ScopedTypeVariables, TemplateHaskell, TypeFamilies,
             UndecidableInstances #-}
{-# OPTIONS_GHC -Wall #-}
module Main
    ( main
    ) where

import Report ( ReportElemID(ReportElemID))
import ListLens ( ReportElems )
import Order
import ListLens (listReorder)

-- Main creates a value of type ReportMap with one element, and then
-- tries to reorder a list inside that element using listReorder.
main :: IO ()
main = (`seq` return ()) $ listReorder order reportMap'
    where
      order :: ReportElemID
      order = ReportElemID 1

      report :: ReportElems
      report = Order.insert

      reportMap' :: ReportElems
      reportMap' =  report
