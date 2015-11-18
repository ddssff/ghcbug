{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module ListLens where

import qualified Data.Map as M
import Order (Order, order, insert, empty, permute, OrderKey)
import Report (ReportElemID(..))
import Data.Int (Int32)
import Control.Applicative
import Data.Monoid


data ReportElem = ReportUndecided deriving (Eq, Ord)
type ReportElems = Order ReportElemID ReportElem

data Report
    = Report { reportBody :: ReportElems
             }

type ReportID = ()
type ReportMap = M.Map () Report

listReorder :: [ReportElemID] ->  ReportMap -> ReportMap
listReorder ps rmp = listReorder'' ps (foo rmp)

foo :: ReportMap -> Maybe ReportElems
-- foo rmp = rmp ^? lns
foo rmp = Just (reportBody (rmp M.! ()))

listReorder'' :: forall k v. (Enum k, OrderKey k) =>
                 [k] -> (Maybe (Order k v)) -> ReportMap
listReorder'' ps order =
    maybe (error "foo") reorder order
    where
      reorder xs =
          case Order.permute ps xs of
            (_, [], []) -> error "foo"
