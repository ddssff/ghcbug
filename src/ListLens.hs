{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module ListLens where

import Lens.Micro (Traversal', lens, (^?), (.~), _Just)
import qualified Data.Map as M (Map, insert, lookup)
import Order (Order, order, permute, OrderKey)
import Report (ReportElemID(..))
import Data.Int (Int32)



data ReportElem = ReportUndecided deriving (Eq, Ord)
type ReportElems = Order ReportElemID ReportElem

data Report
    = Report { reportBody :: ReportElems
             }

type ReportID = ()
type ReportMap = M.Map ReportID Report

mat :: Traversal' (M.Map () a) a
mat = lens (M.lookup ()) (\ mp ma -> maybe mp (\ a -> M.insert () a mp) ma) . _Just

listReorder :: [ReportElemID] ->  ReportMap -> ReportMap
listReorder ps rmp = listReorder'' rmp (map id ps) lns
    where
      lns :: Traversal' ReportMap ReportElems
      lns = lens id (\ _ x -> x) . mat . lens_Report__reportBody . lens id (\_ x -> x)
      lens_Report__reportBody f (Report x27 ) = fmap (\y1 -> Report y1) (f x27)

listReorder'' :: forall k v. (Enum k, OrderKey k) =>
                 ReportMap -> [k] -> Traversal' ReportMap (Order k v) -> ReportMap
listReorder'' rmp ps lns =
    maybe (error "foo") reorder (rmp ^? lns)
    where
      reorder xs =
          case Order.permute ps xs of
            (_, [], []) -> error "foo"
