{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module ListLens
    ( listReorder
    ) where

import Appraisal.Report (Report, ReportElems)
import Appraisal.ReportImage (ReportImages)
import Appraisal.ReportInstances (ReportView(ReportView))
import Appraisal.ReportMap (ReportID, ReportMap(ReportMap))
-- import Appraisal.ReportPaths -- (Path_Report(Report_ReportCertification), Path_ReportID, Path_ReportMap(ReportMap_UnReportMap))
import Control.Lens
import Language.Haskell.TH.Path.Core (Path_OMap(Path_OMap), Path(toLens), mat)
import Language.Haskell.TH.Path.Order as Order (Order, order, permute, OrderKey)
import Language.Haskell.TH.Path.View ( View(viewLens) )
import SiteMap (WhichList(..), ElemID(..))
import Text.PrettyPrint.HughesPJClass (Pretty, prettyShow)

listReorder :: (WhichList, [ElemID]) -> ReportID -> ReportMap -> (ReportMap, [String])
listReorder which rid rmp =
  case which of
    (ElementList, ps) ->
        listReorder'' rmp (map SiteMap.unElemID ps) (lns)
    (ItemImage _, _) -> listReorder'' undefined undefined (undefined :: Traversal' ReportMap ReportImages)
    where
      lns :: Traversal' ReportMap ReportElems
      -- lns = toLens (Path_ReportMap_unReportMap (Path_Look rid (Path_Report_View (Path_ReportView__reportBody Path_OMap))))
      lns = (iso (\(ReportMap x) -> x) ReportMap) . mat rid . (viewLens :: Lens' Report ReportView) . id . lens_ReportView__reportBody . toLens Path_OMap
      lens_ReportView__reportBody f (ReportView x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44) =
                        fmap (\y1 -> ReportView x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 y1  x28 x29 x30 x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44) (f x27)

      -- toLens (Path_ReportMap_unReportMap _x) = lens_ReportMap_unReportMap . toLens _x
      -- toLens (Path_Look k v) = mat k . toLens v
      -- toLens (Path_Report_View v) = (viewLens :: Lens' Report ReportView) . toLens v
      -- toLens (Path_ReportView__reportBody _x) = lens_ReportView__reportBody . toLens _x

listReorder'' :: forall k v. (Pretty k, Enum k, OrderKey k) =>
                 ReportMap -> [k] -> Traversal' ReportMap (Order k v) -> (ReportMap, [String])
listReorder'' rmp ps lns =
    maybe (rmp, []) reorder (rmp ^? lns)
    where
      reorder xs =
          case Order.permute ps xs of
            (xs', [], []) ->
                ((lns .~ xs') rmp,
                 ["permutation: " ++ prettyShow ps,
                  "original:    " ++ prettyShow (Order.order xs),
                  "permuted:    " ++ prettyShow (Order.order xs')])
            (_, deleted, invalid) ->
                (rmp,
                 ["Error reordering list:",
                  "original: " ++ prettyShow (Order.order xs),
                  "permuted: " ++ prettyShow ps,
                  "deleted:  " ++ prettyShow (map fst deleted),
                  "invalid:  " ++ prettyShow invalid])
