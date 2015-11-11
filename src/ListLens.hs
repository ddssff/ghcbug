{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module ListLens
    ( listReorder
    ) where

import Appraisal.Report (ReportElems)
import Appraisal.ReportImage (ReportImages)
import Appraisal.ReportMap (ReportID, ReportMap)
-- import Appraisal.ReportPaths -- (Path_Report(Report_ReportCertification), Path_ReportID, Path_ReportMap(ReportMap_UnReportMap))
import Control.Lens
import Language.Haskell.TH.Path.Core (Path_Map(Path_Look), Path_OMap(Path_OMap), Path(toLens))
import Language.Haskell.TH.Path.Order as Order (Order, order, permute, OrderKey)
import SiteMap (WhichList(..), ElemID(..), Path_ReportMap(..), Path_Report(..), Path_ReportView(..))
import Text.PrettyPrint.HughesPJClass (Pretty, prettyShow)

listReorder :: (WhichList, [ElemID]) -> ReportID -> ReportMap -> (ReportMap, [String])
listReorder which rid rmp =
  case which of
    (ElementList, ps) ->
        listReorder'' rmp (map SiteMap.unReportElemID ps) (toLens (Path_ReportMap_unReportMap (Path_Look rid (Path_Report_View (Path_ReportView__reportBody Path_OMap)))) :: Traversal' ReportMap ReportElems)
    (ItemImage _, _) -> listReorder'' undefined undefined (undefined :: Traversal' ReportMap ReportImages)

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
