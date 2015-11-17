{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module ListLens
    ( WhichList(..)
    , ElemID(..)
    , ReportImages
    , listReorder
    ) where

import Report (Report(Report), ReportElems, ReportID, ReportMap(ReportMap))
import Control.Lens
import qualified Data.Map as M (Map, insert, lookup)
import Order (Order, order, permute, OrderKey)
import Data.Data ( Data )
import Data.Typeable ( Typeable )
import Report (ReportElemID(..), IntJS, ToIntJS(intJS))

data ReportImage
    = Pic Bool
    deriving (Eq, Ord, Show, Data, Typeable)

newtype ReportImageID
      = ReportImageID {unReportImageID :: IntJS}
      deriving (Eq, Ord, Read, Show, Data, Typeable)
type ReportImages =
        Order ReportImageID ReportImage
instance Enum ReportImageID where
      toEnum = (ReportImageID . toEnum)
      fromEnum = (fromEnum . unReportImageID)
instance ToIntJS ReportImageID where
      intJS = unReportImageID

data ElemID
    = ElemImageID {unElemImageID :: ReportImageID}
    | ElemID {unElemID :: Report.ReportElemID}
    deriving (Eq, Ord, Read, Show, Typeable, Data)

data WhichList = LimitingConditionsList
               | AuthorsList
               | SourcesList
               | AbbrevsList
               | CertificationList
               | GlossaryList
               | ElementList
               | ItemImage Report.ReportElemID
               | TestDataList
               deriving (Eq, Ord, Read, Show, Typeable, Data)

mat :: forall k a. (Show k, Ord k) => k -> Traversal' (M.Map k a) a
mat k = lens (M.lookup k) (\ mp ma -> maybe mp (\ a -> M.insert k a mp) ma) . _Just

listReorder :: (WhichList, [ElemID]) -> ReportID -> ReportMap -> (ReportMap, [String])
listReorder which rid rmp =
  case which of
    (ElementList, ps) ->
        listReorder'' rmp (map unElemID ps) (lns)
    (ItemImage _, _) ->
        -- Removing or changing this makes the bug vanish
        listReorder'' undefined undefined (undefined :: Traversal' ReportMap ReportImages)
    where
      lns :: Traversal' ReportMap ReportElems
      lns = (iso (\(ReportMap x) -> x) ReportMap) . mat rid . lens_Report__reportBody . iso id id
      lens_Report__reportBody f (Report x27 x28) =
                    fmap (\y1 -> Report y1  x28) (f x27)

listReorder'' :: forall k v. (Show k, Enum k, OrderKey k) =>
                 ReportMap -> [k] -> Traversal' ReportMap (Order k v) -> (ReportMap, [String])
listReorder'' rmp ps lns =
    maybe (rmp, []) reorder (rmp ^? lns)
    where
      reorder xs =
          case Order.permute ps xs of
            (xs', [], []) ->
                ((lns .~ xs') rmp,
                 ["permutation: " ++ show ps,
                  "original:    " ++ show (Order.order xs),
                  "permuted:    " ++ show (Order.order xs')])
            (_, deleted, invalid) ->
                (rmp,
                 ["Error reordering list:",
                  "original: " ++ show (Order.order xs),
                  "permuted: " ++ show ps,
                  "deleted:  " ++ show (map fst deleted),
                  "invalid:  " ++ show invalid])
