{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeFamilies #-}
module Appraisal.ReportItem
    ( Item(..)
    , ItemFieldName(..)
    ) where

import Appraisal.Currency(Priceable(..), parseCashValue)
import Appraisal.Markup (Markup, markupText)
import Appraisal.ReportImage(ReportImages)
import Data.Generics (Data, Typeable)
import Data.Map as Map (Map, lookup)
--import Data.SafeCopy (deriveSafeCopy, base)
import qualified Data.Text as T
import Language.Haskell.TH.Path.Graph (SelfPath)

data ItemFieldName
    = ItemLocation
    | ItemDataSheetNumber
    | ItemTypeOfObject
    | ItemArtistOrMaker
    | ItemArtistDate
    | ItemCountryOrNationality
    | ItemSubjectOrTitle
    | ItemSignatureInscriptionsMarkings
    | ItemMediumMaterialsTechniques
    | ItemDateOrPeriod
    | ItemSupportBaseFrame
    | ItemFrameMeasurement
    | ItemItemMeasurement
    | ItemProvenance
    | ItemCost
    | ItemCondition
    | ItemEdition
    | ItemDescription
    | ItemMarketAnalysis
    | ItemExhibitionsAndPublications
    | ItemAdditionalNotes
    | ItemCashValue
    deriving (Show, Read, Eq, Ord, Data, Typeable)

{-
$(deriveSafeCopy 2 'base ''ItemFieldName)
-}

instance SelfPath ItemFieldName

data Item
    = Item { itemName :: T.Text
           , fields :: Map ItemFieldName Markup
           , images :: ReportImages -- Use a typedef here so that paths like Path_Report ReportImages are generated
           } deriving (Show, Read, Eq, Ord, Data, Typeable)

noFieldValue :: Markup
noFieldValue = mempty

getItemFieldValue :: ItemFieldName -> Item -> Markup
getItemFieldValue name item =
    mapFindWithDefault noFieldValue (fields item)
    where
      mapFindWithDefault def mp =
          maybe def id (Map.lookup name mp)

instance Priceable Item where
    cashValue item = parseCashValue $ T.unpack $ markupText $ getItemFieldValue ItemCashValue item

{-
$(deriveSafeCopy 7 'base ''Item)
-}

