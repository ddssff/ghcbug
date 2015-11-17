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

import Appraisal.Markup (Markup)
import Appraisal.ReportImage(ReportImages)
import Data.Generics (Data, Typeable)
import Data.Map as Map (Map)
import qualified Data.Text as T

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

data Item
    = Item { itemName :: T.Text
           , fields :: Map ItemFieldName Markup
           , images :: ReportImages
           } deriving (Show, Read, Eq, Ord, Data, Typeable)
