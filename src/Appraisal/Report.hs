{-# LANGUAGE CPP, DeriveDataTypeable, FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses,
             OverloadedStrings, RecordWildCards, StandaloneDeriving, TypeSynonymInstances, TemplateHaskell, TypeFamilies #-}
{-# OPTIONS -fcontext-stack=100 -fno-warn-orphans -fno-warn-missing-signatures -fno-warn-name-shadowing -fwarn-incomplete-patterns #-}
{-# OPTIONS_GHC -O0 -Wall -fno-warn-orphans #-}
module Appraisal.Report
    ( AuthorID(..),
      ReportElemID(..),
      MarkupID(..),
      MarkupPairID(..),
      AbbrevPairID(..),
      AbbrevPair,
      MarkupPair,
      ReportIntendedUse,
      ReportStatus,
      MaybeReportIntendedUse,
      MarkupPairs,
      EpochMilli,
      Markups,
      Branding,
      ReportElems,
      Authors,
      AbbrevPairs,
      ReportValueTypeInfo(ReportValueTypeInfo),
      ReportValueApproachInfo(ReportValueApproachInfo),
      Report(Report, reportUUID, reportBody),
      ReportFlags(ReportFlags),
      ReportElem(ReportItem, ReportParagraph, ReportUndecided),
      Author(Author),
      reportBrandingLens
    ) where

import Appraisal.Config (Paths(reports), reportsURIPath)
import Appraisal.Currency (addCashValues, CashValue, Priceable(..))
import Appraisal.File (File(File, fileSource, fileChksum, fileMessages), FileSource(TheURI))
import Appraisal.ImageFile (ImageFile(ImageFile, imageFile, imageFileType, imageFileWidth, imageFileHeight, imageFileMaxVal), ImageType(..))
import Appraisal.IntJS (deriveOrderJS)
import Appraisal.Markup as M (Markup, mapChars, rawMarkdown, markupText)
import Appraisal.Permissions (Permissions)
import Appraisal.ReportItem (Item(..))
import qualified Appraisal.ReportItem as I (Item(fields), ItemFieldName(ItemDataSheetNumber))
import Appraisal.Utils.CIString (CIString)
import Appraisal.Utils.Debug (trace'')
import Appraisal.Utils.List (spanBy)
import Appraisal.Utils.Text (read)
import qualified Data.UUID.Types as UUID (toString)
import Data.UUID.Types (UUID)
import Data.Char (isDigit, toLower)
import Data.Function (on)
import Data.Generics (Data, everywhere, mkT, Typeable)
import Data.Int (Int64)
-- import qualified Data.IxSet.Revision as R (Ident(..), Revision(..), RevisionInfo(..))
import Control.Lens (Lens', lens)
import Data.List as List (groupBy, sortBy)
import qualified Data.ListLike as LL
import Data.Map as Map (lookup)
import Data.Maybe (catMaybes)
import Data.SafeCopy (base, deriveSafeCopy)
import Data.Text as T (Text, groupBy, pack, unpack, strip, uncons, empty)
import Debug.Trace (trace)
import Language.Haskell.TH.Path.Core (lens_mrs, readShowLens)
import Language.Haskell.TH.Path.Graph (SelfPath)
import Language.Haskell.TH.Path.Order as Order (toList, asList)
import Language.Haskell.TH.Path.View (View(ViewType, viewLens))
import Data.UUID.Orphans ()
import Prelude hiding (read)
import System.FilePath ((</>))
import Text.PrettyPrint.HughesPJClass (Pretty(pPrint), text)
import Web.Routes.TH (derivePathInfo)

data ReportFieldLabel
    = ReportName
    | ReportDate
    | ReportContractDate
    | ReportInspectionDate
    | ReportEffectiveDate
    | ReportAuthor
    | ReportPreparer
    | ReportPreparerEIN
    | ReportPreparerAddress
    | ReportPreparerEMail
    | ReportPreparerWebsite
    | ReportTitle
    | ReportIntendedUse
    | ReportValueType
    | ReportValueApproach
    | ReportClient
    | ReportClientGreeting
    | ReportItemsOwnerFull
    | ReportItemsOwner
    | ReportBriefItems
    | ReportInspectionLocation
    | ParagraphText
    | ReportElemType
    deriving (Read, Show, Eq)

data Author
    = Author { _authorName :: Markup, _authorCredentials :: Markup }
    deriving (Read, Show, Eq, Ord, Typeable, Data)

$(deriveOrderJS ''Author)
$(deriveSafeCopy 1 'base ''AuthorID)
$(derivePathInfo ''AuthorID)

data AuthorFieldLabel
    = AuthorName
    | AuthorCredentials
    deriving (Read, Show, Eq, Ord, Typeable, Data)

data ReportValueTypeInfo
    = ReportValueTypeInfo
      { _reportValueTypeName :: Markup
      , _reportValueTypeDescription :: Markup
      , _reportValueTypeDefinition :: Markup
      } deriving (Read, Show, Eq, Ord, Typeable, Data)

data ReportIntendedUse_1
    = SalesAdvisory_1
    | EstatePlanning_1
    | EstateTax_1
    | Insurance_1
    | CharitableDonation_1
    deriving (Read, Show, Eq, Ord, Typeable, Data)

data ReportIntendedUse_2
    = SalesAdvisory_2
    | EstatePlanning_2
    | EstateTax_2
    | Insurance_2
    | CharitableDonation_2
    | EquitableDistribution_2
    | MaritalDissolution_2
    | EstateDivision_2
    deriving (Read, Show, Eq, Ord, Bounded, Enum, Typeable, Data)

data ReportIntendedUse
    = SalesAdvisory
    | EstatePlanning
    | EstateTax
    | InsuranceCoverage
    | InsuranceClaim
    | CharitableDonation
    | EquitableDistribution
    | MaritalDissolution
    | EstateDivision
    deriving (Read, Show, Eq, Ord, Bounded, Enum, Typeable, Data)

instance View ReportIntendedUse where
    type ViewType ReportIntendedUse = String
    viewLens = readShowLens

data ReportValueApproachInfo
    = ReportValueApproachInfo
    { _reportValueApproachName :: Markup
    , _reportValueApproachDescription :: Markup
    } deriving (Read, Show, Eq, Ord, Typeable, Data)

data ReportElem
    = ReportItem {_elemItem :: Item}
    | ReportParagraph {_elemText :: Markup}
    | ReportUndecided
    deriving (Read, Show, Eq, Ord, Typeable, Data)

data ReportStatus
    = Draft
    -- ^ This is the current authoritative version of the report
    | Final
    -- ^ The report has been downloaded, and perhaps uploaded to a
    -- different server.
    deriving (Read, Show, Eq, Ord, Typeable, Data)

-- Does ReportStatus belong in ReportFlags?  Seems more like meta info.
data ReportFlags = ReportFlags {
  _hideEmptyItemFields :: Bool
  } deriving (Read, Show, Eq, Ord, Typeable, Data)

data Branding
    = NoLogo
    | Logo ImageFile
    deriving (Read, Show, Eq, Ord, Typeable, Data)

type EpochMilli = Int64

type MarkupPair = (Markup, Markup)
type AbbrevPair = (CIString, Markup)

$(deriveOrderJS ''ReportElem)
$(deriveOrderJS ''MarkupPair)
$(deriveOrderJS ''AbbrevPair)
$(deriveOrderJS ''Markup)

type MaybeReportIntendedUse = Maybe ReportIntendedUse

instance View MaybeReportIntendedUse where
    type ViewType MaybeReportIntendedUse = String
    viewLens = lens_mrs

data Report
    = Report { _reportFolder :: FilePath
             , _reportName :: Markup
             , _reportDate :: Markup
             , _reportContractDate :: Markup
             , _reportInspectionDate :: Markup
             , _reportEffectiveDate :: Markup
             , _reportAuthors :: Authors
             , _reportPreparer :: Markup
             , _reportPreparerEIN :: Markup
             , _reportPreparerAddress :: Markup
             , _reportPreparerEMail :: Markup
             , _reportPreparerWebsite :: Markup
             , _reportAbbrevs :: AbbrevPairs
             , _reportTitle :: Markup
             , _reportHeader :: Markup
             , _reportFooter :: Markup
             , _reportIntendedUse :: MaybeReportIntendedUse
             , _reportValueTypeInfo :: ReportValueTypeInfo
             , _reportValueApproachInfo :: ReportValueApproachInfo
             , _reportClientName :: Markup
             , _reportClientAddress :: Markup
             , _reportClientGreeting :: Markup
             , _reportItemsOwnerFull :: Markup
             , _reportItemsOwner :: Markup
             , _reportBriefItems :: Markup
             , _reportInspectionLocation :: Markup
             , reportBody :: ReportElems
             , _reportGlossary :: MarkupPairs
             , _reportSources :: MarkupPairs
             , _reportLetterOfTransmittal :: Markup
             , _reportScopeOfWork :: Markup
             , _reportCertification :: Markups
             , _reportLimitingConditions :: Markups
             , _reportPrivacyPolicy :: Markup
             , _reportPerms :: Permissions
             , _reportRevision :: Integer
             , _reportCreated :: EpochMilli
             , _reportBranding :: Branding
             , _reportStatus :: ReportStatus
             , _reportRedacted :: Bool
             , _reportFlags :: ReportFlags
             , reportUUID :: UUID
             , _reportOrderByItemName :: Bool
             , _reportDisplayItemName :: Bool
             }
    deriving (Read, Show, Eq, Ord, Typeable, Data)

data ReportElemTypeName
    = UndecidedElem
    | ItemElem
    | CommentaryElem
    deriving (Read, Show, Eq)

-- |This is a simplified version of the debian version number
-- comparison algorithm.  It splits a string into numeric and non
-- numeric stretches, and compares the non-numeric portions lexically
-- and the numeric portions as numbers.
compareVersions :: Markup -> Markup -> Ordering
compareVersions a b = compareVersions' (markupText a) (markupText b)

compareVersions' :: T.Text -> T.Text -> Ordering
compareVersions' a b =
    cmp a' b'
    where
      a' :: [Tagged]
      a' = map (tag . T.strip) (T.groupBy (\ x y -> isDigit x == isDigit y) a)
      b' :: [Tagged]
      b' = map (tag . T.strip) (T.groupBy (\ x y -> isDigit x == isDigit y) b)
      -- Tag the groups
      tag :: Text -> Tagged
      tag t =
          case T.uncons t of
            Nothing -> Chars T.empty -- Should not happen
            Just (c, _) | isDigit c -> Digits t
            Just _ -> Chars t
      -- Compare tagged groups
      cmp (Digits x : xs) (Digits y : ys) = case compare (read (T.unpack x) :: Int) (read (T.unpack y) :: Int) of EQ -> cmp xs ys; other -> other
      cmp (Chars x : xs) (Chars y : ys) = case compare x y of EQ -> cmp xs ys; other -> other
      cmp (Digits _ : _) (Chars _ : _) = GT
      cmp (Chars _ : _) (Digits _ : _) = LT
      cmp (_ : _) [] = GT
      cmp [] (_ : _) = LT
      cmp _ _ = EQ

data Tagged = Digits Text | Chars Text

reportBrandingLens :: Lens' Branding Text
reportBrandingLens = lens getter setter
  where getter NoLogo = pack ""
        getter (Logo (ImageFile {imageFile = File {fileSource = Just (TheURI sURI)}})) = pack $ sURI
        getter (Logo (ImageFile {imageFile = File {fileSource = Nothing, fileChksum = csum}}))
            | csum == "17e667c2bbe83e098510607571cffc00" =
                pack $ "Thompson & Martinez"
            | csum == "62e7310af0008fa68de56ab9d1b60e8f" =
                pack $ "Thompson & Martinez New"
            | csum == "c3bd1388b41fa5d956e4308ce518a8bd" =
                pack $ "Thompson & Martinez Wide"
            | csum == "cb913fc45e16135fc540a114c25c8a28" =
                pack $ "Goldfield Appraisals"
            | csum == "6ad232e854c6ff80fd2ec11b2d3af21d" =
                pack $ "Thompson Martinez Goldfield"
            | csum == "4ffb5f95b3baf7790a413e768f1fb2b2" =
                pack $ "Goldfield Appraisals 2"
            | csum == "f92d08935f8ba2cee3427b24fb3c263f" =
                pack $ "Goldfield Appraisals 3"
        getter (Logo x) = trace ("Unhandled Logo condition: " ++ show x) (pack $ "Unhandled Logo condition, see DSF")
        setter _logo x =
            case x of
              _ | unpack (T.strip x) == "Thompson & Martinez" ->
                    Logo (ImageFile { imageFile = File {fileSource = Nothing, fileChksum = "17e667c2bbe83e098510607571cffc00", fileMessages = []}
                                    , imageFileType = JPEG, imageFileWidth = 348, imageFileHeight = 140, imageFileMaxVal = 255 })
              _ | unpack (T.strip x) == "Thompson & Martinez New" ->
                    Logo (ImageFile { imageFile = File {fileSource = Nothing, fileChksum = "62e7310af0008fa68de56ab9d1b60e8f", fileMessages = []}
                                    , imageFileType = JPEG, imageFileWidth = 324, imageFileHeight = 400, imageFileMaxVal = 255 })
              _ | unpack (T.strip x) == "Thompson & Martinez Wide" ->
                    Logo (ImageFile { imageFile = File {fileSource = Nothing, fileChksum = "c3bd1388b41fa5d956e4308ce518a8bd", fileMessages = []}
                                    , imageFileType = PNG, imageFileWidth = 595, imageFileHeight = 114, imageFileMaxVal = 255 })
              _ | unpack (T.strip x) == "Goldfield Appraisals" ->
                    Logo (ImageFile { imageFile = File {fileSource = Nothing, fileChksum = "cb913fc45e16135fc540a114c25c8a28", fileMessages = []}
                                    , imageFileType = JPEG, imageFileWidth = 229, imageFileHeight = 90, imageFileMaxVal = 255 })
              _ | unpack (T.strip x) == "Thompson Martinez Goldfield" ->
                    Logo (ImageFile { imageFile = File {fileSource = Nothing, fileChksum = "6ad232e854c6ff80fd2ec11b2d3af21d", fileMessages = []}
                                    , imageFileType = JPEG, imageFileWidth = 704, imageFileHeight = 140, imageFileMaxVal = 255 })
              _ | unpack (T.strip x) == "Goldfield Appraisals 2" ->
                    Logo (ImageFile { imageFile = File {fileSource = Nothing, fileChksum = "4ffb5f95b3baf7790a413e768f1fb2b2", fileMessages = []}
                                    , imageFileType = JPEG, imageFileWidth = 2250, imageFileHeight = 225, imageFileMaxVal = 255 })
              _ | unpack (T.strip x) == "Goldfield Appraisals 3" ->
                    Logo (ImageFile { imageFile = File {fileSource = Nothing, fileChksum = "f92d08935f8ba2cee3427b24fb3c263f", fileMessages = []}
                                    , imageFileType = JPEG, imageFileWidth = 1280, imageFileHeight = 113, imageFileMaxVal = 255 })
              _ -> case reads (unpack x) of
                     [(b,_)] -> b
                     _ -> trace'' ("reportBrandingLens dropping value " ++ unpack x) NoLogo

$(deriveSafeCopy 1 'base ''Author)
$(deriveSafeCopy 1 'base ''ReportValueTypeInfo)
$(deriveSafeCopy 1 'base ''ReportIntendedUse_1)
$(deriveSafeCopy 3 'base ''ReportIntendedUse)
$(deriveSafeCopy 1 'base ''ReportValueApproachInfo)
$(deriveSafeCopy 1 'base ''ReportElem)
$(deriveSafeCopy 17 'base ''Report)
$(deriveSafeCopy 1 'base ''ReportStatus)
$(deriveSafeCopy 0 'base ''ReportFlags)
$(deriveSafeCopy 1 'base ''Branding)

$(deriveSafeCopy 1 'base ''ReportElemID)
$(deriveSafeCopy 1 'base ''MarkupPairID)
$(deriveSafeCopy 1 'base ''AbbrevPairID)
$(deriveSafeCopy 1 'base ''MarkupID)
$(derivePathInfo ''ReportElemID)
$(derivePathInfo ''MarkupPairID)
$(derivePathInfo ''AbbrevPairID)
$(derivePathInfo ''MarkupID)

instance Pretty MarkupID where
    pPrint = text . show . unMarkupID

instance Pretty AuthorID where
    pPrint = text . show . unAuthorID

instance Pretty MarkupPairID where
    pPrint = text . show . unMarkupPairID

instance Pretty AbbrevPairID where
    pPrint = text . show . unAbbrevPairID

instance Pretty ReportElemID where
    pPrint = text . show . unReportElemID

instance SelfPath AbbrevPairID
instance SelfPath AuthorID
instance SelfPath MarkupID
instance SelfPath MarkupPairID
instance SelfPath ReportElemID
